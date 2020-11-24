// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "lldb-eval/eval.h"

#include <memory>

#include "clang/Basic/TokenKinds.h"
#include "lldb-eval/ast.h"
#include "lldb-eval/context.h"
#include "lldb-eval/defines.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"
#include "llvm/Support/FormatVariadic.h"

namespace {

const char* kInvalidOperandsToUnaryExpression =
    "invalid argument type '{0}' to unary expression";

const char* kInvalidOperandsToBinaryExpression =
    "invalid operands to binary expression ('{0}' and '{1}')";

template <typename T>
bool Compare(clang::tok::TokenKind op, const T& l, const T& r) {
  switch (op) {
    case clang::tok::equalequal:
      return l == r;
    case clang::tok::exclaimequal:
      return l != r;
    case clang::tok::less:
      return l < r;
    case clang::tok::lessequal:
      return l <= r;
    case clang::tok::greater:
      return l > r;
    case clang::tok::greaterequal:
      return l >= r;

    default:
      lldb_eval_unreachable("Invalid comparison operation.");
  }
}

// Comparison operators for llvm::APFloat were introduced in LLVM 11.
#if LLVM_VERSION_MAJOR < 11

template <>
bool Compare(clang::tok::TokenKind op, const llvm::APFloat& l,
             const llvm::APFloat& r) {
  switch (op) {
    case clang::tok::equalequal:
      return l.compare(r) == llvm::APFloat::cmpEqual;
    case clang::tok::exclaimequal:
      return l.compare(r) != llvm::APFloat::cmpEqual;
    case clang::tok::less:
      return l.compare(r) == llvm::APFloat::cmpLessThan;
    case clang::tok::greater:
      return l.compare(r) == llvm::APFloat::cmpGreaterThan;

    case clang::tok::lessequal: {
      llvm::APFloat::cmpResult result = l.compare(r);
      return result == llvm::APFloat::cmpLessThan ||
             result == llvm::APFloat::cmpEqual;
    }
    case clang::tok::greaterequal: {
      llvm::APFloat::cmpResult result = l.compare(r);
      return result == llvm::APFloat::cmpGreaterThan ||
             result == llvm::APFloat::cmpEqual;
    }

    default:
      lldb_eval_unreachable("Invalid comparison operation.");
  }
}

#endif

}  // namespace

namespace lldb_eval {

Value Interpreter::Eval(const AstNode* tree, Error& error) {
  // Evaluate an AST.
  EvalNode(tree);
  // Grab the error and reset the interpreter state.
  error = error_;
  error_.Clear();
  // Return the computed result. If there was an error, it will be invalid.
  return result_;
}

Value Interpreter::EvalNode(const AstNode* node) {
  // Traverse an AST pointed by the `node`.
  node->Accept(this);
  // If there was an error, reset the result.
  if (error_) result_ = {};
  // Return the computed value for convenience. The caller is responsible for
  // checking if an error occured during the evaluation.
  return result_;
}

void Interpreter::Visit(const ErrorNode*) {
  error_.Set(ErrorCode::kUnknown, "The AST is not valid.");
}

void Interpreter::Visit(const LiteralNode* node) { result_ = node->value(); }

void Interpreter::Visit(const IdentifierNode* node) {
  result_ = node->value();

  // If value is a reference, dereference it to get to the underlying type. All
  // operations on a reference should be actually operations on the referent.
  if (result_.type().IsReferenceType()) {
    result_ = result_.Dereference();
  }
}

void Interpreter::Visit(const CStyleCastNode* node) {
  // TODO(werat): CStyleCastNode should already contain a resolved type.

  // Resolve the type from the type declaration.
  TypeDeclaration type_decl = node->type_decl();

  // Resolve the type within the current expression context.
  lldb::SBType type = ctx_->ResolveTypeByName(type_decl.GetBaseName().c_str());

  if (!type.IsValid()) {
    // TODO(werat): Make sure we don't have false negative errors here.
    std::string msg =
        "use of undeclared identifier '" + type_decl.GetBaseName() + "'";
    error_.Set(ErrorCode::kUndeclaredIdentifier, msg);
    return;
  }

  // Resolve pointers/references.
  for (clang::tok::TokenKind tk : type_decl.ptr_operators_) {
    if (tk == clang::tok::star) {
      // Pointers to reference types are forbidden.
      if (type.IsReferenceType()) {
        std::string msg = llvm::formatv(
            "'type name' declared as a pointer to a reference of type '{0}'",
            type.GetName());
        error_.Set(ErrorCode::kInvalidOperandType, msg);
        return;
      }
      // Get pointer type for the base type: e.g. int* -> int**.
      type = type.GetPointerType();

    } else if (tk == clang::tok::amp) {
      // References to references are forbidden.
      if (type.IsReferenceType()) {
        std::string msg = "type name declared as a reference to a reference";
        error_.Set(ErrorCode::kInvalidOperandType, msg);
        return;
      }
      // Get reference type for the base type: e.g. int -> int&.
      type = type.GetReferenceType();
    }
  }

  // At this point we need to know the type of the value we're going to cast.
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  // Cast to basic type (integer/float).
  if (type.GetCanonicalType().GetTypeFlags() & lldb::eTypeIsScalar) {
    // Cast result
    Value value;

    // Pointers can be cast to integers of the same or larger size.
    if (rhs.IsPointer()) {
      // C-style cast from pointer to float/double is not allowed.
      if (type.GetCanonicalType().GetTypeFlags() & lldb::eTypeIsFloat) {
        std::string type_name = type.GetName();
        std::string msg =
            "C-style cast from '{0}' to '" + type_name + "' is not allowed";
        ReportTypeError(msg.c_str(), rhs);
        return;
      }

      // Check if the result type is at least as big as the pointer size.
      if (type.GetByteSize() < sizeof(void*)) {
        std::string msg = llvm::formatv(
            "cast from pointer to smaller type '{0}' loses information",
            type.GetName());
        ReportTypeError(msg.c_str());
        return;
      }

      value = CastPointerToBasicType(target_, rhs, type);

    } else if (rhs.IsScalar()) {
      value = CastScalarToBasicType(target_, rhs, type);

    } else {
      std::string msg = llvm::formatv(
          "cannot convert '{{0}' to '{0}' without a conversion operator",
          type.GetName());
      ReportTypeError(msg.c_str(), rhs);
      return;
    }

    if (!value.IsValid()) {
      std::string msg = llvm::formatv("casting '{0}' to '{1}' invalid",
                                      rhs.type().GetName(), type.GetName());
      // This can be a false-negative error (the cast is actually valid), so
      // make it unknown for now.
      // TODO(werat): Make sure there are not false-negative errors.
      error_.Set(ErrorCode::kUnknown, msg);
      return;
    }

    result_ = value;
    return;
  }

  // Cast to pointer type.
  if (type.IsPointerType()) {
    // TODO(b/161677840): Implement type compatibility checks.
    // TODO(b/161677840): Do some error handling here.
    uintptr_t addr;

    if (rhs.IsInteger()) {
      addr = rhs.ConvertTo<uintptr_t>();
    } else if (rhs.IsPointer()) {
      addr = rhs.GetValueAsAddress();
    } else {
      std::string msg = llvm::formatv(
          "cannot cast from type '{{0}' to pointer type '{0}'", type.GetName());
      ReportTypeError(msg.c_str(), rhs);
      return;
    }

    result_ = CreateValueFromPointer(target_, addr, type);
    return;
  }

  // If target type is a reference, just do reinterpret_cast.
  if (type.IsReferenceType()) {
    result_ = Value(rhs.inner_value().Cast(type.GetDereferencedType()),
                    /*is_rvalue*/ true);
    return;
  }

  std::string msg =
      llvm::formatv("casting of '{0}' to '{1}' is not implemented yet",
                    rhs.type().GetName(), type.GetName());
  error_.Set(ErrorCode::kNotImplemented, msg);
}

void Interpreter::Visit(const MemberOfNode* node) {
  Value lhs = EvalNode(node->lhs());
  if (!lhs) {
    return;
  }

  switch (node->type()) {
    case MemberOfNode::Type::OF_OBJECT:
      // "member of object" operator, check that LHS is an object.
      if (lhs.IsPointer()) {
        ReportTypeError(
            "member reference type '{0}' is a pointer; "
            "did you mean to use '->'?",
            lhs);
        return;
      }
      break;

    case MemberOfNode::Type::OF_POINTER:
      // "member of pointer" operator, check that LHS is a pointer and
      // dereference it.
      if (!lhs.IsPointer()) {
        ReportTypeError(
            "member reference type '{0}' is not a pointer; "
            "did you mean to use '.'?",
            lhs);
        return;
      }
      lhs = lhs.Dereference();
      break;
  }

  // Check if LHS is a record type, i.e. class/struct or union.
  lldb::TypeClass class_specifier =
      (lldb::eTypeClassClass | lldb::eTypeClassStruct | lldb::eTypeClassUnion);

  if (!(lhs.type().GetTypeClass() & class_specifier)) {
    ReportTypeError(
        "member reference base type '{0}' is not a structure or union", lhs);
    return;
  }

  Value member_val = Value(
      lhs.inner_value().GetChildMemberWithName(node->member_id().c_str()));

  if (!member_val) {
    auto msg =
        llvm::formatv("no member named '{0}' in '{1}'", node->member_id(),
                      lhs.type().GetUnqualifiedType().GetName());
    error_.Set(ErrorCode::kInvalidOperandType, msg);
    return;
  }

  // If value is a reference, dereference it to get to the underlying type. All
  // operations on a reference should be actually operations on the referent.
  if (member_val.type().IsReferenceType()) {
    member_val = member_val.Dereference();
  }

  result_ = member_val;
}

void Interpreter::Visit(const BinaryOpNode* node) {
  // Short-circuit logical operators.
  if (node->op() == clang::tok::ampamp || node->op() == clang::tok::pipepipe) {
    auto lhs = EvalNode(node->lhs());
    if (!lhs || !BoolConvertible(lhs)) {
      return;
    }

    if (node->op() == clang::tok::ampamp) {
      // Check if the left condition is false, then break out early.
      if (!lhs.GetBool()) {
        result_ = CreateValueFromBool(target_, false);
        return;
      }
    } else {
      // Check if the left condition is true, then break out early.
      if (lhs.GetBool()) {
        result_ = CreateValueFromBool(target_, true);
        return;
      }
    }

    auto rhs = EvalNode(node->rhs());
    if (!rhs || !BoolConvertible(rhs)) {
      return;
    }
    result_ = CreateValueFromBool(target_, rhs.GetBool());
    return;
  }

  // All other binary operations require evaluating both operands.
  auto lhs = EvalNode(node->lhs());
  if (!lhs) {
    return;
  }
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  switch (node->op()) {
    // "l_square" is a subscript operator -- array[index].
    case clang::tok::l_square:
      result_ = EvaluateSubscript(lhs, rhs);
      return;
    case clang::tok::plus:
      result_ = EvaluateBinaryAddition(lhs, rhs);
      return;
    case clang::tok::minus:
      result_ = EvaluateBinarySubtraction(lhs, rhs);
      return;
    case clang::tok::star:
      result_ = EvaluateBinaryMultiplication(lhs, rhs);
      return;
    case clang::tok::slash:
      result_ = EvaluateBinaryDivision(lhs, rhs);
      return;
    case clang::tok::percent:
      result_ = EvaluateBinaryRemainder(lhs, rhs);
      return;
    case clang::tok::amp:
      result_ = EvaluateBinaryBitAnd(lhs, rhs);
      return;
    case clang::tok::pipe:
      result_ = EvaluateBinaryBitOr(lhs, rhs);
      return;
    case clang::tok::caret:
      result_ = EvaluateBinaryBitXor(lhs, rhs);
      return;
    case clang::tok::lessless:
      result_ = EvaluateBinaryBitShl(lhs, rhs);
      return;
    case clang::tok::greatergreater:
      result_ = EvaluateBinaryBitShr(lhs, rhs);
      return;

    // Comparison operations.
    case clang::tok::equalequal:
    case clang::tok::exclaimequal:
    case clang::tok::less:
    case clang::tok::lessequal:
    case clang::tok::greater:
    case clang::tok::greaterequal:
      result_ = EvaluateComparison(node->op(), lhs, rhs);
      return;

    default:
      break;
  }

  std::string msg = "Unexpected op: " + node->op_name();
  error_.Set(ErrorCode::kUnknown, msg);
}

void Interpreter::Visit(const UnaryOpNode* node) {
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  // TODO(werat): Should dereference be a separate AST node?
  if (node->op() == clang::tok::star) {
    if (!rhs.IsPointer()) {
      // TODO(werat): Add literal value to the error message.
      ReportTypeError("indirection requires pointer operand. ('{0}' invalid)",
                      rhs);
      return;
    }

    result_ = rhs.Dereference();
    return;
  }

  // Address-of operator.
  if (node->op() == clang::tok::amp) {
    if (rhs.IsRvalue()) {
      ReportTypeError("cannot take the address of an rvalue of type '{0}'",
                      rhs);
      return;
    }
    result_ = rhs.AddressOf();
    return;
  }

  // Unary plus.
  if (node->op() == clang::tok::plus) {
    result_ = EvaluateUnaryPlus(rhs);
    return;
  }

  // Unary minus.
  if (node->op() == clang::tok::minus) {
    result_ = EvaluateUnaryMinus(rhs);
    return;
  }

  // Unary negation (!).
  if (node->op() == clang::tok::exclaim) {
    result_ = EvaluateUnaryNegation(rhs);
    return;
  }

  // Bitwise NOT (~).
  if (node->op() == clang::tok::tilde) {
    result_ = EvaluateUnaryBitwiseNot(rhs);
    return;
  }

  // Unsupported/invalid operation.
  std::string msg = "Unexpected op: " + node->op_name();
  error_.Set(ErrorCode::kUnknown, msg);
}

void Interpreter::Visit(const TernaryOpNode* node) {
  auto cond = EvalNode(node->cond());
  if (!cond || !BoolConvertible(cond)) {
    return;
  }

  if (cond.GetBool()) {
    result_ = EvalNode(node->lhs());
  } else {
    result_ = EvalNode(node->rhs());
  }
}

Value Interpreter::EvaluateSubscript(Value lhs, Value rhs) {
  // C99 6.5.2.1p2: the expression e1[e2] is by definition precisely
  // equivalent to the expression *((e1)+(e2)).
  // We need to figure out which expression is "base" and which is "index".

  Value base, index;

  lldb::SBType lhs_type = lhs.type();
  lldb::SBType rhs_type = rhs.type();

  if (lhs_type.IsArrayType() || lhs_type.IsPointerType()) {
    base = lhs;
    index = rhs;
  } else if (rhs_type.IsArrayType() || rhs_type.IsPointerType()) {
    base = rhs;
    index = lhs;
  } else {
    ReportTypeError("subscripted value is not an array or pointer");
    return Value();
  }

  // Index can be a typedef of a typedef of a typedef of a typedef...
  // Get canonical underlying type.
  lldb::SBType index_type = index.type().GetCanonicalType();

  // Check if the index is of an integral type.
  if (index_type.GetBasicType() < lldb::eBasicTypeChar ||
      index_type.GetBasicType() > lldb::eBasicTypeBool) {
    ReportTypeError("array subscript is not an integer");
    return Value();
  }

  lldb::SBType item_type;
  lldb::addr_t base_addr;

  if (base.type().IsArrayType()) {
    item_type = base.type().GetArrayElementType();
    base_addr = base.AddressOf().GetValueAsAddress();
  } else if (base.type().IsPointerType()) {
    item_type = base.type().GetPointeeType();
    base_addr = static_cast<lldb::addr_t>(base.GetValueAsAddress());
  } else {
    lldb_eval_unreachable("Subscripted value must be either array or pointer.");
  }

  // Create a pointer and add the index, i.e. "base + index".
  auto val = PointerAdd(
      CreateValueFromPointer(target_, base_addr, item_type.GetPointerType()),
      index.GetInt64());

  // Dereference the result, i.e. *(base + index).
  return val.Dereference();
}

Value Interpreter::EvaluateComparison(clang::tok::TokenKind op, Value lhs,
                                      Value rhs) {
  // Comparison works for:
  //
  //  scalar <-> scalar
  //  pointer <-> pointer (if pointee types are compatible)
  //  pointer <-> integer
  //  integer <-> pointer

  if (lhs.IsScalar() && rhs.IsScalar()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);

    // Evaluate arithmetic operation for two integral values.
    if (rtype.GetTypeFlags() & lldb::eTypeIsInteger) {
      bool ret = Compare(op, lhs.GetInteger(), rhs.GetInteger());
      return CreateValueFromBool(target_, ret);
    }

    // Evaluate arithmetic operation for two floating point values.
    if (rtype.GetTypeFlags() & lldb::eTypeIsFloat) {
      bool ret = Compare(op, lhs.GetFloat(), rhs.GetFloat());
      return CreateValueFromBool(target_, ret);
    }

    lldb_eval_unreachable("Result of arithmetic conversion is not a scalar");
  }

  bool is_ordered = op == clang::tok::less || op == clang::tok::lessequal ||
                    op == clang::tok::greater || op == clang::tok::greaterequal;

  // Check if the value can be compared to a pointer. We allow all pointers,
  // integers and a nullptr literal if it's an equality/inequality comparison.
  // For "pointer <-> integer" C++ allows only equality/inequality comparison
  // against literal zero and nullptr. However in the debugger context it's
  // often useful to compare a pointer with an integer representing an address.
  // That said, this also allows comparing nullptr and any integer, not just
  // literal zero, e.g. "nullptr == 1 -> false". C++ doesn't allow it, but we
  // implement this for convenience.
  auto comparable_to_pointer = [&](Value v) {
    return v.IsPointer() || v.IsInteger() || (!is_ordered && v.IsNullPtrType());
  };

  if (comparable_to_pointer(lhs) && comparable_to_pointer(rhs)) {
    // If both are pointers, check if they have comparable types. Comparing
    // pointers to void is always allowed.
    if ((lhs.IsPointer() && !lhs.IsPointerToVoid()) &&
        (rhs.IsPointer() && !rhs.IsPointerToVoid())) {
      auto lhs_type = lhs.type().GetCanonicalType().GetUnqualifiedType();
      auto rhs_type = rhs.type().GetCanonicalType().GetUnqualifiedType();

      if (lhs_type != rhs_type) {
        ReportTypeError(
            "comparison of distinct pointer types ('{0}' and '{1}')", lhs, rhs);
        return Value();
      }
    }

    // Use `GetValueAsAddress()` for pointer operands and `GetInt64` for
    // integers/nullptr literals. We can't just read an unsigned value, because
    // the integer might be signed and needs to be promoted to a proper type
    // (i.e. uintptr_t).
    auto lhs_addr = lhs.IsPointer() ? lhs.GetValueAsAddress() : lhs.GetInt64();
    auto rhs_addr = rhs.IsPointer() ? rhs.GetValueAsAddress() : rhs.GetInt64();

    return CreateValueFromBool(target_, Compare(op, lhs_addr, rhs_addr));
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateUnaryPlus(Value rhs) {
  // Integer values may require integral promotion.
  if (rhs.IsInteger()) {
    return IntegralPromotion(target_, rhs);
  }

  // Floats and pointers are just converted to rvalues.
  if (rhs.IsFloat() || rhs.IsPointer()) {
    return rhs.GetRvalueRef();
  }

  ReportTypeError(kInvalidOperandsToUnaryExpression, rhs);
  return Value();
}

Value Interpreter::EvaluateUnaryMinus(Value rhs) {
  // Integer values may require integral promotion.
  if (rhs.IsInteger()) {
    rhs = IntegralPromotion(target_, rhs);
    llvm::APSInt v = rhs.GetInteger();
    v.negate();
    return CreateValueFromAPInt(target_, v, rhs.type());
  }

  if (rhs.IsFloat()) {
    llvm::APFloat v = rhs.GetFloat();
    v.changeSign();
    return CreateValueFromAPFloat(target_, v, rhs.type());
  }

  ReportTypeError(kInvalidOperandsToUnaryExpression, rhs);
  return Value();
}

Value Interpreter::EvaluateUnaryNegation(Value rhs) {
  if (!BoolConvertible(rhs)) {
    return Value();
  }

  return CreateValueFromBool(target_, !rhs.GetBool());
}

Value Interpreter::EvaluateUnaryBitwiseNot(Value rhs) {
  // Integer values may require integral promotion.
  if (rhs.IsInteger()) {
    rhs = IntegralPromotion(target_, rhs);
    llvm::APSInt v = rhs.GetInteger();
    v.flipAllBits();
    return CreateValueFromAPInt(target_, v, rhs.type());
  }

  ReportTypeError(kInvalidOperandsToUnaryExpression, rhs);
  return Value();
}

Value Interpreter::EvaluateBinaryAddition(Value lhs, Value rhs) {
  // Operation '+' works for:
  //
  //  scalar <-> scalar
  //  scalar <-> pointer
  //  pointer <-> scalar

  if (lhs.IsScalar() && rhs.IsScalar()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::ADD, lhs, rhs, rtype);
  }

  bool pointer_arithmetic_operation = (lhs.IsPointer() && rhs.IsScalar()) ||
                                      (lhs.IsScalar() && rhs.IsPointer());

  if (pointer_arithmetic_operation) {
    // Figure out which operand is the pointer and which one is the offset.
    Value ptr, offset;

    if (lhs.IsPointer()) {
      ptr = lhs;
      offset = rhs;
    } else {
      ptr = rhs;
      offset = lhs;
    }

    if (ptr.IsPointerToVoid()) {
      ReportTypeError("arithmetic on a pointer to void");
      return Value();
    }

    return PointerAdd(ptr, offset.GetInt64());
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateBinarySubtraction(Value lhs, Value rhs) {
  // Operation '-' works for:
  //
  //  scalar <-> scalar
  //  pointer <-> scalar
  //  pointer <-> pointer (if pointee types are compatible)

  if (lhs.IsScalar() && rhs.IsScalar()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::SUB, lhs, rhs, rtype);
  }

  if (lhs.IsPointer() && rhs.IsScalar()) {
    if (lhs.IsPointerToVoid()) {
      ReportTypeError("arithmetic on a pointer to void");
      return Value();
    }
    return PointerAdd(lhs, -rhs.GetInt64());
  }

  if (lhs.IsPointer() && rhs.IsPointer()) {
    if (lhs.IsPointerToVoid() && rhs.IsPointerToVoid()) {
      ReportTypeError("arithmetic on pointers to void");
      return Value();
    }

    auto lhs_type = lhs.type().GetCanonicalType().GetUnqualifiedType();
    auto rhs_type = rhs.type().GetCanonicalType().GetUnqualifiedType();

    if (lhs_type != rhs_type) {
      ReportTypeError("'{0}' and '{1}' are not pointers to compatible types",
                      lhs, rhs);
      return Value();
    }

    // Since pointers have compatible types, both have the same pointee size.
    uint64_t item_size = lhs.type().GetPointeeType().GetByteSize();

    // Pointer difference is technically ptrdiff_t, but the important part is
    // that it is signed.
    int64_t diff = static_cast<ptrdiff_t>(lhs.GetValueAsAddress() -
                                          rhs.GetValueAsAddress()) /
                   static_cast<int64_t>(item_size);

    return CreateValueFromBytes(target_, &diff, lldb::eBasicTypeLongLong);
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateBinaryMultiplication(Value lhs, Value rhs) {
  if (!lhs.IsScalar() || !rhs.IsScalar()) {
    ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
    return Value();
  }

  lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
  return EvaluateArithmeticOp(target_, ArithmeticOp::MUL, lhs, rhs, rtype);
}

Value Interpreter::EvaluateBinaryDivision(Value lhs, Value rhs) {
  if (!lhs.IsScalar() || !rhs.IsScalar()) {
    ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
    return Value();
  }

  // Check if one of the arguments is float. Is this case we can safely proceed,
  // there will be no zero division.
  bool float_div = lhs.IsFloat() || rhs.IsFloat();

  if (!float_div && rhs.GetInt64() == 0) {
    // This is UB and the compiler would generate a warning:
    //
    //  warning: division by zero is undefined [-Wdivision-by-zero]
    //
    return CreateValueZero(target_);
  }

  lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
  return EvaluateArithmeticOp(target_, ArithmeticOp::DIV, lhs, rhs, rtype);
}

Value Interpreter::EvaluateBinaryRemainder(Value lhs, Value rhs) {
  // Remainder is defined only for integral types.
  if (!lhs.IsInteger() || !rhs.IsInteger()) {
    ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
    return Value();
  }

  if (rhs.GetInt64() == 0) {
    // This is UB and the compiler would generate a warning:
    //
    //  warning: remainder by zero is undefined [-Wdivision-by-zero]
    //
    return CreateValueZero(target_);
  }

  lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
  return EvaluateArithmeticOp(target_, ArithmeticOp::REM, lhs, rhs, rtype);
}

Value Interpreter::EvaluateBinaryBitAnd(Value lhs, Value rhs) {
  if (lhs.IsInteger() && rhs.IsInteger()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::BIT_AND, lhs, rhs,
                                rtype);
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateBinaryBitOr(Value lhs, Value rhs) {
  if (lhs.IsInteger() && rhs.IsInteger()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::BIT_OR, lhs, rhs, rtype);
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateBinaryBitXor(Value lhs, Value rhs) {
  if (lhs.IsInteger() && rhs.IsInteger()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::BIT_XOR, lhs, rhs,
                                rtype);
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateBinaryBitShl(Value lhs, Value rhs) {
  if (lhs.IsInteger() && rhs.IsInteger()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::BIT_SHL, lhs, rhs,
                                rtype);
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateBinaryBitShr(Value lhs, Value rhs) {
  if (lhs.IsInteger() && rhs.IsInteger()) {
    lldb::SBType rtype = UsualArithmeticConversions(target_, &lhs, &rhs);
    return EvaluateArithmeticOp(target_, ArithmeticOp::BIT_SHR, lhs, rhs,
                                rtype);
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

bool Interpreter::BoolConvertible(Value val) {
  if (val.IsScalar() || val.IsPointer()) {
    return true;
  }

  ReportTypeError(
      "value of type '{0}' is not contextually convertible to 'bool'", val);
  return false;
}

void Interpreter::ReportTypeError(const char* fmt) {
  error_.Set(ErrorCode::kInvalidOperandType, fmt);
}

void Interpreter::ReportTypeError(const char* fmt, Value val) {
  std::string rhs_type = val.type().GetName();

  auto msg = llvm::formatv(fmt, rhs_type);
  error_.Set(ErrorCode::kInvalidOperandType, msg);
}

void Interpreter::ReportTypeError(const char* fmt, Value lhs, Value rhs) {
  std::string lhs_type = lhs.type().GetName();
  std::string rhs_type = rhs.type().GetName();

  auto msg = llvm::formatv(fmt, lhs_type, rhs_type);
  error_.Set(ErrorCode::kInvalidOperandType, msg);
}

Value Interpreter::PointerAdd(Value lhs, int64_t offset) {
  uintptr_t addr = lhs.GetValueAsAddress() +
                   offset * lhs.type().GetPointeeType().GetByteSize();

  return CreateValueFromPointer(target_, addr, lhs.type());
}

Value EvaluateArithmeticOp(lldb::SBTarget target, ArithmeticOp op, Value lhs,
                           Value rhs, lldb::SBType rtype) {
  // Evaluate arithmetic operation for two integral values.
  if (rtype.GetTypeFlags() & lldb::eTypeIsInteger) {
    auto l = lhs.GetInteger();
    auto r = rhs.GetInteger();

    auto wrap = [target, rtype](auto value) {
      return CreateValueFromAPInt(target, value, rtype);
    };

    switch (op) {
      case ArithmeticOp::ADD:
        return wrap(l + r);
      case ArithmeticOp::SUB:
        return wrap(l - r);
      case ArithmeticOp::DIV:
        return wrap(l / r);
      case ArithmeticOp::MUL:
        return wrap(l * r);
      case ArithmeticOp::REM:
        return wrap(l % r);
      case ArithmeticOp::BIT_AND:
        return wrap(l & r);
      case ArithmeticOp::BIT_OR:
        return wrap(l | r);
      case ArithmeticOp::BIT_XOR:
        return wrap(l ^ r);
      case ArithmeticOp::BIT_SHL:
        return wrap(l.shl(r));
      case ArithmeticOp::BIT_SHR:
        return wrap(l.lshr(r));
    }
  }

  // Evaluate arithmetic operation for two floating point values.
  if (rtype.GetTypeFlags() & lldb::eTypeIsFloat) {
    auto l = lhs.GetFloat();
    auto r = rhs.GetFloat();

    auto wrap = [target, rtype](auto value) {
      return CreateValueFromAPFloat(target, value, rtype);
    };

    switch (op) {
      case ArithmeticOp::ADD:
        return wrap(l + r);
      case ArithmeticOp::SUB:
        return wrap(l - r);
      case ArithmeticOp::DIV:
        return wrap(l / r);
      case ArithmeticOp::MUL:
        return wrap(l * r);

      default:
        break;
    }
  }

  lldb_eval_unreachable("Result of arithmetic conversion is not a scalar");
}

}  // namespace lldb_eval
