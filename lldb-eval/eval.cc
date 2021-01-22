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

namespace {

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

static Value EvaluateArithmeticOpInteger(lldb::SBTarget target,
                                         clang::tok::TokenKind kind, Value lhs,
                                         Value rhs, lldb::SBType rtype) {
  assert((lhs.IsInteger() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be integers and have the same type");

  auto wrap = [target, rtype](auto value) {
    return CreateValueFromAPInt(target, value, rtype);
  };

  auto l = lhs.GetInteger();
  auto r = rhs.GetInteger();

  switch (kind) {
    case clang::tok::plus:
      return wrap(l + r);
    case clang::tok::minus:
      return wrap(l - r);
    case clang::tok::slash:
      return wrap(l / r);
    case clang::tok::star:
      return wrap(l * r);
    case clang::tok::percent:
      return wrap(l % r);
    case clang::tok::amp:
      return wrap(l & r);
    case clang::tok::pipe:
      return wrap(l | r);
    case clang::tok::caret:
      return wrap(l ^ r);
    case clang::tok::lessless:
      return wrap(l.shl(r));
    case clang::tok::greatergreater:
      return wrap(l.lshr(r));

    default:
      break;
  }

  lldb_eval_unreachable("invalid arithmetic op");
}

static Value EvaluateArithmeticOpFloat(lldb::SBTarget target,
                                       clang::tok::TokenKind kind, Value lhs,
                                       Value rhs, lldb::SBType rtype) {
  assert((lhs.IsFloat() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be floats and have the same type");

  auto wrap = [target, rtype](auto value) {
    return CreateValueFromAPFloat(target, value, rtype);
  };

  auto l = lhs.GetFloat();
  auto r = rhs.GetFloat();

  switch (kind) {
    case clang::tok::plus:
      return wrap(l + r);
    case clang::tok::minus:
      return wrap(l - r);
    case clang::tok::slash:
      return wrap(l / r);
    case clang::tok::star:
      return wrap(l * r);

    default:
      break;
  }

  lldb_eval_unreachable("invalid arithmetic op");
}

static Value EvaluateArithmeticOp(lldb::SBTarget target,
                                  clang::tok::TokenKind kind, Value lhs,
                                  Value rhs, lldb::SBType rtype) {
  // Evaluate arithmetic operation for two integral values.
  if (rtype.GetTypeFlags() & lldb::eTypeIsInteger) {
    return EvaluateArithmeticOpInteger(target, kind, lhs, rhs, rtype);
  }

  // Evaluate arithmetic operation for two floating point values.
  if (rtype.GetTypeFlags() & lldb::eTypeIsFloat) {
    return EvaluateArithmeticOpFloat(target, kind, lhs, rhs, rtype);
  }

  lldb_eval_unreachable("Result of arithmetic conversion is not a scalar");
}

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
  // Get the type and the value we need to cast.
  lldb::SBType type = node->type();
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  switch (node->kind()) {
    case CStyleCastKind::kArithmetic: {
      // Pick an appropriate cast.
      if (rhs.IsPointer() || rhs.IsNullPtrType()) {
        result_ = CastPointerToBasicType(target_, rhs, type);
      } else if (rhs.IsScalar()) {
        result_ = CastScalarToBasicType(target_, rhs, type);
      } else if (rhs.IsEnum()) {
        result_ = CastEnumToBasicType(target_, rhs, type);
      } else {
        assert(false &&
               "invalid ast: operand is not convertible to arithmetic type");
      }
      return;
    }
    case CStyleCastKind::kEnumeration: {
      uint64_t value = rhs.GetUInt64();
      result_ = CreateValueFromBytes(target_, &value, type);
      return;
    }
    case CStyleCastKind::kPointer: {
      result_ = CreateValueFromPointer(target_, rhs.GetUInt64(), type);
      return;
    }
    case CStyleCastKind::kReference: {
      result_ = Value(rhs.inner_value().Cast(type.GetDereferencedType()));
      return;
    }
  }
  lldb_eval_unreachable("invalid c-style cast kind");
}

void Interpreter::Visit(const MemberOfNode* node) {
  assert(!node->member_index().empty() && "invalid ast: member index is empty");

  Value lhs = EvalNode(node->lhs());
  if (!lhs) {
    return;
  }

  // LHS can be a pointer to value, but GetChildAtIndex works for pointers too,
  // so we don't need to dereference it explicitely. This also avoid having an
  // "ephemeral" parent Value, representing the dereferenced LHS.
  lldb::SBValue member_val = lhs.inner_value();
  for (uint32_t idx : node->member_index()) {
    member_val = member_val.GetChildAtIndex(idx);
  }
  assert(member_val && "invalid ast: invalid member access");

  // If value is a reference, dereference it to get to the underlying type. All
  // operations on a reference should be actually operations on the referent.
  if (member_val.GetType().IsReferenceType()) {
    member_val = member_val.Dereference();
  }

  result_ = Value(member_val);
}

void Interpreter::Visit(const ArraySubscriptNode* node) {
  auto base = EvalNode(node->base());
  if (!base) {
    return;
  }
  auto index = EvalNode(node->index());
  if (!index) {
    return;
  }

  lldb::SBType item_type;
  lldb::addr_t base_addr;

  if (node->is_pointer_base()) {
    item_type = base.type().GetPointeeType();
    base_addr = base.GetUInt64();
  } else {
    item_type = base.type().GetArrayElementType();
    base_addr = base.AddressOf().GetUInt64();
  }

  // Create a pointer and add the index, i.e. "base + index".
  Value value = PointerAdd(
      CreateValueFromPointer(target_, base_addr, item_type.GetPointerType()),
      index.GetUInt64());

  // Dereference the result, i.e. *(base + index).
  result_ = value.Dereference();
}

void Interpreter::Visit(const BinaryOpNode* node) {
  // Short-circuit logical operators.
  if (node->op() == clang::tok::ampamp || node->op() == clang::tok::pipepipe) {
    auto lhs = EvalNode(node->lhs());
    if (!lhs) {
      return;
    }
    assert(lhs.type().IsContextuallyConvertibleToBool() &&
           "invalid ast: must be convertible to bool");

    // For "&&" break if LHS is "false", for "||" if LHS is "true".
    bool lhs_val = lhs.GetBool();
    bool break_early = (node->op() == clang::tok::ampamp) ? !lhs_val : lhs_val;

    if (break_early) {
      result_ = CreateValueFromBool(target_, lhs_val);
      return;
    }

    // Breaking early didn't happen, evaluate the RHS and use it as a result.
    auto rhs = EvalNode(node->rhs());
    if (!rhs) {
      return;
    }
    assert(rhs.type().IsContextuallyConvertibleToBool() &&
           "invalid ast: must be convertible to bool");

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
    case clang::tok::pipe:
    case clang::tok::caret:
    case clang::tok::lessless:
    case clang::tok::greatergreater:
      result_ = EvaluateBinaryBitwise(node->op(), lhs, rhs);
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

  // Unsupported/invalid operation.
  std::string msg = "Unexpected op: " + node->op_name();
  error_.Set(ErrorCode::kUnknown, msg);
}

void Interpreter::Visit(const UnaryOpNode* node) {
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  switch (node->op()) {
    case clang::tok::star:
      // TODO(werat): Should dereference be a separate AST node?
      assert(rhs.type().IsPointerType() && "invalid ast: should be a pointer");
      result_ = rhs.Dereference();
      return;
    case clang::tok::amp:
      result_ = rhs.AddressOf();
      return;
    case clang::tok::plus:
      result_ = rhs;
      return;
    case clang::tok::minus:
      result_ = EvaluateUnaryMinus(rhs);
      return;
    case clang::tok::exclaim:
      result_ = EvaluateUnaryNegation(rhs);
      return;
    case clang::tok::tilde:
      result_ = EvaluateUnaryBitwiseNot(rhs);
      return;

    default:
      break;
  }

  // Unsupported/invalid operation.
  std::string msg = "Unexpected op: " + node->op_name();
  error_.Set(ErrorCode::kUnknown, msg);
}

void Interpreter::Visit(const TernaryOpNode* node) {
  auto cond = EvalNode(node->cond());
  if (!cond) {
    return;
  }
  assert(cond.type().IsContextuallyConvertibleToBool() &&
         "invalid ast: must be convertible to bool");

  if (cond.GetBool()) {
    result_ = EvalNode(node->lhs());
  } else {
    result_ = EvalNode(node->rhs());
  }
}

Value Interpreter::EvaluateComparison(clang::tok::TokenKind op, Value lhs,
                                      Value rhs) {
  // Evaluate arithmetic operation for two integral values.
  if (lhs.IsInteger() && rhs.IsInteger()) {
    bool ret = Compare(op, lhs.GetInteger(), rhs.GetInteger());
    return CreateValueFromBool(target_, ret);
  }

  // Evaluate arithmetic operation for two floating point values.
  if (lhs.IsFloat() && rhs.IsFloat()) {
    bool ret = Compare(op, lhs.GetFloat(), rhs.GetFloat());
    return CreateValueFromBool(target_, ret);
  }

  // Evaluate arithmetic operation for two scoped enum values.
  if (lhs.IsScopedEnum() && rhs.IsScopedEnum()) {
    bool ret = Compare(op, lhs.GetUInt64(), rhs.GetUInt64());
    return CreateValueFromBool(target_, ret);
  }

  // Must be pointer/integer and/or nullptr comparison.
  bool ret = Compare(op, lhs.GetUInt64(), rhs.GetUInt64());
  return CreateValueFromBool(target_, ret);
}

Value Interpreter::EvaluateUnaryMinus(Value rhs) {
  assert((rhs.IsInteger() || rhs.IsFloat()) &&
         "invalid ast: must be an arithmetic type");

  if (rhs.IsInteger()) {
    llvm::APSInt v = rhs.GetInteger();
    v.negate();
    return CreateValueFromAPInt(target_, v, rhs.type());
  }
  if (rhs.IsFloat()) {
    llvm::APFloat v = rhs.GetFloat();
    v.changeSign();
    return CreateValueFromAPFloat(target_, v, rhs.type());
  }

  error_.Set(ErrorCode::kUnknown, "operand is not arithmetic");
  return Value();
}

Value Interpreter::EvaluateUnaryNegation(Value rhs) {
  assert(rhs.type().IsContextuallyConvertibleToBool() &&
         "invalid ast: must be convertible to bool");
  return CreateValueFromBool(target_, !rhs.GetBool());
}

Value Interpreter::EvaluateUnaryBitwiseNot(Value rhs) {
  assert(rhs.IsInteger() && "invalid ast: must be an integer");
  llvm::APSInt v = rhs.GetInteger();
  v.flipAllBits();
  return CreateValueFromAPInt(target_, v, rhs.type());
}

Value Interpreter::EvaluateBinaryAddition(Value lhs, Value rhs) {
  // Addition of two arithmetic types.
  if (lhs.IsScalar() && rhs.IsScalar()) {
    assert(CompareTypes(lhs.type(), rhs.type()) &&
           "invalid ast: operand must have the same type");
    return EvaluateArithmeticOp(target_, clang::tok::plus, lhs, rhs,
                                lhs.type());
  }

  // Here one of the operands must be a pointer and the other one an integer.
  Value ptr, offset;
  if (lhs.IsPointer()) {
    ptr = lhs;
    offset = rhs;
  } else {
    ptr = rhs;
    offset = lhs;
  }
  assert(ptr.IsPointer() && "invalid ast: ptr must be a pointer");
  assert(offset.IsInteger() && "invalid ast: offset must be an integer");
  return PointerAdd(ptr, offset.GetUInt64());
}

Value Interpreter::EvaluateBinarySubtraction(Value lhs, Value rhs) {
  if (lhs.IsScalar() && rhs.IsScalar()) {
    assert(CompareTypes(lhs.type(), rhs.type()) &&
           "invalid ast: operand must have the same type");
    return EvaluateArithmeticOp(target_, clang::tok::minus, lhs, rhs,
                                lhs.type());
  }
  assert(lhs.IsPointer() && "invalid ast: lhs must be a pointer");

  // "pointer - integer" operation.
  if (rhs.IsInteger()) {
    return PointerAdd(lhs, -rhs.GetUInt64());
  }

  // "pointer - pointer" operation.
  assert(rhs.IsPointer() && "invalid ast: rhs must an integer or a pointer");
  assert((lhs.type().GetPointeeType().GetByteSize() ==
          rhs.type().GetPointeeType().GetByteSize()) &&
         "invalid ast: pointees should be the same size");

  // Since pointers have compatible types, both have the same pointee size.
  uint64_t item_size = lhs.type().GetPointeeType().GetByteSize();

  // Pointer difference is technically ptrdiff_t, but the important part is
  // that it is signed.
  int64_t diff = static_cast<ptrdiff_t>(lhs.GetUInt64() - rhs.GetUInt64()) /
                 static_cast<int64_t>(item_size);

  return CreateValueFromBytes(target_, &diff, lldb::eBasicTypeLongLong);
}

Value Interpreter::EvaluateBinaryMultiplication(Value lhs, Value rhs) {
  assert((lhs.IsScalar() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be arithmetic and have the same type");

  return EvaluateArithmeticOp(target_, clang::tok::star, lhs, rhs, lhs.type());
}

Value Interpreter::EvaluateBinaryDivision(Value lhs, Value rhs) {
  assert((lhs.IsScalar() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be arithmetic and have the same type");

  // Check for zero only for integer division.
  if (rhs.IsInteger() && rhs.GetUInt64() == 0) {
    // This is UB and the compiler would generate a warning:
    //
    //  warning: division by zero is undefined [-Wdivision-by-zero]
    //
    return rhs;
  }

  return EvaluateArithmeticOp(target_, clang::tok::slash, lhs, rhs, lhs.type());
}

Value Interpreter::EvaluateBinaryRemainder(Value lhs, Value rhs) {
  assert((lhs.IsInteger() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be integers and have the same type");

  if (rhs.GetUInt64() == 0) {
    // This is UB and the compiler would generate a warning:
    //
    //  warning: remainder by zero is undefined [-Wdivision-by-zero]
    //
    return rhs;
  }

  return EvaluateArithmeticOpInteger(target_, clang::tok::percent, lhs, rhs,
                                     lhs.type());
}

Value Interpreter::EvaluateBinaryBitwise(clang::tok::TokenKind kind, Value lhs,
                                         Value rhs) {
  assert((lhs.IsInteger() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be integers and have the same type");

  return EvaluateArithmeticOpInteger(target_, kind, lhs, rhs, lhs.type());
}

Value Interpreter::PointerAdd(Value lhs, int64_t offset) {
  uintptr_t addr =
      lhs.GetUInt64() + offset * lhs.type().GetPointeeType().GetByteSize();

  return CreateValueFromPointer(target_, addr, lhs.type());
}

}  // namespace lldb_eval
