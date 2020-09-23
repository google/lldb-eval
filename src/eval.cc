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

#include "eval.h"

#include <limits>
#include <memory>

#include "ast.h"
#include "clang/Basic/TokenKinds.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "llvm/Support/FormatVariadic.h"
#include "value.h"

namespace {

const char* kInvalidOperandsToBinaryExpression =
    "invalid operands to binary expression ('{0}' and '{1}')";

}  // namespace

namespace lldb_eval {

EvalError::EvalError() : code_(EvalErrorCode::OK) {}

void EvalError::Set(EvalErrorCode code, const std::string& message) {
  code_ = code;
  message_ = message;
}

void EvalError::Clear() { *this = {}; }

EvalErrorCode EvalError::code() const { return code_; }
const std::string& EvalError::message() const { return message_; };

EvalError::operator bool() const { return code_ != EvalErrorCode::OK; }

Value Interpreter::Eval(const AstNode* tree, EvalError& error) {
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
  error_.Set(EvalErrorCode::UNKNOWN, "The AST is not valid.");
}

void Interpreter::Visit(const BooleanLiteralNode* node) {
  result_ = Value(node->value());
}

void Interpreter::Visit(const NumericLiteralNode* node) {
  result_ = Value(node->value());
}

void Interpreter::Visit(const IdentifierNode* node) {
  // Internally values don't have global scope qualifier in their names and
  // LLDB doesn't support queries with it too.
  std::string name = node->name();
  bool global_scope = false;

  if (name.rfind("::", 0) == 0) {
    name = name.substr(2);
    global_scope = true;
  }

  lldb::SBValue value;

  // If the identifier doesn't refer to the global scope and doesn't have any
  // other scope qualifiers, try looking among the local and instance variables.
  if (!global_scope && name.find("::") == std::string::npos) {
    // Try looking for a local variable in current scope.
    if (!value) {
      value = frame_.FindVariable(name.c_str());
    }
    // Try looking for an instance variable (class member).
    if (!value) {
      value = frame_.FindVariable("this").GetChildMemberWithName(name.c_str());
    }
  }

  // Try looking for a global or static variable.
  if (!value) {
    // TODO(werat): Implement scope-aware lookup. Relative scopes should be
    // resolved relative to the current scope. I.e. if the current frame is in
    // "ns1::ns2::Foo()", then "ns2::x" should resolve to "ns1::ns2::x".

    // List global variable with the same "basename". There can be many matches
    // from other scopes (namespaces, classes), so we do additional filtering
    // later.
    lldb::SBValueList values = target_.FindGlobalVariables(
        name.c_str(), /*max_matches=*/std::numeric_limits<uint32_t>::max());

    // Find the corrent variable by matching the name. lldb::SBValue::GetName()
    // can return strings like "::globarVar", "ns::i" or "int const ns::foo"
    // depending on the version and the platform.
    for (uint32_t i = 0; i < values.GetSize(); ++i) {
      lldb::SBValue val = values.GetValueAtIndex(i);
      llvm::StringRef val_name = val.GetName();

      if (val_name == name || val_name == "::" + name ||
          val_name.endswith(" " + name)) {
        value = val;
        break;
      }
    }
  }

  if (!value) {
    std::string msg = "use of undeclared identifier '" + node->name() + "'";
    error_.Set(EvalErrorCode::UNDECLARED_IDENTIFIER, msg);
    return;
  }

  // Special case for "this" pointer. As per C++ standard, it's a prvalue.
  bool is_rvalue = node->name() == "this";

  result_ = Value(value, is_rvalue);
}

void Interpreter::Visit(const CStyleCastNode* node) {
  // Resolve the type from the type declaration.
  TypeDeclaration type_decl = node->type_decl();

  // Resolve the type within the current expression context.
  lldb::SBType type =
      expr_ctx_->ResolveTypeByName(type_decl.GetBaseName().c_str());

  if (!type.IsValid()) {
    // TODO(werat): Make sure we don't have false negative errors here.
    std::string msg =
        "use of undeclared identifier '" + type_decl.GetBaseName() + "'";
    error_.Set(EvalErrorCode::UNDECLARED_IDENTIFIER, msg);
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
        error_.Set(EvalErrorCode::INVALID_OPERAND_TYPE, msg);
        return;
      }
      // Get pointer type for the base type: e.g. int* -> int**.
      type = type.GetPointerType();

    } else if (tk == clang::tok::amp) {
      // References to references are forbidden.
      if (type.IsReferenceType()) {
        std::string msg = "type name declared as a reference to a reference";
        error_.Set(EvalErrorCode::INVALID_OPERAND_TYPE, msg);
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

      value = CastPointerToBasicType(rhs.AsPointer(), type, target_);

    } else if (rhs.IsScalar()) {
      value = CastScalarToBasicType(rhs.AsScalar(), type, target_);

    } else {
      std::string type_name = type.GetName();
      std::string msg = "cannot convert '{0}' to '" + type_name +
                        "' without a conversion operator";
      ReportTypeError(msg.c_str(), rhs);
      return;
    }

    if (!value.IsValid()) {
      std::string msg =
          llvm::formatv("casting '{0}' to '{1}' invalid",
                        rhs.AsSbValue(target_).GetTypeName(), type.GetName());
      // This can be a false-negative error (the cast is actually valid), so
      // make it unknown for now.
      // TODO(werat): Make sure there are not false-negative errors.
      error_.Set(EvalErrorCode::UNKNOWN, msg);
      return;
    }

    result_ = value;
    return;
  }

  // Cast to pointer type.
  if (type.IsPointerType()) {
    // TODO(b/161677840): Implement type compatibility checks.
    // TODO(b/161677840): Do some error handling here.
    result_ = Value(rhs.AsSbValue(target_).Cast(type));
    return;
  }

  std::string msg =
      llvm::formatv("casting of '{0}' to '{1}' is not implemented yet",
                    rhs.AsSbValue(target_).GetTypeName(), type.GetName());
  error_.Set(EvalErrorCode::NOT_IMPLEMENTED, msg);
}

void Interpreter::Visit(const MemberOfNode* node) {
  auto lhs = EvalNode(node->lhs());
  if (!lhs) {
    return;
  }

  lldb::SBValue lhs_val = lhs.AsSbValue(target_);

  switch (node->type()) {
    case MemberOfNode::Type::OF_OBJECT:
      // "member of object" operator, check that LHS is an object.
      if (lhs_val.GetType().IsPointerType()) {
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
      if (!lhs_val.GetType().IsPointerType()) {
        ReportTypeError(
            "member reference type '{0}' is not a pointer; "
            "did you mean to use '.'?",
            lhs);
        return;
      }
      lhs_val = lhs_val.Dereference();
      break;
  }

  // Check if LHS is a record type, i.e. class/struct or union.
  if (!(lhs_val.GetType().GetDereferencedType().GetTypeClass() &
        (lldb::eTypeClassClass | lldb::eTypeClassStruct |
         lldb::eTypeClassUnion))) {
    ReportTypeError(
        "member reference base type '{0}' is not a structure or union", lhs);
    return;
  }

  lldb::SBValue member_val =
      lhs_val.GetChildMemberWithName(node->member_id()->name().c_str());

  if (!member_val) {
    auto msg = llvm::formatv("no member named '{0}' in '{1}'",
                             node->member_id()->name(),
                             lhs_val.GetType().GetUnqualifiedType().GetName());
    error_.Set(EvalErrorCode::INVALID_OPERAND_TYPE, msg);
    return;
  }

  result_ = Value(member_val);
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
      if (!lhs.AsBool()) {
        result_ = Value(false);
        return;
      }
    } else {
      // Check if the left condition is true, then break out early.
      if (lhs.AsBool()) {
        result_ = Value(true);
        return;
      }
    }

    auto rhs = EvalNode(node->rhs());
    if (!rhs || !BoolConvertible(rhs)) {
      return;
    }
    result_ = Value(rhs.AsBool());
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

    // Binary addition.
    case clang::tok::plus:
      result_ = EvaluateAddition(lhs, rhs);
      return;

    // Binary subtraction.
    case clang::tok::minus:
      result_ = EvaluateSubtraction(lhs, rhs);
      return;

    // Comparison operations.
    case clang::tok::equalequal:
    case clang::tok::exclaimequal:
    case clang::tok::less:
    case clang::tok::lessequal:
    case clang::tok::greater:
    case clang::tok::greaterequal:
      result_ = EvaluateComparison(lhs, rhs, node->op());
      return;

    default:
      break;
  }

  // Everything else works only for scalar values.
  if (!lhs.IsScalar() || !rhs.IsScalar()) {
    ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
    return;
  }

  auto lhs_scalar = lhs.AsScalar();
  auto rhs_scalar = rhs.AsScalar();

  switch (node->op()) {
    case clang::tok::slash:
      result_ = Value(lhs_scalar / rhs_scalar);
      break;
    case clang::tok::star:
      result_ = Value(lhs_scalar * rhs_scalar);
      break;
    case clang::tok::pipe:
      result_ = Value(lhs_scalar | rhs_scalar);
      break;
    case clang::tok::amp:
      result_ = Value(lhs_scalar & rhs_scalar);
      break;
    case clang::tok::percent:
      result_ = Value(lhs_scalar % rhs_scalar);
      break;
    case clang::tok::caret:
      result_ = Value(lhs_scalar ^ rhs_scalar);
      break;
    case clang::tok::lessless:
      result_ = Value(lhs_scalar << rhs_scalar);
      break;
    case clang::tok::greatergreater:
      result_ = Value(lhs_scalar >> rhs_scalar);
      break;

    default: {
      std::string msg = "Unexpected op: " + node->op_name();
      error_.Set(EvalErrorCode::UNKNOWN, msg);
      return;
    }
  }
}

void Interpreter::Visit(const UnaryOpNode* node) {
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  // TODO(werat): Should dereference be a separate AST node?
  if (node->op() == clang::tok::star) {
    lldb::SBValue rhs_val = rhs.AsSbValue(target_);

    if (!rhs.IsPointer()) {
      // TODO(werat): Add literal value to the error message.
      ReportTypeError("indirection requires pointer operand. ('{0}' invalid)",
                      rhs);
      return;
    }

    result_ = Value(rhs_val.Dereference());
    return;
  }

  // Address-of operator.
  if (node->op() == clang::tok::amp) {
    lldb::SBValue rhs_val = rhs.AsSbValue(target_);

    if (rhs.IsRValue()) {
      ReportTypeError("cannot take the address of an rvalue of type '{0}'",
                      rhs);
      return;
    }

    result_ = Value(rhs_val.AddressOf(), /* is_rvalue */ true);
    return;
  }

  // Unary plus.
  if (node->op() == clang::tok::plus) {
    if (rhs.IsPointer()) {
      result_ = Value(rhs.AsPointer());
      return;
    }
    if (rhs.IsScalar()) {
      result_ = Value(rhs.AsScalar());
      return;
    }
  }

  // Unary minus.
  if (node->op() == clang::tok::minus) {
    if (rhs.IsPointer()) {
      ReportTypeError("invalid argument type '{0}' to unary expression", rhs);
      return;
    }
    if (rhs.IsScalar()) {
      result_ = Value(rhs.AsScalar() * Scalar(-1));
      return;
    }
  }

  // Unsupported/invalid operation.
  std::string msg = "Unexpected op: " + node->op_name();
  error_.Set(EvalErrorCode::UNKNOWN, msg);
}

void Interpreter::Visit(const TernaryOpNode* node) {
  auto cond = EvalNode(node->cond());
  if (!cond || !BoolConvertible(cond)) {
    return;
  }

  if (cond.AsBool()) {
    result_ = EvalNode(node->lhs());
  } else {
    result_ = EvalNode(node->rhs());
  }
}

Value Interpreter::EvaluateSubscript(Value& lhs, Value& rhs) {
  lldb::SBValue lhs_val = lhs.AsSbValue(target_);
  lldb::SBValue rhs_val = rhs.AsSbValue(target_);

  // C99 6.5.2.1p2: the expression e1[e2] is by definition precisely
  // equivalent to the expression *((e1)+(e2)).
  // We need to figure out which expression is "base" and which is "index".

  lldb::SBValue base, index;

  // Both lhs and rhs can be references, but that's acceptable. Look at
  // underlying types.
  lldb::SBType lhs_type = lhs_val.GetType().GetDereferencedType();
  lldb::SBType rhs_type = rhs_val.GetType().GetDereferencedType();

  if (lhs_type.IsArrayType() || lhs_type.IsPointerType()) {
    base = lhs_val;
    index = rhs_val;
  } else if (rhs_type.IsArrayType() || rhs_type.IsPointerType()) {
    base = rhs_val;
    index = lhs_val;
  } else {
    ReportTypeError("subscripted value is not an array or pointer");
    return Value();
  }

  // Base can be a reference type (e.g. "int (&)[]"). In this case we need to
  // dereference it, so we can get the underlying value.
  if (base.GetType().IsReferenceType()) {
    base = base.Dereference();
  }
  // Index can be a reference type too (e.g. "int&").
  if (index.GetType().IsReferenceType()) {
    index = index.Dereference();
  }

  // Index can be a typedef of a typedef of a typedef of a typedef...
  // Get canonical underlying type.
  lldb::SBType index_type = index.GetType().GetCanonicalType();

  // Check if the index is of an integral type.
  if (index_type.GetBasicType() < lldb::eBasicTypeChar ||
      index_type.GetBasicType() > lldb::eBasicTypeBool) {
    ReportTypeError("array subscript is not an integer");
    return Value();
  }

  lldb::SBType item_type;
  lldb::addr_t base_addr;

  if (base.GetType().IsArrayType()) {
    item_type = base.GetType().GetArrayElementType();
    base_addr = base.GetAddress().GetLoadAddress(target_);
  } else if (base.GetType().IsPointerType()) {
    item_type = base.GetType().GetPointeeType();
    base_addr = static_cast<lldb::addr_t>(base.GetValueAsUnsigned());
  } else {
    unreachable("Subscripted value must be either array or pointer.");
  }

  // Create a pointer and add the index, i.e. "base + index".
  auto pointer = Value(Pointer(base_addr, item_type.GetPointerType())
                           .Add(index.GetValueAsSigned()));
  // Dereference the result, i.e. *(base + index).
  return Value(pointer.AsSbValue(target_).Dereference());
}

Value Interpreter::EvaluateAddition(Value& lhs, Value& rhs) {
  // Operation '+' works for:
  //
  //  scalar <-> scalar
  //  scalar <-> pointer
  //  pointer <-> scalar

  if (lhs.IsScalar() && rhs.IsScalar()) {
    return Value(lhs.AsScalar() + rhs.AsScalar());
  }

  bool pointer_arithmetic_operation = (lhs.IsPointer() && rhs.IsScalar()) ||
                                      (lhs.IsScalar() && rhs.IsPointer());

  if (pointer_arithmetic_operation) {
    // Figure out which operand is pointer and which one is scalar;
    Pointer pointer;
    Scalar scalar;

    if (lhs.IsPointer()) {
      pointer = lhs.AsPointer();
      scalar = rhs.AsScalar();
    } else {
      pointer = rhs.AsPointer();
      scalar = lhs.AsScalar();
    }

    if (pointer.IsPointerToVoid()) {
      ReportTypeError("arithmetic on a pointer to void");
      return Value();
    }

    return Value(pointer.Add(scalar.GetInt64()));
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateSubtraction(Value& lhs, Value& rhs) {
  // Operation '-' works for:
  //
  //  scalar <-> scalar
  //  pointer <-> scalar
  //  pointer <-> pointer (if pointee types are compatible)

  if (lhs.IsScalar() && rhs.IsScalar()) {
    return Value(lhs.AsScalar() - rhs.AsScalar());
  }

  if (lhs.IsPointer() && rhs.IsScalar()) {
    if (lhs.AsPointer().IsPointerToVoid()) {
      ReportTypeError("arithmetic on a pointer to void");
      return Value();
    }
    return Value(lhs.AsPointer().Add(-rhs.AsScalar().GetInt64()));
  }

  if (lhs.IsPointer() && rhs.IsPointer()) {
    auto lhs_pointer = lhs.AsPointer();
    auto rhs_pointer = rhs.AsPointer();

    if (lhs_pointer.IsPointerToVoid() && rhs_pointer.IsPointerToVoid()) {
      ReportTypeError("arithmetic on pointers to void");
      return Value();
    }

    auto lhs_type = lhs_pointer.type().GetCanonicalType().GetUnqualifiedType();
    auto rhs_type = rhs_pointer.type().GetCanonicalType().GetUnqualifiedType();

    if (lhs_type != rhs_type) {
      ReportTypeError("'{0}' and '{1}' are not pointers to compatible types",
                      lhs, rhs);
      return Value();
    }

    // Since pointers have compatible types, both have the same pointee size.
    uint64_t item_size = lhs_pointer.type().GetPointeeType().GetByteSize();

    // Pointer difference is technically ptrdiff_t, but the important part is
    // that it is signed.
    int64_t diff =
        static_cast<ptrdiff_t>(lhs_pointer.addr() - rhs_pointer.addr()) /
        static_cast<int64_t>(item_size);

    return Value(Scalar(diff));
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

Value Interpreter::EvaluateComparison(Value& lhs, Value& rhs,
                                      clang::tok::TokenKind op) {
  // Comparison works for:
  //
  //  scalar <-> scalar
  //  pointer <-> pointer (if pointee types are compatible)

  if (lhs.IsScalar() && rhs.IsScalar()) {
    auto lhs_scalar = lhs.AsScalar();
    auto rhs_scalar = rhs.AsScalar();

    switch (op) {
      case clang::tok::equalequal:
        return Value(lhs_scalar == rhs_scalar);
      case clang::tok::exclaimequal:
        return Value(lhs_scalar != rhs_scalar);
      case clang::tok::less:
        return Value(lhs_scalar < rhs_scalar);
      case clang::tok::lessequal:
        return Value(lhs_scalar <= rhs_scalar);
      case clang::tok::greater:
        return Value(lhs_scalar > rhs_scalar);
      case clang::tok::greaterequal:
        return Value(lhs_scalar >= rhs_scalar);
      default:
        unreachable("Invalid comparison operation.");
    }
  }

  if (lhs.IsPointer() && rhs.IsPointer()) {
    auto lhs_pointer = lhs.AsPointer();
    auto rhs_pointer = rhs.AsPointer();

    // Comparing pointers to void is always allowed.
    if (!lhs_pointer.IsPointerToVoid() && !rhs_pointer.IsPointerToVoid()) {
      auto lhs_type =
          lhs_pointer.type().GetCanonicalType().GetUnqualifiedType();
      auto rhs_type =
          rhs_pointer.type().GetCanonicalType().GetUnqualifiedType();

      if (lhs_type != rhs_type) {
        ReportTypeError(
            "comparison of distinct pointer types ('{0}' and '{1}')", lhs, rhs);
        return Value();
      }
    }

    auto lhs_addr = lhs_pointer.addr();
    auto rhs_addr = rhs_pointer.addr();

    switch (op) {
      case clang::tok::equalequal:
        return Value(lhs_addr == rhs_addr);
      case clang::tok::exclaimequal:
        return Value(lhs_addr != rhs_addr);
      case clang::tok::less:
        return Value(lhs_addr < rhs_addr);
      case clang::tok::lessequal:
        return Value(lhs_addr <= rhs_addr);
      case clang::tok::greater:
        return Value(lhs_addr > rhs_addr);
      case clang::tok::greaterequal:
        return Value(lhs_addr >= rhs_addr);
      default:
        unreachable("Invalid comparison operation.");
    }
  }

  ReportTypeError(kInvalidOperandsToBinaryExpression, lhs, rhs);
  return Value();
}

bool Interpreter::BoolConvertible(Value& val) {
  if (val.IsScalar() || val.IsPointer()) {
    return true;
  }

  ReportTypeError(
      "value of type '{0}' is not contextually convertible to 'bool'", val);
  return false;
}

void Interpreter::ReportTypeError(const char* fmt) {
  error_.Set(EvalErrorCode::INVALID_OPERAND_TYPE, fmt);
}

void Interpreter::ReportTypeError(const char* fmt, const Value& val) {
  std::string rhs_type = val.AsSbValue(target_).GetTypeName();

  auto msg = llvm::formatv(fmt, rhs_type);
  error_.Set(EvalErrorCode::INVALID_OPERAND_TYPE, msg);
}

void Interpreter::ReportTypeError(const char* fmt, const Value& lhs,
                                  const Value& rhs) {
  std::string lhs_type = lhs.AsSbValue(target_).GetTypeName();
  std::string rhs_type = rhs.AsSbValue(target_).GetTypeName();

  auto msg = llvm::formatv(fmt, lhs_type, rhs_type);
  error_.Set(EvalErrorCode::INVALID_OPERAND_TYPE, msg);
}

}  // namespace lldb_eval
