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
#include "lldb-eval/value.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"

namespace lldb_eval {

template <typename T>
bool Compare(BinaryOpKind kind, const T& l, const T& r) {
  switch (kind) {
    case BinaryOpKind::EQ:
      return l == r;
    case BinaryOpKind::NE:
      return l != r;
    case BinaryOpKind::LT:
      return l < r;
    case BinaryOpKind::LE:
      return l <= r;
    case BinaryOpKind::GT:
      return l > r;
    case BinaryOpKind::GE:
      return l >= r;

    default:
      assert(false && "invalid ast: invalid comparison operation");
      return false;
  }
}

// Comparison operators for llvm::APFloat were introduced in LLVM 11.
#if LLVM_VERSION_MAJOR < 11

template <>
bool Compare(BinaryOpKind kind, const llvm::APFloat& l,
             const llvm::APFloat& r) {
  switch (kind) {
    case BinaryOpKind::EQ:
      return l.compare(r) == llvm::APFloat::cmpEqual;
    case BinaryOpKind::NE:
      return l.compare(r) != llvm::APFloat::cmpEqual;
    case BinaryOpKind::LT:
      return l.compare(r) == llvm::APFloat::cmpLessThan;
    case BinaryOpKind::GT:
      return l.compare(r) == llvm::APFloat::cmpGreaterThan;

    case BinaryOpKind::LE: {
      llvm::APFloat::cmpResult result = l.compare(r);
      return result == llvm::APFloat::cmpLessThan ||
             result == llvm::APFloat::cmpEqual;
    }
    case BinaryOpKind::GE: {
      llvm::APFloat::cmpResult result = l.compare(r);
      return result == llvm::APFloat::cmpGreaterThan ||
             result == llvm::APFloat::cmpEqual;
    }

    default:
      assert(false && "invalid ast: invalid comparison operation");
      return false;
  }
}

#endif

static Value EvaluateArithmeticOpInteger(lldb::SBTarget target,
                                         BinaryOpKind kind, Value lhs,
                                         Value rhs, lldb::SBType rtype) {
  assert((lhs.IsInteger() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be integers and have the same type");

  auto wrap = [target, rtype](auto value) {
    return CreateValueFromAPInt(target, value, rtype);
  };

  auto l = lhs.GetInteger();
  auto r = rhs.GetInteger();

  switch (kind) {
    case BinaryOpKind::Add:
      return wrap(l + r);
    case BinaryOpKind::Sub:
      return wrap(l - r);
    case BinaryOpKind::Div:
      return wrap(l / r);
    case BinaryOpKind::Mul:
      return wrap(l * r);
    case BinaryOpKind::Rem:
      return wrap(l % r);
    case BinaryOpKind::And:
      return wrap(l & r);
    case BinaryOpKind::Or:
      return wrap(l | r);
    case BinaryOpKind::Xor:
      return wrap(l ^ r);
    case BinaryOpKind::Shl:
      return wrap(l.shl(r));
    case BinaryOpKind::Shr:
      return wrap(l.lshr(r));

    default:
      assert(false && "invalid ast: invalid arithmetic operation");
      return Value();
  }
}

static Value EvaluateArithmeticOpFloat(lldb::SBTarget target, BinaryOpKind kind,
                                       Value lhs, Value rhs,
                                       lldb::SBType rtype) {
  assert((lhs.IsFloat() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be floats and have the same type");

  auto wrap = [target, rtype](auto value) {
    return CreateValueFromAPFloat(target, value, rtype);
  };

  auto l = lhs.GetFloat();
  auto r = rhs.GetFloat();

  switch (kind) {
    case BinaryOpKind::Add:
      return wrap(l + r);
    case BinaryOpKind::Sub:
      return wrap(l - r);
    case BinaryOpKind::Div:
      return wrap(l / r);
    case BinaryOpKind::Mul:
      return wrap(l * r);

    default:
      assert(false && "invalid ast: invalid arithmetic operation");
      return Value();
  }
}

static Value EvaluateArithmeticOp(lldb::SBTarget target, BinaryOpKind kind,
                                  Value lhs, Value rhs, Type rtype) {
  assert((rtype.IsInteger() || rtype.IsFloat()) &&
         "invalid ast: result type must either integer or floating point");

  // Evaluate arithmetic operation for two integral values.
  if (rtype.IsInteger()) {
    return EvaluateArithmeticOpInteger(target, kind, lhs, rhs, rtype);
  }

  // Evaluate arithmetic operation for two floating point values.
  if (rtype.IsFloat()) {
    return EvaluateArithmeticOpFloat(target, kind, lhs, rhs, rtype);
  }

  return Value();
}

Value Interpreter::Eval(const AstNode* tree) {
  // Evaluate an AST.
  EvalNode(tree);
  // Return the computed result. If there was an error, it will be invalid.
  return result_;
}

Value Interpreter::EvalNode(const AstNode* node, FlowAnalysis* flow) {
  // Set up the evaluation context for the current node.
  flow_analysis_chain_.push_back(flow);
  // Traverse an AST pointed by the `node`.
  node->Accept(this);
  // Cleanup the context.
  flow_analysis_chain_.pop_back();
  // Return the computed value for convenience. The caller is responsible for
  // checking if an error occured during the evaluation.
  return result_;
}

void Interpreter::Visit(const ErrorNode*) {
  // The AST is not valid.
  result_ = Value();
}

void Interpreter::Visit(const LiteralNode* node) { result_ = node->value(); }

void Interpreter::Visit(const IdentifierNode* node) {
  Value val = node->value();

  // If value is a reference, dereference it to get to the underlying type. All
  // operations on a reference should be actually operations on the referent.
  if (val.type().IsReferenceType()) {
    // TODO(werat): LLDB canonizes the type upon a dereference. This looks like
    // a bug, but for now we need to mitigate it. Check if the resulting type is
    // incorrect and fix it up.
    lldb::SBType deref_type = val.type().GetDereferencedType();
    val = val.Dereference();

    if (val.type() != deref_type) {
      val = Value(val.inner_value().Cast(deref_type));
    }
  }

  result_ = val;
}

void Interpreter::Visit(const SizeOfNode* node) {
  lldb::SBType operand = node->operand();

  // For reference type (int&) we need to look at the referenced type.
  size_t size = operand.IsReferenceType()
                    ? operand.GetDereferencedType().GetByteSize()
                    : operand.GetByteSize();
  result_ =
      CreateValueFromBytes(target_, &size, lldb::eBasicTypeUnsignedLongLong);
}

void Interpreter::Visit(const BuiltinFunctionCallNode* node) {
  if (node->name() == "__log2") {
    assert(node->arguments().size() == 1 &&
           "invalid ast: expected exactly one argument to `__log2`");
    // Get the first (and the only) argument and evaluate it.
    auto& arg = node->arguments()[0];
    Value val = EvalNode(arg.get());
    if (!val) {
      return;
    }
    assert(val.IsInteger() &&
           "invalid ast: argument to __log2 must be an interger");

    // Use Log2_32 to match the behaviour of Visual Studio debugger.
    uint32_t ret = llvm::Log2_32(static_cast<uint32_t>(val.GetUInt64()));
    result_ = CreateValueFromBytes(target_, &ret, lldb::eBasicTypeUnsignedInt);
    return;
  }

  assert(false && "invalid ast: unknown builtin function");
  result_ = Value();
}

void Interpreter::Visit(const CStyleCastNode* node) {
  // Get the type and the value we need to cast.
  Type type = node->type();
  auto rhs = EvalNode(node->rhs());
  if (!rhs) {
    return;
  }

  switch (node->kind()) {
    case CStyleCastKind::kArithmetic: {
      assert(type.IsBasicType() &&
             "invalid ast: target type should be a basic type.");
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
      assert(type.IsEnum() &&
             "invalid ast: target type should be an enumeration.");
      uint64_t value = rhs.GetUInt64();
      result_ = CreateValueFromBytes(target_, &value, type);
      return;
    }
    case CStyleCastKind::kPointer: {
      assert(type.IsPointerType() &&
             "invalid ast: target type should be a pointer.");
      if (rhs.type().IsArrayType()) {
        rhs = rhs.AddressOf();
      }
      result_ = CreateValueFromPointer(target_, rhs.GetUInt64(), type);
      return;
    }
    case CStyleCastKind::kNullptr: {
      assert(type.IsNullPtrType() &&
             "invalid ast: target type should be a nullptr_t.");
      result_ = CreateValueNullptr(target_);
      return;
    }
    case CStyleCastKind::kReference: {
      result_ = Value(rhs.inner_value().Cast(type.GetDereferencedType()));
      return;
    }
  }

  assert(false && "invalid ast: unexpected c-style cast kind");
  result_ = Value();
}

void Interpreter::Visit(const MemberOfNode* node) {
  assert(!node->member_index().empty() && "invalid ast: member index is empty");

  // TODO(werat): Implement address-of elision for member-of:
  //
  //  &(*ptr).foo -> (ptr + foo_offset)
  //  &ptr->foo -> (ptr + foo_offset)
  //
  // This requires calculating the offset of "foo" and generally possible only
  // for members from non-virtual bases.

  Value lhs = EvalNode(node->lhs());
  if (!lhs) {
    return;
  }

  // LHS can be a pointer to value, but GetChildAtIndex works for pointers too,
  // so we don't need to dereference it explicitely. This also avoid having an
  // "ephemeral" parent Value, representing the dereferenced LHS.
  lldb::SBValue member_val = lhs.inner_value();
  for (uint32_t idx : node->member_index()) {
    // Force static value, otherwise we can end up with the "real" type.
    member_val = member_val.GetChildAtIndex(idx, lldb::eNoDynamicValues,
                                            /* can_create_synthetic */ false);
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

  assert(base.type().IsPointerType() &&
         "array subscript: base must be a pointer");
  assert(index.type().IsIntegerOrUnscopedEnum() &&
         "array subscript: index must be integer or unscoped enum");

  lldb::SBType item_type = base.type().GetPointeeType();
  lldb::addr_t base_addr = base.GetUInt64();

  // Create a pointer and add the index, i.e. "base + index".
  Value value = PointerAdd(
      CreateValueFromPointer(target_, base_addr, item_type.GetPointerType()),
      index.GetUInt64());

  // If we're in the address-of context, skip the dereference and cancel the
  // pending address-of operation as well.
  if (flow_analysis() && flow_analysis()->AddressOfIsPending()) {
    flow_analysis()->DiscardAddressOf();
    result_ = value;
  } else {
    result_ = value.Dereference();
  }
}

void Interpreter::Visit(const BinaryOpNode* node) {
  // Short-circuit logical operators.
  if (node->kind() == BinaryOpKind::LAnd || node->kind() == BinaryOpKind::LOr) {
    auto lhs = EvalNode(node->lhs());
    if (!lhs) {
      return;
    }
    assert(lhs.type().IsContextuallyConvertibleToBool() &&
           "invalid ast: must be convertible to bool");

    // For "&&" break if LHS is "false", for "||" if LHS is "true".
    bool lhs_val = lhs.GetBool();
    bool break_early =
        (node->kind() == BinaryOpKind::LAnd) ? !lhs_val : lhs_val;

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

  switch (node->kind()) {
    case BinaryOpKind::Add:
      result_ = EvaluateBinaryAddition(lhs, rhs);
      return;
    case BinaryOpKind::Sub:
      result_ = EvaluateBinarySubtraction(lhs, rhs);
      return;
    case BinaryOpKind::Mul:
      result_ = EvaluateBinaryMultiplication(lhs, rhs);
      return;
    case BinaryOpKind::Div:
      result_ = EvaluateBinaryDivision(lhs, rhs);
      return;
    case BinaryOpKind::Rem:
      result_ = EvaluateBinaryRemainder(lhs, rhs);
      return;
    case BinaryOpKind::And:
    case BinaryOpKind::Or:
    case BinaryOpKind::Xor:
    case BinaryOpKind::Shl:
    case BinaryOpKind::Shr:
      result_ = EvaluateBinaryBitwise(node->kind(), lhs, rhs);
      return;

    // Comparison operations.
    case BinaryOpKind::EQ:
    case BinaryOpKind::NE:
    case BinaryOpKind::LT:
    case BinaryOpKind::LE:
    case BinaryOpKind::GT:
    case BinaryOpKind::GE:
      result_ = EvaluateComparison(node->kind(), lhs, rhs);
      return;

    case BinaryOpKind::Assign:
      result_ = EvaluateAssignment(lhs, rhs);
      return;

    case BinaryOpKind::AddAssign:
      result_ = EvaluateBinaryAddAssign(lhs, rhs);
      return;
    case BinaryOpKind::SubAssign:
      result_ = EvaluateBinarySubAssign(lhs, rhs);
      return;
    case BinaryOpKind::MulAssign:
      result_ = EvaluateBinaryMulAssign(lhs, rhs);
      return;
    case BinaryOpKind::DivAssign:
      result_ = EvaluateBinaryDivAssign(lhs, rhs);
      return;
    case BinaryOpKind::RemAssign:
      result_ = EvaluateBinaryRemAssign(lhs, rhs);
      return;

    case BinaryOpKind::AndAssign:
    case BinaryOpKind::OrAssign:
    case BinaryOpKind::XorAssign:
    case BinaryOpKind::ShlAssign:
    case BinaryOpKind::ShrAssign:
      result_ = EvaluateBinaryBitwiseAssign(node->kind(), lhs, rhs);
      return;

    default:
      break;
  }

  // Unsupported/invalid operation.
  assert(false && "invalid ast: unexpected binary operator");
  result_ = Value();
}

void Interpreter::Visit(const UnaryOpNode* node) {
  FlowAnalysis rhs_flow(
      /* address_of_is_pending */ node->kind() == UnaryOpKind::AddrOf);

  auto rhs = EvalNode(node->rhs(), &rhs_flow);
  if (!rhs) {
    return;
  }

  switch (node->kind()) {
    case UnaryOpKind::Deref:
      result_ = EvaluateDereference(rhs);
      return;
    case UnaryOpKind::AddrOf:
      // If the address-of operation wasn't cancelled during the evaluation of
      // RHS (e.g. because of the address-of-a-dereference elision), apply it
      // here.
      if (rhs_flow.AddressOfIsPending()) {
        result_ = rhs.AddressOf();
      } else {
        result_ = rhs;
      }
      return;
    case UnaryOpKind::Plus:
      result_ = rhs;
      return;
    case UnaryOpKind::Minus:
      result_ = EvaluateUnaryMinus(rhs);
      return;
    case UnaryOpKind::LNot:
      result_ = EvaluateUnaryNegation(rhs);
      return;
    case UnaryOpKind::Not:
      result_ = EvaluateUnaryBitwiseNot(rhs);
      return;
    case UnaryOpKind::PreInc:
      result_ = EvaluateUnaryPrefixIncrement(rhs);
      return;
    case UnaryOpKind::PreDec:
      result_ = EvaluateUnaryPrefixDecrement(rhs);
      return;
    case UnaryOpKind::PostInc:
      // In postfix inc/dec the result is the original value.
      result_ = rhs.Clone();
      EvaluateUnaryPrefixIncrement(rhs);
      return;
    case UnaryOpKind::PostDec:
      // In postfix inc/dec the result is the original value.
      result_ = rhs.Clone();
      EvaluateUnaryPrefixDecrement(rhs);
      return;

    default:
      break;
  }

  // Unsupported/invalid operation.
  assert(false && "invalid ast: unexpected binary operator");
  result_ = Value();
}

void Interpreter::Visit(const TernaryOpNode* node) {
  auto cond = EvalNode(node->cond());
  if (!cond) {
    return;
  }
  assert(cond.type().IsContextuallyConvertibleToBool() &&
         "invalid ast: must be convertible to bool");

  // Pass down the flow analysis because the conditional operator is a "flow
  // control" construct -- LHS/RHS might be lvalues and eligible for some
  // optimizations (e.g. "&*" elision).
  if (cond.GetBool()) {
    result_ = EvalNode(node->lhs(), flow_analysis());
  } else {
    result_ = EvalNode(node->rhs(), flow_analysis());
  }
}

Value Interpreter::EvaluateComparison(BinaryOpKind kind, Value lhs, Value rhs) {
  // Evaluate arithmetic operation for two integral values.
  if (lhs.IsInteger() && rhs.IsInteger()) {
    bool ret = Compare(kind, lhs.GetInteger(), rhs.GetInteger());
    return CreateValueFromBool(target_, ret);
  }

  // Evaluate arithmetic operation for two floating point values.
  if (lhs.IsFloat() && rhs.IsFloat()) {
    bool ret = Compare(kind, lhs.GetFloat(), rhs.GetFloat());
    return CreateValueFromBool(target_, ret);
  }

  // Evaluate arithmetic operation for two scoped enum values.
  if (lhs.IsScopedEnum() && rhs.IsScopedEnum()) {
    bool ret = Compare(kind, lhs.GetUInt64(), rhs.GetUInt64());
    return CreateValueFromBool(target_, ret);
  }

  // Must be pointer/integer and/or nullptr comparison.
  bool ret = Compare(kind, lhs.GetUInt64(), rhs.GetUInt64());
  return CreateValueFromBool(target_, ret);
}

Value Interpreter::EvaluateDereference(Value rhs) {
  assert(rhs.type().IsPointerType() && "invalid ast: must be a pointer type");

  lldb::SBType pointer_type = rhs.type();
  lldb::addr_t base_addr = rhs.GetUInt64();

  Value value = CreateValueFromPointer(target_, base_addr, pointer_type);

  // If we're in the address-of context, skip the dereference and cancel the
  // pending address-of operation as well.
  if (flow_analysis() && flow_analysis()->AddressOfIsPending()) {
    flow_analysis()->DiscardAddressOf();
    return value;
  }

  return value.Dereference();
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

Value Interpreter::EvaluateUnaryPrefixIncrement(Value rhs) {
  assert((rhs.IsInteger() || rhs.IsFloat() || rhs.IsPointer()) &&
         "invalid ast: must be either arithmetic type or pointer");

  if (rhs.IsInteger()) {
    llvm::APSInt v = rhs.GetInteger();
    ++v;  // Do the increment.

    rhs.Update(v);
    return rhs;
  }
  if (rhs.IsFloat()) {
    llvm::APFloat v = rhs.GetFloat();
    // Do the increment.
    v = v + llvm::APFloat(v.getSemantics(), 1ULL);

    rhs.Update(v.bitcastToAPInt());
    return rhs;
  }
  if (rhs.IsPointer()) {
    uint64_t v = rhs.GetUInt64();
    v += rhs.type().GetPointeeType().GetByteSize();  // Do the increment.

    rhs.Update(llvm::APInt(64, v));
    return rhs;
  }

  return Value();
}

Value Interpreter::EvaluateUnaryPrefixDecrement(Value rhs) {
  assert((rhs.IsInteger() || rhs.IsFloat() || rhs.IsPointer()) &&
         "invalid ast: must be either arithmetic type or pointer");

  if (rhs.IsInteger()) {
    llvm::APSInt v = rhs.GetInteger();
    --v;  // Do the decrement.

    rhs.Update(v);
    return rhs;
  }
  if (rhs.IsFloat()) {
    llvm::APFloat v = rhs.GetFloat();
    // Do the decrement.
    v = v - llvm::APFloat(v.getSemantics(), 1ULL);

    rhs.Update(v.bitcastToAPInt());
    return rhs;
  }
  if (rhs.IsPointer()) {
    uint64_t v = rhs.GetUInt64();
    v -= rhs.type().GetPointeeType().GetByteSize();  // Do the decrement.

    rhs.Update(llvm::APInt(64, v));
    return rhs;
  }

  return Value();
}

Value Interpreter::EvaluateBinaryAddition(Value lhs, Value rhs) {
  // Addition of two arithmetic types.
  if (lhs.IsScalar() && rhs.IsScalar()) {
    assert(CompareTypes(lhs.type(), rhs.type()) &&
           "invalid ast: operand must have the same type");
    return EvaluateArithmeticOp(target_, BinaryOpKind::Add, lhs, rhs,
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
    return EvaluateArithmeticOp(target_, BinaryOpKind::Sub, lhs, rhs,
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

  return EvaluateArithmeticOp(target_, BinaryOpKind::Mul, lhs, rhs, lhs.type());
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

  return EvaluateArithmeticOp(target_, BinaryOpKind::Div, lhs, rhs, lhs.type());
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

  return EvaluateArithmeticOpInteger(target_, BinaryOpKind::Rem, lhs, rhs,
                                     lhs.type());
}

Value Interpreter::EvaluateBinaryBitwise(BinaryOpKind kind, Value lhs,
                                         Value rhs) {
  assert((lhs.IsInteger() && CompareTypes(lhs.type(), rhs.type())) &&
         "invalid ast: operands must be integers and have the same type");

  return EvaluateArithmeticOpInteger(target_, kind, lhs, rhs, lhs.type());
}

Value Interpreter::EvaluateAssignment(Value lhs, Value rhs) {
  assert(CompareTypes(lhs.type(), rhs.type()) &&
         "invalid ast: operands must have the same type");

  lhs.Update(rhs);
  return lhs;
}

Value Interpreter::EvaluateBinaryAddAssign(Value lhs, Value rhs) {
  Value ret;

  if (lhs.IsPointer()) {
    assert(rhs.IsInteger() && "invalid ast: rhs must be an integer");
    ret = EvaluateBinaryAddition(lhs, rhs);
  } else {
    assert(lhs.IsScalar() && "invalid ast: lhs must be an arithmetic type");
    assert(rhs.type().IsBasicType() && "invalid ast: rhs must be a basic type");
    ret = CastScalarToBasicType(target_, lhs, rhs.type());
    ret = EvaluateBinaryAddition(ret, rhs);
    ret = CastScalarToBasicType(target_, ret, lhs.type());
  }

  lhs.Update(ret);
  return lhs;
}

Value Interpreter::EvaluateBinarySubAssign(Value lhs, Value rhs) {
  Value ret;

  if (lhs.IsPointer()) {
    assert(rhs.IsInteger() && "invalid ast: rhs must be an integer");
    ret = EvaluateBinarySubtraction(lhs, rhs);
  } else {
    assert(lhs.IsScalar() && "invalid ast: lhs must be an arithmetic type");
    assert(rhs.type().IsBasicType() && "invalid ast: rhs must be a basic type");
    ret = CastScalarToBasicType(target_, lhs, rhs.type());
    ret = EvaluateBinarySubtraction(ret, rhs);
    ret = CastScalarToBasicType(target_, ret, lhs.type());
  }

  lhs.Update(ret);
  return lhs;
}

Value Interpreter::EvaluateBinaryMulAssign(Value lhs, Value rhs) {
  assert(lhs.IsScalar() && "invalid ast: lhs must be an arithmetic type");
  assert(rhs.type().IsBasicType() && "invalid ast: rhs must be a basic type");

  Value ret = CastScalarToBasicType(target_, lhs, rhs.type());
  ret = EvaluateBinaryMultiplication(ret, rhs);
  ret = CastScalarToBasicType(target_, ret, lhs.type());

  lhs.Update(ret);
  return lhs;
}

Value Interpreter::EvaluateBinaryDivAssign(Value lhs, Value rhs) {
  assert(lhs.IsScalar() && "invalid ast: lhs must be an arithmetic type");
  assert(rhs.type().IsBasicType() && "invalid ast: rhs must be a basic type");

  Value ret = CastScalarToBasicType(target_, lhs, rhs.type());
  ret = EvaluateBinaryDivision(ret, rhs);
  ret = CastScalarToBasicType(target_, ret, lhs.type());

  lhs.Update(ret);
  return lhs;
}

Value Interpreter::EvaluateBinaryRemAssign(Value lhs, Value rhs) {
  assert(lhs.IsScalar() && "invalid ast: lhs must be an arithmetic type");
  assert(rhs.type().IsBasicType() && "invalid ast: rhs must be a basic type");

  Value ret = CastScalarToBasicType(target_, lhs, rhs.type());
  ret = EvaluateBinaryRemainder(ret, rhs);
  ret = CastScalarToBasicType(target_, ret, lhs.type());

  lhs.Update(ret);
  return lhs;
}

Value Interpreter::EvaluateBinaryBitwiseAssign(BinaryOpKind kind, Value lhs,
                                               Value rhs) {
  switch (kind) {
    case BinaryOpKind::AndAssign:
      kind = BinaryOpKind::And;
      break;
    case BinaryOpKind::OrAssign:
      kind = BinaryOpKind::Or;
      break;
    case BinaryOpKind::XorAssign:
      kind = BinaryOpKind::Xor;
      break;
    case BinaryOpKind::ShlAssign:
      kind = BinaryOpKind::Shl;
      break;
    case BinaryOpKind::ShrAssign:
      kind = BinaryOpKind::Shr;
      break;
    default:
      assert(false &&
             "invalid BinaryOpKind: must be a composite assignment operation");
      break;
  }
  assert(lhs.IsScalar() && "invalid ast: lhs must be an arithmetic type");
  assert(rhs.type().IsBasicType() && "invalid ast: rhs must be a basic type");

  Value ret = CastScalarToBasicType(target_, lhs, rhs.type());
  ret = EvaluateBinaryBitwise(kind, ret, rhs);
  ret = CastScalarToBasicType(target_, ret, lhs.type());

  lhs.Update(ret);
  return lhs;
}

Value Interpreter::PointerAdd(Value lhs, int64_t offset) {
  uintptr_t addr =
      lhs.GetUInt64() + offset * lhs.type().GetPointeeType().GetByteSize();

  return CreateValueFromPointer(target_, addr, lhs.type());
}

}  // namespace lldb_eval
