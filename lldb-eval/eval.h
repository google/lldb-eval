/*
 * Copyright 2020 Google LLC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#ifndef LLDB_EVAL_EVAL_H_
#define LLDB_EVAL_EVAL_H_

#include <memory>
#include <vector>

#include "clang/Basic/TokenKinds.h"
#include "lldb-eval/ast.h"
#include "lldb-eval/context.h"
#include "lldb-eval/defines.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBValue.h"

namespace lldb_eval {

class FlowAnalysis {
 public:
  FlowAnalysis(bool address_of_is_pending)
      : address_of_is_pending_(address_of_is_pending) {}

  bool AddressOfIsPending() const { return address_of_is_pending_; }
  void DiscardAddressOf() { address_of_is_pending_ = false; }

 private:
  bool address_of_is_pending_;
};

class Interpreter : Visitor {
 public:
  explicit Interpreter(std::shared_ptr<Context> ctx) : ctx_(std::move(ctx)) {
    target_ = ctx_->GetExecutionContext().GetTarget();
  }

 public:
  Value Eval(const AstNode* tree, Error& error);

 private:
  void SetError(ErrorCode error_code, std::string error,
                clang::SourceLocation loc);

  void Visit(const ErrorNode* node) override;
  void Visit(const LiteralNode* node) override;
  void Visit(const IdentifierNode* node) override;
  void Visit(const SizeOfNode* node) override;
  void Visit(const BuiltinFunctionCallNode* node) override;
  void Visit(const CStyleCastNode* node) override;
  void Visit(const MemberOfNode* node) override;
  void Visit(const ArraySubscriptNode* node) override;
  void Visit(const BinaryOpNode* node) override;
  void Visit(const UnaryOpNode* node) override;
  void Visit(const TernaryOpNode* node) override;

  Value EvalNode(const AstNode* node, FlowAnalysis* flow = nullptr);

  Value EvaluateComparison(BinaryOpKind kind, Value lhs, Value rhs);

  Value EvaluateDereference(Value rhs);

  Value EvaluateUnaryMinus(Value rhs);
  Value EvaluateUnaryNegation(Value rhs);
  Value EvaluateUnaryBitwiseNot(Value rhs);
  Value EvaluateUnaryPrefixIncrement(Value rhs);
  Value EvaluateUnaryPrefixDecrement(Value rhs);

  Value EvaluateBinaryAddition(Value lhs, Value rhs);
  Value EvaluateBinarySubtraction(Value lhs, Value rhs);
  Value EvaluateBinaryMultiplication(Value lhs, Value rhs);
  Value EvaluateBinaryDivision(Value lhs, Value rhs);
  Value EvaluateBinaryRemainder(Value lhs, Value rhs);
  Value EvaluateBinaryBitwise(BinaryOpKind kind, Value lhs, Value rhs);
  Value EvaluateBinaryShift(BinaryOpKind kind, Value lhs, Value rhs);

  Value EvaluateAssignment(Value lhs, Value rhs);
  Value EvaluateBinaryAddAssign(Value lhs, Value rhs);
  Value EvaluateBinarySubAssign(Value lhs, Value rhs);
  Value EvaluateBinaryMulAssign(Value lhs, Value rhs);
  Value EvaluateBinaryDivAssign(Value lhs, Value rhs);
  Value EvaluateBinaryRemAssign(Value lhs, Value rhs);
  Value EvaluateBinaryBitwiseAssign(BinaryOpKind kind, Value lhs, Value rhs);
  Value EvaluateBinaryShiftAssign(BinaryOpKind kind, Value lhs, Value rhs,
                                  Type comp_assign_type);

  Value PointerAdd(Value lhs, int64_t offset);

  FlowAnalysis* flow_analysis() { return flow_analysis_chain_.back(); }

 private:
  // Interpreter doesn't own the evaluation context. The expression is evaluated
  // in the given context and the produced result may depend on it.
  std::shared_ptr<Context> ctx_;

  // Convenience references, used by the interpreter to lookup variables and
  // types, create objects, perform casts, etc.
  lldb::SBTarget target_;

  // Flow analysis chain represents the expression evaluation flow for the
  // current code branch. Each node in the chain corresponds to an AST node,
  // describing the semantics of the evaluation for it. Currently, flow analysis
  // propagates the information about the pending address-of operator, so that
  // combination of address-of and a subsequent dereference can be eliminated.
  // End of the chain (i.e. `back()`) contains the flow analysis instance for
  // the current node. It may be `nullptr` if no relevant information is
  // available, the caller/user is supposed to check.
  std::vector<FlowAnalysis*> flow_analysis_chain_;

  Value result_;

  Error error_;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_EVAL_H_
