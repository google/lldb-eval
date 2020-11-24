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

class Interpreter : Visitor {
 public:
  explicit Interpreter(std::shared_ptr<Context> ctx) : ctx_(std::move(ctx)) {
    target_ = ctx_->GetExecutionContext().GetTarget();
  }

 public:
  Value Eval(const AstNode* tree, Error& error);

 private:
  void Visit(const ErrorNode* node) override;

  void Visit(const LiteralNode* node) override;

  void Visit(const IdentifierNode* node) override;

  void Visit(const CStyleCastNode* node) override;

  void Visit(const MemberOfNode* node) override;

  void Visit(const BinaryOpNode* node) override;

  void Visit(const UnaryOpNode* node) override;

  void Visit(const TernaryOpNode* node) override;

 private:
  Value EvalNode(const AstNode* node);

  Value EvaluateSubscript(Value lhs, Value rhs);
  Value EvaluateComparison(clang::tok::TokenKind op, Value lhs, Value rhs);

  Value EvaluateUnaryPlus(Value rhs);
  Value EvaluateUnaryMinus(Value rhs);
  Value EvaluateUnaryNegation(Value rhs);
  Value EvaluateUnaryBitwiseNot(Value rhs);

  Value EvaluateBinaryAddition(Value lhs, Value rhs);
  Value EvaluateBinarySubtraction(Value lhs, Value rhs);
  Value EvaluateBinaryMultiplication(Value lhs, Value rhs);
  Value EvaluateBinaryDivision(Value lhs, Value rhs);
  Value EvaluateBinaryRemainder(Value lhs, Value rhs);

  Value EvaluateBinaryBitAnd(Value lhs, Value rhs);
  Value EvaluateBinaryBitOr(Value lhs, Value rhs);
  Value EvaluateBinaryBitXor(Value lhs, Value rhs);
  Value EvaluateBinaryBitShl(Value lhs, Value rhs);
  Value EvaluateBinaryBitShr(Value lhs, Value rhs);

  bool BoolConvertible(Value val);

  void ReportTypeError(const char* fmr);
  void ReportTypeError(const char* fmt, Value val);
  void ReportTypeError(const char* fmt, Value lhs, Value rhs);

  Value PointerAdd(Value lhs, int64_t offset);

 private:
  // Interpreter doesn't own the evaluation context. The expression is evaluated
  // in the given context and the produced result may depend on it.
  std::shared_ptr<Context> ctx_;

  // Convenience references, used by the interpreter to lookup variables and
  // types, create objects, perform casts, etc.
  lldb::SBTarget target_;

  Value result_;
  Error error_;
};

enum class ArithmeticOp {
  ADD,
  SUB,
  DIV,
  MUL,
  REM,
  BIT_AND,
  BIT_OR,
  BIT_XOR,
  BIT_SHL,
  BIT_SHR,
};

Value EvaluateArithmeticOp(lldb::SBTarget target, ArithmeticOp op, Value lhs,
                           Value rhs, lldb::SBType rtype);

}  // namespace lldb_eval

#endif  // LLDB_EVAL_EVAL_H_
