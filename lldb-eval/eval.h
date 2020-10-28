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

#include "clang/Basic/TokenKinds.h"
#include "lldb-eval/ast.h"
#include "lldb-eval/defines.h"
#include "lldb-eval/expression_context.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBValue.h"

namespace lldb_eval {

enum class EvalErrorCode {
  OK = 0,
  INVALID_EXPRESSION_SYNTAX,
  INVALID_OPERAND_TYPE,
  UNDECLARED_IDENTIFIER,
  NOT_IMPLEMENTED,
  UNKNOWN,
};

class EvalError {
 public:
  EvalError();

  void Set(EvalErrorCode code, const std::string& message);
  void Clear();

  EvalErrorCode code() const;
  const std::string& message() const;

  explicit operator bool() const;

 private:
  EvalErrorCode code_;
  std::string message_;
};

class Interpreter : Visitor {
 public:
  explicit Interpreter(ExpressionContext& expr_ctx) : expr_ctx_(&expr_ctx) {
    target_ = expr_ctx_->GetExecutionContext().GetTarget();
    frame_ = expr_ctx_->GetExecutionContext().GetFrame();
  }

 public:
  Value Eval(const AstNode* tree, EvalError& error);

 private:
  void Visit(const ErrorNode* node) override;

  void Visit(const BooleanLiteralNode* node) override;

  void Visit(const NumericLiteralNode* node) override;

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
  // Interpreter doesn't own expression context. The expression is evaluated in
  // the given context and the produced result may depend on it.
  ExpressionContext* expr_ctx_;

  // Convenience references, used by the interpreter to lookup variables and
  // types, create objects, perform casts, etc.
  lldb::SBTarget target_;
  lldb::SBFrame frame_;

  Value result_;
  EvalError error_;
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
