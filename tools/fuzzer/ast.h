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

#ifndef INCLUDE_AST_H
#define INCLUDE_AST_H

#include <cinttypes>
#include <memory>
#include <string>
#include <variant>

namespace fuzzer {

class BinaryExpr;
class UnaryExpr;
class VariableExpr;
class IntegerConstant;
class DoubleConstant;

enum class UnOp : unsigned char {
  Plus,
  Neg,
  LogicalNot,
  BitNot,
  EnumLast = BitNot,
};

enum class BinOp : unsigned char {
  // Arithmetic operators
  Plus,
  Minus,
  Mult,
  Div,
  Mod,
  // Logical operators
  LogicalAnd,
  LogicalOr,
  // Bitwise operators
  BitAnd,
  BitOr,
  BitXor,
  Shl,
  Shr,
  // Comparison operators
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
  // Used to determine the last enum element
  EnumLast = Ge,
};

using Expr = std::variant<IntegerConstant, DoubleConstant, VariableExpr,
                          BinaryExpr, UnaryExpr>;
constexpr size_t NUM_EXPR_KINDS = std::variant_size_v<Expr>;

class BinaryExpr {
 public:
  BinaryExpr(Expr lhs, BinOp op, Expr rhs, bool gen_parens);

  const Expr& lhs() const;
  const Expr& rhs() const;
  BinOp op() const;
  bool gen_parens() const;
  int precedence() const;

 private:
  std::unique_ptr<Expr> lhs_;
  std::unique_ptr<Expr> rhs_;
  BinOp op_;
  bool gen_parens_;
};

class UnaryExpr {
 public:
  UnaryExpr(UnOp op, Expr expr, bool gen_parens);

  UnOp op() const;
  const Expr& expr() const;
  bool gen_parens() const;

 private:
  std::unique_ptr<Expr> expr_;
  UnOp op_;
  bool gen_parens_;
};

class VariableExpr {
 public:
  VariableExpr(std::string name, bool gen_parens);

  const std::string& name() const;
  bool gen_parens() const;

 private:
  std::string name_;
  bool gen_parens_;
};

class IntegerConstant {
 public:
  IntegerConstant(uint64_t value, bool gen_parens);

  uint64_t value() const;
  bool gen_parens() const;

 private:
  uint64_t value_;
  bool gen_parens_;
};

class DoubleConstant {
 public:
  DoubleConstant(double value, bool gen_parens);

  double value() const;
  bool gen_parens() const;

 private:
  double value_;
  bool gen_parens_;
};

std::string stringify_expr(const Expr& expr);
void dump_expr(const Expr& expr);

}  // namespace fuzzer

#endif  // INCLUDE_AST_H
