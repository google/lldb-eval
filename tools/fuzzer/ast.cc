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

#include "fuzzer/ast.h"

#include <cstdint>
#include <cstdio>
#include <memory>
#include <utility>
#include <variant>

namespace fuzzer {

struct BinOpInfo {
  int precedence;
  const char* symbol;
};

/**
 * Precedence and symbol for each binary operator.
 *
 * Precedence values are taken from the following:
 * https://en.cppreference.com/w/cpp/language/operator_precedence
 */
static const BinOpInfo BIN_OP_TABLE[NUM_BIN_OPS] = {
    {6, "+"},    // BinOp::Plus
    {6, "-"},    // BinOp::Minus
    {5, "*"},    // BinOp::Mult
    {5, "/"},    // BinOp::Div
    {5, "%"},    // BinOp::Mod
    {14, "&&"},  // BinOp::LogicalAnd
    {15, "||"},  // BinOp::LogicalOr
    {11, "&"},   // BinOp::BitAnd
    {13, "|"},   // BinOp::BitOr
    {12, "^"},   // BinOp::BitXor
    {7, "<<"},   // BinOp::Shl
    {7, ">>"},   // BinOp::Shr
    {10, "=="},  // BinOp::Eq
    {10, "!="},  // BinOp::Ne
    {9, "<"},    // BinOp::Lt
    {9, "<="},   // BinOp::Le
    {9, ">"},    // BinOp::Gt
    {9, ">="},   // BinOp::Ge
};

static const char* UN_OP_TABLE[NUM_UN_OPS] = {
    "+",  // UnOp::Plus
    "-",  // UnOp::Neg
    "!",  // UnOp::LogicalNot
    "~",  // UnOp::BitNot
};

BinaryExpr::BinaryExpr(Expr lhs, BinOp op, Expr rhs)
    : lhs_(std::make_unique<Expr>(std::move(lhs))),
      rhs_(std::make_unique<Expr>(std::move(rhs))),
      op_(op) {}
const Expr& BinaryExpr::lhs() const { return *lhs_; }
const Expr& BinaryExpr::rhs() const { return *rhs_; }
BinOp BinaryExpr::op() const { return op_; }
int BinaryExpr::precedence() const {
  return BIN_OP_TABLE[(size_t)op_].precedence;
}
std::ostream& operator<<(std::ostream& os, const BinaryExpr& e) {
  const char* symbol = BIN_OP_TABLE[(size_t)e.op()].symbol;
  return os << e.lhs() << " " << symbol << " " << e.rhs();
}

VariableExpr::VariableExpr(std::string name) : name_(std::move(name)) {}
const std::string& VariableExpr::name() const { return name_; }
std::ostream& operator<<(std::ostream& os, const VariableExpr& e) {
  return os << e.name();
}

UnaryExpr::UnaryExpr(UnOp op, Expr expr)
    : expr_(std::make_unique<Expr>(std::move(expr))), op_(op) {}
UnOp UnaryExpr::op() const { return op_; }
const Expr& UnaryExpr::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const UnaryExpr& e) {
  os << UN_OP_TABLE[(size_t)e.op()];

  const auto* inner_as_unary = std::get_if<UnaryExpr>(&e.expr());
  if (inner_as_unary != nullptr) {
    // Avoid emitting cases such as `++3` or `--3`, print `+ +3` and `- -3`
    // instead.
    bool needs_space = (e.op() == UnOp::Plus || e.op() == UnOp::Neg) &&
                       e.op() == inner_as_unary->op();
    if (needs_space) {
      os << " ";
    }
  }
  return os << e.expr();
}

IntegerConstant::IntegerConstant(uint64_t value) : value_(value) {}
uint64_t IntegerConstant::value() const { return value_; }
std::ostream& operator<<(std::ostream& os, const IntegerConstant& e) {
  return os << e.value();
}

DoubleConstant::DoubleConstant(double value) : value_(value) {}
double DoubleConstant::value() const { return value_; }
std::ostream& operator<<(std::ostream& os, const DoubleConstant& e) {
  return os << e.value();
}

ParenthesizedExpr::ParenthesizedExpr(Expr expr)
    : expr_(std::make_unique<Expr>(std::move(expr))) {}
const Expr& ParenthesizedExpr::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const ParenthesizedExpr& e) {
  return os << "(" << e.expr() << ")";
}

AddressOf::AddressOf(Expr expr)
    : expr_(std::make_unique<Expr>(std::move(expr))) {}
const Expr& AddressOf::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const AddressOf& e) {
  os << "&";
  if (std::holds_alternative<AddressOf>(e.expr())) {
    // Avoid accidentally printing e.g. `&&x`, print `& &x` instead.
    os << " ";
  }
  return os << e.expr();
}

MemberOf::MemberOf(Expr expr, std::string field)
    : expr_(std::make_unique<Expr>(std::move(expr))),
      field_(std::move(field)) {}
const Expr& MemberOf::expr() const { return *expr_; }
const std::string& MemberOf::field() const { return field_; }
std::ostream& operator<<(std::ostream& os, const MemberOf& e) {
  return os << e.expr() << "." << e.field();
}

MemberOfPtr::MemberOfPtr(Expr expr, std::string field)
    : expr_(std::make_unique<Expr>(std::move(expr))),
      field_(std::move(field)) {}
const Expr& MemberOfPtr::expr() const { return *expr_; }
const std::string& MemberOfPtr::field() const { return field_; }
std::ostream& operator<<(std::ostream& os, const MemberOfPtr& e) {
  return os << e.expr() << "->" << e.field();
}

ArrayIndex::ArrayIndex(Expr expr, Expr idx)
    : expr_(std::make_unique<Expr>(std::move(expr))),
      idx_(std::make_unique<Expr>(std::move(idx))) {}
const Expr& ArrayIndex::expr() const { return *expr_; }
const Expr& ArrayIndex::idx() const { return *idx_; }
std::ostream& operator<<(std::ostream& os, const ArrayIndex& e) {
  return os << e.expr() << "[" << e.idx() << "]";
}

TernaryExpr::TernaryExpr(Expr cond, Expr lhs, Expr rhs)
    : cond_(std::make_unique<Expr>(std::move(cond))),
      lhs_(std::make_unique<Expr>(std::move(lhs))),
      rhs_(std::make_unique<Expr>(std::move(rhs))) {}
const Expr& TernaryExpr::cond() const { return *cond_; }
const Expr& TernaryExpr::lhs() const { return *lhs_; }
const Expr& TernaryExpr::rhs() const { return *rhs_; }
std::ostream& operator<<(std::ostream& os, const TernaryExpr& e) {
  return os << e.cond() << " ? " << e.lhs() << e.rhs();
}

std::ostream& operator<<(std::ostream& os, const Expr& e) {
  std::visit([&os](const auto& expr) { os << expr; }, e);
  return os;
}

/**
 * A visitor that dumps an expression to `stdout` for debugging purposes.
 */
class ExprDumper {
 public:
  void operator()(const BinaryExpr& e) {
    emit_marked_indentation();

    const auto* symbol = BIN_OP_TABLE[(size_t)e.op()].symbol;
    printf("Binary expression of type `%s`:\n", symbol);

    emit_indentation();
    printf("Left-hand side:\n");
    indented_visit(e.lhs());

    emit_indentation();
    printf("Right-hand side:\n");
    indented_visit(e.rhs());
  }

  void operator()(const VariableExpr& e) {
    emit_marked_indentation();
    printf("Variable expression for identifier `%s`\n", e.name().c_str());
  }

  void operator()(const IntegerConstant& e) {
    emit_marked_indentation();
    printf("Integer constant with value `%" PRIu64 "`\n", e.value());
  }

  void operator()(const DoubleConstant& e) {
    emit_marked_indentation();
    printf("Double constant with value `%f`\n", e.value());
  }

  void operator()(const UnaryExpr& e) {
    emit_marked_indentation();
    const auto* symbol = UN_OP_TABLE[(size_t)e.op()];
    printf("Unary expression of type %s\n", symbol);

    indented_visit(e.expr());
  }

  void operator()(const ParenthesizedExpr& e) {
    emit_marked_indentation();
    printf("Parenthesized expression:\n");

    indented_visit(e.expr());
  }

  void operator()(const AddressOf& e) {
    emit_marked_indentation();
    printf("Address of:\n");

    indented_visit(e.expr());
  }

  void operator()(const MemberOf& e) {
    emit_marked_indentation();
    printf("Field access on `%s`:\n", e.field().c_str());

    indented_visit(e.expr());
  }

  void operator()(const MemberOfPtr& e) {
    emit_marked_indentation();
    printf("Pointer field access on `%s`:\n", e.field().c_str());

    indented_visit(e.expr());
  }

  void operator()(const ArrayIndex& e) {
    emit_marked_indentation();
    printf("Array index:\n");

    emit_indentation();
    printf("Array:\n");
    indented_visit(e.expr());

    emit_indentation();
    printf("Index:\n");
    indented_visit(e.idx());
  }

  void operator()(const TernaryExpr& e) {
    emit_marked_indentation();
    printf("Ternary expression:\n");

    emit_indentation();
    printf("Condition:\n");
    indented_visit(e.cond());

    emit_indentation();
    printf("Left-hand side:\n");
    indented_visit(e.lhs());

    emit_indentation();
    printf("Right-hand side:\n");
    indented_visit(e.rhs());
  }

 private:
  static constexpr int SPACES_PER_INDENTATION = 2;

  void emit_marked_indentation() { printf("%*s%s", indent_spaces_, "", "+ "); }
  void emit_indentation() { printf("%*s%s", indent_spaces_, "", "  "); }

  void indented_visit(const Expr& e) {
    indent_spaces_ += SPACES_PER_INDENTATION;
    std::visit(*this, e);
    indent_spaces_ -= SPACES_PER_INDENTATION;
  }

 private:
  int indent_spaces_ = 0;
};

void dump_expr(const Expr& expr) { std::visit(ExprDumper(), expr); }

int bin_op_precedence(BinOp op) { return BIN_OP_TABLE[(size_t)op].precedence; }

}  // namespace fuzzer
