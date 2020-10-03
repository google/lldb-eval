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

#include "ast.h"

#include <cstdint>
#include <cstdio>
#include <memory>
#include <sstream>
#include <utility>
#include <variant>

#include "ast_visitor.h"

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
static const BinOpInfo BIN_OP_TABLE[] = {
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

static const char* UN_OP_TABLE[((size_t)UnOp::EnumLast) + 1] = {
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

VariableExpr::VariableExpr(std::string name) : name_(std::move(name)) {}
const std::string& VariableExpr::name() const { return name_; }

UnaryExpr::UnaryExpr(UnOp op, Expr expr)
    : expr_(std::make_unique<Expr>(std::move(expr))), op_(op) {}
UnOp UnaryExpr::op() const { return op_; }
const Expr& UnaryExpr::expr() const { return *expr_; }

IntegerConstant::IntegerConstant(uint64_t value) : value_(value) {}
uint64_t IntegerConstant::value() const { return value_; }

DoubleConstant::DoubleConstant(double value) : value_(value) {}
double DoubleConstant::value() const { return value_; }

ParenthesizedExpr::ParenthesizedExpr(Expr expr)
    : expr_(std::make_unique<Expr>(std::move(expr))) {}
const Expr& ParenthesizedExpr::expr() const { return *expr_; };

AddressOf::AddressOf(Expr expr)
    : expr_(std::make_unique<Expr>(std::move(expr))) {}
const Expr& AddressOf::expr() const { return *expr_; }

FieldAccess::FieldAccess(Expr expr, std::string field)
    : expr_(std::make_unique<Expr>(std::move(expr))),
      field_(std::move(field)) {}
const Expr& FieldAccess::expr() const { return *expr_; }
const std::string& FieldAccess::field() const { return field_; }

PtrFieldAccess::PtrFieldAccess(Expr expr, std::string field)
    : expr_(std::make_unique<Expr>(std::move(expr))),
      field_(std::move(field)) {}
const Expr& PtrFieldAccess::expr() const { return *expr_; }
const std::string& PtrFieldAccess::field() const { return field_; }

ArrayIndex::ArrayIndex(Expr expr, Expr idx)
    : expr_(std::make_unique<Expr>(std::move(expr))),
      idx_(std::make_unique<Expr>(std::move(idx))) {}
const Expr& ArrayIndex::expr() const { return *expr_; }
const Expr& ArrayIndex::idx() const { return *idx_; }

TernaryExpr::TernaryExpr(Expr cond, Expr lhs, Expr rhs)
    : cond_(std::make_unique<Expr>(std::move(cond))),
      lhs_(std::make_unique<Expr>(std::move(lhs))),
      rhs_(std::make_unique<Expr>(std::move(rhs))) {}
const Expr& TernaryExpr::cond() const { return *cond_; }
const Expr& TernaryExpr::lhs() const { return *lhs_; }
const Expr& TernaryExpr::rhs() const { return *rhs_; }

/**
 * A visitor that converts an expression into a string.
 */
class ExprPrinter final : public Visitor {
 public:
  using Visitor::visit;

  void visit(const BinaryExpr& e) override {
    visit(e.lhs());
    os_ << " " << BIN_OP_TABLE[(size_t)e.op()].symbol << " ";
    visit(e.rhs());
  }

  void visit(const VariableExpr& e) override { os_ << e.name(); }

  void visit(const IntegerConstant& e) override { os_ << e.value(); }

  void visit(const DoubleConstant& e) override { os_ << e.value(); }

  void visit(const UnaryExpr& e) override {
    os_ << UN_OP_TABLE[(size_t)e.op()];

    const auto* inner_as_unary = std::get_if<UnaryExpr>(&e.expr());
    if (inner_as_unary != nullptr) {
      // Avoid emitting cases such as `++3` or `--3`, print `+ +3` and `- -3`
      // instead.
      bool needs_space = (e.op() == UnOp::Plus || e.op() == UnOp::Neg) &&
                         e.op() == inner_as_unary->op();
      if (needs_space) {
        os_ << ' ';
      }

      visit(*inner_as_unary);
    } else {
      visit(e.expr());
    }
  }

  void visit(const ParenthesizedExpr& e) override {
    os_ << "(";
    visit(e.expr());
    os_ << ")";
  }

  void visit(const AddressOf& e) override {
    os_ << "&";

    if (std::holds_alternative<AddressOf>(e.expr())) {
      // Avoid accidentally printing e.g. `&&x`, print `& &x` instead.
      os_ << " ";
    }
    visit(e.expr());
  }

  void visit(const FieldAccess& e) override {
    visit(e.expr());
    os_ << "." << e.field();
  }

  void visit(const PtrFieldAccess& e) override {
    visit(e.expr());
    os_ << "->" << e.field();
  }

  void visit(const ArrayIndex& e) override {
    visit(e.expr());
    os_ << "[";
    visit(e.idx());
    os_ << "]";
  }

  void visit(const TernaryExpr& e) override {
    visit(e.cond());
    os_ << " ? ";
    visit(e.lhs());
    os_ << " : ";
    visit(e.rhs());
  }

  std::string to_string() const { return os_.str(); }

 private:
  std::ostringstream os_;
};

std::string stringify_expr(const Expr& expr) {
  ExprPrinter printer;
  printer.Visitor::visit(expr);

  return printer.to_string();
}

/**
 * A visitor that dumps an expression to `stdout` for debugging purposes.
 */
class ExprDumper final : public Visitor {
 public:
  using Visitor::visit;

  void visit(const BinaryExpr& e) override {
    emit_marked_indentation();

    const auto* symbol = BIN_OP_TABLE[(size_t)e.op()].symbol;
    printf("Binary expression of type `%s`:\n", symbol);

    emit_indentation();
    printf("Left-hand side:\n");
    nested_indentation_visit(e.lhs());

    emit_indentation();
    printf("Right-hand side:\n");
    nested_indentation_visit(e.rhs());
  }

  void visit(const VariableExpr& e) override {
    emit_marked_indentation();
    printf("Variable expression for identifier `%s`\n", e.name().c_str());
  }

  void visit(const IntegerConstant& e) override {
    emit_marked_indentation();
    printf("Integer constant with value `%" PRIu64 "`\n", e.value());
  }

  void visit(const DoubleConstant& e) override {
    emit_marked_indentation();
    printf("Double constant with value `%f`\n", e.value());
  }

  void visit(const UnaryExpr& e) override {
    emit_marked_indentation();
    const auto* symbol = UN_OP_TABLE[(size_t)e.op()];
    printf("Unary expression of type %s\n", symbol);

    nested_indentation_visit(e.expr());
  }

  void visit(const ParenthesizedExpr& e) override {
    emit_marked_indentation();
    printf("Parenthesized expression:\n");

    nested_indentation_visit(e.expr());
  }

  void visit(const AddressOf& e) override {
    emit_marked_indentation();
    printf("Address of:\n");

    nested_indentation_visit(e.expr());
  }

  void visit(const FieldAccess& e) override {
    emit_marked_indentation();
    printf("Field access on `%s`:\n", e.field().c_str());

    nested_indentation_visit(e.expr());
  }

  void visit(const PtrFieldAccess& e) override {
    emit_marked_indentation();
    printf("Pointer field access on `%s`:\n", e.field().c_str());

    nested_indentation_visit(e.expr());
  }

  void visit(const ArrayIndex& e) override {
    emit_marked_indentation();
    printf("Array index:\n");

    emit_indentation();
    printf("Array:\n");
    nested_indentation_visit(e.expr());

    emit_indentation();
    printf("Index:\n");
    nested_indentation_visit(e.idx());
  }

  void visit(const TernaryExpr& e) override {
    emit_marked_indentation();
    printf("Ternary expression:\n");

    emit_indentation();
    printf("Condition:\n");
    nested_indentation_visit(e.cond());

    emit_indentation();
    printf("Left-hand side:\n");
    nested_indentation_visit(e.lhs());

    emit_indentation();
    printf("Right-hand side:\n");
    nested_indentation_visit(e.rhs());
  }

 private:
  static constexpr int SPACES_PER_INDENTATION = 2;

  void emit_marked_indentation() { printf("%*s%s", indent_spaces_, "", "+ "); }
  void emit_indentation() { printf("%*s%s", indent_spaces_, "", "  "); }

  void nested_indentation_visit(const Expr& e) {
    indent_spaces_ += SPACES_PER_INDENTATION;
    visit(e);
    indent_spaces_ -= SPACES_PER_INDENTATION;
  }

 private:
  int indent_spaces_ = 0;
};

void dump_expr(const Expr& expr) {
  ExprDumper dumper;
  dumper.Visitor::visit(expr);
}

int bin_op_precedence(BinOp op) { return BIN_OP_TABLE[(size_t)op].precedence; }

}  // namespace fuzzer
