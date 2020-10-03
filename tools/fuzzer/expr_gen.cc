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

#include "expr_gen.h"

#include <cassert>
#include <random>
#include <variant>

#include "ast.h"
#include "ast_visitor.h"

namespace fuzzer {

struct ExprKindInfo {
  float initial_weight;
  float dampening_factor;
};

static const std::array<ExprKindInfo, NUM_EXPR_KINDS> EXPR_KIND_INFO = {{
    {1.0, 0.0},  // ExprKind::IntegerConstant
    {0.0, 0.0},  // ExprKind::DoubleConstant
    {1.0, 0.0},  // ExprKind::VariableExpr
    {7.0, 0.4},  // ExprKind::BinaryExpr
    {3.0, 0.4},  // ExprKind::UnaryExpr
}};

class PrecedenceVisitor final : public Visitor {
 public:
  using Visitor::visit;

  int precedence() const { return precedence_; }

  void visit(const UnaryExpr&) override { precedence_ = 3; }

  void visit(const BinaryExpr& e) override {
    precedence_ = bin_op_precedence(e.op());
  }

  // Merely set a precedence higher than the largest one possible
  void visit(const VariableExpr&) override { precedence_ = 0; }

  // Merely set a precedence higher than the largest one possible
  void visit(const IntegerConstant&) override { precedence_ = 0; }

  // Merely set a precedence higher than the largest one possible
  void visit(const DoubleConstant&) override { precedence_ = 0; }

  // Merely set a precedence higher than the largest one possible
  void visit(const ParenthesizedExpr&) override { precedence_ = 0; }

  void visit(const AddressOf&) override { precedence_ = 3; }

  void visit(const FieldAccess&) override { precedence_ = 2; }

  void visit(const PtrFieldAccess&) override { precedence_ = 2; }

  void visit(const ArrayIndex&) override { precedence_ = 2; }

  void visit(const TernaryExpr&) override { precedence_ = 16; }

 private:
  // Default value, large enough to be lower than
  int precedence_ = 20;
};

int expr_precedence(const Expr& e) {
  PrecedenceVisitor visitor;
  visitor.visit(e);

  return visitor.precedence();
}

IntegerConstant ExprGenerator::gen_integer_constant(const WeightsArray&) {
  auto value = rng_->gen_u64(0, MAX_INT_VALUE);

  return IntegerConstant(value);
}

DoubleConstant ExprGenerator::gen_double_constant(const WeightsArray&) {
  std::uniform_real_distribution<double> value_distr(0.0, MAX_DOUBLE_VALUE);
  auto value = rng_->gen_double(0.0, MAX_DOUBLE_VALUE);

  return DoubleConstant(value);
}

VariableExpr ExprGenerator::gen_variable_expr(const WeightsArray&) {
  return VariableExpr(VAR);
}

BinaryExpr ExprGenerator::gen_binary_expr(const WeightsArray& weights) {
  auto op = (BinOp)rng_->gen_int((int)BinOp::EnumFirst, (int)BinOp::EnumLast);

  auto lhs = gen_with_weights(weights);
  auto rhs = gen_with_weights(weights);

  // Rules for parenthesising the left hand side:
  // 1. If the left hand side is not a binary expression, then there is no need
  //    to do anything (TODO: Revise for ternary operator)
  // 2. If the left hand side has a strictly lower precedence than ours,
  //    then we will have to emit parens.
  //    Example: We emit `(3 + 4) * 5` instead of `3 + 4 * 5`.
  // 3. If the left hand side has the same precedence as we do, then we
  //    don't have to emit any parens. This is because all lldb-eval
  //    binary operators have left-to-right associativity.
  //    Example: We do not have to emit `(3 - 4) + 5`, `3 - 4 + 5` will also
  //    do.
  auto lhs_precedence = expr_precedence(lhs);
  if (lhs_precedence > bin_op_precedence(op)) {
    lhs = ParenthesizedExpr(std::move(lhs));
  }

  // Rules for parenthesising the right hand side:
  // 1. If the right hand side is not a binary expression, then there is no need
  //    to do anything (TODO: Revise for ternary operator)
  // 2. If the right hand side has a strictly lower precedence than ours,
  //    then we will have to emit parens.
  //    Example: We emit `5 * (3 + 4)` instead of `5 * 3 + 4`.
  // 3. If the right hand side has the same precedence as we do, then we
  //    should emit parens for good measure. This is because all lldb-eval
  //    binary operators have left-to-right associativity and we do not
  //    want to violate this with respect to the generated AST.
  //    Example: We emit `3 - (4 + 5)` instead of `3 - 4 + 5`. We also
  //    emit `3 + (4 + 5)` instead of `3 + 4 + 5`, even though both
  //    expressions are equivalent.
  auto rhs_precedence = expr_precedence(rhs);
  if (rhs_precedence >= bin_op_precedence(op)) {
    rhs = ParenthesizedExpr(std::move(rhs));
  }

  return BinaryExpr(std::move(lhs), op, std::move(rhs));
}

UnaryExpr ExprGenerator::gen_unary_expr(const WeightsArray& weights) {
  auto expr = gen_with_weights(weights);
  auto op = (UnOp)rng_->gen_int((int)UnOp::EnumFirst, (int)UnOp::EnumLast);

  constexpr int UNARY_PRECEDENCE = 3;

  if (expr_precedence(expr) > UNARY_PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return UnaryExpr(op, std::move(expr));
}

Expr ExprGenerator::gen_with_weights(const WeightsArray& weights) {
  WeightsArray new_weights = weights;

  // No need to pull `<numeric>` just for one sum.
  float sum = 0;
  for (const auto& e : new_weights) {
    sum += e;
  }

  auto val = rng_->gen_double(0, sum);

  float running_sum = 0;
  // Make sure `kind` is always initialized.
  ExprKind kind = ExprKind::IntegerConstant;
  for (size_t i = 0; i < new_weights.size(); i++) {
    running_sum += new_weights[i];
    if (val < running_sum) {
      kind = (ExprKind)i;
      new_weights[i] *= EXPR_KIND_INFO[i].dampening_factor;
      break;
    }
  }

  // Dummy value for initialization
  Expr expr(IntegerConstant(0));
  switch (kind) {
    case ExprKind::IntegerConstant:
      expr = gen_integer_constant(new_weights);
      break;

    case ExprKind::DoubleConstant:
      expr = gen_double_constant(new_weights);
      break;

    case ExprKind::VariableExpr:
      expr = gen_variable_expr(new_weights);
      break;

    case ExprKind::BinaryExpr:
      expr = gen_binary_expr(new_weights);
      break;

    case ExprKind::UnaryExpr:
      expr = gen_unary_expr(new_weights);
      break;

    default:
      assert(false && "Unreachable");
  }

  return maybe_parenthesized(std::move(expr));
}

Expr ExprGenerator::maybe_parenthesized(Expr expr) {
  bool parenthesize = rng_->gen_bernoulli();
  if (parenthesize) {
    return ParenthesizedExpr(std::move(expr));
  }

  return expr;
}

Expr ExprGenerator::generate() {
  WeightsArray weights;
  for (size_t i = 0; i < weights.size(); i++) {
    weights[i] = EXPR_KIND_INFO[i].initial_weight;
  }

  return gen_with_weights(weights);
}

int DefaultGeneratorRng::gen_int(int min, int max) {
  std::uniform_int_distribution<int> distr(min, max);
  return distr(rng_);
}

uint64_t DefaultGeneratorRng::gen_u64(uint64_t min, uint64_t max) {
  std::uniform_int_distribution<uint64_t> distr(min, max);
  return distr(rng_);
}

double DefaultGeneratorRng::gen_double(double min, double max) {
  std::uniform_real_distribution<double> distr(min, max);
  return distr(rng_);
}

bool DefaultGeneratorRng::gen_bernoulli(float p) {
  std::bernoulli_distribution distr(p);
  return distr(rng_);
}

}  // namespace fuzzer
