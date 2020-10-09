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

#include "tools/fuzzer/expr_gen.h"

#include <cassert>
#include <random>
#include <variant>

#include "tools/fuzzer/ast.h"

namespace fuzzer {

struct ExprKindInfo {
  float initial_weight;
  float dampening_factor;
};

static const std::array<ExprKindInfo, NUM_EXPR_KINDS> EXPR_KIND_INFO = {{
    {1.0f, 0.0f},  // ExprKind::IntegerConstant
    {0.0f, 0.0f},  // ExprKind::DoubleConstant
    {1.0f, 0.0f},  // ExprKind::VariableExpr
    {7.0f, 0.4f},  // ExprKind::BinaryExpr
    {3.0f, 0.4f},  // ExprKind::UnaryExpr
}};

int expr_precedence(const Expr& e) {
  return std::visit([](const auto& e) { return e.precedence(); }, e);
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
  auto op = rng_->gen_bin_op();

  auto lhs = gen_with_weights(weights);
  auto rhs = gen_with_weights(weights);

  // Rules for parenthesising the left hand side:
  // 1. If the left hand side has a strictly lower precedence than ours,
  //    then we will have to emit parens.
  //    Example: We emit `(3 + 4) * 5` instead of `3 + 4 * 5`.
  // 2. If the left hand side has the same precedence as we do, then we
  //    don't have to emit any parens. This is because all lldb-eval
  //    binary operators have left-to-right associativity.
  //    Example: We do not have to emit `(3 - 4) + 5`, `3 - 4 + 5` will also
  //    do.
  auto lhs_precedence = expr_precedence(lhs);
  if (lhs_precedence > bin_op_precedence(op)) {
    lhs = ParenthesizedExpr(std::move(lhs));
  }

  // Rules for parenthesising the right hand side:
  // 1. If the right hand side has a strictly lower precedence than ours,
  //    then we will have to emit parens.
  //    Example: We emit `5 * (3 + 4)` instead of `5 * 3 + 4`.
  // 2. If the right hand side has the same precedence as we do, then we
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
  auto op = (UnOp)rng_->gen_un_op();

  if (expr_precedence(expr) > UnaryExpr::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return UnaryExpr(op, std::move(expr));
}

Expr ExprGenerator::gen_with_weights(const WeightsArray& weights) {
  WeightsArray new_weights = weights;

  auto kind = rng_->gen_expr_kind(new_weights);
  auto idx = (size_t)kind;
  new_weights[idx] *= EXPR_KIND_INFO[idx].dampening_factor;

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
      exit(1);
  }

  return maybe_parenthesized(std::move(expr));
}

Expr ExprGenerator::maybe_parenthesized(Expr expr) {
  if (rng_->gen_parenthesize()) {
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

BinOp DefaultGeneratorRng::gen_bin_op() {
  std::uniform_int_distribution<int> distr((int)BinOp::EnumFirst,
                                           (int)BinOp::EnumLast);
  return (BinOp)distr(rng_);
}

UnOp DefaultGeneratorRng::gen_un_op() {
  std::uniform_int_distribution<int> distr((int)UnOp::EnumFirst,
                                           (int)UnOp::EnumLast);
  return (UnOp)distr(rng_);
}

uint64_t DefaultGeneratorRng::gen_u64(uint64_t min, uint64_t max) {
  std::uniform_int_distribution<uint64_t> distr(min, max);
  return distr(rng_);
}

double DefaultGeneratorRng::gen_double(double min, double max) {
  std::uniform_real_distribution<double> distr(min, max);
  return distr(rng_);
}

bool DefaultGeneratorRng::gen_parenthesize(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

ExprKind DefaultGeneratorRng::gen_expr_kind(const WeightsArray& weights) {
  // No need to pull `<numeric>` just for one sum.
  float sum = 0;
  for (const auto& e : weights) {
    sum += e;
  }

  std::uniform_real_distribution<double> distr(0, sum);
  auto val = distr(rng_);

  // Dummy initialization to avoid uninitialized warnings, the loop below will
  // always set `kind`.
  ExprKind kind = ExprKind::IntegerConstant;
  float running_sum = 0;
  for (size_t i = 0; i < weights.size(); i++) {
    running_sum += weights[i];
    if (val < running_sum) {
      kind = (ExprKind)i;
      break;
    }
  }

  return kind;
}

}  // namespace fuzzer
