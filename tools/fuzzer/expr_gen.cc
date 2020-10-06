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

bool ExprGenerator::fifty_fifty() {
  std::bernoulli_distribution fifty_fifty_distr;
  return fifty_fifty_distr(rng_);
}

IntegerConstant ExprGenerator::gen_integer_constant(const WeightsArray&) {
  std::uniform_int_distribution<uint64_t> value_distr(0, MAX_INT_VALUE);
  auto value = value_distr(rng_);
  auto gen_parens = fifty_fifty();

  return IntegerConstant(value, gen_parens);
}

DoubleConstant ExprGenerator::gen_double_constant(const WeightsArray&) {
  std::uniform_real_distribution<double> value_distr(0.0, MAX_DOUBLE_VALUE);
  auto value = value_distr(rng_);
  auto gen_parens = fifty_fifty();

  return DoubleConstant(value, gen_parens);
}

VariableExpr ExprGenerator::gen_variable_expr(const WeightsArray&) {
  auto gen_parens = fifty_fifty();

  return VariableExpr(VAR, gen_parens);
}

BinaryExpr ExprGenerator::gen_binary_expr(const WeightsArray& weights) {
  WeightsArray new_weights = weights;

  auto idx = (size_t)ExprKind::BinaryExpr;
  new_weights[idx] *= EXPR_KIND_INFO[idx].dampening_factor;

  auto lhs = gen_with_weights(new_weights);
  auto rhs = gen_with_weights(new_weights);

  // The interval of `uniform_int_distribution` is inclusive.
  std::uniform_int_distribution<size_t> op_distr(0,
                                                 (size_t)BinOp::EnumLast - 1);
  auto op = (BinOp)op_distr(rng_);
  bool gen_parens = fifty_fifty();

  return BinaryExpr(std::move(lhs), op, std::move(rhs), gen_parens);
}

UnaryExpr ExprGenerator::gen_unary_expr(const WeightsArray& weights) {
  WeightsArray new_weights = weights;

  auto idx = (size_t)ExprKind::UnaryExpr;
  new_weights[idx] *= EXPR_KIND_INFO[idx].dampening_factor;

  auto expr = gen_with_weights(new_weights);

  std::uniform_int_distribution<size_t> op_distr(0, (size_t)UnOp::EnumLast - 1);
  auto op = (UnOp)op_distr(rng_);
  bool gen_parens = fifty_fifty();

  return UnaryExpr(op, std::move(expr), gen_parens);
}

Expr ExprGenerator::gen_with_weights(const WeightsArray& weights) {
  // No need to pull `<numeric>` just for one sum.
  float sum = 0;
  for (const auto& e : weights) {
    sum += e;
  }

  std::uniform_real_distribution<float> distr(0, sum);
  auto val = distr(rng_);

  // Make sure `kind` is always initialized.
  float running_sum = 0;
  ExprKind kind = ExprKind::IntegerConstant;
  for (size_t i = 0; i < weights.size(); i++) {
    running_sum += weights[i];
    if (val < running_sum) {
      kind = (ExprKind)i;
      break;
    }
  }

  switch (kind) {
    case ExprKind::IntegerConstant:
      return gen_integer_constant(weights);

    case ExprKind::DoubleConstant:
      return gen_double_constant(weights);

    case ExprKind::VariableExpr:
      return gen_variable_expr(weights);

    case ExprKind::BinaryExpr:
      return gen_binary_expr(weights);

    case ExprKind::UnaryExpr:
      return gen_unary_expr(weights);

    default:
      assert(false && "Unreachable");
      exit(1);
  }
}

Expr ExprGenerator::generate() {
  WeightsArray weights;
  for (size_t i = 0; i < weights.size(); i++) {
    weights[i] = EXPR_KIND_INFO[i].initial_weight;
  }

  return gen_with_weights(weights);
}

}  // namespace fuzzer
