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

#ifndef INCLUDE_EXPR_GEN_H
#define INCLUDE_EXPR_GEN_H

#include <array>
#include <cstdint>
#include <random>

#include "ast.h"

namespace fuzzer {

class ExprGenerator {
 public:
  explicit ExprGenerator(uint32_t seed) : rng_(seed) {}

  Expr generate();

 private:
  static constexpr uint64_t MAX_INT_VALUE = 10;
  static constexpr double MAX_DOUBLE_VALUE = 10.0;

  static constexpr char VAR[] = "x";

  enum class ExprKind : unsigned char {
    IntegerConstant,
    DoubleConstant,
    VariableExpr,
    BinaryExpr,
    UnaryExpr,
  };

  using WeightsArray = std::array<float, NUM_EXPR_KINDS>;

  bool fifty_fifty();
  IntegerConstant gen_integer_constant(const WeightsArray&);
  DoubleConstant gen_double_constant(const WeightsArray&);
  VariableExpr gen_variable_expr(const WeightsArray&);
  BinaryExpr gen_binary_expr(const WeightsArray&);
  UnaryExpr gen_unary_expr(const WeightsArray&);

  Expr gen_with_weights(const WeightsArray&);

 private:
  std::mt19937 rng_;
};

}  // namespace fuzzer

#endif  // INCLUDE_EXPR_GEN_H
