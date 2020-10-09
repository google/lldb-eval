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

#include "tools/fuzzer/ast.h"

namespace fuzzer {

enum class ExprKind : unsigned char {
  IntegerConstant,
  DoubleConstant,
  VariableExpr,
  UnaryExpr,
  BinaryExpr,
  AddressOf,
  MemberOf,
  MemberOfPtr,
  ArrayIndex,
  TernaryExpr,
  EnumLast = TernaryExpr,
};
constexpr size_t NUM_GEN_EXPR_KINDS = (size_t)ExprKind::EnumLast + 1;

using WeightsArray = std::array<float, NUM_GEN_EXPR_KINDS>;

class GeneratorRng {
 public:
  virtual ~GeneratorRng() {}

  virtual BinOp gen_bin_op() = 0;
  virtual UnOp gen_un_op() = 0;
  virtual ExprKind gen_expr_kind(const WeightsArray& array) = 0;
  virtual uint64_t gen_u64(uint64_t min, uint64_t max) = 0;
  virtual double gen_double(double min, double max) = 0;
  virtual bool gen_parenthesize(float probability = 0.5) = 0;
};

class DefaultGeneratorRng : public GeneratorRng {
 public:
  explicit DefaultGeneratorRng(uint32_t seed) : rng_(seed) {}

  BinOp gen_bin_op() override;
  UnOp gen_un_op() override;
  ExprKind gen_expr_kind(const WeightsArray& array) override;
  uint64_t gen_u64(uint64_t min, uint64_t max) override;
  double gen_double(double min, double max) override;
  bool gen_parenthesize(float probability = 0.5) override;

 private:
  std::mt19937 rng_;
};

class ExprGenerator {
 public:
  explicit ExprGenerator(std::unique_ptr<GeneratorRng> rng)
      : rng_(std::move(rng)) {}

  Expr generate();

 private:
  static constexpr uint64_t MAX_INT_VALUE = 10;
  static constexpr double MAX_DOUBLE_VALUE = 10.0;

  static constexpr char VAR[] = "x";

  Expr maybe_parenthesized(Expr expr);

  IntegerConstant gen_integer_constant(const WeightsArray&);
  DoubleConstant gen_double_constant(const WeightsArray&);
  VariableExpr gen_variable_expr(const WeightsArray&);
  BinaryExpr gen_binary_expr(const WeightsArray&);
  UnaryExpr gen_unary_expr(const WeightsArray&);

  Expr gen_with_weights(const WeightsArray&);

 private:
  std::unique_ptr<GeneratorRng> rng_;
};

}  // namespace fuzzer

#endif  // INCLUDE_EXPR_GEN_H
