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
#include <optional>
#include <random>
#include <unordered_map>

#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/enum_bitset.h"

namespace fuzzer {

enum class ExprKind : unsigned char {
  EnumFirst,
  IntegerConstant = EnumFirst,
  DoubleConstant,
  VariableExpr,
  UnaryExpr,
  BinaryExpr,
  AddressOf,
  MemberOf,
  MemberOfPtr,
  ArrayIndex,
  TernaryExpr,
  BooleanConstant,
  DereferenceExpr,
  CastExpr,
  EnumLast = CastExpr,
};
inline constexpr size_t NUM_GEN_EXPR_KINDS = (size_t)ExprKind::EnumLast + 1;

enum class TypeKind : unsigned char {
  EnumFirst,
  ScalarType = EnumFirst,
  TaggedType,
  PointerType,
  VoidPointerType,
  EnumLast = VoidPointerType,
};
inline constexpr size_t NUM_GEN_TYPE_KINDS = (size_t)TypeKind::EnumLast + 1;

using ExprKindMask = EnumBitset<ExprKind>;
using TypeKindMask = EnumBitset<TypeKind>;

class Weights;
class ExprConstraints;
class TypeConstraints;

struct ExprKindWeightInfo {
  float initial_weight;
  float dampening_factor;
};

struct TypeKindWeightInfo {
  float initial_weight;
  float dampening_factor;
};

using BinOpMask = EnumBitset<BinOp>;
using UnOpMask = EnumBitset<UnOp>;

struct GenConfig {
  int num_exprs_to_generate = 8;

  int max_depth = 10;

  uint64_t int_const_min = 0;
  uint64_t int_const_max = 1000;

  double double_constant_min = 0;
  double double_constant_max = 10;

  float parenthesize_prob = 0.2f;

  float binop_gen_ptr_expr_prob = 0.2f;
  float binop_gen_ptrdiff_expr_prob = 0.1f;
  float binop_flip_operands_prob = 0.1f;

  float const_prob = 0.3f;
  float volatile_prob = 0.05f;

  BinOpMask bin_op_mask = BinOpMask::all_set();
  UnOpMask un_op_mask = UnOpMask::all_set();

  ExprKindMask expr_kind_mask = ExprKindMask::all_set();

  std::array<ExprKindWeightInfo, NUM_GEN_EXPR_KINDS> expr_kind_weights = {{
      {1.0f, 0.0f},  // ExprKind::IntegerConstant
      {2.0f, 0.0f},  // ExprKind::DoubleConstant
      {1.0f, 0.0f},  // ExprKind::VariableExpr
      {3.0f, 0.4f},  // ExprKind::UnaryExpr
      {3.0f, 0.4f},  // ExprKind::BinaryExpr
      {2.0f, 0.1f},  // ExprKind::AddressOf
      {1.0f, 0.1f},  // ExprKind::MemberOf
      {1.0f, 0.1f},  // ExprKind::MemberOfPtr
      {1.0f, 0.1f},  // ExprKind::ArrayIndex
      {1.0f, 0.1f},  // ExprKind::TernaryExpr
      {1.0f, 0.0f},  // ExprKind::BooleanConstant
      {1.0f, 0.1f},  // ExprKind::DereferenceExpr
      {1.0f, 0.4f},  // ExprKind::CastExpr
  }};

  std::array<TypeKindWeightInfo, NUM_GEN_TYPE_KINDS> type_kind_weights = {{
      {2.0f, 0.0f},  // TypeKind::ScalarType
      {1.0f, 0.0f},  // TypeKind::TaggedType
      {1.0f, 0.1f},  // TypeKind::PointerType
      {1.0f, 0.1f},  // TypeKind::VoidPointerType
  }};

  std::unordered_map<Type, std::vector<std::string>> symbol_table;
};

class GeneratorRng {
 public:
  virtual ~GeneratorRng() {}

  virtual BinOp gen_bin_op(BinOpMask mask) = 0;
  virtual UnOp gen_un_op(UnOpMask mask) = 0;
  virtual ExprKind gen_expr_kind(const Weights& weights,
                                 const ExprKindMask& mask) = 0;
  virtual TypeKind gen_type_kind(const Weights& weights,
                                 const TypeKindMask& mask) = 0;
  virtual ScalarType gen_scalar_type(EnumBitset<ScalarType> mask) = 0;
  virtual bool gen_boolean() = 0;
  virtual IntegerConstant gen_integer_constant(uint64_t min, uint64_t max) = 0;
  virtual DoubleConstant gen_double_constant(double min, double max) = 0;
  virtual bool gen_parenthesize(float probability) = 0;
  virtual bool gen_binop_ptr_expr(float probability) = 0;
  virtual bool gen_binop_flip_operands(float probability) = 0;
  virtual bool gen_binop_ptrdiff_expr(float probability) = 0;
  virtual CvQualifiers gen_cv_qualifiers(float const_prob,
                                         float volatile_prob) = 0;
};

class DefaultGeneratorRng : public GeneratorRng {
 public:
  explicit DefaultGeneratorRng(uint32_t seed) : rng_(seed) {}

  BinOp gen_bin_op(BinOpMask mask) override;
  UnOp gen_un_op(UnOpMask mask) override;
  ExprKind gen_expr_kind(const Weights& weights,
                         const ExprKindMask& mask) override;
  TypeKind gen_type_kind(const Weights& weights,
                         const TypeKindMask& mask) override;
  ScalarType gen_scalar_type(EnumBitset<ScalarType> mask) override;
  bool gen_boolean() override;
  IntegerConstant gen_integer_constant(uint64_t min, uint64_t max) override;
  DoubleConstant gen_double_constant(double min, double max) override;
  bool gen_parenthesize(float probability) override;
  bool gen_binop_ptr_expr(float probability) override;
  bool gen_binop_flip_operands(float probability) override;
  bool gen_binop_ptrdiff_expr(float probability) override;
  CvQualifiers gen_cv_qualifiers(float const_prob,
                                 float volatile_prob) override;

 private:
  std::mt19937 rng_;
};

class ExprGenerator {
 public:
  ExprGenerator(std::unique_ptr<GeneratorRng> rng, GenConfig cfg)
      : rng_(std::move(rng)), cfg_(std::move(cfg)) {}

  std::optional<Expr> generate();

 private:
  Expr maybe_parenthesized(Expr expr);

  std::optional<Expr> gen_boolean_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_integer_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_double_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_variable_expr(const ExprConstraints& constraints);
  std::optional<Expr> gen_binary_expr(const Weights& weights,
                                      const ExprConstraints& constraints);
  std::optional<Expr> gen_unary_expr(const Weights& weights,
                                     const ExprConstraints& constraints);
  std::optional<Expr> gen_ternary_expr(const Weights& weights,
                                       const ExprConstraints& constraints);
  std::optional<Expr> gen_cast_expr(const Weights& weights,
                                    const ExprConstraints& constraints);
  std::optional<Expr> gen_dereference_expr(const Weights& weights,
                                           const ExprConstraints& constraints);
  std::optional<Expr> gen_address_of_expr(const Weights& weights,
                                          const ExprConstraints& constraints);
  std::optional<Expr> gen_member_of_expr(const Weights& weights,
                                         const ExprConstraints& constraints);
  std::optional<Expr> gen_member_of_ptr_expr(
      const Weights& weights, const ExprConstraints& constraints);
  std::optional<Expr> gen_array_index_expr(const Weights& weights,
                                           const ExprConstraints& constraints);

  std::optional<Type> gen_type(const Weights& weights,
                               const TypeConstraints& constraints);
  std::optional<Type> gen_type_impl(const Weights& weights,
                                    const TypeConstraints& constraints);
  std::optional<QualifiedType> gen_qualified_type(
      const Weights& weights, const TypeConstraints& constraints);
  std::optional<Type> gen_pointer_type(const Weights& weights,
                                       const TypeConstraints& constraints);
  std::optional<Type> gen_void_pointer_type(const TypeConstraints& constraints);
  std::optional<Type> gen_tagged_type(const TypeConstraints& constraints);
  std::optional<Type> gen_scalar_type(const TypeConstraints& constraints);
  CvQualifiers gen_cv_qualifiers();

  std::optional<Expr> gen_with_weights(const Weights& weights,
                                       const ExprConstraints& constraints);

 private:
  std::unique_ptr<GeneratorRng> rng_;
  GenConfig cfg_;
};

}  // namespace fuzzer

#endif  // INCLUDE_EXPR_GEN_H
