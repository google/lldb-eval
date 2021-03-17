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
#include "tools/fuzzer/symbol_table.h"

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
  NullptrConstant,
  EnumConstant,
  DereferenceExpr,
  FunctionCallExpr,
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
  NullptrType,
  EnumType,
  ArrayType,
  EnumLast = ArrayType,
};
inline constexpr size_t NUM_GEN_TYPE_KINDS = (size_t)TypeKind::EnumLast + 1;

using ExprKindMask = EnumBitset<ExprKind>;
using TypeKindMask = EnumBitset<TypeKind>;
using CastKindMask = EnumBitset<CastExpr::Kind>;

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

/*
 * Fuzzer configuration, specifies the various parameters (e.g. expression
 * weights, min/max values for integer and double constants, etc).
 */
struct GenConfig {
  // Number of expressions to generate in non-interactive mode
  int num_exprs_to_generate = 100;

  // Maximum recursion depth
  int max_depth = 8;

  // Min/max integer constant value
  uint64_t int_const_min = 0;
  uint64_t int_const_max = 1000;

  // Min/max double constant value
  double double_constant_min = 0;
  double double_constant_max = 10;

  // Probability that an expression will be wrapped with extra parentheses.
  float parenthesize_prob = 0.2f;

  // Probability that an binary pointer arithmetic generation of the form
  // `ptr + idx` or `ptr - idx`.
  float binop_gen_ptr_expr_prob = 0.2f;
  // Probability that a pointer difference expression (`ptr1 - ptr2`) will
  // be generated.
  float binop_gen_ptrdiff_expr_prob = 0.1f;
  // Probability that the operands of a binary expression or array indexing will
  // be flipped (`ptr + idx` -> `idx + ptr`, `arr[idx]` -> `idx[arr]`, etc).
  float binop_flip_operands_prob = 0.1f;
  // Probability that pointer or scoped enum types will be compared in a binary
  // expression (the alternative is to compare scalars).
  float binop_gen_ptr_or_enum_prob = 0.5;

  // `const` and `volatile` qualifiers aren't that important for expression
  // evaluation. In order to simplify cv-qualifiers constraints (e.g. casting
  // them away), this option is introduced for easier support of the
  // expression types (e.g. static_cast, reinterpret_cast). Note that these
  // expression types aren't supported if this option is enabled.
  bool cv_qualifiers_enabled = false;

  // Probabilities that a const/volatile qualifier will be generated
  // respectively.
  float const_prob = 0.3f;
  float volatile_prob = 0.05f;

  BinOpMask bin_op_mask = BinOpMask::all_set();
  UnOpMask un_op_mask = UnOpMask::all_set();

  // Mask to disable specific expression kinds. Modify this instead of setting
  // a weight to zero to disable an expression kind.
  ExprKindMask expr_kind_mask = ExprKindMask::all_set();

  // Expression kind weights. Make sure that weights are all non-zero and that
  // any non-terminal expression has a dampening factor in the range `(0, 1)`.
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
      {1.0f, 0.0f},  // ExprKind::NullptrConstant
      {1.0f, 0.0f},  // ExprKind::EnumConstant
      {1.0f, 0.1f},  // ExprKind::DereferenceExpr
      {1.0f, 0.1f},  // ExprKind::FunctionCallExpr
      {1.0f, 0.4f},  // ExprKind::CastExpr
  }};

  // Type kind weights. Make sure that weights are all non-zero and that
  // any non-terminal expression has a dampening factor in the range `(0, 1)`.
  std::array<TypeKindWeightInfo, NUM_GEN_TYPE_KINDS> type_kind_weights = {{
      {3.0f, 0.0f},  // TypeKind::ScalarType
      {1.0f, 0.0f},  // TypeKind::TaggedType
      {1.0f, 0.1f},  // TypeKind::PointerType
      {1.0f, 0.1f},  // TypeKind::VoidPointerType
      {0.2f, 0.0f},  // TypeKind::NullptrType
      {1.0f, 0.0f},  // TypeKind::EnumType
      {1.0f, 0.1f},  // TypeKind::ArrayType
  }};
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
  virtual CastExpr::Kind gen_cast_kind(const CastKindMask& mask) = 0;
  virtual ScalarType gen_scalar_type(EnumBitset<ScalarType> mask) = 0;
  virtual bool gen_boolean() = 0;
  virtual IntegerConstant gen_integer_constant(uint64_t min, uint64_t max) = 0;
  virtual DoubleConstant gen_double_constant(double min, double max) = 0;
  virtual bool gen_parenthesize(float probability) = 0;
  virtual bool gen_binop_ptr_expr(float probability) = 0;
  virtual bool gen_binop_flip_operands(float probability) = 0;
  virtual bool gen_binop_ptrdiff_expr(float probability) = 0;
  virtual bool gen_binop_ptr_or_enum(float probability) = 0;
  virtual CvQualifiers gen_cv_qualifiers(float const_prob,
                                         float volatile_prob) = 0;
  virtual VariableExpr pick_variable(
      const std::vector<std::reference_wrapper<const VariableExpr>>& vars) = 0;
  virtual TaggedType pick_tagged_type(
      const std::vector<std::reference_wrapper<const TaggedType>>& types) = 0;
  virtual Field pick_field(
      const std::vector<std::reference_wrapper<const Field>>& fields) = 0;
  virtual EnumType pick_enum_type(
      const std::vector<std::reference_wrapper<const EnumType>>& types) = 0;
  virtual EnumConstant pick_enum_literal(
      const std::vector<std::reference_wrapper<const EnumConstant>>& enums) = 0;
  virtual Function pick_function(
      const std::vector<std::reference_wrapper<const Function>>& functions) = 0;
  virtual ArrayType pick_array_type(
      const std::vector<std::reference_wrapper<const ArrayType>>& types) = 0;
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
  CastExpr::Kind gen_cast_kind(const CastKindMask& mask) override;
  ScalarType gen_scalar_type(EnumBitset<ScalarType> mask) override;
  bool gen_boolean() override;
  IntegerConstant gen_integer_constant(uint64_t min, uint64_t max) override;
  DoubleConstant gen_double_constant(double min, double max) override;
  bool gen_parenthesize(float probability) override;
  bool gen_binop_ptr_expr(float probability) override;
  bool gen_binop_flip_operands(float probability) override;
  bool gen_binop_ptrdiff_expr(float probability) override;
  bool gen_binop_ptr_or_enum(float probability) override;
  CvQualifiers gen_cv_qualifiers(float const_prob,
                                 float volatile_prob) override;
  VariableExpr pick_variable(
      const std::vector<std::reference_wrapper<const VariableExpr>>& vars)
      override;
  TaggedType pick_tagged_type(
      const std::vector<std::reference_wrapper<const TaggedType>>& types)
      override;
  Field pick_field(
      const std::vector<std::reference_wrapper<const Field>>& fields) override;
  EnumType pick_enum_type(
      const std::vector<std::reference_wrapper<const EnumType>>& types)
      override;
  EnumConstant pick_enum_literal(
      const std::vector<std::reference_wrapper<const EnumConstant>>& enums)
      override;
  Function pick_function(
      const std::vector<std::reference_wrapper<const Function>>& functions)
      override;
  ArrayType pick_array_type(
      const std::vector<std::reference_wrapper<const ArrayType>>& types)
      override;

 private:
  std::mt19937 rng_;
};

class ExprGenerator {
 public:
  ExprGenerator(std::unique_ptr<GeneratorRng> rng, GenConfig cfg,
                SymbolTable symtab)
      : rng_(std::move(rng)),
        cfg_(std::move(cfg)),
        symtab_(std::move(symtab)) {}

  std::optional<Expr> generate();

 private:
  Expr maybe_parenthesized(Expr expr);

  std::optional<Expr> gen_boolean_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_integer_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_double_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_nullptr_constant(const ExprConstraints& constraints);
  std::optional<Expr> gen_enum_constant(const ExprConstraints& constraints);
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
  std::optional<Expr> gen_function_call_expr(
      const Weights& weights, const ExprConstraints& constraints);

  std::optional<Type> gen_type(const Weights& weights,
                               const TypeConstraints& constraints,
                               bool allow_array_types = false);
  std::optional<QualifiedType> gen_qualified_type(
      const Weights& weights, const TypeConstraints& constraints,
      bool allow_array_types = false);
  std::optional<Type> gen_pointer_type(const Weights& weights,
                                       const TypeConstraints& constraints,
                                       bool allow_array_types = false);
  std::optional<Type> gen_void_pointer_type(const TypeConstraints& constraints);
  std::optional<Type> gen_tagged_type(const TypeConstraints& constraints);
  std::optional<Type> gen_scalar_type(const TypeConstraints& constraints);
  std::optional<Type> gen_enum_type(const TypeConstraints& constraints);
  std::optional<Type> gen_array_type(const TypeConstraints& constraints);
  CvQualifiers gen_cv_qualifiers();

  std::optional<Expr> gen_with_weights(const Weights& weights,
                                       const ExprConstraints& constraints);

 private:
  std::unique_ptr<GeneratorRng> rng_;
  GenConfig cfg_;
  SymbolTable symtab_;
};

}  // namespace fuzzer

#endif  // INCLUDE_EXPR_GEN_H
