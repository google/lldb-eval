/*
 * Copyright 2021 Google LLC
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

#ifndef INCLUDE_FUZZED_RNG_H
#define INCLUDE_FUZZED_RNG_H

#include "tools/fuzzer/expr_gen.h"
#include "tools/fuzzer/libfuzzer_utils.h"

namespace fuzzer {

class FixedGeneratorRng : public GeneratorRng {
 public:
  explicit FixedGeneratorRng(const uint8_t* data, size_t size)
      : reader_(data, size) {}

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
  bool gen_sizeof_type(float probability) override;
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

  void set_rng_callback(std::function<void(uint8_t)> callback) override {
    writer_.set_callback(std::move(callback));
  }

 private:
  LibfuzzerReader reader_;
  LibfuzzerWriter writer_;
};

}  // namespace fuzzer

#endif  // INCLUDE_FUZZED_RNG_H
