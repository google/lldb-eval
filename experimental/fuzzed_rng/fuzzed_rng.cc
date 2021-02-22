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

#include "experimental/fuzzed_rng/fuzzed_rng.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <optional>
#include <random>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>

#include "lldb-eval/defines.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/enum_bitset.h"
#include "tools/fuzzer/symbol_table.h"

namespace fuzzer {

using ScalarMask = EnumBitset<ScalarType>;

template <typename Enum>
Enum pick_nth_set_bit(const EnumBitset<Enum> mask,
                      FuzzedDataProvider& data_provider) {
  assert(mask.any() && "Mask must not be empty");

  size_t choice = data_provider.ConsumeIntegralInRange<size_t>(1, mask.count());
  size_t running_ones = 0;
  for (size_t i = 0; i < mask.size(); i++) {
    if (mask[i]) running_ones++;
    if (running_ones == choice) return (Enum)i;
  }
  lldb_eval_unreachable("Mask has no bits set");
}

bool gen_bool(float probability_true, FuzzedDataProvider& data_provider) {
  return data_provider.ConsumeProbability<float>() < probability_true;
}

template <typename T>
const T& pick_element(const std::vector<T>& vec,
                      FuzzedDataProvider& data_provider) {
  assert(!vec.empty() && "Can't pick an element out of an empty vector");
  auto choice = data_provider.ConsumeIntegralInRange<size_t>(0, vec.size() - 1);
  return vec[choice];
}

BinOp FuzzedGeneratorRng::gen_bin_op(BinOpMask mask) {
  return pick_nth_set_bit(mask, data_provider_);
}

UnOp FuzzedGeneratorRng::gen_un_op(UnOpMask mask) {
  return pick_nth_set_bit(mask, data_provider_);
}

IntegerConstant FuzzedGeneratorRng::gen_integer_constant(uint64_t min,
                                                         uint64_t max) {
  using Base = IntegerConstant::Base;
  using Length = IntegerConstant::Length;
  using Signedness = IntegerConstant::Signedness;

  auto value = data_provider_.ConsumeIntegralInRange(min, max);
  auto base = (Base)data_provider_.ConsumeIntegralInRange((int)Base::EnumFirst,
                                                          (int)Base::EnumLast);
  auto length = (Length)data_provider_.ConsumeIntegralInRange(
      (int)Length::EnumFirst, (int)Length::EnumLast);
  auto signedness = (Signedness)data_provider_.ConsumeIntegralInRange(
      (int)Signedness::EnumFirst, (int)Signedness::EnumLast);

  return IntegerConstant(value, base, length, signedness);
}

DoubleConstant FuzzedGeneratorRng::gen_double_constant(double min, double max) {
  using Format = DoubleConstant::Format;
  using Length = DoubleConstant::Length;

  auto value = data_provider_.ConsumeFloatingPointInRange(min, max);
  auto format = (Format)data_provider_.ConsumeIntegralInRange(
      (int)Format::EnumFirst, (int)Format::EnumLast);
  auto length = (Length)data_provider_.ConsumeIntegralInRange(
      (int)Length::EnumFirst, (int)Length::EnumLast);

  return DoubleConstant(value, format, length);
}

CvQualifiers FuzzedGeneratorRng::gen_cv_qualifiers(float const_prob,
                                                   float volatile_prob) {
  CvQualifiers retval;
  retval[CvQualifier::Const] = gen_bool(const_prob, data_provider_);
  retval[CvQualifier::Volatile] = gen_bool(volatile_prob, data_provider_);

  return retval;
}

VariableExpr FuzzedGeneratorRng::pick_variable(
    const std::vector<std::reference_wrapper<const VariableExpr>>& vars) {
  return pick_element(vars, data_provider_);
}

Field FuzzedGeneratorRng::pick_field(
    const std::vector<std::reference_wrapper<const Field>>& fields) {
  return pick_element(fields, data_provider_);
}

TaggedType FuzzedGeneratorRng::pick_tagged_type(
    const std::vector<std::reference_wrapper<const TaggedType>>& types) {
  return pick_element(types, data_provider_);
}

EnumType FuzzedGeneratorRng::pick_enum_type(
    const std::vector<std::reference_wrapper<const EnumType>>& types) {
  return pick_element(types, data_provider_);
}

EnumConstant FuzzedGeneratorRng::pick_enum_literal(
    const std::vector<std::reference_wrapper<const EnumConstant>>& enums) {
  return pick_element(enums, data_provider_);
}

bool FuzzedGeneratorRng::gen_binop_ptr_expr(float probability) {
  return gen_bool(probability, data_provider_);
}

bool FuzzedGeneratorRng::gen_binop_flip_operands(float probability) {
  return gen_bool(probability, data_provider_);
}

bool FuzzedGeneratorRng::gen_binop_ptrdiff_expr(float probability) {
  return gen_bool(probability, data_provider_);
}

bool FuzzedGeneratorRng::gen_binop_ptr_or_enum(float probability) {
  return gen_bool(probability, data_provider_);
}

bool FuzzedGeneratorRng::gen_parenthesize(float probability) {
  return gen_bool(probability, data_provider_);
}

bool FuzzedGeneratorRng::gen_boolean() { return data_provider_.ConsumeBool(); }

ExprKind FuzzedGeneratorRng::gen_expr_kind(const Weights&,
                                           const ExprKindMask& mask) {
  return pick_nth_set_bit(mask, data_provider_);
  // return weighted_pick(weights.expr_weights(), mask, rng_);
}

TypeKind FuzzedGeneratorRng::gen_type_kind(const Weights&,
                                           const TypeKindMask& mask) {
  return pick_nth_set_bit(mask, data_provider_);
  // return weighted_pick(weights.type_weights(), mask, rng_);
}

ScalarType FuzzedGeneratorRng::gen_scalar_type(ScalarMask mask) {
  return pick_nth_set_bit(mask, data_provider_);
}

}  // namespace fuzzer
