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

#include "tools/fuzzer/fixed_rng.h"

#include "tools/fuzzer/constraints.h"

namespace fuzzer {

namespace {

template <typename Enum>
void write_enum(LibfuzzerWriter& writer, Enum value) {
  writer.write_int((int)value, (int)Enum::EnumFirst, (int)Enum::EnumLast);
}

template <typename Enum>
Enum read_enum(LibfuzzerReader& reader) {
  return (Enum)reader.read_int((int)Enum::EnumFirst, (int)Enum::EnumLast);
}

bool gen_and_record_bool(LibfuzzerReader& reader, LibfuzzerWriter& writer) {
  bool value = reader.read_bool();
  writer.write_bool(value);
  return value;
}

template <typename Enum>
Enum pick_enum_from_mask(const EnumBitset<Enum> mask, LibfuzzerReader& reader,
                         LibfuzzerWriter& writer) {
  // At least one bit needs to be set
  assert(mask.any() && "Mask must not be empty");

  Enum result = read_enum<Enum>(reader);

  // Recover mechanism.
  while (!mask[(int)result]) {
    if (result == Enum::EnumLast) {
      result = Enum::EnumFirst;
    } else {
      result = (Enum)((int)result + 1);
    }
  }

  write_enum(writer, result);

  return result;
}

template <typename T>
const T& pick_element(const std::vector<T>& vec, LibfuzzerReader& reader,
                      LibfuzzerWriter& writer) {
  assert(!vec.empty() && "Can't pick an element out of an empty vector");

  auto choice = reader.read_int<size_t>(0, vec.size() - 1);
  writer.write_int<size_t>(choice, 0, vec.size() - 1);

  return vec[choice];
}

}  // namespace

BinOp FixedGeneratorRng::gen_bin_op(BinOpMask mask) {
  return pick_enum_from_mask(mask, reader_, writer_);
}

UnOp FixedGeneratorRng::gen_un_op(UnOpMask mask) {
  return pick_enum_from_mask(mask, reader_, writer_);
}

IntegerConstant FixedGeneratorRng::gen_integer_constant(uint64_t min,
                                                        uint64_t max) {
  using Base = IntegerConstant::Base;
  using Length = IntegerConstant::Length;
  using Signedness = IntegerConstant::Signedness;

  uint64_t value = reader_.read_int(min, max);
  writer_.write_int(value, min, max);

  auto base = read_enum<Base>(reader_);
  auto length = read_enum<Length>(reader_);
  auto signedness = read_enum<Signedness>(reader_);

  write_enum(writer_, base);
  write_enum(writer_, length);
  write_enum(writer_, signedness);

  return IntegerConstant(value, base, length, signedness);
}

DoubleConstant FixedGeneratorRng::gen_double_constant(double min, double max) {
  using Format = DoubleConstant::Format;
  using Length = DoubleConstant::Length;

  double value = reader_.read_float(min, max);
  writer_.write_float(value, min, max);

  auto format = read_enum<Format>(reader_);
  auto length = read_enum<Length>(reader_);

  write_enum(writer_, format);
  write_enum(writer_, length);

  return DoubleConstant(value, format, length);
}

CvQualifiers FixedGeneratorRng::gen_cv_qualifiers(float /*const_prob*/,
                                                  float /*volatile_prob*/) {
  CvQualifiers retval;
  retval[CvQualifier::Const] = reader_.read_bool();
  retval[CvQualifier::Volatile] = reader_.read_bool();

  return retval;
}

VariableExpr FixedGeneratorRng::pick_variable(
    const std::vector<std::reference_wrapper<const VariableExpr>>& vars) {
  return pick_element(vars, reader_, writer_);
}

Field FixedGeneratorRng::pick_field(
    const std::vector<std::reference_wrapper<const Field>>& fields) {
  return pick_element(fields, reader_, writer_);
}

TaggedType FixedGeneratorRng::pick_tagged_type(
    const std::vector<std::reference_wrapper<const TaggedType>>& types) {
  return pick_element(types, reader_, writer_);
}

EnumType FixedGeneratorRng::pick_enum_type(
    const std::vector<std::reference_wrapper<const EnumType>>& types) {
  return pick_element(types, reader_, writer_);
}

EnumConstant FixedGeneratorRng::pick_enum_literal(
    const std::vector<std::reference_wrapper<const EnumConstant>>& enums) {
  return pick_element(enums, reader_, writer_);
}

Function FixedGeneratorRng::pick_function(
    const std::vector<std::reference_wrapper<const Function>>& functions) {
  return pick_element(functions, reader_, writer_);
}

ArrayType FixedGeneratorRng::pick_array_type(
    const std::vector<std::reference_wrapper<const ArrayType>>& types) {
  return pick_element(types, reader_, writer_);
}

bool FixedGeneratorRng::gen_binop_ptr_expr(float) {
  return gen_and_record_bool(reader_, writer_);
}

bool FixedGeneratorRng::gen_binop_flip_operands(float) {
  return gen_and_record_bool(reader_, writer_);
}

bool FixedGeneratorRng::gen_binop_ptrdiff_expr(float) {
  return gen_and_record_bool(reader_, writer_);
}

bool FixedGeneratorRng::gen_binop_ptr_or_enum(float) {
  return gen_and_record_bool(reader_, writer_);
}

bool FixedGeneratorRng::gen_sizeof_type(float) {
  return gen_and_record_bool(reader_, writer_);
}

bool FixedGeneratorRng::gen_parenthesize(float) {
  return gen_and_record_bool(reader_, writer_);
}

bool FixedGeneratorRng::gen_boolean() {
  return gen_and_record_bool(reader_, writer_);
}

ExprKind FixedGeneratorRng::gen_expr_kind(const Weights&,
                                          const ExprKindMask& mask) {
  return pick_enum_from_mask(mask, reader_, writer_);
}

TypeKind FixedGeneratorRng::gen_type_kind(const Weights&,
                                          const TypeKindMask& mask) {
  return pick_enum_from_mask(mask, reader_, writer_);
}

CastExpr::Kind FixedGeneratorRng::gen_cast_kind(const CastKindMask& mask) {
  return pick_enum_from_mask(mask, reader_, writer_);
}

ScalarType FixedGeneratorRng::gen_scalar_type(ScalarMask mask) {
  return pick_enum_from_mask(mask, reader_, writer_);
}

}  // namespace fuzzer
