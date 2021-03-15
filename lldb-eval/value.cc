// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include "lldb-eval/value.h"

#include "lldb-eval/defines.h"
#include "lldb-eval/traits.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"

namespace lldb_eval {

template <typename T>
static T ReadValue(lldb::SBValue value) {
  static_assert(std::is_scalar_v<T>, "T must be scalar");

  // For integral type just use GetValueAsUnsigned, it will get the correct
  // value regardless of the value byte size (and it also handles bitfields).
  if constexpr (std::is_integral_v<T>) {
    return static_cast<T>(value.GetValueAsUnsigned());
  }

  // For floating point type read raw bytes directly.
  if constexpr (std::is_floating_point_v<T>) {
    lldb::SBError ignore;
    T ret = 0;
    value.GetData().ReadRawData(ignore, 0, &ret, sizeof(T));
    return ret;
  }
}

template <typename T>
static T ConvertTo(lldb::SBValue value) {
  static_assert(std::is_scalar_v<T>, "T must be scalar");

  switch (value.GetType().GetCanonicalType().GetBasicType()) {
#define CASE(basic_type, builtin_type)                     \
  case basic_type: {                                       \
    return static_cast<T>(ReadValue<builtin_type>(value)); \
  }

    LLDB_TYPE_BUILTIN(CASE)
#undef CASE

    default:
      return T();
  }
}

template <typename T>
bool IsScopedEnum_V(T type) {
  // SBType::IsScopedEnumerationType was introduced in
  // https://reviews.llvm.org/D93690. If it's not available yet, fallback to the
  // "default" implementation.
  if constexpr (HAS_METHOD(T, IsScopedEnumerationType())) {
    return type.IsScopedEnumerationType();
  }
  return false;
}

template <typename T>
lldb::SBType GetEnumerationIntegerType_V(T type, lldb::SBTarget target) {
  // SBType::GetEnumerationIntegerType was introduced in
  // https://reviews.llvm.org/D93696. If it's not available yet, fallback to the
  // "default" implementation.
  if constexpr (HAS_METHOD(T, GetEnumerationIntegerType())) {
    return type.GetEnumerationIntegerType();
  } else {
    // Assume "int" by default and hope for the best.
    return target.GetBasicType(lldb::eBasicTypeInt);
  }
}

Type::Type() {}

Type::Type(const lldb::SBType& type) : lldb::SBType(type) {}

bool Type::IsScalar() { return GetTypeFlags() & lldb::eTypeIsScalar; }

bool Type::IsInteger() { return GetTypeFlags() & lldb::eTypeIsInteger; }

bool Type::IsFloat() { return GetTypeFlags() & lldb::eTypeIsFloat; }

bool Type::IsPointerToVoid() {
  return IsPointerType() &&
         GetPointeeType().GetBasicType() == lldb::eBasicTypeVoid;
}

bool Type::IsNullPtrType() {
  return GetCanonicalType().GetBasicType() == lldb::eBasicTypeNullPtr;
}

bool Type::IsSigned() { return GetTypeFlags() & lldb::eTypeIsSigned; }

bool Type::IsEnum() { return GetTypeFlags() & lldb::eTypeIsEnumeration; }

bool Type::IsScopedEnum() { return IsScopedEnum_V<lldb::SBType>(*this); }

bool Type::IsUnscopedEnum() { return IsEnum() && !IsScopedEnum(); }

bool Type::IsScalarOrUnscopedEnum() { return IsScalar() || IsUnscopedEnum(); }

bool Type::IsIntegerOrUnscopedEnum() { return IsInteger() || IsUnscopedEnum(); }

bool Type::IsRecordType() {
  return GetCanonicalType().GetTypeClass() &
         (lldb::eTypeClassClass | lldb::eTypeClassStruct |
          lldb::eTypeClassUnion);
}

bool Type::IsPromotableIntegerType() {
  // Unscoped enums are always considered as promotable, even if their
  // underlying type does not need to be promoted (e.g. "int").
  if (IsUnscopedEnum()) {
    return true;
  }

  switch (GetCanonicalType().GetBasicType()) {
    case lldb::eBasicTypeBool:
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
    case lldb::eBasicTypeUnsignedChar:
    case lldb::eBasicTypeShort:
    case lldb::eBasicTypeUnsignedShort:
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeUnsignedWChar:
    case lldb::eBasicTypeChar16:
    case lldb::eBasicTypeChar32:
      return true;

    default:
      return false;
  }
}

bool Type::IsContextuallyConvertibleToBool() {
  return IsScalar() || IsUnscopedEnum() || IsPointerType() || IsNullPtrType() ||
         IsArrayType();
}

lldb::SBType Type::GetEnumerationIntegerType(lldb::SBTarget target) {
  return GetEnumerationIntegerType_V<lldb::SBType>(*this, target);
}

bool CompareTypes(lldb::SBType lhs, lldb::SBType rhs) {
  if (lhs == rhs) {
    return true;
  }

  // TODO(werat): Figure out why the equality doesn't work sometimes. For now
  // workaround by comparing underlying types for builtins and pointers.
  lldb::BasicType lhs_basic_type = lhs.GetCanonicalType().GetBasicType();
  lldb::BasicType rhs_basic_type = rhs.GetCanonicalType().GetBasicType();
  if (lhs_basic_type != lldb::eBasicTypeInvalid &&
      lhs_basic_type == rhs_basic_type) {
    return true;
  }

  if (lhs.IsPointerType() && rhs.IsPointerType()) {
    lldb::SBType lhs_pointee = lhs.GetPointeeType().GetCanonicalType();
    lldb::SBType rhs_pointee = rhs.GetPointeeType().GetCanonicalType();
    if (CompareTypes(lhs_pointee, rhs_pointee)) {
      return true;
    }
  }

  return false;
}

bool Value::IsScalar() { return type_.IsScalar(); }

bool Value::IsInteger() { return type_.IsInteger(); }

bool Value::IsFloat() { return type_.IsFloat(); }

bool Value::IsPointer() { return type_.IsPointerType(); }

bool Value::IsNullPtrType() { return type_.IsNullPtrType(); }

bool Value::IsSigned() { return type_.GetTypeFlags() & lldb::eTypeIsSigned; }

bool Value::IsEnum() { return type_.GetTypeFlags() & lldb::eTypeIsEnumeration; }

bool Value::IsScopedEnum() { return IsScopedEnum_V<lldb::SBType>(type_); }

bool Value::IsUnscopedEnum() { return IsEnum() && !IsScopedEnum(); }

bool Value::GetBool() {
  if (IsInteger() || IsUnscopedEnum() || IsPointer()) {
    return GetInteger().getBoolValue();
  }
  if (IsFloat()) {
    return GetFloat().isNonZero();
  }
  if (type_.IsArrayType()) {
    return AddressOf().GetUInt64() != 0;
  }
  // Either invalid value, or some complex SbValue (e.g. struct or class).
  return false;
}

uint64_t Value::GetUInt64() {
  // GetValueAsUnsigned performs overflow according to the underlying type. For
  // example, if the underlying type is `int32_t` and the value is `-1`,
  // GetValueAsUnsigned will return 4294967295.
  return IsSigned() ? value_.GetValueAsSigned() : value_.GetValueAsUnsigned();
}

Value Value::AddressOf() { return Value(value_.AddressOf()); }

Value Value::Dereference() { return Value(value_.Dereference()); }

llvm::APSInt Value::GetInteger() {
  unsigned bit_width = static_cast<unsigned>(type_.GetByteSize() * CHAR_BIT);
  uint64_t value = value_.GetValueAsUnsigned();
  bool is_signed = IsSigned();

  return llvm::APSInt(llvm::APInt(bit_width, value, is_signed), !is_signed);
}

llvm::APFloat Value::GetFloat() {
  lldb::BasicType basic_type = type_.GetCanonicalType().GetBasicType();

  switch (basic_type) {
    case lldb::eBasicTypeFloat:
      return llvm::APFloat(ReadValue<float>(value_));
    case lldb::eBasicTypeDouble:
      return llvm::APFloat(ReadValue<double>(value_));
    case lldb::eBasicTypeLongDouble:
      return llvm::APFloat(ReadValue<double>(value_));
    default:
      return llvm::APFloat(NAN);
  }
}

Value Value::Clone() {
  lldb::SBData data = value_.GetData();
  lldb::SBError ignore;
  auto raw_data = std::make_unique<uint8_t[]>(data.GetByteSize());
  data.ReadRawData(ignore, 0, raw_data.get(), data.GetByteSize());
  return CreateValueFromBytes(value_.GetTarget(), raw_data.get(), type_);
}

void Value::Update(const llvm::APInt& v) {
  lldb::SBData data;
  lldb::SBError ignore;
  lldb::SBTarget target = value_.GetTarget();
  data.SetData(ignore, v.getRawData(), type_.GetByteSize(),
               target.GetByteOrder(),
               static_cast<uint8_t>(target.GetAddressByteSize()));
  value_.SetData(data, ignore);
}

Value CastScalarToBasicType(lldb::SBTarget target, Value val,
                            lldb::SBType type) {
  Value ret;

  switch (type.GetCanonicalType().GetBasicType()) {
#define CASE(basic_type, builtin_type)                   \
  case basic_type: {                                     \
    auto v = ConvertTo<builtin_type>(val.inner_value()); \
    ret = CreateValueFromBytes(target, &v, type);        \
    break;                                               \
  }

    LLDB_TYPE_BUILTIN(CASE)
#undef CASE

    default:
      // Invalid basic type, can't cast to it.
      break;
  }

  return ret;
}

Value CastEnumToBasicType(lldb::SBTarget target, Value val, lldb::SBType type) {
  Value ret;

  switch (type.GetCanonicalType().GetBasicType()) {
#define CASE(basic_type, builtin_type)                   \
  case basic_type: {                                     \
    auto v = static_cast<builtin_type>(val.GetUInt64()); \
    ret = CreateValueFromBytes(target, &v, type);        \
    break;                                               \
  }

    LLDB_TYPE_BUILTIN(CASE)
#undef CASE

    default:
      // Invalid basic type, can't cast to it.
      break;
  }

  return ret;
}

Value CastPointerToBasicType(lldb::SBTarget target, Value val,
                             lldb::SBType type) {
  Value ret;

  switch (type.GetCanonicalType().GetBasicType()) {
#define CASE(basic_type, builtin_type)                   \
  case basic_type: {                                     \
    auto v = static_cast<builtin_type>(val.GetUInt64()); \
    ret = CreateValueFromBytes(target, &v, type);        \
    break;                                               \
  }

    LLDB_TYPE_BUILTIN_INTEGRAL(CASE)
#undef CASE

    default:
      // Invalid basic type, can't cast to it.
      break;
  }

  return ret;
}

Value CreateValueFromBytes(lldb::SBTarget target, const void* bytes,
                           lldb::SBType type) {
  lldb::SBError ignore;
  lldb::SBData data;
  data.SetData(ignore, bytes, type.GetByteSize(), target.GetByteOrder(),
               static_cast<uint8_t>(target.GetAddressByteSize()));

  // CreateValueFromData copies the data referenced by `bytes` to its own
  // storage. `value` should be valid up until this point.
  return Value(
      // Force static value, otherwise we can end up with the "real" type.
      target.CreateValueFromData("result", data, type).GetStaticValue());
}

Value CreateValueFromBytes(lldb::SBTarget target, const void* bytes,
                           lldb::BasicType basic_type) {
  return CreateValueFromBytes(target, bytes, target.GetBasicType(basic_type));
}

Value CreateValueFromAPInt(lldb::SBTarget target, const llvm::APInt& v,
                           lldb::SBType type) {
  return CreateValueFromBytes(target, v.getRawData(), type);
}

Value CreateValueFromAPFloat(lldb::SBTarget target, const llvm::APFloat& v,
                             lldb::SBType type) {
  return CreateValueFromAPInt(target, v.bitcastToAPInt(), type);
}

Value CreateValueFromPointer(lldb::SBTarget target, uintptr_t addr,
                             lldb::SBType type) {
  return CreateValueFromBytes(target, &addr, type);
}

Value CreateValueFromBool(lldb::SBTarget target, bool value) {
  return CreateValueFromBytes(target, &value, lldb::eBasicTypeBool);
}

Value CreateValueNullptr(lldb::SBTarget target) {
  uintptr_t zero = 0;
  return CreateValueFromBytes(target, &zero, lldb::eBasicTypeNullPtr);
}

}  // namespace lldb_eval
