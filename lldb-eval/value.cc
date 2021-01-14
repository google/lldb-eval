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

#include <variant>

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

bool Value::IsScalar() {
  return type_.GetCanonicalType().GetTypeFlags() & lldb::eTypeIsScalar;
}

bool Value::IsInteger() {
  return type_.GetCanonicalType().GetTypeFlags() & lldb::eTypeIsInteger;
}

bool Value::IsFloat() {
  return type_.GetCanonicalType().GetTypeFlags() & lldb::eTypeIsFloat;
}

bool Value::IsPointer() {
  return type_.GetCanonicalType().GetTypeFlags() & lldb::eTypeIsPointer;
}

bool Value::IsPointerToVoid() {
  return type_.IsPointerType() &&
         type_.GetPointeeType().GetBasicType() == lldb::eBasicTypeVoid;
}

bool Value::IsNullPtrType() {
  return type_.GetBasicType() == lldb::eBasicTypeNullPtr;
}

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
  // Either invalid value, or some complex SbValue (e.g. struct or class).
  return false;
}

int64_t Value::GetInt64() { return value_.GetValueAsSigned(); }

uint64_t Value::GetUInt64() {
  // GetValueAsUnsigned performs overflow according to the underlying type. For
  // example, if the underlying type is `int32_t` and the value is `-1`,
  // GetValueAsUnsigned will return 4294967295.
  return IsSigned() ? value_.GetValueAsSigned() : value_.GetValueAsUnsigned();
}

Value Value::AddressOf() {
  return Value(value_.AddressOf(), /* is_rvalue */ true);
}

Value Value::Dereference() { return Value(value_.Dereference()); }

Value Value::GetRvalueRef() const {
  return Value{value_, /* is_rvalue */ true};
}

llvm::APSInt Value::GetInteger() {
  unsigned bit_width = static_cast<unsigned>(type_.GetByteSize() * CHAR_BIT);
  uint64_t value = value_.GetValueAsUnsigned();
  bool is_signed = type_.GetTypeFlags() & lldb::eTypeIsSigned;

  return llvm::APSInt(llvm::APInt(bit_width, value, is_signed), !is_signed);
}

llvm::APFloat Value::GetFloat() {
  lldb::BasicType basic_type = type_.GetCanonicalType().GetBasicType();

  switch (basic_type) {
    case lldb::eBasicTypeFloat:
      return llvm::APFloat(ReadValue<float>(value_));
    case lldb::eBasicTypeDouble:
      return llvm::APFloat(ReadValue<double>(value_));
    default:
      return llvm::APFloat(NAN);
  }
}

Value IntegralPromotion(lldb::SBTarget target, Value value) {
  // Perform itergal promotion on the operand:
  // https://eel.is/c++draft/conv.prom

  assert((value.IsInteger() || value.IsUnscopedEnum()) &&
         "Integral promotion works only for integers and unscoped enums.");

  using IntegralPromotionResult =
      std::variant<int, unsigned int, long, unsigned long, long long,
                   unsigned long long>;

  lldb::SBError error;
  IntegralPromotionResult ipr;

  // Get the value type. In case of an unscoped enumeration drill down to the
  // underlying type.
  lldb::SBType type =
      value.IsUnscopedEnum()
          ? GetEnumerationIntegerType_V<lldb::SBType>(value.type(), target)
          : value.type();

  switch (type.GetCanonicalType().GetBasicType()) {
#define CASE(basic_type, builtin_type)                  \
  case basic_type:                                      \
    ipr = ReadValue<builtin_type>(value.inner_value()); \
    break;

    LLDB_TYPE_BUILTIN_PROMOTABLE_INTEGER(CASE)
#undef CASE

    default: {
      // Unscoped enumerations need to be unwrapped into the underlying type.
      if (value.IsUnscopedEnum()) {
        uint64_t bytes = value.GetUInt64();
        return CreateValueFromBytes(target, &bytes, type);
      }
      // Other types don't need integral promotion.
      return value.GetRvalueRef();
    }
  }

  if (error) {
    // Some error happened, integral promotion result is likely invalid.
    return Value();
  }

  // Assign a value from integral promotion result.
  Value ret = std::visit(
      [target](auto&& arg) {
        return CreateValueFromBytes(
            target, &arg,
            builtin_to_lldb_type<std::decay_t<decltype(arg)>>::value);
      },
      ipr);

  return ret;
}

size_t ConversionRank(lldb::BasicType basic_type) {
  // Get integer conversion rank
  // https://eel.is/c++draft/conv.rank
  switch (basic_type) {
    case lldb::eBasicTypeBool:
      return 1;
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
    case lldb::eBasicTypeUnsignedChar:
      return 2;
    case lldb::eBasicTypeShort:
    case lldb::eBasicTypeUnsignedShort:
      return 3;
    case lldb::eBasicTypeInt:
    case lldb::eBasicTypeUnsignedInt:
      return 4;
    case lldb::eBasicTypeLong:
    case lldb::eBasicTypeUnsignedLong:
      return 5;
    case lldb::eBasicTypeLongLong:
    case lldb::eBasicTypeUnsignedLongLong:
      return 6;

      // TODO: The ranks of char16_t, char32_t, and wchar_t are equal to the
      // ranks of their underlying types.
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeUnsignedWChar:
      return 3;
    case lldb::eBasicTypeChar16:
      return 3;
    case lldb::eBasicTypeChar32:
      return 4;

      // Technically float and double don't have a conversion rank, but we give
      // them one for uniform ordering.
    case lldb::eBasicTypeFloat:
      return 1;
    case lldb::eBasicTypeDouble:
      return 2;

    default:
      break;
  }
  return 0;
}

lldb::BasicType BasicTypeToUnsigned(lldb::BasicType basic_type) {
  switch (basic_type) {
    case lldb::eBasicTypeInt:
      return lldb::eBasicTypeUnsignedInt;
    case lldb::eBasicTypeLong:
      return lldb::eBasicTypeUnsignedLong;
    case lldb::eBasicTypeLongLong:
      return lldb::eBasicTypeUnsignedLongLong;
    default:
      return basic_type;
  }
}

void PerformArithmeticConversions(lldb::SBTarget target, Value* l, Value* r) {
  if (r->IsFloat()) {
    // Convert the candidate to the appropriate floating point value.
    if (l->IsInteger()) {
      // Integral value -> floating point value.
      llvm::APFloat f = llvm::APFloat(r->GetFloat().getSemantics());
      llvm::APSInt i = l->GetInteger();
      f.convertFromAPInt(i, i.isSigned(), llvm::APFloat::rmNearestTiesToEven);

      *l = CreateValueFromAPFloat(target, f, r->type());
      return;
    }

    if (l->IsFloat()) {
      // Floating point -> floating point value.
      llvm::APFloat f = l->GetFloat();
      bool ignore;
      f.convert(r->GetFloat().getSemantics(),
                llvm::APFloat::rmNearestTiesToEven, &ignore);

      *l = CreateValueFromAPFloat(target, f, r->type());
      return;
    }
  }

  if (r->IsInteger()) {
    // if `r` is signed and `l` is unsigned, check whether it can represent all
    // of the values of the type of the `l`. If not, then promote `r` to the
    // unsigned version of its type.
    if (r->IsSigned() && !l->IsSigned()) {
      auto l_size = l->type().GetByteSize();
      auto r_size = r->type().GetByteSize();

      assert(l_size <= r_size &&
             "left value must not be larger then the right!");

      if (r_size == l_size) {
        llvm::APSInt i = r->GetInteger();
        i.setIsUnsigned(true);

        auto type = target.GetBasicType(
            BasicTypeToUnsigned(r->type().GetCanonicalType().GetBasicType()));
        *r = CreateValueFromAPInt(target, i, type);
      }
    }

    llvm::APSInt i = l->GetInteger();
    i = i.extOrTrunc(static_cast<uint32_t>(r->type().GetByteSize() * CHAR_BIT));
    i.setIsSigned(r->IsSigned());

    *l = CreateValueFromAPInt(target, i, r->type());
    return;
  }

  lldb_eval_unreachable("Invalid arithmetic type");
}

lldb::SBType UsualArithmeticConversions(lldb::SBTarget target, Value* lhs,
                                        Value* rhs) {
  // Perform usual arithmetic conversions on the operands:
  // https://eel.is/c++draft/expr.arith.conv

  // If the operand passed to an arithmetic operator is integral or unscoped
  // enumeration type, then before any other action (but after lvalue-to-rvalue
  // conversion, if applicable), the operand undergoes integral promotion.
  if (lhs->IsInteger() || lhs->IsUnscopedEnum()) {
    *lhs = IntegralPromotion(target, *lhs);
  }
  if (rhs->IsInteger() || rhs->IsUnscopedEnum()) {
    *rhs = IntegralPromotion(target, *rhs);
  }

  // Tuples are ordered element-wise, e.g. (2, 1) > (1, 2) > (1, 2). We use this
  // property to figure out the operant with overall highest rank (e.g. `float`
  // is higher than any of the integral types).
  //
  // <is_float, conversion_rank, is_unsigned>
  using Rank = std::tuple<bool, size_t, bool>;

  auto GetRank = [](Value v) {
    return Rank{v.IsFloat(),
                ConversionRank(v.type().GetCanonicalType().GetBasicType()),
                !v.IsSigned()};
  };

  auto l_rank = GetRank(*lhs);
  auto r_rank = GetRank(*rhs);

  if (l_rank < r_rank) {
    PerformArithmeticConversions(target, lhs, rhs);
  } else if (l_rank > r_rank) {
    PerformArithmeticConversions(target, rhs, lhs);
  }

  // TODO: make sure the types match.
  return lhs->type().GetCanonicalType();
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
  uint64_t bytes = val.GetUInt64();
  return CreateValueFromBytes(target, &bytes, type);
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
  lldb::SBValue v = target.CreateValueFromData("result", data, type);

  // Values created from data (i.e. _not_ acquired from LLDB) are rvalues.
  return Value(v, /* is_rvalue */ true);
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

Value CreateValueZero(lldb::SBTarget target) {
  int zero = 0;
  return CreateValueFromBytes(target, &zero, lldb::eBasicTypeInt);
}

Value CreateValueNullptr(lldb::SBTarget target) {
  uintptr_t zero = 0;
  return CreateValueFromBytes(target, &zero, lldb::eBasicTypeNullPtr);
}

}  // namespace lldb_eval
