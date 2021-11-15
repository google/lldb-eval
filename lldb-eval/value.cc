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

#include "lldb-eval/context.h"
#include "lldb-eval/defines.h"
#include "lldb-eval/traits.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"
#include "llvm/Support/Regex.h"

namespace lldb_eval {

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
TypeSP GetEnumerationIntegerType_V(T type, Context& ctx) {
  // SBType::GetEnumerationIntegerType was introduced in
  // https://reviews.llvm.org/D93696. If it's not available yet, fallback to the
  // "default" implementation.
  if constexpr (HAS_METHOD(T, GetEnumerationIntegerType())) {
    return LLDBType::CreateSP(type.GetEnumerationIntegerType());
  } else {
    // Assume "int" by default and hope for the best.
    return ctx.GetBasicType(lldb::eBasicTypeInt);
  }
}

template <typename T>
bool IsEnumerationIntegerTypeSigned_V(T type) {
  // SBType::GetEnumerationIntegerType was introduced in
  // https://reviews.llvm.org/D93696. If it's not available yet, fallback to the
  // "default" implementation.
  if constexpr (HAS_METHOD(T, GetEnumerationIntegerType())) {
    return type.GetEnumerationIntegerType().GetTypeFlags() &
           lldb::eTypeIsSigned;
  } else {
    // Assume "int" by default and hope for the best.
    return true;
  }
}

static uint64_t GetValueAsUnsigned(lldb::SBValue& value) {
  uint64_t ret = value.GetValueAsUnsigned();

  // Workaround for reading values of boolean bitfields. Not necessary if
  // https://reviews.llvm.org/D102685 is available.
  if (value.GetType().GetCanonicalType().GetBasicType() ==
      lldb::eBasicTypeBool) {
    return ret > 0 ? 1 : 0;
  }
  return ret;
}

bool LLDBType::IsScopedEnum() { return IsScopedEnum_V(type_); }

TypeSP LLDBType::GetEnumerationIntegerType(ParserContext& ctx) {
  return GetEnumerationIntegerType_V(type_, static_cast<Context&>(ctx));
}

bool LLDBType::IsEnumerationIntegerTypeSigned() {
  return IsEnumerationIntegerTypeSigned_V(type_);
}

bool LLDBType::CompareTo(TypeSP other) {
  auto rhs = ToSBType(other);
  if (type_ == rhs) {
    return true;
  }

  // Comparing two lldb::SBType doesn't always work reliably:
  // https://github.com/google/lldb-eval/blob/master/docs/lldb-bugs.md#comparing-lldbsbtype-objects-representing-the-same-type-doesnt-always-work

  // In LLDB the type name is stored as `llvm_private::ConstString`, which
  // points to global pool of unique strings. The equal names will point to the
  // same "const char*" object, so we can just check the pointers for equality.
  // Note that `GetCanonicalType()` and `GetUnqualifiedType()` fully
  // canonizes and removes qualifiers from the type, e.g. "int **" and
  // "int const * const * const" will be matched as the same type.
  const char* name = type_.GetCanonicalType().GetUnqualifiedType().GetName();
  const char* rhs_name = rhs.GetCanonicalType().GetUnqualifiedType().GetName();
  return name == rhs_name;
}

bool Value::IsScalar() { return type_->IsScalar(); }

bool Value::IsInteger() { return type_->IsInteger(); }

bool Value::IsFloat() { return type_->IsFloat(); }

bool Value::IsPointer() { return type_->IsPointerType(); }

bool Value::IsNullPtrType() { return type_->IsNullPtrType(); }

bool Value::IsSigned() { return type_->IsSigned(); }

bool Value::IsEnum() {
  return type_->GetTypeFlags() & lldb::eTypeIsEnumeration;
}

bool Value::IsScopedEnum() { return type_->IsScopedEnum(); }

bool Value::IsUnscopedEnum() { return IsEnum() && !IsScopedEnum(); }

bool Value::GetBool() {
  if (IsInteger() || IsUnscopedEnum() || IsPointer()) {
    return GetInteger().getBoolValue();
  }
  if (IsFloat()) {
    return GetFloat().isNonZero();
  }
  if (type_->IsArrayType()) {
    return AddressOf().GetUInt64() != 0;
  }
  // Either invalid value, or some complex SbValue (e.g. struct or class).
  return false;
}

uint64_t Value::GetUInt64() {
  // GetValueAsUnsigned performs overflow according to the underlying type. For
  // example, if the underlying type is `int32_t` and the value is `-1`,
  // GetValueAsUnsigned will return 4294967295.
  return IsSigned() ? value_.GetValueAsSigned() : GetValueAsUnsigned(value_);
}

int64_t Value::GetValueAsSigned() { return value_.GetValueAsSigned(); }

Value Value::AddressOf() { return Value(value_.AddressOf()); }

Value Value::Dereference() { return Value(value_.Dereference()); }

llvm::APSInt Value::GetInteger() {
  unsigned bit_width = static_cast<unsigned>(type_->GetByteSize() * CHAR_BIT);
  uint64_t value = GetValueAsUnsigned(value_);
  bool is_signed = IsSigned();

  return llvm::APSInt(llvm::APInt(bit_width, value, is_signed), !is_signed);
}

llvm::APFloat Value::GetFloat() {
  lldb::BasicType basic_type = type_->GetCanonicalType()->GetBasicType();
  lldb::SBError ignore;

  switch (basic_type) {
    case lldb::eBasicTypeFloat: {
      float v = 0;
      value_.GetData().ReadRawData(ignore, 0, &v, sizeof(float));
      return llvm::APFloat(v);
    }
    case lldb::eBasicTypeDouble:
      // No way to get more precision at the moment.
    case lldb::eBasicTypeLongDouble: {
      double v = 0;
      value_.GetData().ReadRawData(ignore, 0, &v, sizeof(double));
      return llvm::APFloat(v);
    }
    default:
      return llvm::APFloat(NAN);
  }
}

Value Value::Clone() {
  lldb::SBData data = value_.GetData();
  lldb::SBError ignore;
  auto raw_data = std::make_unique<uint8_t[]>(data.GetByteSize());
  data.ReadRawData(ignore, 0, raw_data.get(), data.GetByteSize());
  return CreateValueFromBytes(value_.GetTarget(), raw_data.get(),
                              ToSBType(type_));
}

void Value::Update(const llvm::APInt& v) {
  assert(v.getBitWidth() == type_->GetByteSize() * CHAR_BIT &&
         "illegal argument: new value should be of the same size");

  lldb::SBData data;
  lldb::SBError ignore;
  lldb::SBTarget target = value_.GetTarget();
  data.SetData(ignore, v.getRawData(), type_->GetByteSize(),
               target.GetByteOrder(),
               static_cast<uint8_t>(target.GetAddressByteSize()));
  value_.SetData(data, ignore);
}

void Value::Update(Value v) {
  assert((v.IsInteger() || v.IsFloat() || v.IsPointer()) &&
         "illegal argument: new value should be of the same size");

  if (v.IsInteger()) {
    Update(v.GetInteger());
  } else if (v.IsFloat()) {
    Update(v.GetFloat().bitcastToAPInt());
  } else if (v.IsPointer()) {
    Update(llvm::APInt(64, v.GetUInt64()));
  }
}

static llvm::APFloat CreateAPFloatFromAPSInt(const llvm::APSInt& value,
                                             lldb::BasicType basic_type) {
  switch (basic_type) {
    case lldb::eBasicTypeFloat:
      return llvm::APFloat(value.isSigned()
                               ? llvm::APIntOps::RoundSignedAPIntToFloat(value)
                               : llvm::APIntOps::RoundAPIntToFloat(value));
    case lldb::eBasicTypeDouble:
      // No way to get more precision at the moment.
    case lldb::eBasicTypeLongDouble:
      return llvm::APFloat(value.isSigned()
                               ? llvm::APIntOps::RoundSignedAPIntToDouble(value)
                               : llvm::APIntOps::RoundAPIntToDouble(value));
    default:
      return llvm::APFloat(NAN);
  }
}

static llvm::APFloat CreateAPFloatFromAPFloat(llvm::APFloat value,
                                              lldb::BasicType basic_type) {
  switch (basic_type) {
    case lldb::eBasicTypeFloat: {
      bool loses_info;
      value.convert(llvm::APFloat::IEEEsingle(),
                    llvm::APFloat::rmNearestTiesToEven, &loses_info);
      return value;
    }
    case lldb::eBasicTypeDouble:
      // No way to get more precision at the moment.
    case lldb::eBasicTypeLongDouble: {
      bool loses_info;
      value.convert(llvm::APFloat::IEEEdouble(),
                    llvm::APFloat::rmNearestTiesToEven, &loses_info);
      return value;
    }
    default:
      return llvm::APFloat(NAN);
  }
}

Value CastScalarToBasicType(lldb::SBTarget target, Value val, TypeSP type,
                            Error& error) {
  assert(type->IsScalar() && "target type must be an scalar");
  assert(val.type()->IsScalar() && "argument must be a scalar");

  if (type->IsBool()) {
    if (val.type()->IsInteger()) {
      return CreateValueFromBool(target, val.GetUInt64() != 0);
    }
    if (val.type()->IsFloat()) {
      return CreateValueFromBool(target, !val.GetFloat().isZero());
    }
  }
  if (type->IsInteger()) {
    if (val.type()->IsInteger()) {
      llvm::APSInt ext =
          val.GetInteger().extOrTrunc(type->GetByteSize() * CHAR_BIT);
      return CreateValueFromAPInt(target, ext, ToSBType(type));
    }
    if (val.type()->IsFloat()) {
      llvm::APSInt integer(type->GetByteSize() * CHAR_BIT, !type->IsSigned());
      bool is_exact;
      llvm::APFloatBase::opStatus status = val.GetFloat().convertToInteger(
          integer, llvm::APFloat::rmTowardZero, &is_exact);

      // Casting floating point values that are out of bounds of the target type
      // is undefined behaviour.
      if (status & llvm::APFloatBase::opInvalidOp) {
        error.SetUbStatus(UbStatus::kInvalidCast);
      }

      return CreateValueFromAPInt(target, integer, ToSBType(type));
    }
  }
  if (type->IsFloat()) {
    if (val.type()->IsInteger()) {
      llvm::APFloat f = CreateAPFloatFromAPSInt(
          val.GetInteger(), type->GetCanonicalType()->GetBasicType());
      return CreateValueFromAPFloat(target, f, ToSBType(type));
    }
    if (val.type()->IsFloat()) {
      llvm::APFloat f = CreateAPFloatFromAPFloat(
          val.GetFloat(), type->GetCanonicalType()->GetBasicType());
      return CreateValueFromAPFloat(target, f, ToSBType(type));
    }
  }
  assert(false && "invalid target type: must be a scalar");
  return Value();
}

Value CastEnumToBasicType(lldb::SBTarget target, Value val, TypeSP type) {
  assert(type->IsScalar() && "target type must be a scalar");
  assert(val.type()->IsEnum() && "argument must be an enum");

  if (type->IsBool()) {
    return CreateValueFromBool(target, val.GetUInt64() != 0);
  }

  // Get the value as APSInt and extend or truncate it to the requested size.
  llvm::APSInt ext =
      val.GetInteger().extOrTrunc(type->GetByteSize() * CHAR_BIT);

  if (type->IsInteger()) {
    return CreateValueFromAPInt(target, ext, ToSBType(type));
  }
  if (type->IsFloat()) {
    llvm::APFloat f =
        CreateAPFloatFromAPSInt(ext, type->GetCanonicalType()->GetBasicType());
    return CreateValueFromAPFloat(target, f, ToSBType(type));
  }
  assert(false && "invalid target type: must be a scalar");
  return Value();
}

Value CastPointerToBasicType(lldb::SBTarget target, Value val, TypeSP type) {
  assert(type->IsInteger() && "target type must be an integer");
  assert((type->IsBool() || type->GetByteSize() >= val.type()->GetByteSize()) &&
         "target type cannot be smaller than the pointer type");

  if (type->IsBool()) {
    return CreateValueFromBool(target, val.GetUInt64() != 0);
  }

  // Get the value as APSInt and extend or truncate it to the requested size.
  llvm::APSInt ext =
      val.GetInteger().extOrTrunc(type->GetByteSize() * CHAR_BIT);
  return CreateValueFromAPInt(target, ext, ToSBType(type));
}

Value CastIntegerOrEnumToEnumType(lldb::SBTarget target, Value val,
                                  TypeSP type) {
  assert(type->IsEnum() && "target type must be an enum");
  assert((val.type()->IsInteger() || val.type()->IsEnum()) &&
         "argument must be an integer or an enum");

  // Get the value as APSInt and extend or truncate it to the requested size.
  llvm::APSInt ext =
      val.GetInteger().extOrTrunc(type->GetByteSize() * CHAR_BIT);
  return CreateValueFromAPInt(target, ext, ToSBType(type));
}

Value CastFloatToEnumType(lldb::SBTarget target, Value val, TypeSP type,
                          Error& error) {
  assert(type->IsEnum() && "target type must be an enum");
  assert(val.type()->IsFloat() && "argument must be a float");

  llvm::APSInt integer(type->GetByteSize() * CHAR_BIT, !type->IsSigned());
  bool is_exact;

  llvm::APFloatBase::opStatus status = val.GetFloat().convertToInteger(
      integer, llvm::APFloat::rmTowardZero, &is_exact);

  // Casting floating point values that are out of bounds of the target type
  // is undefined behaviour.
  if (status & llvm::APFloatBase::opInvalidOp) {
    error.SetUbStatus(UbStatus::kInvalidCast);
  }

  return CreateValueFromAPInt(target, integer, ToSBType(type));
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

Value CreateValueNullptr(lldb::SBTarget target, lldb::SBType type) {
  assert(LLDBType(type).IsNullPtrType() && "target type must be nullptr");
  uintptr_t zero = 0;
  return CreateValueFromBytes(target, &zero, type);
}

}  // namespace lldb_eval
