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
#include "lldb-eval/scalar.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"

namespace {

template <typename T>
lldb::SBValue CreateSbValue(lldb::SBTarget target, const T& value,
                            lldb::SBType type) {
  // Borrow bytes from a value;
  const uint8_t* bytes = reinterpret_cast<const uint8_t*>(&value);
  size_t bytes_length = sizeof(T);
  // lldb::SBData::SetData() doesn't actually use "error".
  lldb::SBError error;

  lldb::SBData data;
  data.SetData(error, bytes, bytes_length, target.GetByteOrder(),
               static_cast<uint8_t>(target.GetAddressByteSize()));

  // CreateValueFromData copies the data referenced by `bytes` to its own
  // storage. `value` should be valid up until this point.
  return target.CreateValueFromData("result", data, type);
}

template <typename T>
lldb::SBValue CreateSbValue(lldb::SBTarget target, const T& value,
                            lldb::BasicType type) {
  return CreateSbValue(target, value, target.GetBasicType(type));
}

}  // namespace

namespace lldb_eval {

bool Value::IsScalar() {
  if (type_ == Type::SB_VALUE) {
    return sb_value_.GetType().GetCanonicalType().GetBasicType() !=
           lldb::eBasicTypeInvalid;
  }
  return type_ == Type::BOOLEAN || type_ == Type::SCALAR;
}

bool Value::IsPointer() {
  if (type_ == Type::SB_VALUE) {
    return sb_value_.GetType().GetCanonicalType().IsPointerType();
  }
  return type_ == Type::POINTER;
}

bool Value::AsBool() {
  if (IsScalar()) {
    return AsScalar().AsBool();
  }
  if (IsPointer()) {
    return AsPointer().AsBool();
  }
  // Either invalid value, or some complex SbValue (e.g. struct or class).
  return false;
}

Scalar Value::AsScalar() const {
  switch (type_) {
    case Type::INVALID:
    case Type::POINTER: {
      return Scalar();
    }
    case Type::BOOLEAN:
    case Type::SCALAR: {
      return scalar_;
    }
    case Type::SB_VALUE: {
      return Scalar::FromSbValue(sb_value_);
    }
  }
  unreachable("Value::Type enum wasn't exhausted in the switch statement.");
}

Pointer Value::AsPointer() const {
  switch (type_) {
    case Type::INVALID:
    case Type::BOOLEAN:
    case Type::SCALAR: {
      return Pointer();
    }
    case Type::POINTER: {
      return pointer_;
    }
    case Type::SB_VALUE: {
      return Pointer::FromSbValue(sb_value_);
    }
  }
  unreachable("Value::Type enum wasn't exhausted in the switch statement.");
}

lldb::SBValue Value::AsSbValue(lldb::SBTarget target) const {
  switch (type_) {
    case Type::INVALID: {
      break;
    }
    case Type::BOOLEAN: {
      return CreateSbValue(target, scalar_.value_.int32_, lldb::eBasicTypeBool);
    }
    case Type::SCALAR: {
      switch (scalar_.type_) {
        case Scalar::Type::INVALID: {
          break;
        }
        case Scalar::Type::INT32: {
          return CreateSbValue(target, scalar_.value_.int32_,
                               lldb::eBasicTypeInt);
        }
        case Scalar::Type::UINT32: {
          return CreateSbValue(target, scalar_.value_.uint32_,
                               lldb::eBasicTypeUnsignedInt);
        }
        case Scalar::Type::INT64: {
          return CreateSbValue(target, scalar_.value_.int64_,
                               lldb::eBasicTypeLongLong);
        }
        case Scalar::Type::UINT64: {
          return CreateSbValue(target, scalar_.value_.uint64_,
                               lldb::eBasicTypeUnsignedLongLong);
        }
        case Scalar::Type::FLOAT: {
          return CreateSbValue(target, scalar_.value_.float_,
                               lldb::eBasicTypeFloat);
        }
        case Scalar::Type::DOUBLE: {
          return CreateSbValue(target, scalar_.value_.double_,
                               lldb::eBasicTypeDouble);
        }
      }
      unreachable("Scalar::Type wasn't exhausted in the switch statement.");
    }
    case Type::POINTER: {
      return CreateSbValue(target, pointer_.addr(), pointer_.type());
    }
    case Type::SB_VALUE: {
      return sb_value_;
    }
  }

  // Invalid value.
  return lldb::SBValue();
}

Value CastScalarToBasicType(const Scalar& value, lldb::SBType type,
                            lldb::SBTarget target) {
  // The result is lldb::SBValue, because we need the value to have a specific
  // target type (e.g. "wchar_t" or "unsigned short").
  lldb::SBValue ret;

  switch (type.GetCanonicalType().GetBasicType()) {
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
      ret = CreateSbValue(target, value.GetAs<char>(), type);
      break;
    case lldb::eBasicTypeChar16:
      ret = CreateSbValue(target, value.GetAs<char16_t>(), type);
      break;
    case lldb::eBasicTypeChar32:
      ret = CreateSbValue(target, value.GetAs<char32_t>(), type);
      break;
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeUnsignedWChar:
      ret = CreateSbValue(target, value.GetAs<wchar_t>(), type);
      break;
    case lldb::eBasicTypeShort:
      ret = CreateSbValue(target, value.GetAs<short>(), type);
      break;
    case lldb::eBasicTypeInt:
      ret = CreateSbValue(target, value.GetAs<int>(), type);
      break;
    case lldb::eBasicTypeLong:
      ret = CreateSbValue(target, value.GetAs<long>(), type);
      break;
    case lldb::eBasicTypeLongLong:
      ret = CreateSbValue(target, value.GetAs<long long>(), type);
      break;
    case lldb::eBasicTypeUnsignedChar:
      ret = CreateSbValue(target, value.GetAs<unsigned char>(), type);
      break;
    case lldb::eBasicTypeUnsignedShort:
      ret = CreateSbValue(target, value.GetAs<unsigned short>(), type);
      break;
    case lldb::eBasicTypeUnsignedInt:
      ret = CreateSbValue(target, value.GetAs<unsigned int>(), type);
      break;
    case lldb::eBasicTypeUnsignedLong:
      ret = CreateSbValue(target, value.GetAs<unsigned long>(), type);
      break;
    case lldb::eBasicTypeUnsignedLongLong:
      ret = CreateSbValue(target, value.GetAs<unsigned long long>(), type);
      break;
    case lldb::eBasicTypeFloat:
      ret = CreateSbValue(target, value.GetAs<float>(), type);
      break;
    case lldb::eBasicTypeDouble:
      ret = CreateSbValue(target, value.GetAs<double>(), type);
      break;

    default:
      // Invalid basic type, can't cast to it.
      break;
  }

  return Value(ret);
}

Value CastPointerToBasicType(const Pointer& value, lldb::SBType type,
                             lldb::SBTarget target) {
  // The result is lldb::SBValue, because we need the value to have a specific
  // target type (e.g. "wchar_t" or "unsigned short").
  lldb::SBValue ret;

  uint64_t addr = value.addr();

  switch (type.GetCanonicalType().GetBasicType()) {
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
      ret = CreateSbValue(target, static_cast<char>(addr), type);
      break;
    case lldb::eBasicTypeChar16:
      ret = CreateSbValue(target, static_cast<char16_t>(addr), type);
      break;
    case lldb::eBasicTypeChar32:
      ret = CreateSbValue(target, static_cast<char32_t>(addr), type);
      break;
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeUnsignedWChar:
      ret = CreateSbValue(target, static_cast<wchar_t>(addr), type);
      break;
    case lldb::eBasicTypeShort:
      ret = CreateSbValue(target, static_cast<short>(addr), type);
      break;
    case lldb::eBasicTypeInt:
      ret = CreateSbValue(target, static_cast<int>(addr), type);
      break;
    case lldb::eBasicTypeLong:
      ret = CreateSbValue(target, static_cast<long>(addr), type);
      break;
    case lldb::eBasicTypeLongLong:
      ret = CreateSbValue(target, static_cast<long long>(addr), type);
      break;
    case lldb::eBasicTypeUnsignedChar:
      ret = CreateSbValue(target, static_cast<unsigned char>(addr), type);
      break;
    case lldb::eBasicTypeUnsignedShort:
      ret = CreateSbValue(target, static_cast<unsigned short>(addr), type);
      break;
    case lldb::eBasicTypeUnsignedInt:
      ret = CreateSbValue(target, static_cast<unsigned int>(addr), type);
      break;
    case lldb::eBasicTypeUnsignedLong:
      ret = CreateSbValue(target, static_cast<unsigned long>(addr), type);
      break;
    case lldb::eBasicTypeUnsignedLongLong:
      ret = CreateSbValue(target, static_cast<unsigned long long>(addr), type);
      break;

    default:
      // Invalid basic type, can't cast to it.
      break;
  }

  return Value(ret);
}

}  // namespace lldb_eval
