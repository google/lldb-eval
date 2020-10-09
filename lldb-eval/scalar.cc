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

#include "lldb-eval/scalar.h"

#include <string>

#include "lldb-eval/defines.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"

namespace lldb_eval {

Scalar::Type PromoteOperands(const Scalar& lhs, const Scalar& rhs, Scalar* a,
                             Scalar* b) {
  *a = lhs;
  *b = rhs;

  if (a->type_ > b->type_) {
    b->PromoteTo(a->type_);
  } else if (a->type_ < b->type_) {
    a->PromoteTo(b->type_);
  }

  if (a->type_ == b->type_) {
    return a->type_;
  }
  return Scalar::Type::INVALID;
}

void Scalar::PromoteTo(Type type) {
  switch (type_) {
    case Type::INVALID:
      // Can't promote undefined values.
      SetInvalid();
      break;

    case Type::INT32:
      switch (type) {
        case Type::INVALID:
          SetInvalid();
          break;
        case Type::INT32:
          // No-op, already required type.
          break;
        case Type::UINT32:
          SetValueUInt32(value_.int32_);
          break;
        case Type::INT64:
          SetValueInt64(value_.int32_);
          break;
        case Type::UINT64:
          SetValueUInt64(value_.int32_);
          break;
        case Type::FLOAT:
          SetValueFloat(static_cast<float>(value_.int32_));
          break;
        case Type::DOUBLE:
          SetValueDouble(static_cast<double>(value_.int32_));
          break;
      }
      break;

    case Type::UINT32:
      switch (type) {
        case Type::INVALID:
        case Type::INT32:
          SetInvalid();
          break;
        case Type::UINT32:
          // No-op, already required type.
          break;
        case Type::INT64:
          SetValueInt64(value_.uint32_);
          break;
        case Type::UINT64:
          SetValueUInt64(value_.uint32_);
          break;
        case Type::FLOAT:
          SetValueFloat(static_cast<float>(value_.uint32_));
          break;
        case Type::DOUBLE:
          SetValueDouble(static_cast<double>(value_.uint32_));
          break;
      }
      break;

    case Type::INT64:
      switch (type) {
        case Type::INVALID:
        case Type::INT32:
        case Type::UINT32:
          SetInvalid();
          break;
        case Type::INT64:
          // No-op, already required type.
          break;
        case Type::UINT64:
          SetValueUInt64(value_.int64_);
          break;
        case Type::FLOAT:
          SetValueFloat(static_cast<float>(value_.int64_));
          break;
        case Type::DOUBLE:
          SetValueDouble(static_cast<double>(value_.int64_));
          break;
      }
      break;

    case Type::UINT64:
      switch (type) {
        case Type::INVALID:
        case Type::INT32:
        case Type::UINT32:
        case Type::INT64:
          SetInvalid();
          break;
        case Type::UINT64:
          // No-op, already required type.
          break;
        case Type::FLOAT:
          SetValueFloat(static_cast<float>(value_.uint64_));
          break;
        case Type::DOUBLE:
          SetValueDouble(static_cast<double>(value_.uint64_));
          break;
      }
      break;

    case Type::FLOAT:
      switch (type) {
        case Type::INVALID:
        case Type::INT32:
        case Type::UINT32:
        case Type::INT64:
        case Type::UINT64:
          SetInvalid();
          break;
        case Type::FLOAT:
          // No-op, already required type.
          break;
        case Type::DOUBLE:
          SetValueDouble(static_cast<double>(value_.float_));
          break;
      }
      break;

    case Type::DOUBLE:
      switch (type) {
        case Type::INVALID:
        case Type::INT32:
        case Type::UINT32:
        case Type::INT64:
        case Type::UINT64:
        case Type::FLOAT:
          SetInvalid();
          break;
        case Type::DOUBLE:
          // No-op, already required type.
          break;
      }
      break;
  }
}

Scalar Scalar::FromSbValue(lldb::SBValue value) {
  // Get the canonical type, because the initial one can be a typedef/alias.
  lldb::SBType type = value.GetType().GetCanonicalType();

  switch (type.GetBasicType()) {
    case lldb::eBasicTypeInvalid: {
      // Can't get a Scalar out of SBValue with non-basic type.
      break;
    }
    case lldb::eBasicTypeVoid: {
      // Can't get a Scalar out of 'void' SBValue.
      break;
    }
    case lldb::eBasicTypeHalf:
    case lldb::eBasicTypeInt128:
    case lldb::eBasicTypeUnsignedInt128:
    case lldb::eBasicTypeLongDouble:
    case lldb::eBasicTypeFloatComplex:
    case lldb::eBasicTypeDoubleComplex:
    case lldb::eBasicTypeLongDoubleComplex:
    case lldb::eBasicTypeObjCID:
    case lldb::eBasicTypeObjCClass:
    case lldb::eBasicTypeObjCSel:
    case lldb::eBasicTypeNullPtr:
    case lldb::eBasicTypeOther: {
      // Don't support this type yet: value.GetTypeName()
      break;
    }
    case lldb::eBasicTypeBool: {
      lldb::SBError error;
      uint8_t val = value.GetData().GetUnsignedInt8(error, 0);
      if (error) {
        // Error trying to get uint8_t: error.GetCString()
        break;
      }
      return Scalar(static_cast<uint32_t>(val));
    }
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeChar16:
    case lldb::eBasicTypeChar32:
    case lldb::eBasicTypeShort:
    case lldb::eBasicTypeInt:
    case lldb::eBasicTypeLong:
    case lldb::eBasicTypeLongLong: {
      Scalar ret;
      lldb::SBError error;

      switch (value.GetByteSize()) {
        case 1:
          ret = Scalar(value.GetData().GetSignedInt8(error, 0));
          break;
        case 2:
          ret = Scalar(value.GetData().GetSignedInt16(error, 0));
          break;
        case 4:
          ret = Scalar(value.GetData().GetSignedInt32(error, 0));
          break;
        case 8:
          ret = Scalar(value.GetData().GetSignedInt64(error, 0));
          break;
        default:
          // Unexpected byte size, maybe it's int128?
          break;
      }

      if (error) {
        // Error trying to get int32: error.GetCString()
        break;
      }

      return ret;
    }
    case lldb::eBasicTypeUnsignedChar:
    case lldb::eBasicTypeUnsignedWChar:
    case lldb::eBasicTypeUnsignedShort:
    case lldb::eBasicTypeUnsignedInt:
    case lldb::eBasicTypeUnsignedLong:
    case lldb::eBasicTypeUnsignedLongLong: {
      Scalar ret;
      lldb::SBError error;

      switch (value.GetByteSize()) {
        case 1:
          ret = Scalar(value.GetData().GetUnsignedInt8(error, 0));
          break;
        case 2:
          ret = Scalar(value.GetData().GetUnsignedInt16(error, 0));
          break;
        case 4:
          ret = Scalar(value.GetData().GetUnsignedInt32(error, 0));
          break;
        case 8:
          ret = Scalar(value.GetData().GetUnsignedInt64(error, 0));
          break;
        default:
          // Unexpected byte size, maybe it's int128?
          break;
      }

      if (error) {
        // Error trying to get int32: error.GetCString()
        break;
      }

      return ret;
    }
    case lldb::eBasicTypeFloat: {
      lldb::SBError error;
      float val = value.GetData().GetFloat(error, 0);
      if (error) {
        // Error trying to get float: error.GetCString()
        break;
      }
      return Scalar(val);
    }
    case lldb::eBasicTypeDouble: {
      lldb::SBError error;
      double val = value.GetData().GetDouble(error, 0);
      if (error) {
        // Error trying to get double: error.GetCString()
        break;
      }
      return Scalar(val);
    }
  }

  // Failed to get a Scalar value from lldb::SBValue.
  return Scalar();
}

const Scalar operator+(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ + b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ + b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ + b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ + b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
      ret.SetValueFloat(a.value_.float_ + b.value_.float_);
      break;
    case Scalar::Type::DOUBLE:
      ret.SetValueDouble(a.value_.double_ + b.value_.double_);
      break;
  }

  return ret;
}

const Scalar operator-(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ - b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ - b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ - b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ - b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
      ret.SetValueFloat(a.value_.float_ - b.value_.float_);
      break;
    case Scalar::Type::DOUBLE:
      ret.SetValueDouble(a.value_.double_ - b.value_.double_);
      break;
  }

  return ret;
}

const Scalar operator/(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ / b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ / b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ / b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ / b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
      ret.SetValueFloat(a.value_.float_ / b.value_.float_);
      break;
    case Scalar::Type::DOUBLE:
      ret.SetValueDouble(a.value_.double_ / b.value_.double_);
      break;
  }

  return ret;
}

const Scalar operator*(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ * b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ * b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ * b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ * b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
      ret.SetValueFloat(a.value_.float_ * b.value_.float_);
      break;
    case Scalar::Type::DOUBLE:
      ret.SetValueDouble(a.value_.double_ * b.value_.double_);
      break;
  }

  return ret;
}

const Scalar operator&(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ & b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ & b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ & b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ & b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
    case Scalar::Type::DOUBLE:
      // Can't do & on float/double.
      break;
  }

  return ret;
}

const Scalar operator|(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ | b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ | b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ | b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ | b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
    case Scalar::Type::DOUBLE:
      // Can't do | on float/double.
      break;
  }

  return ret;
}

const Scalar operator%(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ % b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ % b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ % b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ % b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
    case Scalar::Type::DOUBLE:
      // Can't do % on float/double.
      break;
  }

  return ret;
}

const Scalar operator^(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ ^ b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ ^ b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ ^ b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ ^ b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
    case Scalar::Type::DOUBLE:
      // Can't do ^ on float/double.
      break;
  }

  return ret;
}

const Scalar operator<<(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ << b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ << b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ << b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ << b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
    case Scalar::Type::DOUBLE:
      // Can't do << on float/double.
      break;
  }

  return ret;
}

const Scalar operator>>(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b, ret;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      break;
    case Scalar::Type::INT32:
      ret.SetValueInt32(a.value_.int32_ >> b.value_.int32_);
      break;
    case Scalar::Type::UINT32:
      ret.SetValueUInt32(a.value_.uint32_ >> b.value_.uint32_);
      break;
    case Scalar::Type::INT64:
      ret.SetValueInt64(a.value_.int64_ >> b.value_.int64_);
      break;
    case Scalar::Type::UINT64:
      ret.SetValueUInt64(a.value_.uint64_ >> b.value_.uint64_);
      break;
    case Scalar::Type::FLOAT:
    case Scalar::Type::DOUBLE:
      // Can't do >> on float/double.
      break;
  }

  return ret;
}

bool operator==(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      // TODO(werat): Handle error somehow.
      return false;
    case Scalar::Type::INT32:
      return a.value_.int32_ == b.value_.int32_;
    case Scalar::Type::UINT32:
      return a.value_.uint32_ == b.value_.uint32_;
    case Scalar::Type::INT64:
      return a.value_.int64_ == b.value_.int64_;
    case Scalar::Type::UINT64:
      return a.value_.uint64_ == b.value_.uint64_;
    case Scalar::Type::FLOAT:
      return a.value_.float_ == b.value_.float_;
    case Scalar::Type::DOUBLE:
      return a.value_.double_ == b.value_.double_;
  }
  unreachable("Scalar::Type enum wasn't exhausted in the switch statement.");
}

bool operator!=(const Scalar& lhs, const Scalar& rhs) { return !(lhs == rhs); }

bool operator<(const Scalar& lhs, const Scalar& rhs) {
  Scalar a, b;

  switch (PromoteOperands(lhs, rhs, &a, &b)) {
    case Scalar::Type::INVALID:
      // TODO(werat): Handle error somehow.
      return false;
    case Scalar::Type::INT32:
      return a.value_.int32_ < b.value_.int32_;
    case Scalar::Type::UINT32:
      return a.value_.uint32_ < b.value_.uint32_;
    case Scalar::Type::INT64:
      return a.value_.int64_ < b.value_.int64_;
    case Scalar::Type::UINT64:
      return a.value_.uint64_ < b.value_.uint64_;
    case Scalar::Type::FLOAT:
      return a.value_.float_ < b.value_.float_;
    case Scalar::Type::DOUBLE:
      return a.value_.double_ < b.value_.double_;
  }
  unreachable("Scalar::Type enum wasn't exhausted in the switch statement.");
}

bool operator<=(const Scalar& lhs, const Scalar& rhs) { return !(rhs < lhs); }

bool operator>(const Scalar& lhs, const Scalar& rhs) { return rhs < lhs; }

bool operator>=(const Scalar& lhs, const Scalar& rhs) { return !(lhs < rhs); }

}  // namespace lldb_eval
