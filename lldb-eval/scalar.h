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

#ifndef LLDB_EVAL_SCALAR_H_
#define LLDB_EVAL_SCALAR_H_

#include <cstdint>
#include <string>

#include "lldb-eval/defines.h"
#include "lldb/API/SBValue.h"

namespace lldb_eval {

class Scalar {
 public:
  enum class Type {
    INVALID,
    INT32,
    UINT32,
    INT64,
    UINT64,
    FLOAT,
    DOUBLE,
  };
  union Data {
    int32_t int32_;
    uint32_t uint32_;
    int64_t int64_;
    uint64_t uint64_;
    float float_;
    double double_;
  };

 public:
  Scalar() : type_(Type::INVALID) {}
  explicit Scalar(int32_t value) { SetValueInt32(value); }
  explicit Scalar(uint32_t value) { SetValueUInt32(value); }
  explicit Scalar(int64_t value) { SetValueInt64(value); }
  explicit Scalar(uint64_t value) { SetValueUInt64(value); }
  explicit Scalar(float value) { SetValueFloat(value); }
  explicit Scalar(double value) { SetValueDouble(value); }

 public:
  void SetInvalid() { type_ = Type::INVALID; }

  void SetValueInt32(int32_t value) {
    type_ = Type::INT32;
    value_.int32_ = value;
  }

  void SetValueUInt32(uint32_t value) {
    type_ = Type::UINT32;
    value_.uint32_ = value;
  }

  void SetValueInt64(int64_t value) {
    type_ = Type::INT64;
    value_.int64_ = value;
  }

  void SetValueUInt64(uint64_t value) {
    type_ = Type::UINT64;
    value_.uint64_ = value;
  }

  void SetValueFloat(float value) {
    type_ = Type::FLOAT;
    value_.float_ = value;
  }

  void SetValueDouble(double value) {
    type_ = Type::DOUBLE;
    value_.double_ = value;
  }

  void PromoteTo(Type type);

  template <typename T>
  T GetAs() const {
    switch (type_) {
      case Type::INVALID:
        return T();
      case Type::INT32:
        return static_cast<T>(value_.int32_);
      case Type::UINT32:
        return static_cast<T>(value_.uint32_);
      case Type::INT64:
        return static_cast<T>(value_.int64_);
      case Type::UINT64:
        return static_cast<T>(value_.uint64_);
      case Type::FLOAT:
        return static_cast<T>(value_.float_);
      case Type::DOUBLE:
        return static_cast<T>(value_.double_);
    }
    unreachable("Scalar::Type enum wasn't exhausted in the switch statement.");
  }

  bool AsBool() const { return GetAs<bool>(); }

  int64_t GetInt64() const { return GetAs<int64_t>(); }

  static Scalar FromSbValue(lldb::SBValue value);

  friend const Scalar operator+(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator-(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator/(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator*(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator&(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator|(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator%(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator^(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator<<(const Scalar& lhs, const Scalar& rhs);
  friend const Scalar operator>>(const Scalar& lhs, const Scalar& rhs);

  friend bool operator==(const Scalar& lhs, const Scalar& rhs);
  friend bool operator!=(const Scalar& lhs, const Scalar& rhs);
  friend bool operator<(const Scalar& lhs, const Scalar& rhs);
  friend bool operator<=(const Scalar& lhs, const Scalar& rhs);
  friend bool operator>(const Scalar& lhs, const Scalar& rhs);
  friend bool operator>=(const Scalar& lhs, const Scalar& rhs);

 public:
  Type type_;
  Data value_;
};

const Scalar operator+(const Scalar& lhs, const Scalar& rhs);
const Scalar operator-(const Scalar& lhs, const Scalar& rhs);
const Scalar operator/(const Scalar& lhs, const Scalar& rhs);
const Scalar operator*(const Scalar& lhs, const Scalar& rhs);
const Scalar operator&(const Scalar& lhs, const Scalar& rhs);
const Scalar operator|(const Scalar& lhs, const Scalar& rhs);
const Scalar operator%(const Scalar& lhs, const Scalar& rhs);
const Scalar operator^(const Scalar& lhs, const Scalar& rhs);
const Scalar operator<<(const Scalar& lhs, const Scalar& rhs);
const Scalar operator>>(const Scalar& lhs, const Scalar& rhs);
bool operator==(const Scalar& lhs, const Scalar& rhs);
bool operator!=(const Scalar& lhs, const Scalar& rhs);
bool operator<(const Scalar& lhs, const Scalar& rhs);
bool operator<=(const Scalar& lhs, const Scalar& rhs);
bool operator>(const Scalar& lhs, const Scalar& rhs);
bool operator>=(const Scalar& lhs, const Scalar& rhs);

}  // namespace lldb_eval
#endif  // LLDB_EVAL_SCALAR_H_
