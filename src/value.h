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

#ifndef LLDB_EVAL_VALUE_H_
#define LLDB_EVAL_VALUE_H_

#include <cstdint>
#include <iostream>
#include <string>

#include "lldb-eval/pointer.h"
#include "lldb-eval/scalar.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBValue.h"

namespace lldb_eval {

class Value {
 public:
  enum class Type {
    INVALID,
    BOOLEAN,
    SCALAR,
    POINTER,
    SB_VALUE,
  };

 public:
  Value() : type_(Type::INVALID) {}

  explicit Value(bool value) {
    type_ = Type::BOOLEAN;
    scalar_ = Scalar(static_cast<int32_t>(value));
    is_rvalue_ = true;
  }
  explicit Value(const Scalar& value) {
    type_ = Type::SCALAR;
    scalar_ = value;
    is_rvalue_ = true;
  }
  explicit Value(const Pointer& value) {
    type_ = Type::POINTER;
    pointer_ = value;
    is_rvalue_ = true;
  }
  explicit Value(lldb::SBValue value, bool is_rvalue = false) {
    type_ = Type::SB_VALUE;
    sb_value_ = value;
    is_rvalue_ = is_rvalue;
  }

 public:
  bool IsValid() const { return type_ != Type::INVALID; }

  bool IsRValue() const { return is_rvalue_; }

  bool IsScalar();
  bool IsPointer();

  bool AsBool();
  Scalar AsScalar() const;
  Pointer AsPointer() const;
  lldb::SBValue AsSbValue(lldb::SBTarget target) const;

  explicit operator bool() const { return IsValid(); }

 private:
  Type type_;
  bool is_rvalue_;

  // Possible values.
  Scalar scalar_;
  Pointer pointer_;
  lldb::SBValue sb_value_;
};

Value CastScalarToBasicType(const Scalar& value, lldb::SBType type,
                            lldb::SBTarget target);

Value CastPointerToBasicType(const Pointer& value, lldb::SBType type,
                             lldb::SBTarget target);

}  // namespace lldb_eval

#endif  // LLDB_EVAL_VALUE_H_
