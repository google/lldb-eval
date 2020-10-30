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
#include <iosfwd>
#include <string>
#include <type_traits>

#include "lldb-eval/traits.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APSInt.h"

namespace lldb_eval {

class Value {
 public:
  Value() {}

  explicit Value(lldb::SBValue value, bool is_rvalue = false) {
    value_ = value;
    type_ = value_.GetType();
    is_rvalue_ = is_rvalue;
  }

 public:
  bool IsValid() { return value_.IsValid(); }
  explicit operator bool() { return IsValid(); }

  lldb::SBValue inner_value() const { return value_; }
  lldb::SBType type() const { return type_; };

  bool IsRvalue() const { return is_rvalue_; }
  bool IsScalar();
  bool IsInteger();
  bool IsFloat();
  bool IsPointer();
  bool IsPointerToVoid();
  bool IsSigned();

  bool GetBool();
  int64_t GetInt64();
  uintptr_t GetValueAsAddress();
  Value GetRvalueRef() const;

  Value AddressOf();
  Value Dereference();

  llvm::APSInt GetInteger();
  llvm::APFloat GetFloat();

  template <typename T>
  T ReadValue() {
    static_assert(std::is_scalar<T>::value, "T must be scalar");

    lldb::SBError ignore;
    T ret = 0;
    value_.GetData().ReadRawData(ignore, 0, &ret, sizeof(T));

    return ret;
  }

  template <typename T>
  T ConvertTo() {
    static_assert(std::is_scalar<T>::value, "T must be scalar");

    switch (type_.GetCanonicalType().GetBasicType()) {
#define CASE(basic_type, builtin_type)                \
  case basic_type: {                                  \
    return static_cast<T>(ReadValue<builtin_type>()); \
  }

      LLDB_TYPE_BUILTIN(CASE)
#undef CASE

      default:
        return T();
    }
  }

 private:
  lldb::SBValue value_;
  bool is_rvalue_;

  // Same as value_.GetType(). Just a shortcut, because it's used extensively.
  lldb::SBType type_;
};

Value IntegralPromotion(lldb::SBTarget target, Value value);

lldb::SBType UsualArithmeticConversions(lldb::SBTarget target, Value* lhs,
                                        Value* rhs);

Value CastScalarToBasicType(lldb::SBTarget target, Value val,
                            lldb::SBType type);

Value CastPointerToBasicType(lldb::SBTarget target, Value val,
                             lldb::SBType type);

Value CreateValueFromBytes(lldb::SBTarget target, const void* bytes,
                           lldb::SBType type);

Value CreateValueFromBytes(lldb::SBTarget target, const void* bytes,
                           lldb::BasicType basic_type);

Value CreateValueFromAP(lldb::SBTarget target, const llvm::APInt& v,
                        lldb::SBType type);

Value CreateValueFromAP(lldb::SBTarget target, const llvm::APFloat& v,
                        lldb::SBType type);

Value CreateValueFromPointer(lldb::SBTarget target, uintptr_t addr,
                             lldb::SBType type);

Value CreateValueFromBool(lldb::SBTarget target, bool value);

Value CreateValueZero(lldb::SBTarget target);

}  // namespace lldb_eval

#endif  // LLDB_EVAL_VALUE_H_
