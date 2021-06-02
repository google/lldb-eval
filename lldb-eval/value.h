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

#include "lldb-eval/context.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APSInt.h"

namespace lldb_eval {

/// Wrapper for lldb::SBType adding some convenience methods.
class Type : public lldb::SBType {
 public:
  Type();
  Type(const lldb::SBType& type);

  bool IsScalar();
  bool IsBool();
  bool IsInteger();
  bool IsFloat();
  bool IsPointerToVoid();
  bool IsNullPtrType();
  bool IsSigned();
  bool IsBasicType();
  bool IsEnum();
  bool IsScopedEnum();
  bool IsUnscopedEnum();
  bool IsScalarOrUnscopedEnum();
  bool IsIntegerOrUnscopedEnum();
  bool IsRecordType();
  bool IsPromotableIntegerType();
  bool IsContextuallyConvertibleToBool();

  lldb::SBType GetEnumerationIntegerType(std::shared_ptr<Context> ctx);
};

bool CompareTypes(lldb::SBType lhs, lldb::SBType rhs);

class Value {
 public:
  Value() {}

  explicit Value(lldb::SBValue value) {
    value_ = value;
    type_ = value_.GetType();
  }

 public:
  bool IsValid() { return value_.IsValid(); }
  explicit operator bool() { return IsValid(); }

  lldb::SBValue inner_value() const { return value_; }
  Type type() const { return type_; };

  bool IsScalar();
  bool IsInteger();
  bool IsFloat();
  bool IsPointer();
  bool IsNullPtrType();
  bool IsSigned();
  bool IsEnum();
  bool IsScopedEnum();
  bool IsUnscopedEnum();

  bool GetBool();
  uint64_t GetUInt64();

  Value AddressOf();
  Value Dereference();

  llvm::APSInt GetInteger();
  llvm::APFloat GetFloat();

  Value Clone();
  void Update(const llvm::APInt& v);
  void Update(Value v);

 private:
  lldb::SBValue value_;
  Type type_;
};

Value CastScalarToBasicType(lldb::SBTarget target, Value val, Type type,
                            Error& error);

Value CastEnumToBasicType(lldb::SBTarget target, Value val, Type type);

Value CastPointerToBasicType(lldb::SBTarget target, Value val, Type type);

Value CastIntegerOrEnumToEnumType(lldb::SBTarget target, Value val, Type type);

Value CastFloatToEnumType(lldb::SBTarget target, Value val, Type type,
                          Error& error);

Value CreateValueFromBytes(lldb::SBTarget target, const void* bytes,
                           lldb::SBType type);

Value CreateValueFromBytes(lldb::SBTarget target, const void* bytes,
                           lldb::BasicType basic_type);

Value CreateValueFromAPInt(lldb::SBTarget target, const llvm::APInt& v,
                           lldb::SBType type);

Value CreateValueFromAPFloat(lldb::SBTarget target, const llvm::APFloat& v,
                             lldb::SBType type);

Value CreateValueFromPointer(lldb::SBTarget target, uintptr_t addr,
                             lldb::SBType type);

Value CreateValueFromBool(lldb::SBTarget target, bool value);

Value CreateValueNullptr(lldb::SBTarget target);

}  // namespace lldb_eval

#endif  // LLDB_EVAL_VALUE_H_
