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

#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APSInt.h"
#include "type.h"

namespace lldb_eval {

class Error;

/// Wrapper for lldb::SBType adding some convenience methods.
class LLDBType : public Type {
 public:
  LLDBType() = default;
  LLDBType(const lldb::SBType& t) : type_(t) {}

  uint64_t GetByteSize() override { return type_.GetByteSize(); }
  uint32_t GetTypeFlags() override { return type_.GetTypeFlags(); }
  bool IsArrayType() override { return type_.IsArrayType(); }
  bool IsPointerType() override { return type_.IsPointerType(); }
  bool IsReferenceType() override { return type_.IsReferenceType(); }
  bool IsPolymorphicClass() override { return type_.IsPolymorphicClass(); }
  bool IsScopedEnum() override;
  bool IsAnonymousType() override { return type_.IsAnonymousType(); }
  bool IsValid() const override { return type_.IsValid(); }
  bool CompareTo(TypeSP other) override;
  llvm::StringRef GetName() override { return type_.GetName(); }
  TypeSP GetArrayElementType() override {
    return CreateSP(type_.GetArrayElementType());
  }
  TypeSP GetPointerType() override {
    return LLDBType::CreateSP(type_.GetPointerType());
  }
  TypeSP GetPointeeType() override {
    return LLDBType::CreateSP(type_.GetPointeeType());
  }
  TypeSP GetReferenceType() override {
    return LLDBType::CreateSP(type_.GetReferenceType());
  }
  TypeSP GetDereferencedType() override {
    return LLDBType::CreateSP(type_.GetDereferencedType());
  }
  TypeSP GetCanonicalType() override {
    return LLDBType::CreateSP(type_.GetCanonicalType());
  }
  TypeSP GetUnqualifiedType() override {
    return LLDBType::CreateSP(type_.GetUnqualifiedType());
  }
  lldb::BasicType GetBasicType() override { return type_.GetBasicType(); }
  lldb::TypeClass GetTypeClass() override { return type_.GetTypeClass(); }
  uint32_t GetNumberOfDirectBaseClasses() override {
    return type_.GetNumberOfDirectBaseClasses();
  }
  uint32_t GetNumberOfVirtualBaseClasses() override {
    return type_.GetNumberOfVirtualBaseClasses();
  }
  uint32_t GetNumberOfFields() override { return type_.GetNumberOfFields(); }
  BaseInfo GetDirectBaseClassAtIndex(uint32_t idx) override {
    auto member = type_.GetDirectBaseClassAtIndex(idx);
    return {LLDBType::CreateSP(member.GetType()), member.GetOffsetInBytes()};
  }
  TypeSP GetVirtualBaseClassAtIndex(uint32_t idx) override {
    return LLDBType::CreateSP(type_.GetVirtualBaseClassAtIndex(idx).GetType());
  }
  TypeSP GetEnumerationIntegerType(ParserContext&) override;
  bool IsEnumerationIntegerTypeSigned() override;
  MemberInfo GetFieldAtIndex(uint32_t idx) override {
    auto member = type_.GetFieldAtIndex(idx);
    auto name = member.GetName() ? std::string(member.GetName())
                                 : llvm::Optional<std::string>();
    return {name, LLDBType::CreateSP(member.GetType()), member.IsBitfield(),
            member.GetBitfieldSizeInBits()};
  }
  TypeSP GetSmartPtrPointeeType() override {
    assert(
        IsSmartPtrType() &&
        "the type should be a smart pointer (std::unique_ptr, std::shared_ptr "
        "or std::weak_ptr");
    return LLDBType::CreateSP(type_.GetTemplateArgumentType(0));
  }

  static std::shared_ptr<LLDBType> CreateSP(lldb::SBType type) {
    return std::make_shared<LLDBType>(type);
  }

 private:
  lldb::SBType type_;

  friend lldb::SBType ToSBType(TypeSP type);
};

class Value {
 public:
  Value() {}

  explicit Value(lldb::SBValue value) {
    value_ = value;
    type_ = LLDBType::CreateSP(value_.GetType());
  }

 public:
  bool IsValid() { return value_.IsValid(); }
  explicit operator bool() { return IsValid(); }

  lldb::SBValue inner_value() const { return value_; }
  std::shared_ptr<LLDBType> type() { return type_; }

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
  int64_t GetValueAsSigned();

  Value AddressOf();
  Value Dereference();

  llvm::APSInt GetInteger();
  llvm::APFloat GetFloat();

  Value Clone();
  void Update(const llvm::APInt& v);
  void Update(Value v);

 private:
  lldb::SBValue value_;
  std::shared_ptr<LLDBType> type_;
};

Value CastScalarToBasicType(lldb::SBTarget target, Value val, TypeSP type,
                            Error& error);

Value CastEnumToBasicType(lldb::SBTarget target, Value val, TypeSP type);

Value CastPointerToBasicType(lldb::SBTarget target, Value val, TypeSP type);

Value CastIntegerOrEnumToEnumType(lldb::SBTarget target, Value val,
                                  TypeSP type);

Value CastFloatToEnumType(lldb::SBTarget target, Value val, TypeSP type,
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

Value CreateValueNullptr(lldb::SBTarget target, lldb::SBType type);

inline lldb::SBType ToSBType(TypeSP type) {
  return static_cast<LLDBType&>(*type).type_;
}

}  // namespace lldb_eval

#endif  // LLDB_EVAL_VALUE_H_
