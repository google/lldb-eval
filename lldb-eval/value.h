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
class LLDBType : public lldb::SBType, public Type {
 public:
  LLDBType() = default;
  LLDBType(const lldb::SBType& t) : lldb::SBType(t) {}

  uint64_t GetByteSize() override { return lldb::SBType::GetByteSize(); }
  uint32_t GetTypeFlags() override { return lldb::SBType::GetTypeFlags(); }
  bool IsArrayType() override { return lldb::SBType::IsArrayType(); }
  bool IsPointerType() override { return lldb::SBType::IsPointerType(); }
  bool IsReferenceType() override { return lldb::SBType::IsReferenceType(); }
  bool IsPolymorphicClass() override {
    return lldb::SBType::IsPolymorphicClass();
  }
  bool IsScopedEnum() override;
  bool IsAnonymousType() override { return lldb::SBType::IsAnonymousType(); }
  bool IsValid() const override { return lldb::SBType::IsValid(); }
  bool CompareTo(TypeSP other) override;
  llvm::StringRef GetName() override { return lldb::SBType::GetName(); }
  TypeSP GetArrayElementType() override {
    return CreateSP(lldb::SBType::GetArrayElementType());
  }
  TypeSP GetPointerType() override {
    return LLDBType::CreateSP(lldb::SBType::GetPointerType());
  }
  TypeSP GetPointeeType() override {
    return LLDBType::CreateSP(lldb::SBType::GetPointeeType());
  }
  TypeSP GetReferenceType() override {
    return LLDBType::CreateSP(lldb::SBType::GetReferenceType());
  }
  TypeSP GetDereferencedType() override {
    return LLDBType::CreateSP(lldb::SBType::GetDereferencedType());
  }
  TypeSP GetCanonicalType() override {
    return LLDBType::CreateSP(lldb::SBType::GetCanonicalType());
  }
  TypeSP GetUnqualifiedType() override {
    return LLDBType::CreateSP(lldb::SBType::GetUnqualifiedType());
  }
  lldb::BasicType GetBasicType() override {
    return lldb::SBType::GetBasicType();
  }
  lldb::TypeClass GetTypeClass() override {
    return lldb::SBType::GetTypeClass();
  }
  uint32_t GetNumberOfDirectBaseClasses() override {
    return lldb::SBType::GetNumberOfDirectBaseClasses();
  }
  uint32_t GetNumberOfVirtualBaseClasses() override {
    return lldb::SBType::GetNumberOfVirtualBaseClasses();
  }
  uint32_t GetNumberOfFields() override {
    return lldb::SBType::GetNumberOfFields();
  }
  BaseInfo GetDirectBaseClassAtIndex(uint32_t idx) override {
    auto member = lldb::SBType::GetDirectBaseClassAtIndex(idx);
    return {LLDBType::CreateSP(member.GetType()), member.GetOffsetInBytes()};
  }
  TypeSP GetVirtualBaseClassAtIndex(uint32_t idx) override {
    return LLDBType::CreateSP(
        lldb::SBType::GetVirtualBaseClassAtIndex(idx).GetType());
  }
  TypeSP GetEnumerationIntegerType(ParserContext&) override;
  bool IsEnumerationIntegerTypeSigned() override;
  MemberInfo GetFieldAtIndex(uint32_t idx) override {
    auto member = lldb::SBType::GetFieldAtIndex(idx);
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
    return LLDBType::CreateSP(lldb::SBType::GetTemplateArgumentType(0));
  }

  static std::shared_ptr<LLDBType> CreateSP(lldb::SBType type) {
    return std::make_shared<LLDBType>(type);
  }
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
  return static_cast<LLDBType&>(*type);
}

}  // namespace lldb_eval

#endif  // LLDB_EVAL_VALUE_H_
