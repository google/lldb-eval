/*
 * Copyright 2021 Google LLC
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

#include "tools/fuzzer/symbol_table.h"

#include <cstring>
#include <optional>
#include <string>

#include "lldb-eval/traits.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBMemoryRegionInfo.h"
#include "lldb/API/SBMemoryRegionInfoList.h"
#include "lldb/API/SBModule.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBTypeEnumMember.h"
#include "lldb/API/SBValue.h"
#include "lldb/API/SBVariablesOptions.h"
#include "tools/fuzzer/ast.h"

namespace fuzzer {
namespace {

// Guesses type qualifiers depending on difference of name length of type and
// unqualified version of type (unfortunately, there isn't a convenient way to
// get type qualifiers in LLDB API).
CvQualifiers guess_cv_qualifiers(lldb::SBType& type) {
  const size_t len_diff =
      strlen(type.GetName()) - strlen(type.GetUnqualifiedType().GetName());

  if (len_diff == 5 || len_diff == 6) {
    return CvQualifier::Const;
  }

  if (len_diff == 8 || len_diff == 9) {
    return CvQualifier::Volatile;
  }

  if (len_diff == 14 || len_diff == 15) {
    return CvQualifiers::all_set();
  }

  return CvQualifiers();
}

bool is_tagged_type(lldb::SBType& type) {
  if (type.GetNumberOfTemplateArguments() != 0) {
    // LLDB doesn't work well with template types.
    return false;
  }
  return type.GetTypeClass() == lldb::eTypeClassStruct ||
         type.GetTypeClass() == lldb::eTypeClassClass;
}

template <typename T>
bool is_scoped_enum(T type) {
  if constexpr (HAS_METHOD(T, IsScopedEnumerationType())) {
    return type.IsScopedEnumerationType();
  }
  return false;
}

std::optional<Type> convert_type(lldb::SBType type,
                                 bool ignore_qualified_types) {
  type = type.GetCanonicalType();

  // There isn't a convenient way to get type qualifiers of lldb::SBType.
  if (ignore_qualified_types &&
      strcmp(type.GetName(), type.GetUnqualifiedType().GetName()) != 0) {
    return {};
  }

  if (type.IsReferenceType()) {
    // Currenty, the fuzzer doesn't support reference types.
    type = type.GetDereferencedType();
  }

  if (type.IsPointerType()) {
    auto pointee_type = type.GetPointeeType();
    const auto inner_type = convert_type(pointee_type, ignore_qualified_types);
    if (!inner_type.has_value()) {
      return {};
    }
    return PointerType(QualifiedType(std::move(inner_type.value()),
                                     guess_cv_qualifiers(pointee_type)));
  }

  if (type.IsArrayType()) {
    auto element_type = type.GetArrayElementType();
    const auto inner_type = convert_type(element_type, ignore_qualified_types);
    if (!inner_type.has_value()) {
      return {};
    }
    // We have to calculate the array size manually.
    uint64_t size = type.GetByteSize() / element_type.GetByteSize();
    return ArrayType(std::move(inner_type.value()), size);
  }

  if (is_tagged_type(type)) {
    return TaggedType(type.GetName());
  }

  if (type.GetTypeClass() == lldb::eTypeClassEnumeration) {
    return EnumType(type.GetName(), is_scoped_enum(type));
  }

  const lldb::BasicType basic_type = type.GetBasicType();

  switch (basic_type) {
    case lldb::eBasicTypeVoid:
      return ScalarType::Void;
    case lldb::eBasicTypeChar:
      return ScalarType::Char;
    case lldb::eBasicTypeSignedChar:
      // Definition of char is compiler-dependent and LLDB seems to return
      // eBasicTypeSignedChar for the char type. To improve type conversion,
      // we explicitly check if there is a "signed" keyword in the string
      // representation.
      if (std::string(type.GetName()).find("signed") == std::string::npos) {
        return ScalarType::Char;
      }
      return ScalarType::SignedChar;
    case lldb::eBasicTypeUnsignedChar:
      if (std::string(type.GetName()).find("unsigned") == std::string::npos) {
        return ScalarType::Char;
      }
      return ScalarType::UnsignedChar;
    case lldb::eBasicTypeShort:
      return ScalarType::SignedShort;
    case lldb::eBasicTypeUnsignedShort:
      return ScalarType::UnsignedShort;
    case lldb::eBasicTypeInt:
      return ScalarType::SignedInt;
    case lldb::eBasicTypeUnsignedInt:
      return ScalarType::UnsignedInt;
    case lldb::eBasicTypeLong:
      return ScalarType::SignedLong;
    case lldb::eBasicTypeUnsignedLong:
      return ScalarType::UnsignedLong;
    case lldb::eBasicTypeLongLong:
      return ScalarType::SignedLongLong;
    case lldb::eBasicTypeUnsignedLongLong:
      return ScalarType::UnsignedLongLong;
    case lldb::eBasicTypeBool:
      return ScalarType::Bool;
    case lldb::eBasicTypeFloat:
      return ScalarType::Float;
    case lldb::eBasicTypeDouble:
      return ScalarType::Double;
    case lldb::eBasicTypeLongDouble:
      return ScalarType::LongDouble;
    case lldb::eBasicTypeNullPtr:
      return NullptrType{};

    default:
      return {};
  }
}

bool is_valid_address(lldb::addr_t address,
                      lldb::SBMemoryRegionInfoList& regions) {
  for (size_t i = 0; i < regions.GetSize(); ++i) {
    lldb::SBMemoryRegionInfo region;
    if (!regions.GetMemoryRegionAtIndex(i, region) || !region.IsReadable()) {
      continue;
    }
    if (address >= region.GetRegionBase() && address < region.GetRegionEnd()) {
      return true;
    }
  }
  return false;
}

// Calculates freedom index of the given variable, i.e. a number of times the
// variable can be dereferenced (it ignores references).
int calculate_freedom_index(lldb::SBValue value,
                            lldb::SBMemoryRegionInfoList& memory_regions) {
  auto type = value.GetType().GetCanonicalType();
  if (type.IsReferenceType()) {
    value = value.Dereference();
    type = value.GetType().GetCanonicalType();
  }

  if (type.IsPointerType()) {
    lldb::addr_t address =
        static_cast<lldb::addr_t>(value.GetValueAsUnsigned());
    if (is_valid_address(address, memory_regions)) {
      return 1 + calculate_freedom_index(value.Dereference(), memory_regions);
    }
  }

  if (type.IsArrayType()) {
    lldb::addr_t address =
        static_cast<lldb::addr_t>(value.AddressOf().GetValueAsUnsigned());
    if (is_valid_address(address, memory_regions)) {
      // The first array element.
      lldb::SBValue element_value = value.GetChildAtIndex(0);
      return 1 + calculate_freedom_index(element_value, memory_regions);
    }
  }

  return 0;
}

// Fix variable/field names returned by LLDB API. E.g. name is sometimes in
// the form of "type name" or "type *name", so it ignores everything in front of
// the last occurrence of ' ' (space), '*' or '&'.
const char* fix_name(const char* name) {
  // Find the last occurrence of ' ', '*' or '&' in the `name`.
  const char* last_occurrence = nullptr;
  for (const char* c = name; *c != '\0'; ++c) {
    if (*c == ' ' || *c == '*' || *c == '&') {
      last_occurrence = c;
    }
  }
  if (last_occurrence != nullptr) {
    return last_occurrence + 1;
  }
  return name;
}

// A helper class that analyzes relations among structs and classes and collects
// necessary field information.
class ClassAnalyzer {
 public:
  struct FieldInfo {
    std::string name;
    Type type;
    uint32_t id;
    bool is_virtual;  // Is it a virtually inherited field?

    FieldInfo(std::string name, Type type, uint32_t id, bool is_virtual = false)
        : name(std::move(name)),
          type(std::move(type)),
          id(id),
          is_virtual(is_virtual) {}

    friend bool operator==(const FieldInfo& lhs, const FieldInfo& rhs) {
      return lhs.id == rhs.id && lhs.is_virtual == rhs.is_virtual &&
             lhs.name == rhs.name && lhs.type == rhs.type;
    }

    // Needed for std::unordered_set.
    struct Hash {
      size_t operator()(const FieldInfo& field_info) const {
        return field_info.id;
      }
    };
  };

  class ClassInfo {
   public:
    using FieldSet = std::unordered_set<FieldInfo, FieldInfo::Hash>;

    ClassInfo() = default;
    explicit ClassInfo(FieldSet fields) : fields_(std::move(fields)) {
      for (const auto& field : fields_) {
        num_field_names_[field.name]++;
      }
    }

    bool is_unique_field_name(const std::string& field_name) const {
      auto it = num_field_names_.find(field_name);
      return it != num_field_names_.end() && it->second == 1;
    }

    const FieldSet& fields() const { return fields_; }

   private:
    FieldSet fields_;
    std::unordered_map<std::string, size_t> num_field_names_;
  };

  explicit ClassAnalyzer(bool ignore_qualified_types)
      : ignore_qualified_types_(ignore_qualified_types) {}

  const ClassInfo& get_class_info(lldb::SBType type) {
    return get_cached_class_info(type);
  }

 private:
  const ClassInfo& get_cached_class_info(lldb::SBType type) {
    const std::string type_name = type.GetName();
    if (!is_tagged_type(type) ||
        cached_types_.find(type_name) != cached_types_.end()) {
      return cached_types_[type_name];
    }

    ClassInfo::FieldSet fields;

    // Collect fields declared in the `type`.
    for (uint32_t i = 0; i < type.GetNumberOfFields(); ++i) {
      lldb::SBTypeMember field = type.GetFieldAtIndex(i);
      auto maybe_type = convert_type(field.GetType(), ignore_qualified_types_);
      if (maybe_type.has_value()) {
        fields.emplace(fix_name(field.GetName()), std::move(maybe_type.value()),
                       next_field_id());
      }
    }

    // SBType::GetDirectBaseClass includes both virtual and non-virtual base
    // types. This set is used to store virtual base types in order to skip
    // them during processing of non-virtual base types.
    std::unordered_set<std::string> virtually_inherited;

    // Collect fields from virtual base types.
    for (uint32_t i = 0; i < type.GetNumberOfVirtualBaseClasses(); ++i) {
      lldb::SBType base_type = type.GetVirtualBaseClassAtIndex(i).GetType();
      virtually_inherited.insert(base_type.GetName());
      const auto& base_class_info = get_cached_class_info(base_type);
      for (auto field : base_class_info.fields()) {
        field.is_virtual = true;
        fields.insert(std::move(field));
      }
    }

    // Collect fields from non-virtual base types.
    for (uint32_t i = 0; i < type.GetNumberOfDirectBaseClasses(); ++i) {
      lldb::SBType base_type = type.GetDirectBaseClassAtIndex(i).GetType();
      if (virtually_inherited.find(base_type.GetName()) !=
          virtually_inherited.end()) {
        // Skip virtual base types.
        continue;
      }
      const auto& base_class_info = get_cached_class_info(base_type);
      for (auto field : base_class_info.fields()) {
        if (!field.is_virtual) {
          field.id = next_field_id();
        }
        fields.insert(std::move(field));
      }
    }

    return cached_types_[type_name] = ClassInfo(std::move(fields));
  }

  uint32_t next_field_id() { return next_field_id_++; }

  bool ignore_qualified_types_;
  uint32_t next_field_id_ = 0;
  std::unordered_map<std::string, ClassInfo> cached_types_;
};

void load_frame_variables(SymbolTable& symtab, lldb::SBFrame& frame,
                          lldb::SBMemoryRegionInfoList& memory_regions,
                          bool ignore_qualified_types,
                          bool include_local_vars) {
  lldb::SBVariablesOptions options;
  options.SetIncludeLocals(include_local_vars);
  options.SetIncludeStatics(true);

  lldb::SBValueList variables = frame.GetVariables(options);
  uint32_t variables_size = variables.GetSize();

  for (uint32_t i = 0; i < variables_size; ++i) {
    lldb::SBValue value = variables.GetValueAtIndex(i);
    auto maybe_type = convert_type(value.GetType(), ignore_qualified_types);
    if (maybe_type.has_value()) {
      symtab.add_var(maybe_type.value(),
                     VariableExpr(fix_name(value.GetName())),
                     calculate_freedom_index(value, memory_regions));
    }
  }
}

// Loads structs, classes and enumerations.
ClassAnalyzer load_tagged_types(SymbolTable& symtab, lldb::SBFrame& frame,
                                bool ignore_qualified_types) {
  ClassAnalyzer classes(ignore_qualified_types);

  lldb::SBTypeList types = frame.GetModule().GetTypes(
      lldb::eTypeClassStruct | lldb::eTypeClassClass |
      lldb::eTypeClassEnumeration);
  uint32_t types_size = types.GetSize();

  for (uint32_t i = 0; i < types_size; ++i) {
    lldb::SBType type = types.GetTypeAtIndex(i);

    // Structs and classes.
    if (is_tagged_type(type)) {
      const auto tagged_type = TaggedType(type.GetName());
      const auto& info = classes.get_class_info(type);
      for (const auto& field : info.fields()) {
        if (info.is_unique_field_name(field.name)) {
          symtab.add_field(tagged_type, field.name, field.type);
        }
      }
    }

    // Enumerations.
    if (type.GetTypeClass() == lldb::eTypeClassEnumeration) {
      const auto enum_type = EnumType(type.GetName(), is_scoped_enum(type));
      lldb::SBTypeEnumMemberList members = type.GetEnumMembers();
      for (uint32_t i = 0; i < members.GetSize(); ++i) {
        lldb::SBTypeEnumMember member = members.GetTypeEnumMemberAtIndex(i);
        symtab.add_enum_literal(enum_type, member.GetName());
      }
    }
  }

  return classes;
}

}  // namespace

// Creates a symbol table from the `frame`. It populates local and global
// (static) variables of the following types: basic types, structs, classes
// and pointers. Reference variables are imported, but treated as
// non-references.
SymbolTable SymbolTable::create_from_frame(lldb::SBFrame& frame,
                                           bool ignore_qualified_types) {
  SymbolTable symtab;

  lldb::SBMemoryRegionInfoList memory_regions =
      frame.GetThread().GetProcess().GetMemoryRegions();

  load_frame_variables(symtab, frame, memory_regions, ignore_qualified_types,
                       /*include_local_vars*/ true);
  load_tagged_types(symtab, frame, ignore_qualified_types);

  return symtab;
}

// Creates a symbol table from the `value`. The `value` has to be object of
// a struct or class. It populates global (static) variables and fields of
// the `value` of the following types: basic types, structs, classes and
// pointers.
SymbolTable SymbolTable::create_from_value(lldb::SBValue& value,
                                           bool ignore_qualified_types) {
  lldb::SBType value_type = value.GetType();
  if (!is_tagged_type(value_type)) {
    return SymbolTable();
  }

  SymbolTable symtab;

  lldb::SBFrame frame = value.GetFrame();
  lldb::SBMemoryRegionInfoList memory_regions =
      frame.GetThread().GetProcess().GetMemoryRegions();

  load_frame_variables(symtab, frame, memory_regions, ignore_qualified_types,
                       /*include_local_vars*/ false);

  ClassAnalyzer classes =
      load_tagged_types(symtab, frame, ignore_qualified_types);

  const auto tagged_type = TaggedType(value_type.GetName());
  const auto& value_type_info = classes.get_class_info(value_type);

  for (const auto& field : value_type_info.fields()) {
    if (value_type_info.is_unique_field_name(field.name)) {
      // TODO: `GetChildMemberWithName` may not work for virtually inherited
      // fields. We should find another way of looking those fields up.
      lldb::SBValue member = value.GetChildMemberWithName(field.name.c_str());
      if (!member.IsValid()) {
        continue;
      }
      symtab.add_var(field.type, VariableExpr(field.name),
                     calculate_freedom_index(member, memory_regions));
    }
  }

  // Add "this" as additional variable.
  symtab.add_var(PointerType(QualifiedType(tagged_type)), VariableExpr("this"),
                 /*freedom_index*/ 1);

  return symtab;
}

}  // namespace fuzzer
