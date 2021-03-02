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

#include "lldb/API/SBError.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBMemoryRegionInfo.h"
#include "lldb/API/SBMemoryRegionInfoList.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "lldb/API/SBVariablesOptions.h"
#include "tools/fuzzer/ast.h"

namespace fuzzer {
namespace {

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
    const auto inner_type =
        convert_type(type.GetPointeeType(), ignore_qualified_types);
    if (!inner_type.has_value()) {
      return {};
    }
    return PointerType(QualifiedType(std::move(inner_type.value())));
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

  return 0;
}

}  // namespace

// Creates a symbol table from the lldb context. It populates most of the
// basic type as well as pointers to basic types. Referenced types are
// dereferenced during the process and type qualifiers (const, volatile)
// are ignored.
SymbolTable SymbolTable::create_from_lldb_context(lldb::SBFrame& frame,
                                                  bool ignore_qualified_types) {
  SymbolTable symtab;

  lldb::SBVariablesOptions options;
  options.SetIncludeLocals(true);

  lldb::SBValueList variables = frame.GetVariables(options);
  uint32_t variables_size = variables.GetSize();

  lldb::SBMemoryRegionInfoList memory_regions =
      frame.GetThread().GetProcess().GetMemoryRegions();

  for (uint32_t i = 0; i < variables_size; ++i) {
    lldb::SBValue value = variables.GetValueAtIndex(i);

    auto maybe_type = convert_type(value.GetType(), ignore_qualified_types);
    if (maybe_type.has_value()) {
      symtab.add_var(maybe_type.value(), VariableExpr(value.GetName()),
                     calculate_freedom_index(value, memory_regions));
    }
  }

  return symtab;
}

}  // namespace fuzzer
