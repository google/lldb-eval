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

#ifndef INCLUDE_SYMBOL_TABLE_H_
#define INCLUDE_SYMBOL_TABLE_H_

#include <functional>
#include <unordered_map>
#include <unordered_set>
#include <vector>

#include "ast.h"
#include "lldb/API/SBFrame.h"

namespace fuzzer {

// A variable representation that contains "freedom index". The freedom index
// guarantees that the variable can be dereferenced a certain number of times.
// For example, if a variable `ptr` has a freedom index of 2, it means that
// expressions `*ptr`, `**ptr` or `***&ptr` are valid, while `***ptr` is not,
// as it may result in an invalid memory access.
struct VariableFreedomPair {
  VariableFreedomPair(VariableExpr expr, int freedom_index)
      : expr(std::move(expr)), freedom_index(freedom_index) {}

  VariableExpr expr;
  int freedom_index;
};

class Field {
 public:
  Field(TaggedType containing_type, std::string name,
        bool is_reference_or_virtual = false)
      : containing_type_(std::move(containing_type)),
        name_(std::move(name)),
        is_reference_or_virtual_(is_reference_or_virtual) {}

  const TaggedType& containing_type() const { return containing_type_; }
  const std::string& name() const { return name_; }
  bool is_reference_or_virtual() const { return is_reference_or_virtual_; }

 private:
  TaggedType containing_type_;
  std::string name_;
  bool is_reference_or_virtual_;
};

class Function {
 public:
  Function(std::string name, std::vector<Type> argument_types)
      : name_(std::move(name)), argument_types_(std::move(argument_types)) {}

  const std::string& name() const { return name_; }
  const std::vector<Type>& argument_types() const { return argument_types_; }

 private:
  std::string name_;
  std::vector<Type> argument_types_;
};

class SymbolTable {
 public:
  SymbolTable() = default;

  static SymbolTable create_from_frame(lldb::SBFrame& frame,
                                       bool ignore_qualified_types = false);

  static SymbolTable create_from_value(lldb::SBValue& value,
                                       bool ignore_qualified_types = false);

  void add_var(Type type, VariableExpr var, int freedom_index = 0) {
    var_map_[type].emplace_back(std::move(var), freedom_index);

    // Collect all array types contained in the `type`.
    while (std::holds_alternative<PointerType>(type) ||
           std::holds_alternative<ArrayType>(type)) {
      const auto* as_array = std::get_if<ArrayType>(&type);
      if (as_array != nullptr) {
        array_types_.insert(*as_array);
        type = as_array->type();
      }
      const auto* as_pointer = std::get_if<PointerType>(&type);
      if (as_pointer != nullptr) {
        type = as_pointer->type().type();
      }
    }
  }

  const std::unordered_map<Type, std::vector<VariableFreedomPair>>& vars()
      const {
    return var_map_;
  }

  void add_field(TaggedType containing_type, std::string field_name,
                 Type field_type, bool reference_or_virtual) {
    fields_by_type_[std::move(field_type)].emplace_back(
        containing_type, std::move(field_name), reference_or_virtual);

    tagged_types_.insert(std::move(containing_type));
  }

  void add_enum_literal(const EnumType& enum_type, std::string enum_literal) {
    enum_map_[enum_type].emplace_back(enum_type, std::move(enum_literal));
  }

  void add_function(Type return_type, std::string name,
                    std::vector<Type> argument_types) {
    function_map_[std::move(return_type)].emplace_back(
        std::move(name), std::move(argument_types));
  }

  const std::unordered_map<Type, std::vector<Field>>& fields_by_type() const {
    return fields_by_type_;
  }

  const std::unordered_map<Type, std::vector<Function>>& functions() const {
    return function_map_;
  }

  const std::unordered_map<EnumType, std::vector<EnumConstant>>& enums() const {
    return enum_map_;
  }

  const std::unordered_set<TaggedType>& tagged_types() const {
    return tagged_types_;
  }

  const std::unordered_set<ArrayType>& array_types() const {
    return array_types_;
  }

 private:
  std::unordered_map<Type, std::vector<VariableFreedomPair>> var_map_;
  std::unordered_map<Type, std::vector<Function>> function_map_;
  std::unordered_map<Type, std::vector<Field>> fields_by_type_;
  std::unordered_map<EnumType, std::vector<EnumConstant>> enum_map_;
  std::unordered_set<TaggedType> tagged_types_;
  std::unordered_set<ArrayType> array_types_;
};

}  // namespace fuzzer

#endif  // INCLUDE_SYMBOL_TABLE_H_
