#ifndef INCLUDE_SYMBOL_TABLE_H_
#define INCLUDE_SYMBOL_TABLE_H_

#include <unordered_map>
#include <vector>

#include "ast.h"

namespace fuzzer {

class Field {
 public:
  Field(TaggedType containing_type, std::string name)
      : containing_type_(std::move(containing_type)), name_(std::move(name)) {}

  const TaggedType& containing_type() const { return containing_type_; }
  const std::string& name() const { return name_; }

 private:
  TaggedType containing_type_;
  std::string name_;
};

class SymbolTable {
 public:
  SymbolTable() = default;

  void add_var(Type type, VariableExpr var) {
    var_map_[std::move(type)].emplace_back(std::move(var));
  }

  const std::unordered_map<Type, std::vector<VariableExpr>>& vars() const {
    return var_map_;
  }

  void add_field(TaggedType containing_type, std::string field_name,
                 Type field_type) {
    fields_by_type_[std::move(field_type)].emplace_back(
        std::move(containing_type), std::move(field_name));
  }
  const std::unordered_map<Type, std::vector<Field>>& fields_by_type() const {
    return fields_by_type_;
  }

 private:
  std::unordered_map<Type, std::vector<VariableExpr>> var_map_;
  std::unordered_map<Type, std::vector<Field>> fields_by_type_;
};

}  // namespace fuzzer

#endif  // INCLUDE_SYMBOL_TABLE_H_
