#ifndef INCLUDE_SYMBOL_TABLE_H_
#define INCLUDE_SYMBOL_TABLE_H_

#include <unordered_map>
#include <vector>

#include "ast.h"

namespace fuzzer {

class Field {
 public:
  Field(TaggedType type, std::string field)
      : type_(std::move(type)), field_(std::move(field)) {}

  const TaggedType& type() const { return type_; }
  const std::string& field() const { return field_; }

 private:
  TaggedType type_;
  std::string field_;
};

class SymbolTable {
 public:
  using VarsContainer = std::unordered_multimap<Type, VariableExpr>;
  using FieldsContainer = std::unordered_multimap<Type, Field>;

  SymbolTable() = default;

  void add_var(Type type, VariableExpr var);
  const std::unordered_multimap<Type, VariableExpr>& vars() const {
    return var_map_;
  }

  void add_field(TaggedType tagged_type, std::string field_name,
                 Type field_type);
  const std::unordered_multimap<Type, Field>& fields_by_type() const {
    return fields_by_type_;
  }

 private:
  std::unordered_multimap<Type, VariableExpr> var_map_;
  std::unordered_multimap<Type, Field> fields_by_type_;
};

}  // namespace fuzzer

#endif  // INCLUDE_SYMBOL_TABLE_H_
