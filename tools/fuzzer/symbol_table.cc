#include "symbol_table.h"

#include <cassert>

namespace fuzzer {

void SymbolTable::add_var(Type type, VariableExpr var) {
  var_map_.emplace(std::move(type), std::move(var));
}

void SymbolTable::add_field(TaggedType tagged_type, std::string field_name,
                            Type field_type) {
  fields_by_type_.emplace(std::move(field_type),
                          Field(std::move(tagged_type), std::move(field_name)));
}

}  // namespace fuzzer
