#include "symbol_table.h"

#include <cassert>

namespace fuzzer {

void SymbolTable::add_vars(Type type, std::vector<VariableExpr> vars) {
  assert(var_map_.count(type) == 0);

  var_map_.emplace(std::move(type), std::move(vars));
}

const std::unordered_map<Type, std::vector<VariableExpr>>& SymbolTable::vars()
    const {
  return var_map_;
}

}  // namespace fuzzer
