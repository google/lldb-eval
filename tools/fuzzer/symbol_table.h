#ifndef INCLUDE_SYMBOL_TABLE_H_
#define INCLUDE_SYMBOL_TABLE_H_

#include <unordered_map>
#include <vector>

#include "ast.h"

namespace fuzzer {

class SymbolTable {
 public:
  SymbolTable() = default;

  void add_vars(Type type, std::vector<VariableExpr> vars);
  const std::unordered_map<Type, std::vector<VariableExpr>>& vars() const;

 private:
  std::unordered_map<Type, std::vector<VariableExpr>> var_map_;
};

}  // namespace fuzzer

#endif  // INCLUDE_SYMBOL_TABLE_H_
