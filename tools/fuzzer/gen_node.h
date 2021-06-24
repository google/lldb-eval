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

#ifndef INCLUDE_GEN_NODE_H
#define INCLUDE_GEN_NODE_H

#include <memory>
#include <optional>
#include <variant>
#include <vector>

#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/symbol_table.h"

namespace fuzzer {

class GenNode;
class ExprGenerator;

using GenNodeOrByte = std::variant<std::shared_ptr<GenNode>, uint8_t>;
using GenerateExprFn = std::function<std::optional<Expr>(ExprGenerator*)>;

// A node that represents one expression generation method of `ExprGenerator`
// class (e.g. `gen_integer_constant`, `gen_binary_expr`, `gen_with_weights`,
// etc.) with context necessary to re-evaluate the method. A tree formed of
// these nodes represents a call hierarchy starting from the root method.
class GenNode {
 public:
  GenNode(std::string name, GenerateExprFn callback)
      : name_(std::move(name)), callback_(std::move(callback)) {}

  // Name of the method (e.g. "fuzzer::ExprGenerator::gen_binary_expr").
  // Useful for testing and debugging.
  const std::string& name() const { return name_; }

  // List of children. A child is either another method generation node or a
  // byte representing a part of serialized format.
  const std::vector<GenNodeOrByte>& children() const { return children_; }

  // Does the method result with a valid expression or a `std::nullopt`?
  bool is_valid() const { return valid_; }

 private:
  friend class ExprGenerator;

  std::string name_;
  GenerateExprFn callback_;  // Callback for re-evaluating the method.
  bool valid_ = false;
  std::vector<GenNodeOrByte> children_;
};

class GenTreeVisitor {
 public:
  virtual ~GenTreeVisitor() {}

  virtual void visit_node(std::shared_ptr<GenNode>) {}
  virtual void visit_byte(uint8_t) {}
};

// Recursively visits nodes and random values contained in the `node`.
void walk_gen_tree(std::shared_ptr<GenNode> node, GenTreeVisitor* visitor);

}  // namespace fuzzer

#endif  // INCLUDE_GEN_NODE_H
