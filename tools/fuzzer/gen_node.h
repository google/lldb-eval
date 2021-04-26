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

struct GenConfig;

class GenNode;
using GenNodePtr = std::shared_ptr<GenNode>;
using GenNodeOrInt = std::variant<GenNodePtr, unsigned int>;

class ExprGenerator;
using GenerateMutateFn =
    std::function<std::optional<Expr>(ExprGenerator*, bool)>;

class GenNode {
 public:
  GenNode(std::string name, GenerateMutateFn fn, const GenConfig& cfg,
          const SymbolTable& symtab)
      : name_(std::move(name)),
        fn_(std::move(fn)),
        cfg_(cfg),
        symtab_(symtab) {}

  const std::string& name() const { return name_; }

  const std::vector<GenNodeOrInt>& children() const { return children_; }

  bool is_valid() const { return valid_; }

  void add_child(GenNodeOrInt child) { children_.push_back(child); }

  void set_valid(bool valid) { valid_ = valid; }

  bool mutate(ExprGenerator* generator);

 private:
  std::string name_;
  GenerateMutateFn fn_;
  bool valid_ = false;

  const GenConfig& cfg_;
  const SymbolTable& symtab_;

  std::vector<GenNodeOrInt> children_;
};

class GenTreeVisitor {
 public:
  virtual ~GenTreeVisitor() {}

  virtual void visit_node(GenNodePtr node) {}
  virtual void visit_random_value(unsigned int value) {}
};

void walk_gen_tree(GenNodePtr node, GenTreeVisitor* visitor, int depth = 0);

}  // namespace fuzzer

#endif  // INCLUDE_GEN_NODE_H
