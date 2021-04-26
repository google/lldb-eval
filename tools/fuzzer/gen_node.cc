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

#include "tools/fuzzer/gen_node.h"

#include <variant>

#include "tools/fuzzer/expr_gen.h"

namespace fuzzer {

bool GenNode::mutate(ExprGenerator* generator) {
  // Don't mutate invalid nodes.
  if (!valid_) {
    return false;
  }

  fn_(generator, /*mutate*/ true);

  auto mutated_node = generator->node();
  if (mutated_node->is_valid()) {
    children_.swap(mutated_node->children_);
    return true;
  }

  return false;
}

void walk_gen_tree(GenNodePtr node, GenTreeVisitor* visitor, int depth) {
  visitor->visit_node(node);
  for (auto child : node->children()) {
    auto* as_int = std::get_if<unsigned int>(&child);
    if (as_int) {
      visitor->visit_random_value(*as_int);
    }
    auto* as_node = std::get_if<GenNodePtr>(&child);
    if (as_node) {
      walk_gen_tree(*as_node, visitor, depth + 1);
    }
  }
}

}  // namespace fuzzer
