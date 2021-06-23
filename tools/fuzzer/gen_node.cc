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

void walk_gen_tree(std::shared_ptr<GenNode> node, GenTreeVisitor* visitor) {
  visitor->visit_node(node);
  for (auto child : node->children()) {
    auto* as_byte = std::get_if<uint8_t>(&child);
    if (as_byte) {
      visitor->visit_byte(*as_byte);
    }
    auto* as_node = std::get_if<std::shared_ptr<GenNode>>(&child);
    if (as_node) {
      walk_gen_tree(*as_node, visitor);
    }
  }
}

}  // namespace fuzzer
