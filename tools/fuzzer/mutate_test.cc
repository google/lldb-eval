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

#include <memory>
#include <optional>
#include <random>
#include <sstream>
#include <variant>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"
#include "tools/fuzzer/expr_gen.h"
#include "tools/fuzzer/fixed_rng.h"
#include "tools/fuzzer/gen_node.h"

using namespace fuzzer;
using namespace testing;
using bazel::tools::cpp::runfiles::Runfiles;

// Generation tree visitor used to randomly pick nodes.
class GenNodePicker : public GenTreeVisitor {
 public:
  void visit_node(std::shared_ptr<GenNode> node) {
    if (node->is_valid()) {
      options_.emplace_back(node);
    }
  }

  template <class Rng>
  std::shared_ptr<GenNode> pick(Rng& rng) {
    std::uniform_int_distribution<size_t> distr(0, options_.size() - 1);
    return options_[distr(rng)];
  }

 private:
  std::vector<std::shared_ptr<GenNode>> options_;
};

template <class Rng>
std::shared_ptr<GenNode> pick_random_node(std::shared_ptr<GenNode> root,
                                          Rng& rng) {
  GenNodePicker picker;
  walk_gen_tree(root, &picker);
  return picker.pick(rng);
}

// Generation tree visitor used to extract a byte sequence.
class SequenceBuilder : public GenTreeVisitor {
 public:
  void visit_byte(uint8_t value) { queue_.emplace_back(value); }

  std::vector<uint8_t>& queue() { return queue_; }

 private:
  std::vector<uint8_t> queue_;
};

std::vector<uint8_t> make_rng_sequence(std::shared_ptr<GenNode> root) {
  SequenceBuilder builder;
  walk_gen_tree(root, &builder);
  return builder.queue();
}

bool compare_gen_nodes(const std::shared_ptr<GenNode>& lhs,
                       const std::shared_ptr<GenNode>& rhs) {
  if (lhs->is_valid() != rhs->is_valid() || lhs->name() != rhs->name() ||
      lhs->children().size() != rhs->children().size()) {
    return false;
  }

  for (size_t i = 0; i < lhs->children().size(); ++i) {
    const auto& lhs_child = lhs->children()[i];
    const auto& rhs_child = rhs->children()[i];

    const auto* lhs_child_int = std::get_if<uint8_t>(&lhs_child);
    const auto* rhs_child_int = std::get_if<uint8_t>(&rhs_child);
    if ((lhs_child_int == nullptr) != (rhs_child_int == nullptr)) {
      return false;
    }
    if (lhs_child_int && rhs_child_int && *lhs_child_int != *rhs_child_int) {
      return false;
    }

    const auto* lhs_child_node =
        std::get_if<std::shared_ptr<GenNode>>(&lhs_child);
    const auto* rhs_child_node =
        std::get_if<std::shared_ptr<GenNode>>(&rhs_child);
    if ((lhs_child_node == nullptr) != (rhs_child_node == nullptr)) {
      return false;
    }
    if (lhs_child_node && rhs_child_node &&
        !compare_gen_nodes(*lhs_child_node, *rhs_child_node)) {
      return false;
    }
  }
  return true;
}

TEST(MutateTest, MutateMultipleTimes) {
  // Set up the test.
  std::unique_ptr<Runfiles> runfiles(Runfiles::CreateForTest());
  lldb_eval::SetupLLDBServerEnv(*runfiles);
  lldb::SBDebugger::Initialize();
  auto binary_path = runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary");
  auto source_path = runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary.cc");
  auto debugger = lldb::SBDebugger::Create(false);
  auto process = lldb_eval::LaunchTestProgram(debugger, source_path,
                                              binary_path, "// BREAK HERE");
  auto frame = process.GetSelectedThread().GetSelectedFrame();

  // Configuration for generating expressions.
  auto cfg = GenConfig();

  // Create a symbol table from LLDB context.
  SymbolTable symtab = SymbolTable::create_from_frame(frame);

  // Create a random expression generator.
  ExprGenerator random_generator(std::make_unique<DefaultGeneratorRng>(1337),
                                 cfg, symtab);

  // Create an initial expression.
  std::optional<Expr> maybe_expr;
  do {
    maybe_expr = random_generator.generate();
  } while (!maybe_expr.has_value());

  // Get the root gen-node of created expression.
  std::shared_ptr<GenNode> root = random_generator.node();

  // Rng used for randomly selecting gen-nodes for mutation.
  std::mt19937 rng(12345);

  // Perform mutation multiple times.
  for (int i = 0; i < 500; ++i) {
    // Pick a random node.
    std::shared_ptr<GenNode> to_be_mutated = pick_random_node(root, rng);
    ASSERT_NE(to_be_mutated, nullptr);

    // Re-evaluate the node.
    random_generator.mutate_gen_node(to_be_mutated);

    // Construct expression generator over the fixed byte sequence.
    std::vector<uint8_t> bytes = make_rng_sequence(root);
    ExprGenerator fixed_generator(
        std::make_unique<FixedGeneratorRng>(bytes.data(), bytes.size()), cfg,
        symtab);

    // Fixed expression generator should be able to generate an expression.
    maybe_expr = fixed_generator.generate();
    ASSERT_NE(maybe_expr, std::nullopt);

    // Compare mutated root gen-node with the root gen-node obtained by running
    // the expression generator over the fixed byte sequence.
    ASSERT_TRUE(compare_gen_nodes(root, fixed_generator.node()));
  }

  // Teardown the test.
  process.Destroy();
  lldb::SBDebugger::Terminate();
}
