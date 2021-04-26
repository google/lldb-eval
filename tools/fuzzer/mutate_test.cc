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
#include <queue>
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
#include "tools/fuzzer/gen_node.h"

using namespace fuzzer;
using namespace testing;
using bazel::tools::cpp::runfiles::Runfiles;

// Random number generator over fixed number sequence.
class FixedRng : public BaseRng {
 public:
  explicit FixedRng(std::queue<BaseRng::result_type> q)
      : queue_(std::move(q)) {}

  BaseRng::result_type operator()() {
    assert(!queue_.empty());
    auto value = queue_.front();
    queue_.pop();
    return consume(value);
  }

 private:
  std::queue<BaseRng::result_type> queue_;
};

// Generation tree visitor used to randomly pick nodes.
class GenNodePicker : public GenTreeVisitor {
 public:
  void visit_node(GenNodePtr node) {
    if (node->is_valid()) {
      options_.emplace_back(node);
    }
  }

  template <class Rng>
  GenNodePtr pick(Rng& rng) {
    std::uniform_int_distribution<size_t> distr(0, options_.size() - 1);
    return options_[distr(rng)];
  }

 private:
  std::vector<GenNodePtr> options_;
};

template <class Rng>
GenNodePtr pick_random_node(GenNodePtr root, Rng& rng) {
  GenNodePicker picker;
  walk_gen_tree(root, &picker);
  return picker.pick(rng);
}

// Generation tree visitor used to extract a sequence produced by RNG.
class SequenceBuilder : public GenTreeVisitor {
 public:
  void visit_random_value(unsigned int value) { queue_.push(value); }

  std::queue<unsigned int>& queue() { return queue_; }

 private:
  std::queue<unsigned int> queue_;
};

std::queue<BaseRng::result_type> make_rng_sequence(GenNodePtr root) {
  SequenceBuilder builder;
  walk_gen_tree(root, &builder);
  return builder.queue();
}

bool compare_gen_nodes(const GenNodePtr& lhs, const GenNodePtr& rhs) {
  if (lhs->is_valid() != rhs->is_valid() || lhs->name() != rhs->name() ||
      lhs->children().size() != rhs->children().size()) {
    return false;
  }

  for (size_t i = 0; i < lhs->children().size(); ++i) {
    const auto& lhs_child = lhs->children()[i];
    const auto& rhs_child = rhs->children()[i];

    const auto* lhs_child_int = std::get_if<unsigned int>(&lhs_child);
    const auto* rhs_child_int = std::get_if<unsigned int>(&rhs_child);
    if ((lhs_child_int == nullptr) != (rhs_child_int == nullptr)) {
      return false;
    }
    if (lhs_child_int && rhs_child_int && *lhs_child_int != *rhs_child_int) {
      return false;
    }

    const auto& lhs_child_node = std::get_if<GenNodePtr>(&lhs_child);
    const auto& rhs_child_node = std::get_if<GenNodePtr>(&lhs_child);
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
  ExprGenerator random_generator(
      std::make_unique<DefaultGeneratorRng>(std::make_unique<Mt19937>(1337)),
      cfg, symtab);

  // Create an initial expression.
  std::optional<Expr> maybe_expr;
  do {
    maybe_expr = random_generator.generate();
  } while (!maybe_expr.has_value());

  // Get the root gen-node of created expression.
  GenNodePtr root = random_generator.node();

  // Rng used for randomly selecting gen-nodes for mutation.
  std::mt19937 rng(1337);

  // Perform mutation multiple times.
  for (int i = 0; i < 2000; ++i) {
    // Pick a random node.
    GenNodePtr to_be_mutated = pick_random_node(root, rng);
    ASSERT_NE(to_be_mutated, nullptr);

    // Re-evaluate the node.
    to_be_mutated->mutate(&random_generator);

    // Construct expression generator over the fixed rng sequence.
    std::queue<unsigned int> rng_sequence = make_rng_sequence(root);
    ExprGenerator fixed_generator(
        std::make_unique<DefaultGeneratorRng>(
            std::make_unique<FixedRng>(std::move(rng_sequence))),
        cfg, symtab);

    // Fixed expression generator should be able to generate an expression.
    maybe_expr = fixed_generator.generate();
    ASSERT_NE(maybe_expr, std::nullopt);

    // Compare mutated root gen-node with the root gen-node obtained by running
    // the expression generator over the fixed random sequence.
    ASSERT_TRUE(compare_gen_nodes(root, fixed_generator.node()));
  }

  // Teardown the test.
  process.Destroy();
  lldb::SBDebugger::Terminate();
}
