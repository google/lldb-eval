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

#include <cassert>
#include <cstdint>
#include <cstdio>
#include <iostream>
#include <memory>
#include <sstream>

#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/expr_gen.h"
#include "tools/fuzzer/fixed_rng.h"
#include "tools/fuzzer/gen_node.h"
#include "tools/fuzzer/symbol_table.h"

using bazel::tools::cpp::runfiles::Runfiles;
using std::size_t;
using namespace fuzzer;

// Global variables that are initialized in `LLVMFuzzerInitialize`.
static lldb::SBDebugger g_debugger;
static lldb::SBFrame g_frame;
static fuzzer::SymbolTable g_symtab;

// Creates an expression generator using fixed configuration and symbol table.
ExprGenerator create_generator(std::unique_ptr<GeneratorRng> rng) {
  auto cfg = GenConfig();
  cfg.bin_op_mask[BinOp::Shl] = false;
  cfg.bin_op_mask[BinOp::Shr] = false;

  return ExprGenerator(std::move(rng), cfg, g_symtab);
}

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

// Generation tree visitor used to produce a byte sequence representing a
// libFuzzer input.
class GenNodeWriter : public GenTreeVisitor {
 public:
  explicit GenNodeWriter(ByteWriter& writer) : writer_(writer) {}

  void visit_byte(uint8_t value) { writer_.write_byte(value); }

 private:
  ByteWriter& writer_;
};

void write_node(std::shared_ptr<GenNode> root, ByteWriter& writer) {
  GenNodeWriter node_writer(writer);
  walk_gen_tree(root, &node_writer);
}

extern "C" size_t LLVMFuzzerCustomMutator(uint8_t* data, size_t size,
                                          size_t max_size, unsigned int seed) {
  LibfuzzerReader reader(data, size);
  auto fixed_generator =
      create_generator(std::make_unique<FixedGeneratorRng>(data, size));
  auto maybe_expr = fixed_generator.generate();
  assert(maybe_expr.has_value() && "Expression couldn't be generated!");

  std::mt19937 rng(seed);
  auto root = fixed_generator.node();
  auto mutable_node = pick_random_node(root, rng);

  auto random_generator =
      create_generator(std::make_unique<DefaultGeneratorRng>(rng()));
  if (!random_generator.mutate_gen_node(mutable_node)) {
    return size;
  }

  ByteWriter writer(data, max_size);
  write_node(root, writer);

  // It's possible that `root`'s sequence of random values overflows the size of
  // `data`. Overflowed values will be ignored. This isn't ideal, but also isn't
  // critical since expression generator is able to generate an expression from
  // any byte sequence.
  // TODO: Compare number of random values in the `root` to the `max_size`.

  return writer.size();
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  auto generator =
      create_generator(std::make_unique<FixedGeneratorRng>(data, size));
  auto maybe_expr = generator.generate();
  assert(maybe_expr.has_value() && "Expression couldn't be generated!");

  std::ostringstream os;
  os << maybe_expr.value();

  lldb::SBError error;
  lldb_eval::EvaluateExpression(g_frame, os.str().c_str(), error);
  return 0;
}

extern "C" int LLVMFuzzerInitialize(int* /*argc*/, char*** argv) {
  std::string err;
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create((*argv)[0], &err));
  if (runfiles == nullptr) {
    fprintf(stderr, "Could not launch the libfuzzer target: %s\n", err.c_str());
    return 1;
  }

  lldb_eval::SetupLLDBServerEnv(*runfiles);
  lldb::SBDebugger::Initialize();

  auto binary_path = runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary");
  auto source_path = runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary.cc");

  g_debugger = lldb::SBDebugger::Create(false);

  lldb::SBProcess process = lldb_eval::LaunchTestProgram(
      g_debugger, source_path, binary_path, "// BREAK HERE");

  g_frame = process.GetSelectedThread().GetSelectedFrame();

  g_symtab =
      fuzzer::SymbolTable::create_from_frame(g_frame,
                                             /*ignore_qualified_types*/ true);

  return 0;
}
