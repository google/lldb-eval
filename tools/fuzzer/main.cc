/*
 * Copyright 2020 Google LLC
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

#include <algorithm>
#include <cstdint>
#include <cstdio>
#include <cstring>
#include <random>
#include <sstream>
#include <string>
#include <utility>

#include "cpp-linenoise/linenoise.hpp"
#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBBreakpoint.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBValue.h"
#include "tools/cpp/runfiles/runfiles.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/expr_gen.h"

using bazel::tools::cpp::runfiles::Runfiles;

static constexpr char SOURCE_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary.cc";
static constexpr char BINARY_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary";

enum Verbosity {
  ShowMismatchesOrErrors,
  ShowEverything,
};

void eval_and_print_expr(lldb::SBFrame& frame, const std::string& expr,
                         Verbosity verbosity) {
  auto lldb_value = frame.EvaluateExpression(expr.c_str());
  auto lldb_err = lldb_value.GetError();

  lldb::SBError lldb_eval_err;
  auto lldb_eval_value =
      lldb_eval::EvaluateExpression(frame, expr.c_str(), lldb_eval_err);

  bool value_mismatch;
  if (lldb_value.GetValue() != nullptr &&
      lldb_eval_value.GetValue() != nullptr) {
    value_mismatch =
        strcmp(lldb_value.GetValue(), lldb_eval_value.GetValue()) != 0;
  } else {
    // Pointer comparison: Mismatch if one value is null and the other is not
    value_mismatch = lldb_value.GetValue() != lldb_eval_value.GetValue();
  }

  bool type_mismatch;
  if (lldb_value.GetTypeName() != nullptr &&
      lldb_eval_value.GetTypeName() != nullptr) {
    type_mismatch =
        strcmp(lldb_value.GetTypeName(), lldb_eval_value.GetTypeName()) != 0;
  } else {
    // Pointer comparison: Mismatch if one type is null and the other is not
    type_mismatch = lldb_value.GetTypeName() != lldb_eval_value.GetTypeName();
  }
  bool has_error =
      lldb_err.GetCString() != nullptr || lldb_eval_err.GetCString() != nullptr;

  bool must_print = value_mismatch || type_mismatch || has_error ||
                    verbosity == Verbosity::ShowEverything;
  if (!must_print) {
    return;
  }
  printf("expr : `%s`\n", expr.c_str());

  if (value_mismatch) {
    if (lldb_value.GetValue() != nullptr) {
      printf("lldb value     : `%s`\n", lldb_value.GetValue());
    } else {
      printf("lldb value     : No value returned\n");
    }
    if (lldb_eval_value.GetValue() != nullptr) {
      printf("lldb-eval value: `%s`\n", lldb_eval_value.GetValue());
    } else {
      printf("lldb-eval value: No value returned\n");
    }
  } else if (verbosity == Verbosity::ShowEverything) {
    printf("value: `%s`\n", lldb_value.GetValue());
  }

  if (type_mismatch) {
    if (lldb_value.GetTypeName() != nullptr) {
      printf("lldb type     : `%s`\n", lldb_value.GetTypeName());
    } else {
      printf("lldb type     : No type name\n");
    }
    if (lldb_eval_value.GetTypeName() != nullptr) {
      printf("lldb-eval type: `%s`\n", lldb_eval_value.GetTypeName());
    } else {
      printf("lldb-eval type: No type name\n");
    }
  } else if (verbosity == Verbosity::ShowEverything) {
    printf("type: `%s`\n", lldb_value.GetTypeName());
  }

  if (has_error) {
    printf("== Reported errors ==\n");
    if (lldb_err.GetCString() != nullptr) {
      printf("lldb     : %s\n", lldb_err.GetCString());
    } else {
      printf("lldb     : No error reported\n");
    }

    if (lldb_eval_err.GetCString() != nullptr) {
      printf("lldb-eval: %s\n", lldb_eval_err.GetCString());
    } else {
      printf("lldb-eval: No error reported\n");
    }
  }

  printf("============================================================\n");
}

void run_repl(lldb::SBFrame& frame) {
  linenoise::SetMultiLine(true);
  std::string expr;
  for (;;) {
    auto quit = linenoise::Readline("> ", expr);
    if (quit) {
      break;
    }

    eval_and_print_expr(frame, expr, Verbosity::ShowEverything);
    linenoise::AddHistory(expr.c_str());
  }
}

void run_fuzzer(lldb::SBFrame& frame) {
  std::random_device rd;
  auto seed = rd();
  printf("==== Seed for this run is: %u ====\n", seed);

  auto rng = std::make_unique<fuzzer::DefaultGeneratorRng>(seed);
  auto cfg = fuzzer::GenConfig();
  // Disable shift and division for now
  cfg.bin_op_mask[fuzzer::BinOp::Shl] = false;
  cfg.bin_op_mask[fuzzer::BinOp::Shr] = false;
  cfg.bin_op_mask[fuzzer::BinOp::Div] = false;
  cfg.bin_op_mask[fuzzer::BinOp::Mod] = false;

  cfg.expr_kind_mask[fuzzer::ExprKind::DereferenceExpr] = false;
  cfg.expr_kind_mask[fuzzer::ExprKind::AddressOf] = false;
  cfg.expr_kind_mask[fuzzer::ExprKind::ArrayIndex] = false;
  cfg.expr_kind_mask[fuzzer::ExprKind::MemberOf] = false;
  cfg.expr_kind_mask[fuzzer::ExprKind::MemberOfPtr] = false;

  cfg.symbol_table.emplace(
      std::make_pair(fuzzer::Type(fuzzer::ScalarType::SignedInt),
                     std::vector<std::string>{"x", "int_min", "int_max"}));
  cfg.symbol_table.emplace(
      std::make_pair(fuzzer::Type(fuzzer::ScalarType::SignedLong),
                     std::vector<std::string>{"long_min", "long_max"}));
  cfg.symbol_table.emplace(std::make_pair(
      fuzzer::PointerType(fuzzer::QualifiedType(fuzzer::ScalarType::SignedInt)),
      std::vector<std::string>{"p"}));
  cfg.symbol_table.emplace(std::make_pair(
      fuzzer::PointerType(fuzzer::QualifiedType(fuzzer::ScalarType::Char)),
      std::vector<std::string>{"test_str"}));

  fuzzer::ExprGenerator gen(std::move(rng), std::move(cfg));
  std::vector<std::string> exprs;

  for (int i = 0; i < cfg.num_exprs_to_generate; i++) {
    auto maybe_gen_expr = gen.generate();
    if (!maybe_gen_expr.has_value()) {
      fprintf(stderr, "Warning: Could not generate expression #:%d\n", i);
      continue;
    }
    const auto& gen_expr = maybe_gen_expr.value();

    std::ostringstream os;
    os << gen_expr;
    auto str = os.str();

    exprs.emplace_back(std::move(str));
  }

  for (const auto& e : exprs) {
    eval_and_print_expr(frame, e, Verbosity::ShowMismatchesOrErrors);
  }
}

int main(int argc, char** argv) {
  std::string err;
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv[0], &err));
  if (runfiles == nullptr) {
    fprintf(stderr, "Could not launch the fuzzer: %s\n", err.c_str());
    return 1;
  }

  bool repl_mode = argc >= 2 && strcmp(argv[1], "--repl") == 0;

  lldb_eval::SetupLLDBServerEnv(*runfiles);

  auto source_path = runfiles->Rlocation(SOURCE_PATH_KEY);
  auto binary_path = runfiles->Rlocation(BINARY_PATH_KEY);

  lldb::SBDebugger::Initialize();
  {
    auto debugger = lldb::SBDebugger::Create();
    auto proc = lldb_eval::LaunchTestProgram(debugger, source_path, binary_path,
                                             "// BREAK HERE");
    auto thread = proc.GetSelectedThread();
    auto frame = thread.GetSelectedFrame();

    if (repl_mode) {
      run_repl(frame);
    } else {
      run_fuzzer(frame);
    }

    proc.Destroy();
  }
  lldb::SBDebugger::Terminate();

  return 0;
}
