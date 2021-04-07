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
#include "lldb/API/SBExpressionOptions.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBValue.h"
#include "tools/cpp/runfiles/runfiles.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/expr_gen.h"
#include "tools/fuzzer/symbol_table.h"

using bazel::tools::cpp::runfiles::Runfiles;

static constexpr char SOURCE_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary.cc";
static constexpr char BINARY_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary";

enum Verbosity {
  ShowMismatchesOrErrors,
  ShowEverything,
};

// Compares LLDB types ignoring type qualifiers (const, volatile).
bool compare_types(lldb::SBType lhs, lldb::SBType rhs) {
  if (!lhs.IsValid() || !rhs.IsValid()) {
    return false;
  }

  lhs = lhs.GetCanonicalType().GetUnqualifiedType();
  rhs = rhs.GetCanonicalType().GetUnqualifiedType();

  return strcmp(lhs.GetName(), rhs.GetName()) == 0;
}

void eval_and_print_expr(lldb::SBFrame& frame, const std::string& expr,
                         Verbosity verbosity) {
  // Disable auto fix-its in LLDB evaluations.
  lldb::SBExpressionOptions options;
  options.SetAutoApplyFixIts(false);
  auto lldb_value = frame.EvaluateExpression(expr.c_str(), options);
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

  // Since we don't care about type qualifiers, ignore them when comparing
  // types. Otherwise there will be many noise caused by type mismatch, e.g.
  // `int` vs `const int`.
  bool type_mismatch =
      !compare_types(lldb_value.GetType(), lldb_eval_value.GetType());
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

fuzzer::SymbolTable gen_symtab(lldb::SBFrame& frame,
                               bool cv_qualifiers_enabled) {
  fuzzer::SymbolTable symtab = fuzzer::SymbolTable::create_from_lldb_context(
      frame, /*ignore_qualified_types=*/!cv_qualifiers_enabled);

  {
    fuzzer::EnumType type("CStyleEnum", /*scoped=*/false);
    symtab.add_enum_literal(type, "VALUE1");
    symtab.add_enum_literal(type, "VALUE2");
    symtab.add_enum_literal(type, "VALUE3");
    symtab.add_var(type, fuzzer::VariableExpr("c_enum"));
  }

  {
    fuzzer::EnumType type("ns::CStyleEnum", /*scoped=*/false);
    symtab.add_enum_literal(type, "V1");
    symtab.add_enum_literal(type, "V2");
    symtab.add_enum_literal(type, "V3");
    symtab.add_var(type, fuzzer::VariableExpr("ns_enum"));
  }

  {
    fuzzer::EnumType type("EnumClass", /*scoped=*/true);
    symtab.add_enum_literal(type, "ZERO");
    symtab.add_enum_literal(type, "ONE");
    symtab.add_enum_literal(type, "TWO");
    symtab.add_enum_literal(type, "THREE");
    symtab.add_var(type, fuzzer::VariableExpr("enum_class"));
  }

  {
    fuzzer::EnumType type("ns::EnumClass", /*scoped=*/true);
    symtab.add_enum_literal(type, "ZERO");
    symtab.add_enum_literal(type, "ONE");
    symtab.add_enum_literal(type, "TWO");
    symtab.add_enum_literal(type, "THREE");
    symtab.add_var(type, fuzzer::VariableExpr("ns_enum_class"));
  }

  {
    fuzzer::ArrayType array3(fuzzer::ScalarType::SignedInt, 3);
    fuzzer::ArrayType array33(array3, 3);
    fuzzer::ArrayType array23(array3, 2);
    fuzzer::ArrayType array32(
        fuzzer::ArrayType(fuzzer::ScalarType::SignedInt, 2), 3);
    fuzzer::PointerType ptr_to_arr{fuzzer::QualifiedType(array3)};
    fuzzer::ArrayType flt_array23(
        fuzzer::ArrayType(fuzzer::ScalarType::Float, 3), 2);
    fuzzer::ArrayType ts_array(fuzzer::TaggedType("TestStruct"), 2);

    symtab.add_var(array33, fuzzer::VariableExpr("array33"), 2);
    symtab.add_var(array23, fuzzer::VariableExpr("array23"), 2);
    symtab.add_var(array32, fuzzer::VariableExpr("array32"), 2);
    symtab.add_var(ptr_to_arr, fuzzer::VariableExpr("ptr_to_arr3"), 2);
    symtab.add_var(flt_array23, fuzzer::VariableExpr("flt_array23"), 2);
    symtab.add_var(ts_array, fuzzer::VariableExpr("ts_array"), 1);
  }

  // Add functions.
  symtab.add_function(fuzzer::ScalarType::UnsignedInt, "__log2",
                      {fuzzer::ScalarType::UnsignedInt});

  return symtab;
}

void run_fuzzer(lldb::SBFrame& frame, const unsigned* seed_ptr) {
  std::random_device rd;
  unsigned seed = seed_ptr ? *seed_ptr : rd();
  printf("==== Seed for this run is: %u ====\n", seed);

  auto rng = std::make_unique<fuzzer::DefaultGeneratorRng>(seed);
  // Fuzzer configuration. Refer to `tools/fuzzer/expr_gen.h` to see what
  // parameters are available.
  auto cfg = fuzzer::GenConfig();
  // Disable shift and division for now
  cfg.bin_op_mask[fuzzer::BinOp::Shl] = false;
  cfg.bin_op_mask[fuzzer::BinOp::Shr] = false;

  // Symbol table
  fuzzer::SymbolTable symtab = gen_symtab(frame, cfg.cv_qualifiers_enabled);

  fuzzer::ExprGenerator gen(std::move(rng), std::move(cfg), std::move(symtab));
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

  bool repl_mode = false;
  bool custom_seed = false;
  bool print_help = false;

  unsigned seed = 0;
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--repl") == 0) {
      repl_mode = true;
    }
    if (strcmp(argv[i], "--help") == 0) {
      print_help = true;
      break;
    }
    if (strcmp(argv[i], "--seed") == 0 && i < argc - 1) {
      custom_seed = true;

      i++;
      seed = std::stoul(argv[i]);
    }
  }
  if (print_help) {
    printf("Usage: %s [--repl] [--help] [--seed <rng_seed>]\n", argv[0]);
    printf("--help: Print this message\n");
    printf("--repl: REPL mode, evaluate expressions on lldb and lldb-eval\n");
    printf("--seed <rng_seed>: Specify the RNG seed to use\n");

    return 0;
  }

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
      const unsigned* seed_ptr = custom_seed ? &seed : nullptr;
      run_fuzzer(frame, seed_ptr);
    }

    proc.Destroy();
  }
  lldb::SBDebugger::Terminate();

  return 0;
}
