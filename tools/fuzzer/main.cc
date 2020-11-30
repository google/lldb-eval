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
#include "tools/fuzzer/symbol_table.h"

using bazel::tools::cpp::runfiles::Runfiles;

constexpr char SOURCE_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary.cc";
constexpr char BINARY_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary";

void run_repl(lldb::SBFrame& frame) {
  linenoise::SetMultiLine(true);
  std::string expr;
  for (;;) {
    auto quit = linenoise::Readline("> ", expr);
    if (quit) {
      break;
    }

    auto lldb_value = frame.EvaluateExpression(expr.c_str());
    auto lldb_err = lldb_value.GetError();
    printf("expr: `%s`\n", expr.c_str());
    printf("lldb:      `%s`\n", lldb_value.GetValue());

    lldb::SBError lldb_eval_err;
    auto lldb_eval_value =
        lldb_eval::EvaluateExpression(frame, expr.c_str(), lldb_eval_err);
    printf("lldb-eval: `%s`\n", lldb_eval_value.GetValue());
    printf("======\n");

    printf("lldb error:      `%s`\n", lldb_err.GetCString());
    printf("lldb-eval error: `%s`\n", lldb_eval_err.GetCString());

    printf("============================================================\n");

    linenoise::AddHistory(expr.c_str());
  }
}

fuzzer::SymbolTable gen_symtab() {
  fuzzer::SymbolTable symtab;

  {
    fuzzer::Type type(fuzzer::ScalarType::Char);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("char_min"),
                                           fuzzer::VariableExpr("char_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::UnsignedChar);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("uchar_min"),
                                           fuzzer::VariableExpr("uchar_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::SignedChar);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("schar_min"),
                                           fuzzer::VariableExpr("schar_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::UnsignedShort);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("ushort_min"),
                                           fuzzer::VariableExpr("ushort_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::SignedShort);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("short_min"),
                                           fuzzer::VariableExpr("short_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::UnsignedInt);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("uint_min"),
                                           fuzzer::VariableExpr("uint_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::SignedInt);
    std::vector<fuzzer::VariableExpr> vars{
        fuzzer::VariableExpr("int_min"), fuzzer::VariableExpr("int_max"),
        fuzzer::VariableExpr("x"), fuzzer::VariableExpr("ref")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::UnsignedLong);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("ulong_min"),
                                           fuzzer::VariableExpr("ulong_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::SignedLong);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("long_min"),
                                           fuzzer::VariableExpr("long_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::UnsignedLongLong);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("ullong_min"),
                                           fuzzer::VariableExpr("ullong_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::SignedLongLong);
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("llong_min"),
                                           fuzzer::VariableExpr("llong_max")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::Float);
    std::vector<fuzzer::VariableExpr> vars{
        fuzzer::VariableExpr("fnan"), fuzzer::VariableExpr("finf"),
        fuzzer::VariableExpr("fsnan"), fuzzer::VariableExpr("fmax"),
        fuzzer::VariableExpr("fdenorm")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::Double);
    std::vector<fuzzer::VariableExpr> vars{
        fuzzer::VariableExpr("dnan"), fuzzer::VariableExpr("dinf"),
        fuzzer::VariableExpr("dsnan"), fuzzer::VariableExpr("dmax"),
        fuzzer::VariableExpr("ddenorm")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::ScalarType::LongDouble);
    std::vector<fuzzer::VariableExpr> vars{
        fuzzer::VariableExpr("ldnan"), fuzzer::VariableExpr("ldinf"),
        fuzzer::VariableExpr("ldsnan"), fuzzer::VariableExpr("ldmax"),
        fuzzer::VariableExpr("lddenorm")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::Type type(fuzzer::TaggedType("TestStruct"));
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("ts")};

    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::PointerType type{
        fuzzer::QualifiedType(fuzzer::ScalarType::SignedInt)};
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("p")};
    symtab.add_vars(std::move(type), std::move(vars));
  }
  {
    fuzzer::PointerType type{fuzzer::QualifiedType(fuzzer::ScalarType::Char)};
    std::vector<fuzzer::VariableExpr> vars{fuzzer::VariableExpr("test_str")};
    symtab.add_vars(std::move(type), std::move(vars));
  }

  return symtab;
}

void run_fuzzer(lldb::SBFrame& frame, const unsigned* seed_ptr) {
  std::random_device rd;
  unsigned seed = seed_ptr ? *seed_ptr : rd();
  printf("Seed for this run is: %u\n", seed);

  auto rng = std::make_unique<fuzzer::DefaultGeneratorRng>(seed);
  auto cfg = fuzzer::GenConfig();
  // Disable shift and division for now
  cfg.bin_op_mask[fuzzer::BinOp::Shl] = false;
  cfg.bin_op_mask[fuzzer::BinOp::Shr] = false;

  cfg.expr_kind_mask[fuzzer::ExprKind::MemberOf] = false;
  cfg.expr_kind_mask[fuzzer::ExprKind::MemberOfPtr] = false;

  fuzzer::SymbolTable symtab = gen_symtab();

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
    auto lldb_value = frame.EvaluateExpression(e.c_str());
    auto lldb_err = lldb_value.GetError();
    printf("expr: `%s`\n", e.c_str());
    printf("lldb:      `%s`\n", lldb_value.GetValue());

    lldb::SBError lldb_eval_err;
    auto lldb_eval_value =
        lldb_eval::EvaluateExpression(frame, e.c_str(), lldb_eval_err);
    printf("lldb-eval: `%s`\n", lldb_eval_value.GetValue());
    printf("======\n");

    printf("lldb error:      `%s`\n", lldb_err.GetCString());
    printf("lldb-eval error: `%s`\n", lldb_eval_err.GetCString());

    printf("============================================================\n");
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
  unsigned seed = 0;
  for (int i = 1; i < argc; i++) {
    if (strcmp(argv[i], "--repl") == 0) {
      repl_mode = true;
    }
    if (strcmp(argv[i], "--seed") == 0 && i < argc - 1) {
      custom_seed = true;

      i++;
      seed = std::stoul(argv[i]);
    }
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
