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

constexpr char VAR[] = "x";

constexpr char LLDB_SERVER_KEY[] = "llvm_project/bin/lldb-server";
constexpr char EXECUTABLE_PATH_KEY[] = "lldb_eval/testdata/fuzzer_binary";

void run_repl(lldb::SBFrame& frame) {
  linenoise::SetMultiLine(true);
  std::string expr;
  for (;;) {
    auto quit = linenoise::Readline("> ", expr);
    if (quit) {
      break;
    }

    auto lldb_value = frame.EvaluateExpression(expr.c_str());
    printf("lldb yields: `%s`\n", lldb_value.GetValue());

    lldb::SBError err;
    auto lldb_eval_value =
        lldb_eval::EvaluateExpression(frame, expr.c_str(), err);
    printf("lldb-eval yields: `%s`\n", lldb_eval_value.GetValue());
    printf("------------------------------------------------------------\n");

    linenoise::AddHistory(expr.c_str());
  }
}

void run_fuzzer(lldb::SBFrame& frame) {
  std::random_device rd;
  auto seed = rd();
  printf("Seed for this run is: %u\n", seed);

  auto var_value = frame.GetValueForVariablePath(VAR);
  printf("Value of variable `%s` is: %s\n", VAR, var_value.GetValue());

  auto rng = std::make_unique<fuzzer::DefaultGeneratorRng>(seed);
  fuzzer::ExprGenerator gen(std::move(rng));
  std::vector<std::string> exprs;

  size_t padding = 0;
  for (int i = 0; i < 20; i++) {
    auto gen_expr = gen.generate();
    std::ostringstream os;
    os << gen_expr;
    auto str = os.str();

    padding = std::max(padding, str.size());
    exprs.emplace_back(std::move(str));
  }

  for (const auto& e : exprs) {
    auto lldb_value = frame.EvaluateExpression(e.c_str());
    printf("lldb:      `%-*s` yields: `%s`\n", (int)padding, e.c_str(),
           lldb_value.GetValue());

    lldb::SBError err;
    auto lldb_eval_value = lldb_eval::EvaluateExpression(frame, e.c_str(), err);
    printf("lldb-eval: `%-*s` yields: `%s`\n", (int)padding, e.c_str(),
           lldb_eval_value.GetValue());
    printf("------------------------------------------------------------\n");
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

#ifndef _WIN32
  std::string lldb_server = runfiles->Rlocation(LLDB_SERVER_KEY);
  setenv("LLDB_DEBUGSERVER_PATH", lldb_server.c_str(), 0);
#endif  // !_WIN32

  std::string exe_path = runfiles->Rlocation(EXECUTABLE_PATH_KEY);
  std::string dirname_buf = exe_path;

  const char* ARGV[] = {exe_path.c_str(), nullptr};

  lldb::SBDebugger::Initialize();
  {
    auto debugger = lldb::SBDebugger::Create();
    debugger.SetAsync(false);

    auto target = debugger.CreateTarget(exe_path.c_str());
    auto bp = target.BreakpointCreateByName("break_here", exe_path.c_str());
    // Test program does not perform any I/O, so current directory doesn't
    // matter.
    auto proc = target.LaunchSimple(ARGV, nullptr, ".");
    auto thread = proc.GetSelectedThread();

    auto frame = thread.SetSelectedFrame(1);

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
