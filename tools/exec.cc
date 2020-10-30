// Copyright 2020 Google LLC
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

#include <chrono>
#include <iostream>
#include <memory>
#include <string>

#include "cpp-linenoise/linenoise.hpp"
#include "lldb-eval/eval.h"
#include "lldb-eval/parser.h"
#include "lldb-eval/runner.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

void EvalExpr(lldb::SBFrame frame, const std::string& expr) {
  lldb_eval::ExpressionContext expr_ctx(expr, lldb::SBExecutionContext(frame));

  auto time_start = std::chrono::high_resolution_clock::now();

  lldb_eval::Parser p(expr_ctx);
  auto expr_result = p.Run();

  auto time_parse = std::chrono::high_resolution_clock::now();

  if (p.HasError()) {
    std::cerr << p.GetError() << std::endl;
  }

  lldb_eval::Interpreter eval(expr_ctx);

  lldb_eval::EvalError error;
  lldb_eval::Value result = eval.Eval(expr_result.get(), error);

  auto time_eval = std::chrono::high_resolution_clock::now();

  if (error) {
    std::cerr << error.message() << std::endl;
  } else {
    // Due to various bugs result can still be NULL even though there was no
    // error reported. Printing NULL leads to segfault, so check and replace it.
    auto sb_value = result.inner_value();

    if (sb_value.IsValid()) {
      std::cerr << "value = " << sb_value.GetValue() << std::endl;
      std::cerr << "type  = " << sb_value.GetTypeName() << std::endl;
    } else {
      std::cerr << "Unknown error, result is invalid." << std::endl;
    }
  }

  auto total = std::chrono::duration_cast<std::chrono::microseconds>(
      time_eval - time_start);
  auto elapsed_parse = std::chrono::duration_cast<std::chrono::microseconds>(
      time_parse - time_start);
  auto elapsed_eval = std::chrono::duration_cast<std::chrono::microseconds>(
      time_eval - time_parse);

  std::cerr << "----------" << std::endl
            << "elapsed = " << total.count()
            << "us (parse = " << elapsed_parse.count()
            << "us, eval = " << elapsed_eval.count() << "us)" << std::endl;
}

void RunRepl(lldb::SBFrame frame) {
  linenoise::SetMultiLine(true);
  std::string expr;

  std::cerr << "Stopped at:" << std::endl;
  std::cerr << "\t" << frame.GetFunctionName() << ":"
            << frame.GetLineEntry().GetLine() << ":"
            << frame.GetLineEntry().GetColumn() << std::endl;

  while (true) {
    bool quit = linenoise::Readline("> ", expr);
    if (quit) {
      break;
    }

    EvalExpr(frame, expr);

    linenoise::AddHistory(expr.c_str());
  }
}

int main(int argc, char** argv) {
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv[0]));

  bool repl_mode = false;
  std::string break_line = "// break here";
  std::string expr;

  if (argc == 1) {
    repl_mode = true;
  } else if (argc == 2) {
    expr = argv[1];
  } else {
    break_line = "// BREAK(" + std::string(argv[1]) + ")";
    expr = argv[2];
  }

  lldb_eval::SetupLLDBServerEnv(*runfiles);
  lldb::SBDebugger::Initialize();
  lldb::SBDebugger debugger = lldb::SBDebugger::Create(false);

  auto binary_path = runfiles->Rlocation("lldb_eval/testdata/test_binary");
  auto source_path = runfiles->Rlocation("lldb_eval/testdata/test_binary.cc");

  lldb::SBProcess process = lldb_eval::LaunchTestProgram(
      debugger, source_path, binary_path, break_line);

  lldb::SBFrame frame = process.GetSelectedThread().GetSelectedFrame();

  if (repl_mode) {
    RunRepl(frame);
  } else {
    EvalExpr(frame, expr);
  }

  process.Destroy();
  lldb::SBDebugger::Terminate();

  return 0;
}
