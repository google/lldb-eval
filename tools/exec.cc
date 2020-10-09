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

int main(int argc, char** argv) {
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv[0]));

  std::string break_line;
  std::string expr;

  if (argc == 2) {
    break_line = "// break here";
    expr = argv[1];
  } else {
    break_line = "// BREAK(" + std::string(argv[1]) + ")";
    expr = argv[2];
  }

  lldb_eval::SetupLLDBServerEnv(*runfiles);
  lldb::SBDebugger::Initialize();
  lldb::SBDebugger debugger = lldb::SBDebugger::Create(false);
  lldb::SBProcess process =
      lldb_eval::LaunchTestProgram(*runfiles, debugger, break_line);

  lldb::SBFrame frame = process.GetSelectedThread().GetSelectedFrame();
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
    auto sb_value =
        result.AsSbValue(expr_ctx.GetExecutionContext().GetTarget());

    if (sb_value.IsValid()) {
      std::cerr << "expr result = " << sb_value.GetValue() << std::endl;
      std::cerr << "expr type = " << sb_value.GetTypeName() << std::endl;
    } else {
      std::cerr << "expr error: Unknown, result is invalid." << std::endl;
    }
  }

  auto total = std::chrono::duration_cast<std::chrono::microseconds>(
      time_eval - time_start);
  auto elapsed_parse = std::chrono::duration_cast<std::chrono::microseconds>(
      time_parse - time_start);
  auto elapsed_eval = std::chrono::duration_cast<std::chrono::microseconds>(
      time_eval - time_parse);

  std::cerr << "----------" << std::endl
            << "total = " << total.count()
            << "us (parse = " << elapsed_parse.count()
            << "us, eval = " << elapsed_eval.count() << "us)" << std::endl;

  process.Destroy();
  lldb::SBDebugger::Terminate();

  return 0;
}
