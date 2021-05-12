// Copyright 2021 Google LLC
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

#include <filesystem>
#include <memory>

#include "benchmark/benchmark.h"
#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "llvm/Support/FormatVariadic.h"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

class DebuggerFixture : public benchmark::Fixture {
 public:
  void SetUp(const ::benchmark::State&) {
    auto cwd = std::filesystem::current_path();

#ifdef _WIN32
    auto argv0 = cwd.parent_path().append("eval_benchmark.exe");
#else
    auto argv0 = cwd.parent_path().append("eval_benchmark");
#endif

    runfiles.reset(Runfiles::Create(argv0.string()));

    lldb_eval::SetupLLDBServerEnv(*runfiles);
    lldb::SBDebugger::Initialize();

    auto binary_path =
        runfiles->Rlocation("lldb_eval/testdata/benchmark_binary");
    auto source_path =
        runfiles->Rlocation("lldb_eval/testdata/benchmark_binary.cc");

    debugger = lldb::SBDebugger::Create(false);
    process = lldb_eval::LaunchTestProgram(debugger, source_path, binary_path,
                                           "// BREAK HERE");
    frame = process.GetSelectedThread().GetSelectedFrame();
  }

  void TearDown(::benchmark::State&) {
    process.Destroy();
    lldb::SBDebugger::Destroy(debugger);
    lldb::SBDebugger::Terminate();
  }

  lldb::SBDebugger debugger;
  lldb::SBProcess process;
  lldb::SBFrame frame;

  std::unique_ptr<Runfiles> runfiles;
};

BENCHMARK_F(DebuggerFixture, AddTwoNumbers)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(frame, "1 + 1", error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

BENCHMARK_F(DebuggerFixture, ArrayDereference)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(frame, "*arr", error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

BENCHMARK_F(DebuggerFixture, ArraySubscript)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(frame, "arr[0]", error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

#undef SKIP_IF_ERROR

BENCHMARK_MAIN();
