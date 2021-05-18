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

#ifdef _WIN32
#include <filesystem>
#else
#include <errno.h>  // for `program_invocation_name`
#endif

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

class BM : public benchmark::Fixture {
 public:
  void SetUp(::benchmark::State&) override {
#ifdef _WIN32
    auto cwd = std::filesystem::current_path();
    std::string argv0 = cwd.parent_path().append("eval_benchmark.exe").string();
#else
    std::string argv0 = program_invocation_name;
#endif

    runfiles.reset(Runfiles::Create(argv0));

    lldb_eval::SetupLLDBServerEnv(*runfiles);

    auto binary_path =
        runfiles->Rlocation("lldb_eval/testdata/benchmark_binary");
    auto source_path =
        runfiles->Rlocation("lldb_eval/testdata/benchmark_binary.cc");

    debugger = lldb::SBDebugger::Create(false);
    process = lldb_eval::LaunchTestProgram(debugger, source_path, binary_path,
                                           "// BREAK HERE");
    frame = process.GetSelectedThread().GetSelectedFrame();
  }

  void TearDown(::benchmark::State&) override {
    process.Destroy();
    lldb::SBDebugger::Destroy(debugger);
  }

  lldb::SBDebugger debugger;
  lldb::SBProcess process;
  lldb::SBFrame frame;

  std::unique_ptr<Runfiles> runfiles;
};

BENCHMARK_F(BM, AddTwoNumbers)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(frame, "1 + 1", error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

BENCHMARK_F(BM, ArrayDereference)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(frame, "*arr", error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

BENCHMARK_F(BM, ArraySubscript)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(frame, "arr[0]", error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

BENCHMARK_F(BM, TypeCasting)(benchmark::State& state) {
  for (auto _ : state) {
    lldb::SBError error;
    lldb_eval::EvaluateExpression(
        frame, "(char)1 + (short)1.1f + (long long)1.1 + (double)(char)2",
        error);

    if (error.Fail()) {
      state.SkipWithError("Failed to evaluate the expression!");
    }
  }
}

int main(int argc, char** argv) {
  lldb::SBDebugger::Initialize();

  // Same as BENCHMARK_MAIN()
  // clang-format off
  ::benchmark::Initialize(&argc, argv);
  if (::benchmark::ReportUnrecognizedArguments(argc, argv)) return 1;
  ::benchmark::RunSpecifiedBenchmarks();
  // clang-format on

  lldb::SBDebugger::Terminate();
}
