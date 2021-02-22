#include <iostream>
#include <ostream>
#include <sstream>
#include <optional>


#include <unistd.h>

#include "experimental/protobuf/expression.pb.h"
#include "experimental/protobuf/proto_to_str.h"
#include "libprotobuf_mutator/src/libfuzzer/libfuzzer_macro.h"
#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"

#include <fuzzer/FuzzedDataProvider.h>

using bazel::tools::cpp::runfiles::Runfiles;

static Runfiles* g_runfiles;
static lldb::SBDebugger g_debugger;
static lldb::SBProcess g_process;
static lldb::SBFrame g_frame;

static lldb::SBFrame& GetLldbFrame() {
  static bool initialized = false;
  if (!initialized) {
    g_runfiles = Runfiles::CreateForTest();
    lldb_eval::SetupLLDBServerEnv(*g_runfiles);
    lldb::SBDebugger::Initialize();

    auto binary_path =
        g_runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary");
    auto source_path =
        g_runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary.cc");
    g_debugger = lldb::SBDebugger::Create(false);
    g_process = lldb_eval::LaunchTestProgram(g_debugger, source_path,
                                             binary_path, "// BREAK HERE");
    g_frame = g_process.GetSelectedThread().GetSelectedFrame();

    initialized = true;
  }

  return g_frame;
}

std::optional<lldb::SBValue> Eval(const std::string& expr) {
  lldb::SBError error;
  auto value =
      lldb_eval::EvaluateExpression(GetLldbFrame(), expr.c_str(), error);
  // note: possible bug
  if (error.Fail()) {
    return {};
  }
  return value;
}

DEFINE_BINARY_PROTO_FUZZER(const Expr& expr) {
  std::cout << expr << std::endl;
  usleep(50000);

  auto value = Eval(DumpExpression(expr));
  if (value.has_value()) {
    assert(value.value().IsValid());
  }
}
