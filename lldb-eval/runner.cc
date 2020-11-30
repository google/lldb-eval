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

#include "lldb-eval/runner.h"

#include <fstream>
#include <iostream>
#include <string>

#include "lldb/API/SBBreakpoint.h"
#include "lldb/API/SBBreakpointLocation.h"
#include "lldb/API/SBCommandInterpreter.h"
#include "lldb/API/SBCommandReturnObject.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBDefines.h"
#include "lldb/API/SBEvent.h"
#include "lldb/API/SBFileSpec.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBListener.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"
#include "lldb/lldb-types.h"
#include "tools/cpp/runfiles/runfiles.h"

namespace lldb_eval {

using bazel::tools::cpp::runfiles::Runfiles;

#ifdef CONFIG_VALGRIND
// Running a process under Valgrind can be extremely slow.
const uint32_t kWaitForEventTimeout = 30;
#else
// Running a process can be slow when built with sanitizers.
const uint32_t kWaitForEventTimeout = 5;
#endif

void SetupLLDBServerEnv(const Runfiles& runfiles) {
#ifndef _WIN32
  std::string lldb_server = runfiles.Rlocation("llvm_project/bin/lldb-server");
  setenv("LLDB_DEBUGSERVER_PATH", lldb_server.c_str(), 0);
#else
  (void)runfiles;
#endif  // !_WIN32
}

int FindBreakpointLine(const std::string& file_path,
                       const std::string& break_line) {
  // Read the source file to find the breakpoint location.
  std::ifstream infile(file_path);
  std::string line;
  int line_num = 1;
  while (std::getline(infile, line)) {
    if (line.find(break_line) != std::string::npos) {
      return line_num;
    }
    ++line_num;
  }

  std::cerr << "Can't find the breakpoint location." << std::endl;
  exit(1);
}

std::string filename_of_source_path(const std::string& source_path) {
  auto idx = source_path.find_last_of("/\\");
  if (idx == std::string::npos) {
    idx = 0;
  } else {
    idx++;
  }

  return source_path.substr(idx);
}

lldb::SBProcess LaunchTestProgram(lldb::SBDebugger debugger,
                                  const std::string& source_path,
                                  const std::string& binary_path,
                                  const std::string& break_line) {
  auto target = debugger.CreateTarget(binary_path.c_str());

  auto source_file = filename_of_source_path(source_path);

  const char* argv[] = {binary_path.c_str(), nullptr};

  auto bp = target.BreakpointCreateByLocation(
      source_file.c_str(), FindBreakpointLine(source_path.c_str(), break_line));
  // Test programs don't perform any I/O, so current directory doesn't
  // matter.
  auto process = target.LaunchSimple(argv, nullptr, ".");

  lldb::SBEvent event;
  auto listener = debugger.GetListener();

  while (true) {
    if (!listener.WaitForEvent(kWaitForEventTimeout, event)) {
      std::cerr
          << "Timeout while waiting for the event, kill the process and exit."
          << std::endl;
      process.Destroy();
      exit(1);
    }

    if (!lldb::SBProcess::EventIsProcessEvent(event)) {
      std::cerr << "Got some random event: "
                << lldb::SBEvent::GetCStringFromEvent(event) << std::endl;
      continue;
    }

    auto state = lldb::SBProcess::GetStateFromEvent(event);
    if (state == lldb::eStateInvalid) {
      std::cerr << "process event: "
                << lldb::SBEvent::GetCStringFromEvent(event) << std::endl;
      continue;
    }

    if (state == lldb::eStateExited) {
      std::cerr << "Process exited: " << process.GetExitStatus() << std::endl;
      process.Destroy();
      exit(1);
    }

    if (state != lldb::eStateStopped) {
      continue;
    }

    auto thread = process.GetSelectedThread();
    auto stopReason = thread.GetStopReason();

    if (stopReason != lldb::eStopReasonBreakpoint) {
      continue;
    }

    auto bpId =
        static_cast<lldb::break_id_t>(thread.GetStopReasonDataAtIndex(0));
    if (bpId != bp.GetID()) {
      std::cerr << "Stopped at unknown breakpoint: " << bpId << std::endl
                << "Now killing process and exiting" << std::endl;
      process.Destroy();
      exit(1);
    }

    return process;
  }
}

}  // namespace lldb_eval
