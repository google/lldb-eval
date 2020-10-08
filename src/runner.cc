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

// Running a process can be slow when built with sanitizers.
const uint32_t kWaitForEventTimeout = 5;

void SetupLLDBServerEnv(const Runfiles& runfiles) {
#ifndef _WIN32
  std::string lldb_server = runfiles.Rlocation("llvm_project/bin/lldb-server");
  setenv("LLDB_DEBUGSERVER_PATH", lldb_server.c_str(), 0);
#else
  (void)runfiles;
#endif  // !_WIN32
}

int FindBreakpointLine(const Runfiles& runfiles,
                       const std::string& break_line) {
  // Read the source file to find the breakpoint location.
  std::ifstream infile(runfiles.Rlocation("lldb_eval/testdata/test_binary.cc"));
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

lldb::SBProcess LaunchTestProgram(const Runfiles& runfiles,
                                  lldb::SBDebugger debugger,
                                  const std::string& break_line) {
  std::string binary = runfiles.Rlocation("lldb_eval/testdata/test_binary");
  lldb::SBTarget target = debugger.CreateTarget(binary.c_str());

  lldb::SBBreakpoint bp = target.BreakpointCreateByLocation(
      "test_binary.cc", FindBreakpointLine(runfiles, break_line));
  lldb::SBProcess process = target.LaunchSimple(nullptr, nullptr, ".");

  bool running = true;
  lldb::SBEvent event;
  lldb::SBListener listener = debugger.GetListener();

  while (running) {
    if (listener.WaitForEvent(kWaitForEventTimeout, event)) {
      if (!lldb::SBProcess::EventIsProcessEvent(event)) {
        std::cerr << "Got some random event: "
                  << lldb::SBEvent::GetCStringFromEvent(event) << std::endl;
        continue;
      }

      lldb::StateType state = lldb::SBProcess::GetStateFromEvent(event);
      if (state == lldb::eStateInvalid) {
        std::cerr << "process event: "
                  << lldb::SBEvent::GetCStringFromEvent(event) << std::endl;
        continue;
      }

      switch (state) {
        case lldb::eStateStopped: {
          auto thread = process.GetSelectedThread();
          auto stopReason = thread.GetStopReason();
          if (stopReason == lldb::eStopReasonBreakpoint) {
            lldb::break_id_t bpId = static_cast<lldb::break_id_t>(
                thread.GetStopReasonDataAtIndex(0));
            if (bpId == bp.GetID()) {
              running = false;
            } else {
              std::cerr << "Stopped at unknown breakpoint: " << bpId
                        << std::endl;
            }
          }
          break;
        }
        default:
          break;
      }

    } else {
      std::cerr
          << "Timeout while waiting for the event, kill the process and exit."
          << std::endl;
      process.Destroy();
      exit(1);
    }
  }

  return process;
}

}  // namespace lldb_eval
