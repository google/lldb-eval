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

#ifndef LLDB_EVAL_DEFINES_H_
#define LLDB_EVAL_DEFINES_H_

#include <cstdio>
#include <cstdlib>

#define lldb_eval_unreachable(msg) \
  lldb_eval::unreachable(msg, __FILE__, __LINE__)

namespace lldb_eval {

[[noreturn]] inline void unreachable(const char* msg, const char* file,
                                     int line) {
  fprintf(stderr, "Unreachable statement at %s:%d (%s)\n", file, line, msg);
  abort();
}

}  // namespace lldb_eval

#endif  // LLDB_EVAL_DEFINES_H_
