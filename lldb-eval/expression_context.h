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

#ifndef LLDB_EVAL_EXPRESSION_CONTEXT_H_
#define LLDB_EVAL_EXPRESSION_CONTEXT_H_

#include <string>

#include "clang/Basic/SourceManager.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBType.h"

namespace lldb_eval {

class ExpressionContext {
 public:
  ExpressionContext(std::string expr, lldb::SBExecutionContext exec_ctx);

  clang::SourceManager& GetSourceManager() const { return smff_->get(); }
  lldb::SBExecutionContext GetExecutionContext() const { return exec_ctx_; }

 public:
  lldb::SBType ResolveTypeByName(const char* name);

 private:
  // Store the expression, since SourceManager doesn't take the ownership.
  std::string expr_;
  std::unique_ptr<clang::SourceManagerForFile> smff_;

  // The expression exists in the context of an LLDB target. Execution context
  // provides information for semantic analysis (e.g. resolving types, looking
  // up variables, etc).
  lldb::SBExecutionContext exec_ctx_;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_EXPRESSION_CONTEXT_H_
