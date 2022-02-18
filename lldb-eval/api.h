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

#ifndef LLDB_EVAL_API_H_
#define LLDB_EVAL_API_H_

#include <memory>

#include "lldb-eval/ast.h"
#include "lldb-eval/context.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBValue.h"

#ifdef _MSC_VER
#if LLDB_EVAL_LINKED_AS_SHARED_LIBRARY
#define LLDB_EVAL_API __declspec(dllimport)
#elif LLDB_EVAL_CREATE_SHARED_LIBRARY
#define LLDB_EVAL_API __declspec(dllexport)
#endif
#elif __GNUC__ >= 4 || defined(__clang__)
#define LLDB_EVAL_API __attribute__((visibility("default")))
#endif

#ifndef LLDB_EVAL_API
#define LLDB_EVAL_API
#endif

namespace lldb_eval {

// Context variables (aka. convenience variables) are variables living entirely
// within LLDB. They are prefixed with '$' and created via expression evaluation
// interface (e.g. 'expr int $foo = 13' in the LLDB's interactive console).
// Since lldb-eval can't access context variables out of the box, they have to
// be explicitly passed as arguments in lldb-eval API methods.
struct ContextVariable {
  const char* name;
  lldb::SBValue value;
};

struct ContextVariableList {
  const ContextVariable* data;
  size_t size;
};

// Context arguments are similar to context variables. The difference is that
// context arguments maps variable names to types. They are intended to be used
// for parsing and allow re-using parsed expressions with different values of
// context variables.
struct ContextArgument {
  const char* name;
  lldb::SBType type;
};

struct ContextArgumentList {
  const ContextArgument* data;
  size_t size;
};

struct Options {
  bool allow_side_effects = false;
  ContextArgumentList context_args = {};
  ContextVariableList context_vars = {};
};

struct CompiledExpr {
  std::shared_ptr<SourceManager> source;
  std::unique_ptr<AstNode> tree;
  lldb::SBType scope;

  CompiledExpr(std::shared_ptr<SourceManager> source,
               std::unique_ptr<AstNode> tree, lldb::SBType scope)
      : source(std::move(source)),
        tree(std::move(tree)),
        scope(std::move(scope)) {}
};

LLDB_EVAL_API
lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error);

LLDB_EVAL_API
lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 Options opts, lldb::SBError& error);

LLDB_EVAL_API
lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 lldb::SBError& error);

LLDB_EVAL_API
lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 Options opts, lldb::SBError& error);

LLDB_EVAL_API
std::shared_ptr<CompiledExpr> CompileExpression(lldb::SBTarget target,
                                                lldb::SBType scope,
                                                const char* expression,
                                                lldb::SBError& error);

LLDB_EVAL_API
std::shared_ptr<CompiledExpr> CompileExpression(lldb::SBTarget target,
                                                lldb::SBType scope,
                                                const char* expression,
                                                Options opts,
                                                lldb::SBError& error);

LLDB_EVAL_API
lldb::SBValue EvaluateExpression(lldb::SBValue scope,
                                 std::shared_ptr<CompiledExpr> expression,
                                 lldb::SBError& error);

LLDB_EVAL_API
lldb::SBValue EvaluateExpression(lldb::SBValue scope,
                                 std::shared_ptr<CompiledExpr> expression,
                                 ContextVariableList context_vars,
                                 lldb::SBError& error);

}  // namespace lldb_eval

#endif  // LLDB_EVAL_API_H_
