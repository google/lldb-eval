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

#include "lldb-eval/api.h"

#include <memory>
#include <string>
#include <unordered_map>

#include "lldb-eval/context.h"
#include "lldb-eval/eval.h"
#include "lldb-eval/parser.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBValue.h"

namespace lldb_eval {

static std::unordered_map<std::string, TypeSP> ConvertToTypeMap(
    ContextVariableList context_vars) {
  std::unordered_map<std::string, TypeSP> ret;
  for (size_t i = 0; i < context_vars.size; ++i) {
    lldb::SBValue value = context_vars.data[i].value;
    ret.emplace(context_vars.data[i].name, LLDBType::CreateSP(value.GetType()));
  }
  return ret;
}

static std::unordered_map<std::string, Value> ConvertToValueMap(
    ContextVariableList context_vars) {
  std::unordered_map<std::string, Value> ret;
  for (size_t i = 0; i < context_vars.size; ++i) {
    ret.emplace(context_vars.data[i].name, Value(context_vars.data[i].value));
  }
  return ret;
}

static lldb::SBValue EvaluateExpressionImpl(std::shared_ptr<Context> ctx,
                                            std::shared_ptr<SourceManager> sm,
                                            Options opts, lldb::SBTarget target,
                                            Value scope, lldb::SBError& error) {
  error.Clear();

  // Handle parsing options.
  if (opts.context_vars.size > 0) {
    ctx->SetContextVars(ConvertToTypeMap(opts.context_vars));
  }
  ctx->SetAllowSideEffects(opts.allow_side_effects);

  Error err;
  Parser p(ctx);
  ExprResult tree = p.Run(err);
  if (err) {
    error.SetError(static_cast<uint32_t>(err.code()), lldb::eErrorTypeGeneric);
    error.SetErrorString(err.message().c_str());
    return lldb::SBValue();
  }

  // Note that `scope` may be invalid in the case of frame context evaluation.
  Interpreter eval(target, sm, scope);
  if (opts.context_vars.size > 0) {
    eval.SetContextVars(ConvertToValueMap(opts.context_vars));
  }
  Value ret = eval.Eval(tree.get(), err);
  if (err) {
    error.SetError(static_cast<uint32_t>(err.code()), lldb::eErrorTypeGeneric);
    error.SetErrorString(err.message().c_str());
    return ret.inner_value();
  }

  // Check if the inner value holds an error (this could be a runtime evaluation
  // failure, e.g. dereferencing a null pointer).
  lldb::SBValue value = ret.inner_value();

  if (value.GetError().GetError()) {
    // This is not an "error" per se, because the expression was valid and the
    // result is what it should be. Runtime error indicates operations on the
    // invalid data (e.g. null defererence).
    error = value.GetError();
  }

  return value;
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpression(frame, expression, Options{}, error);
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 Options opts, lldb::SBError& error) {
  auto sm = SourceManager::Create(expression);
  auto context = Context::Create(sm, frame);
  auto target = frame.GetThread().GetProcess().GetTarget();
  return EvaluateExpressionImpl(context, sm, opts, target, Value(), error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpression(scope, expression, Options{}, error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 Options opts, lldb::SBError& error) {
  auto target = scope.GetTarget();
  auto scope_value = Value(scope);
  auto sm = SourceManager::Create(expression);
  auto context = Context::Create(sm, target, scope_value.type());
  return EvaluateExpressionImpl(context, sm, opts, target, scope_value, error);
}

}  // namespace lldb_eval
