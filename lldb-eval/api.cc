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

static lldb::SBError ConvertError(const Error& error) {
  lldb::SBError ret;
  ret.SetError(static_cast<uint32_t>(error.code()), lldb::eErrorTypeGeneric);
  ret.SetErrorString(error.message().c_str());
  return ret;
}

static lldb::SBValue EvaluateExpressionImpl(std::shared_ptr<Context> ctx,
                                            lldb::SBError& error) {
  error.Clear();

  Error err;
  Parser p(ctx);
  ExprResult tree = p.Run(err);
  if (err) {
    error = ConvertError(err);
    return lldb::SBValue();
  }

  Interpreter eval(ctx);
  Value ret = eval.Eval(tree.get(), err);
  if (err) {
    error = ConvertError(err);
    return lldb::SBValue();
  }

  // Check if the inner value holds an error (this could be a runtime evaluation
  // failure, e.g. dereferencing a null pointer).
  lldb::SBValue value = ret.inner_value();

  if (value.GetError().GetError()) {
    // This is not an "error" per se, because the expression was valid and the
    // result is what it should be. Runtime error indicates operations on the
    // invalid data (e.g. null defererence).
    error = value.GetError();
    return lldb::SBValue();
  }

  return value;
}

static std::unordered_map<std::string, lldb::SBValue> ConvertToMap(
    ContextVariableList context_vars) {
  std::unordered_map<std::string, lldb::SBValue> ret;
  for (size_t i = 0; i < context_vars.size; ++i) {
    ret.emplace(context_vars.data[i].name, context_vars.data[i].value);
  }
  return ret;
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpressionImpl(Context::Create(expression, frame), error);
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 ContextVariableList context_vars,
                                 lldb::SBError& error) {
  return EvaluateExpressionImpl(
      Context::Create(expression, frame, ConvertToMap(context_vars)), error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpressionImpl(Context::Create(expression, scope), error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 ContextVariableList context_vars,
                                 lldb::SBError& error) {
  return EvaluateExpressionImpl(
      Context::Create(expression, scope, ConvertToMap(context_vars)), error);
}

}  // namespace lldb_eval
