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

#include <string>

#include "lldb-eval/eval.h"
#include "lldb-eval/expression_context.h"
#include "lldb-eval/parser.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBValue.h"

namespace lldb_eval {

namespace {

Value EvaluateExpression(lldb::SBFrame frame, const char* expr, Error& error) {
  ExpressionContext ctx(expr, lldb::SBExecutionContext(frame));

  Parser p(ctx);
  ExprResult tree = p.Run(error);
  if (error) {
    return Value();
  }

  Interpreter eval(ctx);
  Value ret = eval.Eval(tree.get(), error);
  if (error) {
    return Value();
  }

  return ret;
}

}  // namespace

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error) {
  error.Clear();

  Error err;
  Value result = EvaluateExpression(frame, expression, err);

  if (err) {
    error.SetError(static_cast<uint32_t>(err.code()), lldb::eErrorTypeGeneric);
    error.SetErrorString(err.message().c_str());
    return lldb::SBValue();
  }

  return result.inner_value();
}

}  // namespace lldb_eval
