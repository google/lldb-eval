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

  error.Clear();
  return ret.inner_value();
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpressionImpl(Context::Create(expression, frame), error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpressionImpl(Context::Create(expression, scope), error);
}

}  // namespace lldb_eval
