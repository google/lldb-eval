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
#include "lldb-eval/parser_context.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBError.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"

namespace lldb_eval {

static std::unordered_map<std::string, TypeSP> ConvertToTypeMap(
    ContextArgumentList context_args) {
  std::unordered_map<std::string, TypeSP> ret;
  for (size_t i = 0; i < context_args.size; ++i) {
    lldb::SBType type = context_args.data[i].type;
    ret.emplace(context_args.data[i].name, LLDBType::CreateSP(type));
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

static lldb::SBError CreateError(ErrorCode code, const char* message) {
  lldb::SBError error;
  error.SetError(static_cast<uint32_t>(code), lldb::eErrorTypeGeneric);
  error.SetErrorString(message);
  return error;
}

static ParsedExpression ParseExpressionImpl(
    std::shared_ptr<SourceManager> source, std::shared_ptr<Context> ctx,
    Options opts, lldb::SBTarget target, lldb::SBType scope,
    lldb::SBError& error) {
  error.Clear();

  // Handle parsing options.
  if (opts.context_vars.size > 0) {
    ctx->SetContextVars(ConvertToValueMap(opts.context_vars));
  }
  if (opts.context_args.size > 0) {
    ctx->SetContextArgs(ConvertToTypeMap(opts.context_args));
  }
  ctx->SetAllowSideEffects(opts.allow_side_effects);

  Error err;
  Parser p(ctx);
  ExprResult tree = p.Run(err);
  if (err) {
    error = CreateError(err.code(), err.message().c_str());
    return {};
  }

  return {source, std::move(tree), target, scope};
}

static lldb::SBValue EvaluateExpressionImpl(ParsedExpression& parsed_expr,
                                            ContextVariableList context_vars,
                                            Value scope, lldb::SBError& error) {
  Interpreter eval(parsed_expr.target, parsed_expr.source, scope);
  if (context_vars.size > 0) {
    eval.SetContextVars(ConvertToValueMap(context_vars));
  }
  Error err;
  Value ret = eval.Eval(parsed_expr.tree.get(), err);
  if (err) {
    error = CreateError(err.code(), err.message().c_str());
    return ret.inner_value();
  }

  lldb::SBValue value = ret.inner_value();
  if (value.GetError().GetError()) {
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
  ParsedExpression parsed_expr =
      ParseExpression(frame, expression, opts, error);
  if (error.GetError()) {
    return lldb::SBValue();
  }
  return EvaluateParsedExpression(parsed_expr, error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpression(scope, expression, Options{}, error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 Options opts, lldb::SBError& error) {
  ParsedExpression parsed_expr = ParseExpression(
      scope.GetTarget(), scope.GetType(), expression, opts, error);
  if (error.GetError()) {
    return lldb::SBValue();
  }
  return EvaluateParsedExpression(scope, parsed_expr, error);
}

ParsedExpression ParseExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error) {
  return ParseExpression(frame, expression, Options{}, error);
}

ParsedExpression ParseExpression(lldb::SBFrame frame, const char* expression,
                                 Options opts, lldb::SBError& error) {
  auto source = SourceManager::Create(expression);
  auto context = Context::Create(source, frame);
  auto target = frame.GetThread().GetProcess().GetTarget();
  return ParseExpressionImpl(source, context, opts, target, lldb::SBType(),
                             error);
}

ParsedExpression ParseExpression(lldb::SBTarget target, lldb::SBType scope,
                                 const char* expression, lldb::SBError& error) {
  return ParseExpression(target, scope, expression, Options{}, error);
}

ParsedExpression ParseExpression(lldb::SBTarget target, lldb::SBType scope,
                                 const char* expression, Options opts,
                                 lldb::SBError& error) {
  auto source = SourceManager::Create(expression);
  auto context = Context::Create(source, target, LLDBType::CreateSP(scope));
  return ParseExpressionImpl(source, context, opts, target, scope, error);
}

lldb::SBValue EvaluateParsedExpression(ParsedExpression& expression,
                                       lldb::SBError& error) {
  return EvaluateParsedExpression(expression, ContextVariableList{}, error);
}

lldb::SBValue EvaluateParsedExpression(ParsedExpression& expression,
                                       ContextVariableList context_vars,
                                       lldb::SBError& error) {
  return EvaluateExpressionImpl(expression, context_vars, Value(), error);
}

lldb::SBValue EvaluateParsedExpression(lldb::SBValue scope,
                                       ParsedExpression& expression,
                                       lldb::SBError& error) {
  return EvaluateParsedExpression(scope, expression, ContextVariableList{},
                                  error);
}

lldb::SBValue EvaluateParsedExpression(lldb::SBValue scope,
                                       ParsedExpression& expression,
                                       ContextVariableList context_vars,
                                       lldb::SBError& error) {
  // The `scope` value should be casted to the context type used for parsing.
  // This is allowed only in cases when the `scope`'s type is equal to the
  // context type or it is derived from the context type.

  if (!expression.scope.IsValid()) {
    error = CreateError(ErrorCode::kUnknown,
                        "expression isn't parsed in the context of a type");
    return lldb::SBValue();
  }

  std::vector<uint32_t> path;
  if (!GetPathToBaseType(LLDBType::CreateSP(scope.GetType()),
                         LLDBType::CreateSP(expression.scope), &path,
                         /*offset*/ nullptr)) {
    // If it's not possible to cast the given `scope` value to the type context
    // of parsed expression, return with an error.
    error = CreateError(
        ErrorCode::kUnknown,
        "expression isn't parsed in the context of compatible type");
    return lldb::SBValue();
  }

  // Cast the given `scope` to the expected type.
  std::reverse(path.begin(), path.end());
  for (const auto idx : path) {
    scope = scope.GetChildAtIndex(idx);
  }
  assert(scope.IsValid() && "failed to cast scope variable");

  return EvaluateExpressionImpl(expression, context_vars, Value(scope), error);
}

}  // namespace lldb_eval
