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

#include <cassert>
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

static lldb::SBError CreateError(ErrorCode code, const char* message) {
  lldb::SBError error;
  error.SetError(static_cast<uint32_t>(code), lldb::eErrorTypeGeneric);
  error.SetErrorString(message);
  return error;
}

static std::shared_ptr<CompiledExpr> CompileExpressionImpl(
    std::shared_ptr<SourceManager> source, std::shared_ptr<Context> ctx,
    Options opts, lldb::SBType scope, lldb::SBError& error) {
  error.Clear();

  // Handle parsing options.
  std::unordered_map<std::string, TypeSP> context_args =
      ConvertToTypeMap(opts.context_args);
  context_args.merge(ConvertToTypeMap(opts.context_vars));
  ctx->SetContextArgs(std::move(context_args));
  ctx->SetAllowSideEffects(opts.allow_side_effects);

  Error err;
  Parser p(ctx);
  ExprResult tree = p.Run(err);
  if (err) {
    error = CreateError(err.code(), err.message().c_str());
    return nullptr;
  }

  return std::make_shared<CompiledExpr>(source, std::move(tree), scope);
}

static lldb::SBValue EvaluateExpressionImpl(
    std::shared_ptr<CompiledExpr> parsed_expr, ContextVariableList context_vars,
    lldb::SBTarget target, Value scope, lldb::SBError& error) {
  Interpreter eval(target, parsed_expr->source, scope);
  if (context_vars.size > 0) {
    eval.SetContextVars(ConvertToValueMap(context_vars));
  }
  Error err;
  Value ret = eval.Eval(parsed_expr->tree.get(), err);
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

CompiledExpr::CompiledExpr(std::shared_ptr<SourceManager> source,
                           std::unique_ptr<AstNode> tree, lldb::SBType scope)
    : source(std::move(source)),
      tree(std::move(tree)),
      scope(std::move(scope)) {
  assert(this->tree && this->tree->result_type() && "ast node should be valid");
  result_type = ToSBType(this->tree->result_type());
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpression(frame, expression, Options{}, error);
}

lldb::SBValue EvaluateExpression(lldb::SBFrame frame, const char* expression,
                                 Options opts, lldb::SBError& error) {
  auto source = SourceManager::Create(expression);
  auto context = Context::Create(source, frame);
  auto compiled_expr =
      CompileExpressionImpl(source, context, opts, lldb::SBType(), error);

  if (error) {
    return lldb::SBValue();
  }

  auto target = frame.GetThread().GetProcess().GetTarget();
  return EvaluateExpressionImpl(compiled_expr, opts.context_vars, target,
                                Value(), error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 lldb::SBError& error) {
  return EvaluateExpression(scope, expression, Options{}, error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope, const char* expression,
                                 Options opts, lldb::SBError& error) {
  auto compiled_expr = CompileExpression(scope.GetTarget(), scope.GetType(),
                                         expression, opts, error);
  if (error.GetError()) {
    return lldb::SBValue();
  }
  return EvaluateExpression(scope, compiled_expr, opts.context_vars, error);
}

std::shared_ptr<CompiledExpr> CompileExpression(lldb::SBTarget target,
                                                lldb::SBType scope,
                                                const char* expression,
                                                lldb::SBError& error) {
  return CompileExpression(target, scope, expression, Options{}, error);
}

std::shared_ptr<CompiledExpr> CompileExpression(lldb::SBTarget target,
                                                lldb::SBType scope,
                                                const char* expression,
                                                Options opts,
                                                lldb::SBError& error) {
  auto source = SourceManager::Create(expression);
  auto context = Context::Create(source, target, LLDBType::CreateSP(scope));
  return CompileExpressionImpl(source, context, opts, scope, error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope,
                                 std::shared_ptr<CompiledExpr> expression,
                                 lldb::SBError& error) {
  return EvaluateExpression(scope, expression, ContextVariableList{}, error);
}

lldb::SBValue EvaluateExpression(lldb::SBValue scope,
                                 std::shared_ptr<CompiledExpr> expression,
                                 ContextVariableList context_vars,
                                 lldb::SBError& error) {
  // The `scope` value should be casted to the context type used for parsing.
  // This is allowed only in cases when the `scope`'s type is equal to the
  // context type or it is derived from the context type.

  std::vector<uint32_t> path;
  if (!GetPathToBaseType(LLDBType::CreateSP(scope.GetType()),
                         LLDBType::CreateSP(expression->scope), &path,
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

  return EvaluateExpressionImpl(expression, context_vars, scope.GetTarget(),
                                Value(scope), error);
}

}  // namespace lldb_eval
