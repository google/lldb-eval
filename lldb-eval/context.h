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

#include <memory>
#include <string>
#include <unordered_map>

#include "ast.h"
#include "clang/Basic/SourceManager.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "parser.h"
#include "parser_context.h"
#include "value.h"

namespace lldb_eval {

class Context : public ParserContext {
 public:
  class IdentifierInfo : public ParserContext::IdentifierInfo {
   public:
    IdentifierInfo(lldb::SBValue value) : value_(std::move(value)) {}

    TypeSP GetType() override { return value_.type(); }
    Value GetValue() const { return value_; }
    bool IsValid() const override { return !!value_.inner_value(); }

   private:
    Value value_;
  };
  static std::shared_ptr<Context> Create(std::string expr, lldb::SBFrame frame);
  static std::shared_ptr<Context> Create(std::string expr, lldb::SBValue scope);

  // This class cannot be safely moved because of the dependency between `expr_`
  // and `smff_`. Users are supposed to pass around the shared pointer.
  Context(Context&&) = delete;
  Context(const Context&) = delete;
  Context& operator=(Context const&) = delete;

  clang::SourceManager& GetSourceManager() const override {
    return smff_->get();
  }
  lldb::SBExecutionContext GetExecutionContext() const { return ctx_; }

  void SetContextVars(
      std::unordered_map<std::string, lldb::SBValue> context_vars);

 public:
  TypeSP GetBasicType(lldb::BasicType basic_type) override;
  TypeSP GetEmptyType() override;
  lldb::BasicType GetSizeType() override;
  lldb::BasicType GetPtrDiffType() override;
  TypeSP ResolveTypeByName(const std::string& name) const override;
  std::unique_ptr<ParserContext::IdentifierInfo> LookupIdentifier(
      const std::string& name) const override;
  bool IsContextVar(const std::string& name) const override;

 private:
  Context(std::string expr, lldb::SBExecutionContext ctx, lldb::SBValue scope);

 public:
  // Store the expression, since SourceManager doesn't take the ownership.
  std::string expr_;
  std::unique_ptr<clang::SourceManagerForFile> smff_;

  // The expression exists in the context of an LLDB target. Execution context
  // provides information for semantic analysis (e.g. resolving types, looking
  // up variables, etc).
  lldb::SBExecutionContext ctx_;

  // If set, the expression is evaluated in the scope of this value: `scope_` is
  // used as `this` pointer and local variables from the current frame are not
  // available.
  mutable lldb::SBValue scope_;

  // Context variables used for identifier lookup.
  std::unordered_map<std::string, lldb::SBValue> context_vars_;

  // Cache of the basic types for the current target.
  std::unordered_map<lldb::BasicType, TypeSP> basic_types_;

  // Cache of the `size_t` type.
  lldb::SBType size_type_;

  // Cache of the `ptrdiff_t` type.
  lldb::SBType ptrdiff_type_;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_EXPRESSION_CONTEXT_H_
