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

#include "clang/Basic/SourceManager.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "parser_context.h"
#include "value.h"

namespace lldb_eval {

// clang::SourceManager wrapper which takes ownership of the expression string.
class SourceManager {
 public:
  static std::shared_ptr<SourceManager> Create(std::string expr);

  // This class cannot be safely moved because of the dependency between `expr_`
  // and `smff_`. Users are supposed to pass around the shared pointer.
  SourceManager(SourceManager&&) = delete;
  SourceManager(const SourceManager&) = delete;
  SourceManager& operator=(SourceManager const&) = delete;

  clang::SourceManager& GetSourceManager() const { return smff_->get(); }

 private:
  explicit SourceManager(std::string expr);

 private:
  // Store the expression, since SourceManagerForFile doesn't take the
  // ownership.
  std::string expr_;
  std::unique_ptr<clang::SourceManagerForFile> smff_;
};

class Context : public ParserContext {
 public:
  class IdentifierInfo : public ParserContext::IdentifierInfo {
   private:
    using MemberPath = std::vector<uint32_t>;
    using IdentifierInfoPtr = std::unique_ptr<ParserContext::IdentifierInfo>;

   public:
    enum class Kind {
      kValue,
      kContextArg,
      kMemberPath,
      kThisKeyword,
    };

    static IdentifierInfoPtr FromValue(lldb::SBValue value) {
      TypeSP type = LLDBType::CreateSP(value.GetType());
      return IdentifierInfoPtr(new IdentifierInfo(Kind::kValue, std::move(type),
                                                  Value(std::move(value)), {}));
    }
    static IdentifierInfoPtr FromContextArg(TypeSP type) {
      return IdentifierInfoPtr(
          new IdentifierInfo(Kind::kContextArg, std::move(type), Value(), {}));
    }
    static IdentifierInfoPtr FromMemberPath(TypeSP type, MemberPath path) {
      return IdentifierInfoPtr(new IdentifierInfo(
          Kind::kMemberPath, std::move(type), Value(), std::move(path)));
    }
    static IdentifierInfoPtr FromThisKeyword(TypeSP type) {
      return IdentifierInfoPtr(
          new IdentifierInfo(Kind::kThisKeyword, std::move(type), Value(), {}));
    }

    Kind kind() const { return kind_; }
    Value value() const { return value_; }
    const MemberPath& path() const { return path_; }

    // from ParserContext::IdentifierInfo:
    TypeSP GetType() override { return type_; }
    bool IsValid() const override { return type_->IsValid(); }

   private:
    IdentifierInfo(Kind kind, TypeSP type, Value value, MemberPath path)
        : kind_(kind),
          type_(std::move(type)),
          value_(std::move(value)),
          path_(std::move(path)) {}

   private:
    Kind kind_;
    TypeSP type_;
    Value value_;
    MemberPath path_;
  };

  static std::shared_ptr<Context> Create(std::shared_ptr<SourceManager> sm,
                                         lldb::SBFrame frame);
  static std::shared_ptr<Context> Create(std::shared_ptr<SourceManager> sm,
                                         lldb::SBTarget target, TypeSP scope);

  clang::SourceManager& GetSourceManager() const override {
    return sm_->GetSourceManager();
  }
  lldb::SBExecutionContext GetExecutionContext() const { return ctx_; }

  void SetContextArgs(std::unordered_map<std::string, TypeSP> context_args);

 public:
  TypeSP GetBasicType(lldb::BasicType basic_type) override;
  TypeSP GetEmptyType() const override;
  lldb::BasicType GetSizeType() override;
  lldb::BasicType GetPtrDiffType() override;
  TypeSP ResolveTypeByName(const std::string& name) const override;
  std::unique_ptr<ParserContext::IdentifierInfo> LookupIdentifier(
      const std::string& name) const override;
  bool IsContextVar(const std::string& name) const override;

 private:
  Context(std::shared_ptr<SourceManager> sm, lldb::SBExecutionContext ctx,
          TypeSP scope);

 private:
  std::shared_ptr<SourceManager> sm_;

  // The expression exists in the context of an LLDB target. Execution context
  // provides information for semantic analysis (e.g. resolving types, looking
  // up variables, etc).
  lldb::SBExecutionContext ctx_;

  // If set, the expression is evaluated in the scope of this value: `scope_` is
  // used as `this` pointer and local variables from the current frame are not
  // available.
  TypeSP scope_;

  // Context arguments used for identifier lookup.
  std::unordered_map<std::string, TypeSP> context_args_;

  // Cache of the basic types for the current target.
  std::unordered_map<lldb::BasicType, TypeSP> basic_types_;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_EXPRESSION_CONTEXT_H_
