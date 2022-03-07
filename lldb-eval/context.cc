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

#include "lldb-eval/context.h"

#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/FileManager.h"
#include "clang/Basic/SourceManager.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBTypeEnumMember.h"
#include "lldb/API/SBValue.h"
#include "lldb/API/SBValueList.h"
#include "llvm/ADT/Triple.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Support/FormatAdapters.h"
#include "llvm/Support/FormatVariadic.h"

namespace {

lldb::SBValue CreateSBValue(lldb::SBTarget target, const void* bytes,
                            lldb::SBType type) {
  lldb::SBError ignore;
  lldb::SBData data;
  data.SetData(ignore, bytes, type.GetByteSize(), target.GetByteOrder(),
               static_cast<uint8_t>(target.GetAddressByteSize()));

  // CreateValueFromData copies the data referenced by `bytes` to its own
  // storage. `value` should be valid up until this point.
  return target.CreateValueFromData("result", data, type);
}

}  // namespace

namespace lldb_eval {

SourceManager::SourceManager(std::string expr) : expr_(std::move(expr)) {
  // This holds a SourceManager and all of its dependencies.
  smff_ = std::make_unique<clang::SourceManagerForFile>("<expr>", expr_);

  // Disable default diagnostics reporting.
  // TODO(werat): Add custom consumer to keep track of errors.
  clang::DiagnosticsEngine& de = smff_->get().getDiagnostics();
  de.setClient(new clang::IgnoringDiagConsumer);
}

std::shared_ptr<SourceManager> SourceManager::Create(std::string expr) {
  return std::shared_ptr<SourceManager>(new SourceManager(std::move(expr)));
};

void Context::SetContextArgs(
    std::unordered_map<std::string, TypeSP> context_args) {
  context_args_ = std::move(context_args);
}

Context::Context(std::shared_ptr<SourceManager> sm,
                 lldb::SBExecutionContext ctx, TypeSP scope)
    : sm_(std::move(sm)), ctx_(std::move(ctx)), scope_(std::move(scope)) {
  // If `scope_` is a reference, dereference it. This makes identifier lookup
  // in the reference value context more convenient (e.g. avoids constructing
  // qualified name "ScopeType &::IDENTIFIER" for static members).
  if (scope_->IsValid() && scope_->IsReferenceType()) {
    scope_ = scope_->GetDereferencedType();
  }
}

TypeSP Context::GetBasicType(lldb::BasicType basic_type) {
  auto type = basic_types_.find(basic_type);
  if (type != basic_types_.end()) {
    return type->second;
  }

  // Get the basic type from the target and cache it for future calls.
  TypeSP ret = LLDBType::CreateSP(ctx_.GetTarget().GetBasicType(basic_type));
  basic_types_.insert({basic_type, ret});
  return ret;
}

lldb::BasicType Context::GetSizeType() {
  // Determine "size_t" based on OS and architecture. It is "unsigned int" on
  // most 32-bit architectures and "unsigned long" on most 64-bit architectures.
  // On 64-bit Windows, it is "unsigned long long". To see a complete definition
  // for all architectures, refer to
  // https://github.com/llvm/llvm-project/blob/main/clang/lib/Basic/Targets.

  llvm::Triple triple(llvm::Twine(ctx_.GetTarget().GetTriple()));
  if (triple.isOSWindows()) {
    return triple.isArch64Bit() ? lldb::eBasicTypeUnsignedLongLong
                                : lldb::eBasicTypeUnsignedInt;
  } else {
    return triple.isArch64Bit() ? lldb::eBasicTypeUnsignedLong
                                : lldb::eBasicTypeUnsignedInt;
  }
}

lldb::BasicType Context::GetPtrDiffType() {
  // Determine "ptrdiff_t" based on OS and architecture. It is "int" on most
  // 32-bit architectures and "long" on most 64-bit architectures. On 64-bit
  // Windows, it is "long long". To see a complete definition for all
  // architectures, refer to
  // https://github.com/llvm/llvm-project/blob/main/clang/lib/Basic/Targets.

  llvm::Triple triple(llvm::Twine(ctx_.GetTarget().GetTriple()));
  if (triple.isOSWindows()) {
    return triple.isArch64Bit() ? lldb::eBasicTypeLongLong
                                : lldb::eBasicTypeInt;
  } else {
    return triple.isArch64Bit() ? lldb::eBasicTypeLong : lldb::eBasicTypeInt;
  }
}

TypeSP Context::GetEmptyType() const {
  return LLDBType::CreateSP(lldb::SBType());
}

TypeSP Context::ResolveTypeByName(const std::string& name) const {
  // TODO(b/163308825): Do scope-aware type lookup. Look for the types defined
  // in the current scope (function, class, namespace) and prioritize them.

  // Internally types don't have global scope qualifier in their names and
  // LLDB doesn't support queries with it too.
  llvm::StringRef name_ref(name);
  bool global_scope = false;

  if (name_ref.startswith("::")) {
    name_ref = name_ref.drop_front(2);
    global_scope = true;
  }

  // SBTarget::FindTypes will return all matched types, including the ones one
  // in different scopes. I.e. if seaching for "myint", this will also return
  // "ns::myint" and "Foo::myint".
  lldb::SBTypeList types = ctx_.GetTarget().FindTypes(name_ref.data());

  // We've found multiple types, try finding the "correct" one.
  lldb::SBType full_match;
  std::vector<lldb::SBType> partial_matches;

  for (uint32_t i = 0; i < types.GetSize(); ++i) {
    lldb::SBType type = types.GetTypeAtIndex(i);
    llvm::StringRef type_name = type.GetName();

    if (type_name == name_ref) {
      full_match = type;
    } else if (type_name.endswith(name_ref)) {
      partial_matches.push_back(type);
    }
  }

  if (global_scope) {
    // Look only for full matches when looking for a globally qualified type.
    if (full_match.IsValid()) {
      return LLDBType::CreateSP(full_match);
    }
  } else {
    // TODO(b/163308825): We're looking for type, but there may be multiple
    // candidates and which one is correct depends on the currect scope. For now
    // just pick the most "probable" type.

    // Full match is always correct if we're currently in the global scope.
    if (full_match.IsValid()) {
      return LLDBType::CreateSP(full_match);
    }

    // If we have partial matches, pick a "random" one.
    if (partial_matches.size() > 0) {
      return LLDBType::CreateSP(partial_matches.back());
    }
  }

  return LLDBType::CreateSP(lldb::SBType());
}

static lldb::SBValue LookupStaticIdentifier(lldb::SBTarget target,
                                            const llvm::StringRef& name_ref) {
  // List global variable with the same "basename". There can be many matches
  // from other scopes (namespaces, classes), so we do additional filtering
  // later.
  lldb::SBValueList values = target.FindGlobalVariables(
      name_ref.data(), /*max_matches=*/std::numeric_limits<uint32_t>::max());

  // Find the corrent variable by matching the name. lldb::SBValue::GetName()
  // can return strings like "::globarVar", "ns::i" or "int const ns::foo"
  // depending on the version and the platform.
  for (uint32_t i = 0; i < values.GetSize(); ++i) {
    lldb::SBValue val = values.GetValueAtIndex(i);
    llvm::StringRef val_name = val.GetName();

    if (val_name == name_ref ||
        val_name == llvm::formatv("::{0}", name_ref).str() ||
        val_name.endswith(llvm::formatv(" {0}", name_ref).str()) ||
        val_name.endswith(llvm::formatv("*{0}", name_ref).str()) ||
        val_name.endswith(llvm::formatv("&{0}", name_ref).str())) {
      return val;
    }
  }
  return lldb::SBValue();
}

std::unique_ptr<ParserContext::IdentifierInfo> Context::LookupIdentifier(
    const std::string& name) const {
  // Context arguments take precedence over other identifiers (local/global
  // variables, enum values, registers).
  auto context_arg = context_args_.find(name);
  if (context_arg != context_args_.end()) {
    return IdentifierInfo::FromContextArg(context_arg->second);
  }

  llvm::StringRef name_ref(name);

  // Support $rax as a special syntax for accessing registers.
  // Will return an invalid value in case the requested register doesn't exist.
  if (name_ref.startswith("$")) {
    const char* reg_name = name_ref.drop_front(1).data();
    return IdentifierInfo::FromValue(ctx_.GetFrame().FindRegister(reg_name));
  }

  // Internally values don't have global scope qualifier in their names and
  // LLDB doesn't support queries with it too.
  bool global_scope = false;
  if (name_ref.startswith("::")) {
    name_ref = name_ref.drop_front(2);
    global_scope = true;
  }

  // If the identifier doesn't refer to the global scope and doesn't have any
  // other scope qualifiers, try looking among the local and instance variables.
  if (!global_scope && !name_ref.contains("::")) {
    if (!scope_->IsValid()) {
      // Lookup in the current frame.
      lldb::SBFrame frame = ctx_.GetFrame();
      // Try looking for a local variable in current scope.
      lldb::SBValue value = frame.FindVariable(name_ref.data());
      if (value) {
        // Force static value, otherwise we can end up with the "real" type.
        return IdentifierInfo::FromValue(value.GetStaticValue());
      }
      // Try looking for an instance variable (class member).
      value =
          frame.FindVariable("this").GetChildMemberWithName(name_ref.data());
      if (value) {
        // Force static value, otherwise we can end up with the "real" type.
        return IdentifierInfo::FromValue(value.GetStaticValue());
      }
    } else {
      // In a "value" scope `this` refers to the scope object itself.
      if (name_ref == "this") {
        return IdentifierInfo::FromThisKeyword(scope_->GetPointerType());
      }
      // Lookup the variable as a member of the current scope value.
      auto [member, path] = GetMemberInfo(scope_, name_ref.data());
      if (member) {
        return IdentifierInfo::FromMemberPath(member.type, std::move(path));
      }
    }
  }

  // Try looking for a global or static variable.

  // TODO(werat): Implement scope-aware lookup. Relative scopes should be
  // resolved relative to the current scope. I.e. if the current frame is in
  // "ns1::ns2::Foo()", then "ns2::x" should resolve to "ns1::ns2::x".

  lldb::SBValue value;
  if (scope_ && !global_scope) {
    // Try looking for static member of the current scope value, e.g.
    // `ScopeType::NAME`. NAME can include nested struct (`Nested::SUBNAME`),
    // but it cannot be part of the global scope (start with "::").
    const char* type_name = scope_->GetCanonicalType()->GetName().data();
    std::string name_with_type_prefix =
        llvm::formatv("{0}::{1}", type_name, name_ref).str();
    value = LookupStaticIdentifier(ctx_.GetTarget(), name_with_type_prefix);
  }

  // Lookup a regular global variable.
  if (!value) {
    value = LookupStaticIdentifier(ctx_.GetTarget(), name_ref);
  }

  // Try looking up enum value.
  if (!value && name_ref.contains("::")) {
    auto [enum_typename, enumerator_name] = name_ref.rsplit("::");

    auto type = ResolveTypeByName(enum_typename.str());
    lldb::SBTypeEnumMemberList members = ToSBType(type).GetEnumMembers();

    for (size_t i = 0; i < members.GetSize(); i++) {
      lldb::SBTypeEnumMember member = members.GetTypeEnumMemberAtIndex(i);
      if (member.GetName() == enumerator_name) {
        uint64_t bytes = member.GetValueAsUnsigned();
        value = CreateSBValue(ctx_.GetTarget(), &bytes, ToSBType(type));
        break;
      }
    }
  }

  // Last resort, lookup as a register (e.g. `rax` or `rip`).
  if (!value) {
    value = ctx_.GetFrame().FindRegister(name_ref.data());
  }

  // Force static value, otherwise we can end up with the "real" type.
  return IdentifierInfo::FromValue(value.GetStaticValue());
}

bool Context::IsContextVar(const std::string& name) const {
  return context_args_.find(name) != context_args_.end();
}

std::shared_ptr<Context> Context::Create(std::shared_ptr<SourceManager> sm,
                                         lldb::SBFrame frame) {
  return std::shared_ptr<Context>(
      new Context(std::move(sm), lldb::SBExecutionContext(frame),
                  LLDBType::CreateSP(lldb::SBType())));
}

std::shared_ptr<Context> Context::Create(std::shared_ptr<SourceManager> sm,
                                         lldb::SBTarget target, TypeSP scope) {
  // SBValues created via SBTarget::CreateValueFromData don't have SBFrame
  // associated with them. But they still have a process/target, so use that
  // instead.
  return std::shared_ptr<Context>(
      new Context(std::move(sm), lldb::SBExecutionContext(target), scope));
}

}  // namespace lldb_eval
