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

#include "lldb-eval/ast.h"

#include <string>

#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/FormatVariadic.h"

namespace {

void StringReplace(std::string& str, const std::string& old_value,
                   const std::string& new_value) {
  size_t pos = str.find(old_value);
  if (pos != std::string::npos) {
    str.replace(pos, old_value.length(), new_value);
  }
}

}  // namespace

namespace lldb_eval {

std::string TypeDeclaration::GetName() const {
  // Full name is a combination of a base name and pointer operators.
  std::string name = GetBaseName();

  // In LLDB pointer operators are separated with a single whitespace.
  if (ptr_operators_.size() > 0) {
    name.append(" ");
  }
  for (auto tok : ptr_operators_) {
    if (tok == clang::tok::star) {
      name.append("*");
    } else if (tok == clang::tok::amp) {
      name.append("&");
    }
  }
  return name;
}

std::string TypeDeclaration::GetBaseName() const {
  // TODO(werat): Implement more robust textual type representation.
  std::string base_name = llvm::formatv(
      "{0:$[ ]}", llvm::make_range(typenames_.begin(), typenames_.end()));

  // TODO(werat): Handle these type aliases and detect invalid type combinations
  // (e.g. "long char") during the TypeDeclaration construction.
  StringReplace(base_name, "short int", "short");
  StringReplace(base_name, "long int", "long");

  return base_name;
}

void ErrorNode::Accept(Visitor* v) const { v->Visit(this); }

void LiteralNode::Accept(Visitor* v) const { v->Visit(this); }

void IdentifierNode::Accept(Visitor* v) const { v->Visit(this); }

void CStyleCastNode::Accept(Visitor* v) const { v->Visit(this); }

void MemberOfNode::Accept(Visitor* v) const { v->Visit(this); }

void BinaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void UnaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void TernaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

}  // namespace lldb_eval
