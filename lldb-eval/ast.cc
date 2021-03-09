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

#include "lldb-eval/defines.h"

namespace lldb_eval {

std::string to_string(UnaryOpKind kind) {
  switch (kind) {
    case UnaryOpKind::PostInc:
      return "postfix '++'";
    case UnaryOpKind::PostDec:
      return "postfix '--'";
    case UnaryOpKind::PreInc:
      return "prefix '++'";
    case UnaryOpKind::PreDec:
      return "prefix '--'";
    case UnaryOpKind::AddrOf:
      return "prefix '&'";
    case UnaryOpKind::Deref:
      return "prefix '*'";
    case UnaryOpKind::Plus:
      return "prefix '+'";
    case UnaryOpKind::Minus:
      return "prefix '-'";
    case UnaryOpKind::Not:
      return "prefix '~'";
    case UnaryOpKind::LNot:
      return "prefix '!'";
  }
  lldb_eval_unreachable("did you add an element to UnaryOpKind?");
}

lldb::SBType AstNode::result_type_deref() {
  lldb::SBType type = result_type();
  return type.IsReferenceType() ? type.GetDereferencedType() : type;
}

void ErrorNode::Accept(Visitor* v) const { v->Visit(this); }

void LiteralNode::Accept(Visitor* v) const { v->Visit(this); }

void IdentifierNode::Accept(Visitor* v) const { v->Visit(this); }

void SizeOfNode::Accept(Visitor* v) const { v->Visit(this); }

void BuiltinFunctionCallNode::Accept(Visitor* v) const { v->Visit(this); }

void CStyleCastNode::Accept(Visitor* v) const { v->Visit(this); }

void MemberOfNode::Accept(Visitor* v) const { v->Visit(this); }

void ArraySubscriptNode::Accept(Visitor* v) const { v->Visit(this); }

void BinaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void UnaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void TernaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

}  // namespace lldb_eval
