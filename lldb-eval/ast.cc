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

std::string to_string(BinaryOpKind kind) {
  switch (kind) {
      // clang-format off
    case BinaryOpKind::Mul:       return "'*'";
    case BinaryOpKind::Div:       return "'/'";
    case BinaryOpKind::Rem:       return "'%'";
    case BinaryOpKind::Add:       return "'+'";
    case BinaryOpKind::Sub:       return "'-'";
    case BinaryOpKind::Shl:       return "'<<'";
    case BinaryOpKind::Shr:       return "'>>'";
    case BinaryOpKind::LT:        return "'<'";
    case BinaryOpKind::GT:        return "'>'";
    case BinaryOpKind::LE:        return "'<='";
    case BinaryOpKind::GE:        return "'>='";
    case BinaryOpKind::EQ:        return "'=='";
    case BinaryOpKind::NE:        return "'!='";
    case BinaryOpKind::And:       return "'&'";
    case BinaryOpKind::Xor:       return "'^'";
    case BinaryOpKind::Or:        return "'|'";
    case BinaryOpKind::LAnd:      return "'&&'";
    case BinaryOpKind::LOr:       return "'||'";
    case BinaryOpKind::Assign:    return "'='";
    case BinaryOpKind::MulAssign: return "'*='";
    case BinaryOpKind::DivAssign: return "'/='";
    case BinaryOpKind::RemAssign: return "'%='";
    case BinaryOpKind::AddAssign: return "'+='";
    case BinaryOpKind::SubAssign: return "'-='";
    case BinaryOpKind::ShlAssign: return "'<<='";
    case BinaryOpKind::ShrAssign: return "'>>='";
    case BinaryOpKind::AndAssign: return "'&='";
    case BinaryOpKind::XorAssign: return "'^='";
    case BinaryOpKind::OrAssign:  return "'|='";
      // clang-format on
  }
  lldb_eval_unreachable("did you add an element to BinaryOpKind?");
}

BinaryOpKind clang_token_kind_to_binary_op_kind(
    clang::tok::TokenKind token_kind) {
  switch (token_kind) {
      // clang-format off
    case clang::tok::star:                return BinaryOpKind::Mul;
    case clang::tok::slash:               return BinaryOpKind::Div;
    case clang::tok::percent:             return BinaryOpKind::Rem;
    case clang::tok::plus:                return BinaryOpKind::Add;
    case clang::tok::minus:               return BinaryOpKind::Sub;
    case clang::tok::lessless:            return BinaryOpKind::Shl;
    case clang::tok::greatergreater:      return BinaryOpKind::Shr;
    case clang::tok::less:                return BinaryOpKind::LT;
    case clang::tok::greater:             return BinaryOpKind::GT;
    case clang::tok::lessequal:           return BinaryOpKind::LE;
    case clang::tok::greaterequal:        return BinaryOpKind::GE;
    case clang::tok::equalequal:          return BinaryOpKind::EQ;
    case clang::tok::exclaimequal:        return BinaryOpKind::NE;
    case clang::tok::amp:                 return BinaryOpKind::And;
    case clang::tok::caret:               return BinaryOpKind::Xor;
    case clang::tok::pipe:                return BinaryOpKind::Or;
    case clang::tok::ampamp:              return BinaryOpKind::LAnd;
    case clang::tok::pipepipe:            return BinaryOpKind::LOr;
    case clang::tok::equal:               return BinaryOpKind::Assign;
    case clang::tok::starequal:           return BinaryOpKind::MulAssign;
    case clang::tok::slashequal:          return BinaryOpKind::DivAssign;
    case clang::tok::percentequal:        return BinaryOpKind::RemAssign;
    case clang::tok::plusequal:           return BinaryOpKind::AddAssign;
    case clang::tok::minusequal:          return BinaryOpKind::SubAssign;
    case clang::tok::lesslessequal:       return BinaryOpKind::ShlAssign;
    case clang::tok::greatergreaterequal: return BinaryOpKind::ShrAssign;
    case clang::tok::ampequal:            return BinaryOpKind::AndAssign;
    case clang::tok::caretequal:          return BinaryOpKind::XorAssign;
    case clang::tok::pipeequal:           return BinaryOpKind::OrAssign;
      // clang-format on

    default:
      break;
  }
  lldb_eval_unreachable("did you add an element to BinaryOpKind?");
}

bool binary_op_kind_is_comp_assign(BinaryOpKind kind) {
  switch (kind) {
    case BinaryOpKind::Assign:
    case BinaryOpKind::MulAssign:
    case BinaryOpKind::DivAssign:
    case BinaryOpKind::RemAssign:
    case BinaryOpKind::AddAssign:
    case BinaryOpKind::SubAssign:
    case BinaryOpKind::ShlAssign:
    case BinaryOpKind::ShrAssign:
    case BinaryOpKind::AndAssign:
    case BinaryOpKind::XorAssign:
    case BinaryOpKind::OrAssign:
      return true;

    default:
      return false;
  }
}

std::string to_string(UnaryOpKind kind) {
  switch (kind) {
      // clang-format off
    case UnaryOpKind::PostInc: return "postfix '++'";
    case UnaryOpKind::PostDec: return "postfix '--'";
    case UnaryOpKind::PreInc:  return "prefix '++'";
    case UnaryOpKind::PreDec:  return "prefix '--'";
    case UnaryOpKind::AddrOf:  return "prefix '&'";
    case UnaryOpKind::Deref:   return "prefix '*'";
    case UnaryOpKind::Plus:    return "prefix '+'";
    case UnaryOpKind::Minus:   return "prefix '-'";
    case UnaryOpKind::Not:     return "prefix '~'";
    case UnaryOpKind::LNot:    return "prefix '!'";
      // clang-format on
  }
  lldb_eval_unreachable("did you add an element to UnaryOpKind?");
}

TypeSP AstNode::result_type_deref() const {
  auto type = result_type();
  return type->IsReferenceType() ? type->GetDereferencedType() : type;
}

void ErrorNode::Accept(Visitor* v) const { v->Visit(this); }

void LiteralNode::Accept(Visitor* v) const { v->Visit(this); }

void IdentifierNode::Accept(Visitor* v) const { v->Visit(this); }

void SizeOfNode::Accept(Visitor* v) const { v->Visit(this); }

void BuiltinFunctionCallNode::Accept(Visitor* v) const { v->Visit(this); }

void CStyleCastNode::Accept(Visitor* v) const { v->Visit(this); }

void CxxStaticCastNode::Accept(Visitor* v) const { return v->Visit(this); }

void CxxReinterpretCastNode::Accept(Visitor* v) const { v->Visit(this); }

void MemberOfNode::Accept(Visitor* v) const { v->Visit(this); }

void ArraySubscriptNode::Accept(Visitor* v) const { v->Visit(this); }

void BinaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void UnaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void TernaryOpNode::Accept(Visitor* v) const { v->Visit(this); }

void SmartPtrToPtrDecay::Accept(Visitor* v) const { v->Visit(this); }

}  // namespace lldb_eval
