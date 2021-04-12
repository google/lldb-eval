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

#ifndef LLDB_EVAL_AST_H_
#define LLDB_EVAL_AST_H_

#include <memory>
#include <string>
#include <vector>

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TokenKinds.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBType.h"

namespace lldb_eval {

class Visitor;

// TODO(werat): Save original token and the source position, so we can give
// better diagnostic messages during the evaluation.
class AstNode {
 public:
  AstNode(clang::SourceLocation location) : location_(location) {}
  virtual ~AstNode() {}

  virtual void Accept(Visitor* v) const = 0;

  virtual bool is_error() const { return false; };
  virtual bool is_rvalue() const = 0;
  virtual bool is_bitfield() const { return false; };
  virtual bool is_context_var() const { return false; };
  virtual bool is_literal_zero() const { return false; }
  virtual lldb::SBType result_type() const = 0;

  clang::SourceLocation location() const { return location_; }

  // The expression result type, but dereferenced in case it's a reference. This
  // is for convenience, since for the purposes of the semantic analysis only
  // the dereferenced type matters.
  lldb::SBType result_type_deref() const;

 private:
  clang::SourceLocation location_;
};

using ExprResult = std::unique_ptr<AstNode>;

class ErrorNode : public AstNode {
 public:
  ErrorNode() : AstNode(clang::SourceLocation()) {}
  void Accept(Visitor* v) const override;
  bool is_error() const override { return true; }
  bool is_rvalue() const override { return false; }
  lldb::SBType result_type() const override { return lldb::SBType(); }
};

class LiteralNode : public AstNode {
 public:
  LiteralNode(clang::SourceLocation location, Value value, bool is_literal_zero)
      : AstNode(location),
        value_(std::move(value)),
        is_literal_zero_(is_literal_zero) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  bool is_literal_zero() const override { return is_literal_zero_; }
  lldb::SBType result_type() const override { return value_.type(); }

  Value value() const { return value_; }

 private:
  Value value_;
  bool is_literal_zero_;
};

class IdentifierNode : public AstNode {
 public:
  IdentifierNode(clang::SourceLocation location, std::string name, Value value,
                 bool is_rvalue, bool is_context_var)
      : AstNode(location),
        is_rvalue_(is_rvalue),
        is_context_var_(is_context_var),
        name_(std::move(name)),
        value_(std::move(value)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return is_rvalue_; }
  bool is_context_var() const override { return is_context_var_; };
  lldb::SBType result_type() const override { return value_.type(); }

  std::string name() const { return name_; }
  Value value() const { return value_; }

 private:
  bool is_rvalue_;
  bool is_context_var_;
  std::string name_;
  Value value_;
};

class SizeOfNode : public AstNode {
 public:
  SizeOfNode(clang::SourceLocation location, lldb::SBType type,
             lldb::SBType operand)
      : AstNode(location), type_(type), operand_(operand) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  lldb::SBType result_type() const override { return type_; }

  lldb::SBType operand() const { return operand_; }

 private:
  lldb::SBType type_;
  lldb::SBType operand_;
};

class BuiltinFunctionCallNode : public AstNode {
 public:
  BuiltinFunctionCallNode(clang::SourceLocation location,
                          lldb::SBType result_type, std::string name,
                          std::vector<ExprResult> arguments)
      : AstNode(location),
        result_type_(result_type),
        name_(std::move(name)),
        arguments_(std::move(arguments)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  lldb::SBType result_type() const override { return result_type_; }

  std::string name() const { return name_; }
  const std::vector<ExprResult>& arguments() const { return arguments_; };

 private:
  lldb::SBType result_type_;
  std::string name_;
  std::vector<ExprResult> arguments_;
};

enum class CStyleCastKind {
  kArithmetic,
  kEnumeration,
  kPointer,
  kNullptr,
  kReference,
};

class CStyleCastNode : public AstNode {
 public:
  CStyleCastNode(clang::SourceLocation location, lldb::SBType type,
                 ExprResult rhs, CStyleCastKind kind)
      : AstNode(location),
        type_(std::move(type)),
        rhs_(std::move(rhs)),
        kind_(kind) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  lldb::SBType result_type() const override { return type_; }

  lldb::SBType type() const { return type_; }
  AstNode* rhs() const { return rhs_.get(); }
  CStyleCastKind kind() const { return kind_; }

 private:
  lldb::SBType type_;
  ExprResult rhs_;
  CStyleCastKind kind_;
};

class MemberOfNode : public AstNode {
 public:
  MemberOfNode(clang::SourceLocation location, lldb::SBType result_type,
               ExprResult lhs, bool is_bitfield,
               std::vector<uint32_t> member_index, bool is_arrow)
      : AstNode(location),
        result_type_(result_type),
        lhs_(std::move(lhs)),
        is_bitfield_(is_bitfield),
        member_index_(std::move(member_index)),
        is_arrow_(is_arrow) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return false; }
  bool is_bitfield() const override { return is_bitfield_; }
  lldb::SBType result_type() const override { return result_type_; }

  AstNode* lhs() const { return lhs_.get(); }
  const std::vector<uint32_t>& member_index() const { return member_index_; }
  bool is_arrow() const { return is_arrow_; }

 private:
  lldb::SBType result_type_;
  ExprResult lhs_;
  bool is_bitfield_;
  std::vector<uint32_t> member_index_;
  bool is_arrow_;
};

class ArraySubscriptNode : public AstNode {
 public:
  ArraySubscriptNode(clang::SourceLocation location, lldb::SBType result_type,
                     ExprResult base, ExprResult index, bool is_pointer_base)
      : AstNode(location),
        result_type_(result_type),
        base_(std::move(base)),
        index_(std::move(index)),
        is_pointer_base_(is_pointer_base) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return false; }
  lldb::SBType result_type() const override { return result_type_; }

  AstNode* base() const { return base_.get(); }
  AstNode* index() const { return index_.get(); }
  bool is_pointer_base() const { return is_pointer_base_; }

 private:
  lldb::SBType result_type_;
  ExprResult base_;
  ExprResult index_;
  bool is_pointer_base_;
};

enum class BinaryOpKind {
  Mul,        // "*"
  Div,        // "/"
  Rem,        // "%"
  Add,        // "+"
  Sub,        // "-"
  Shl,        // "<<"
  Shr,        // ">>"
  LT,         // "<"
  GT,         // ">"
  LE,         // "<="
  GE,         // ">="
  EQ,         // "=="
  NE,         // "!="
  And,        // "&"
  Xor,        // "^"
  Or,         // "|"
  LAnd,       // "&&"
  LOr,        // "||"
  Assign,     // "="
  MulAssign,  // "*="
  DivAssign,  // "/="
  RemAssign,  // "%="
  AddAssign,  // "+="
  SubAssign,  // "-="
  ShlAssign,  // "<<="
  ShrAssign,  // ">>="
  AndAssign,  // "&="
  XorAssign,  // "^="
  OrAssign,   // "|="
};

std::string to_string(BinaryOpKind kind);
BinaryOpKind clang_token_kind_to_binary_op_kind(
    clang::tok::TokenKind token_kind);
bool binary_op_kind_is_comp_assign(BinaryOpKind kind);

class BinaryOpNode : public AstNode {
 public:
  BinaryOpNode(clang::SourceLocation location, lldb::SBType result_type,
               BinaryOpKind kind, ExprResult lhs, ExprResult rhs)
      : AstNode(location),
        result_type_(result_type),
        kind_(kind),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override {
    return !binary_op_kind_is_comp_assign(kind_);
  }
  lldb::SBType result_type() const override { return result_type_; }

  BinaryOpKind kind() const { return kind_; }
  AstNode* lhs() const { return lhs_.get(); }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  lldb::SBType result_type_;
  BinaryOpKind kind_;
  ExprResult lhs_;
  ExprResult rhs_;
};

enum class UnaryOpKind {
  PostInc,  // "++"
  PostDec,  // "--"
  PreInc,   // "++"
  PreDec,   // "--"
  AddrOf,   // "&"
  Deref,    // "*"
  Plus,     // "+"
  Minus,    // "-"
  Not,      // "~"
  LNot,     // "!"
};

std::string to_string(UnaryOpKind kind);

class UnaryOpNode : public AstNode {
 public:
  UnaryOpNode(clang::SourceLocation location, lldb::SBType result_type,
              UnaryOpKind kind, ExprResult rhs)
      : AstNode(location),
        result_type_(result_type),
        kind_(kind),
        rhs_(std::move(rhs)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return kind_ != UnaryOpKind::Deref; }
  lldb::SBType result_type() const override { return result_type_; }

  UnaryOpKind kind() const { return kind_; }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  lldb::SBType result_type_;
  UnaryOpKind kind_;
  ExprResult rhs_;
};

class TernaryOpNode : public AstNode {
 public:
  TernaryOpNode(clang::SourceLocation location, lldb::SBType result_type,
                ExprResult cond, ExprResult lhs, ExprResult rhs)
      : AstNode(location),
        result_type_(result_type),
        cond_(std::move(cond)),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override {
    return lhs_->is_rvalue() || rhs_->is_rvalue();
  }
  bool is_bitfield() const override {
    return lhs_->is_bitfield() || rhs_->is_bitfield();
  }
  lldb::SBType result_type() const override { return result_type_; }

  AstNode* cond() const { return cond_.get(); }
  AstNode* lhs() const { return lhs_.get(); }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  lldb::SBType result_type_;
  ExprResult cond_;
  ExprResult lhs_;
  ExprResult rhs_;
};

class Visitor {
 public:
  virtual ~Visitor() {}
  virtual void Visit(const ErrorNode* node) = 0;
  virtual void Visit(const LiteralNode* node) = 0;
  virtual void Visit(const IdentifierNode* node) = 0;
  virtual void Visit(const SizeOfNode* node) = 0;
  virtual void Visit(const BuiltinFunctionCallNode* node) = 0;
  virtual void Visit(const CStyleCastNode* node) = 0;
  virtual void Visit(const MemberOfNode* node) = 0;
  virtual void Visit(const ArraySubscriptNode* node) = 0;
  virtual void Visit(const BinaryOpNode* node) = 0;
  virtual void Visit(const UnaryOpNode* node) = 0;
  virtual void Visit(const TernaryOpNode* node) = 0;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_AST_H_
