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

#include "clang/Basic/TokenKinds.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBType.h"

namespace lldb_eval {

class Visitor;

// TODO(werat): Save original token and the source position, so we can give
// better diagnostic messages during the evaluation.
class AstNode {
 public:
  virtual ~AstNode() {}

  virtual void Accept(Visitor* v) const = 0;

  virtual bool is_rvalue() const = 0;
  virtual bool is_bitfield() const { return false; };
  virtual lldb::SBType result_type() const = 0;

  // The expression result type, but dereferenced in case it's a reference. This
  // is for convenience, since for the purposes of the semantic analysis only
  // the dereferenced type matters.
  lldb::SBType result_type_deref();
};

using ExprResult = std::unique_ptr<AstNode>;

class ErrorNode : public AstNode {
  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return false; }
  lldb::SBType result_type() const override { return lldb::SBType(); }
};

class LiteralNode : public AstNode {
 public:
  LiteralNode(Value value) : value_(std::move(value)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  lldb::SBType result_type() const override { return value_.type(); }

  Value value() const { return value_; }

 private:
  Value value_;
};

class IdentifierNode : public AstNode {
 public:
  IdentifierNode(std::string name, Value value, bool is_rvalue)
      : is_rvalue_(is_rvalue),
        name_(std::move(name)),
        value_(std::move(value)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return is_rvalue_; }
  lldb::SBType result_type() const override { return value_.type(); }

  std::string name() const { return name_; }
  Value value() const { return value_; }

 private:
  bool is_rvalue_;
  std::string name_;
  Value value_;
};

class SizeOfNode : public AstNode {
 public:
  SizeOfNode(lldb::SBType type, lldb::SBType operand)
      : type_(type), operand_(operand) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  lldb::SBType result_type() const override { return type_; }

  lldb::SBType operand() const { return operand_; }

 private:
  lldb::SBType type_;
  lldb::SBType operand_;
};

enum class CStyleCastKind {
  kArithmetic,
  kEnumeration,
  kPointer,
  kReference,
};

class CStyleCastNode : public AstNode {
 public:
  CStyleCastNode(lldb::SBType type, ExprResult rhs, CStyleCastKind kind)
      : type_(std::move(type)), rhs_(std::move(rhs)), kind_(kind) {}

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
  MemberOfNode(lldb::SBType result_type, ExprResult lhs, bool is_bitfield,
               std::vector<uint32_t> member_index, bool is_arrow)
      : result_type_(result_type),
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
  ArraySubscriptNode(lldb::SBType result_type, ExprResult base,
                     ExprResult index, bool is_pointer_base)
      : result_type_(result_type),
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

class BinaryOpNode : public AstNode {
 public:
  BinaryOpNode(lldb::SBType result_type, clang::tok::TokenKind op,
               ExprResult lhs, ExprResult rhs)
      : result_type_(result_type),
        op_(op),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  lldb::SBType result_type() const override { return result_type_; }

  clang::tok::TokenKind op() const { return op_; }
  std::string op_name() const { return clang::tok::getTokenName(op_); }
  AstNode* lhs() const { return lhs_.get(); }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  lldb::SBType result_type_;
  // TODO(werat): Use custom enum with binary operators.
  clang::tok::TokenKind op_;
  ExprResult lhs_;
  ExprResult rhs_;
};

class UnaryOpNode : public AstNode {
 public:
  UnaryOpNode(lldb::SBType result_type, clang::tok::TokenKind op,
              ExprResult rhs)
      : result_type_(result_type), op_(op), rhs_(std::move(rhs)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return op_ != clang::tok::star; }
  lldb::SBType result_type() const override { return result_type_; }

  clang::tok::TokenKind op() const { return op_; }
  std::string op_name() const { return clang::tok::getTokenName(op_); }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  lldb::SBType result_type_;
  // TODO(werat): Use custom enum with unary operators.
  clang::tok::TokenKind op_;
  ExprResult rhs_;
};

class TernaryOpNode : public AstNode {
 public:
  TernaryOpNode(lldb::SBType result_type, ExprResult cond, ExprResult lhs,
                ExprResult rhs)
      : result_type_(result_type),
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
  virtual void Visit(const CStyleCastNode* node) = 0;
  virtual void Visit(const MemberOfNode* node) = 0;
  virtual void Visit(const ArraySubscriptNode* node) = 0;
  virtual void Visit(const BinaryOpNode* node) = 0;
  virtual void Visit(const UnaryOpNode* node) = 0;
  virtual void Visit(const TernaryOpNode* node) = 0;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_AST_H_
