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
#include <variant>
#include <vector>

#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TokenKinds.h"
#include "lldb-eval/parser_context.h"
#include "lldb-eval/type.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"

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
  virtual uint32_t bitfield_size() const { return 0; }
  virtual TypeSP result_type() const = 0;

  clang::SourceLocation location() const { return location_; }

  // The expression result type, but dereferenced in case it's a reference. This
  // is for convenience, since for the purposes of the semantic analysis only
  // the dereferenced type matters.
  TypeSP result_type_deref() const;

 private:
  clang::SourceLocation location_;
};

using ExprResult = std::unique_ptr<AstNode>;

class ErrorNode : public AstNode {
 public:
  ErrorNode(TypeSP empty_type)
      : AstNode(clang::SourceLocation()), empty_type_(std::move(empty_type)) {}
  void Accept(Visitor* v) const override;
  bool is_error() const override { return true; }
  bool is_rvalue() const override { return false; }
  TypeSP result_type() const override { return empty_type_; }

 private:
  TypeSP empty_type_;
};

class LiteralNode : public AstNode {
 public:
  template <typename ValueT>
  LiteralNode(clang::SourceLocation location, TypeSP type, ValueT&& value,
              bool is_literal_zero)
      : AstNode(location),
        type_(std::move(type)),
        value_(std::forward<ValueT>(value)),
        is_literal_zero_(is_literal_zero) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  bool is_literal_zero() const override { return is_literal_zero_; }
  TypeSP result_type() const override { return type_; }

  template <typename ValueT>
  ValueT value() const {
    return std::get<ValueT>(value_);
  }

  auto value() const { return value_; }

 private:
  TypeSP type_;
  std::variant<llvm::APInt, llvm::APFloat, bool> value_;
  bool is_literal_zero_;
};

class IdentifierNode : public AstNode {
 public:
  IdentifierNode(clang::SourceLocation location, std::string name,
                 std::unique_ptr<ParserContext::IdentifierInfo> identifier,
                 bool is_rvalue, bool is_context_var)
      : AstNode(location),
        is_rvalue_(is_rvalue),
        is_context_var_(is_context_var),
        name_(std::move(name)),
        identifier_(std::move(identifier)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return is_rvalue_; }
  bool is_context_var() const override { return is_context_var_; };
  TypeSP result_type() const override { return identifier_->GetType(); }

  std::string name() const { return name_; }
  const ParserContext::IdentifierInfo& info() const { return *identifier_; }

 private:
  bool is_rvalue_;
  bool is_context_var_;
  std::string name_;
  std::unique_ptr<ParserContext::IdentifierInfo> identifier_;
};

class SizeOfNode : public AstNode {
 public:
  SizeOfNode(clang::SourceLocation location, TypeSP type, TypeSP operand)
      : AstNode(location),
        type_(std::move(type)),
        operand_(std::move(operand)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  TypeSP result_type() const override { return type_; }

  TypeSP operand() const { return operand_; }

 private:
  TypeSP type_;
  TypeSP operand_;
};

class BuiltinFunctionCallNode : public AstNode {
 public:
  BuiltinFunctionCallNode(clang::SourceLocation location, TypeSP result_type,
                          std::string name, std::vector<ExprResult> arguments)
      : AstNode(location),
        result_type_(std::move(result_type)),
        name_(std::move(name)),
        arguments_(std::move(arguments)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return true; }
  TypeSP result_type() const override { return result_type_; }

  std::string name() const { return name_; }
  const std::vector<ExprResult>& arguments() const { return arguments_; };

 private:
  TypeSP result_type_;
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
  CStyleCastNode(clang::SourceLocation location, TypeSP type, ExprResult rhs,
                 CStyleCastKind kind)
      : AstNode(location),
        type_(std::move(type)),
        rhs_(std::move(rhs)),
        kind_(kind) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override {
    return kind_ != CStyleCastKind::kReference;
  }
  TypeSP result_type() const override { return type_; }

  TypeSP type() const { return type_; }
  AstNode* rhs() const { return rhs_.get(); }
  CStyleCastKind kind() const { return kind_; }

 private:
  TypeSP type_;
  ExprResult rhs_;
  CStyleCastKind kind_;
};

enum class CxxStaticCastKind {
  kNoOp,
  kArithmetic,
  kEnumeration,
  kPointer,
  kNullptr,
  kBaseToDerived,
  kDerivedToBase,
};

class CxxStaticCastNode : public AstNode {
 public:
  CxxStaticCastNode(clang::SourceLocation location, TypeSP type, ExprResult rhs,
                    CxxStaticCastKind kind, bool is_rvalue)
      : AstNode(location),
        type_(std::move(type)),
        rhs_(std::move(rhs)),
        kind_(kind),
        is_rvalue_(is_rvalue) {
    assert(kind != CxxStaticCastKind::kBaseToDerived &&
           kind != CxxStaticCastKind::kDerivedToBase &&
           "invalid constructor for base-to-derived and derived-to-base casts");
  }

  CxxStaticCastNode(clang::SourceLocation location, TypeSP type, ExprResult rhs,
                    std::vector<uint32_t> idx, bool is_rvalue)
      : AstNode(location),
        type_(std::move(type)),
        rhs_(std::move(rhs)),
        idx_(std::move(idx)),
        kind_(CxxStaticCastKind::kDerivedToBase),
        is_rvalue_(is_rvalue) {}

  CxxStaticCastNode(clang::SourceLocation location, TypeSP type, ExprResult rhs,
                    uint64_t offset, bool is_rvalue)
      : AstNode(location),
        type_(std::move(type)),
        rhs_(std::move(rhs)),
        offset_(offset),
        kind_(CxxStaticCastKind::kBaseToDerived),
        is_rvalue_(is_rvalue) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return is_rvalue_; }
  TypeSP result_type() const override { return type_; }

  TypeSP type() const { return type_; }
  AstNode* rhs() const { return rhs_.get(); }
  const std::vector<uint32_t>& idx() const { return idx_; }
  uint64_t offset() const { return offset_; }
  CxxStaticCastKind kind() const { return kind_; }

 private:
  TypeSP type_;
  ExprResult rhs_;
  std::vector<uint32_t> idx_;
  uint64_t offset_ = 0;
  CxxStaticCastKind kind_;
  bool is_rvalue_;
};

class CxxReinterpretCastNode : public AstNode {
 public:
  CxxReinterpretCastNode(clang::SourceLocation location, TypeSP type,
                         ExprResult rhs, bool is_rvalue)
      : AstNode(location),
        type_(std::move(type)),
        rhs_(std::move(rhs)),
        is_rvalue_(is_rvalue) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return is_rvalue_; }
  TypeSP result_type() const override { return type_; }

  TypeSP type() const { return type_; }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  TypeSP type_;
  ExprResult rhs_;
  bool is_rvalue_;
};

class MemberOfNode : public AstNode {
 public:
  MemberOfNode(clang::SourceLocation location, TypeSP result_type,
               ExprResult lhs, bool is_bitfield, uint32_t bitfield_size,
               std::vector<uint32_t> member_index, bool is_arrow)
      : AstNode(location),
        result_type_(std::move(result_type)),
        lhs_(std::move(lhs)),
        is_bitfield_(is_bitfield),
        bitfield_size_(bitfield_size),
        member_index_(std::move(member_index)),
        is_arrow_(is_arrow) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return false; }
  bool is_bitfield() const override { return is_bitfield_; }
  uint32_t bitfield_size() const override { return bitfield_size_; }
  TypeSP result_type() const override { return result_type_; }

  AstNode* lhs() const { return lhs_.get(); }
  const std::vector<uint32_t>& member_index() const { return member_index_; }
  bool is_arrow() const { return is_arrow_; }

 private:
  TypeSP result_type_;
  ExprResult lhs_;
  bool is_bitfield_;
  uint32_t bitfield_size_;
  std::vector<uint32_t> member_index_;
  bool is_arrow_;
};

class ArraySubscriptNode : public AstNode {
 public:
  ArraySubscriptNode(clang::SourceLocation location, TypeSP result_type,
                     ExprResult base, ExprResult index)
      : AstNode(location),
        result_type_(std::move(result_type)),
        base_(std::move(base)),
        index_(std::move(index)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return false; }
  TypeSP result_type() const override { return result_type_; }

  AstNode* base() const { return base_.get(); }
  AstNode* index() const { return index_.get(); }

 private:
  TypeSP result_type_;
  ExprResult base_;
  ExprResult index_;
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
  BinaryOpNode(clang::SourceLocation location, TypeSP result_type,
               BinaryOpKind kind, ExprResult lhs, ExprResult rhs,
               TypeSP comp_assign_type)
      : AstNode(location),
        result_type_(std::move(result_type)),
        kind_(kind),
        lhs_(std::move(lhs)),
        rhs_(std::move(rhs)),
        comp_assign_type_(comp_assign_type) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override {
    return !binary_op_kind_is_comp_assign(kind_);
  }
  TypeSP result_type() const override { return result_type_; }

  BinaryOpKind kind() const { return kind_; }
  AstNode* lhs() const { return lhs_.get(); }
  AstNode* rhs() const { return rhs_.get(); }
  TypeSP comp_assign_type() const { return comp_assign_type_; }

 private:
  TypeSP result_type_;
  BinaryOpKind kind_;
  ExprResult lhs_;
  ExprResult rhs_;
  TypeSP comp_assign_type_;
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
  UnaryOpNode(clang::SourceLocation location, TypeSP result_type,
              UnaryOpKind kind, ExprResult rhs)
      : AstNode(location),
        result_type_(std::move(result_type)),
        kind_(kind),
        rhs_(std::move(rhs)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return kind_ != UnaryOpKind::Deref; }
  TypeSP result_type() const override { return result_type_; }

  UnaryOpKind kind() const { return kind_; }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  TypeSP result_type_;
  UnaryOpKind kind_;
  ExprResult rhs_;
};

class TernaryOpNode : public AstNode {
 public:
  TernaryOpNode(clang::SourceLocation location, TypeSP result_type,
                ExprResult cond, ExprResult lhs, ExprResult rhs)
      : AstNode(location),
        result_type_(std::move(result_type)),
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
  TypeSP result_type() const override { return result_type_; }

  AstNode* cond() const { return cond_.get(); }
  AstNode* lhs() const { return lhs_.get(); }
  AstNode* rhs() const { return rhs_.get(); }

 private:
  TypeSP result_type_;
  ExprResult cond_;
  ExprResult lhs_;
  ExprResult rhs_;
};

class SmartPtrToPtrDecay : public AstNode {
 public:
  SmartPtrToPtrDecay(clang::SourceLocation location, TypeSP result_type,
                     ExprResult ptr)
      : AstNode(location), result_type_(result_type), ptr_(std::move(ptr)) {}

  void Accept(Visitor* v) const override;
  bool is_rvalue() const override { return false; }
  TypeSP result_type() const override { return result_type_; }

  AstNode* ptr() const { return ptr_.get(); }

 private:
  TypeSP result_type_;
  ExprResult ptr_;
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
  virtual void Visit(const CxxStaticCastNode* node) = 0;
  virtual void Visit(const CxxReinterpretCastNode* node) = 0;
  virtual void Visit(const MemberOfNode* node) = 0;
  virtual void Visit(const ArraySubscriptNode* node) = 0;
  virtual void Visit(const BinaryOpNode* node) = 0;
  virtual void Visit(const UnaryOpNode* node) = 0;
  virtual void Visit(const TernaryOpNode* node) = 0;
  virtual void Visit(const SmartPtrToPtrDecay* node) = 0;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_AST_H_
