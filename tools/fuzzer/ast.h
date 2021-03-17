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

#ifndef INCLUDE_AST_H
#define INCLUDE_AST_H

#include <bitset>
#include <cinttypes>
#include <iosfwd>
#include <memory>
#include <string>
#include <typeindex>  // forward references `std::hash`
#include <variant>
#include <vector>

#include "tools/fuzzer/enum_bitset.h"

namespace fuzzer {

enum class ScalarType : unsigned char;
class TaggedType;
class PointerType;
class NullptrType;
class EnumType;
class ArrayType;

using Type = std::variant<ScalarType, TaggedType, PointerType, NullptrType,
                          EnumType, ArrayType>;
std::ostream& operator<<(std::ostream& os, const Type& type);

enum class CvQualifier : unsigned char {
  EnumFirst,
  Const = EnumFirst,
  Volatile,
  EnumLast = Volatile,
};
using CvQualifiers = EnumBitset<CvQualifier>;

std::ostream& operator<<(std::ostream& os, CvQualifiers qualifiers);

enum class ScalarType : unsigned char {
  EnumFirst,
  Void = EnumFirst,
  Bool,
  // Have `char` explicitly because it is implementation dependent whether
  // `char` maps to `signed char` or `unsigned char`.
  Char,
  SignedChar,
  UnsignedChar,
  SignedShort,
  UnsignedShort,
  SignedInt,
  UnsignedInt,
  SignedLong,
  UnsignedLong,
  SignedLongLong,
  UnsignedLongLong,
  Float,
  Double,
  LongDouble,
  EnumLast = LongDouble,
};
inline constexpr size_t NUM_SCALAR_TYPES = (size_t)ScalarType::EnumLast + 1;
std::ostream& operator<<(std::ostream& os, ScalarType type);

inline constexpr bool is_int_scalar_type(ScalarType type) {
  return ScalarType::Bool <= type && type <= ScalarType::UnsignedLongLong;
}
inline constexpr bool is_float_scalar_type(ScalarType type) {
  return ScalarType::Float <= type && type <= ScalarType::LongDouble;
}

class TaggedType {
 public:
  TaggedType() = default;
  explicit TaggedType(std::string name);

  const std::string& name() const;

  friend std::ostream& operator<<(std::ostream& os, const TaggedType& type);
  bool operator==(const TaggedType& rhs) const;
  bool operator!=(const TaggedType& rhs) const;

 private:
  std::string name_;
};

class QualifiedType {
 public:
  QualifiedType() = default;
  explicit QualifiedType(Type type,
                         CvQualifiers cv_qualifiers = CvQualifiers());

  const Type& type() const;
  CvQualifiers cv_qualifiers() const;

  friend std::ostream& operator<<(std::ostream& os, const QualifiedType& type);
  bool operator==(const QualifiedType& type) const;
  bool operator!=(const QualifiedType& type) const;

 private:
  std::shared_ptr<Type> type_;
  CvQualifiers cv_qualifiers_;
};

class PointerType {
 public:
  PointerType() = default;
  explicit PointerType(QualifiedType type);

  const QualifiedType& type() const;

  friend std::ostream& operator<<(std::ostream& os, const PointerType& type);
  bool operator==(const PointerType& type) const;
  bool operator!=(const PointerType& type) const;

 private:
  QualifiedType type_;
};

class NullptrType {
 public:
  NullptrType() = default;

  friend std::ostream& operator<<(std::ostream& os, const QualifiedType& type);
  bool operator==(const NullptrType& type) const;
  bool operator!=(const NullptrType& type) const;
};

class EnumType {
 public:
  EnumType() = default;
  EnumType(std::string name, bool scoped);

  const std::string& name() const;

  bool is_scoped() const;

  friend std::ostream& operator<<(std::ostream& os, const EnumType& type);
  bool operator==(const EnumType& type) const;
  bool operator!=(const EnumType& type) const;

 private:
  std::string name_;
  bool scoped_;
};

class ArrayType {
 public:
  ArrayType() = default;
  ArrayType(Type type, size_t size);

  const Type& type() const;
  size_t size() const;

  friend std::ostream& operator<<(std::ostream& os, const ArrayType& type);
  bool operator==(const ArrayType& type) const;
  bool operator!=(const ArrayType& type) const;

 private:
  std::shared_ptr<Type> type_;
  size_t size_;
};

class BinaryExpr;
class UnaryExpr;
class VariableExpr;
class IntegerConstant;
class DoubleConstant;
class ParenthesizedExpr;
class AddressOf;
class MemberOf;
class MemberOfPtr;
class ArrayIndex;
class TernaryExpr;
class CastExpr;
class DereferenceExpr;
class FunctionCallExpr;
class BooleanConstant;
class NullptrConstant;
class EnumConstant;

enum class UnOp : unsigned char {
  // Used to determine the first enum element.
  EnumFirst,
  Plus = EnumFirst,
  Neg,
  LogicalNot,
  BitNot,
  // Used to determine the last enum element.
  EnumLast = BitNot,
};
inline constexpr size_t NUM_UN_OPS = (size_t)UnOp::EnumLast + 1;

enum class BinOp : unsigned char {
  // Used to determine the first enum element.
  EnumFirst,
  // Arithmetic operators.
  Plus = EnumFirst,
  Minus,
  Mult,
  Div,
  Mod,
  // Logical operators.
  LogicalAnd,
  LogicalOr,
  // Bitwise operators.
  BitAnd,
  BitOr,
  BitXor,
  Shl,
  Shr,
  // Comparison operators.
  Eq,
  Ne,
  Lt,
  Le,
  Gt,
  Ge,
  // Used to determine the last enum element.
  EnumLast = Ge,
};
inline constexpr size_t NUM_BIN_OPS = (size_t)BinOp::EnumLast + 1;
int bin_op_precedence(BinOp op);

using Expr = std::variant<IntegerConstant, DoubleConstant, VariableExpr,
                          UnaryExpr, BinaryExpr, AddressOf, MemberOf,
                          MemberOfPtr, ArrayIndex, TernaryExpr, CastExpr,
                          DereferenceExpr, FunctionCallExpr, BooleanConstant,
                          NullptrConstant, EnumConstant, ParenthesizedExpr>;
inline constexpr size_t NUM_EXPR_KINDS = std::variant_size_v<Expr>;
void dump_expr(const Expr& expr);
std::ostream& operator<<(std::ostream& os, const Expr& expr);

class BinaryExpr {
 public:
  BinaryExpr() = default;
  BinaryExpr(Expr lhs, BinOp op, Expr rhs);
  BinaryExpr(Expr lhs, BinOp op, Expr rhs, Type expr_type);

  const Expr& lhs() const;
  const Expr& rhs() const;
  BinOp op() const;
  int precedence() const;
  const Type* expr_type() const;

  friend std::ostream& operator<<(std::ostream& os, const BinaryExpr& expr);

 private:
  std::shared_ptr<Expr> lhs_;
  std::shared_ptr<Expr> rhs_;
  BinOp op_ = BinOp::Plus;  // Just pick one for the default ctor
  std::shared_ptr<Type> expr_type_;
};

class UnaryExpr {
 public:
  static constexpr int PRECEDENCE = 3;

  UnaryExpr() = default;
  UnaryExpr(UnOp op, Expr expr);

  UnOp op() const;
  const Expr& expr() const;
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os, const UnaryExpr& expr);

 private:
  std::shared_ptr<Expr> expr_;
  UnOp op_ = UnOp::Plus;  // Just pick one for the default ctor
};

class VariableExpr {
 public:
  static constexpr int PRECEDENCE = 0;

  VariableExpr() = default;
  explicit VariableExpr(std::string name);

  const std::string& name() const;
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os, const VariableExpr& expr);

 private:
  std::string name_;
};

class IntegerConstant {
 public:
  enum class Base : unsigned char {
    EnumFirst,
    Dec = EnumFirst,
    Hex,
    Oct,
    Bin,
    EnumLast = Bin,
  };

  enum class Length {
    EnumFirst,
    Int = EnumFirst,
    Long,
    LongLong,
    EnumLast = LongLong,
  };
  enum class Signedness {
    EnumFirst,
    Signed = EnumFirst,
    Unsigned,
    EnumLast = Unsigned,
  };

  static constexpr int PRECEDENCE = 0;

  IntegerConstant() = default;
  explicit IntegerConstant(uint64_t value) : value_(value) {}
  IntegerConstant(uint64_t value, Base base, Length length,
                  Signedness signedness)
      : value_(value), base_(base), length_(length), signedness_(signedness) {}

  uint64_t value() const { return value_; }
  Base base() const { return base_; }
  Length length() const { return length_; }
  Signedness signedness() const { return signedness_; }
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os,
                                  const IntegerConstant& expr);

 private:
  uint64_t value_ = 0;
  Base base_ = Base::Dec;
  Length length_ = Length::Int;
  Signedness signedness_ = Signedness::Signed;
};

class DoubleConstant {
 public:
  enum class Format : unsigned char {
    EnumFirst,
    Default = EnumFirst,
    Scientific,
    Hex,
    EnumLast = Hex,
  };

  // TODO(alextasos): Add long doubles when lldb-eval adds support for them
  enum class Length : unsigned char {
    EnumFirst,
    Float = EnumFirst,
    Double,
    EnumLast = Double,
  };

  static constexpr int PRECEDENCE = 0;

  DoubleConstant() = default;
  explicit DoubleConstant(double value) : value_(value) {}
  DoubleConstant(double value, Format format, Length length)
      : value_(value), format_(format), length_(length) {}

  double value() const { return value_; }
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os, const DoubleConstant& expr);

 private:
  double value_ = 0;
  Format format_ = Format::Default;
  Length length_ = Length::Double;
};

class ParenthesizedExpr {
 public:
  static constexpr int PRECEDENCE = 0;

  ParenthesizedExpr() = default;
  explicit ParenthesizedExpr(Expr expr);

  const Expr& expr() const;
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os,
                                  const ParenthesizedExpr& expr);

 private:
  std::shared_ptr<Expr> expr_;
};

class AddressOf {
 public:
  static constexpr int PRECEDENCE = 3;

  AddressOf() = default;
  explicit AddressOf(Expr expr);

  const Expr& expr() const;
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os, const AddressOf& expr);

 private:
  std::shared_ptr<Expr> expr_;
};

class MemberOf {
 public:
  static constexpr int PRECEDENCE = 2;

  MemberOf() = default;
  MemberOf(Expr expr, std::string field);

  // Useful only for unit testing
  MemberOf(Expr expr, std::string field, TaggedType expr_type);

  const Expr& expr() const;
  const std::string& field() const;
  int precedence() const { return PRECEDENCE; }

  const TaggedType& expr_type() const { return expr_type_; }

  friend std::ostream& operator<<(std::ostream& os, const MemberOf& expr);

 private:
  std::shared_ptr<Expr> expr_;
  std::string field_;

  TaggedType expr_type_;
};

class MemberOfPtr {
 public:
  static constexpr int PRECEDENCE = 2;

  MemberOfPtr() = default;
  MemberOfPtr(Expr expr, std::string field);

  // Useful only for unit testing
  MemberOfPtr(Expr expr, std::string field, TaggedType expr_type);

  const Expr& expr() const;
  const std::string& field() const;
  int precedence() const { return PRECEDENCE; }

  const TaggedType& expr_type() const { return expr_type_; }

  friend std::ostream& operator<<(std::ostream& os, const MemberOfPtr& expr);

 private:
  std::shared_ptr<Expr> expr_;
  std::string field_;

  TaggedType expr_type_;
};

class ArrayIndex {
 public:
  static constexpr int PRECEDENCE = 2;

  ArrayIndex() = default;
  ArrayIndex(Expr expr, Expr idx);

  const Expr& expr() const;
  const Expr& idx() const;
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os, const ArrayIndex& expr);

 private:
  std::shared_ptr<Expr> expr_;
  std::shared_ptr<Expr> idx_;
};

class TernaryExpr {
 public:
  static constexpr int PRECEDENCE = 16;

  TernaryExpr() = default;
  TernaryExpr(Expr cond, Expr lhs, Expr rhs);
  TernaryExpr(Expr cond, Expr lhs, Expr rhs, Type expr_type);

  const Expr& cond() const;
  const Expr& lhs() const;
  const Expr& rhs() const;
  int precedence() const { return PRECEDENCE; }
  const Type* expr_type() const;

  friend std::ostream& operator<<(std::ostream& os, const TernaryExpr& expr);

 private:
  std::shared_ptr<Expr> cond_;
  std::shared_ptr<Expr> lhs_;
  std::shared_ptr<Expr> rhs_;

  std::shared_ptr<Type> expr_type_;
};

class CastExpr {
 public:
  enum class Kind {
    EnumFirst,
    CStyleCast = EnumFirst,
    StaticCast,
    ReinterpretCast,
    EnumLast = ReinterpretCast,
  };

  CastExpr() = default;
  CastExpr(Kind kind, Type type, Expr expr);

  Kind kind() const;
  const Type& type() const;
  const Expr& expr() const;
  int precedence() const;

  friend std::ostream& operator<<(std::ostream& os, const CastExpr& expr);

 private:
  Kind kind_;
  Type type_;
  std::shared_ptr<Expr> expr_;
};

int cast_kind_precedence(CastExpr::Kind kind);

class DereferenceExpr {
 public:
  static constexpr int PRECEDENCE = 3;

  DereferenceExpr() = default;
  explicit DereferenceExpr(Expr expr);

  const Expr& expr() const;
  int precedence() const { return PRECEDENCE; }

  friend std::ostream& operator<<(std::ostream& os,
                                  const DereferenceExpr& expr);

 private:
  std::shared_ptr<Expr> expr_;
};

class FunctionCallExpr {
 public:
  static constexpr int PRECEDENCE = 2;

  FunctionCallExpr() = default;
  FunctionCallExpr(std::string name, std::vector<std::shared_ptr<Expr>> args);

  friend std::ostream& operator<<(std::ostream& os,
                                  const FunctionCallExpr& expr);

  const std::string& name() const;
  const std::vector<std::shared_ptr<Expr>>& args() const;
  int precedence() const { return PRECEDENCE; }

 private:
  std::string name_;
  std::vector<std::shared_ptr<Expr>> args_;
};

class BooleanConstant {
 public:
  static constexpr int PRECEDENCE = 0;

  BooleanConstant() = default;
  explicit BooleanConstant(bool value) : value_(value) {}

  friend std::ostream& operator<<(std::ostream& os,
                                  const BooleanConstant& expr);

  bool value() const { return value_; }
  int precedence() const { return PRECEDENCE; }

 private:
  bool value_ = false;
};

class NullptrConstant {
 public:
  static constexpr int PRECEDENCE = 0;

  NullptrConstant() = default;

  friend std::ostream& operator<<(std::ostream& os,
                                  const NullptrConstant& expr);

  int precedence() const { return PRECEDENCE; }
};

class EnumConstant {
 public:
  static constexpr int PRECEDENCE = 0;

  EnumConstant() = default;
  EnumConstant(EnumType type, std::string literal)
      : type_(std::move(type)), literal_(std::move(literal)) {}

  friend std::ostream& operator<<(std::ostream& os, const EnumConstant& expr);

  const EnumType& type() const { return type_; }
  const std::string& literal() const { return literal_; }
  int precedence() const { return PRECEDENCE; }

 private:
  EnumType type_;
  std::string literal_;
};

}  // namespace fuzzer

// Forward declarations of hash specializations
namespace std {

template <>
struct hash<fuzzer::PointerType> {
  size_t operator()(const fuzzer::PointerType& type) const;
};

template <>
struct hash<fuzzer::QualifiedType> {
  size_t operator()(const fuzzer::QualifiedType& type) const;
};

template <>
struct hash<fuzzer::TaggedType> {
  size_t operator()(const fuzzer::TaggedType& type) const;
};

template <>
struct hash<fuzzer::NullptrType> {
  size_t operator()(const fuzzer::NullptrType& type) const;
};

template <>
struct hash<fuzzer::EnumType> {
  size_t operator()(const fuzzer::EnumType& type) const;
};

template <>
struct hash<fuzzer::ArrayType> {
  size_t operator()(const fuzzer::ArrayType& type) const;
};

}  // namespace std

#endif  // INCLUDE_AST_H
