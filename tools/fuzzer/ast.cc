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

#include "tools/fuzzer/ast.h"

#include <cassert>
#include <climits>
#include <cstdint>
#include <cstdio>
#include <ios>
#include <memory>
#include <optional>
#include <sstream>
#include <type_traits>
#include <utility>
#include <variant>

namespace fuzzer {

struct BinOpInfo {
  int precedence;
  const char* symbol;
};

/**
 * Precedence and symbol for each binary operator.
 *
 * Precedence values are taken from the following:
 * https://en.cppreference.com/w/cpp/language/operator_precedence
 */
static const BinOpInfo BIN_OP_TABLE[NUM_BIN_OPS] = {
    {6, "+"},    // BinOp::Plus
    {6, "-"},    // BinOp::Minus
    {5, "*"},    // BinOp::Mult
    {5, "/"},    // BinOp::Div
    {5, "%"},    // BinOp::Mod
    {14, "&&"},  // BinOp::LogicalAnd
    {15, "||"},  // BinOp::LogicalOr
    {11, "&"},   // BinOp::BitAnd
    {13, "|"},   // BinOp::BitOr
    {12, "^"},   // BinOp::BitXor
    {7, "<<"},   // BinOp::Shl
    {7, ">>"},   // BinOp::Shr
    {10, "=="},  // BinOp::Eq
    {10, "!="},  // BinOp::Ne
    {9, "<"},    // BinOp::Lt
    {9, "<="},   // BinOp::Le
    {9, ">"},    // BinOp::Gt
    {9, ">="},   // BinOp::Ge
};

static const char* UN_OP_TABLE[NUM_UN_OPS] = {
    "+",  // UnOp::Plus
    "-",  // UnOp::Neg
    "!",  // UnOp::LogicalNot
    "~",  // UnOp::BitNot
};

static const char* SCALAR_TYPES_STRINGS[NUM_SCALAR_TYPES] = {
    "void",                // ScalarType::Void
    "bool",                // ScalarType::Bool
    "char",                // ScalarType::Char
    "signed char",         // ScalarType::SignedChar
    "unsigned char",       // ScalarType::UnsignedChar
    "short",               // ScalarType::SignedShort
    "unsigned short",      // ScalarType::UnsignedShort
    "int",                 // ScalarType::SignedInt
    "unsigned int",        // ScalarType::UnsignedInt
    "long",                // ScalarType::SignedLong
    "unsigned long",       // ScalarType::UnsignedLong
    "long long",           // ScalarType::SignedLongLong
    "unsigned long long",  // ScalarType::UnsignedLongLong
    "float",               // ScalarType::Float
    "double",              // ScalarType::Double
    "long double",         // ScalarType::LongDouble
};

std::ostream& operator<<(std::ostream& os, CvQualifiers qualifiers) {
  const char* to_print;
  bool is_const = qualifiers[CvQualifier::Const];
  bool is_volatile = qualifiers[CvQualifier::Volatile];
  if (is_const && is_volatile) {
    to_print = "const volatile";
  } else if (is_const) {
    to_print = "const";
  } else if (is_volatile) {
    to_print = "volatile";
  } else {
    return os;
  }

  return os << to_print;
}

std::ostream& operator<<(std::ostream& os, ScalarType type) {
  return os << SCALAR_TYPES_STRINGS[(size_t)type];
}

TaggedType::TaggedType(std::string name) : name_(std::move(name)) {}
const std::string& TaggedType::name() const { return name_; }
std::ostream& operator<<(std::ostream& os, const TaggedType& type) {
  return os << type.name();
}
bool TaggedType::operator==(const TaggedType& rhs) const {
  return name_ == rhs.name_;
}
bool TaggedType::operator!=(const TaggedType& rhs) const {
  return name_ != rhs.name_;
}

PointerType::PointerType(QualifiedType type) : type_(std::move(type)) {}
const QualifiedType& PointerType::type() const { return type_; }
std::ostream& operator<<(std::ostream& os, const PointerType& type) {
  return os << type.type() << "*";
}
bool PointerType::operator==(const PointerType& rhs) const {
  return type_ == rhs.type_;
}
bool PointerType::operator!=(const PointerType& rhs) const {
  return type_ != rhs.type_;
}

std::ostream& operator<<(std::ostream& os, const NullptrType&) {
  return os << "std::nullptr_t";
}
bool NullptrType::operator==(const NullptrType&) const { return true; }
bool NullptrType::operator!=(const NullptrType&) const { return false; }

EnumType::EnumType(std::string name, bool scoped)
    : name_(std::move(name)), scoped_(scoped) {}
const std::string& EnumType::name() const { return name_; }
bool EnumType::is_scoped() const { return scoped_; }
std::ostream& operator<<(std::ostream& os, const EnumType& type) {
  return os << type.name();
}
bool EnumType::operator==(const EnumType& rhs) const {
  return name_ == rhs.name_ && scoped_ == rhs.scoped_;
}
bool EnumType::operator!=(const EnumType& rhs) const {
  return name_ != rhs.name_ || scoped_ != rhs.scoped_;
}

ArrayType::ArrayType(Type type, size_t size)
    : type_(std::make_shared<Type>(std::move(type))), size_(size) {}
const Type& ArrayType::type() const { return *type_; }
size_t ArrayType::size() const { return size_; }
std::ostream& operator<<(std::ostream& os, const ArrayType& type) {
  // TODO: Fix formatting of types consisting of arrays and pointers.
  // E.g. the correct formatting of pointer to array of ints is `int (*)[N]`,
  // while the current formatting outputs `int[N]*`. Right now, this isn't
  // critical since casting to array types isn't supported yet.
  return os << type.type() << "[" << type.size() << "]";
}
bool ArrayType::operator==(const ArrayType& rhs) const {
  return size_ == rhs.size_ && *type_ == *rhs.type_;
}
bool ArrayType::operator!=(const ArrayType& rhs) const {
  return size_ != rhs.size_ || *type_ != *rhs.type_;
}

QualifiedType::QualifiedType(Type type, CvQualifiers cv_qualifiers)
    : type_(std::make_shared<Type>(std::move(type))),
      cv_qualifiers_(cv_qualifiers) {}
const Type& QualifiedType::type() const { return *type_; }
CvQualifiers QualifiedType::cv_qualifiers() const { return cv_qualifiers_; }
bool QualifiedType::operator==(const QualifiedType& rhs) const {
  return cv_qualifiers_ == rhs.cv_qualifiers_ && *type_ == *rhs.type_;
}
bool QualifiedType::operator!=(const QualifiedType& rhs) const {
  return cv_qualifiers_ != rhs.cv_qualifiers_ || *type_ != *rhs.type_;
}

std::ostream& operator<<(std::ostream& os, const QualifiedType& type) {
  const auto& inner_type = type.type();
  if (std::holds_alternative<PointerType>(inner_type)) {
    os << inner_type;
    if (type.cv_qualifiers().any()) {
      os << " " << type.cv_qualifiers();
    }
  } else {
    if (type.cv_qualifiers().any()) {
      os << type.cv_qualifiers() << " ";
    }
    os << inner_type;
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const Type& type) {
  std::visit([&os](const auto& type) { os << type; }, type);
  return os;
}

BinaryExpr::BinaryExpr(Expr lhs, BinOp op, Expr rhs)
    : lhs_(std::make_shared<Expr>(std::move(lhs))),
      rhs_(std::make_shared<Expr>(std::move(rhs))),
      op_(op) {}
BinaryExpr::BinaryExpr(Expr lhs, BinOp op, Expr rhs, Type expr_type)
    : lhs_(std::make_shared<Expr>(std::move(lhs))),
      rhs_(std::make_shared<Expr>(std::move(rhs))),
      op_(op),
      expr_type_(std::make_unique<Type>(std::move(expr_type))) {}
const Expr& BinaryExpr::lhs() const { return *lhs_; }
const Expr& BinaryExpr::rhs() const { return *rhs_; }
const Type* BinaryExpr::expr_type() const { return expr_type_.get(); }
BinOp BinaryExpr::op() const { return op_; }
int BinaryExpr::precedence() const {
  return BIN_OP_TABLE[(size_t)op_].precedence;
}
std::ostream& operator<<(std::ostream& os, const BinaryExpr& e) {
  const char* symbol = BIN_OP_TABLE[(size_t)e.op()].symbol;
  return os << e.lhs() << " " << symbol << " " << e.rhs();
}

VariableExpr::VariableExpr(std::string name) : name_(std::move(name)) {}
const std::string& VariableExpr::name() const { return name_; }
std::ostream& operator<<(std::ostream& os, const VariableExpr& e) {
  return os << e.name();
}

UnaryExpr::UnaryExpr(UnOp op, Expr expr)
    : expr_(std::make_shared<Expr>(std::move(expr))), op_(op) {}
UnOp UnaryExpr::op() const { return op_; }
const Expr& UnaryExpr::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const UnaryExpr& e) {
  os << UN_OP_TABLE[(size_t)e.op()];

  const auto* inner_as_unary = std::get_if<UnaryExpr>(&e.expr());
  if (inner_as_unary != nullptr) {
    // Avoid emitting cases such as `++3` or `--3`, print `+ +3` and `- -3`
    // instead.
    bool needs_space = (e.op() == UnOp::Plus || e.op() == UnOp::Neg) &&
                       e.op() == inner_as_unary->op();
    if (needs_space) {
      os << " ";
    }
  }
  return os << e.expr();
}

std::ostream& operator<<(std::ostream& os, const IntegerConstant& e) {
  using Base = IntegerConstant::Base;
  using Length = IntegerConstant::Length;
  using Signedness = IntegerConstant::Signedness;

  auto saved_flags = os.flags();
  switch (e.base_) {
    case Base::Bin: {
      // iostream doesn't support binary numbers yet, so we'll do it ourselves.
      std::bitset<CHAR_BIT * sizeof(e.value())> bits(e.value());
      auto str = bits.to_string();
      auto idx = str.find('1');
      // Print from the first '1' onward (or print `0` if the value is zero).
      const char* to_print = (idx != std::string::npos) ? &str[idx] : "0";

      os << "0b" << to_print;
      break;
    }

    case Base::Hex:
      os << std::hex << std::showbase << e.value();
      break;

    case Base::Oct:
      os << std::oct << std::showbase << e.value();
      break;

    case Base::Dec:
      os << std::dec << e.value();
      break;
  }

  switch (e.length_) {
    case Length::Int:
      break;

    case Length::Long:
      os << "L";
      break;

    case Length::LongLong:
      os << "LL";
      break;
  }

  switch (e.signedness_) {
    case Signedness::Signed:
      break;

    case Signedness::Unsigned:
      os << "U";
      break;
  }

  os.flags(saved_flags);
  return os;
}

std::ostream& operator<<(std::ostream& os, const DoubleConstant& e) {
  using Format = DoubleConstant::Format;
  using Length = DoubleConstant::Length;

  auto saved_flags = os.flags();
  switch (e.format_) {
    case Format::Default:
      os << std::defaultfloat << e.value_;
      break;

    case Format::Scientific:
      os << std::fixed << e.value_;
      break;

    case Format::Hex:
      os << std::hexfloat << e.value_;
      break;
  }

  switch (e.length_) {
    case Length::Float:
      os << "f";
      break;

    case Length::Double:
      break;
  }

  os.flags(saved_flags);
  return os;
}

ParenthesizedExpr::ParenthesizedExpr(Expr expr)
    : expr_(std::make_shared<Expr>(std::move(expr))) {}
const Expr& ParenthesizedExpr::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const ParenthesizedExpr& e) {
  return os << "(" << e.expr() << ")";
}

AddressOf::AddressOf(Expr expr)
    : expr_(std::make_shared<Expr>(std::move(expr))) {}
const Expr& AddressOf::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const AddressOf& e) {
  os << "&";
  if (std::holds_alternative<AddressOf>(e.expr())) {
    // Avoid accidentally printing e.g. `&&x`, print `& &x` instead.
    os << " ";
  }
  return os << e.expr();
}

MemberOf::MemberOf(Expr expr, std::string field)
    : expr_(std::make_shared<Expr>(std::move(expr))),
      field_(std::move(field)) {}
MemberOf::MemberOf(Expr expr, std::string field, TaggedType expr_type)
    : expr_(std::make_shared<Expr>(std::move(expr))),
      field_(std::move(field)),
      expr_type_(std::move(expr_type)) {}
const Expr& MemberOf::expr() const { return *expr_; }
const std::string& MemberOf::field() const { return field_; }
std::ostream& operator<<(std::ostream& os, const MemberOf& e) {
  return os << e.expr() << "." << e.field();
}

MemberOfPtr::MemberOfPtr(Expr expr, std::string field)
    : expr_(std::make_shared<Expr>(std::move(expr))),
      field_(std::move(field)) {}
MemberOfPtr::MemberOfPtr(Expr expr, std::string field, TaggedType expr_type)
    : expr_(std::make_shared<Expr>(std::move(expr))),
      field_(std::move(field)),
      expr_type_(std::move(expr_type)) {}
const Expr& MemberOfPtr::expr() const { return *expr_; }
const std::string& MemberOfPtr::field() const { return field_; }
std::ostream& operator<<(std::ostream& os, const MemberOfPtr& e) {
  return os << e.expr() << "->" << e.field();
}

ArrayIndex::ArrayIndex(Expr expr, Expr idx)
    : expr_(std::make_shared<Expr>(std::move(expr))),
      idx_(std::make_shared<Expr>(std::move(idx))) {}
const Expr& ArrayIndex::expr() const { return *expr_; }
const Expr& ArrayIndex::idx() const { return *idx_; }
std::ostream& operator<<(std::ostream& os, const ArrayIndex& e) {
  return os << e.expr() << "[" << e.idx() << "]";
}

TernaryExpr::TernaryExpr(Expr cond, Expr lhs, Expr rhs)
    : cond_(std::make_shared<Expr>(std::move(cond))),
      lhs_(std::make_shared<Expr>(std::move(lhs))),
      rhs_(std::make_shared<Expr>(std::move(rhs))) {}
TernaryExpr::TernaryExpr(Expr cond, Expr lhs, Expr rhs, Type expr_type)
    : cond_(std::make_shared<Expr>(std::move(cond))),
      lhs_(std::make_shared<Expr>(std::move(lhs))),
      rhs_(std::make_shared<Expr>(std::move(rhs))),
      expr_type_(std::make_shared<Type>(std::move(expr_type))) {}
const Expr& TernaryExpr::cond() const { return *cond_; }
const Expr& TernaryExpr::lhs() const { return *lhs_; }
const Expr& TernaryExpr::rhs() const { return *rhs_; }
const Type* TernaryExpr::expr_type() const { return expr_type_.get(); }
std::ostream& operator<<(std::ostream& os, const TernaryExpr& e) {
  return os << e.cond() << " ? " << e.lhs() << " : " << e.rhs();
}

CastExpr::CastExpr(Kind kind, Type type, Expr expr)
    : kind_(kind),
      type_(std::move(type)),
      expr_(std::make_shared<Expr>(std::move(expr))) {}
CastExpr::Kind CastExpr::kind() const { return kind_; }
const Type& CastExpr::type() const { return type_; }
const Expr& CastExpr::expr() const { return *expr_; }
int CastExpr::precedence() const { return cast_kind_precedence(kind_); }
std::ostream& operator<<(std::ostream& os, const CastExpr& e) {
  using Kind = CastExpr::Kind;
  switch (e.kind()) {
    case Kind::CStyleCast:
      return os << "(" << e.type() << ") " << e.expr();
    case Kind::StaticCast:
      return os << "static_cast<" << e.type() << ">(" << e.expr() << ")";
    case Kind::ReinterpretCast:
      return os << "reinterpret_cast<" << e.type() << ">(" << e.expr() << ")";

    default:
      assert(false && "Did you introduce a new cast kind?");
  }
  return os;
}

DereferenceExpr::DereferenceExpr(Expr expr)
    : expr_(std::make_shared<Expr>(std::move(expr))) {}
const Expr& DereferenceExpr::expr() const { return *expr_; }
std::ostream& operator<<(std::ostream& os, const DereferenceExpr& expr) {
  return os << "*" << expr.expr();
}

FunctionCallExpr::FunctionCallExpr(std::string name,
                                   std::vector<std::shared_ptr<Expr>> args)
    : name_(std::move(name)), args_(std::move(args)) {}
const std::string& FunctionCallExpr::name() const { return name_; }
const std::vector<std::shared_ptr<Expr>>& FunctionCallExpr::args() const {
  return args_;
}
std::ostream& operator<<(std::ostream& os, const FunctionCallExpr& expr) {
  os << expr.name() << "(";
  const auto& args = expr.args();
  for (size_t i = 0; i < args.size(); ++i) {
    if (i > 0) {
      os << ", ";
    }
    os << *args[i];
  }
  return os << ")";
}

SizeofExpr::SizeofExpr(Expr expr)
    : arg_(std::make_unique<Expr>(std::move(expr))) {}
SizeofExpr::SizeofExpr(Type type) : arg_(std::move(type)) {}
std::optional<std::reference_wrapper<const Expr>> SizeofExpr::maybe_expr()
    const {
  const auto* as_expr = std::get_if<std::shared_ptr<Expr>>(&arg_);
  if (as_expr != nullptr) {
    return **as_expr;
  }
  return {};
}
std::optional<std::reference_wrapper<const Type>> SizeofExpr::maybe_type()
    const {
  const auto* as_type = std::get_if<Type>(&arg_);
  if (as_type != nullptr) {
    return *as_type;
  }
  return {};
}
std::ostream& operator<<(std::ostream& os, const SizeofExpr& expr) {
  os << "sizeof";
  auto maybe_expr = expr.maybe_expr();
  if (maybe_expr.has_value()) {
    const Expr& child = maybe_expr.value();
    // If the child isn't a parenthesized expression, separate the expression
    // and 'sizeof' with a space.
    if (!std::holds_alternative<ParenthesizedExpr>(child)) {
      os << " ";
    }
    return os << child;
  }
  auto maybe_type = expr.maybe_type();
  if (maybe_type.has_value()) {
    return os << "(" << maybe_type.value() << ")";
  }
  assert(false && "Did you introduce a new alternative?");
  return os;
}

std::ostream& operator<<(std::ostream& os, const BooleanConstant& expr) {
  const char* to_print = expr.value() ? "true" : "false";
  return os << to_print;
}

std::ostream& operator<<(std::ostream& os, const NullptrConstant&) {
  return os << "nullptr";
}

std::ostream& operator<<(std::ostream& os, const EnumConstant& expr) {
  // TODO: Support unscoped enum literals. Currently, unscoped enums aren't
  // supported well by LLDB.
  return os << expr.type() << "::" << expr.literal();
}

std::ostream& operator<<(std::ostream& os, const Expr& e) {
  std::visit([&os](const auto& expr) { os << expr; }, e);
  return os;
}

/**
 * A visitor that dumps an expression to `stdout` for debugging purposes.
 */
class ExprDumper {
 public:
  void operator()(const BinaryExpr& e) {
    emit_marked_indentation();

    const auto* symbol = BIN_OP_TABLE[(size_t)e.op()].symbol;
    printf("Binary expression of type `%s`:\n", symbol);

    emit_indentation();
    printf("Left-hand side:\n");
    indented_visit(e.lhs());

    emit_indentation();
    printf("Right-hand side:\n");
    indented_visit(e.rhs());
  }

  void operator()(const VariableExpr& e) {
    emit_marked_indentation();
    printf("Variable expression for identifier `%s`\n", e.name().c_str());
  }

  void operator()(const IntegerConstant& e) {
    emit_marked_indentation();
    printf("Integer constant with value `%" PRIu64 "`\n", e.value());
  }

  void operator()(const DoubleConstant& e) {
    emit_marked_indentation();
    printf("Double constant with value `%f`\n", e.value());
  }

  void operator()(const NullptrConstant&) {
    emit_marked_indentation();
    printf("Pointer constant: `nullptr`\n");
  }

  void operator()(const EnumConstant& e) {
    emit_marked_indentation();
    printf("Enum constant: `%s`\n", e.literal().c_str());
  }

  void operator()(const UnaryExpr& e) {
    emit_marked_indentation();
    const auto* symbol = UN_OP_TABLE[(size_t)e.op()];
    printf("Unary expression of type %s\n", symbol);

    indented_visit(e.expr());
  }

  void operator()(const ParenthesizedExpr& e) {
    emit_marked_indentation();
    printf("Parenthesized expression:\n");

    indented_visit(e.expr());
  }

  void operator()(const AddressOf& e) {
    emit_marked_indentation();
    printf("Address of:\n");

    indented_visit(e.expr());
  }

  void operator()(const MemberOf& e) {
    emit_marked_indentation();
    printf("Field access on `%s`:\n", e.field().c_str());

    indented_visit(e.expr());
  }

  void operator()(const MemberOfPtr& e) {
    emit_marked_indentation();
    printf("Pointer field access on `%s`:\n", e.field().c_str());

    indented_visit(e.expr());
  }

  void operator()(const ArrayIndex& e) {
    emit_marked_indentation();
    printf("Array index:\n");

    emit_indentation();
    printf("Array:\n");
    indented_visit(e.expr());

    emit_indentation();
    printf("Index:\n");
    indented_visit(e.idx());
  }

  void operator()(const TernaryExpr& e) {
    emit_marked_indentation();
    printf("Ternary expression:\n");

    emit_indentation();
    printf("Condition:\n");
    indented_visit(e.cond());

    emit_indentation();
    printf("Left-hand side:\n");
    indented_visit(e.lhs());

    emit_indentation();
    printf("Right-hand side:\n");
    indented_visit(e.rhs());
  }

  void operator()(const CastExpr& e) {
    emit_marked_indentation();
    std::ostringstream os;
    os << e.type();
    printf("Cast expression into type: `%s`\n", os.str().c_str());

    indented_visit(e.expr());
  }

  void operator()(const DereferenceExpr& e) {
    emit_marked_indentation();
    printf("Dereference:\n");
    indented_visit(e.expr());
  }

  void operator()(const FunctionCallExpr& e) {
    emit_marked_indentation();
    printf("Function call:\n");

    emit_indentation();
    printf("Name: `%s`\n", e.name().c_str());

    const auto& args = e.args();
    for (size_t i = 0; i < args.size(); ++i) {
      emit_indentation();
      printf("Argument #%zu:", i + 1);
      indented_visit(*args[i]);
    }
  }

  void operator()(const SizeofExpr& e) {
    emit_marked_indentation();
    printf("Sizeof:");

    auto maybe_type = e.maybe_type();
    if (maybe_type.has_value()) {
      std::ostringstream os;
      os << maybe_type.value();
      printf("  Type: %s\n", os.str().c_str());
      return;
    }

    auto maybe_expr = e.maybe_expr();
    if (maybe_expr.has_value()) {
      indented_visit(maybe_expr.value());
    }
  }

  void operator()(const BooleanConstant& e) {
    emit_marked_indentation();

    const char* to_print = e.value() ? "true" : "false";
    printf("Boolean constant of value `%s`\n", to_print);
  }

 private:
  static constexpr int SPACES_PER_INDENTATION = 2;

  void emit_marked_indentation() { printf("%*s%s", indent_spaces_, "", "+ "); }
  void emit_indentation() { printf("%*s%s", indent_spaces_, "", "  "); }

  void indented_visit(const Expr& e) {
    indent_spaces_ += SPACES_PER_INDENTATION;
    std::visit(*this, e);
    indent_spaces_ -= SPACES_PER_INDENTATION;
  }

 private:
  int indent_spaces_ = 0;
};

void dump_expr(const Expr& expr) { std::visit(ExprDumper(), expr); }

int bin_op_precedence(BinOp op) { return BIN_OP_TABLE[(size_t)op].precedence; }

int cast_kind_precedence(CastExpr::Kind kind) {
  switch (kind) {
    case CastExpr::Kind::CStyleCast:
      return 3;
    case CastExpr::Kind::StaticCast:
    case CastExpr::Kind::ReinterpretCast:
      return 2;

    default:
      assert(false && "Did you introduce a new cast kind?");
  }
  return 0;  // all control paths must return a value
}

}  // namespace fuzzer

static inline size_t hash_combine_impl(size_t acc) { return acc; }

template <typename T, typename... Rest>
static inline size_t hash_combine_impl(size_t acc, T&& v, Rest&&... rest) {
  // std::hash has specializations for e.g. `std::string`, but not for `const
  // std::string&`, so remove any cv-qualified reference from type `T`.
  using Type = std::remove_cv_t<std::remove_reference_t<T>>;
  std::hash<Type> hasher;
  // Disclaimer: Hash combining algorithm is taken from `boost::hash_combine`,
  // no idea how it fares in practice.
  acc ^= hasher(std::forward<T>(v)) + 0x9e3779b9u + (acc << 6) + (acc >> 2);
  return hash_combine_impl(acc, std::forward<Rest>(rest)...);
}

/*
 * Combines multiple hash values together. This is equivalent to
 * `boost::hash_combine`, albeit with support for perfect forwarding (so that
 * invocation of `hash_combine` can be conveniently a one-liner).
 */
template <typename... Args>
static inline size_t hash_combine(Args&&... args) {
  return hash_combine_impl(0, std::forward<Args>(args)...);
}

enum class HashingTypeKind {
  PointerType,
  QualifiedType,
  TaggedType,
  NullptrType,
  EnumType,
  ArrayType,
};

namespace std {

using fuzzer::ArrayType;
using fuzzer::EnumType;
using fuzzer::NullptrType;
using fuzzer::PointerType;
using fuzzer::QualifiedType;
using fuzzer::TaggedType;

size_t hash<PointerType>::operator()(const PointerType& type) const {
  return hash_combine(HashingTypeKind::PointerType, type.type());
}

size_t hash<QualifiedType>::operator()(const QualifiedType& type) const {
  return hash_combine(HashingTypeKind::QualifiedType, type.cv_qualifiers(),
                      type.type());
}

size_t hash<TaggedType>::operator()(const TaggedType& type) const {
  return hash_combine(HashingTypeKind::TaggedType, type.name());
}

size_t hash<NullptrType>::operator()(const NullptrType&) const {
  return hash_combine(HashingTypeKind::NullptrType);
}

size_t hash<EnumType>::operator()(const EnumType& type) const {
  return hash_combine(HashingTypeKind::EnumType, type.name(), type.is_scoped());
}

size_t hash<ArrayType>::operator()(const ArrayType& type) const {
  return hash_combine(HashingTypeKind::ArrayType, type.type(), type.size());
}

}  // namespace std
