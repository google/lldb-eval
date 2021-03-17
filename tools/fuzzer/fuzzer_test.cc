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

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <functional>
#include <sstream>
#include <type_traits>
#include <unordered_set>
#include <utility>
#include <vector>

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/expr_gen.h"

using namespace fuzzer;
using namespace testing;

namespace fuzzer {

void PrintTo(const Expr& expr, std::ostream* os) { *os << "`" << expr << "`"; }

std::ostream& operator<<(std::ostream& os, CastExpr::Kind cast_kind) {
  switch (cast_kind) {
    case CastExpr::Kind::CStyleCast:
      return os << "CStyleCast";
    case CastExpr::Kind::StaticCast:
      return os << "StaticCast";
    case CastExpr::Kind::ReinterpretCast:
      return os << "ReinterpretCast";
  }
  return os;
}

}  // namespace fuzzer

class FakeGeneratorRng : public GeneratorRng {
 public:
  FakeGeneratorRng() {}

  BinOp gen_bin_op(BinOpMask) override {
    assert(!bin_ops_.empty());
    BinOp op = bin_ops_.back();
    bin_ops_.pop_back();

    return op;
  }

  UnOp gen_un_op(UnOpMask) override {
    assert(!un_ops_.empty());
    UnOp op = un_ops_.back();
    un_ops_.pop_back();

    return op;
  }

  ExprKind gen_expr_kind(const Weights&, const ExprKindMask&) override {
    assert(!expr_kinds_.empty());
    ExprKind kind = expr_kinds_.back();
    expr_kinds_.pop_back();

    return kind;
  }

  TypeKind gen_type_kind(const Weights&, const TypeKindMask&) override {
    assert(!type_kinds_.empty());
    TypeKind kind = type_kinds_.back();
    type_kinds_.pop_back();

    return kind;
  }

  CastExpr::Kind gen_cast_kind(const CastKindMask&) override {
    assert(!cast_kinds_.empty());
    CastExpr::Kind kind = cast_kinds_.back();
    cast_kinds_.pop_back();

    return kind;
  }

  ScalarType gen_scalar_type(EnumBitset<ScalarType>) override {
    assert(!scalar_types_.empty());
    ScalarType type = scalar_types_.back();
    scalar_types_.pop_back();

    return type;
  }

  bool gen_boolean() override {
    assert(!bools_.empty());
    bool constant = bools_.back();
    bools_.pop_back();

    return constant;
  }

  IntegerConstant gen_integer_constant(uint64_t, uint64_t) override {
    assert(!int_constants_.empty());
    IntegerConstant constant = int_constants_.back();
    int_constants_.pop_back();

    return constant;
  }

  DoubleConstant gen_double_constant(double, double) override {
    assert(!double_constants_.empty());
    DoubleConstant constant = double_constants_.back();
    double_constants_.pop_back();

    return constant;
  }

  VariableExpr pick_variable(
      const std::vector<std::reference_wrapper<const VariableExpr>>&) override {
    assert(!vars_.empty());
    VariableExpr var = vars_.back();
    vars_.pop_back();

    return var;
  }

  EnumConstant pick_enum_literal(
      const std::vector<std::reference_wrapper<const EnumConstant>>&) override {
    assert(!enums_.empty());
    EnumConstant e = enums_.back();
    enums_.pop_back();

    return e;
  }

  fuzzer::Field pick_field(
      const std::vector<std::reference_wrapper<const fuzzer::Field>>&)
      override {
    assert(!fields_.empty());
    std::string field_name = fields_.back();

    fields_.pop_back();

    TaggedType type = pick_tagged_type(
        std::vector<std::reference_wrapper<const TaggedType>>());

    return fuzzer::Field(std::move(type), std::move(field_name));
  }

  fuzzer::Function pick_function(
      const std::vector<std::reference_wrapper<const fuzzer::Function>>&)
      override {
    assert(false && "Not implemented yet!");
    return fuzzer::Function("dummy", {});
  }

  TaggedType pick_tagged_type(
      const std::vector<std::reference_wrapper<const TaggedType>>&) override {
    assert(!tagged_types_.empty());
    TaggedType type = tagged_types_.back();
    tagged_types_.pop_back();

    return type;
  }

  EnumType pick_enum_type(
      const std::vector<std::reference_wrapper<const EnumType>>&) override {
    assert(!enum_types_.empty());
    EnumType type = enum_types_.back();
    enum_types_.pop_back();

    return type;
  }

  ArrayType pick_array_type(
      const std::vector<std::reference_wrapper<const ArrayType>>&) override {
    assert(false && "Not implemented yet!");
    return ArrayType(ScalarType::SignedInt, 2);
  }

  bool gen_binop_ptr_expr(float) override {
    bool gen = false;
    if (!gen_binop_ptr_expr_.empty()) {
      gen = gen_binop_ptr_expr_.back();
      gen_binop_ptr_expr_.pop_back();
    }

    return gen;
  }

  bool gen_binop_flip_operands(float) override {
    bool flip = false;
    if (!flip_operands_.empty()) {
      flip = flip_operands_.back();
      flip_operands_.pop_back();
    }

    return flip;
  }

  bool gen_binop_ptrdiff_expr(float) override {
    bool gen = false;
    if (!gen_binop_ptrdiff_expr_.empty()) {
      gen = gen_binop_ptrdiff_expr_.back();
      gen_binop_ptrdiff_expr_.pop_back();
    }

    return gen;
  }

  bool gen_binop_ptr_or_enum(float) override {
    bool gen = false;
    if (!gen_binop_ptr_or_enum_.empty()) {
      gen = gen_binop_ptr_or_enum_.back();
      gen_binop_ptr_or_enum_.pop_back();
    }

    return gen;
  }

  CvQualifiers gen_cv_qualifiers(float, float) override {
    assert(!cv_qualifiers_.empty());
    CvQualifiers cv = cv_qualifiers_.back();
    cv_qualifiers_.pop_back();

    return cv;
  }

  bool gen_parenthesize(float) override { return false; }

  static FakeGeneratorRng from_expr(
      const Expr& expr, const std::vector<bool>& flip_operands,
      const std::vector<bool>& gen_binop_ptr_expr,
      const std::vector<bool>& gen_binop_ptrdiff_expr,
      const std::vector<bool>& gen_binop_ptr_or_enum) {
    FakeGeneratorRng rng;

    std::visit(rng, expr);

    std::reverse(rng.un_ops_.begin(), rng.un_ops_.end());
    std::reverse(rng.bin_ops_.begin(), rng.bin_ops_.end());
    std::reverse(rng.int_constants_.begin(), rng.int_constants_.end());
    std::reverse(rng.double_constants_.begin(), rng.double_constants_.end());
    std::reverse(rng.bools_.begin(), rng.bools_.end());
    std::reverse(rng.expr_kinds_.begin(), rng.expr_kinds_.end());
    std::reverse(rng.cast_kinds_.begin(), rng.cast_kinds_.end());

    std::reverse(rng.type_kinds_.begin(), rng.type_kinds_.end());
    std::reverse(rng.cv_qualifiers_.begin(), rng.cv_qualifiers_.end());
    std::reverse(rng.scalar_types_.begin(), rng.scalar_types_.end());
    std::reverse(rng.tagged_types_.begin(), rng.tagged_types_.end());
    std::reverse(rng.enum_types_.begin(), rng.enum_types_.end());

    std::reverse(rng.vars_.begin(), rng.vars_.end());

    rng.flip_operands_.assign(flip_operands.rbegin(), flip_operands.rend());
    rng.gen_binop_ptr_expr_.assign(gen_binop_ptr_expr.rbegin(),
                                   gen_binop_ptr_expr.rend());
    rng.gen_binop_ptrdiff_expr_.assign(gen_binop_ptrdiff_expr.rbegin(),
                                       gen_binop_ptrdiff_expr.rend());
    rng.gen_binop_ptr_or_enum_.assign(gen_binop_ptr_or_enum.rbegin(),
                                      gen_binop_ptr_or_enum.rend());

    return rng;
  }

  void operator()(const UnaryExpr& e) {
    expr_kinds_.push_back(ExprKind::UnaryExpr);
    un_ops_.push_back(e.op());
    std::visit(*this, e.expr());
  }

  void operator()(const BinaryExpr& e) {
    expr_kinds_.push_back(ExprKind::BinaryExpr);

    bin_ops_.push_back(e.op());

    const auto* unit_test_type = e.expr_type();
    if (unit_test_type != nullptr) {
      std::visit(*this, *unit_test_type);
    }

    std::visit(*this, e.lhs());
    std::visit(*this, e.rhs());
  }

  void operator()(const VariableExpr& e) {
    expr_kinds_.push_back(ExprKind::VariableExpr);
    vars_.push_back(e);
  }

  void operator()(const IntegerConstant& e) {
    expr_kinds_.push_back(ExprKind::IntegerConstant);
    int_constants_.push_back(e);
  }

  void operator()(const DoubleConstant& e) {
    expr_kinds_.push_back(ExprKind::DoubleConstant);
    double_constants_.push_back(e);
  }

  void operator()(const BooleanConstant& e) {
    expr_kinds_.push_back(ExprKind::BooleanConstant);
    bools_.push_back(e.value());
  }

  void operator()(const NullptrConstant&) {
    expr_kinds_.push_back(ExprKind::NullptrConstant);
  }

  void operator()(const EnumConstant& e) {
    expr_kinds_.push_back(ExprKind::EnumConstant);
    enums_.push_back(e);
  }

  void operator()(const ParenthesizedExpr& e) { std::visit(*this, e.expr()); }

  void operator()(const AddressOf& e) {
    expr_kinds_.push_back(ExprKind::AddressOf);
    std::visit(*this, e.expr());
  }

  void operator()(const MemberOf& e) {
    expr_kinds_.push_back(ExprKind::MemberOf);
    (*this)(e.expr_type());
    fields_.push_back(e.field());
    std::visit(*this, e.expr());
  }

  void operator()(const MemberOfPtr& e) {
    expr_kinds_.push_back(ExprKind::MemberOfPtr);
    (*this)(e.expr_type());
    fields_.push_back(e.field());
    std::visit(*this, e.expr());
  }

  void operator()(const ArrayIndex& e) {
    expr_kinds_.push_back(ExprKind::ArrayIndex);

    std::visit(*this, e.expr());
    std::visit(*this, e.idx());
  }

  void operator()(const TernaryExpr& e) {
    expr_kinds_.push_back(ExprKind::TernaryExpr);
    std::visit(*this, e.cond());

    const auto* unit_test_type = e.expr_type();
    if (unit_test_type != nullptr) {
      std::visit(*this, *unit_test_type);
    }

    std::visit(*this, e.lhs());
    std::visit(*this, e.rhs());
  }

  void operator()(const CastExpr& e) {
    expr_kinds_.push_back(ExprKind::CastExpr);
    cast_kinds_.push_back(e.kind());
    std::visit(*this, e.type());
    std::visit(*this, e.expr());
  }

  void operator()(const DereferenceExpr& e) {
    expr_kinds_.push_back(ExprKind::DereferenceExpr);
    std::visit(*this, e.expr());
  }

  void operator()(const FunctionCallExpr&) {
    assert(false && "Not implemented yet!");
  }

  void operator()(const QualifiedType& e) {
    cv_qualifiers_.push_back(e.cv_qualifiers());
    std::visit(*this, e.type());
  }

  void operator()(const PointerType& e) {
    type_kinds_.push_back(TypeKind::PointerType);
    (*this)(e.type());
  }

  void operator()(const TaggedType& e) {
    type_kinds_.push_back(TypeKind::TaggedType);
    tagged_types_.push_back(e);
  }

  void operator()(const NullptrType&) {
    type_kinds_.push_back(TypeKind::NullptrType);
  }

  void operator()(const EnumType& e) {
    type_kinds_.push_back(TypeKind::EnumType);
    enum_types_.push_back(e);
  }

  void operator()(const ScalarType& e) {
    type_kinds_.push_back(TypeKind::ScalarType);
    scalar_types_.push_back(e);
  }

  void operator()(const ArrayType&) { assert(false && "Not implemented"); }

 private:
  std::vector<UnOp> un_ops_;
  std::vector<BinOp> bin_ops_;
  std::vector<IntegerConstant> int_constants_;
  std::vector<DoubleConstant> double_constants_;
  std::vector<bool> bools_;
  std::vector<EnumConstant> enums_;
  std::vector<ExprKind> expr_kinds_;
  std::vector<CastExpr::Kind> cast_kinds_;

  std::vector<TypeKind> type_kinds_;
  std::vector<CvQualifiers> cv_qualifiers_;
  std::vector<ScalarType> scalar_types_;
  std::vector<TaggedType> tagged_types_;
  std::vector<EnumType> enum_types_;
  std::vector<std::string> fields_;

  std::vector<VariableExpr> vars_;

  std::vector<bool> flip_operands_;
  std::vector<bool> gen_binop_ptr_expr_;
  std::vector<bool> gen_binop_ptrdiff_expr_;
  std::vector<bool> gen_binop_ptr_or_enum_;
};

struct Mismatch {
  std::string lhs;
  std::string rhs;

  Mismatch(std::string lhs, std::string rhs) : lhs(lhs), rhs(rhs) {}
};

class AstComparator {
 public:
  void operator()(const UnaryExpr& lhs, const UnaryExpr& rhs) {
    if (lhs.op() != rhs.op()) {
      add_mismatch(lhs, rhs);
      return;
    }

    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const BinaryExpr& lhs, const BinaryExpr& rhs) {
    if (lhs.op() != rhs.op()) {
      add_mismatch(lhs, rhs);
      return;
    }

    std::visit(*this, lhs.lhs(), rhs.lhs());
    std::visit(*this, lhs.rhs(), rhs.rhs());
  }

  void operator()(const VariableExpr& lhs, const VariableExpr& rhs) {
    if (lhs.name() != rhs.name()) {
      add_mismatch(lhs, rhs);
    }
  }

  void operator()(const IntegerConstant& lhs, const IntegerConstant& rhs) {
    if (lhs.value() != rhs.value()) {
      add_mismatch(lhs, rhs);
    }
  }

  void operator()(const DoubleConstant& lhs, const DoubleConstant& rhs) {
    if (lhs.value() != rhs.value()) {
      add_mismatch(lhs, rhs);
    }
  }

  void operator()(const DereferenceExpr& lhs, const DereferenceExpr& rhs) {
    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const ParenthesizedExpr& lhs, const ParenthesizedExpr& rhs) {
    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const AddressOf& lhs, const AddressOf& rhs) {
    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const MemberOf& lhs, const MemberOf& rhs) {
    if (lhs.field() != rhs.field()) {
      add_mismatch(lhs, rhs);
      return;
    }

    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const MemberOfPtr& lhs, const MemberOfPtr& rhs) {
    if (lhs.field() != rhs.field()) {
      add_mismatch(lhs, rhs);
      return;
    }

    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const ArrayIndex& lhs, const ArrayIndex& rhs) {
    std::visit(*this, lhs.expr(), rhs.expr());
    std::visit(*this, lhs.idx(), rhs.idx());
  }

  void operator()(const TernaryExpr& lhs, const TernaryExpr& rhs) {
    std::visit(*this, lhs.cond(), rhs.cond());
    std::visit(*this, lhs.lhs(), rhs.lhs());
    std::visit(*this, lhs.rhs(), rhs.rhs());
  }

  void operator()(const FunctionCallExpr& lhs, const FunctionCallExpr& rhs) {
    if (lhs.name() != rhs.name()) {
      add_mismatch(lhs.name(), rhs.name());
      return;
    }

    if (lhs.args().size() != rhs.args().size()) {
      std::string lhs_message =
          std::to_string(lhs.args().size()) + " arguments";
      std::string rhs_message =
          std::to_string(rhs.args().size()) + " arguments";
      add_mismatch(lhs_message, rhs_message);
    }

    for (size_t i = 0; i < lhs.args().size(); ++i) {
      std::visit(*this, *lhs.args()[i], *rhs.args()[i]);
    }
  }

  void operator()(const CastExpr& lhs, const CastExpr& rhs) {
    if (lhs.kind() != rhs.kind()) {
      add_mismatch(lhs.kind(), rhs.kind());
      return;
    }

    std::visit(*this, lhs.type(), rhs.type());
    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const QualifiedType& lhs, const QualifiedType& rhs) {
    std::visit(*this, lhs.type(), rhs.type());
    if (lhs.cv_qualifiers() != rhs.cv_qualifiers()) {
      add_mismatch(lhs.cv_qualifiers(), rhs.cv_qualifiers());
    }
  }

  void operator()(const PointerType& lhs, const PointerType& rhs) {
    (*this)(lhs.type(), rhs.type());
  }

  void operator()(const TaggedType& lhs, const TaggedType& rhs) {
    if (lhs.name() != rhs.name()) {
      add_mismatch(lhs.name(), rhs.name());
    }
  }

  void operator()(const NullptrType&, const NullptrType&) {
    // Nothing to compare here.
  }

  void operator()(const EnumType& lhs, const EnumType& rhs) {
    if (lhs.name() != rhs.name()) {
      add_mismatch(lhs.name(), rhs.name());
    }
  }

  void operator()(const ArrayType& lhs, const ArrayType& rhs) {
    std::visit(*this, lhs.type(), rhs.type());
    if (lhs.size() != rhs.size()) {
      add_mismatch(lhs.size(), rhs.size());
    }
  }

  void operator()(ScalarType lhs, ScalarType rhs) {
    if (lhs != rhs) {
      add_mismatch(lhs, rhs);
    }
  }

  void operator()(BooleanConstant lhs, BooleanConstant rhs) {
    if (lhs.value() != rhs.value()) {
      add_mismatch(lhs.value(), rhs.value());
    }
  }

  void operator()(const NullptrConstant&, const NullptrConstant&) {
    // Nothing to compare here.
  }

  void operator()(const EnumConstant& lhs, const EnumConstant& rhs) {
    if (lhs.literal() != rhs.literal()) {
      add_mismatch(lhs.literal(), rhs.literal());
    }
  }

  template <typename T, typename U,
            typename = std::enable_if_t<!std::is_same_v<T, U>>>
  void operator()(const T& lhs, const U& rhs) {
    add_mismatch(lhs, rhs);
  }

  const std::vector<Mismatch>& mismatches() const { return mismatches_; }

 private:
  template <typename T, typename U>
  void add_mismatch(const T& lhs, const U& rhs) {
    std::ostringstream lhs_stream;
    std::ostringstream rhs_stream;

    lhs_stream << lhs;
    rhs_stream << rhs;

    mismatches_.emplace_back(lhs_stream.str(), rhs_stream.str());
  }

 private:
  std::vector<Mismatch> mismatches_;
};

MATCHER_P(MatchesAst, expected,
          (negation ? "does not match AST " : "matches AST ") +
              PrintToString(expected.get())) {
  AstComparator cmp;

  std::visit(cmp, expected.get(), arg);
  const auto& mismatches = cmp.mismatches();
  if (mismatches.empty()) {
    return true;
  }

  *result_listener << "with mismatches occuring as follows:\n";
  for (const auto& e : mismatches) {
    *result_listener << "* Expected: " << PrintToString(e.lhs) << "\n"
                     << "*   Actual: " << PrintToString(e.rhs) << "\n";
  }

  return false;
}

struct PrecedenceTestParam {
  std::string str;
  Expr expr;
  std::vector<bool> flip_operands;
  std::vector<bool> gen_binop_ptr_expr;
  std::vector<bool> gen_binop_ptrdiff_expr;
  std::vector<bool> gen_binop_ptr_or_enum;

  PrecedenceTestParam(
      std::string str, Expr expr,
      std::vector<bool> flip_operands = std::vector<bool>(),
      std::vector<bool> gen_binop_ptr_expr = std::vector<bool>(),
      std::vector<bool> gen_binop_ptrdiff_expr = std::vector<bool>(),
      std::vector<bool> gen_binop_ptr_or_enum = std::vector<bool>())
      : str(std::move(str)),
        expr(std::move(expr)),
        flip_operands(std::move(flip_operands)),
        gen_binop_ptr_expr(std::move(gen_binop_ptr_expr)),
        gen_binop_ptrdiff_expr(std::move(gen_binop_ptrdiff_expr)),
        gen_binop_ptr_or_enum(std::move(gen_binop_ptr_or_enum)) {}
};

std::ostream& operator<<(std::ostream& os, const PrecedenceTestParam& param) {
  return os << "`" << param.str << "`";
}

class OperatorPrecedence : public TestWithParam<PrecedenceTestParam> {};

TEST_P(OperatorPrecedence, CorrectAst) {
  const auto& param = GetParam();

  auto fake_rng =
      std::make_unique<FakeGeneratorRng>(FakeGeneratorRng::from_expr(
          param.expr, param.flip_operands, param.gen_binop_ptr_expr,
          param.gen_binop_ptrdiff_expr, param.gen_binop_ptr_or_enum));

  ExprGenerator gen(std::move(fake_rng), GenConfig(), SymbolTable());
  auto maybe_expr = gen.generate();
  auto& expr = maybe_expr.value();
  std::ostringstream os;
  os << expr;

  EXPECT_THAT(expr, MatchesAst(std::cref(param.expr)));
  EXPECT_THAT(os.str(), StrEq(param.str));
}

std::vector<PrecedenceTestParam> gen_precedence_params() {
  std::vector<PrecedenceTestParam> params;
  {
    // clang-format off
    Expr expected = BinaryExpr(
        IntegerConstant(3),
        BinOp::Mult,
        ParenthesizedExpr(
            BinaryExpr(IntegerConstant(4), BinOp::Plus, IntegerConstant(5))));
    // clang-format on

    std::string str = "3 * (4 + 5)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = BinaryExpr(
        BinaryExpr(IntegerConstant(3), BinOp::Mult, IntegerConstant(4)),
        BinOp::Plus,
        IntegerConstant(5));
    // clang-format on

    std::string str = "3 * 4 + 5";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = BinaryExpr(
        BinaryExpr(IntegerConstant(3), BinOp::Minus, IntegerConstant(4)),
        BinOp::Plus,
        IntegerConstant(5));
    // clang-format on

    std::string str = "3 - 4 + 5";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = BinaryExpr(
        IntegerConstant(3),
        BinOp::Minus,
        ParenthesizedExpr(
            BinaryExpr(IntegerConstant(4), BinOp::Plus, IntegerConstant(5))));
    // clang-format on

    std::string str = "3 - (4 + 5)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = UnaryExpr(UnOp::Neg,
                              UnaryExpr(UnOp::Neg, IntegerConstant(1)));
    // clang-format on

    std::string str = "- -1";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected =
        CastExpr(CastExpr::Kind::CStyleCast, ScalarType::SignedInt,
                 IntegerConstant(50));
    // clang-format on

    std::string str = "(int) 50";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = BinaryExpr(
        CastExpr(CastExpr::Kind::CStyleCast, ScalarType::SignedInt,
                 IntegerConstant(50)),
        BinOp::Plus,
        IntegerConstant(1));
    // clang-format on

    std::string str = "(int) 50 + 1";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = CastExpr(
        CastExpr::Kind::CStyleCast, ScalarType::SignedInt,
        ParenthesizedExpr(
            BinaryExpr(IntegerConstant(50), BinOp::Plus, IntegerConstant(1))));
    // clang-format on

    std::string str = "(int) (50 + 1)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = CastExpr(
        CastExpr::Kind::StaticCast, ScalarType::SignedInt,
        BinaryExpr(IntegerConstant(50), BinOp::Plus, IntegerConstant(1)));
    // clang-format on

    std::string str = "static_cast<int>(50 + 1)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = CastExpr(
        CastExpr::Kind::CStyleCast, ScalarType::Float,
        CastExpr(
            CastExpr::Kind::StaticCast, ScalarType::SignedInt,
            IntegerConstant(5)));
    // clang-format on

    std::string str = "(float) static_cast<int>(5)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = UnaryExpr(
        UnOp::BitNot,
        ParenthesizedExpr(TernaryExpr(
            IntegerConstant(1),
            IntegerConstant(2),
            IntegerConstant(3),
            ScalarType::SignedInt)));
    // clang-format on

    std::string str = "~(1 ? 2 : 3)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = BinaryExpr(
        IntegerConstant(1),
        BinOp::Mult,
        ParenthesizedExpr(
            TernaryExpr(
                BooleanConstant(true),
                IntegerConstant(0),
                TernaryExpr(
                    BooleanConstant(false),
                    IntegerConstant(1),
                    IntegerConstant(2), ScalarType::SignedInt),
                ScalarType::SignedInt)));
    // clang-format on

    std::string str = "1 * (true ? 0 : false ? 1 : 2)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = BinaryExpr(
        IntegerConstant(4),
        BinOp::Mult,
        ParenthesizedExpr(
            TernaryExpr(ParenthesizedExpr(TernaryExpr(
                BinaryExpr(IntegerConstant(1), BinOp::Eq, IntegerConstant(2)),
                BooleanConstant(false),
                BooleanConstant(true),
                ScalarType::Bool)),
        IntegerConstant(1),
        IntegerConstant(0),
        ScalarType::SignedInt)));
    // clang-format on

    std::string str = "4 * ((1 == 2 ? false : true) ? 1 : 0)";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    // clang-format off
    Expr expected = AddressOf(
        ArrayIndex(
            ParenthesizedExpr(
                CastExpr(CastExpr::Kind::CStyleCast, ScalarType::SignedInt,
                         BooleanConstant(true))),
            CastExpr(
                CastExpr::Kind::CStyleCast,
                PointerType(QualifiedType(ScalarType::Char)),
                IntegerConstant(0))));
    // clang-format on

    std::string str = "&((int) true)[(char*) 0]";
    std::vector<bool> flip_operands = {true};
    params.emplace_back(std::move(str), std::move(expected),
                        std::move(flip_operands));
  }
  {
    Expr expected = UnaryExpr(UnOp::Neg, DoubleConstant(1.5));

    std::string str = "-1.5";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    Expr expected = IntegerConstant(0xbadf00d, IntegerConstant::Base::Hex,
                                    IntegerConstant::Length::LongLong,
                                    IntegerConstant::Signedness::Unsigned);

    std::string str = "0xbadf00dLLU";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    Expr expected = IntegerConstant(0b1001'1001, IntegerConstant::Base::Bin,
                                    IntegerConstant::Length::Long,
                                    IntegerConstant::Signedness::Signed);

    std::string str = "0b10011001L";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    Expr expected = DoubleConstant(1.5, DoubleConstant::Format::Default,
                                   DoubleConstant::Length::Float);

    std::string str = "1.5f";
    params.emplace_back(std::move(str), std::move(expected));
  }
  {
    Expr expected = DoubleConstant(0x0.1p-1070, DoubleConstant::Format::Hex,
                                   DoubleConstant::Length::Double);

#if defined(_MSC_VER) && _MSC_VER < 1928
    // Apparently MSVC seems to not be adhering to the C++11 standard properly
    // and it takes into account the precision modifier when printing hex
    // floats (https://en.cppreference.com/w/cpp/locale/num_put/put#Notes).
    //
    // Hence we're using this ifdef guard as a temporary workaround.
    std::string str = "0x0.000000p-1022";
#else
    std::string str = "0x0.0000000000001p-1022";
#endif

    params.emplace_back(std::move(str), std::move(expected));
  }

  return params;
}

INSTANTIATE_TEST_SUITE_P(AstGen, OperatorPrecedence,
                         ValuesIn(gen_precedence_params()));

struct TypePrintTestParam {
  std::string str;
  Type type;

  TypePrintTestParam(std::string str, Type type)
      : str(std::move(str)), type(std::move(type)) {}
};

std::ostream& operator<<(std::ostream& os, const TypePrintTestParam& param) {
  return os << param.str;
}

class TypePrinting : public TestWithParam<TypePrintTestParam> {};

TEST_P(TypePrinting, CorrectStrType) {
  const auto& param = GetParam();

  std::ostringstream os;
  os << param.type;
  EXPECT_THAT(os.str(), StrEq(param.str));
}

std::vector<TypePrintTestParam> gen_typing_params() {
  std::vector<TypePrintTestParam> params;

  {
    auto type = ScalarType::SignedInt;
    std::string str = "int";
    params.emplace_back(std::move(str), std::move(type));
  }

  {
    Type type(PointerType(QualifiedType(
        PointerType(QualifiedType(ScalarType::Char, CvQualifier::Const)))));
    std::string str = "const char**";

    params.emplace_back(std::move(str), std::move(type));
  }

  {
    PointerType type(QualifiedType(
        PointerType(QualifiedType(ScalarType::SignedInt, CvQualifier::Const)),
        CvQualifier::Volatile));
    std::string str = "const int* volatile*";

    params.emplace_back(std::move(str), std::move(type));
  }

  {
    PointerType type(
        QualifiedType(TaggedType("TestStruct"), CvQualifier::Const));
    std::string str = "const TestStruct*";

    params.emplace_back(std::move(str), std::move(type));
  }

  {
    PointerType type(QualifiedType(PointerType(QualifiedType(ScalarType::Void)),
                                   CvQualifier::Const));
    std::string str = "void* const*";

    params.emplace_back(std::move(str), std::move(type));
  }

  return params;
}

INSTANTIATE_TEST_SUITE_P(AstGen, TypePrinting, ValuesIn(gen_typing_params()));
