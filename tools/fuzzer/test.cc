#include <algorithm>
#include <cassert>
#include <type_traits>
#include <utility>
#include <vector>

#include "ast.h"
#include "expr_gen.h"
#include "gmock/gmock.h"
#include "gtest/gtest.h"

using namespace fuzzer;
using namespace testing;

namespace fuzzer {

void PrintTo(const Expr& expr, std::ostream* os) {
  *os << "`" << stringify_expr(expr) << "`";
}

}  // namespace fuzzer

class FakeGeneratorRng : public GeneratorRng {
 public:
  FakeGeneratorRng(std::vector<uint64_t> fake_u64_results,
                   std::vector<int> fake_int_results,
                   std::vector<double> fake_double_results,
                   std::vector<bool> fake_bool_results)
      : fake_u64_results_(std::move(fake_u64_results)),
        fake_int_results_(std::move(fake_int_results)),
        fake_double_results_(std::move(fake_double_results)),
        fake_bool_results_(std::move(fake_bool_results)) {
    std::reverse(fake_u64_results_.begin(), fake_u64_results_.end());
    std::reverse(fake_int_results_.begin(), fake_int_results_.end());
    std::reverse(fake_double_results_.begin(), fake_double_results_.end());
    std::reverse(fake_bool_results_.begin(), fake_bool_results_.end());
  }

  int gen_int(int min, int max) override {
    assert(!fake_int_results_.empty());
    auto val = fake_int_results_.back();
    fake_int_results_.pop_back();

    assert(min <= val && val <= max);
    return val;
  }

  uint64_t gen_u64(uint64_t min, uint64_t max) override {
    assert(!fake_u64_results_.empty());
    auto val = fake_u64_results_.back();
    fake_u64_results_.pop_back();

    assert(min <= val && val <= max);
    return val;
  }

  double gen_double(double min, double max) override {
    assert(!fake_double_results_.empty());
    auto val = fake_double_results_.back();
    fake_double_results_.pop_back();

    assert(min <= val && val <= max);
    return val;
  }

  bool gen_bernoulli(float p = 0.5) override {
    assert(!fake_bool_results_.empty());
    auto val = fake_bool_results_.back();
    fake_bool_results_.pop_back();

    if (p == 0.0) {
      assert(!val);
    } else if (p == 1.0) {
      assert(val);
    }

    return val;
  }

 private:
  std::vector<uint64_t> fake_u64_results_;
  std::vector<int> fake_int_results_;
  std::vector<double> fake_double_results_;
  std::vector<bool> fake_bool_results_;
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

  void operator()(const ParenthesizedExpr& lhs, const ParenthesizedExpr& rhs) {
    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const AddressOf& lhs, const AddressOf& rhs) {
    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const FieldAccess& lhs, const FieldAccess& rhs) {
    if (lhs.field() != rhs.field()) {
      add_mismatch(lhs, rhs);
      return;
    }

    std::visit(*this, lhs.expr(), rhs.expr());
  }

  void operator()(const PtrFieldAccess& lhs, const PtrFieldAccess& rhs) {
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

  template <typename T, typename U,
            typename = std::enable_if_t<!std::is_same_v<T, U>>>
  void operator()(const T& lhs, const U& rhs) {
    add_mismatch(lhs, rhs);
  }

  const std::vector<Mismatch>& mismatches() const { return mismatches_; }

 private:
  template <typename T, typename U>
  void add_mismatch(const T& lhs, const U& rhs) {
    mismatches_.emplace_back(stringify_expr(lhs), stringify_expr(rhs));
  }

 private:
  std::vector<Mismatch> mismatches_;
};

MATCHER_P(MatchesAst, expected,
          (negation ? "does not match AST " : "matches AST ") +
              PrintToString(expected)) {
  AstComparator cmp;

  const Expr& e = expected;

  std::visit(cmp, arg, e);
  const auto& mismatches = cmp.mismatches();
  if (mismatches.empty()) {
    return true;
  }

  *result_listener << "with mismatches occuring as follows:\n";
  for (const auto& e : mismatches) {
    *result_listener << "Given: `" << e.lhs << "`, expected: " << e.rhs
                     << "`\n";
  }

  return false;
}

TEST(Fuzzer, BinaryPrecedenceLower) {
  // clang-format off
  Expr expected = BinaryExpr(
      IntegerConstant(3),
      BinOp::Mult,
      ParenthesizedExpr(
          BinaryExpr(IntegerConstant(4), BinOp::Plus, IntegerConstant(5))));
  // clang-format on

  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Mult, (int)BinOp::Plus},
      std::vector<double>{2.1, 0.0, 2.1, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();

  EXPECT_THAT(expr, MatchesAst(std::cref(expected)));
}

TEST(Fuzzer, BinaryPrecedenceHigher) {
  // clang-format off
  Expr expected = BinaryExpr(
      BinaryExpr(IntegerConstant(3), BinOp::Mult, IntegerConstant(4)),
      BinOp::Plus,
      IntegerConstant(5));
  // clang-format on

  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Plus, (int)BinOp::Mult},
      std::vector<double>{2.1, 2.1, 0.0, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();

  EXPECT_THAT(expr, MatchesAst(std::cref(expected)));
}

TEST(Fuzzer, BinaryAssociativityLeftLeaning) {
  // clang-format off
  Expr expected = BinaryExpr(
      BinaryExpr(IntegerConstant(3), BinOp::Minus, IntegerConstant(4)),
      BinOp::Plus,
      IntegerConstant(5));
  // clang-format on

  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Plus, (int)BinOp::Minus},
      std::vector<double>{2.1, 2.1, 0.0, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();

  EXPECT_THAT(expr, MatchesAst(std::cref(expected)));
}

TEST(Fuzzer, BinaryAssociativityRightLeaning) {
  // clang-format off
  Expr expected = BinaryExpr(
      IntegerConstant(3),
      BinOp::Minus,
      ParenthesizedExpr(
          BinaryExpr(IntegerConstant(4), BinOp::Plus, IntegerConstant(5))));
  // clang-format on

  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Minus, (int)BinOp::Plus},
      std::vector<double>{2.1, 0.0, 2.1, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();

  EXPECT_THAT(expr, MatchesAst(std::cref(expected)));
}
