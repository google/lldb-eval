#include <algorithm>
#include <cassert>
#include <vector>

#include "ast.h"
#include "expr_gen.h"
#include "gtest/gtest.h"

using namespace fuzzer;
using namespace testing;

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

TEST(Fuzzer, BinaryPrecedenceLower) {
  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Mult, (int)BinOp::Plus},
      std::vector<double>{2.1, 0.0, 2.1, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();
  auto str = stringify_expr(expr);

  EXPECT_EQ(str, "3 * (4 + 5)");
}

TEST(Fuzzer, BinaryPrecedenceHigher) {
  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Plus, (int)BinOp::Mult},
      std::vector<double>{2.1, 2.1, 0.0, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();
  auto str = stringify_expr(expr);

  EXPECT_EQ(str, "3 * 4 + 5");
}

TEST(Fuzzer, BinaryAssociativityLeftLeaning) {
  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Plus, (int)BinOp::Minus},
      std::vector<double>{2.1, 2.1, 0.0, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();
  auto str = stringify_expr(expr);

  EXPECT_EQ(str, "3 - 4 + 5");
}

TEST(Fuzzer, BinaryAssociativityRightLeaning) {
  auto fake_rng = std::make_unique<FakeGeneratorRng>(
      std::vector<uint64_t>{3, 4, 5},
      std::vector<int>{(int)BinOp::Minus, (int)BinOp::Plus},
      std::vector<double>{2.1, 0.0, 2.1, 0.0, 0.0},
      std::vector<bool>(5, false));

  ExprGenerator gen(std::move(fake_rng));
  auto expr = gen.generate();
  auto str = stringify_expr(expr);

  EXPECT_EQ(str, "3 - (4 + 5)");
}
