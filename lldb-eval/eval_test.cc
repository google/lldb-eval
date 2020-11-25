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

#include <memory>
#include <optional>
#include <string>
#include <type_traits>

#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "lldb/API/SBType.h"
#include "tools/cpp/runfiles/runfiles.h"

// DISALLOW_COPY_AND_ASSIGN is also defined in
// lldb/lldb-defines.h
#undef DISALLOW_COPY_AND_ASSIGN
#include "gmock/gmock.h"
#include "gtest/gtest.h"

using bazel::tools::cpp::runfiles::Runfiles;

using ::testing::MakeMatcher;
using ::testing::Matcher;
using ::testing::MatcherInterface;
using ::testing::MatchResultListener;

struct EvalResult {
  lldb::SBError lldb_eval_error;
  mutable lldb::SBValue lldb_eval_value;
  mutable std::optional<lldb::SBValue> lldb_value;

  friend std::ostream& operator<<(std::ostream& os, const EvalResult& result) {
    auto maybe_null = [](const char* str) {
      return str == nullptr ? "NULL" : str;
    };

    os << "{ lldb-eval: " << maybe_null(result.lldb_eval_value.GetValue());

    if (result.lldb_value.has_value()) {
      os << ", lldb: " << maybe_null(result.lldb_value.value().GetValue());
    }
    os << " }";

    return os;
  }
};

class EvaluatorHelper {
 public:
  EvaluatorHelper(lldb::SBFrame frame, bool lldb)
      : frame_(frame), lldb_(lldb) {}
  EvaluatorHelper(lldb::SBValue scope, bool lldb)
      : scope_(scope), lldb_(lldb) {}

 public:
  EvalResult Eval(std::string expr) {
    EvalResult ret;

    if (scope_) {
      // Evaluate in the variable context.
      ret.lldb_eval_value = lldb_eval::EvaluateExpression(scope_, expr.c_str(),
                                                          ret.lldb_eval_error);

      if (lldb_) {
        ret.lldb_value = scope_.EvaluateExpression(expr.c_str());
      }

    } else {
      // Evaluate in the frame context.
      ret.lldb_eval_value = lldb_eval::EvaluateExpression(frame_, expr.c_str(),
                                                          ret.lldb_eval_error);

      if (lldb_) {
        ret.lldb_value = frame_.EvaluateExpression(expr.c_str());
      }
    }

    return ret;
  }

 private:
  lldb::SBFrame frame_;
  lldb::SBValue scope_;
  bool lldb_;
};

void PrintError(::testing::MatchResultListener* listener,
                const std::string& error) {
  *listener << "error:";
  // Print multiline errors on a separate line.
  if (error.find('\n') != std::string::npos) {
    *listener << "\n";
  } else {
    *listener << " ";
  }
  *listener << error;
}

class IsOkMatcher : public MatcherInterface<EvalResult> {
 public:
  bool MatchAndExplain(EvalResult result,
                       MatchResultListener* listener) const override {
    if (result.lldb_eval_error.GetError()) {
      PrintError(listener, result.lldb_eval_error.GetCString());
      return false;
    }

    std::string actual = result.lldb_eval_value.GetValue();
    // Compare only if we tried to evaluate with LLDB.
    if (result.lldb_value.has_value() &&
        actual != result.lldb_value.value().GetValue()) {
      *listener << "values produced by lldb-eval and LLDB don't match\n"
                << "lldb-eval: " << actual << "\n"
                << "lldb     : " << result.lldb_value.value().GetValue();
      return false;
    }

    return true;
  }

  void DescribeTo(std::ostream* os) const override {
    *os << "evaluates without an error and equals to LLDB";
  }
};

Matcher<EvalResult> IsOk() { return MakeMatcher(new IsOkMatcher()); }

class IsEqualMatcher : public MatcherInterface<EvalResult> {
 public:
  explicit IsEqualMatcher(std::string value) : value_(std::move(value)) {}

 public:
  bool MatchAndExplain(EvalResult result,
                       MatchResultListener* listener) const override {
    if (result.lldb_eval_error.GetError()) {
      PrintError(listener, result.lldb_eval_error.GetCString());
      return false;
    }

    std::string actual = result.lldb_eval_value.GetValue();
    if (actual != value_) {
      *listener << "evaluated to '" << actual << "'";
      return false;
    }

    // Compare only if we tried to evaluate with LLDB.
    if (result.lldb_value.has_value() &&
        actual != result.lldb_value.value().GetValue()) {
      *listener << "values produced by lldb-eval and LLDB don't match\n"
                << "lldb-eval: " << actual << "\n"
                << "lldb     : " << result.lldb_value.value().GetValue();
      return false;
    }

    return true;
  }

  void DescribeTo(std::ostream* os) const override {
    *os << "evaluates to '" << value_ << "'";
  }

 private:
  std::string value_;
};

Matcher<EvalResult> IsEqual(std::string value) {
  return MakeMatcher(new IsEqualMatcher(std::move(value)));
}

class IsErrorMatcher : public MatcherInterface<EvalResult> {
 public:
  explicit IsErrorMatcher(std::string value) : value_(std::move(value)) {}

 public:
  bool MatchAndExplain(EvalResult result,
                       MatchResultListener* listener) const override {
    if (!result.lldb_eval_error.GetError()) {
      *listener << "evaluated to '" << result.lldb_eval_value.GetValue() << "'";
      return false;
    }
    std::string message = result.lldb_eval_error.GetCString();
    if (message.find(value_) == std::string::npos) {
      PrintError(listener, message);
      return false;
    }

    return true;
  }

  void DescribeTo(std::ostream* os) const override {
    *os << "evaluates with an error: '" << value_ << "'";
  }

 private:
  std::string value_;
};

Matcher<EvalResult> IsError(std::string value) {
  return MakeMatcher(new IsErrorMatcher(std::move(value)));
}

class EvalTest : public ::testing::Test {
 protected:
  static void SetUpTestSuite() {
    runfiles_ = Runfiles::CreateForTest();
    lldb_eval::SetupLLDBServerEnv(*runfiles_);
    lldb::SBDebugger::Initialize();
  }

  static void TearDownTestSuite() {
    lldb::SBDebugger::Terminate();
    delete runfiles_;
    runfiles_ = nullptr;
  }

  void SetUp() {
    std::string test_name =
        ::testing::UnitTest::GetInstance()->current_test_info()->name();
    std::string break_line = "// BREAK(" + test_name + ")";

    auto binary_path = runfiles_->Rlocation("lldb_eval/testdata/test_binary");
    auto source_path =
        runfiles_->Rlocation("lldb_eval/testdata/test_binary.cc");

    debugger_ = lldb::SBDebugger::Create(false);
    process_ = lldb_eval::LaunchTestProgram(debugger_, source_path, binary_path,
                                            break_line);
    frame_ = process_.GetSelectedThread().GetSelectedFrame();
  }

  void TearDown() {
    process_.Destroy();
    lldb::SBDebugger::Destroy(debugger_);
  }

  EvalResult Eval(std::string expr) {
    return EvaluatorHelper(frame_, compare_with_lldb_).Eval(std::move(expr));
  }
  EvaluatorHelper Scope(std::string scope) {
    // Resolve the scope variable (assume it's a local variable).
    lldb::SBValue scope_var = frame_.FindVariable(scope.c_str());
    return EvaluatorHelper(scope_var, compare_with_lldb_);
  }

 protected:
  lldb::SBDebugger debugger_;
  lldb::SBProcess process_;
  lldb::SBFrame frame_;

  // Evaluate with both lldb-eval and LLDB by default.
  bool compare_with_lldb_ = true;

  static Runfiles* runfiles_;
};

Runfiles* EvalTest::runfiles_ = nullptr;

TEST_F(EvalTest, TestSymbols) {
  EXPECT_GT(frame_.GetModule().GetNumSymbols(), 0)
      << "No symbols might indicate that the test binary was built incorrectly";
}

TEST_F(EvalTest, TestArithmetic) {
  EXPECT_THAT(Eval("1 + 2"), IsEqual("3"));
  EXPECT_THAT(Eval("1 + 2*3"), IsEqual("7"));
  EXPECT_THAT(Eval("1 + (2 - 3)"), IsEqual("0"));
  EXPECT_THAT(Eval("1 == 2"), IsEqual("false"));
  EXPECT_THAT(Eval("1 == 1"), IsEqual("true"));

  // Note: Signed overflow is UB.
  EXPECT_THAT(Eval("int_max + 1"), IsOk());
  EXPECT_THAT(Eval("int_min - 1"), IsOk());
  EXPECT_THAT(Eval("2147483647 + 1"), IsOk());
  EXPECT_THAT(Eval("-2147483648 - 1"), IsOk());

  EXPECT_THAT(Eval("uint_max + 1"), IsEqual("0"));
  EXPECT_THAT(Eval("uint_zero - 1"), IsEqual("4294967295"));
  EXPECT_THAT(Eval("4294967295 + 1"), IsEqual("4294967296"));
  EXPECT_THAT(Eval("4294967295U + 1"), IsEqual("0"));

  // Note: Signed overflow is UB.
  EXPECT_THAT(Eval("ll_max + 1"), IsOk());
  EXPECT_THAT(Eval("ll_min - 1"), IsOk());
  EXPECT_THAT(Eval("9223372036854775807 + 1"), IsOk());
  EXPECT_THAT(Eval("-9223372036854775808 - 1"), IsOk());

  EXPECT_THAT(Eval("ull_max + 1"), IsEqual("0"));
  EXPECT_THAT(Eval("ull_zero - 1"), IsEqual("18446744073709551615"));
  EXPECT_THAT(Eval("9223372036854775807 + 1"), IsEqual("-9223372036854775808"));
  EXPECT_THAT(Eval("9223372036854775807LL + 1"),
              IsEqual("-9223372036854775808"));
  EXPECT_THAT(Eval("18446744073709551615ULL + 1"), IsEqual("0"));

  // Integer literal is too large to be represented in a signed integer type,
  // interpreting as unsigned.
  EXPECT_THAT(Eval("-9223372036854775808"), IsEqual("9223372036854775808"));
  EXPECT_THAT(Eval("-9223372036854775808 - 1"), IsEqual("9223372036854775807"));
  EXPECT_THAT(Eval("-9223372036854775808 + 1"), IsEqual("9223372036854775809"));
  EXPECT_THAT(Eval("-9223372036854775808LL / -1"), IsEqual("0"));
  EXPECT_THAT(Eval("-9223372036854775808LL % -1"),
              IsEqual("9223372036854775808"));

  EXPECT_THAT(Eval("-20 / 1U"), IsEqual("4294967276"));
  EXPECT_THAT(Eval("-20LL / 1U"), IsEqual("-20"));
  EXPECT_THAT(Eval("-20LL / 1ULL"), IsEqual("18446744073709551596"));

  // Unary arithmetic.
  EXPECT_THAT(Eval("+0"), IsEqual("0"));
  EXPECT_THAT(Eval("-0"), IsEqual("0"));
  EXPECT_THAT(Eval("+1"), IsEqual("1"));
  EXPECT_THAT(Eval("-1"), IsEqual("-1"));
  EXPECT_THAT(Eval("c"), IsEqual("'\\n'"));
  EXPECT_THAT(Eval("+c"), IsEqual("10"));
  EXPECT_THAT(Eval("-c"), IsEqual("-10"));
  EXPECT_THAT(Eval("uc"), IsEqual("'\\x01'"));
  EXPECT_THAT(Eval("-uc"), IsEqual("-1"));
  EXPECT_THAT(Eval("+p"), IsOk());
  EXPECT_THAT(Eval("-p"),
              IsError("invalid argument type 'int *' to unary expression"));

  // Floating tricks.
  EXPECT_THAT(Eval("+0.0"), IsEqual("0"));
  EXPECT_THAT(Eval("-0.0"), IsEqual("-0"));
  EXPECT_THAT(Eval("0.0 / 0"), IsEqual("NaN"));
  EXPECT_THAT(Eval("0 / 0.0"), IsEqual("NaN"));
  EXPECT_THAT(Eval("1 / +0.0"), IsEqual("+Inf"));
  EXPECT_THAT(Eval("1 / -0.0"), IsEqual("-Inf"));
  EXPECT_THAT(Eval("+0.0 / +0.0  != +0.0 / +0.0"), IsEqual("true"));
  EXPECT_THAT(Eval("-1.f * 0"), IsEqual("-0"));
  EXPECT_THAT(Eval("0x0.123p-1"), IsEqual("0.0355224609375"));

  EXPECT_THAT(Eval("fnan < fnan"), IsEqual("false"));
  EXPECT_THAT(Eval("fnan <= fnan"), IsEqual("false"));
  EXPECT_THAT(Eval("fnan == fnan"), IsEqual("false"));
  EXPECT_THAT(Eval("(unsigned int) fdenorm"), IsEqual("0"));
  EXPECT_THAT(Eval("(unsigned int) (1.0f + fdenorm)"), IsEqual("1"));

  // References and typedefs.
  EXPECT_THAT(Eval("r + 1"), IsEqual("3"));
  EXPECT_THAT(Eval("r - 1"), IsEqual("1"));
  EXPECT_THAT(Eval("r * 2"), IsEqual("4"));
  EXPECT_THAT(Eval("r / 2"), IsEqual("1"));
  EXPECT_THAT(Eval("my_r + 1"), IsEqual("3"));
  EXPECT_THAT(Eval("my_r - 1"), IsEqual("1"));
  EXPECT_THAT(Eval("my_r * 2"), IsEqual("4"));
  EXPECT_THAT(Eval("my_r / 2"), IsEqual("1"));
  EXPECT_THAT(Eval("r + my_r"), IsEqual("4"));
  EXPECT_THAT(Eval("r - my_r"), IsEqual("0"));
  EXPECT_THAT(Eval("r * my_r"), IsEqual("4"));
  EXPECT_THAT(Eval("r / my_r"), IsEqual("1"));

  // Some promotions and conversions.
  EXPECT_THAT(Eval("(uint8_t)250 + (uint8_t)250"), IsEqual("500"));

#ifdef _WIN32
  // On Windows sizeof(int) == sizeof(long) == 4.
  EXPECT_THAT(Eval("(unsigned int)4294967295 + (long)2"), IsEqual("1"));
  EXPECT_THAT(Eval("((unsigned int)1 + (long)1) - 3"), IsEqual("4294967295"));
#else
  // On Linux sizeof(int) == 4 and sizeof(long) == 8.
  EXPECT_THAT(Eval("(unsigned int)4294967295 + (long)2"),
              IsEqual("4294967297"));
  EXPECT_THAT(Eval("((unsigned int)1 + (long)1) - 3"), IsEqual("-1"));
#endif
}

TEST_F(EvalTest, TestZeroDivision) {
  // Zero division and remainder is UB and LLDB return garbage values. Our
  // implementation returns zero, but that might change in the future. The
  // important thing here is to avoid crashing with SIGFPE.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("1 / 0"), IsEqual("0"));
  EXPECT_THAT(Eval("1 / uint_zero"), IsEqual("0"));
  EXPECT_THAT(Eval("1 % 0"), IsEqual("0"));
  EXPECT_THAT(Eval("1 % uint_zero"), IsEqual("0"));
}

TEST_F(EvalTest, TestBitwiseOperators) {
  EXPECT_THAT(Eval("~(-1)"), IsEqual("0"));
  EXPECT_THAT(Eval("~~0"), IsEqual("0"));
  EXPECT_THAT(Eval("~0"), IsEqual("-1"));
  EXPECT_THAT(Eval("~1"), IsEqual("-2"));
  EXPECT_THAT(Eval("~0LL"), IsEqual("-1"));
  EXPECT_THAT(Eval("~1LL"), IsEqual("-2"));
  EXPECT_THAT(Eval("~true"), IsEqual("-2"));
  EXPECT_THAT(Eval("~false"), IsEqual("-1"));
  EXPECT_THAT(Eval("~var_true"), IsEqual("-2"));
  EXPECT_THAT(Eval("~var_false"), IsEqual("-1"));
  EXPECT_THAT(Eval("~ull_max"), IsEqual("0"));
  EXPECT_THAT(Eval("~ull_zero"), IsEqual("18446744073709551615"));

  EXPECT_THAT(Eval("~s"),
              IsError("invalid argument type 'S' to unary expression"));
  EXPECT_THAT(
      Eval("~p"),
      IsError("invalid argument type 'const char *' to unary expression"));

  EXPECT_THAT(Eval("(1 << 5)"), IsEqual("32"));
  EXPECT_THAT(Eval("(32 >> 2)"), IsEqual("8"));

  EXPECT_THAT(Eval("0b1011 & 0xFF"), IsEqual("11"));
  EXPECT_THAT(Eval("0b1011 & mask_ff"), IsEqual("11"));
  EXPECT_THAT(Eval("0b1011 & 0b0111"), IsEqual("3"));
  EXPECT_THAT(Eval("0b1011 | 0b0111"), IsEqual("15"));
  EXPECT_THAT(Eval("-0b1011 | 0xFF"), IsEqual("-1"));
  EXPECT_THAT(Eval("-0b1011 | 0xFFu"), IsEqual("4294967295"));
  EXPECT_THAT(Eval("0b1011 ^ 0b0111"), IsEqual("12"));
  EXPECT_THAT(Eval("~0b1011"), IsEqual("-12"));
}

TEST_F(EvalTest, TestPointerArithmetic) {
  EXPECT_THAT(Eval("p_char1"), IsOk());
  EXPECT_THAT(Eval("p_char1 + 1"), IsOk());
  EXPECT_THAT(Eval("p_char1 + offset"), IsOk());

  EXPECT_THAT(Eval("my_p_char1"), IsOk());
  EXPECT_THAT(Eval("my_p_char1 + 1"), IsOk());
  EXPECT_THAT(Eval("my_p_char1 + offset"), IsOk());

  EXPECT_THAT(Eval("*(p_char1 + 0)"), IsEqual("'h'"));
  EXPECT_THAT(Eval("*(1 + p_char1)"), IsEqual("'e'"));
  EXPECT_THAT(Eval("*(p_char1 + 2)"), IsEqual("'l'"));
  EXPECT_THAT(Eval("*(3 + p_char1)"), IsEqual("'l'"));
  EXPECT_THAT(Eval("*(p_char1 + 4)"), IsEqual("'o'"));
  EXPECT_THAT(Eval("*(p_char1 + offset - 1)"), IsEqual("'o'"));

  EXPECT_THAT(Eval("*p_int0"), IsEqual("0"));
  EXPECT_THAT(Eval("*cp_int5"), IsEqual("5"));
  EXPECT_THAT(Eval("*(&*(cp_int5 + 1) - 1)"), IsEqual("5"));

  EXPECT_THAT(Eval("p_int0 - p_int0"), IsEqual("0"));
  EXPECT_THAT(Eval("cp_int5 - p_int0"), IsEqual("5"));
  EXPECT_THAT(Eval("cp_int5 - td_int_ptr0"), IsEqual("5"));
  EXPECT_THAT(Eval("td_int_ptr0 - cp_int5"), IsEqual("-5"));

  EXPECT_THAT(
      Eval("-p_char1"),
      IsError("invalid argument type 'const char *' to unary expression"));
  EXPECT_THAT(Eval("cp_int5 - p_char1"),
              IsError("'const int *' and 'const char *' are not pointers to "
                      "compatible types"));
  EXPECT_THAT(Eval("p_int0 + cp_int5"),
              IsError("invalid operands to binary expression ('int *' and "
                      "'const int *')"));
  EXPECT_THAT(Eval("p_int0 > p_char1"),
              IsError("comparison of distinct pointer types ('int *' and "
                      "'const char *')"));

  EXPECT_THAT(Eval("cp_int5 > td_int_ptr0"), IsEqual("true"));
  EXPECT_THAT(Eval("cp_int5 < td_int_ptr0"), IsEqual("false"));
  EXPECT_THAT(Eval("cp_int5 != td_int_ptr0"), IsEqual("true"));
  EXPECT_THAT(Eval("cp_int5 == td_int_ptr0 + offset"), IsEqual("true"));

  EXPECT_THAT(Eval("p_void + 1"), IsError("arithmetic on a pointer to void"));
  EXPECT_THAT(Eval("p_void - 1"), IsError("arithmetic on a pointer to void"));
  EXPECT_THAT(Eval("p_void - p_char1"),
              IsError("'void *' and 'const char *' are not pointers to "
                      "compatible types"));
  EXPECT_THAT(Eval("p_void - p_void"),
              IsError("arithmetic on pointers to void"));

  EXPECT_THAT(Eval("pp_void0 - p_char1"),
              IsError("'void **' and 'const char *' are not pointers to "
                      "compatible types"));
  EXPECT_THAT(Eval("pp_void0 == p_char1"),
              IsError("comparison of distinct pointer types ('void **' and "
                      "'const char *')"));
}

TEST_F(EvalTest, PointerPointerComparison) {
  EXPECT_THAT(Eval("p_void == p_void"), IsEqual("true"));
  EXPECT_THAT(Eval("p_void == p_char1"), IsEqual("true"));
  EXPECT_THAT(Eval("p_void != p_char1"), IsEqual("false"));
  EXPECT_THAT(Eval("p_void > p_char1"), IsEqual("false"));
  EXPECT_THAT(Eval("p_void >= p_char1"), IsEqual("true"));
  EXPECT_THAT(Eval("p_void < (p_char1 + 1)"), IsEqual("true"));
  EXPECT_THAT(Eval("pp_void0 + 1 == pp_void1"), IsEqual("true"));

  EXPECT_THAT(Eval("(void*)1 == (void*)1"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)1 != (void*)1"), IsEqual("false"));
  EXPECT_THAT(Eval("(void*)2 > (void*)1"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)2 < (void*)1"), IsEqual("false"));

  EXPECT_THAT(Eval("(void*)1 == (char*)1"), IsEqual("true"));
  EXPECT_THAT(Eval("(char*)1 != (void*)1"), IsEqual("false"));
  EXPECT_THAT(Eval("(void*)2 > (char*)1"), IsEqual("true"));
  EXPECT_THAT(Eval("(char*)2 < (void*)1"), IsEqual("false"));
}

TEST_F(EvalTest, PointerIntegerComparison) {
  EXPECT_THAT(Eval("(void*)0 == 0"), IsEqual("true"));
  EXPECT_THAT(Eval("0 != (void*)0"), IsEqual("false"));

  EXPECT_THAT(Eval("(void*)0 == nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)0 != nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("nullptr == (void*)1"), IsEqual("false"));
  EXPECT_THAT(Eval("nullptr != (void*)1"), IsEqual("true"));

  EXPECT_THAT(Eval("nullptr == nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("nullptr != nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("nullptr == 0"), IsEqual("true"));
  EXPECT_THAT(Eval("0 != nullptr"), IsEqual("false"));

  EXPECT_THAT(
      Eval("(void*)0 > nullptr"),
      IsError(
          "invalid operands to binary expression ('void *' and 'nullptr_t')"));

  // These are not allowed by C++, but we support it as an extension.
  EXPECT_THAT(Eval("(void*)1 == 1"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)1 == 0"), IsEqual("false"));
  EXPECT_THAT(Eval("(void*)1 > 0"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)1 < 0"), IsEqual("false"));
  EXPECT_THAT(Eval("1 > (void*)0"), IsEqual("true"));
  EXPECT_THAT(Eval("2 < (void*)3"), IsEqual("true"));

  // Integer is converted to uintptr_t, so negative numbers because large
  // positive numbers.
  EXPECT_THAT(Eval("(void*)0xffffffffffffffff == -1"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)-1 == -1"), IsEqual("true"));
  EXPECT_THAT(Eval("(void*)1 > -1"), IsEqual("false"));
}

TEST_F(EvalTest, TestLogicalOperators) {
  EXPECT_THAT(Eval("1 > 2"), IsEqual("false"));
  EXPECT_THAT(Eval("1 == 1"), IsEqual("true"));
  EXPECT_THAT(Eval("1 > 0.1"), IsEqual("true"));
  EXPECT_THAT(Eval("1 && 2"), IsEqual("true"));
  EXPECT_THAT(Eval("0 && 1"), IsEqual("false"));
  EXPECT_THAT(Eval("0 || 1"), IsEqual("true"));
  EXPECT_THAT(Eval("0 || 0"), IsEqual("false"));

  EXPECT_THAT(Eval("!1"), IsEqual("false"));
  EXPECT_THAT(Eval("!!1"), IsEqual("true"));

  EXPECT_THAT(Eval("!trueVar"), IsEqual("false"));
  EXPECT_THAT(Eval("!!trueVar"), IsEqual("true"));
  EXPECT_THAT(Eval("!falseVar"), IsEqual("true"));
  EXPECT_THAT(Eval("!!falseVar"), IsEqual("false"));

  EXPECT_THAT(Eval("trueVar && true"), IsEqual("true"));
  EXPECT_THAT(Eval("trueVar && (2 > 1)"), IsEqual("true"));
  EXPECT_THAT(Eval("trueVar && (2 < 1)"), IsEqual("false"));

  EXPECT_THAT(Eval("falseVar || true"), IsEqual("true"));
  EXPECT_THAT(Eval("falseVar && true"), IsEqual("false"));
  EXPECT_THAT(Eval("falseVar || (2 > 1)"), IsEqual("true"));
  EXPECT_THAT(Eval("falseVar || (2 < 1)"), IsEqual("false"));

  EXPECT_THAT(Eval("true || __doesnt_exist"),
              IsError("use of undeclared identifier '__doesnt_exist'"));
  EXPECT_THAT(Eval("false && __doesnt_exist"),
              IsError("use of undeclared identifier '__doesnt_exist'"));

  EXPECT_THAT(Eval("!p_ptr"), IsEqual("false"));
  EXPECT_THAT(Eval("!!p_ptr"), IsEqual("true"));
  EXPECT_THAT(Eval("p_ptr && true"), IsEqual("true"));
  EXPECT_THAT(Eval("p_ptr && false"), IsEqual("false"));
  EXPECT_THAT(Eval("!p_nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("!!p_nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("p_nullptr || true"), IsEqual("true"));
  EXPECT_THAT(Eval("p_nullptr || false"), IsEqual("false"));

  EXPECT_THAT(
      Eval("!s || false"),
      IsError("value of type 'S' is not contextually convertible to 'bool'"));
  EXPECT_THAT(
      Eval("s || false"),
      IsError("value of type 'S' is not contextually convertible to 'bool'"));
  EXPECT_THAT(
      Eval("s ? 1 : 2"),
      IsError("value of type 'S' is not contextually convertible to 'bool'"));
}

TEST_F(EvalTest, TestLocalVariables) {
  EXPECT_THAT(Eval("a"), IsEqual("1"));
  EXPECT_THAT(Eval("b"), IsEqual("2"));
  EXPECT_THAT(Eval("a + b"), IsEqual("3"));

  EXPECT_THAT(Eval("c + 1"), IsEqual("-2"));
  EXPECT_THAT(Eval("s + 1"), IsEqual("5"));
  EXPECT_THAT(Eval("c + s"), IsEqual("1"));

  EXPECT_THAT(Eval("__test_non_variable + 1"),
              IsError("use of undeclared identifier '__test_non_variable'"));
}

TEST_F(EvalTest, TestMemberOf) {
  EXPECT_THAT(Eval("s.x"), IsEqual("1"));
  EXPECT_THAT(Eval("s.r"), IsEqual("2"));
  EXPECT_THAT(Eval("s.r + 1"), IsEqual("3"));
  EXPECT_THAT(Eval("sr.x"), IsEqual("1"));
  EXPECT_THAT(Eval("sr.r"), IsEqual("2"));
  EXPECT_THAT(Eval("sr.r + 1"), IsEqual("3"));
  EXPECT_THAT(Eval("sp->x"), IsEqual("1"));
  EXPECT_THAT(Eval("sp->r"), IsEqual("2"));
  EXPECT_THAT(Eval("sp->r + 1"), IsEqual("3"));

  EXPECT_THAT(
      Eval("sp->4"),
      IsError(
          "<expr>:1:5: expected 'identifier', got: <'4' (numeric_constant)>\n"
          "sp->4\n"
          "    ^"));
  EXPECT_THAT(Eval("sp->foo"), IsError("no member named 'foo' in 'S'"));
  EXPECT_THAT(
      Eval("sp->r / (void*)0"),
      IsError("invalid operands to binary expression ('int' and 'void *')"));
}

TEST_F(EvalTest, TestInstanceVariables) {
  EXPECT_THAT(Eval("this->field_"), IsEqual("1"));
  EXPECT_THAT(Eval("this.field_"),
              IsError("member reference type 'TestMethods *' is a pointer; did "
                      "you mean to use '->'?"));

  EXPECT_THAT(Eval("c.field_"), IsEqual("-1"));
  EXPECT_THAT(Eval("c_ref.field_"), IsEqual("-1"));
  EXPECT_THAT(Eval("c_ptr->field_"), IsEqual("-1"));
  EXPECT_THAT(Eval("c->field_"), IsError("member reference type 'C' is not a "
                                         "pointer; did you mean to use '.'?"));
}

TEST_F(EvalTest, TestIndirection) {
  EXPECT_THAT(Eval("*p"), IsEqual("1"));
  EXPECT_THAT(Eval("p"), IsOk());
  EXPECT_THAT(Eval("*my_p"), IsEqual("1"));
  EXPECT_THAT(Eval("my_p"), IsOk());
  EXPECT_THAT(Eval("*my_pr"), IsEqual("1"));
  EXPECT_THAT(Eval("my_pr"), IsOk());

  EXPECT_THAT(Eval("*1"),
              IsError("indirection requires pointer operand. ('int' invalid)"));
  EXPECT_THAT(Eval("*val"),
              IsError("indirection requires pointer operand. ('int' invalid)"));
}

TEST_F(EvalTest, TestAddressOf) {
  EXPECT_THAT(Eval("&x"), IsOk());
  EXPECT_THAT(Eval("r"), IsOk());
  EXPECT_THAT(Eval("&r"), IsOk());
  EXPECT_THAT(Eval("pr"), IsOk());
  EXPECT_THAT(Eval("&pr"), IsOk());
  EXPECT_THAT(Eval("my_pr"), IsOk());
  EXPECT_THAT(Eval("&my_pr"), IsOk());

  EXPECT_THAT(Eval("&x == &r"), IsEqual("true"));
  EXPECT_THAT(Eval("&x != &r"), IsEqual("false"));

  EXPECT_THAT(Eval("&p == &pr"), IsEqual("true"));
  EXPECT_THAT(Eval("&p != &pr"), IsEqual("false"));
  EXPECT_THAT(Eval("&p == &my_pr"), IsEqual("true"));
  EXPECT_THAT(Eval("&p != &my_pr"), IsEqual("false"));

  EXPECT_THAT(Eval("&globalVar"), IsOk());
  EXPECT_THAT(Eval("&externGlobalVar"), IsOk());
  EXPECT_THAT(Eval("&s_str"), IsOk());
  EXPECT_THAT(Eval("&param"), IsOk());

  EXPECT_THAT(Eval("&1"),
              IsError("cannot take the address of an rvalue of type 'int'"));
  EXPECT_THAT(Eval("&0.1"),
              IsError("cannot take the address of an rvalue of type 'double'"));

  EXPECT_THAT(
      Eval("&this"),
      IsError("cannot take the address of an rvalue of type 'TestMethods *'"));
  EXPECT_THAT(
      Eval("&(&s_str)"),
      IsError("cannot take the address of an rvalue of type 'const char **'"));
}

TEST_F(EvalTest, TestSubscript) {
  // const char*
  EXPECT_THAT(Eval("char_ptr[0]"), IsEqual("'l'"));
  EXPECT_THAT(Eval("1[char_ptr]"), IsEqual("'o'"));

  // const char[]
  EXPECT_THAT(Eval("char_arr[0]"), IsEqual("'i'"));
  EXPECT_THAT(Eval("1[char_arr]"), IsEqual("'p'"));

  // Boolean types are integral too!
  EXPECT_THAT(Eval("int_arr[false]"), IsEqual("1"));
  EXPECT_THAT(Eval("true[int_arr]"), IsEqual("2"));

  // But floats are not.
  EXPECT_THAT(Eval("int_arr[1.0]"),
              IsError("array subscript is not an integer"));

  // Base should be a "pointer to T" and index should be of an integral type.
  EXPECT_THAT(Eval("char_arr[char_ptr]"),
              IsError("array subscript is not an integer"));
  EXPECT_THAT(Eval("1[2]"),
              IsError("subscripted value is not an array or pointer"));

  // Test when base and index are references.
  EXPECT_THAT(Eval("c_arr[0].field_"), IsEqual("0"));
  EXPECT_THAT(Eval("c_arr[idx_1_ref].field_"), IsEqual("1"));
  EXPECT_THAT(Eval("c_arr_ref[0].field_"), IsEqual("0"));
  EXPECT_THAT(Eval("c_arr_ref[idx_1_ref].field_"), IsEqual("1"));

  // Test when base and index are typedefs.
  EXPECT_THAT(Eval("td_int_arr[0]"), IsEqual("1"));
  EXPECT_THAT(Eval("td_int_arr[td_int_idx_1]"), IsEqual("2"));
  EXPECT_THAT(Eval("td_int_arr[td_td_int_idx_2]"), IsEqual("3"));
  EXPECT_THAT(Eval("td_int_ptr[0]"), IsEqual("1"));
  EXPECT_THAT(Eval("td_int_ptr[td_int_idx_1]"), IsEqual("2"));
  EXPECT_THAT(Eval("td_int_ptr[td_td_int_idx_2]"), IsEqual("3"));
  // Both typedefs and refs!
  EXPECT_THAT(Eval("td_int_arr_ref[td_int_idx_1_ref]"), IsEqual("2"));

  // Test for index out of bounds.
  EXPECT_THAT(Eval("int_arr[42]"), IsOk());
  EXPECT_THAT(Eval("int_arr[100]"), IsOk());

  // Test for negative index.
  EXPECT_THAT(Eval("int_arr[-1]"), IsOk());
  EXPECT_THAT(Eval("int_arr[-42]"), IsOk());

  // Test for "max unsigned char".
  EXPECT_THAT(Eval("uint8_arr[uchar_idx]"), IsEqual("'\\xab'"));

  // Test address-of of the subscripted value.
  EXPECT_THAT(Eval("(&c_arr[1])->field_"), IsEqual("1"));
}

TEST_F(EvalTest, TestCStyleCastBasicType) {
  // Test with integer literals.
  EXPECT_THAT(Eval("(char)1"), IsEqual("'\\x01'"));
  EXPECT_THAT(Eval("(unsigned char)-1"), IsEqual("'\\xff'"));
  EXPECT_THAT(Eval("(short)-1"), IsEqual("-1"));
  EXPECT_THAT(Eval("(unsigned short)-1"), IsEqual("65535"));
  EXPECT_THAT(Eval("(long long)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(unsigned long long)-1"), IsEqual("18446744073709551615"));
  EXPECT_THAT(Eval("(short)65534"), IsEqual("-2"));
  EXPECT_THAT(Eval("(unsigned short)100000"), IsEqual("34464"));
  EXPECT_THAT(Eval("(float)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(float)1.1"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(float)1.1f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(float)-1.1"), IsEqual("-1.10000002"));
  EXPECT_THAT(Eval("(float)-1.1f"), IsEqual("-1.10000002"));
  EXPECT_THAT(Eval("(double)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(double)1.1"), IsEqual("1.1000000000000001"));
  EXPECT_THAT(Eval("(double)1.1f"), IsEqual("1.1000000238418579"));
  EXPECT_THAT(Eval("(double)-1.1"), IsEqual("-1.1000000000000001"));
  EXPECT_THAT(Eval("(double)-1.1f"), IsEqual("-1.1000000238418579"));
  EXPECT_THAT(Eval("(int)1.1"), IsEqual("1"));
  EXPECT_THAT(Eval("(int)1.1f"), IsEqual("1"));
  EXPECT_THAT(Eval("(int)-1.1"), IsEqual("-1"));
  EXPECT_THAT(Eval("(long)1.1"), IsEqual("1"));
  EXPECT_THAT(Eval("(long)-1.1f"), IsEqual("-1"));

  EXPECT_THAT(Eval("&(int)1"),
              IsError("cannot take the address of an rvalue of type 'int'"));

  // Test with variables.
  EXPECT_THAT(Eval("(char)a"), IsEqual("'\\x01'"));
  EXPECT_THAT(Eval("(unsigned char)na"), IsEqual("'\\xff'"));
  EXPECT_THAT(Eval("(short)na"), IsEqual("-1"));
  EXPECT_THAT(Eval("(unsigned short)-a"), IsEqual("65535"));
  EXPECT_THAT(Eval("(long long)a"), IsEqual("1"));
  EXPECT_THAT(Eval("(unsigned long long)-1"), IsEqual("18446744073709551615"));
  EXPECT_THAT(Eval("(float)a"), IsEqual("1"));
  EXPECT_THAT(Eval("(float)f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(double)f"), IsEqual("1.1000000238418579"));
  EXPECT_THAT(Eval("(int)f"), IsEqual("1"));
  EXPECT_THAT(Eval("(long)f"), IsEqual("1"));

  EXPECT_THAT(
      Eval("(int)ns_foo_"),
      IsError(
          "cannot convert 'ns::Foo' to 'int' without a conversion operator"));

  // Test with typedefs and namespaces.
  EXPECT_THAT(Eval("(myint)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(myint)1LL"), IsEqual("1"));
  EXPECT_THAT(Eval("(ns::myint)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(::ns::myint)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(::ns::myint)myint_"), IsEqual("1"));

  EXPECT_THAT(Eval("(int)myint_"), IsEqual("1"));
  EXPECT_THAT(Eval("(int)ns_myint_"), IsEqual("2"));
  EXPECT_THAT(Eval("(long long)myint_"), IsEqual("1"));
  EXPECT_THAT(Eval("(long long)ns_myint_"), IsEqual("2"));
  EXPECT_THAT(Eval("(::ns::myint)myint_"), IsEqual("1"));

  EXPECT_THAT(Eval("(ns::inner::mydouble)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(::ns::inner::mydouble)1.2"), IsEqual("1.2"));
  EXPECT_THAT(Eval("(ns::inner::mydouble)myint_"), IsEqual("1"));
  EXPECT_THAT(Eval("(::ns::inner::mydouble)ns_inner_mydouble_"),
              IsEqual("1.2"));
  EXPECT_THAT(Eval("(myint)ns_inner_mydouble_"), IsEqual("1"));

  // Test with pointers.
  EXPECT_THAT(Eval("(long long)ap"), IsOk());
  EXPECT_THAT(Eval("(unsigned long long)vp"), IsOk());
  EXPECT_THAT(
      Eval("(char)ap"),
      IsError("cast from pointer to smaller type 'char' loses information"));
  EXPECT_THAT(Eval("(float)ap"),
              IsError("C-style cast from 'int *' to 'float' is not allowed"));
}

TEST_F(EvalTest, TestCStyleCastPointer) {
  EXPECT_THAT(Eval("(void*)&a"), IsOk());
  EXPECT_THAT(Eval("(void*)ap"), IsOk());
  EXPECT_THAT(Eval("(long long*)vp"), IsOk());
  EXPECT_THAT(Eval("(short int*)vp"), IsOk());
  EXPECT_THAT(Eval("(unsigned long long*)vp"), IsOk());
  EXPECT_THAT(Eval("(unsigned short int*)vp"), IsOk());

  EXPECT_THAT(Eval("(void*)0"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("(void*)1"), IsEqual("0x0000000000000001"));
  EXPECT_THAT(Eval("(void*)a"), IsEqual("0x0000000000000001"));
  EXPECT_THAT(Eval("(void*)na"), IsEqual("0xffffffffffffffff"));
  EXPECT_THAT(Eval("(int*&)ap"), IsOk());

  EXPECT_THAT(
      Eval("(char*) 1.0"),
      IsError("cannot cast from type 'double' to pointer type 'char *'"));

  EXPECT_THAT(Eval("*(const int* const)ap"), IsEqual("1"));
  EXPECT_THAT(Eval("*(volatile int* const)ap"), IsEqual("1"));
  EXPECT_THAT(Eval("*(const int* const)vp"), IsEqual("1"));
  EXPECT_THAT(Eval("*(const int* const volatile const)vp"), IsEqual("1"));
  EXPECT_THAT(Eval("*(int*)(void*)ap"), IsEqual("1"));
  EXPECT_THAT(Eval("*(int*)(const void* const volatile)ap"), IsEqual("1"));

  EXPECT_THAT(Eval("(ns::Foo*)ns_inner_foo_ptr_"), IsOk());
  EXPECT_THAT(Eval("(ns::inner::Foo*)ns_foo_ptr_"), IsOk());

  EXPECT_THAT(Eval("(int& &)ap"),
              IsError("type name declared as a reference to a reference"));
  EXPECT_THAT(Eval("(int&*)ap"), IsError("'type name' declared as a pointer "
                                         "to a reference of type 'int &'"));
}

TEST_F(EvalTest, TestCStyleCastReference) {
  EXPECT_THAT(Eval("((InnerFoo&)arr[1]).a"), IsEqual("2"));
  EXPECT_THAT(Eval("((InnerFoo&)arr[1]).b"), IsEqual("3"));

  EXPECT_THAT(Eval("(int&)arr[0]"), IsEqual("1"));
  EXPECT_THAT(Eval("(int&)arr[1]"), IsEqual("2"));
}

TEST_F(EvalTest, TestQualifiedId) {
  EXPECT_THAT(Eval("::ns::i"), IsEqual("1"));
  EXPECT_THAT(Eval("ns::i"), IsEqual("1"));
  EXPECT_THAT(Eval("::ns::ns::i"), IsEqual("2"));
  EXPECT_THAT(Eval("ns::ns::i"), IsEqual("2"));

  EXPECT_THAT(Eval("::Foo::y"), IsEqual("42"));
  EXPECT_THAT(Eval("Foo::y"), IsEqual("42"));

  // Static consts with no definition can't be looked up by name.
  EXPECT_THAT(Eval("::Foo::x"),
              IsError("use of undeclared identifier '::Foo::x'"));
  EXPECT_THAT(Eval("Foo::x"), IsError("use of undeclared identifier 'Foo::x'"));
}

TEST_F(EvalTest, TestTemplateTypes) {
  // Template types lookup doesn't work well in the upstream LLDB.
  this->compare_with_lldb_ = false;

  // Get the pointer value and use it to check the expressions with lldb-eval.
  auto expected = frame_.EvaluateExpression("p").GetValue();

  for (std::string arg : {"int", "int*", "int**", "int&", "int*&", "double"}) {
    EXPECT_THAT(Eval("(T_1<" + arg + ">*)p"), IsEqual(expected));
    EXPECT_THAT(Eval("(::T_1<" + arg + ">*)p"), IsEqual(expected));
  }
  EXPECT_THAT(Eval("(T_2<int, char>*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(::T_2<int, char>*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(T_2<char, int>*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(::T_2<char, int>*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(T_2<T_1<int>, T_1<char> >*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(::T_2<T_1<int>, T_1<char> >*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(T_2<T_1<T_1<int> >, T_1<char> >*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(::T_2<T_1<T_1<int> >, T_1<char> >*)p"), IsEqual(expected));

  EXPECT_THAT(Eval("(ns::T_1<int>*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(::ns::T_1<int>*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(ns::T_1<ns::T_1<int> >*)p"), IsEqual(expected));
  EXPECT_THAT(Eval("(::ns::T_1<ns::T_1<int> >*)p"), IsEqual(expected));
#ifdef _WIN32
  EXPECT_THAT(
      Eval("ns::T_1<ns::T_1<int> >::cx"),
      IsError("use of undeclared identifier 'ns::T_1<ns::T_1<int> >::cx'"));
#else
  EXPECT_THAT(Eval("ns::T_1<ns::T_1<int> >::cx"), IsEqual("46"));
#endif

  EXPECT_THAT(Eval("T_1<int>::cx"), IsEqual("24"));
  EXPECT_THAT(Eval("T_1<double>::cx"), IsEqual("42"));
  EXPECT_THAT(Eval("ns::T_1<int>::cx"), IsEqual("64"));

  for (std::string arg : {"int", "int*", "int**", "int&", "int*&"}) {
    EXPECT_THAT(Eval("(T_1<" + arg + ">::myint)1.2"), IsEqual("1.2"));
    EXPECT_THAT(Eval("(::T_1<" + arg + ">::myint)1.2"), IsEqual("1.2"));
    EXPECT_THAT(Eval("(T_1<T_1<" + arg + "> >::myint)1.2"), IsEqual("1.2"));
    EXPECT_THAT(Eval("(::T_1<T_1<" + arg + "> >::myint)1.2"), IsEqual("1.2"));

    EXPECT_THAT(Eval("(ns::T_1<" + arg + ">::myint)1.1"), IsEqual("1"));
    EXPECT_THAT(Eval("(::ns::T_1<" + arg + ">::myint)1.1"), IsEqual("1"));
    EXPECT_THAT(Eval("(ns::T_1<T_1<" + arg + "> >::myint)1.1"), IsEqual("1"));
    EXPECT_THAT(Eval("(::ns::T_1<T_1<" + arg + "> >::myint)1.1"), IsEqual("1"));
    EXPECT_THAT(Eval("(ns::T_1<ns::T_1<" + arg + "> >::myint)1.1"),
                IsEqual("1"));
    EXPECT_THAT(Eval("(::ns::T_1<ns::T_1<" + arg + "> >::myint)1.1"),
                IsEqual("1"));
  }

  EXPECT_THAT(Eval("(T_2<int, char>::myint)1.1f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(::T_2<int, char>::myint)1.1f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(T_2<int*, char&>::myint)1.1f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(::T_2<int&, char*>::myint)1.1f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(T_2<T_1<T_1<int> >, T_1<char> >::myint)1.1"),
              IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(::T_2<T_1<T_1<int> >, T_1<char> >::myint)1.1"),
              IsEqual("1.10000002"));
}

TEST_F(EvalTest, TestValueScope) {
  EXPECT_THAT(Scope("var").Eval("x_"), IsEqual("1"));
  EXPECT_THAT(Scope("var").Eval("y_"), IsEqual("2.5"));
  EXPECT_THAT(Scope("var").Eval("z_"),
              IsError("use of undeclared identifier 'z_'"));

  EXPECT_THAT(Eval("x_"), IsError("use of undeclared identifier 'x_'"));
  EXPECT_THAT(Eval("y_"), IsError("use of undeclared identifier 'y_'"));
  EXPECT_THAT(Eval("z_"), IsEqual("3"));
}
