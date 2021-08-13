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
#include <unordered_map>

#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb-eval/traits.h"
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
  EvaluatorHelper(lldb::SBFrame frame, bool lldb, bool side_effects)
      : frame_(frame), lldb_(lldb), side_effects_(side_effects) {}
  EvaluatorHelper(lldb::SBValue scope, bool lldb, bool side_effects)
      : scope_(scope), lldb_(lldb), side_effects_(side_effects) {}

 public:
  EvalResult Eval(const std::string& expr) {
    EvalResult ret;

    lldb_eval::Options opts;
    opts.allow_side_effects = side_effects_;

    if (scope_) {
      // Evaluate in the variable context.
      ret.lldb_eval_value = lldb_eval::EvaluateExpression(
          scope_, expr.c_str(), opts, ret.lldb_eval_error);

      if (lldb_) {
        ret.lldb_value = scope_.EvaluateExpression(expr.c_str());
      }

    } else {
      // Evaluate in the frame context.
      ret.lldb_eval_value = lldb_eval::EvaluateExpression(
          frame_, expr.c_str(), opts, ret.lldb_eval_error);

      if (lldb_) {
        ret.lldb_value = frame_.EvaluateExpression(expr.c_str());
      }
    }

    return ret;
  }

  EvalResult EvalWithContext(
      const std::string& expr,
      const std::unordered_map<std::string, lldb::SBValue>& vars) {
    EvalResult ret;

    std::vector<lldb_eval::ContextVariable> ctx_vec;
    ctx_vec.reserve(vars.size());
    for (const auto& [name, value] : vars) {
      ctx_vec.push_back({name.c_str(), value});
    }

    lldb_eval::Options opts;
    opts.allow_side_effects = side_effects_;
    opts.context_vars = {ctx_vec.data(), ctx_vec.size()};

    if (scope_) {
      // Evaluate in the variable context.
      ret.lldb_eval_value = lldb_eval::EvaluateExpression(
          scope_, expr.c_str(), opts, ret.lldb_eval_error);

      if (lldb_) {
        ret.lldb_value = scope_.EvaluateExpression(expr.c_str());
      }
    } else {
      // Evaluate in the frame context.
      ret.lldb_eval_value = lldb_eval::EvaluateExpression(
          frame_, expr.c_str(), opts, ret.lldb_eval_error);

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
  bool side_effects_;
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
    if (result.lldb_value.has_value()) {
      if (result.lldb_value.value().GetError().GetError()) {
        *listener << "values produced by lldb-eval and LLDB don't match\n"
                  << "lldb-eval: " << actual << "\n"
                  << "lldb     : "
                  << result.lldb_value.value().GetError().GetCString();
        return false;

      } else if (actual != result.lldb_value.value().GetValue()) {
        *listener << "values produced by lldb-eval and LLDB don't match\n"
                  << "lldb-eval: " << actual << "\n"
                  << "lldb     : " << result.lldb_value.value().GetValue();
        return false;
      }
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
    if (result.lldb_value.has_value()) {
      if (result.lldb_value.value().GetError().GetError()) {
        *listener << "values produced by lldb-eval and LLDB don't match\n"
                  << "lldb-eval: " << actual << "\n"
                  << "lldb     : "
                  << result.lldb_value.value().GetError().GetCString();
        return false;

      } else if (actual != result.lldb_value.value().GetValue()) {
        *listener << "values produced by lldb-eval and LLDB don't match\n"
                  << "lldb-eval: " << actual << "\n"
                  << "lldb     : " << result.lldb_value.value().GetValue();
        return false;
      }
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

  EvalResult Eval(const std::string& expr) {
    return EvaluatorHelper(frame_, compare_with_lldb_, allow_side_effects_)
        .Eval(expr);
  }

  EvalResult EvalWithContext(
      const std::string& expr,
      const std::unordered_map<std::string, lldb::SBValue>& vars) {
    return EvaluatorHelper(frame_, compare_with_lldb_, allow_side_effects_)
        .EvalWithContext(expr, vars);
  }

  EvaluatorHelper Scope(std::string scope) {
    // Resolve the scope variable (assume it's a local variable).
    lldb::SBValue scope_var = frame_.FindVariable(scope.c_str());
    return EvaluatorHelper(scope_var, compare_with_lldb_, allow_side_effects_);
  }

  bool CreateContextVariable(std::string type, std::string name, bool is_array,
                             std::string assignment) {
    std::string expr = type + " " + name + (is_array ? "[]" : "") + " = " +
                       assignment + "; " + name;
    lldb::SBValue value = frame_.EvaluateExpression(expr.c_str());
    if (value.GetError().Fail()) {
      return false;
    }
    vars_.emplace(name, value);
    return true;
  }

  bool CreateContextVariable(std::string name, std::string assignment) {
    return CreateContextVariable("auto", name, false, assignment);
  }

  bool CreateContextVariableArray(std::string type, std::string name,
                                  std::string assignment) {
    return CreateContextVariable(type, name, true, assignment);
  }

 protected:
  lldb::SBDebugger debugger_;
  lldb::SBProcess process_;
  lldb::SBFrame frame_;

  // Evaluate with both lldb-eval and LLDB by default.
  bool compare_with_lldb_ = true;

  // Allow the expressions to have side-effects.
  bool allow_side_effects_ = false;

  // Context variables.
  std::unordered_map<std::string, lldb::SBValue> vars_;

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

  // Invalid remainder.
  EXPECT_THAT(
      Eval("1.1 % 2"),
      IsError("invalid operands to binary expression ('double' and 'int')"));

  // References and typedefs.
  EXPECT_THAT(Eval("r + 1"), IsEqual("3"));
  EXPECT_THAT(Eval("r - 1l"), IsEqual("1"));
  EXPECT_THAT(Eval("r * 2u"), IsEqual("4"));
  EXPECT_THAT(Eval("r / 2ull"), IsEqual("1"));
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
  EXPECT_THAT(Eval("1ll / 0 + 1"), IsEqual("1"));

  EXPECT_THAT(Eval("1 % 0"), IsEqual("0"));
  EXPECT_THAT(Eval("1 % uint_zero"), IsEqual("0"));
  EXPECT_THAT(Eval("1 % uint_zero + 1"), IsEqual("1"));
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
  EXPECT_THAT(Eval("(-1 >> 10)"), IsEqual("-1"));
  EXPECT_THAT(Eval("(-100 >> 5)"), IsEqual("-4"));
  EXPECT_THAT(Eval("(-3 << 6)"), IsEqual("-192"));
  EXPECT_THAT(Eval("(2000000000U << 1)"), IsEqual("4000000000"));
  EXPECT_THAT(Eval("(-1 >> 1U)"), IsEqual("-1"));
  EXPECT_THAT(Eval("(char)1 << 16"), IsEqual("65536"));
  EXPECT_THAT(Eval("(signed char)-123 >> 8"), IsEqual("-1"));

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

  EXPECT_THAT(Eval("+array"), IsOk());
  EXPECT_THAT(Eval("+array_ref"), IsOk());
  EXPECT_THAT(Eval("-array"),
              IsError("invalid argument type 'int *' to unary expression\n"
                      "-array\n"
                      "^"));

  EXPECT_THAT(Eval("array + 1"), IsOk());
  EXPECT_THAT(Eval("1 + array"), IsOk());
  EXPECT_THAT(Eval("array_ref + 1"), IsOk());
  EXPECT_THAT(Eval("1 + array_ref"), IsOk());

  EXPECT_THAT(Eval("array - 1"), IsOk());
  EXPECT_THAT(Eval("array_ref - 1"), IsOk());
  EXPECT_THAT(
      Eval("1 - array"),
      IsError("invalid operands to binary expression ('int' and 'int [10]')\n"
              "1 - array\n"
              "  ^"));

  EXPECT_THAT(Eval("array - array"), IsEqual("0"));
  EXPECT_THAT(Eval("array - array_ref"), IsEqual("0"));
  EXPECT_THAT(Eval("array_ref - array_ref"), IsEqual("0"));
  EXPECT_THAT(
      Eval("array + array"),
      IsError(
          "invalid operands to binary expression ('int [10]' and 'int [10]')\n"
          "array + array\n"
          "      ^"));
}

TEST_F(EvalTest, PointerPointerArithmeticFloat) {
  EXPECT_THAT(
      Eval("(int*)0 + 1.1"),
      IsError("invalid operands to binary expression ('int *' and 'double')"));
  EXPECT_THAT(
      Eval("1.1 + (int*)0"),
      IsError("invalid operands to binary expression ('double' and 'int *')"));
  EXPECT_THAT(
      Eval("(int*)0 - 1.1"),
      IsError("invalid operands to binary expression ('int *' and 'double')"));
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
  EXPECT_THAT(Eval("nullptr == 0U"), IsEqual("true"));
  EXPECT_THAT(Eval("0L != nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("nullptr == 0UL"), IsEqual("true"));
  EXPECT_THAT(Eval("0ULL != nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("nullptr == 0x0"), IsEqual("true"));
  EXPECT_THAT(Eval("0b0 != nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("nullptr == 00"), IsEqual("true"));
  EXPECT_THAT(Eval("0x0LLU != nullptr"), IsEqual("false"));

  EXPECT_THAT(Eval("0 == std_nullptr_t"), IsEqual("true"));
  EXPECT_THAT(Eval("std_nullptr_t != 0"), IsEqual("false"));

  EXPECT_THAT(
      Eval("(void*)0 > nullptr"),
      IsError(
          "invalid operands to binary expression ('void *' and 'nullptr_t')"));

  EXPECT_THAT(
      Eval("nullptr > 0"),
      IsError("invalid operands to binary expression ('nullptr_t' and 'int')"));

  EXPECT_THAT(
      Eval("1 == nullptr"),
      IsError("invalid operands to binary expression ('int' and 'nullptr_t')"));

  EXPECT_THAT(
      Eval("nullptr == (int)0"),
      IsError("invalid operands to binary expression ('nullptr_t' and 'int')"));

  EXPECT_THAT(
      Eval("false == nullptr"),
      IsError(
          "invalid operands to binary expression ('bool' and 'nullptr_t')"));

  EXPECT_THAT(
      Eval("nullptr == (true ? 0 : 0)"),
      IsError("invalid operands to binary expression ('nullptr_t' and 'int')"));

  // TODO: Enable when we support char literals.
  // EXPECT_THAT(
  //     Eval("'\0' == nullptr"),
  //     IsError(
  //         "invalid operands to binary expression ('char' and 'nullptr_t')"));

  EXPECT_THAT(Eval("nullptr > nullptr"),
              IsError("invalid operands to binary expression ('nullptr_t' and "
                      "'nullptr_t')"));

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

TEST_F(EvalTest, TestPointerDereference) {
  EXPECT_THAT(Eval("*p_int0"), IsEqual("0"));
  EXPECT_THAT(Eval("*p_int0 + 1"), IsEqual("1"));
  EXPECT_THAT(Eval("*cp_int5"), IsEqual("5"));
  EXPECT_THAT(Eval("*cp_int5 - 1"), IsEqual("4"));

  EXPECT_THAT(Eval("&*p_null"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("&p_null[4]"), IsEqual("0x0000000000000010"));
  EXPECT_THAT(Eval("&*(int*)0"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("&((int*)0)[1]"), IsEqual("0x0000000000000004"));

  EXPECT_THAT(Eval("&p_void[0]"),
              IsError("subscript of pointer to incomplete type 'void'"));
  EXPECT_THAT(Eval("&*p_void"), IsOk());
  EXPECT_THAT(Eval("&pp_void0[2]"), IsOk());

  EXPECT_THAT(Eval("**pp_int0"), IsEqual("0"));
  EXPECT_THAT(Eval("**pp_int0 + 1"), IsEqual("1"));
  EXPECT_THAT(Eval("&**pp_int0"), IsOk());
  EXPECT_THAT(Eval("&**pp_int0 + 1"), IsOk());

  EXPECT_THAT(Eval("&(true ? *p_null : *p_null)"),
              IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("&(false ? *p_null : *p_null)"),
              IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("&*(true ? p_null : nullptr)"),
              IsEqual("0x0000000000000000"));
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

  EXPECT_THAT(Eval("!array"), IsEqual("false"));
  EXPECT_THAT(Eval("!!array"), IsEqual("true"));
  EXPECT_THAT(Eval("array || true"), IsEqual("true"));
  EXPECT_THAT(Eval("false || array"), IsEqual("true"));
  EXPECT_THAT(Eval("array && true"), IsEqual("true"));
  EXPECT_THAT(Eval("array && false"), IsEqual("false"));

  EXPECT_THAT(Eval("false || !s"),
              IsError("invalid argument type 'S' to unary expression\n"
                      "false || !s\n"
                      "         ^"));
  EXPECT_THAT(
      Eval("s || false"),
      IsError("value of type 'S' is not contextually convertible to 'bool'\n"
              "s || false\n"
              "^"));
  EXPECT_THAT(
      Eval("true || s"),
      IsError("value of type 'S' is not contextually convertible to 'bool'\n"
              "true || s\n"
              "        ^"));
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
  EXPECT_THAT(Eval("sarr->x"), IsEqual("5"));
  EXPECT_THAT(Eval("sarr->r"), IsEqual("2"));
  EXPECT_THAT(Eval("sarr->r + 1"), IsEqual("3"));
  EXPECT_THAT(Eval("(sarr + 1)->x"), IsEqual("1"));

  EXPECT_THAT(
      Eval("sp->4"),
      IsError(
          "<expr>:1:5: expected 'identifier', got: <'4' (numeric_constant)>\n"
          "sp->4\n"
          "    ^"));
  EXPECT_THAT(Eval("sp->foo"), IsError("no member named 'foo' in 'Sx'"));
  EXPECT_THAT(
      Eval("sp->r / (void*)0"),
      IsError("invalid operands to binary expression ('int' and 'void *')"));

  EXPECT_THAT(Eval("sp.x"), IsError("member reference type 'Sx *' is a "
                                    "pointer; did you mean to use '->'"));
  EXPECT_THAT(
      Eval("sarr.x"),
      IsError(
          "member reference base type 'Sx [2]' is not a structure or union"));

  // Test for record typedefs.
  EXPECT_THAT(Eval("sa.x"), IsEqual("3"));
  EXPECT_THAT(Eval("sa.y"), IsEqual("'\\x04'"));

  // TODO(werat): Implement address-of-member-or combination.
  // EXPECT_THAT(Eval("&((Sx*)0)->x"), IsEqual("0x0000000000000000"));
  // EXPECT_THAT(Eval("&((Sx*)0)->y"), IsEqual("0x0000000000000010"));
  // EXPECT_THAT(Eval("&(*(Sx*)0).x"), IsEqual("0x0000000000000000"));
  // EXPECT_THAT(Eval("&(*(Sx*)0).y"), IsEqual("0x0000000000000010"));
}

TEST_F(EvalTest, TestMemberOfInheritance) {
  EXPECT_THAT(Eval("a.a_"), IsEqual("1"));
  EXPECT_THAT(Eval("b.b_"), IsEqual("2"));
  EXPECT_THAT(Eval("c.a_"), IsEqual("1"));
  EXPECT_THAT(Eval("c.b_"), IsEqual("2"));
  EXPECT_THAT(Eval("c.c_"), IsEqual("3"));
  EXPECT_THAT(Eval("d.a_"), IsEqual("1"));
  EXPECT_THAT(Eval("d.b_"), IsEqual("2"));
  EXPECT_THAT(Eval("d.c_"), IsEqual("3"));
  EXPECT_THAT(Eval("d.d_"), IsEqual("4"));
  EXPECT_THAT(Eval("d.fa_.a_"), IsEqual("5"));

  EXPECT_THAT(Eval("bat.weight_"), IsEqual("10"));

  EXPECT_THAT(Eval("plugin.x"), IsEqual("1"));
  EXPECT_THAT(Eval("plugin.y"), IsEqual("2"));

  EXPECT_THAT(Eval("engine.x"), IsEqual("1"));
  EXPECT_THAT(Eval("engine.y"), IsEqual("2"));
  EXPECT_THAT(Eval("engine.z"), IsEqual("3"));

  EXPECT_THAT(Eval("parent_base->x"), IsEqual("1"));
  EXPECT_THAT(Eval("parent_base->y"), IsEqual("2"));
  EXPECT_THAT(Eval("parent->x"), IsEqual("1"));
  EXPECT_THAT(Eval("parent->y"), IsEqual("2"));
  EXPECT_THAT(Eval("parent->z"), IsEqual("3"));
}

TEST_F(EvalTest, TestMemberOfAnonymousMember) {
  EXPECT_THAT(Eval("a.x"), IsEqual("1"));
  EXPECT_THAT(Eval("a.y"), IsEqual("2"));

  EXPECT_THAT(Eval("b.x"), IsError("no member named 'x' in 'B'"));
  EXPECT_THAT(Eval("b.y"), IsError("no member named 'y' in 'B'"));
  EXPECT_THAT(Eval("b.z"), IsEqual("3"));
  EXPECT_THAT(Eval("b.w"), IsEqual("4"));
  EXPECT_THAT(Eval("b.a.x"), IsEqual("1"));
  EXPECT_THAT(Eval("b.a.y"), IsEqual("2"));

  EXPECT_THAT(Eval("c.x"), IsEqual("5"));
  EXPECT_THAT(Eval("c.y"), IsEqual("6"));

  EXPECT_THAT(Eval("d.x"), IsEqual("7"));
  EXPECT_THAT(Eval("d.y"), IsEqual("8"));
  EXPECT_THAT(Eval("d.z"), IsEqual("9"));
  EXPECT_THAT(Eval("d.w"), IsEqual("10"));

  EXPECT_THAT(Eval("e.x"), IsError("no member named 'x' in 'E'"));
  EXPECT_THAT(Eval("f.x"), IsError("no member named 'x' in 'F'"));
  EXPECT_THAT(Eval("f.named_field.x"), IsEqual("12"));

  EXPECT_THAT(Eval("unnamed_derived.x"), IsEqual("1"));
  EXPECT_THAT(Eval("unnamed_derived.y"), IsEqual("2"));
  EXPECT_THAT(Eval("unnamed_derived.z"), IsEqual("13"));

  EXPECT_THAT(Eval("derb.x"), IsError("no member named 'x' in 'DerivedB'"));
  EXPECT_THAT(Eval("derb.y"), IsError("no member named 'y' in 'DerivedB'"));
  EXPECT_THAT(Eval("derb.z"), IsEqual("3"));
  EXPECT_THAT(Eval("derb.w"), IsEqual("14"));
  EXPECT_THAT(Eval("derb.k"), IsEqual("15"));
  EXPECT_THAT(Eval("derb.a.x"), IsEqual("1"));
  EXPECT_THAT(Eval("derb.a.y"), IsEqual("2"));
}

TEST_F(EvalTest, TestGlobalVariableLookup) {
  EXPECT_THAT(Eval("globalVar"), IsEqual("-559038737"));  // 0xDEADBEEF
  EXPECT_THAT(Eval("globalPtr"), IsOk());
  EXPECT_THAT(Eval("globalRef"), IsEqual("-559038737"));
  EXPECT_THAT(Eval("::globalPtr"), IsOk());
  EXPECT_THAT(Eval("::globalRef"), IsEqual("-559038737"));

  EXPECT_THAT(Eval("externGlobalVar"), IsEqual("12648430"));  // 0x00C0FFEE
  EXPECT_THAT(Eval("::externGlobalVar"), IsEqual("12648430"));

  EXPECT_THAT(Eval("ns::globalVar"), IsEqual("13"));
  EXPECT_THAT(Eval("ns::globalPtr"), IsOk());
  EXPECT_THAT(Eval("ns::globalRef"), IsEqual("13"));
  EXPECT_THAT(Eval("::ns::globalVar"), IsEqual("13"));
  EXPECT_THAT(Eval("::ns::globalPtr"), IsOk());
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
              IsError("indirection requires pointer operand ('int' invalid)"));
  EXPECT_THAT(Eval("*val"),
              IsError("indirection requires pointer operand ('int' invalid)"));
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

  EXPECT_THAT(Eval("&(true ? x : x)"), IsOk());
  EXPECT_THAT(Eval("&(true ? 1 : 1)"),
              IsError("cannot take the address of an rvalue of type 'int'"));

  EXPECT_THAT(Eval("&(true ? c : c)"), IsOk());
  EXPECT_THAT(Eval("&(true ? c : (char)1)"),
              IsError("cannot take the address of an rvalue of type 'char'"));
  EXPECT_THAT(Eval("&(true ? c : 1)"),
              IsError("cannot take the address of an rvalue of type 'int'"));
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

  // As well as unscoped enums.
  EXPECT_THAT(Eval("int_arr[enum_one]"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_one[int_arr]"), IsEqual("2"));

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
  EXPECT_THAT(Eval("c_arr[enum_ref].field_"), IsEqual("1"));
  EXPECT_THAT(Eval("c_arr_ref[0].field_"), IsEqual("0"));
  EXPECT_THAT(Eval("c_arr_ref[idx_1_ref].field_"), IsEqual("1"));
  EXPECT_THAT(Eval("c_arr_ref[enum_ref].field_"), IsEqual("1"));

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

TEST_F(EvalTest, TestCStyleCastBuiltins) {
  EXPECT_THAT(Eval("(int)1"), IsOk());
  EXPECT_THAT(Eval("(long long)1"), IsOk());
  EXPECT_THAT(Eval("(unsigned long)1"), IsOk());
  EXPECT_THAT(Eval("(long const const)1"), IsOk());
  EXPECT_THAT(Eval("(long const long)1"), IsOk());

  EXPECT_THAT(Eval("(char*)1"), IsOk());
  EXPECT_THAT(Eval("(long long**)1"), IsOk());
  EXPECT_THAT(Eval("(const long const long const* const const)1"), IsOk());

  EXPECT_THAT(
      Eval("(long&*)1"),
      IsError(
          "'type name' declared as a pointer to a reference of type 'long &'\n"
          "(long&*)1\n"
          "      ^"));

  EXPECT_THAT(Eval("(long& &)1"),
              IsError("type name declared as a reference to a reference\n"
                      "(long& &)1\n"
                      "       ^"));

  EXPECT_THAT(
      Eval("(long 1)1"),
      IsError("<expr>:1:7: expected 'r_paren', got: <'1' (numeric_constant)>\n"
              "(long 1)1\n"
              "      ^"));
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
  EXPECT_THAT(Eval("(int)false"), IsEqual("0"));
  EXPECT_THAT(Eval("(int)true"), IsEqual("1"));
  EXPECT_THAT(Eval("(float)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(float)1.1"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(float)1.1f"), IsEqual("1.10000002"));
  EXPECT_THAT(Eval("(float)-1.1"), IsEqual("-1.10000002"));
  EXPECT_THAT(Eval("(float)-1.1f"), IsEqual("-1.10000002"));
  EXPECT_THAT(Eval("(float)false"), IsEqual("0"));
  EXPECT_THAT(Eval("(float)true"), IsEqual("1"));
  EXPECT_THAT(Eval("(double)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(double)1.1"), IsEqual("1.1000000000000001"));
  EXPECT_THAT(Eval("(double)1.1f"), IsEqual("1.1000000238418579"));
  EXPECT_THAT(Eval("(double)-1.1"), IsEqual("-1.1000000000000001"));
  EXPECT_THAT(Eval("(double)-1.1f"), IsEqual("-1.1000000238418579"));
  EXPECT_THAT(Eval("(double)false"), IsEqual("0"));
  EXPECT_THAT(Eval("(double)true"), IsEqual("1"));
  EXPECT_THAT(Eval("(int)1.1"), IsEqual("1"));
  EXPECT_THAT(Eval("(int)1.1f"), IsEqual("1"));
  EXPECT_THAT(Eval("(int)-1.1"), IsEqual("-1"));
  EXPECT_THAT(Eval("(long)1.1"), IsEqual("1"));
  EXPECT_THAT(Eval("(long)-1.1f"), IsEqual("-1"));
  EXPECT_THAT(Eval("(bool)0"), IsEqual("false"));
  EXPECT_THAT(Eval("(bool)0.0"), IsEqual("false"));
  EXPECT_THAT(Eval("(bool)0.0f"), IsEqual("false"));
  EXPECT_THAT(Eval("(bool)3"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)-3"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)-3.4"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)-0.1"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)-0.1f"), IsEqual("true"));

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
  EXPECT_THAT(Eval("(bool)finf"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)fnan"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)fsnan"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)fmax"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)fdenorm"), IsEqual("true"));

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
  EXPECT_THAT(Eval("(bool)ap"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)(int*)0x00000000"), IsEqual("false"));
  EXPECT_THAT(Eval("(bool)nullptr"), IsEqual("false"));
  EXPECT_THAT(
      Eval("(char)ap"),
      IsError("cast from pointer to smaller type 'char' loses information"));

#ifdef _WIN32
  EXPECT_THAT(
      Eval("(long)ap"),
      IsError("cast from pointer to smaller type 'long' loses information"));
#endif

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

  EXPECT_THAT(Eval("(std::nullptr_t)nullptr"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("(std::nullptr_t)0"), IsEqual("0x0000000000000000"));

  EXPECT_THAT(Eval("(std::nullptr_t)1"),
              IsError("C-style cast from 'int' to 'std::nullptr_t' (aka "
                      "'nullptr_t') is not allowed"));
  EXPECT_THAT(Eval("(std::nullptr_t)ap"),
              IsError("C-style cast from 'int *' to 'std::nullptr_t' (aka "
                      "'nullptr_t') is not allowed"));
}

TEST_F(EvalTest, TestCStyleCastNullptrType) {
  EXPECT_THAT(
      Eval("(int)nullptr"),
      IsError("cast from pointer to smaller type 'int' loses information"));
  EXPECT_THAT(Eval("(uint64_t)nullptr"), IsEqual("0"));

  EXPECT_THAT(Eval("(void*)nullptr"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("(char*)nullptr"), IsEqual("0x0000000000000000"));
}

TEST_F(EvalTest, TestCStyleCastArray) {
  EXPECT_THAT(Eval("(int*)arr_1d"), IsOk());
  EXPECT_THAT(Eval("(char*)arr_1d"), IsOk());
  EXPECT_THAT(Eval("((char*)arr_1d)[0]"), IsEqual("'\\x01'"));
  EXPECT_THAT(Eval("((char*)arr_1d)[1]"), IsEqual("'\\0'"));

  // 2D arrays.
  EXPECT_THAT(Eval("(int*)arr_2d"), IsOk());
  EXPECT_THAT(Eval("((int*)arr_2d)[1]"), IsEqual("2"));
  EXPECT_THAT(Eval("((int*)arr_2d)[2]"), IsEqual("3"));
  EXPECT_THAT(Eval("((int*)arr_2d[1])[1]"), IsEqual("5"));
}

TEST_F(EvalTest, TestArrayDereference) {
  EXPECT_THAT(Eval("*arr_1d"), IsEqual("1"));
  EXPECT_THAT(Eval("&*arr_1d"), IsOk());
  EXPECT_THAT(Eval("*(arr_1d + 1)"), IsEqual("2"));

  EXPECT_THAT(Eval("(int*)*arr_2d"), IsOk());
  EXPECT_THAT(Eval("&*arr_2d"), IsOk());
  EXPECT_THAT(Eval("**arr_2d"), IsEqual("1"));
  EXPECT_THAT(Eval("*arr_2d[1]"), IsEqual("4"));
  EXPECT_THAT(Eval("(*arr_2d)[1]"), IsEqual("2"));
  EXPECT_THAT(Eval("**(arr_2d + 1)"), IsEqual("4"));
  EXPECT_THAT(Eval("*(*(arr_2d + 1) + 1)"), IsEqual("5"));
}

TEST_F(EvalTest, TestCStyleCastReference) {
  EXPECT_THAT(Eval("((InnerFoo&)arr_1d[1]).a"), IsEqual("2"));
  EXPECT_THAT(Eval("((InnerFoo&)arr_1d[1]).b"), IsEqual("3"));

  EXPECT_THAT(Eval("(int&)arr_1d[0]"), IsEqual("1"));
  EXPECT_THAT(Eval("(int&)arr_1d[1]"), IsEqual("2"));
}

TEST_F(EvalTest, TestCxxStaticCast) {
  EXPECT_THAT(Eval("static_cast<int>(1.1)"), IsEqual("1"));
  EXPECT_THAT(Eval("static_cast<double>(1)"), IsEqual("1"));
  EXPECT_THAT(Eval("static_cast<char>(128)"), IsEqual("'\\x80'"));
  EXPECT_THAT(Eval("static_cast<char*>(0)"), IsEqual("0x0000000000000000"));

  EXPECT_THAT(Eval("static_cast<void*>(&parent)"), IsOk());
  EXPECT_THAT(Eval("static_cast<CxxBase*>(&parent)"), IsOk());
  EXPECT_THAT(Eval("static_cast<CxxBase*>(&parent)->a"), IsEqual("1"));
  EXPECT_THAT(Eval("static_cast<CxxBase*>(&parent)->b"), IsEqual("2"));
  EXPECT_THAT(Eval("static_cast<CxxBase*>(&parent)->c"),
              IsError("no member named 'c' in 'CxxBase'"));

  EXPECT_THAT(Eval("static_cast<CxxBase&>(parent).a"), IsEqual("1"));
  EXPECT_THAT(Eval("static_cast<CxxBase&>(parent).b"), IsEqual("2"));
}

TEST_F(EvalTest, TestCxxDynamicCast) {
  // LLDB doesn't support `dynamic_cast` in the expression evaluator.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("dynamic_cast<int>(0)"),
              IsError("invalid target type 'int' for dynamic_cast"));
  EXPECT_THAT(Eval("dynamic_cast<int*>(0)"),
              IsError("'int' is not a class type"));
  EXPECT_THAT(
      Eval("dynamic_cast<CxxBase*>(1.1)"),
      IsError(
          "cannot use dynamic_cast to convert from 'double' to 'CxxBase *'"));
  EXPECT_THAT(Eval("dynamic_cast<CxxBase*>((int*)0)"),
              IsError("'int' is not a class type"));
  EXPECT_THAT(Eval("dynamic_cast<CxxVirtualParent*>(base)"),
              IsError("'CxxBase' is not polymorphic"));
  EXPECT_THAT(Eval("dynamic_cast<CxxVirtualParent*>(v_base)"),
              IsError("dynamic_cast is not supported in this context"));
}

TEST_F(EvalTest, TestCxxReinterpretCast) {
  EXPECT_THAT(Eval("reinterpret_cast<CxxBase&>(arr[0]).a"), IsEqual("1"));
  EXPECT_THAT(Eval("reinterpret_cast<CxxBase&>(arr).b"), IsEqual("2"));
  EXPECT_THAT(Eval("reinterpret_cast<CxxParent&>(arr[0]).c"),
              IsEqual("17179869187"));  // 17179869187 == 0x0000000400000003
  EXPECT_THAT(Eval("reinterpret_cast<CxxParent&>(arr).d"), IsEqual("5"));

  EXPECT_THAT(Eval("*reinterpret_cast<int*>(arr)"), IsEqual("1"));
  EXPECT_THAT(Eval("*reinterpret_cast<long long*>(arr)"),
              IsEqual("8589934593"));  // 8589934593 == 0x0000000200000001
}

TEST_F(EvalTest, TestQualifiedId) {
  EXPECT_THAT(Eval("::ns::i"), IsEqual("1"));
  EXPECT_THAT(Eval("ns::i"), IsEqual("1"));
  EXPECT_THAT(Eval("::ns::ns::i"), IsEqual("2"));
  EXPECT_THAT(Eval("ns::ns::i"), IsEqual("2"));
}

// This test depends on one of the following patches:
// * https://reviews.llvm.org/D92223
// * https://reviews.llvm.org/D92643
TEST_F(EvalTest, DISABLED_TestStaticConstDeclaredInline) {
  // Upstream LLDB doesn't handle static const variables.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("::outer::inner::Vars::inline_static"), IsEqual("1.5"));
  EXPECT_THAT(Eval("::outer::inner::Vars::static_constexpr"), IsEqual("2"));
  EXPECT_THAT(Eval("outer::inner::Vars::inline_static"), IsEqual("1.5"));
  EXPECT_THAT(Eval("outer::inner::Vars::static_constexpr"), IsEqual("2"));

  EXPECT_THAT(Eval("::outer::Vars::inline_static"), IsEqual("4.5"));
  EXPECT_THAT(Eval("::outer::Vars::static_constexpr"), IsEqual("5"));
  EXPECT_THAT(Eval("outer::Vars::inline_static"), IsEqual("4.5"));
  EXPECT_THAT(Eval("outer::Vars::static_constexpr"), IsEqual("5"));

  EXPECT_THAT(Eval("::Vars::inline_static"), IsEqual("7.5"));
  EXPECT_THAT(Eval("::Vars::static_constexpr"), IsEqual("8"));
  EXPECT_THAT(Eval("Vars::inline_static"), IsEqual("7.5"));
  EXPECT_THAT(Eval("Vars::static_constexpr"), IsEqual("8"));

  EXPECT_THAT(Scope("outer_inner_vars").Eval("inline_static"), IsEqual("1.5"));
  EXPECT_THAT(Scope("outer_inner_vars").Eval("static_constexpr"), IsEqual("2"));
  EXPECT_THAT(Scope("outer_vars").Eval("inline_static"), IsEqual("4.5"));
  EXPECT_THAT(Scope("outer_vars").Eval("static_constexpr"), IsEqual("5"));
  EXPECT_THAT(Scope("vars").Eval("inline_static"), IsEqual("7.5"));
  EXPECT_THAT(Scope("vars").Eval("inline_static"), IsEqual("8"));
}

TEST_F(EvalTest, TestStaticConstDeclaredOutsideTheClass) {
#if LLVM_VERSION_MAJOR < 12
  // Upstream LLDB doesn't handle static const variables.
  this->compare_with_lldb_ = false;
#endif

  EXPECT_THAT(Eval("::outer::inner::Vars::static_const"), IsEqual("3"));
  EXPECT_THAT(Eval("outer::inner::Vars::static_const"), IsEqual("3"));
  EXPECT_THAT(Eval("::outer::Vars::static_const"), IsEqual("6"));
  EXPECT_THAT(Eval("outer::Vars::static_const"), IsEqual("6"));
  EXPECT_THAT(Eval("::Vars::static_const"), IsEqual("9"));
  EXPECT_THAT(Eval("Vars::static_const"), IsEqual("9"));

  EXPECT_THAT(Eval("::outer::inner::Vars::Nested::static_const"),
              IsEqual("10"));
  EXPECT_THAT(Eval("outer::inner::Vars::Nested::static_const"), IsEqual("10"));
  EXPECT_THAT(Eval("::outer::Vars::Nested::static_const"), IsEqual("20"));
  EXPECT_THAT(Eval("outer::Vars::Nested::static_const"), IsEqual("20"));
  EXPECT_THAT(Eval("::Vars::Nested::static_const"), IsEqual("30"));
  EXPECT_THAT(Eval("Vars::Nested::static_const"), IsEqual("30"));

  EXPECT_THAT(Scope("outer_inner_vars").Eval("static_const"), IsEqual("3"));
  EXPECT_THAT(Scope("outer_vars").Eval("static_const"), IsEqual("6"));
  EXPECT_THAT(Scope("vars").Eval("static_const"), IsEqual("9"));

  EXPECT_THAT(Scope("outer_inner_vars").Eval("Nested::static_const"),
              IsEqual("10"));
  EXPECT_THAT(Scope("outer_vars").Eval("Nested::static_const"), IsEqual("20"));
  EXPECT_THAT(Scope("vars").Eval("Nested::static_const"), IsEqual("30"));

  EXPECT_THAT(Scope("vars").Eval("::static_const"),
              IsError("use of undeclared identifier '::static_const'"));
  EXPECT_THAT(Scope("vars").Eval("::Nested::static_const"),
              IsError("use of undeclared identifier '::Nested::static_const'"));
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

TEST_F(EvalTest, TestTemplateCpp11) {
  // Template types lookup doesn't work well in the upstream LLDB.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("(T_1<T_1<int>>::myint)1"), IsEqual("1"));
  EXPECT_THAT(Eval("(T_1<T_1<T_1<int>>>::myint)2"), IsEqual("2"));
  EXPECT_THAT(Eval("(T_2<T_1<T_1<int>>, T_1<char>>::myint)1.5"),
              IsEqual("1.5"));

  // Here T_1 is a local variable.
  EXPECT_THAT(Eval("T_1<2>1"), IsEqual("false"));   // (p < 2) > 1
  EXPECT_THAT(Eval("T_1<2>>1"), IsEqual("false"));  // (p < 2) >> 1
  // And here it's a template.
  EXPECT_THAT(Eval("T_1<int>::cx + 1"), IsEqual("25"));
}

TEST_F(EvalTest, TestTemplateWithNumericArguments) {
  // Template types lookup doesn't work well in the upstream LLDB.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("(Allocator<4>*)0"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("(TArray<int, Allocator<4> >::ElementType*)0"),
              IsEqual("0x0000000000000000"));
  // Test C++11's ">>" syntax.
  EXPECT_THAT(Eval("(TArray<int, Allocator<4>>::ElementType*)0"),
              IsEqual("0x0000000000000000"));
}

TEST_F(EvalTest, TestValueScope) {
  EXPECT_THAT(Scope("var").Eval("x_"), IsEqual("1"));
  EXPECT_THAT(Scope("var").Eval("y_"), IsEqual("2.5"));
  EXPECT_THAT(Scope("var").Eval("z_"),
              IsError("use of undeclared identifier 'z_'"));

  // In "value" scope `this` refers to the scope object.
  EXPECT_THAT(Scope("var").Eval("this->y_"), IsEqual("2.5"));
  EXPECT_THAT(Scope("var").Eval("(*this).y_"), IsEqual("2.5"));

  // Test for the "artificial" value, i.e. created by the expression.
  lldb::SBError error;
  lldb::SBValue scope_var =
      lldb_eval::EvaluateExpression(frame_, "(test_scope::Value&)bytes", error);
  EXPECT_TRUE(scope_var.IsValid());
  EXPECT_TRUE(error.Success());

  EvaluatorHelper scope(scope_var, true, false);
  EXPECT_THAT(scope.Eval("this->y_"), IsEqual("2.5"));
  EXPECT_THAT(scope.Eval("(*this).y_"), IsEqual("2.5"));

  EXPECT_THAT(Eval("x_"), IsError("use of undeclared identifier 'x_'"));
  EXPECT_THAT(Eval("y_"), IsError("use of undeclared identifier 'y_'"));
  EXPECT_THAT(Eval("z_"), IsEqual("3"));

  // In the frame context `this` is not available here.
  EXPECT_THAT(
      Eval("this->y_"),
      IsError("invalid use of 'this' outside of a non-static member function"));
  EXPECT_THAT(
      Eval("(*this)->y_"),
      IsError("invalid use of 'this' outside of a non-static member function"));

  EXPECT_THAT(Scope("var").Eval("this - (test_scope::Value*)this"),
              IsEqual("0"));
}

TEST_F(EvalTest, TestBitField) {
  EXPECT_THAT(Eval("bf.a"), IsEqual("1023"));
  EXPECT_THAT(Eval("bf.b"), IsEqual("9"));
  EXPECT_THAT(Eval("bf.c"), IsEqual("false"));
  EXPECT_THAT(Eval("bf.d"), IsEqual("true"));
  EXPECT_THAT(Scope("bf").Eval("a"), IsEqual("1023"));
  EXPECT_THAT(Scope("bf").Eval("b"), IsEqual("9"));
  EXPECT_THAT(Scope("bf").Eval("c"), IsEqual("false"));
  EXPECT_THAT(Scope("bf").Eval("d"), IsEqual("true"));

  // Perform an operation to ensure we actually read the value.
  EXPECT_THAT(Eval("0 + bf.a"), IsEqual("1023"));
  EXPECT_THAT(Eval("0 + bf.b"), IsEqual("9"));
  EXPECT_THAT(Eval("0 + bf.c"), IsEqual("0"));
  EXPECT_THAT(Eval("0 + bf.d"), IsEqual("1"));
  EXPECT_THAT(Scope("bf").Eval("0 + a"), IsEqual("1023"));
  EXPECT_THAT(Scope("bf").Eval("0 + b"), IsEqual("9"));
  EXPECT_THAT(Scope("bf").Eval("0 + c"), IsEqual("0"));
  EXPECT_THAT(Scope("bf").Eval("0 + d"), IsEqual("1"));

  EXPECT_THAT(Eval("abf.a"), IsEqual("1023"));
  EXPECT_THAT(Eval("abf.b"), IsEqual("'\\x0f'"));
  EXPECT_THAT(Eval("abf.c"), IsEqual("3"));
  EXPECT_THAT(Scope("abf").Eval("a"), IsEqual("1023"));
  EXPECT_THAT(Scope("abf").Eval("b"), IsEqual("'\\x0f'"));
  EXPECT_THAT(Scope("abf").Eval("c"), IsEqual("3"));

  // Perform an operation to ensure we actually read the value.
  EXPECT_THAT(Eval("abf.a + 0"), IsEqual("1023"));
  EXPECT_THAT(Eval("abf.b + 0"), IsEqual("15"));
  EXPECT_THAT(Eval("abf.c + 0"), IsEqual("3"));
  EXPECT_THAT(Scope("abf").Eval("0 + a"), IsEqual("1023"));
  EXPECT_THAT(Scope("abf").Eval("0 + b"), IsEqual("15"));
  EXPECT_THAT(Scope("abf").Eval("0 + c"), IsEqual("3"));

  // Address-of is not allowed for bit-fields.
  EXPECT_THAT(Eval("&bf.a"), IsError("address of bit-field requested"));
  EXPECT_THAT(Eval("&(true ? bf.a : bf.a)"),
              IsError("address of bit-field requested"));
}

TEST_F(EvalTest, TestBitFieldPromotion) {
  EXPECT_THAT(Eval("bf.b - 10"), IsEqual("-1"));
  EXPECT_THAT(Eval("bf.e - 2"), IsEqual("-1"));
  EXPECT_THAT(Eval("bf.f - 2"), IsEqual("4294967295"));
  EXPECT_THAT(Eval("bf.g - 2"), IsEqual("-1"));
  EXPECT_THAT(Eval("bf.h - 2"), IsEqual("-1"));
  EXPECT_THAT(Eval("bf.i - 2"), IsEqual("18446744073709551615"));
  EXPECT_THAT(Eval("bf.g - bf.b"), IsEqual("-8"));

  EXPECT_THAT(Eval("-(true ? bf.b : bf.a)"), IsEqual("-9"));
  EXPECT_THAT(Eval("-(true ? bf.b : bf.e)"), IsEqual("-9"));
  EXPECT_THAT(Eval("-(true ? bf.b : bf.f)"), IsEqual("4294967287"));
  EXPECT_THAT(Eval("-(true ? bf.b : bf.g)"), IsEqual("4294967287"));
  EXPECT_THAT(Eval("-(true ? bf.b : bf.h)"), IsEqual("-9"));

  if (HAS_METHOD(lldb::SBType, GetEnumerationIntegerType())) {
    EXPECT_THAT(Eval("bf.j - 2"), IsEqual("4294967295"));
    EXPECT_THAT(Eval("-(true ? bf.b : bf.j)"), IsEqual("4294967287"));
    EXPECT_THAT(Eval("-(true ? bf.e : bf.j)"), IsEqual("4294967295"));
  }

  // TODO: Repeat tests in the value context once the bitfield information is
  // correctly retrieved from identifier lookup.
}

TEST_F(EvalTest, TestBitFieldWithSideEffects) {
  this->compare_with_lldb_ = false;
  this->allow_side_effects_ = true;

  EXPECT_THAT(Eval("bf.b -= 10"), IsEqual("15"));
  EXPECT_THAT(Eval("bf.e -= 10"), IsEqual("-9"));
  EXPECT_THAT(Eval("bf.e++"), IsEqual("-9"));
  EXPECT_THAT(Eval("++bf.e"), IsEqual("-7"));

  // TODO: Enable test once the issue is fixed:
  // EXPECT_THAT(Eval("bf.b++"), IsEqual("15"));
}

TEST_F(EvalTest, TestContextVariables) {
  // Context variables don't exist yet.
  EXPECT_THAT(EvalWithContext("$var", vars_),
              IsError("use of undeclared identifier '$var'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var", vars_),
              IsError("use of undeclared identifier '$var'"));

  EXPECT_TRUE(CreateContextVariable("$var", "13"));
  EXPECT_THAT(EvalWithContext("$var", vars_), IsEqual("13"));
  EXPECT_THAT(EvalWithContext("$var + 2", vars_), IsEqual("15"));
  EXPECT_THAT(EvalWithContext("$var - s.a", vars_), IsEqual("3"));
  EXPECT_THAT(EvalWithContext("var", vars_),
              IsError("use of undeclared identifier 'var'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var", vars_), IsEqual("13"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var + 2", vars_), IsEqual("15"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var - a", vars_), IsEqual("3"));
  EXPECT_THAT(Scope("s").EvalWithContext("var", vars_),
              IsError("use of undeclared identifier 'var'"));

  // Context variable is a pointer.
  EXPECT_TRUE(CreateContextVariable("$ptr", "s.ptr"));
  EXPECT_THAT(EvalWithContext("$ptr == s.ptr", vars_), IsEqual("true"));
  EXPECT_THAT(EvalWithContext("*$ptr", vars_), IsEqual("'h'"));
  EXPECT_THAT(EvalWithContext("$ptr[1]", vars_), IsEqual("'e'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$ptr == ptr", vars_),
              IsEqual("true"));
  EXPECT_THAT(Scope("s").EvalWithContext("*$ptr", vars_), IsEqual("'h'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$ptr[1]", vars_), IsEqual("'e'"));

  EXPECT_THAT(EvalWithContext("$var + *$ptr", vars_), IsEqual("117"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var + *$ptr", vars_),
              IsEqual("117"));
}

TEST_F(EvalTest, TestContextVariablesSubset) {
  // All context variables that are created in this test are visible by LLDB.
  // Disable comparisons with LLDB to test subsets of created context variables.
  this->compare_with_lldb_ = false;

  EXPECT_TRUE(CreateContextVariable("$var", "13"));
  EXPECT_TRUE(CreateContextVariable("$ptr", "s.ptr"));

  // Evaluate without the context.
  EXPECT_THAT(Eval("$var"), IsError("use of undeclared identifier '$var'"));
  EXPECT_THAT(Eval("$var + 1"), IsError("use of undeclared identifier '$var'"));
  EXPECT_THAT(Scope("s").Eval("$var"),
              IsError("use of undeclared identifier '$var'"));
  EXPECT_THAT(Scope("s").Eval("$var + 1"),
              IsError("use of undeclared identifier '$var'"));

  std::unordered_map<std::string, lldb::SBValue> var;
  std::unordered_map<std::string, lldb::SBValue> ptr;
  var.emplace("$var", vars_["$var"]);
  ptr.emplace("$ptr", vars_["$ptr"]);

  EXPECT_THAT(EvalWithContext("$var + 0", var), IsEqual("13"));
  EXPECT_THAT(EvalWithContext("*$ptr", var),
              IsError("use of undeclared identifier '$ptr'"));
  EXPECT_THAT(EvalWithContext("$var + *$ptr", var),
              IsError("use of undeclared identifier '$ptr'"));
  EXPECT_THAT(EvalWithContext("$var + *$ptr", ptr),
              IsError("use of undeclared identifier '$var'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var + 0", var), IsEqual("13"));
  EXPECT_THAT(Scope("s").EvalWithContext("*$ptr", var),
              IsError("use of undeclared identifier '$ptr'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var + *$ptr", var),
              IsError("use of undeclared identifier '$ptr'"));
  EXPECT_THAT(Scope("s").EvalWithContext("$var + *$ptr", ptr),
              IsError("use of undeclared identifier '$var'"));
}

TEST_F(EvalTest, TestScopedEnum) {
  EXPECT_THAT(Eval("enum_foo"), IsEqual("kFoo"));
  EXPECT_THAT(Eval("enum_bar"), IsEqual("kBar"));

  EXPECT_THAT(Eval("enum_foo == enum_foo"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_foo != enum_foo"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_foo == enum_bar"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_foo < enum_bar"), IsEqual("true"));

  EXPECT_THAT(Eval("enum_foo == ScopedEnum::kFoo"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_foo == ScopedEnum::kBar"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_foo != ScopedEnum::kBar"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_foo < ScopedEnum::kBar"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_foo < enum_neg"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_neg < enum_bar"), IsEqual("true"));

  EXPECT_THAT(Eval("(ScopedEnum)0"), IsEqual("kFoo"));
  EXPECT_THAT(Eval("(ScopedEnum)1"), IsEqual("kBar"));
  EXPECT_THAT(Eval("(ScopedEnum)0.1"), IsEqual("kFoo"));
  EXPECT_THAT(Eval("(ScopedEnum)1.1"), IsEqual("kBar"));
  EXPECT_THAT(Eval("(ScopedEnum)-1"), IsOk());
  EXPECT_THAT(Eval("(ScopedEnum)-1.1"), IsOk());
  EXPECT_THAT(Eval("(ScopedEnum)256"), IsOk());
  EXPECT_THAT(Eval("(ScopedEnum)257"), IsOk());
  EXPECT_THAT(Eval("(ScopedEnum)false"), IsEqual("kFoo"));
  EXPECT_THAT(Eval("(ScopedEnum)true"), IsEqual("kBar"));

  EXPECT_THAT(Eval("(int)enum_foo"), IsEqual("0"));
  EXPECT_THAT(Eval("(int)enum_neg"), IsEqual("-1"));
  EXPECT_THAT(Eval("(unsigned short)enum_neg"), IsEqual("65535"));
  EXPECT_THAT(Eval("(short)ScopedEnum::kBar"), IsEqual("1"));
  EXPECT_THAT(Eval("(short*)enum_neg"), IsEqual("0xffffffffffffffff"));
  EXPECT_THAT(Eval("(char*)enum_u8_bar"), IsEqual("0x0000000000000001"));
  EXPECT_THAT(Eval("(float)enum_bar"), IsEqual("1"));
  EXPECT_THAT(Eval("(float)enum_foo"), IsEqual("0"));
  EXPECT_THAT(Eval("(float)enum_neg"), IsEqual("-1"));
  EXPECT_THAT(Eval("(double)enum_neg"), IsEqual("-1"));
  EXPECT_THAT(Eval("(bool)enum_foo"), IsEqual("false"));
  EXPECT_THAT(Eval("(bool)enum_bar"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)enum_neg"), IsEqual("true"));
  EXPECT_THAT(Eval("(double)ScopedEnumUInt8::kBar"), IsEqual("1"));
  EXPECT_THAT(Eval("(ScopedEnum)ScopedEnum::kBar"), IsEqual("kBar"));

  // TODO: Enable the following test once the underlying enumeration type
  // becomes fully available. Information about underlying enumeration type
  // isn't available in LLDB 11 or lower, which are currently used by GitHub
  // actions.
  // EXPECT_THAT(Eval("(int)(ScopedEnumUInt8)enum_neg"), IsEqual("255"));
}

TEST_F(EvalTest, TestScopedEnumArithmetic) {
  if (!HAS_METHOD(lldb::SBType, IsScopedEnumerationType())) {
    GTEST_SKIP();
  }

  EXPECT_THAT(Eval("enum_foo == 1"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_foo + 1"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_foo * 2"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_bar / 2"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_foo % 2"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_bar & 2"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_bar | 0x01"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_bar ^ 0b11"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_bar >> 1"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("enum_bar << 1"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("1 >> enum_bar"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("1 << enum_bar"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("(int*)1 + enum_foo"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("(int*)5 - enum_foo"),
              IsError("invalid operands to binary expression"));
  EXPECT_THAT(Eval("+enum_foo"), IsError("invalid argument type"));
  EXPECT_THAT(Eval("-enum_foo"), IsError("invalid argument type"));
  EXPECT_THAT(Eval("~enum_foo"), IsError("invalid argument type"));
  EXPECT_THAT(Eval("!enum_foo"),
              IsError("invalid argument type 'ScopedEnum' to unary expression\n"
                      "!enum_foo\n"
                      "^"));
}

TEST_F(EvalTest, TestScopedEnumWithUnderlyingType) {
  EXPECT_THAT(Eval("(ScopedEnumUInt8)-1"), IsOk());
  EXPECT_THAT(Eval("(ScopedEnumUInt8)256"), IsEqual("kFoo"));
  EXPECT_THAT(Eval("(ScopedEnumUInt8)257"), IsEqual("kBar"));
}

TEST_F(EvalTest, TestUnscopedEnum) {
  EXPECT_THAT(Eval("enum_one"), IsEqual("kOne"));
  EXPECT_THAT(Eval("enum_two"), IsEqual("kTwo"));

  EXPECT_THAT(Eval("enum_one == enum_one"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one != enum_one"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_one == enum_two"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_one < enum_two"), IsEqual("true"));

  EXPECT_THAT(Eval("enum_one == UnscopedEnum::kOne"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one == UnscopedEnum::kTwo"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_one != UnscopedEnum::kTwo"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one < UnscopedEnum::kTwo"), IsEqual("true"));

  EXPECT_THAT(Eval("(UnscopedEnum)0"), IsEqual("kZero"));
  EXPECT_THAT(Eval("(UnscopedEnum)1"), IsEqual("kOne"));
  EXPECT_THAT(Eval("(UnscopedEnum)0.1"), IsEqual("kZero"));
  EXPECT_THAT(Eval("(UnscopedEnum)1.1"), IsEqual("kOne"));
  EXPECT_THAT(Eval("(UnscopedEnum)-1"), IsOk());
  EXPECT_THAT(Eval("(UnscopedEnum)256"), IsOk());
  EXPECT_THAT(Eval("(UnscopedEnum)257"), IsOk());
  EXPECT_THAT(Eval("(UnscopedEnum)false"), IsEqual("kZero"));
  EXPECT_THAT(Eval("(UnscopedEnum)true"), IsEqual("kOne"));
  EXPECT_THAT(Eval("(UnscopedEnum)(UnscopedEnum)0"), IsEqual("kZero"));
  EXPECT_THAT(Eval("(void*)(UnscopedEnum)1"), IsEqual("0x0000000000000001"));

  EXPECT_THAT(Eval("enum_one == 1"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one + 1"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_one * 2"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_two / 2"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_one % 2"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_two & 2"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_two | 0x01"), IsEqual("3"));
  EXPECT_THAT(Eval("enum_two ^ 0b11"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_two >> 1"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_two << 1"), IsEqual("4"));
  EXPECT_THAT(Eval("8 >> enum_two"), IsEqual("2"));
  EXPECT_THAT(Eval("1 << enum_two"), IsEqual("4"));
  EXPECT_THAT(Eval("UnscopedEnumUInt8::kTwoU8 << 1"), IsEqual("4"));
  EXPECT_THAT(Eval("UnscopedEnumInt8::kTwo8 << 1U"), IsEqual("4"));
  EXPECT_THAT(Eval("+enum_one"), IsEqual("1"));
  EXPECT_THAT(Eval("!enum_one"), IsEqual("false"));
  EXPECT_THAT(Eval("(int*)1 + enum_one"), IsEqual("0x0000000000000005"));
  EXPECT_THAT(Eval("(int*)5 - enum_one"), IsEqual("0x0000000000000001"));

  // Use references.
  EXPECT_THAT(Eval("enum_one_ref"), IsEqual("kOne"));
  EXPECT_THAT(Eval("enum_two_ref"), IsEqual("kTwo"));
  EXPECT_THAT(Eval("enum_one_ref == enum_one"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one_ref != enum_one"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_one_ref == enum_two"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_one_ref < enum_two"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one_ref == UnscopedEnum::kOne"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one_ref == UnscopedEnum::kTwo"), IsEqual("false"));
  EXPECT_THAT(Eval("enum_one_ref != UnscopedEnum::kTwo"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one_ref < UnscopedEnum::kTwo"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one_ref == 1"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_one_ref + 1"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_one_ref * 2"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_two_ref / 2"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_one_ref % 2"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_two_ref & 2"), IsEqual("2"));
  EXPECT_THAT(Eval("enum_two_ref | 0x01"), IsEqual("3"));
  EXPECT_THAT(Eval("enum_two_ref ^ 0b11"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_two_ref >> 1"), IsEqual("1"));
  EXPECT_THAT(Eval("enum_two_ref << 1"), IsEqual("4"));
  EXPECT_THAT(Eval("8 >> enum_two_ref"), IsEqual("2"));
  EXPECT_THAT(Eval("1 << enum_two_ref"), IsEqual("4"));
  EXPECT_THAT(Eval("+enum_one_ref"), IsEqual("1"));
  EXPECT_THAT(Eval("!enum_one_ref"), IsEqual("false"));
  EXPECT_THAT(Eval("(int*)1 + enum_one_ref"), IsEqual("0x0000000000000005"));
  EXPECT_THAT(Eval("(int*)5 - enum_one_ref"), IsEqual("0x0000000000000001"));

  EXPECT_THAT(Eval("(long long)UnscopedEnum::kTwo"), IsEqual("2"));
  EXPECT_THAT(Eval("(unsigned int)enum_one"), IsEqual("1"));
  EXPECT_THAT(Eval("(short*)UnscopedEnumUInt8::kTwoU8"),
              IsEqual("0x0000000000000002"));
  EXPECT_THAT(Eval("(float)UnscopedEnum::kOne"), IsEqual("1"));
  EXPECT_THAT(Eval("(float)enum_two"), IsEqual("2"));
  EXPECT_THAT(Eval("(double)enum_one"), IsEqual("1"));
  EXPECT_THAT(Eval("(bool)enum_two"), IsEqual("true"));
  EXPECT_THAT(Eval("(bool)(UnscopedEnum)0"), IsEqual("false"));
  EXPECT_THAT(Eval("(bool)(UnscopedEnum)-1"), IsEqual("true"));
}

TEST_F(EvalTest, TestUnscopedEnumNegation) {
  // Sometimes LLDB is confused about signedness and uses "unsigned int" as an
  // underlying type. Technically, underlying type for the unscoped enum is
  // implementation defined, but it should convert to "int" nonetheless.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("-enum_one"), IsEqual("-1"));
  EXPECT_THAT(Eval("-enum_one_ref"), IsEqual("-1"));
  EXPECT_THAT(Eval("-(UnscopedEnumEmpty)1"), IsOk());
}

TEST_F(EvalTest, TestUnscopedEnumWithUnderlyingType) {
  EXPECT_THAT(Eval("(UnscopedEnumUInt8)-1"), IsOk());
  EXPECT_THAT(Eval("(UnscopedEnumUInt8)256"), IsEqual("kZeroU8"));
  EXPECT_THAT(Eval("(UnscopedEnumUInt8)257"), IsEqual("kOneU8"));

  EXPECT_THAT(Eval("(UnscopedEnumInt8)-2.1"), IsOk());
  EXPECT_THAT(Eval("(int)enum_neg_8"), IsEqual("-1"));
  EXPECT_THAT(Eval("enum_neg_8 < enum_one_8"), IsEqual("true"));
  EXPECT_THAT(Eval("enum_neg_8 > enum_one_8"), IsEqual("false"));
}

TEST_F(EvalTest, TestUnscopedEnumEmpty) {
  EXPECT_THAT(Eval("(UnscopedEnumEmpty)1"), IsOk());
  EXPECT_THAT(Eval("(int)enum_empty"), IsOk());
}

TEST_F(EvalTest, TestTernaryOperator) {
  EXPECT_THAT(Eval("true ? c : c"), IsEqual("'\\x02'"));
  EXPECT_THAT(Eval("true ? c : (char)1"), IsEqual("'\\x02'"));
  EXPECT_THAT(Eval("true ? c : 1"), IsEqual("2"));
  EXPECT_THAT(Eval("false ? 1 : c"), IsEqual("2"));

  EXPECT_THAT(Eval("false ? a_enum : EnumA::kOneA"), IsEqual("kOneA"));
  EXPECT_THAT(Eval("false ? b_enum : a_enum"), IsEqual("2"));
  EXPECT_THAT(Eval("false ? c : b_enum"), IsEqual("1"));
  EXPECT_THAT(Eval("false ? b_enum : c"), IsEqual("2"));

  EXPECT_THAT(Eval("true ? (int*)15 : 0"), IsEqual("0x000000000000000f"));
  EXPECT_THAT(Eval("true ? 0 : (int*)15"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("true ? (int*)15 : nullptr"), IsEqual("0x000000000000000f"));
  EXPECT_THAT(Eval("true ? nullptr : (int*)15"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("true ? 0 : nullptr"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("true ? nullptr : 0"), IsEqual("0x0000000000000000"));

  EXPECT_THAT(Eval("+(true ? arr2 : arr2)"), IsOk());
  EXPECT_THAT(Eval("true ? arr2 : arr3"), IsOk());
  EXPECT_THAT(Eval("true ? arr2 : 0"), IsOk());
  EXPECT_THAT(Eval("true ? 0 : arr2"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("true ? arr2 : nullptr"), IsOk());
  EXPECT_THAT(Eval("true ? nullptr : arr2"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("true ? arr2 : (int*)15"), IsOk());
  EXPECT_THAT(Eval("true ? (int*)15 : arr2"), IsEqual("0x000000000000000f"));

  EXPECT_THAT(Eval("&(true ? arr2 : arr2)"), IsOk());

  EXPECT_THAT(Eval("true ? t : 1"),
              IsError("incompatible operand types ('T' and 'int')\n"
                      "true ? t : 1\n"
                      "     ^"));
  EXPECT_THAT(Eval("true ? 1 : t"),
              IsError("incompatible operand types ('int' and 'T')\n"
                      "true ? 1 : t\n"
                      "     ^"));
  EXPECT_THAT(Eval("true ? (int*)15 : 1"),
              IsError("incompatible operand types ('int *' and 'int')\n"
                      "true ? (int*)15 : 1\n"
                      "     ^"));
  EXPECT_THAT(Eval("true ? arr2 : dbl_arr"),
              IsError("incompatible operand types ('int *' and 'double *')\n"
                      "true ? arr2 : dbl_arr\n"
                      "     ^"));

  EXPECT_THAT(Eval("&(true ? *pi : 0)"),
              IsError("cannot take the address of an rvalue of type 'int'\n"
                      "&(true ? *pi : 0)\n"
                      "^"));
  EXPECT_THAT(Eval("&(true ? arr2 : pi)"),
              IsError("cannot take the address of an rvalue of type 'int *'\n"
                      "&(true ? arr2 : pi)\n"
                      "^"));
  EXPECT_THAT(Eval("&(true ? arr2 : arr3)"),
              IsError("cannot take the address of an rvalue of type 'int *'\n"
                      "&(true ? arr2 : arr3)\n"
                      "^"));

  EXPECT_THAT(Eval("true ? nullptr : pi"), IsEqual("0x0000000000000000"));
  EXPECT_THAT(Eval("true ? pi : nullptr"), IsOk());
  EXPECT_THAT(Eval("false ? nullptr : pi"), IsOk());
  EXPECT_THAT(Eval("false ? pi : nullptr"), IsEqual("0x0000000000000000"));

  // Use pointers and arrays in bool context.
  EXPECT_THAT(Eval("pi ? 1 : 2"), IsEqual("1"));
  EXPECT_THAT(Eval("nullptr ? 1 : 2"), IsEqual("2"));
  EXPECT_THAT(Eval("arr2 ? 1 : 2"), IsEqual("1"));
}

TEST_F(EvalTest, TestSizeOf) {
  EXPECT_THAT(Eval("sizeof(int)"), IsEqual("4"));
  EXPECT_THAT(Eval("sizeof(int&)"), IsEqual("4"));
  EXPECT_THAT(Eval("sizeof(int*)"), IsEqual("8"));
  EXPECT_THAT(Eval("sizeof(int***)"), IsEqual("8"));

  EXPECT_THAT(Eval("sizeof(i)"), IsEqual("4"));
  EXPECT_THAT(Eval("sizeof i "), IsEqual("4"));
  EXPECT_THAT(Eval("sizeof p "), IsEqual("8"));
  EXPECT_THAT(Eval("sizeof(arr)"), IsEqual("12"));
  EXPECT_THAT(Eval("sizeof arr "), IsEqual("12"));
  EXPECT_THAT(Eval("sizeof(arr + 1)"), IsEqual("8"));
  EXPECT_THAT(Eval("sizeof arr + 1 "), IsEqual("13"));

  EXPECT_THAT(Eval("sizeof(foo)"), IsEqual("8"));
  EXPECT_THAT(Eval("sizeof foo "), IsEqual("8"));
  EXPECT_THAT(Eval("sizeof(SizeOfFoo)"), IsEqual("8"));

  EXPECT_THAT(
      Eval("sizeof(int & *)"),
      IsError(
          "'type name' declared as a pointer to a reference of type 'int &'\n"
          "sizeof(int & *)\n"
          "             ^"));

  EXPECT_THAT(Eval("sizeof(intt + 1)"),
              IsError("use of undeclared identifier 'intt'\n"
                      "sizeof(intt + 1)\n"
                      "       ^"));
}

TEST_F(EvalTest, TestBuiltinFunction_Log2) {
  // LLDB doesn't support `__log2` intrinsic function.
  this->compare_with_lldb_ = false;

  // Note: Visual Studio debugger gives an error in this case:
  //
  //   > __log(0)
  //   Attempt to calculate __log2(0).
  //
  EXPECT_THAT(Eval("__log2(0)"), IsEqual("4294967295"));
  EXPECT_THAT(Eval("__log2(1LL << 32)"), IsEqual("4294967295"));

  EXPECT_THAT(Eval("__log2(1)"), IsEqual("0"));
  EXPECT_THAT(Eval("__log2(8)"), IsEqual("3"));
  EXPECT_THAT(Eval("__log2(12345)"), IsEqual("13"));
  EXPECT_THAT(Eval("__log2(-1)"), IsEqual("31"));
  EXPECT_THAT(Eval("__log2((1LL << 32)+128)"), IsEqual("7"));

  EXPECT_THAT(Eval("__log2(32.3f)"), IsEqual("5"));
  EXPECT_THAT(Eval("__log2(12345.12345)"), IsEqual("13"));

  EXPECT_THAT(Eval("__log2(c_enum)"), IsEqual("7"));
  EXPECT_THAT(Eval("__log2(cxx_enum)"), IsEqual("7"));
  EXPECT_THAT(Eval("__log2(c_enum_ref)"), IsEqual("7"));
  EXPECT_THAT(Eval("__log2(cxx_enum_ref)"), IsEqual("7"));
  EXPECT_THAT(Eval("__log2(CEnum::kFoo)"), IsEqual("7"));
  EXPECT_THAT(Eval("__log2(CxxEnum::kFoo)"), IsEqual("7"));

  EXPECT_THAT(Eval("__log2(foo)"),
              IsError("no known conversion from 'Foo' to 'unsigned int'\n"
                      "__log2(foo)\n"
                      "       ^"));

  EXPECT_THAT(Eval("__log2()"),
              IsError("no matching function for call to '__log2': requires 1 "
                      "argument(s), but 0 argument(s) were provided\n"
                      "__log2()\n"
                      "^"));

  EXPECT_THAT(Eval("1 + __log2(1, 2)"),
              IsError("no matching function for call to '__log2': requires 1 "
                      "argument(s), but 2 argument(s) were provided\n"
                      "1 + __log2(1, 2)\n"
                      "    ^"));

  EXPECT_THAT(Eval("dummy(1)"),
              IsError("function 'dummy' is not a supported builtin intrinsic"));
  EXPECT_THAT(
      Eval("::dummy(1)"),
      IsError("function '::dummy' is not a supported builtin intrinsic"));
  EXPECT_THAT(
      Eval("ns::dummy(1)"),
      IsError("function 'ns::dummy' is not a supported builtin intrinsic"));
  EXPECT_THAT(
      Eval("::ns::dummy(1)"),
      IsError("function '::ns::dummy' is not a supported builtin intrinsic"));
}

TEST_F(EvalTest, TestPrefixIncDec) {
  EXPECT_THAT(Eval("++1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("--i"),
              IsError("side effects are not supported in this context"));

  ASSERT_TRUE(CreateContextVariableArray("int", "$arr", "{1,2,3}"));
  EXPECT_THAT(EvalWithContext("++$arr", vars_),
              IsError("cannot increment value of type 'int [3]'"));

  ASSERT_TRUE(CreateContextVariable("$enum_foo", "ScopedEnum::kFoo"));
  EXPECT_THAT(EvalWithContext("++$enum_foo", vars_),
              IsError("cannot increment expression of enum type 'ScopedEnum'"));

  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("++$i", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("++$i + 1", vars_), IsEqual("4"));
  EXPECT_THAT(EvalWithContext("--$i", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("--$i - 1", vars_), IsEqual("0"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(EvalWithContext("++$f", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("++$f + 1", vars_), IsEqual("4.5"));
  EXPECT_THAT(EvalWithContext("--$f", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("--$f - 1", vars_), IsEqual("0.5"));

  ASSERT_TRUE(CreateContextVariable("$d", "1.5"));
  EXPECT_THAT(EvalWithContext("++$d", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("++$d + 1", vars_), IsEqual("4.5"));
  EXPECT_THAT(EvalWithContext("--$d", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("--$d - 1", vars_), IsEqual("0.5"));

  ASSERT_TRUE(CreateContextVariable("$p", "(int*)4"));
  EXPECT_THAT(EvalWithContext("++$p", vars_), IsEqual("0x0000000000000008"));
  EXPECT_THAT(EvalWithContext("++$p + 1", vars_),
              IsEqual("0x0000000000000010"));
  EXPECT_THAT(EvalWithContext("--$p", vars_), IsEqual("0x0000000000000008"));
  EXPECT_THAT(EvalWithContext("--$p - 1", vars_),
              IsEqual("0x0000000000000000"));
}

TEST_F(EvalTest, TestPostfixIncDec) {
  EXPECT_THAT(Eval("1++"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i--"),
              IsError("side effects are not supported in this context"));

  ASSERT_TRUE(CreateContextVariableArray("int", "$arr", "{1,2,3}"));
  EXPECT_THAT(EvalWithContext("$arr--", vars_),
              IsError("cannot decrement value of type 'int [3]'"));

  ASSERT_TRUE(CreateContextVariable("$enum_foo", "ScopedEnum::kFoo"));
  EXPECT_THAT(EvalWithContext("$enum_foo++", vars_),
              IsError("cannot increment expression of enum type 'ScopedEnum'"));

  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("$i++", vars_), IsEqual("1"));
  EXPECT_THAT(EvalWithContext("$i", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i++ + 1", vars_), IsEqual("3"));
  EXPECT_THAT(EvalWithContext("$i--", vars_), IsEqual("3"));
  EXPECT_THAT(EvalWithContext("$i-- - 1", vars_), IsEqual("1"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(EvalWithContext("$f++", vars_), IsEqual("1.5"));
  EXPECT_THAT(EvalWithContext("$f", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("$f++ + 1", vars_), IsEqual("3.5"));
  EXPECT_THAT(EvalWithContext("$f--", vars_), IsEqual("3.5"));
  EXPECT_THAT(EvalWithContext("$f-- - 1", vars_), IsEqual("1.5"));

  ASSERT_TRUE(CreateContextVariable("$d", "1.5"));
  EXPECT_THAT(EvalWithContext("$d++", vars_), IsEqual("1.5"));
  EXPECT_THAT(EvalWithContext("$d++ + 1", vars_), IsEqual("3.5"));
  EXPECT_THAT(EvalWithContext("$d--", vars_), IsEqual("3.5"));
  EXPECT_THAT(EvalWithContext("$d-- - 1", vars_), IsEqual("1.5"));

  ASSERT_TRUE(CreateContextVariable("$p", "(int*)4"));
  EXPECT_THAT(EvalWithContext("$p++", vars_), IsEqual("0x0000000000000004"));
  EXPECT_THAT(EvalWithContext("$p++ + 1", vars_),
              IsEqual("0x000000000000000c"));
  EXPECT_THAT(EvalWithContext("$p--", vars_), IsEqual("0x000000000000000c"));
  EXPECT_THAT(EvalWithContext("$p-- - 1", vars_),
              IsEqual("0x0000000000000004"));
}

TEST_F(EvalTest, TestDereferencedType) {
  // When dereferencing the value LLDB also removes the typedef and the
  // resulting value has the canonical type. We have to mitigate that, because
  // having the correct type is important for visualization logic.
  //
  // In this example we have:
  //
  //   ```
  //   struct TTuple { ... };
  //   using TPair = TTuple;
  //   const TPair& p_ref = ...;
  //   const TPair* p_ptr = ...;
  //   ```
  //
  // The expression evaluator removes the references from all values and we must
  // verify that the resulting type is correct.

  for (auto&& expr : {"p_ref", "*p_ptr"}) {
    // Directly evaluate the expression and verify the types.
    auto ret = Eval(expr);

    ASSERT_TRUE(ret.lldb_eval_error.Success());
    ASSERT_TRUE(ret.lldb_value.has_value());

    ASSERT_STREQ(ret.lldb_eval_value.GetTypeName(), "TPair");
    ASSERT_STREQ(ret.lldb_value.value().GetTypeName(), "TPair");
  }
}

TEST_F(EvalTest, TestMemberFunctionCall) {
  // LLDB supports function calls, so disable it.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("c.m()"),
              IsError("member function calls are not supported"));
}

TEST_F(EvalTest, TestAssignment) {
  EXPECT_THAT(Eval("1 = 1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i = 1"), IsError("side effects are not supported"));

  EXPECT_THAT(Eval("p = 1"),
              IsError("no known conversion from 'int' to 'float *'"));
  EXPECT_THAT(Eval("eOne = 1"),
              IsError("no known conversion from 'int' to 'Enum'"));

  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("$i = 2", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i = -2", vars_), IsEqual("-2"));
  EXPECT_THAT(EvalWithContext("$i", vars_), IsEqual("-2"));
  EXPECT_THAT(EvalWithContext("$i = eOne", vars_), IsEqual("0"));
  EXPECT_THAT(EvalWithContext("$i", vars_), IsEqual("0"));
  EXPECT_THAT(EvalWithContext("$i = eTwo", vars_), IsEqual("1"));
  EXPECT_THAT(EvalWithContext("$i", vars_), IsEqual("1"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(EvalWithContext("$f = 2.5", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("$f", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("$f = 3.5f", vars_), IsEqual("3.5"));
  EXPECT_THAT(EvalWithContext("$f", vars_), IsEqual("3.5"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s = 100000", vars_), IsEqual("-31072"));
  EXPECT_THAT(EvalWithContext("$s", vars_), IsEqual("-31072"));

  ASSERT_TRUE(CreateContextVariable("$p", "(int*)10"));
  EXPECT_THAT(EvalWithContext("$p = 1", vars_),
              IsError("no known conversion from 'int' to 'int *'"));
  EXPECT_THAT(EvalWithContext("$p = (int*)12", vars_),
              IsEqual("0x000000000000000c"));
  EXPECT_THAT(EvalWithContext("$p", vars_), IsEqual("0x000000000000000c"));
  EXPECT_THAT(EvalWithContext("$p = 0", vars_), IsEqual("0x0000000000000000"));
  EXPECT_THAT(EvalWithContext("$p = nullptr", vars_),
              IsEqual("0x0000000000000000"));

  ASSERT_TRUE(CreateContextVariableArray("int", "$arr", "{1, 2}"));
  EXPECT_THAT(EvalWithContext("$p = $arr", vars_), IsOk());

  ASSERT_TRUE(CreateContextVariableArray("float", "$farr", "{1.f, 2.f}"));
  EXPECT_THAT(EvalWithContext("$p = $farr", vars_),
              IsError("no known conversion from 'float [2]' to 'int *'"));
}

TEST_F(EvalTest, TestCompositeAssignmentInvalid) {
  EXPECT_THAT(Eval("1 += 1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i += 1"), IsError("side effects are not supported"));

  EXPECT_THAT(Eval("1 -= 1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i -= 1"), IsError("side effects are not supported"));

  EXPECT_THAT(Eval("1 *= 1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i *= 1"), IsError("side effects are not supported"));

  EXPECT_THAT(Eval("1 /= 1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i /= 1"), IsError("side effects are not supported"));

  EXPECT_THAT(Eval("1 %= 1"), IsError("expression is not assignable"));
  EXPECT_THAT(Eval("i %= 1"), IsError("side effects are not supported"));
  EXPECT_THAT(
      Eval("f %= 1"),
      IsError("invalid operands to binary expression ('float' and 'int')"));

  ASSERT_TRUE(CreateContextVariable("Enum", "$e", false, "Enum::ONE"));
  EXPECT_THAT(
      EvalWithContext("$e *= 1", vars_),
      // TODO(werat): This should actually be:
      // > assigning to 'Enum' from incompatible type 'int'
      IsError("invalid operands to binary expression ('Enum' and 'int')"));

  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("($i += 1) -= 2", vars_),
              IsError("side effects are not supported in this context"));
}

TEST_F(EvalTest, TestCompositeAssignmentAdd) {
  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("$i += 1", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i += 2", vars_), IsEqual("4"));
  EXPECT_THAT(EvalWithContext("$i += -4", vars_), IsEqual("0"));
  EXPECT_THAT(EvalWithContext("$i += eOne", vars_), IsEqual("0"));
  EXPECT_THAT(EvalWithContext("$i += eTwo", vars_), IsEqual("1"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(EvalWithContext("$f += 1", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("$f += -2", vars_), IsEqual("0.5"));
  EXPECT_THAT(EvalWithContext("$f += 2.5", vars_), IsEqual("3"));
  EXPECT_THAT(EvalWithContext("$f += eTwo", vars_), IsEqual("4"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s += 1000", vars_), IsEqual("1100"));
  EXPECT_THAT(EvalWithContext("$s += 100000", vars_), IsEqual("-29972"));

  ASSERT_TRUE(CreateContextVariable("$p", "(int*)10"));
  EXPECT_THAT(EvalWithContext("$p += 1", vars_), IsEqual("0x000000000000000e"));
  EXPECT_THAT(
      EvalWithContext("$p += 1.5", vars_),
      IsError("invalid operands to binary expression ('int *' and 'double')"));
  EXPECT_THAT(
      EvalWithContext("$p += $p", vars_),
      IsError("invalid operands to binary expression ('int *' and 'int *')"));
  EXPECT_THAT(EvalWithContext("$i += $p", vars_),
              IsError("no known conversion from 'int *' to 'int'"));
}

TEST_F(EvalTest, TestCompositeAssignmentSub) {
  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("$i -= 1", vars_), IsEqual("0"));
  EXPECT_THAT(EvalWithContext("$i -= 2", vars_), IsEqual("-2"));
  EXPECT_THAT(EvalWithContext("$i -= -4", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i -= eOne", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i -= eTwo", vars_), IsEqual("1"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(EvalWithContext("$f -= 1", vars_), IsEqual("0.5"));
  EXPECT_THAT(EvalWithContext("$f -= -2", vars_), IsEqual("2.5"));
  EXPECT_THAT(EvalWithContext("$f -= -2.5", vars_), IsEqual("5"));
  EXPECT_THAT(EvalWithContext("$f -= eTwo", vars_), IsEqual("4"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s -= 1000", vars_), IsEqual("-900"));
  EXPECT_THAT(EvalWithContext("$s -= 100000", vars_), IsEqual("30172"));

  ASSERT_TRUE(CreateContextVariable("$p", "(int*)10"));
  EXPECT_THAT(EvalWithContext("$p -= 1", vars_), IsEqual("0x0000000000000006"));

#ifdef _WIN32
  // On Windows, 'ptrdiff_t' is 'long long'.
  EXPECT_THAT(EvalWithContext("$p -= $p", vars_),
              IsError("no known conversion from 'long long' to 'int *'"));
#else
  // On Linux, 'ptrdiff_t' is 'long'.
  EXPECT_THAT(EvalWithContext("$p -= $p", vars_),
              IsError("no known conversion from 'long' to 'int *'"));
#endif
}

TEST_F(EvalTest, TestCompositeAssignmentMul) {
  ASSERT_TRUE(CreateContextVariable("$i", "1"));
  EXPECT_THAT(EvalWithContext("$i *= 1", vars_), IsEqual("1"));
  EXPECT_THAT(EvalWithContext("$i *= 2", vars_), IsEqual("2"));
  EXPECT_THAT(EvalWithContext("$i *= -2.5", vars_), IsEqual("-5"));
  EXPECT_THAT(EvalWithContext("$i *= eTwo", vars_), IsEqual("-5"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(EvalWithContext("$f *= 1", vars_), IsEqual("1.5"));
  EXPECT_THAT(EvalWithContext("$f *= 2", vars_), IsEqual("3"));
  EXPECT_THAT(EvalWithContext("$f *= -2.5", vars_), IsEqual("-7.5"));
  EXPECT_THAT(EvalWithContext("$f *= eTwo", vars_), IsEqual("-7.5"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s *= 1000", vars_), IsEqual("-31072"));
  EXPECT_THAT(EvalWithContext("$s *= -1000", vars_), IsEqual("7936"));
}

TEST_F(EvalTest, TestCompositeAssignmentDiv) {
  ASSERT_TRUE(CreateContextVariable("$i", "15"));
  EXPECT_THAT(EvalWithContext("$i /= 1", vars_), IsEqual("15"));
  EXPECT_THAT(EvalWithContext("$i /= 2", vars_), IsEqual("7"));
  EXPECT_THAT(EvalWithContext("$i /= -2", vars_), IsEqual("-3"));
  EXPECT_THAT(EvalWithContext("$i /= eTwo", vars_), IsEqual("-3"));

  ASSERT_TRUE(CreateContextVariable("$f", "15.5f"));
  EXPECT_THAT(EvalWithContext("$f /= 1", vars_), IsEqual("15.5"));
  EXPECT_THAT(EvalWithContext("$f /= 2", vars_), IsEqual("7.75"));
  EXPECT_THAT(EvalWithContext("$f /= -2.5", vars_), IsEqual("-3.0999999"));
  EXPECT_THAT(EvalWithContext("$f /= eTwo", vars_), IsEqual("-3.0999999"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s /= 10", vars_), IsEqual("10"));
  EXPECT_THAT(EvalWithContext("$s /= -3", vars_), IsEqual("-3"));
}

TEST_F(EvalTest, TestCompositeAssignmentRem) {
  ASSERT_TRUE(CreateContextVariable("$i", "15"));
  EXPECT_THAT(EvalWithContext("$i %= 8", vars_), IsEqual("7"));
  EXPECT_THAT(EvalWithContext("$i %= -3", vars_), IsEqual("1"));
  EXPECT_THAT(EvalWithContext("$i %= eTwo", vars_), IsEqual("0"));

  ASSERT_TRUE(CreateContextVariable("$f", "15.5f"));
  EXPECT_THAT(
      EvalWithContext("$f %= 1", vars_),
      IsError("invalid operands to binary expression ('float' and 'int')"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s %= 23", vars_), IsEqual("8"));
}

TEST_F(EvalTest, TestCompositeAssignmentBitwise) {
  ASSERT_TRUE(CreateContextVariable("$i", "0b11111111"));
  EXPECT_THAT(EvalWithContext("$i &= 0b11110000", vars_), IsEqual("240"));
  EXPECT_THAT(EvalWithContext("$i |= 0b01100011", vars_), IsEqual("243"));
  EXPECT_THAT(EvalWithContext("$i ^= 0b00100010", vars_), IsEqual("209"));
  EXPECT_THAT(EvalWithContext("$i <<= 2", vars_), IsEqual("836"));
  EXPECT_THAT(EvalWithContext("$i >>= 3", vars_), IsEqual("104"));
  EXPECT_THAT(EvalWithContext("$i <<= eTwo", vars_), IsEqual("208"));
  EXPECT_THAT(EvalWithContext("$i >>= eTwo", vars_), IsEqual("104"));
  EXPECT_THAT(EvalWithContext("$i <<= 1U", vars_), IsEqual("208"));
  EXPECT_THAT(EvalWithContext("$i >>= 1LL", vars_), IsEqual("104"));

  ASSERT_TRUE(CreateContextVariable("$c", "(signed char)-1"));
  EXPECT_THAT(EvalWithContext("$c &= 0b11110011", vars_), IsEqual("'\\xf3'"));
  EXPECT_THAT(EvalWithContext("$c |= 0b01001011", vars_), IsEqual("'\\xfb'"));
  EXPECT_THAT(EvalWithContext("$c ^= 0b00000110", vars_), IsEqual("'\\xfd'"));
  EXPECT_THAT(EvalWithContext("$c <<= 5", vars_), IsEqual("'\\xa0'"));
  EXPECT_THAT(EvalWithContext("$c >>= 20", vars_), IsEqual("'\\xff'"));
  EXPECT_THAT(EvalWithContext("$c <<= 2U", vars_), IsEqual("'\\xfc'"));
  EXPECT_THAT(EvalWithContext("$c >>= eTwo", vars_), IsEqual("'\\xfe'"));

  ASSERT_TRUE(CreateContextVariable("$f", "1.5f"));
  EXPECT_THAT(
      EvalWithContext("$f &= 1", vars_),
      IsError("invalid operands to binary expression ('float' and 'int')"));
  EXPECT_THAT(
      EvalWithContext("$f |= 1", vars_),
      IsError("invalid operands to binary expression ('float' and 'int')"));
  EXPECT_THAT(
      EvalWithContext("$f ^= 1", vars_),
      IsError("invalid operands to binary expression ('float' and 'int')"));
  EXPECT_THAT(
      EvalWithContext("$f <<= 1", vars_),
      IsError("invalid operands to binary expression ('float' and 'int')"));
  EXPECT_THAT(
      EvalWithContext("$f >>= 1", vars_),
      IsError("invalid operands to binary expression ('float' and 'int')"));

  ASSERT_TRUE(CreateContextVariable("$s", "(short)100"));
  EXPECT_THAT(EvalWithContext("$s >>= 2", vars_), IsEqual("25"));
  EXPECT_THAT(EvalWithContext("$s <<= 6", vars_), IsEqual("1600"));
  EXPECT_THAT(EvalWithContext("$s <<= 12", vars_), IsEqual("0"));

  ASSERT_TRUE(CreateContextVariable("$e", "eTwo"));
  std::vector<std::string> ops = {"&=", "|=", "^=", "<<=", ">>="};
  for (const auto& op : ops) {
    const std::string expr = "$e " + op + " 1";  // e.g. "$e ^= 1"
    EXPECT_THAT(
        EvalWithContext(expr, vars_),
        IsError("invalid operands to binary expression ('Enum' and 'int')"));
  }

  ASSERT_TRUE(CreateContextVariable("$p", "(int*)10"));
  EXPECT_THAT(
      EvalWithContext("$p &= 1", vars_),
      IsError("invalid operands to binary expression ('int *' and 'int')"));
  EXPECT_THAT(
      EvalWithContext("$p |= (char)1", vars_),
      IsError("invalid operands to binary expression ('int *' and 'char')"));
  EXPECT_THAT(
      EvalWithContext("$p ^= &f", vars_),
      IsError("invalid operands to binary expression ('int *' and 'float *')"));
  EXPECT_THAT(
      EvalWithContext("$p <<= 1", vars_),
      IsError("invalid operands to binary expression ('int *' and 'int')"));
  EXPECT_THAT(
      EvalWithContext("$p >>= $p", vars_),
      IsError("invalid operands to binary expression ('int *' and 'int *')"));
}

TEST_F(EvalTest, TestSideEffects) {
  // Comparing with LLDB is not possible with side effects enabled -- results
  // will always be different (because the same expression is evaluated twice).
  this->compare_with_lldb_ = false;
  this->allow_side_effects_ = true;

  EXPECT_THAT(Eval("x++"), IsEqual("1"));
  EXPECT_THAT(Eval("x"), IsEqual("2"));
  EXPECT_THAT(Eval("++x"), IsEqual("3"));

  EXPECT_THAT(Eval("xa[0] = 4"), IsEqual("4"));
  EXPECT_THAT(Eval("xa[0]"), IsEqual("4"));
  EXPECT_THAT(Eval("xa[1] += xa[0]"), IsEqual("6"));

  EXPECT_THAT(Eval("*p = 5.2"), IsEqual("5"));
  EXPECT_THAT(Eval("*p"), IsEqual("5"));
  EXPECT_THAT(Eval("x"), IsEqual("5"));  // `p` is `&x`
}

TEST_F(EvalTest, TestBuiltinFunction_findnonnull) {
  // LLDB doesn't support `__findnonnull` intrinsic function.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("__findnonnull(array_of_pointers, 0)"), IsEqual("-1"));
  EXPECT_THAT(Eval("__findnonnull(array_of_pointers, 1)"), IsEqual("0"));
  EXPECT_THAT(Eval("__findnonnull(array_of_pointers, 5)"), IsEqual("0"));
  EXPECT_THAT(Eval("__findnonnull(array_of_pointers+2, 3)"), IsEqual("2"));
  EXPECT_THAT(Eval("__findnonnull(array_of_pointers+3, 2)"), IsEqual("1"));

  EXPECT_THAT(Eval("__findnonnull(pointer_to_pointers, 0)"), IsEqual("-1"));
  EXPECT_THAT(Eval("__findnonnull(pointer_to_pointers, 1)"), IsEqual("0"));
  EXPECT_THAT(Eval("__findnonnull(pointer_to_pointers, 5)"), IsEqual("0"));
  EXPECT_THAT(Eval("__findnonnull(pointer_to_pointers+2, 3)"), IsEqual("2"));
  EXPECT_THAT(Eval("__findnonnull(pointer_to_pointers+3, 2)"), IsEqual("1"));

  EXPECT_THAT(Eval("__findnonnull(0, 0)"),
              IsError("no known conversion from 'int' to 'T*' for 1st argument "
                      "of __findnonnull()\n"
                      "__findnonnull(0, 0)\n"
                      "              ^"));
  EXPECT_THAT(
      Eval("__findnonnull(1.0f, 0)"),
      IsError("no known conversion from 'float' to 'T*' for 1st argument "
              "of __findnonnull()\n"
              "__findnonnull(1.0f, 0)\n"
              "              ^"));
  EXPECT_THAT(
      Eval("__findnonnull(array_of_pointers, -1)"),
      IsError("passing in a buffer size ('-1') that is negative or in excess "
              "of 100 million to __findnonnull() is not allowed.\n"
              "__findnonnull(array_of_pointers, -1)\n"
              "                                 ^"));
  EXPECT_THAT(
      Eval("__findnonnull(array_of_pointers, 100000001)"),
      IsError("passing in a buffer size ('100000001') that is negative "
              "or in excess of 100 million to __findnonnull() is not allowed.\n"
              "__findnonnull(array_of_pointers, 100000001)\n"
              "                                 ^"));

  if (process_.GetAddressByteSize() == 4) {
    EXPECT_THAT(Eval("__findnonnull(array_of_uint8, 6)"), IsEqual("0"));
    EXPECT_THAT(Eval("__findnonnull(array_of_uint8+4, 5)"), IsEqual("0"));
    EXPECT_THAT(Eval("__findnonnull(array_of_uint8+8, 4)"), IsEqual("2"));
    EXPECT_THAT(Eval("__findnonnull(array_of_uint8+12, 3)"), IsEqual("1"));
  } else if (process_.GetAddressByteSize() == 8) {
    EXPECT_THAT(Eval("__findnonnull(array_of_uint8, 3)"), IsEqual("0"));
    EXPECT_THAT(Eval("__findnonnull(array_of_uint8+8, 2)"), IsEqual("1"));
  } else {
    GTEST_FATAL_FAILURE_("Unknown pointer size in the target process");
  }
}

TEST_F(EvalTest, TestUniquePtr) {
#ifdef _WIN32
  // On Windows we're not using `libc++` and therefore the layout of
  // `std::unique_ptr` is different.
  GTEST_SKIP() << "not supported on Windows";
#else
  // On Linux this assumes the usage of libc++ standard library.

  EXPECT_THAT(Eval("*(NodeU**)&ptr_node.__ptr_"), IsOk());
  EXPECT_THAT(Eval("(*(NodeU**)&ptr_node.__ptr_)->value"), IsEqual("1"));

  EXPECT_THAT(Eval("ptr_node.__ptr_.__value_"), IsOk());
  EXPECT_THAT(Eval("ptr_node.__ptr_.__value_->value"), IsEqual("1"));
  EXPECT_THAT(Eval("ptr_node.__ptr_.__value_->next.__ptr_.__value_->value"),
              IsEqual("2"));
#endif
}

TEST_F(EvalTest, TestUniquePtrCompare) {
#ifdef _WIN32
  // On Windows we're not using `libc++` and therefore the layout of
  // `std::unique_ptr` is different.
  GTEST_SKIP() << "not supported on Windows";
#else
  // On Linux this assumes the usage of libc++ standard library.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("ptr_int == nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_int != nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_int == ptr_int"), IsEqual("true"));

  // C++ doesn't allow comparing unique_ptr with raw pointers, but we allow it
  // for convenience.
  EXPECT_THAT(Eval("ptr_int == 0"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_int == (int*)0"), IsEqual("false"));

  EXPECT_THAT(Eval("ptr_float == nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_float != nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_float == (float*)0"), IsEqual("false"));

  EXPECT_THAT(Eval("ptr_float == (int*)0"),
              IsError("comparison of distinct pointer types"));
  EXPECT_THAT(Eval("ptr_int == ptr_float"),
              IsError("comparison of distinct pointer types"));

  EXPECT_THAT(Eval("ptr_null == nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_null != nullptr"), IsEqual("false"));

  EXPECT_THAT(Eval("ptr_void == nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_void != nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_void == ptr_void"), IsEqual("true"));

  // Void pointer can be compared with everything.
  EXPECT_THAT(Eval("ptr_void == (int*)0"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_void == (void*)0"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_int == ptr_void"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_float == ptr_void"), IsEqual("false"));

#endif
}

TEST_F(EvalTest, TestSharedPtr) {
#ifdef _WIN32
  // On Windows we're not using `libc++` and therefore the layout of
  // `std::unique_ptr` is different.
  GTEST_SKIP() << "not supported on Windows";
#else
  // On Linux this assumes the usage of libc++ standard library.

  EXPECT_THAT(Eval("*(NodeS**)&ptr_node.__ptr_"), IsOk());
  EXPECT_THAT(Eval("(*(NodeS**)&ptr_node.__ptr_)->value"), IsEqual("1"));

  EXPECT_THAT(Eval("ptr_node.__ptr_"), IsOk());
  EXPECT_THAT(Eval("ptr_node.__ptr_->value"), IsEqual("1"));
  EXPECT_THAT(Eval("ptr_node.__ptr_->next.__ptr_->value"), IsEqual("2"));

  EXPECT_THAT(Eval("ptr_int.__ptr_"), IsOk());
  EXPECT_THAT(Eval("*ptr_int.__ptr_"), IsEqual("1"));
  EXPECT_THAT(Eval("ptr_int_weak.__ptr_"), IsOk());
  EXPECT_THAT(Eval("*ptr_int_weak.__ptr_"), IsEqual("1"));
#endif
}

TEST_F(EvalTest, TestSharedPtrCompare) {
#ifdef _WIN32
  // On Windows we're not using `libc++` and therefore the layout of
  // `std::shared_ptr` is different.
  GTEST_SKIP() << "not supported on Windows";
#else
  // On Linux this assumes the usage of libc++ standard library.
  this->compare_with_lldb_ = false;

  EXPECT_THAT(Eval("ptr_int == nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_int != nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_int == ptr_int"), IsEqual("true"));

  // C++ doesn't allow comparing shared_ptr with raw pointers, but we allow it
  // for convenience.
  EXPECT_THAT(Eval("ptr_int == 0"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_int == (int*)0"), IsEqual("false"));

  EXPECT_THAT(Eval("ptr_float == nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_float != nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_float == (float*)0"), IsEqual("false"));

  EXPECT_THAT(Eval("ptr_float == (int*)0"),
              IsError("comparison of distinct pointer types"));
  EXPECT_THAT(Eval("ptr_int == ptr_float"),
              IsError("comparison of distinct pointer types"));

  EXPECT_THAT(Eval("ptr_null == nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_null != nullptr"), IsEqual("false"));

  EXPECT_THAT(Eval("ptr_void == nullptr"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_void != nullptr"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_void == ptr_void"), IsEqual("true"));

  // Void pointer can be compared with everything.
  EXPECT_THAT(Eval("ptr_void == (int*)0"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_void == (void*)0"), IsEqual("false"));
  EXPECT_THAT(Eval("ptr_int == ptr_void"), IsEqual("true"));
  EXPECT_THAT(Eval("ptr_float == ptr_void"), IsEqual("false"));
#endif
}
