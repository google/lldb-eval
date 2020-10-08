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

#include "eval.h"

#include <memory>
#include <string>

#include "ast.h"
#include "expression_context.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBExecutionContext.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "parser.h"
#include "runner.h"
#include "tools/cpp/runfiles/runfiles.h"
#include "value.h"

// DISALLOW_COPY_AND_ASSIGN is also defined in
// lldb/lldb-defines.h
#undef DISALLOW_COPY_AND_ASSIGN
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace {

using bazel::tools::cpp::runfiles::Runfiles;

class InterpreterTest : public ::testing::Test {
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

    debugger_ = lldb::SBDebugger::Create(false);
    process_ = lldb_eval::LaunchTestProgram(*runfiles_, debugger_, break_line);
    frame_ = process_.GetSelectedThread().GetSelectedFrame();

    // Evaluate expressions with both lldb-eval and LLDB by default.
    skip_lldb = false;
    expect_error_lldb = false;
  }

  void TearDown() { process_.Destroy(); }

  void EvaluateLldbEval(const std::string& expr, lldb::SBValue& result);
  void EvaluateLldb(const std::string& expr, lldb::SBValue& result);

  void TestExpr(const std::string& expr, const std::string& expected_result);

  void TestExprOnlyCompare(const std::string& expr);
  void TestExprErr(const std::string& expr, const std::string& msg);

 protected:
  lldb::SBDebugger debugger_;
  lldb::SBProcess process_;
  lldb::SBFrame frame_;

  static Runfiles* runfiles_;

 private:
  friend class SkipLLDB;
  friend class ExpectErrorLLDB;

  bool skip_lldb;
  bool expect_error_lldb;
};

class SkipLLDB {
 public:
  SkipLLDB(InterpreterTest* test) : test_(test) { test_->skip_lldb = true; }

  ~SkipLLDB() { test_->skip_lldb = false; }

 private:
  InterpreterTest* test_;
};

class ExpectErrorLLDB {
 public:
  ExpectErrorLLDB(InterpreterTest* test) : test_(test) {
    test_->expect_error_lldb = true;
  }

  ~ExpectErrorLLDB() { test_->expect_error_lldb = false; }

 private:
  InterpreterTest* test_;
};

Runfiles* InterpreterTest::runfiles_ = nullptr;

void InterpreterTest::EvaluateLldbEval(const std::string& expr,
                                       lldb::SBValue& result) {
  lldb_eval::ExpressionContext expr_ctx(expr, lldb::SBExecutionContext(frame_));
  lldb_eval::Parser p(expr_ctx);
  auto expr_result = p.Run();
  ASSERT_FALSE(p.HasError()) << p.GetError();
  lldb_eval::EvalError error;
  lldb_eval::Interpreter interpreter(expr_ctx);
  auto ret = interpreter.Eval(expr_result.get(), error);
  EXPECT_EQ(error.code(), lldb_eval::EvalErrorCode::OK);
  EXPECT_EQ(error.message(), "");
  result = ret.AsSbValue(expr_ctx.GetExecutionContext().GetTarget());
}

void InterpreterTest::EvaluateLldb(const std::string& expr,
                                   lldb::SBValue& result) {
  result = frame_.EvaluateExpression(expr.c_str());
  EXPECT_STREQ(result.GetError().GetCString(), NULL);
}

void InterpreterTest::TestExpr(const std::string& expr,
                               const std::string& expected_result) {
  {
    SCOPED_TRACE("[Evaluate with lldb-eval]: " + expr);
    lldb::SBValue result;
    EvaluateLldbEval(expr, result);
    EXPECT_STREQ(result.GetValue(), expected_result.c_str());
  }

  if (!skip_lldb) {
    SCOPED_TRACE("[Evaluate with LLDB]: " + expr);
    lldb::SBValue result = frame_.EvaluateExpression(expr.c_str());

    if (expect_error_lldb) {
      EXPECT_STRNE(result.GetError().GetCString(), NULL);
      EXPECT_STREQ(result.GetValue(), NULL);
    } else {
      EXPECT_STREQ(result.GetError().GetCString(), NULL);
      EXPECT_STREQ(result.GetValue(), expected_result.c_str());
    }
  }
}

void InterpreterTest::TestExprOnlyCompare(const std::string& expr) {
  SCOPED_TRACE("[Compare LLDB and lldb-eval]: " + expr);
  lldb::SBValue lldb_native_result, lldb_eval_result;
  EvaluateLldb(expr, lldb_native_result);
  EvaluateLldbEval(expr, lldb_eval_result);
  EXPECT_STREQ(lldb_native_result.GetValue(), lldb_eval_result.GetValue());
}

void InterpreterTest::TestExprErr(const std::string& expr,
                                  const std::string& msg) {
  SCOPED_TRACE("[Evaluate with lldb-eval]: " + expr);
  lldb_eval::ExpressionContext expr_ctx(expr, lldb::SBExecutionContext(frame_));
  lldb_eval::Parser p(expr_ctx);
  auto expr_result = p.Run();
  ASSERT_FALSE(p.HasError()) << p.GetError();
  lldb_eval::EvalError error;
  lldb_eval::Interpreter interpreter(expr_ctx);
  auto ret = interpreter.Eval(expr_result.get(), error);
  EXPECT_THAT(error.message(), ::testing::HasSubstr(msg));
}

TEST_F(InterpreterTest, TestArithmetic) {
  TestExpr("1 + 2", "3");
  TestExpr("1 + 2*3", "7");
  TestExpr("1 + (2 - 3)", "0");
  TestExpr("1 == 2", "false");
  TestExpr("1 == 1", "true");

  // Note: Signed overflow is UB.
  TestExprOnlyCompare("int_max + 1");
  TestExprOnlyCompare("int_min - 1");
  TestExprOnlyCompare("2147483647 + 1");
  TestExprOnlyCompare("-2147483648 - 1");

  TestExpr("uint_max + 1", "0");
  TestExpr("uint_zero - 1", "4294967295");
  TestExpr("4294967295 + 1", "4294967296");
  TestExpr("4294967295U + 1", "0");

  // Note: Signed overflow is UB.
  TestExprOnlyCompare("ll_max + 1");
  TestExprOnlyCompare("ll_min - 1");
  TestExprOnlyCompare("9223372036854775807 + 1");
  TestExprOnlyCompare("-9223372036854775808 - 1");

  TestExpr("ull_max + 1", "0");
  TestExpr("ull_zero - 1", "18446744073709551615");
  TestExpr("9223372036854775807 + 1", "-9223372036854775808");
  TestExpr("9223372036854775807LL + 1", "-9223372036854775808");
  TestExpr("18446744073709551615ULL + 1", "0");

  TestExpr("-20 / 1U", "4294967276");
  TestExpr("-20LL / 1U", "-20");
  TestExpr("-20LL / 1ULL", "18446744073709551596");
}

TEST_F(InterpreterTest, TestPointerArithmetic) {
  TestExprOnlyCompare("p_char1");
  TestExprOnlyCompare("p_char1 + 1");
  TestExprOnlyCompare("p_char1 + offset");

  TestExpr("*(p_char1 + 0)", "'h'");
  TestExpr("*(1 + p_char1)", "'e'");
  TestExpr("*(p_char1 + 2)", "'l'");
  TestExpr("*(3 + p_char1)", "'l'");
  TestExpr("*(p_char1 + 4)", "'o'");
  TestExpr("*(p_char1 + offset - 1)", "'o'");

  TestExpr("*p_int0", "0");
  TestExpr("*cp_int5", "5");
  TestExpr("*(&*(cp_int5 + 1) - 1)", "5");

  TestExpr("p_int0 - p_int0", "0");
  TestExpr("cp_int5 - p_int0", "5");
  TestExpr("cp_int5 - td_int_ptr0", "5");
  TestExpr("td_int_ptr0 - cp_int5", "-5");

  TestExprErr("-p_char1",
              "invalid argument type 'const char *' to unary expression");
  TestExprErr(
      "cp_int5 - p_char1",
      "'const int *' and 'const char *' are not pointers to compatible types");
  TestExprErr(
      "p_int0 + cp_int5",
      "invalid operands to binary expression ('int *' and 'const int *')");
  TestExprErr(
      "p_int0 > p_char1",
      "comparison of distinct pointer types ('int *' and 'const char *')");

  TestExpr("cp_int5 > td_int_ptr0", "true");
  TestExpr("cp_int5 < td_int_ptr0", "false");
  TestExpr("cp_int5 != td_int_ptr0", "true");
  TestExpr("cp_int5 == td_int_ptr0 + offset", "true");

  TestExpr("p_void == p_void", "true");
  TestExpr("p_void == p_char1", "true");
  TestExpr("p_void != p_char1", "false");
  TestExpr("p_void > p_char1", "false");
  TestExpr("p_void >= p_char1", "true");
  TestExpr("p_void < (p_char1 + 1)", "true");
  TestExpr("pp_void0 + 1 == pp_void1", "true");

  TestExprErr("p_void + 1", "arithmetic on a pointer to void");
  TestExprErr("p_void - 1", "arithmetic on a pointer to void");
  TestExprErr(
      "p_void - p_char1",
      "'void *' and 'const char *' are not pointers to compatible types");
  TestExprErr("p_void - p_void", "arithmetic on pointers to void");

  TestExprErr(
      "pp_void0 - p_char1",
      "'void **' and 'const char *' are not pointers to compatible types");
  TestExprErr(
      "pp_void0 == p_char1",
      "comparison of distinct pointer types ('void **' and 'const char *')");
}

TEST_F(InterpreterTest, TestLogicalOperators) {
  TestExpr("1 > 2", "false");
  TestExpr("1 == 1", "true");
  TestExpr("1 > 0.1", "true");
  TestExpr("1 && 2", "true");
  TestExpr("0 && 1", "false");
  TestExpr("0 || 1", "true");
  TestExpr("0 || 0", "false");

  TestExpr("trueVar && true", "true");
  TestExpr("trueVar && (2 > 1)", "true");
  TestExpr("trueVar && (2 < 1)", "false");

  TestExpr("falseVar || true", "true");
  TestExpr("falseVar && true", "false");
  TestExpr("falseVar || (2 > 1)", "true");
  TestExpr("falseVar || (2 < 1)", "false");

  {
    // TODO(b/155864809): This should fail due to the symbol resolution and type
    // checking. Right now lldb-eval doesn't evaluate the second exprssion.
    SkipLLDB _(this);

    TestExpr("true || __doesnt_exist", "true");
    TestExpr("false && __doesnt_exist", "false");
  }

  TestExpr("p_ptr && true", "true");
  TestExpr("p_ptr && false", "false");
  TestExpr("p_nullptr || true", "true");
  TestExpr("p_nullptr || false", "false");

  TestExprErr("s || false",
              "value of type 'S' is not contextually convertible to 'bool'");
  TestExprErr("s ? 1 : 2",
              "value of type 'S' is not contextually convertible to 'bool'");
}

TEST_F(InterpreterTest, TestLocalVariables) {
  TestExpr("a", "1");
  TestExpr("b", "2");
  TestExpr("a + b", "3");

  TestExpr("c + 1", "-2");
  TestExpr("s + 1", "5");
  TestExpr("c + s", "1");

  TestExprErr("__test_non_variable + 1",
              "use of undeclared identifier '__test_non_variable'");
}

TEST_F(InterpreterTest, TestInstanceVariables) {
  TestExpr("this->field_", "1");
  TestExprErr("this.field_",
              "member reference type 'TestMethods *' is a pointer; did you "
              "mean to use '->'?");

  TestExpr("c.field_", "-1");
  TestExpr("c_ref.field_", "-1");
  TestExpr("c_ptr->field_", "-1");
  TestExprErr(
      "c->field_",
      "member reference type 'C' is not a pointer; did you mean to use '.'?");
}

TEST_F(InterpreterTest, TestIndirection) {
  TestExpr("*p", "1");
  TestExprErr("*1", "indirection requires pointer operand. ('int' invalid)");
}

TEST_F(InterpreterTest, TestAddressOf) {
  TestExprOnlyCompare("&globalVar");
  TestExprOnlyCompare("&externGlobalVar");
  TestExprOnlyCompare("&s_str");
  TestExprOnlyCompare("&param");

  TestExprErr("&this",
              "cannot take the address of an rvalue of type 'TestMethods *'");
  TestExprErr("&(&s_str)",
              "cannot take the address of an rvalue of type 'const char **'");
}

TEST_F(InterpreterTest, TestSubscript) {
  // const char*
  TestExpr("char_ptr[0]", "'l'");
  TestExpr("1[char_ptr]", "'o'");

  // const char[]
  TestExpr("char_arr[0]", "'i'");
  TestExpr("1[char_arr]", "'p'");

  // Boolean types are integral too!
  TestExpr("int_arr[false]", "1");
  TestExpr("true[int_arr]", "2");

  // But floats are not.
  TestExprErr("int_arr[1.0]", "array subscript is not an integer");

  // Base should be a "pointer to T" and index should be of an integral type.
  TestExprErr("char_arr[char_ptr]", "array subscript is not an integer");
  TestExprErr("1[2]", "subscripted value is not an array or pointer");

  // Test when base and index are references.
  TestExpr("c_arr[0].field_", "0");
  TestExpr("c_arr[idx_1_ref].field_", "1");
  TestExpr("c_arr_ref[0].field_", "0");
  TestExpr("c_arr_ref[idx_1_ref].field_", "1");

  // Test when base and index are typedefs.
  TestExpr("td_int_arr[0]", "1");
  TestExpr("td_int_arr[td_int_idx_1]", "2");
  TestExpr("td_int_arr[td_td_int_idx_2]", "3");
  TestExpr("td_int_ptr[0]", "1");
  TestExpr("td_int_ptr[td_int_idx_1]", "2");
  TestExpr("td_int_ptr[td_td_int_idx_2]", "3");
  // Both typedefs and refs!
  TestExpr("td_int_arr_ref[td_int_idx_1_ref]", "2");

  // Test for index out of bounds.
  TestExprOnlyCompare("int_arr[42]");
  TestExprOnlyCompare("int_arr[100]");

  // Test for negative index.
  TestExprOnlyCompare("int_arr[-1]");
  TestExprOnlyCompare("int_arr[-42]");

  // Test for "max unsigned char".
  TestExpr("uint8_arr[uchar_idx]", "'\\xab'");

  // Test address-of of the subscripted value.
  TestExpr("(&c_arr[1])->field_", "1");
}

TEST_F(InterpreterTest, TestCStyleCastBasicType) {
  // Test with integer literals.
  TestExpr("(char)1", "'\\x01'");
  TestExpr("(unsigned char)-1", "'\\xff'");
  TestExpr("(short)-1", "-1");
  TestExpr("(unsigned short)-1", "65535");
  TestExpr("(long long)1", "1");
  TestExpr("(unsigned long long)-1", "18446744073709551615");
  TestExpr("(short)65534", "-2");
  TestExpr("(unsigned short)100000", "34464");
  TestExpr("(float)1", "1");
  TestExpr("(float)1.1", "1.10000002");
  TestExpr("(float)1.1f", "1.10000002");
  TestExpr("(float)-1.1", "-1.10000002");
  TestExpr("(float)-1.1f", "-1.10000002");
  TestExpr("(double)1", "1");
  TestExpr("(double)1.1", "1.1000000000000001");
  TestExpr("(double)1.1f", "1.1000000238418579");
  TestExpr("(double)-1.1", "-1.1000000000000001");
  TestExpr("(double)-1.1f", "-1.1000000238418579");
  TestExpr("(int)1.1", "1");
  TestExpr("(int)1.1f", "1");
  TestExpr("(int)-1.1", "-1");
  TestExpr("(long)1.1", "1");
  TestExpr("(long)-1.1f", "-1");

  // Test with variables.
  TestExpr("(char)a", "'\\x01'");
  TestExpr("(unsigned char)na", "'\\xff'");
  TestExpr("(short)na", "-1");
  TestExpr("(unsigned short)-a", "65535");
  TestExpr("(long long)a", "1");
  TestExpr("(unsigned long long)-1", "18446744073709551615");
  TestExpr("(float)a", "1");
  TestExpr("(float)f", "1.10000002");
  TestExpr("(double)f", "1.1000000238418579");
  TestExpr("(int)f", "1");
  TestExpr("(long)f", "1");

  // Test with typedefs and namespaces.
  TestExpr("(myint)1", "1");
  TestExpr("(myint)1LL", "1");
  TestExpr("(ns::myint)1", "1");
  TestExpr("(::ns::myint)1", "1");
  TestExpr("(::ns::myint)myint_", "1");

  TestExpr("(int)myint_", "1");
  TestExpr("(int)ns_myint_", "2");
  TestExpr("(long long)myint_", "1");
  TestExpr("(long long)ns_myint_", "2");
  TestExpr("(::ns::myint)myint_", "1");

  TestExpr("(ns::inner::mydouble)1", "1");
  TestExpr("(::ns::inner::mydouble)1.2", "1.2");
  TestExpr("(ns::inner::mydouble)myint_", "1");
  TestExpr("(::ns::inner::mydouble)ns_inner_mydouble_", "1.2");
  TestExpr("(myint)ns_inner_mydouble_", "1");

  // Test with pointers.
  TestExprOnlyCompare("(long long)ap");
  TestExprOnlyCompare("(unsigned long long)vp");
  TestExprErr("(char)ap",
              "cast from pointer to smaller type 'char' loses information");
  TestExprErr("(float)ap",
              "C-style cast from 'int *' to 'float' is not allowed");
}

TEST_F(InterpreterTest, TestCStyleCastPointer) {
  TestExprOnlyCompare("(void*)&a");
  TestExprOnlyCompare("(void*)ap");
  TestExprOnlyCompare("(long long*)vp");
  TestExprOnlyCompare("(short int*)vp");
  TestExprOnlyCompare("(unsigned long long*)vp");
  TestExprOnlyCompare("(unsigned short int*)vp");

  TestExpr("*(const int* const)ap", "1");
  TestExpr("*(volatile int* const)ap", "1");
  TestExpr("*(const int* const)vp", "1");
  TestExpr("*(const int* const volatile const)vp", "1");
  TestExpr("*(int*)(void*)ap", "1");
  TestExpr("*(int*)(const void* const volatile)ap", "1");

  TestExprOnlyCompare("(ns::Foo*)ns_inner_foo_ptr_");
  TestExprOnlyCompare("(ns::inner::Foo*)ns_foo_ptr_");

  TestExprErr("(int*&)ap",
              "casting of 'int *' to 'int *&' is not implemented yet");
  TestExprErr("(int& &)ap", "type name declared as a reference to a reference");
  TestExprErr(
      "(int&*)ap",
      "'type name' declared as a pointer to a reference of type 'int &'");
}

TEST_F(InterpreterTest, TestQualifiedId) {
  TestExpr("::ns::i", "1");
  TestExpr("ns::i", "1");
  TestExpr("::ns::ns::i", "2");
  TestExpr("ns::ns::i", "2");

  {
#ifdef _WIN32
    ExpectErrorLLDB _(this);
#endif

    TestExpr("::Foo::y", "42");
    TestExpr("Foo::y", "42");
  }

  // Static consts with no definition can't be looked up by name.
  TestExprErr("::Foo::x", "use of undeclared identifier '::Foo::x'");
  TestExprErr("Foo::x", "use of undeclared identifier 'Foo::x'");
}

TEST_F(InterpreterTest, TestTemplateTypes) {
  // Template types lookup doesn't work well in the upstream LLDB.
  SkipLLDB _(this);

  // Get the pointer value and use it to check the expressions with lldb-eval.
  auto expected = frame_.EvaluateExpression("p").GetValue();

  for (std::string arg : {"int", "int*", "int**", "int&", "int*&", "double"}) {
    TestExpr("(T_1<" + arg + ">*)p", expected);
    TestExpr("(::T_1<" + arg + ">*)p", expected);
  }
  TestExpr("(T_2<int, char>*)p", expected);
  TestExpr("(::T_2<int, char>*)p", expected);
  TestExpr("(T_2<char, int>*)p", expected);
  TestExpr("(::T_2<char, int>*)p", expected);
  TestExpr("(T_2<T_1<int>, T_1<char> >*)p", expected);
  TestExpr("(::T_2<T_1<int>, T_1<char> >*)p", expected);
  TestExpr("(T_2<T_1<T_1<int> >, T_1<char> >*)p", expected);
  TestExpr("(::T_2<T_1<T_1<int> >, T_1<char> >*)p", expected);

  TestExpr("(ns::T_1<int>*)p", expected);
  TestExpr("(::ns::T_1<int>*)p", expected);
  TestExpr("(ns::T_1<ns::T_1<int> >*)p", expected);
  TestExpr("(::ns::T_1<ns::T_1<int> >*)p", expected);

#ifdef _WIN32
  TestExprErr("ns::T_1<ns::T_1<int> >::cx",
              "use of undeclared identifier 'ns::T_1<ns::T_1<int> >::cx'");
#else
  TestExpr("ns::T_1<ns::T_1<int> >::cx", "46");
#endif

  TestExpr("T_1<int>::cx", "24");
  TestExpr("T_1<double>::cx", "42");
  TestExpr("ns::T_1<int>::cx", "64");

  for (std::string arg : {"int", "int*", "int**", "int&", "int*&"}) {
    TestExpr("(T_1<" + arg + ">::myint)1.2", "1.2");
    TestExpr("(::T_1<" + arg + ">::myint)1.2", "1.2");
    TestExpr("(T_1<T_1<" + arg + "> >::myint)1.2", "1.2");
    TestExpr("(::T_1<T_1<" + arg + "> >::myint)1.2", "1.2");

    TestExpr("(ns::T_1<" + arg + ">::myint)1.1", "1");
    TestExpr("(::ns::T_1<" + arg + ">::myint)1.1", "1");
    TestExpr("(ns::T_1<T_1<" + arg + "> >::myint)1.1", "1");
    TestExpr("(::ns::T_1<T_1<" + arg + "> >::myint)1.1", "1");
    TestExpr("(ns::T_1<ns::T_1<" + arg + "> >::myint)1.1", "1");
    TestExpr("(::ns::T_1<ns::T_1<" + arg + "> >::myint)1.1", "1");
  }

  TestExpr("(T_2<int, char>::myint)1.1f", "1.10000002");
  TestExpr("(::T_2<int, char>::myint)1.1f", "1.10000002");
  TestExpr("(T_2<int*, char&>::myint)1.1f", "1.10000002");
  TestExpr("(::T_2<int&, char*>::myint)1.1f", "1.10000002");
  TestExpr("(T_2<T_1<T_1<int> >, T_1<char> >::myint)1.1", "1.10000002");
  TestExpr("(::T_2<T_1<T_1<int> >, T_1<char> >::myint)1.1", "1.10000002");
}

}  // namespace
