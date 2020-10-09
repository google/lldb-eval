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

#include "lldb-eval/parser.h"

#include <memory>
#include <string>

#include "lldb-eval/expression_context.h"
#include "lldb/API/SBExecutionContext.h"

// DISALLOW_COPY_AND_ASSIGN is also defined in
// lldb/lldb-defines.h
#undef DISALLOW_COPY_AND_ASSIGN
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace {

using testing::HasSubstr;

lldb_eval::Parser::Error ParseExpr(const std::string& expr) {
  // Use empty lldb::SBExecutionContext for testing. It is used by parser to
  // resolve ambiguous situations (mostly to resolve types/variables in the
  // target context), but not required if the expression is unambiguous.
  lldb_eval::ExpressionContext expr_ctx(expr, lldb::SBExecutionContext());
  lldb_eval::Parser parser(expr_ctx);
  parser.Run();
  return parser.GetError();
}

class ParserTest : public ::testing::Test {
 protected:
  void TestExpr(const std::string& expr) {
    SCOPED_TRACE("[parsing expr]: " + expr);
    auto error = ParseExpr(expr);
    ASSERT_EQ(error, "");
  }

  void TestExprErr(const std::string& expr, const std::string& msg) {
    SCOPED_TRACE("[parsing expr]: " + expr);
    auto error = ParseExpr(expr);
    ASSERT_THAT(error, HasSubstr(msg));
  }
};

TEST_F(ParserTest, TestBasic) {
  TestExprErr("", "Unexpected token: <'' (eof)");
}

TEST_F(ParserTest, TestArithmeric) {
  TestExpr("1 + 2 * (4 - 5) + 6 / 3 - (7 % 8)");
  TestExpr("1 || 2 && 3 >> 4 << 5 * (7 ^ 8)");
}

TEST_F(ParserTest, TestUnbalancedParentheses) {
  auto msg =
      "<expr>:1:11: expected 'r_paren', got: <'' (eof)>\n"
      "1 + (2 - 3 \n"
      "          ^";
  TestExprErr("1 + (2 - 3", msg);
}

TEST_F(ParserTest, TestMemberAccess) { TestExpr("foo->bar.baz"); }

TEST_F(ParserTest, TestMemberAccessInvalid) {
  auto msg =
      "<expr>:1:6: expected 'identifier', got: <'2' (numeric_constant)>\n"
      "foo->2\n"
      "     ^";
  TestExprErr("foo->2", msg);
}

TEST_F(ParserTest, TestCStyleCast) {
  TestExpr("(int)1");
  TestExpr("(long long)1");
  TestExpr("(unsigned long)1");
  TestExpr("(long const const)1");
  TestExpr("(long const long)1");

  TestExpr("(char*)1");
  TestExpr("(long long**)1");
  TestExpr("(long*&)1");
  TestExpr("(long&*)1");
  TestExpr("(const long const long const* const const)1");

  auto msg =
      "<expr>:1:7: expected 'r_paren', got: <'1' (numeric_constant)>\n"
      "(long 1)1\n"
      "      ^";
  TestExprErr("(long 1)1", msg);
}

TEST_F(ParserTest, TestDiagnostics) {
  auto expr_1 =
      ")1 + 2 +\n"
      "3 + 4 +\n"
      "5 + 6\n";
  auto msg_1 =
      "<expr>:1:1: Unexpected token: <')' (r_paren)>\n"
      ")1 + 2 +\n"
      "^       ";
  TestExprErr(expr_1, msg_1);

  auto expr_2 =
      "1 + 2 +\n"
      "3 + foo->4 +\n"
      "5 + 6\n";
  auto msg_2 =
      "<expr>:2:10: expected 'identifier', got: <'4' (numeric_constant)>\n"
      "3 + foo->4 +\n"
      "         ^  ";
  TestExprErr(expr_2, msg_2);

  auto expr_3 =
      "1 + 2 +\n"
      "3 + 4 +\n"
      "5 + 6 +\n";
  auto msg_3 =
      "<expr>:3:8: Unexpected token: <'' (eof)>\n"
      "5 + 6 + \n"
      "       ^";
  TestExprErr(expr_3, msg_3);
}

TEST_F(ParserTest, TestTemplateTypes) {
  TestExprErr("Foo<int()> + 1",
              "<expr>:1:5: Unexpected token: <'int' (int)>\n"
              "Foo<int()> + 1\n"
              "    ^         ");

  TestExprErr("Foo<bar()> + 1",
              "<expr>:1:8: expected 'eof', got: <'(' (l_paren)>\n"
              "Foo<bar()> + 1\n"
              "       ^      ");
}

}  // namespace
