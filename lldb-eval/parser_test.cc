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

#include "lldb-eval/context.h"

// DISALLOW_COPY_AND_ASSIGN is also defined in
// lldb/lldb-defines.h
#undef DISALLOW_COPY_AND_ASSIGN
#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace {

lldb_eval::Error ParseExpr(const std::string& expr) {
  // Use empty lldb::SBFrame for testing. Parser uses it to resolve types and
  // identifiers, but expression in these tests don't have any.
  auto ctx = lldb_eval::Context::Create(expr, lldb::SBFrame());
  lldb_eval::Parser parser(ctx);
  lldb_eval::Error error;
  parser.Run(error);
  return error;
}

class ParserTest : public ::testing::Test {
 protected:
  void TestExpr(const std::string& expr) {
    SCOPED_TRACE("[parsing expr]: " + expr);
    auto error = ParseExpr(expr);
    EXPECT_EQ(error.code(), lldb_eval::ErrorCode::kOk);
    EXPECT_EQ(error.message(), "");
  }

  void TestExprErr(const std::string& expr, const std::string& msg) {
    SCOPED_TRACE("[parsing expr]: " + expr);
    auto error = ParseExpr(expr);
    EXPECT_THAT(error.message(), ::testing::HasSubstr(msg));
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
      "3 + foo +\n"
      "5 + 6\n";
  auto msg_2 =
      "<expr>:2:5: use of undeclared identifier 'foo'\n"
      "3 + foo +\n"
      "    ^    ";
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

}  // namespace
