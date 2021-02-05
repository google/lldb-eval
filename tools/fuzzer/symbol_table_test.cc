/*
 * Copyright 2021 Google LLC
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

#include "tools/fuzzer/symbol_table.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"

using namespace testing;
using bazel::tools::cpp::runfiles::Runfiles;

TEST(SymbolTableTest, CreateFromLldbContext) {
  // Set up the test.
  std::unique_ptr<Runfiles> runfiles(Runfiles::CreateForTest());
  lldb_eval::SetupLLDBServerEnv(*runfiles);
  lldb::SBDebugger::Initialize();
  auto binary_path = runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary");
  auto source_path = runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary.cc");
  auto debugger = lldb::SBDebugger::Create(false);
  auto process = lldb_eval::LaunchTestProgram(debugger, source_path,
                                              binary_path, "// BREAK HERE");
  auto frame = process.GetSelectedThread().GetSelectedFrame();

  // Create a symbol table from LLDB context.
  fuzzer::SymbolTable symtab =
      fuzzer::SymbolTable::create_from_lldb_context(frame);

  size_t count_checked_types = 0;

  auto expect_vars = [&symtab, &count_checked_types](
                         fuzzer::Type type,
                         const std::set<std::string>& names) {
    auto var_it = symtab.vars().find(type);
    ASSERT_NE(var_it, symtab.vars().end());
    std::set<std::string> names_from_symtab;
    for (const auto& var : var_it->second) {
      names_from_symtab.insert(var.expr.name());
    }
    EXPECT_EQ(names, names_from_symtab);
    count_checked_types++;
  };

  // Check contents of the symbol table.
  expect_vars(fuzzer::ScalarType::Char, {"char_min", "char_max"});
  expect_vars(fuzzer::ScalarType::SignedChar, {"schar_min", "schar_max"});
  expect_vars(fuzzer::ScalarType::UnsignedChar, {"uchar_min", "uchar_max"});
  expect_vars(fuzzer::ScalarType::UnsignedShort, {"ushort_min", "ushort_max"});
  expect_vars(fuzzer::ScalarType::SignedShort, {"short_min", "short_max"});
  expect_vars(fuzzer::ScalarType::UnsignedInt, {"uint_min", "uint_max"});
  expect_vars(
      fuzzer::ScalarType::SignedInt,
      {"int_min", "int_max", "x", "ref" /* references aren't supported yet */});
  expect_vars(fuzzer::ScalarType::UnsignedLong, {"ulong_min", "ulong_max"});
  expect_vars(fuzzer::ScalarType::SignedLong, {"long_min", "long_max"});
  expect_vars(fuzzer::ScalarType::UnsignedLongLong,
              {"ullong_min", "ullong_max"});
  expect_vars(fuzzer::ScalarType::SignedLongLong, {"llong_min", "llong_max"});
  expect_vars(fuzzer::ScalarType::Float,
              {"fnan", "finf", "fsnan", "fmax", "fdenorm"});
  expect_vars(fuzzer::ScalarType::Double,
              {"dnan", "dinf", "dsnan", "dmax", "ddenorm"});
  expect_vars(fuzzer::ScalarType::LongDouble,
              {"ldnan", "ldinf", "ldsnan", "ldmax", "lddenorm"});
  fuzzer::Type int_ptr =
      fuzzer::PointerType(fuzzer::QualifiedType(fuzzer::ScalarType::SignedInt));
  expect_vars(int_ptr, {"p"});
  expect_vars(fuzzer::PointerType(fuzzer::QualifiedType(int_ptr)),
              {"q", "refp"});
  expect_vars(
      fuzzer::PointerType(fuzzer::QualifiedType(fuzzer::ScalarType::Void)),
      {"void_ptr"});
  fuzzer::Type char_ptr =
      fuzzer::PointerType(fuzzer::QualifiedType(fuzzer::ScalarType::Char));
  expect_vars(char_ptr, {"test_str", "null_char_ptr"});
  expect_vars(fuzzer::PointerType(fuzzer::QualifiedType(char_ptr)),
              {"addr_null_char_ptr"});
  expect_vars(fuzzer::NullptrType{}, {"null_ptr", "ref_null_ptr"});
  expect_vars(fuzzer::PointerType(fuzzer::QualifiedType(fuzzer::NullptrType{})),
              {"addr_null_ptr"});

  // Make sure there isn't a type we forgot to check.
  EXPECT_EQ(count_checked_types, symtab.vars().size());

  // Compare freedom indices.
  std::unordered_map<std::string, int> freedom_indices;
  freedom_indices["p"] = 1;
  freedom_indices["q"] = 2;
  freedom_indices["refp"] = 2;
  freedom_indices["void_ptr"] = 1;
  freedom_indices["addr_null_ptr"] = 1;
  freedom_indices["test_str"] = 1;
  freedom_indices["addr_null_char_ptr"] = 1;

  size_t variable_count = 0;
  for (const auto& [type, vars] : symtab.vars()) {
    for (const auto& var : vars) {
      variable_count++;
      EXPECT_EQ(var.freedom_index, freedom_indices[var.expr.name()]);
    }
  }

  // Make sure we checked freedom indices of all variables.
  EXPECT_EQ(variable_count, freedom_indices.size());

  // Teardown the test.
  process.Destroy();
  lldb::SBDebugger::Terminate();
}
