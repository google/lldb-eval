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

using namespace fuzzer;
using namespace testing;
using bazel::tools::cpp::runfiles::Runfiles;

// Removes leading "::" from the identifier name.
static std::string remove_leading_colons(const std::string& name) {
  if (name.rfind("::", 0) == 0) {
    // `name` starts with "::"
    return name.substr(2);
  }
  return name;
}

class PopulateSymbolTableTest : public Test {
 protected:
  static void SetUpTestSuite() {
    runfiles_ = Runfiles::CreateForTest();
    lldb_eval::SetupLLDBServerEnv(*runfiles_);
    lldb::SBDebugger::Initialize();
    auto binary_path = runfiles_->Rlocation("lldb_eval/testdata/fuzzer_binary");
    auto source_path =
        runfiles_->Rlocation("lldb_eval/testdata/fuzzer_binary.cc");
    auto debugger = lldb::SBDebugger::Create(false);
    process_ = lldb_eval::LaunchTestProgram(debugger, source_path, binary_path,
                                            "// BREAK HERE");
    auto frame = process_.GetSelectedThread().GetSelectedFrame();
    symtab_ = SymbolTable::create_from_lldb_context(frame);
  }

  static void TearDownTestSuite() {
    process_.Destroy();
    lldb::SBDebugger::Terminate();
    delete runfiles_;
    runfiles_ = nullptr;
  }

 protected:
  static Runfiles* runfiles_;
  static lldb::SBProcess process_;
  static SymbolTable symtab_;
};

Runfiles* PopulateSymbolTableTest::runfiles_ = nullptr;
lldb::SBProcess PopulateSymbolTableTest::process_;
SymbolTable PopulateSymbolTableTest::symtab_;

TEST_F(PopulateSymbolTableTest, Variables) {
  size_t count_checked_types = 0;

  auto expect_vars = [this, &count_checked_types](
                         Type type, const std::set<std::string>& names) {
    auto var_it = symtab_.vars().find(type);
    ASSERT_NE(var_it, symtab_.vars().end());
    std::set<std::string> names_from_symtab;
    for (const auto& var : var_it->second) {
      names_from_symtab.insert(remove_leading_colons(var.expr.name()));
    }
    EXPECT_EQ(names, names_from_symtab);
    count_checked_types++;
  };

  // Check contents of the symbol table.
  expect_vars(ScalarType::Char, {"char_min", "char_max", "StaticMember::s2"});
  expect_vars(ScalarType::SignedChar, {"schar_min", "schar_max"});
  expect_vars(ScalarType::UnsignedChar, {"uchar_min", "uchar_max"});
  expect_vars(ScalarType::UnsignedShort, {"ushort_min", "ushort_max"});
  expect_vars(ScalarType::SignedShort, {"short_min", "short_max"});
  expect_vars(ScalarType::UnsignedInt, {"uint_min", "uint_max"});
  expect_vars(
      ScalarType::SignedInt,
      {"int_min", "int_max", "x", "ref" /* references aren't supported yet */,
       "global_int", "ns::global_int", "ns::nested_ns::global_int",
       "global_ref", "ns::global_ref", "StaticMember::s1",
       "ns::StaticMember::s1", "ClassWithNestedClass::NestedClass::s1"});
  expect_vars(ScalarType::UnsignedLong, {"ulong_min", "ulong_max"});
  expect_vars(ScalarType::SignedLong, {"long_min", "long_max"});
  expect_vars(ScalarType::UnsignedLongLong, {"ullong_min", "ullong_max"});
  expect_vars(ScalarType::SignedLongLong, {"llong_min", "llong_max"});
  expect_vars(ScalarType::Float, {"fnan", "finf", "fsnan", "fmax", "fdenorm"});
  expect_vars(ScalarType::Double, {"dnan", "dinf", "dsnan", "dmax", "ddenorm"});
  expect_vars(ScalarType::LongDouble,
              {"ldnan", "ldinf", "ldsnan", "ldmax", "lddenorm"});
  Type int_ptr = PointerType(QualifiedType(ScalarType::SignedInt));
  expect_vars(int_ptr, {"p", "global_ptr", "ns::global_ptr"});
  expect_vars(PointerType(QualifiedType(int_ptr)), {"q"});
  expect_vars(PointerType(QualifiedType(int_ptr, CvQualifier::Const)),
              {"refp"});
  expect_vars(PointerType(QualifiedType(ScalarType::Void)), {"void_ptr"});
  Type char_ptr = PointerType(QualifiedType(ScalarType::Char));
  expect_vars(char_ptr, {"null_char_ptr"});
  expect_vars(PointerType(QualifiedType(ScalarType::Char, CvQualifier::Const)),
              {"test_str"});
  expect_vars(PointerType(QualifiedType(char_ptr)), {"addr_null_char_ptr"});
  expect_vars(NullptrType{}, {"null_ptr", "ref_null_ptr"});
  expect_vars(PointerType(QualifiedType(NullptrType{})), {"addr_null_ptr"});
  expect_vars(TaggedType("TestStruct"), {"ts", "global_ts", "ns::global_ts"});
  expect_vars(TaggedType("LocalStruct"), {"ls"});
  expect_vars(TaggedType("ns::nested_ns::TestStruct"),
              {"ns_ts", "ns::nested_ns::global_ts"});

  // Make sure there isn't a type we forgot to check.
  EXPECT_EQ(count_checked_types, symtab_.vars().size());
}

TEST_F(PopulateSymbolTableTest, FreedomIndices) {
  std::unordered_map<std::string, int> freedom_indices;
  freedom_indices["p"] = 1;
  freedom_indices["q"] = 2;
  freedom_indices["refp"] = 2;
  freedom_indices["void_ptr"] = 1;
  freedom_indices["addr_null_ptr"] = 1;
  freedom_indices["test_str"] = 1;
  freedom_indices["addr_null_char_ptr"] = 1;
  freedom_indices["global_ptr"] = 1;
  freedom_indices["ns::global_ptr"] = 1;

  size_t variable_count = 0;
  for (const auto& [type, vars] : symtab_.vars()) {
    for (const auto& var : vars) {
      variable_count++;
      EXPECT_EQ(var.freedom_index,
                freedom_indices[remove_leading_colons(var.expr.name())]);
    }
  }

  // Make sure we checked freedom indices of all variables.
  EXPECT_EQ(variable_count, freedom_indices.size());
}

namespace fuzzer {
bool operator==(const Field& lhs, const Field& rhs) {
  return lhs.containing_type() == rhs.containing_type() &&
         lhs.name() == rhs.name();
}
}  // namespace fuzzer

TEST_F(PopulateSymbolTableTest, TaggedTypesAndFields) {
  auto expect_field = [this](const TaggedType& containing_type,
                             std::string field_name, const Type& field_type) {
    const fuzzer::Field field(containing_type, std::move(field_name));
    const auto fields_it = symtab_.fields_by_type().find(field_type);
    ASSERT_NE(fields_it, symtab_.fields_by_type().end());
    EXPECT_THAT(fields_it->second, Contains(field));
  };

  {
    const TaggedType tagged_type("TestStruct");
    EXPECT_THAT(symtab_.tagged_types(), Contains(tagged_type));
    expect_field(tagged_type, "int_field", ScalarType::SignedInt);
    expect_field(tagged_type, "ch_field", ScalarType::Char);
    expect_field(tagged_type, "flt_field", ScalarType::Float);
    expect_field(tagged_type, "ull_field", ScalarType::UnsignedLongLong);
  }

  {
    const TaggedType tagged_type("ns::nested_ns::TestStruct");
    EXPECT_THAT(symtab_.tagged_types(), Contains(tagged_type));
    expect_field(tagged_type, "int_field", ScalarType::SignedInt);
    expect_field(tagged_type, "ch_field", ScalarType::Char);
    expect_field(tagged_type, "flt_field", ScalarType::Float);
  }

  {
    const TaggedType tagged_type("LocalStruct");
    EXPECT_THAT(symtab_.tagged_types(), Contains(tagged_type));
    expect_field(tagged_type, "int_field", ScalarType::SignedInt);
    expect_field(tagged_type, "ref_field", ScalarType::SignedInt);
    expect_field(tagged_type, "dbl_field", ScalarType::Double);
    const Type ptr_type = PointerType(QualifiedType(ScalarType::SignedInt));
    expect_field(tagged_type, "ptr_field", ptr_type);
    expect_field(tagged_type, "ptr_ref_field", ptr_type);
  }
}
