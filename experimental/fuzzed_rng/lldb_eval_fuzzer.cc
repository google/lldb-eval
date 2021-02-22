#include <fuzzer/FuzzedDataProvider.h>
#include <unistd.h>

#include <iostream>
#include <optional>
#include <ostream>
#include <sstream>

#include "experimental/fuzzed_rng/fuzzed_rng.h"
#include "lldb-eval/api.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBDebugger.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/expr_gen.h"

using bazel::tools::cpp::runfiles::Runfiles;

static Runfiles* g_runfiles;
static lldb::SBDebugger g_debugger;
static lldb::SBProcess g_process;
static lldb::SBFrame g_frame;

static lldb::SBFrame& GetLldbFrame() {
  static bool initialized = false;
  if (!initialized) {
    g_runfiles = Runfiles::CreateForTest();
    lldb_eval::SetupLLDBServerEnv(*g_runfiles);
    lldb::SBDebugger::Initialize();

    auto binary_path =
        g_runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary");
    auto source_path =
        g_runfiles->Rlocation("lldb_eval/testdata/fuzzer_binary.cc");
    g_debugger = lldb::SBDebugger::Create(false);
    g_process = lldb_eval::LaunchTestProgram(g_debugger, source_path,
                                             binary_path, "// BREAK HERE");
    g_frame = g_process.GetSelectedThread().GetSelectedFrame();

    initialized = true;
  }

  return g_frame;
}

fuzzer::SymbolTable gen_symtab(lldb::SBFrame& frame) {
  fuzzer::SymbolTable symtab =
      fuzzer::SymbolTable::create_from_lldb_context(frame);

  // A helper lambda to add the same field to multiple class types.
  auto add_field =
      [&symtab](const std::string& field_name, const fuzzer::Type& field_type,
                const std::vector<fuzzer::TaggedType>& tagged_types) {
        for (const auto& tagged_type : tagged_types) {
          symtab.add_field(tagged_type, field_name, field_type);
        }
      };

  {
    fuzzer::TaggedType type("TestStruct");
    fuzzer::TaggedType ns_type("ns::nested_ns::TestStruct");

    add_field("int_field", fuzzer::ScalarType::SignedInt, {type, ns_type});
    add_field("flt_field", fuzzer::ScalarType::Float, {type, ns_type});
    add_field("ch_field", fuzzer::ScalarType::Char, {type, ns_type});
    symtab.add_field(type, "ull_field", fuzzer::ScalarType::UnsignedLongLong);

    symtab.add_var(type, fuzzer::VariableExpr("ts"));
    symtab.add_var(ns_type, fuzzer::VariableExpr("ns_ts"));

    // Also add global variables.
    symtab.add_var(type, fuzzer::VariableExpr("global_ts"));
    symtab.add_var(type, fuzzer::VariableExpr("ns::global_ts"));
    symtab.add_var(ns_type, fuzzer::VariableExpr("ns::nested_ns::global_ts"));
  }

  {
    fuzzer::TaggedType base1("MultiInheritBase1");
    fuzzer::TaggedType base2("MultiInheritBase2");
    fuzzer::TaggedType derived("MultiInheritDerived");

    add_field("f1", fuzzer::ScalarType::SignedInt, {base1, derived});
    add_field("f2", fuzzer::ScalarType::SignedInt, {base2, derived});
    add_field("f3", fuzzer::ScalarType::SignedInt, {derived});

    symtab.add_var(derived, fuzzer::VariableExpr("multi"));
  }

  {
    fuzzer::TaggedType base("DeadlyDiamondBase");
    fuzzer::TaggedType derived1("DeadlyDiamondDerived1");
    fuzzer::TaggedType derived2("DeadlyDiamondDerived2");
    fuzzer::TaggedType subclass("DeadlyDiamondSubclass");

    // Note: don't add `f1` to `DeadlyDiamondSubclass`.
    add_field("f1", fuzzer::ScalarType::SignedInt, {base, derived1, derived2});
    add_field("f2", fuzzer::ScalarType::SignedInt, {derived1, subclass});
    add_field("f3", fuzzer::ScalarType::SignedInt, {derived2, subclass});
    add_field("f4", fuzzer::ScalarType::SignedInt, {subclass});

    symtab.add_var(subclass, fuzzer::VariableExpr("diamond"));
  }

  {
    fuzzer::TaggedType base("VirtualDiamondBase");
    fuzzer::TaggedType derived1("VirtualDiamondDerived1");
    fuzzer::TaggedType derived2("VirtualDiamondDerived2");
    fuzzer::TaggedType subclass("VirtualDiamondSubclass");

    add_field("f1", fuzzer::ScalarType::SignedInt,
              {base, derived1, derived2, subclass});
    add_field("f2", fuzzer::ScalarType::SignedInt, {derived1, subclass});
    add_field("f3", fuzzer::ScalarType::SignedInt, {derived2, subclass});
    add_field("f4", fuzzer::ScalarType::SignedInt, {subclass});

    symtab.add_var(subclass, fuzzer::VariableExpr("virtual_diamond"));
  }

  {
    fuzzer::TaggedType base1("EmptyBase");
    fuzzer::TaggedType base2("NonEmptyBase");
    fuzzer::TaggedType derived("NonEmptyDerived");

    symtab.add_field(derived, "f1", fuzzer::ScalarType::SignedInt);
    symtab.add_field(derived, "base", base1);
    add_field("f2", fuzzer::ScalarType::SignedInt, {base2, derived});
  }

  {
    fuzzer::TaggedType with_nested("ClassWithNestedClass");
    fuzzer::TaggedType nested("ClassWithNestedClass::NestedClass");

    symtab.add_var(with_nested, fuzzer::VariableExpr("with_nested"));
    symtab.add_field(with_nested, "nested", nested);
    symtab.add_field(nested, "f1", fuzzer::ScalarType::SignedInt);
  }

  {
    fuzzer::TaggedType type("LocalStruct");
    fuzzer::PointerType ptr_int_type{
        fuzzer::QualifiedType(fuzzer::ScalarType::SignedInt)};

    symtab.add_var(type, fuzzer::VariableExpr("ls"));
    symtab.add_field(type, "int_field", fuzzer::ScalarType::SignedInt);
    symtab.add_field(type, "ref_field", fuzzer::ScalarType::SignedInt);
    symtab.add_field(type, "ptr_field", ptr_int_type);
    symtab.add_field(type, "ptr_ref_field", ptr_int_type);
    symtab.add_field(type, "dbl_field", fuzzer::ScalarType::Double);
  }

  {
    // Add static members.
    symtab.add_var(fuzzer::ScalarType::SignedInt,
                   fuzzer::VariableExpr("StaticMember::s1"));
    symtab.add_var(fuzzer::ScalarType::Char,
                   fuzzer::VariableExpr("StaticMember::s2"));
    symtab.add_var(fuzzer::ScalarType::SignedInt,
                   fuzzer::VariableExpr("ns::StaticMember::s1"));
    symtab.add_var(
        fuzzer::ScalarType::SignedInt,
        fuzzer::VariableExpr("ClassWithNestedClass::NestedClass::s1"));

    // Add global variables.
    symtab.add_var(fuzzer::ScalarType::SignedInt,
                   fuzzer::VariableExpr("global_int"));
    symtab.add_var(fuzzer::ScalarType::SignedInt,
                   fuzzer::VariableExpr("ns::global_int"));
    symtab.add_var(fuzzer::ScalarType::SignedInt,
                   fuzzer::VariableExpr("ns::nested_ns::global_int"));
  }

  {
    fuzzer::EnumType type("CStyleEnum", /*scoped=*/false);
    symtab.add_enum_literal(type, "VALUE1");
    symtab.add_enum_literal(type, "VALUE2");
    symtab.add_enum_literal(type, "VALUE3");
    symtab.add_var(type, fuzzer::VariableExpr("c_enum"));
  }

  {
    fuzzer::EnumType type("ns::CStyleEnum", /*scoped=*/false);
    symtab.add_enum_literal(type, "V1");
    symtab.add_enum_literal(type, "V2");
    symtab.add_enum_literal(type, "V3");
    symtab.add_var(type, fuzzer::VariableExpr("ns_enum"));
  }

  {
    fuzzer::EnumType type("EnumClass", /*scoped=*/true);
    symtab.add_enum_literal(type, "ZERO");
    symtab.add_enum_literal(type, "ONE");
    symtab.add_enum_literal(type, "TWO");
    symtab.add_enum_literal(type, "THREE");
    symtab.add_var(type, fuzzer::VariableExpr("enum_class"));
  }

  {
    fuzzer::EnumType type("ns::EnumClass", /*scoped=*/true);
    symtab.add_enum_literal(type, "ZERO");
    symtab.add_enum_literal(type, "ONE");
    symtab.add_enum_literal(type, "TWO");
    symtab.add_enum_literal(type, "THREE");
    symtab.add_var(type, fuzzer::VariableExpr("ns_enum_class"));
  }

  return symtab;
}

std::optional<lldb::SBValue> Eval(const std::string& expr) {
  lldb::SBError error;
  auto value =
      lldb_eval::EvaluateExpression(GetLldbFrame(), expr.c_str(), error);
  // note: possible bug
  if (error.Fail()) {
    return {};
  }
  return value;
}

extern "C" int LLVMFuzzerTestOneInput(const uint8_t* data, size_t size) {
  auto cfg = fuzzer::GenConfig();
  auto rng = std::make_unique<fuzzer::FuzzedGeneratorRng>(data, size);
  cfg.bin_op_mask[fuzzer::BinOp::Shl] = false;
  cfg.bin_op_mask[fuzzer::BinOp::Shr] = false;

  fuzzer::SymbolTable symtab = gen_symtab(GetLldbFrame());

  fuzzer::ExprGenerator gen(std::move(rng), std::move(cfg), std::move(symtab));

  auto expr = gen.generate();
  if (expr == std::nullopt) {
    std::cout << "[no epxr :(]" << std::endl;
  } else {
    std::cout << (*expr) << std::endl;
    std::ostringstream oss;
    oss << (*expr);
    auto res = Eval(oss.str());
    (void)res;
  }

  usleep(5000);

  return 0;
}
