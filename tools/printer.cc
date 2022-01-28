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

#include <iostream>
#include <memory>
#include <string>

#include "lldb-eval/ast.h"
#include "lldb-eval/context.h"
#include "lldb-eval/parser.h"
#include "lldb-eval/runner.h"
#include "lldb/API/SBFrame.h"
#include "lldb/API/SBProcess.h"
#include "lldb/API/SBThread.h"
#include "tools/cpp/runfiles/runfiles.h"

using bazel::tools::cpp::runfiles::Runfiles;

namespace lldb_eval {

std::string print_common_props(const AstNode* node) {
  std::string ret;

  ret += "'";
  ret += node->result_type()->GetName();
  ret += "'";
  ret += " ";
  ret += node->is_rvalue() ? "rvalue" : "lvalue";

  if (node->is_bitfield()) {
    ret += " bitfield";
  }

  return ret;
}

class AstPrinter : Visitor {
 public:
  void Print(const AstNode* tree) { tree->Accept(this); }

  void Visit(const ErrorNode*) override {
    std::cout << "ErrorNode" << std::endl;
  }

  void Visit(const LiteralNode* node) override {
    std::cout << "LiteralNode " << print_common_props(node) << " value=";
    struct {
      void operator()(llvm::APInt val) {
        llvm::SmallVector<char, 32> buffer;
        val.toString(buffer, 10u, is_signed_);
        std::cout << std::string(buffer.data(), buffer.size());
      }
      void operator()(llvm::APFloat val) {
        llvm::SmallVector<char, 32> buffer;
        val.toString(buffer);
        std::cout << std::string(buffer.data(), buffer.size());
      }
      void operator()(bool val) { std::cout << val; }

      bool is_signed_;
    } visitor{node->result_type()->IsInteger() &&
              node->result_type()->IsSigned()};
    std::visit(visitor, node->value());
    std::cout << std::endl;
  }

  void Visit(const IdentifierNode* node) override {
    std::cout << "IdentifierNode " << print_common_props(node) << " "
              << "value=" << node->name() << std::endl;
  }

  void Visit(const SizeOfNode* node) override {
    std::cout << "SizeOfNode " << print_common_props(node)
              << " type=" << node->operand()->GetName().str() << std::endl;
  }

  void Visit(const BuiltinFunctionCallNode* node) override {
    std::cout << "BuiltinFunctionCallNode " << print_common_props(node) << " "
              << "name=" << node->name() << std::endl;

    auto& args = node->arguments();
    for (size_t i = 0; i < args.size() - 1; ++i) {
      PrintChild(args[i].get());
    }
    if (args.size() > 0) {
      PrintLastChild(args[args.size() - 1].get());
    }
  }

  void Visit(const CStyleCastNode* node) override {
    std::cout << "CStyleCastNode " << print_common_props(node) << " "
              << "type=" << node->type()->GetName().str() << std::endl;

    PrintLastChild(node->rhs());
  }

  void Visit(const CxxStaticCastNode* node) override {
    std::cout << "CxxStaticCastNode " << print_common_props(node) << " "
              << "type=" << node->type()->GetName().str() << std::endl;

    PrintLastChild(node->rhs());
  }

  void Visit(const CxxReinterpretCastNode* node) override {
    std::cout << "CxxReinterpretCastNode " << print_common_props(node) << " "
              << "type=" << node->type()->GetName().str() << std::endl;

    PrintLastChild(node->rhs());
  }

  void Visit(const MemberOfNode* node) override {
    std::cout << "MemberOfNode " << print_common_props(node) << " "
              << "member=TODO" << std::endl;

    PrintLastChild(node->lhs());
  }

  void Visit(const ArraySubscriptNode* node) override {
    std::cout << "ArraySubscriptNode " << print_common_props(node) << std::endl;

    PrintChild(node->base());
    PrintLastChild(node->index());
  }

  void Visit(const BinaryOpNode* node) override {
    std::cout << "BinaryOpNode " << print_common_props(node) << " "
              << to_string(node->kind()) << std::endl;

    PrintChild(node->lhs());
    PrintLastChild(node->rhs());
  }

  void Visit(const UnaryOpNode* node) override {
    std::cout << "UnaryOpNode " << print_common_props(node) << " "
              << to_string(node->kind()) << std::endl;
    PrintLastChild(node->rhs());
  }

  void Visit(const TernaryOpNode* node) override {
    std::cout << "TernaryOpNode " << print_common_props(node) << std::endl;

    PrintChild(node->cond());
    PrintChild(node->lhs());
    PrintLastChild(node->rhs());
  }

  void Visit(const SmartPtrToPtrDecay* node) override {
    std::cout << "SmartPtrToPtrDecay " << print_common_props(node) << std::endl;

    PrintLastChild(node->ptr());
  }

 private:
  void PrintChild(const AstNode* node) { Print("|-", "| ", node); }
  void PrintLastChild(const AstNode* node) { Print("`-", "  ", node); }

  void Print(const std::string& header, std::string prefix,
             const AstNode* node) {
    for (const auto& p : prefixes_) {
      std::cout << p;
    }
    std::cout << header;

    prefixes_.push_back(std::move(prefix));
    Print(node);
    prefixes_.pop_back();
  }

 private:
  std::vector<std::string> prefixes_;
};

}  // namespace lldb_eval

void PrintExpr(lldb::SBFrame frame, const std::string& expr) {
  auto ctx =
      lldb_eval::Context::Create(lldb_eval::SourceManager::Create(expr), frame);
  ctx->SetAllowSideEffects(true);

  lldb_eval::Error err;
  auto tree = lldb_eval::Parser(ctx).Run(err);

  if (err) {
    std::cout << err.message() << std::endl;
    return;
  }

  lldb_eval::AstPrinter printer;
  printer.Print(tree.get());
}

int main(int argc, char** argv) {
  std::unique_ptr<Runfiles> runfiles(Runfiles::Create(argv[0]));

  std::string break_line;
  std::string expr;

  if (argc == 2) {
    break_line = "// BREAK HERE";
    expr = argv[1];
  } else {
    break_line = "// BREAK(" + std::string(argv[1]) + ")";
    expr = argv[2];
  }

  lldb_eval::SetupLLDBServerEnv(*runfiles);
  lldb::SBDebugger::Initialize();
  lldb::SBDebugger debugger = lldb::SBDebugger::Create(false);

  auto binary_path = runfiles->Rlocation("lldb_eval/testdata/test_binary");
  auto source_path = runfiles->Rlocation("lldb_eval/testdata/test_binary.cc");

  lldb::SBProcess process = lldb_eval::LaunchTestProgram(
      debugger, source_path, binary_path, break_line);

  lldb::SBFrame frame = process.GetSelectedThread().GetSelectedFrame();

  PrintExpr(frame, expr);

  process.Destroy();
  lldb::SBDebugger::Terminate();

  return 0;
}
