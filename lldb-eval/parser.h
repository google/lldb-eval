/*
 * Copyright 2020 Google LLC
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

#ifndef LLDB_EVAL_PARSER_H_
#define LLDB_EVAL_PARSER_H_

#include <memory>
#include <string>
#include <tuple>
#include <vector>

#include "clang/Basic/FileManager.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceLocation.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Lex/ModuleLoader.h"
#include "clang/Lex/Preprocessor.h"
#include "lldb-eval/ast.h"
#include "lldb-eval/context.h"
#include "lldb/API/SBTarget.h"
#include "lldb/API/SBType.h"

namespace lldb_eval {

// TypeDeclaration holds information about the literal type definition. It
// doesn't perform semantic analysis of the type -- e.g. "long long long" and
// "char&&&" are valid type declarations.
// NOTE: CV qualifiers are ignored.
class TypeDeclaration {
 public:
  // Type declaration is considered valid if it contains at least one typename.
  bool IsValid() const { return typenames_.size() > 0; }

  std::string GetName() const;
  std::string GetBaseName() const;

 public:
  // List of base typenames, e.g. ["long", "long"] or ["uint64_t"].
  std::vector<std::string> typenames_;

  // Pointer and reference operators (* and &).
  std::vector<std::tuple<clang::tok::TokenKind, clang::SourceLocation>>
      ptr_operators_;
};

class BuiltinFunctionDef {
 public:
  BuiltinFunctionDef(std::string name, lldb::SBType return_type,
                     std::vector<lldb::SBType> arguments)
      : name_(std::move(name)),
        return_type_(std::move(return_type)),
        arguments_(std::move(arguments)) {}

  std::string name_;
  lldb::SBType return_type_;
  std::vector<lldb::SBType> arguments_;
};

// Pure recursive descent parser for C++ like expressions.
// EBNF grammar is described here:
// docs/expr-ebnf.txt
class Parser {
 public:
  explicit Parser(std::shared_ptr<Context> ctx);

  ExprResult Run(Error& error);

 private:
  ExprResult ParseExpression();
  ExprResult ParseAssignmentExpression();
  ExprResult ParseLogicalOrExpression();
  ExprResult ParseLogicalAndExpression();
  ExprResult ParseInclusiveOrExpression();
  ExprResult ParseExclusiveOrExpression();
  ExprResult ParseAndExpression();
  ExprResult ParseEqualityExpression();
  ExprResult ParseRelationalExpression();
  ExprResult ParseShiftExpression();
  ExprResult ParseAdditiveExpression();
  ExprResult ParseMultiplicativeExpression();
  ExprResult ParseCastExpression();
  ExprResult ParseUnaryExpression();
  ExprResult ParsePostfixExpression();
  ExprResult ParsePrimaryExpression();

  TypeDeclaration ParseTypeId();
  void ParseTypeSpecifierSeq(TypeDeclaration* type_decl);
  bool ParseTypeSpecifier(TypeDeclaration* type_decl);
  std::string ParseNestedNameSpecifier();
  std::string ParseTypeName();

  std::string ParseTemplateArgumentList();
  std::string ParseTemplateArgument();

  void ParsePtrOperator(TypeDeclaration* type_decl);

  lldb::SBType ResolveTypeFromTypeDecl(const TypeDeclaration& type_decl);
  lldb::SBType ResolveTypeDeclarators(lldb::SBType type,
                                      const TypeDeclaration& type_decl);

  bool IsSimpleTypeSpecifierKeyword(clang::Token token) const;
  bool IsCvQualifier(clang::Token token) const;
  bool IsPtrOperator(clang::Token token) const;

  std::string ParseIdExpression();
  std::string ParseUnqualifiedId();

  ExprResult ParseNumericLiteral();
  ExprResult ParseBooleanLiteral();
  ExprResult ParsePointerLiteral();

  ExprResult ParseNumericConstant(clang::Token token);
  ExprResult ParseFloatingLiteral(clang::NumericLiteralParser& literal,
                                  clang::Token token);
  ExprResult ParseIntegerLiteral(clang::NumericLiteralParser& literal,
                                 clang::Token token);

  ExprResult ParseBuiltinFunction(clang::SourceLocation loc,
                                  std::unique_ptr<BuiltinFunctionDef> func_def);

  bool ImplicitConversionIsAllowed(Type src, Type dst,
                                   bool is_src_literal_zero = false);
  ExprResult InsertImplicitConversion(ExprResult expr, Type type);

  void ConsumeToken();
  void BailOut(ErrorCode error_code, const std::string& error,
               clang::SourceLocation loc);

  void Expect(clang::tok::TokenKind kind);

  template <typename... Ts>
  void ExpectOneOf(clang::tok::TokenKind k, Ts... ks);

  std::string TokenDescription(const clang::Token& token);

  ExprResult BuildCStyleCast(Type type, ExprResult rhs,
                             clang::SourceLocation location);

  ExprResult BuildCxxCast(clang::tok::TokenKind kind, Type type, ExprResult rhs,
                          clang::SourceLocation location);

  ExprResult BuildCxxDynamicCast(Type type, ExprResult rhs,
                                 clang::SourceLocation location);

  ExprResult BuildCxxReinterpretCast(Type type, ExprResult rhs,
                                     clang::SourceLocation location);

  ExprResult BuildUnaryOp(UnaryOpKind kind, ExprResult rhs,
                          clang::SourceLocation location);
  ExprResult BuildIncrementDecrement(UnaryOpKind kind, ExprResult rhs,
                                     clang::SourceLocation location);

  ExprResult BuildBinaryOp(BinaryOpKind kind, ExprResult lhs, ExprResult rhs,
                           clang::SourceLocation location);

  lldb::SBType PrepareBinaryAddition(ExprResult& lhs, ExprResult& rhs,
                                     clang::SourceLocation location,
                                     bool is_comp_assign);
  lldb::SBType PrepareBinarySubtraction(ExprResult& lhs, ExprResult& rhs,
                                        clang::SourceLocation location,
                                        bool is_comp_assign);
  lldb::SBType PrepareBinaryMulDiv(ExprResult& lhs, ExprResult& rhs,
                                   bool is_comp_assign);
  lldb::SBType PrepareBinaryRemainder(ExprResult& lhs, ExprResult& rhs,
                                      bool is_comp_assign);
  lldb::SBType PrepareBinaryBitwise(ExprResult& lhs, ExprResult& rhs,
                                    bool is_comp_assign);
  lldb::SBType PrepareBinaryShift(ExprResult& lhs, ExprResult& rhs,
                                  bool is_comp_assign);
  lldb::SBType PrepareBinaryComparison(BinaryOpKind kind, ExprResult& lhs,
                                       ExprResult& rhs,
                                       clang::SourceLocation location);
  lldb::SBType PrepareBinaryLogical(const ExprResult& lhs,
                                    const ExprResult& rhs);

  ExprResult BuildBinarySubscript(ExprResult lhs, ExprResult rhs,
                                  clang::SourceLocation location);

  lldb::SBType PrepareCompositeAssignment(Type comp_assign_type,
                                          const ExprResult& lhs,
                                          clang::SourceLocation location);

  ExprResult BuildTernaryOp(ExprResult cond, ExprResult lhs, ExprResult rhs,
                            clang::SourceLocation location);

  ExprResult BuildMemberOf(ExprResult lhs, std::string member_id, bool is_arrow,
                           clang::SourceLocation location);

 private:
  friend class TentativeParsingAction;

  // Parser doesn't own the evaluation context. The produced AST may depend on
  // it (for example, for source locations), so it's expected that expression
  // context will outlive the parser.
  std::shared_ptr<Context> ctx_;

  // Convenience references, used by the interpreter to lookup variables and
  // types, create objects, perform casts, etc.
  lldb::SBTarget target_;

  // The token lexer is stopped at (aka "current token").
  clang::Token token_;
  // Holds an error if it occures during parsing.
  Error error_;

  std::unique_ptr<clang::TargetInfo> ti_;
  std::unique_ptr<clang::LangOptions> lang_opts_;
  std::unique_ptr<clang::HeaderSearch> hs_;
  std::unique_ptr<clang::TrivialModuleLoader> tml_;
  std::unique_ptr<clang::Preprocessor> pp_;
};

// Enables tentative parsing mode, allowing to rollback the parser state. Call
// Commit() or Rollback() to control the parser state. If neither was called,
// the destructor will assert.
class TentativeParsingAction {
 public:
  TentativeParsingAction(Parser* parser) : parser_(parser) {
    backtrack_token_ = parser_->token_;
    parser_->pp_->EnableBacktrackAtThisPos();
    enabled_ = true;
  }

  ~TentativeParsingAction() {
    assert(!enabled_ &&
           "Tentative parsing wasn't finalized. Did you forget to call "
           "Commit() or Rollback()?");
  }

  void Commit() {
    parser_->pp_->CommitBacktrackedTokens();
    enabled_ = false;
  }
  void Rollback() {
    parser_->pp_->Backtrack();
    parser_->error_.Clear();
    parser_->token_ = backtrack_token_;
    enabled_ = false;
  }

 private:
  Parser* parser_;
  clang::Token backtrack_token_;
  bool enabled_;
};

}  // namespace lldb_eval

#endif  // LLDB_EVAL_PARSER_H_
