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

#include "parser.h"

#include <stdlib.h>

#include <cstdint>
#include <memory>
#include <string>

#include "ast.h"
#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Lex/ModuleLoader.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/Token.h"
#include "defines.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/Support/FormatAdapters.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Host.h"
#include "scalar.h"

#define TYPE_WIDTH(type) static_cast<unsigned>(sizeof(type)) * 8

namespace {

std::string FormatDiagnostics(const clang::SourceManager& sm,
                              const std::string& message,
                              clang::SourceLocation loc) {
  // Get the source buffer and the location of the current token.
  const llvm::MemoryBuffer* buf = sm.getBuffer(sm.getFileID(loc));
  llvm::StringRef text = buf->getBuffer();
  size_t loc_offset = sm.getCharacterData(loc) - buf->getBufferStart();

  // Look for the start of the line.
  size_t line_start = text.rfind('\n', loc_offset);
  line_start = line_start == llvm::StringRef::npos ? 0 : line_start + 1;

  // Look for the end of the line.
  size_t line_end = text.find('\n', loc_offset);
  line_end = line_end == llvm::StringRef::npos ? text.size() : line_end;

  // Get a view of the current line in the source code and the position of the
  // diagnostics pointer.
  llvm::StringRef line = text.slice(line_start, line_end);
  int32_t arrow = sm.getPresumedColumnNumber(loc);

  // Calculate the padding in case we point outside of the expression (this can
  // happen if the parser expected something, but got EOF).
  size_t expr_rpad = std::max(0, arrow - static_cast<int32_t>(line.size()));
  size_t arrow_rpad = std::max(0, static_cast<int32_t>(line.size()) - arrow);

  return llvm::formatv("{0}: {1}\n{2}\n{3}", loc.printToString(sm), message,
                       llvm::fmt_pad(line, 0, expr_rpad),
                       llvm::fmt_pad("^", arrow - 1, arrow_rpad));
}

struct IntegerType {
  unsigned width;
  bool is_unsigned;
};

IntegerType PickIntegerType(const clang::NumericLiteralParser& literal,
                            const llvm::APInt& value) {
  unsigned int_size = TYPE_WIDTH(int);
  unsigned long_size = TYPE_WIDTH(long);
  unsigned long_long_size = TYPE_WIDTH(long long);

  // Binary, Octal, Hexadecimal and literals with a U suffix are allowed to be
  // an unsigned integer.
  bool unsigned_is_allowed = literal.isUnsigned || literal.getRadix() != 10;

  // Try int/unsigned int.
  if (!literal.isLong && !literal.isLongLong) {
    if (value.isIntN(int_size)) {
      if (!literal.isUnsigned && value.isIntN(int_size - 1)) {
        return {int_size, false};
      }
      if (unsigned_is_allowed) {
        return {int_size, true};
      }
    }
  }
  // Try long/unsigned long.
  if (!literal.isLongLong) {
    if (value.isIntN(long_size)) {
      if (!literal.isUnsigned && value.isIntN(long_size - 1)) {
        return {long_size, false};
      }
      if (unsigned_is_allowed) {
        return {long_size, true};
      }
    }
  }
  // Try long long/unsigned long long.
  if (value.isIntN(long_long_size)) {
    if (value.isIntN(long_long_size)) {
      if (!literal.isUnsigned && value.isIntN(long_long_size - 1)) {
        return {long_long_size, false};
      }
      if (unsigned_is_allowed) {
        return {long_long_size, true};
      }
    }
  }

  // If we still couldn't decide a type, we probably have something that does
  // not fit in a signed long long, but has no U suffix.
  // TODO(werat): Make this an error?
  return {long_long_size, false};
}

}  // namespace

namespace lldb_eval {

Parser::Parser(ExpressionContext& expr_ctx) : expr_ctx_(&expr_ctx) {
  clang::SourceManager& sm = expr_ctx_->GetSourceManager();
  clang::DiagnosticsEngine& de = sm.getDiagnostics();

  auto tOpts = std::make_shared<clang::TargetOptions>();
  tOpts->Triple = llvm::sys::getDefaultTargetTriple();

  ti_.reset(clang::TargetInfo::CreateTargetInfo(de, tOpts));

  lang_opts_ = std::make_unique<clang::LangOptions>();
  lang_opts_->Bool = true;
  lang_opts_->WChar = true;
  lang_opts_->CPlusPlus = true;
  lang_opts_->CPlusPlus11 = true;
  lang_opts_->CPlusPlus14 = true;
  lang_opts_->CPlusPlus17 = true;

  tml_ = std::make_unique<clang::TrivialModuleLoader>();

  auto hOpts = std::make_shared<clang::HeaderSearchOptions>();
  hs_ = std::make_unique<clang::HeaderSearch>(hOpts, sm, de, *lang_opts_,
                                              ti_.get());

  auto pOpts = std::make_shared<clang::PreprocessorOptions>();
  pp_ = std::make_unique<clang::Preprocessor>(pOpts, de, *lang_opts_, sm, *hs_,
                                              *tml_);
  pp_->Initialize(*ti_);
  pp_->EnterMainSourceFile();

  // Initialize the token.
  token_.setKind(clang::tok::unknown);
}

ExprResult Parser::Run() {
  ConsumeToken();
  auto expr = ParseExpression();
  Expect(clang::tok::eof);

  // Explicitly return ErrorNode if there was an error during the parsing. Some
  // routines raise an error, but don't change the return value (e.g. Expect).
  if (HasError()) {
    return std::make_unique<ErrorNode>();
  }
  return expr;
}

void Parser::ConsumeToken() {
  if (token_.is(clang::tok::eof)) {
    // Don't do anything if we're already at eof. This can happen if an error
    // occurred during parsing and we're trying to bail out.
    return;
  }
  pp_->Lex(token_);
}

void Parser::BailOut(const std::string& error, clang::SourceLocation loc) {
  if (!error_.empty()) {
    // If error is already set, then the parser is in the "bail-out" mode. Don't
    // do anything and keep the original error.
    return;
  }

  error_ = FormatDiagnostics(expr_ctx_->GetSourceManager(), error, loc);
  token_.setKind(clang::tok::eof);
}

// Parse an expression.
//
//  expression:
//    assignment_expression
//
ExprResult Parser::ParseExpression() { return ParseAssignmentExpression(); }

// Parse an assigment_expression.
//
//  assignment_expression:
//    conditional_expression
//
ExprResult Parser::ParseAssignmentExpression() {
  return ParseConditionalExpression();
}

// Parse a conditional_expression
//
//  conditional_expression:
//    logical_or_expression
//    logical_or_expression "?" expression ":" assignment_expression
//
ExprResult Parser::ParseConditionalExpression() {
  auto lhs = ParseLogicalOrExpression();

  if (token_.is(clang::tok::question)) {
    ConsumeToken();
    auto true_val = ParseExpression();
    Expect(clang::tok::colon);
    ConsumeToken();
    auto false_val = ParseAssignmentExpression();
    lhs = std::make_unique<TernaryOpNode>(std::move(lhs), std::move(true_val),
                                          std::move(false_val));
  }

  return lhs;
}

// Parse a logical_or_expression.
//
//  logical_or_expression:
//    logical_and_expression {"||" logical_and_expression}
//
ExprResult Parser::ParseLogicalOrExpression() {
  auto lhs = ParseLogicalAndExpression();

  while (token_.is(clang::tok::pipepipe)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseLogicalAndExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse a logical_and_expression.
//
//  logical_and_expression:
//    inclusive_or_expression {"&&" inclusive_or_expression}
//
ExprResult Parser::ParseLogicalAndExpression() {
  auto lhs = ParseInclusiveOrExpression();

  while (token_.is(clang::tok::ampamp)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseInclusiveOrExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse an inclusive_or_expression.
//
//  inclusive_or_expression:
//    exclusive_or_expression {"|" exclusive_or_expression}
//
ExprResult Parser::ParseInclusiveOrExpression() {
  auto lhs = ParseExclusiveOrExpression();

  while (token_.is(clang::tok::pipe)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseExclusiveOrExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse an exclusive_or_expression.
//
//  exclusive_or_expression:
//    and_expression {"^" and_expression}
//
ExprResult Parser::ParseExclusiveOrExpression() {
  auto lhs = ParseAndExpression();

  while (token_.is(clang::tok::caret)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseAndExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse an and_expression.
//
//  and_expression:
//    equality_expression {"&" equality_expression}
//
ExprResult Parser::ParseAndExpression() {
  auto lhs = ParseEqualityExpression();

  while (token_.is(clang::tok::amp)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseEqualityExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse an equality_expression.
//
//  equality_expression:
//    relational_expression {"==" relational_expression}
//    relational_expression {"!=" relational_expression}
//
ExprResult Parser::ParseEqualityExpression() {
  auto lhs = ParseRelationalExpression();

  while (token_.isOneOf(clang::tok::equalequal, clang::tok::exclaimequal)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseRelationalExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse a relational_expression.
//
//  relational_expression:
//    shift_expression {"<" shift_expression}
//    shift_expression {">" shift_expression}
//    shift_expression {"<=" shift_expression}
//    shift_expression {">=" shift_expression}
//
ExprResult Parser::ParseRelationalExpression() {
  auto lhs = ParseShiftExpression();

  while (token_.isOneOf(clang::tok::less, clang::tok::greater,
                        clang::tok::lessequal, clang::tok::greaterequal)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseShiftExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse a shift_expression.
//
//  shift_expression:
//    additive_expression {"<<" additive_expression}
//    additive_expression {">>" additive_expression}
//
ExprResult Parser::ParseShiftExpression() {
  auto lhs = ParseAdditiveExpression();

  while (token_.isOneOf(clang::tok::lessless, clang::tok::greatergreater)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseAdditiveExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse an additive_expression.
//
//  additive_expression:
//    multiplicative_expression {"+" multiplicative_expression}
//    multiplicative_expression {"-" multiplicative_expression}
//
ExprResult Parser::ParseAdditiveExpression() {
  auto lhs = ParseMultiplicativeExpression();

  while (token_.isOneOf(clang::tok::plus, clang::tok::minus)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseMultiplicativeExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse a multiplicative_expression.
//
//  multiplicative_expression:
//    cast_expression {"*" cast_expression}
//    cast_expression {"/" cast_expression}
//    cast_expression {"%" cast_expression}
//
ExprResult Parser::ParseMultiplicativeExpression() {
  auto lhs = ParseCastExpression();

  while (token_.isOneOf(clang::tok::star, clang::tok::slash,
                        clang::tok::percent)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseCastExpression();
    lhs = std::make_unique<BinaryOpNode>(kind, std::move(lhs), std::move(rhs));
  }

  return lhs;
}

// Parse a cast_expression.
//
//  cast_expression:
//    unary_expression
//    "(" type_id ")" cast_expression
//
ExprResult Parser::ParseCastExpression() {
  // This can be a C-style cast, try parsing the contents as a type declaration.
  if (token_.is(clang::tok::l_paren)) {
    // Enable lexer backtracking, so that we can rollback in case it's not
    // actually a type declaration.
    TentativeParsingAction tentative_parsing(this);

    // Consume the token only after enabling the backtracking.
    ConsumeToken();

    // Try parsing the type declaration. If the returned value is not valid,
    // then we should rollback and try parsing the expression.
    TypeDeclaration type_decl = ParseTypeId();

    if (type_decl.IsValid() && ResolveTypeFromTypeDecl(type_decl)) {
      // Successfully parsed the type declaration. Commit the backtracked
      // tokens and parse the cast_expression.
      tentative_parsing.Commit();

      Expect(clang::tok::r_paren);
      ConsumeToken();
      auto rhs = ParseCastExpression();

      return std::make_unique<CStyleCastNode>(type_decl, std::move(rhs));

    } else {
      // Failed to parse the contents of the parentheses as a type declaration.
      // Rollback the lexer and try parsing it as unary_expression.
      tentative_parsing.Rollback();
    }
  }

  return ParseUnaryExpression();
}

// Parse an unary_expression.
//
//  unary_expression:
//    postfix_expression
//    "++" cast_expression
//    "--" cast_expression
//    unary_operator cast_expression
//
//  unary_operator:
//    "&"
//    "*"
//    "+"
//    "-"
//    "~"
//    "!"
//
ExprResult Parser::ParseUnaryExpression() {
  if (token_.isOneOf(clang::tok::plusplus, clang::tok::minusminus,
                     clang::tok::star, clang::tok::amp, clang::tok::plus,
                     clang::tok::minus, clang::tok::exclaim,
                     clang::tok::tilde)) {
    clang::tok::TokenKind kind = token_.getKind();
    ConsumeToken();
    auto rhs = ParseCastExpression();
    return std::make_unique<UnaryOpNode>(kind, std::move(rhs));
  }

  return ParsePostfixExpression();
}

// Parse a postfix_expression.
//
//  postfix_expression:
//    primary_expression {"[" expression "]"}
//    primary_expression {"." id_expression}
//    primary_expression {"->" id_expression}
//    primary_expression {"++"}
//    primary_expression {"--"}
//
ExprResult Parser::ParsePostfixExpression() {
  auto lhs = ParsePrimaryExpression();

  while (token_.isOneOf(clang::tok::l_square, clang::tok::period,
                        clang::tok::arrow, clang::tok::plusplus,
                        clang::tok::minusminus)) {
    switch (token_.getKind()) {
      case clang::tok::period:
      case clang::tok::arrow: {
        auto type = token_.getKind() == clang::tok::period
                        ? MemberOfNode::Type::OF_OBJECT
                        : MemberOfNode::Type::OF_POINTER;
        ConsumeToken();
        auto member_id = ParseIdExpression();
        lhs = std::make_unique<MemberOfNode>(type, std::move(lhs),
                                             std::move(member_id));
        break;
      }
      case clang::tok::plusplus:
      case clang::tok::minusminus: {
        BailOut(
            "Don't support postfix inc/dec yet: " + TokenDescription(token_),
            token_.getLocation());
        return std::make_unique<ErrorNode>();
      }
      case clang::tok::l_square: {
        ConsumeToken();
        auto rhs = ParseExpression();
        Expect(clang::tok::r_square);
        ConsumeToken();
        lhs = std::make_unique<BinaryOpNode>(clang::tok::l_square,
                                             std::move(lhs), std::move(rhs));
        break;
      }
      default: {
        BailOut("Can't parse this: " + TokenDescription(token_),
                token_.getLocation());
        return std::make_unique<ErrorNode>();
      }
    }
  }

  return lhs;
}

// Parse a primary_expression.
//
//  primary_expression:
//    numeric_literal
//    boolean_literal
//    id_expression
//    "this"
//    "(" expression ")"
//
ExprResult Parser::ParsePrimaryExpression() {
  if (token_.is(clang::tok::numeric_constant)) {
    return ParseNumericLiteral();
  } else if (token_.isOneOf(clang::tok::kw_true, clang::tok::kw_false)) {
    return ParseBooleanLiteral();
  } else if (token_.isOneOf(clang::tok::coloncolon, clang::tok::identifier)) {
    return ParseIdExpression();
  } else if (token_.is(clang::tok::kw_this)) {
    ConsumeToken();
    return std::make_unique<IdentifierNode>("this");
  } else if (token_.is(clang::tok::l_paren)) {
    ConsumeToken();
    auto expr = ParseExpression();
    Expect(clang::tok::r_paren);
    ConsumeToken();
    return expr;
  }

  BailOut("Unexpected token: " + TokenDescription(token_),
          token_.getLocation());
  return std::make_unique<ErrorNode>();
}

// Parse a type_id.
//
//  type_id:
//    type_specifier_seq {abstract_declarator}
//
TypeDeclaration Parser::ParseTypeId() {
  TypeDeclaration type_decl;
  type_decl.is_builtin_ = true;

  // type_specifier_seq is required here, start with trying to parse it.
  ParseTypeSpecifierSeq(&type_decl);

  //
  //  abstract_declarator:
  //    ptr_operator {abstract_declarator}
  //
  while (IsPtrOperator(token_)) {
    ParsePtrOperator(&type_decl);
  }

  return type_decl;
}

// Parse a type_specifier_seq.
//
//  type_specifier_seq:
//    type_specifier {type_specifier_seq}
//
void Parser::ParseTypeSpecifierSeq(TypeDeclaration* type_decl) {
  while (true) {
    // TODO(b/161677840): Check if produced type specifiers can be combined
    // together. For example, "long long" is legal, but "char char" is not.
    bool type_specifier = ParseTypeSpecifier(type_decl);
    if (!type_specifier) {
      break;
    }
  }
}

// Parse a type_specifier.
//
//  type_specifier:
//    simple_type_specifier
//    cv_qualifier
//
//  simple_type_specifier:
//    {"::"} {nested_name_specifier} type_name
//    "char"
//    "char16_t"
//    "char32_t"
//    "wchar_t"
//    "bool"
//    "short"
//    "int"
//    "long"
//    "signed"
//    "unsigned"
//    "float"
//    "double"
//    "void"
//
// Returns TRUE if a type_specifier was successfully parsed at this location.
//
bool Parser::ParseTypeSpecifier(TypeDeclaration* type_decl) {
  if (IsCvQualifier(token_)) {
    // Just ignore CV quialifiers, we don't use them in type casting.
    ConsumeToken();
    return true;
  }

  if (IsSimpleTypeSpecifierKeyword(token_)) {
    type_decl->typenames_.push_back(pp_->getSpelling(token_));
    ConsumeToken();
    return true;
  }

  // The type_specifier must be a user-defined type. Try parsing a
  // simple_type_specifier.
  {
    // Try parsing optional global scope operator.
    bool global_scope = false;
    if (token_.is(clang::tok::coloncolon)) {
      global_scope = true;
      ConsumeToken();
    }

    // Try parsing optional nested_name_specifier.
    auto nested_name_specifier = ParseNestedNameSpecifier();

    // Try parsing required type_name.
    auto type_name = ParseTypeName();

    // If there is a type_name, then this is indeed a simple_type_specifier.
    // Global and qualified (namespace/class) scopes can be empty, since they're
    // optional. In this case type_name is type we're looking for.
    if (!type_name.empty()) {
      // Construct the fully qualified typename.
      auto type_specifier = llvm::formatv("{0}{1}{2}", global_scope ? "::" : "",
                                          nested_name_specifier, type_name);

      // This is a user-defined type now. Typedefs from standard library (e.g.
      // "uint64_t") are also considered user-defined.
      type_decl->is_builtin_ = false;
      type_decl->typenames_.push_back(type_specifier);
      return true;
    }
  }

  // No type_specifier was found here.
  return false;
}

// Parse nested_name_specifier.
//
//  nested_name_specifier:
//    type_name "::"
//    namespace_name '::'
//    nested_name_specifier identifier "::"
//    nested_name_specifier simple_template_id "::"
//
std::string Parser::ParseNestedNameSpecifier() {
  // The first token in nested_name_specifier is always an identifier.
  if (token_.isNot(clang::tok::identifier)) {
    return "";
  }

  // If the next token is scope ("::"), then this is indeed a
  // nested_name_specifier
  if (pp_->LookAhead(0).is(clang::tok::coloncolon)) {
    // This nested_name_specifier is a single identifier.
    std::string identifier = pp_->getSpelling(token_);
    ConsumeToken();
    Expect(clang::tok::coloncolon);
    ConsumeToken();
    // Continue parsing the nested_name_specifier.
    return identifier + "::" + ParseNestedNameSpecifier();
  }

  // If the next token starts a template argument list, then we have a
  // simple_template_id here.
  if (pp_->LookAhead(0).is(clang::tok::less)) {
    // We don't know whether this will be a nested_name_identifier or just a
    // type_name. Prepare to rollback if this is not a nested_name_identifier.
    TentativeParsingAction tentative_parsing(this);

    // TODO(werat): Parse just the simple_template_id?
    auto type_name = ParseTypeName();

    // If we did parse the type_name successfully and it's followed by the scope
    // operator ("::"), then this is indeed a nested_name_specifier. Commit the
    // tentative parsing and continue parsing nested_name_specifier.
    if (!type_name.empty() && token_.is(clang::tok::coloncolon)) {
      tentative_parsing.Commit();
      ConsumeToken();
      // Continue parsing the nested_name_specifier.
      return type_name + "::" + ParseNestedNameSpecifier();
    }

    // Not a nested_name_specifier, but could be just a type_name or something
    // else entirely. Rollback the parser and try a different path.
    tentative_parsing.Rollback();
  }

  return "";
}

// Parse a type_name.
//
//  type_name:
//    class_name
//    enum_name
//    typedef_name
//    simple_template_id
//
//  class_name
//    identifier
//
//  enum_name
//    identifier
//
//  typedef_name
//    identifier
//
std::string Parser::ParseTypeName() {
  // Typename always starts with an identifier.
  if (token_.isNot(clang::tok::identifier)) {
    return "";
  }

  // If the next token starts a template argument list, parse this type_name as
  // a simple_template_id.
  if (pp_->LookAhead(0).is(clang::tok::less)) {
    // Parse the template_name. In this case it's just an identifier.
    std::string template_name = pp_->getSpelling(token_);
    ConsumeToken();
    // Consume the "<" token.
    ConsumeToken();

    // Short-circuit for missing template_argument_list.
    if (token_.is(clang::tok::greater)) {
      ConsumeToken();
      return llvm::formatv("{0}<>", template_name);
    }

    // Try parsing template_argument_list.
    auto template_argument_list = ParseTemplateArgumentList();

    // TODO(werat): Handle ">>" situations.
    if (token_.is(clang::tok::greater)) {
      ConsumeToken();
      return llvm::formatv("{0}<{1}>", template_name, template_argument_list);
    }

    // Failed to parse a simple_template_id.
    return "";
  }

  // Otherwise look for a class_name, enum_name or a typedef_name.
  std::string identifier = pp_->getSpelling(token_);
  ConsumeToken();

  return identifier;
}

// Parse a template_argument_list.
//
//  template_argument_list:
//    template_argument
//    template_argument_list "," template_argument
//
std::string Parser::ParseTemplateArgumentList() {
  // Parse template arguments one by one.
  std::vector<std::string> arguments;

  do {
    // Eat the comma if this is not the first iteration.
    if (arguments.size() > 0) {
      ConsumeToken();
    }

    // Try parsing a template_argument. If this fails, then this is actually not
    // a template_argument_list.
    auto argument = ParseTemplateArgument();
    if (argument.empty()) {
      return "";
    }

    arguments.push_back(argument);

  } while (token_.is(clang::tok::comma));

  // Internally in LLDB/Clang nested template type names have extra spaces to
  // avoid having ">>". Add the extra space before the closing ">" if the
  // template argument is also a template.
  if (arguments.back().back() == '>') {
    arguments.back().push_back(' ');
  }

  return llvm::formatv("{0:$[, ]}",
                       llvm::make_range(arguments.begin(), arguments.end()));
}

// Parse a template_argument.
//
//  template_argument:
//    type_id
//    id_expression
//
std::string Parser::ParseTemplateArgument() {
  // There is no way to know at this point whether there is going to be a
  // type_id or something else. Try different options one by one.

  {
    // [temp.arg](http://eel.is/c++draft/temp.arg#2)
    //
    // In a template-argument, an ambiguity between a type-id and an expression
    // is resolved to a type-id, regardless of the form of the corresponding
    // template-parameter.

    // Therefore, first try parsing type_id.
    TentativeParsingAction tentative_parsing(this);

    TypeDeclaration type_decl = ParseTypeId();

    if (type_decl.IsValid() && ResolveTypeFromTypeDecl(type_decl)) {
      // Successfully parsed a type_id, check if the next token can finish the
      // template_argument. If so, commit the parsed tokens and return parsed
      // template_argument.
      if (token_.isOneOf(clang::tok::comma, clang::tok::greater)) {
        tentative_parsing.Commit();
        return type_decl.GetName();
      }
    }
    // Failed to parse a type_id. Rollback the parser and try something else.
    tentative_parsing.Rollback();
  }

  {
    // The next candidate is an id_expression. This can fail too, so prepare to
    // rollback again.
    TentativeParsingAction tentative_parsing(this);

    // TODO(werat): Don't return IdExpression from ParseIdExpression, this makes
    // it weird to perform error checking. Return either std::string (empty in
    // case of an error) or just ExprResult and check it for ErrorNode.
    auto id_expression = ParseIdExpression();

    // If we've parsed the id_expression successfully and the next token can
    // finish the template_argument, then we're done here.
    if (!HasError() && token_.isOneOf(clang::tok::comma, clang::tok::greater)) {
      tentative_parsing.Commit();
      return id_expression->name();
    }
    // Failed to parse a id_expression.
    tentative_parsing.Rollback();
  }

  // TODO(b/164399865): Another valid option here is a constant_expression. We
  // definitely don't want to support constant arithmetic like "Foo<1+2>", but
  // simple constants should be covered.
  // We can probably use ParsePrimaryExpression here, but need to figure out the
  // "stringification", since ParsePrimaryExpression returns ExprResult (and
  // potentially a whole expression, not just a single constant.)

  // This is not a template_argument.
  return "";
}

// Parse a ptr_operator.
//
//  ptr_operator:
//    "*" {cv_qualifier_seq}
//    "&"
//
void Parser::ParsePtrOperator(TypeDeclaration* type_decl) {
  ExpectOneOf(clang::tok::star, clang::tok::amp);

  if (token_.is(clang::tok::star)) {
    type_decl->ptr_operators_.push_back(clang::tok::star);
    ConsumeToken();

    //
    //  cv_qualifier_seq:
    //    cv_qualifier {cv_qualifier_seq}
    //
    //  cv_qualifier:
    //    "const"
    //    "volatile"
    //
    while (IsCvQualifier(token_)) {
      // Just ignore CV quialifiers, we don't use them in type casting.
      ConsumeToken();
    }

  } else if (token_.is(clang::tok::amp)) {
    type_decl->ptr_operators_.push_back(clang::tok::amp);
    ConsumeToken();
  }
}

bool Parser::ResolveTypeFromTypeDecl(const TypeDeclaration& type_decl) {
  // TODO(werat): This is an over-broad simplification, because TypeDeclaration
  // doesn't have any type validation at the moment. Types like "char char" are
  // considered valid and built-in, yet they don't resolve to anything. For the
  // sake of parsing it's OK, it will be handled at the evaluation step.
  if (type_decl.is_builtin_) {
    return true;
  }

  // Resolve the type in the current expression context.
  lldb::SBType type =
      expr_ctx_->ResolveTypeByName(type_decl.GetBaseName().c_str());

  return type.IsValid();
}

bool Parser::IsSimpleTypeSpecifierKeyword(clang::Token token) const {
  return token.isOneOf(
      clang::tok::kw_char, clang::tok::kw_char16_t, clang::tok::kw_char32_t,
      clang::tok::kw_wchar_t, clang::tok::kw_bool, clang::tok::kw_short,
      clang::tok::kw_int, clang::tok::kw_long, clang::tok::kw_signed,
      clang::tok::kw_unsigned, clang::tok::kw_float, clang::tok::kw_double,
      clang::tok::kw_void);
}

bool Parser::IsCvQualifier(clang::Token token) const {
  return token.isOneOf(clang::tok::kw_const, clang::tok::kw_volatile);
}

bool Parser::IsPtrOperator(clang::Token token) const {
  return token.isOneOf(clang::tok::star, clang::tok::amp);
}

// Parse an id_expression.
//
//  id_expression:
//    unqualified_id
//    qualified_id
//
//  qualified_id:
//    {"::"} {nested_name_specifier} unqualified_id
//    {"::"} identifier
//
//  identifier:
//    ? clang::tok::identifier ?
//
IdExpression Parser::ParseIdExpression() {
  // Try parsing optional global scope operator.
  bool global_scope = false;
  if (token_.is(clang::tok::coloncolon)) {
    global_scope = true;
    ConsumeToken();
  }

  // Try parsing optional nested_name_specifier.
  auto nested_name_specifier = ParseNestedNameSpecifier();

  // If nested_name_specifier is present, then it's qualified_id production.
  // Follow the first production rule.
  if (!nested_name_specifier.empty()) {
    // Parse unqualified_id and construct a fully qualified id expression.
    auto unqualified_id = ParseUnqualifiedId();

    auto id_expression = llvm::formatv("{0}{1}{2}", global_scope ? "::" : "",
                                       nested_name_specifier, unqualified_id);
    return std::make_unique<IdentifierNode>(id_expression);
  }

  // No nested_name_specifier, but with global scope -- this is also a
  // qualified_id production. Follow the second production rule.
  else if (global_scope) {
    Expect(clang::tok::identifier);
    std::string identifier = pp_->getSpelling(token_);
    ConsumeToken();
    auto id_expression =
        llvm::formatv("{0}{1}", global_scope ? "::" : "", identifier);
    return std::make_unique<IdentifierNode>(id_expression);
  }

  // This is unqualified_id production.
  auto unqualified_id = ParseUnqualifiedId();
  return std::make_unique<IdentifierNode>(unqualified_id);
}

// Parse an unqualified_id.
//
//  unqualified_id:
//    identifier
//
//  identifier:
//    ? clang::tok::identifier ?
//
std::string Parser::ParseUnqualifiedId() {
  Expect(clang::tok::identifier);
  std::string identifier = pp_->getSpelling(token_);
  ConsumeToken();
  return identifier;
}

// Parse a numeric_literal.
//
//  numeric_literal:
//    ? clang::tok::numeric_constant ?
//
ExprResult Parser::ParseNumericLiteral() {
  Expect(clang::tok::numeric_constant);
  ExprResult numeric_constant = ParseNumericConstant(token_);
  ConsumeToken();
  return numeric_constant;
}

// Parse an boolean_literal.
//
//  boolean_literal:
//    "true"
//    "false"
//
ExprResult Parser::ParseBooleanLiteral() {
  ExpectOneOf(clang::tok::kw_true, clang::tok::kw_false);
  bool literal_value = token_.is(clang::tok::kw_true);
  ConsumeToken();
  return std::make_unique<BooleanLiteralNode>(literal_value);
}

ExprResult Parser::ParseNumericConstant(clang::Token token) {
  // Parse numeric constant, it can be either integer or float.
  std::string tok_spelling = pp_->getSpelling(token);

#if LLVM_VERSION_MAJOR >= 11
  clang::NumericLiteralParser literal(
      tok_spelling, token.getLocation(), pp_->getSourceManager(),
      pp_->getLangOpts(), pp_->getTargetInfo(), pp_->getDiagnostics());
#else
  clang::NumericLiteralParser literal(tok_spelling, token.getLocation(), *pp_);
#endif

  if (literal.hadError) {
    BailOut(
        "Failed to parse token as numeric-constant: " + TokenDescription(token),
        token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  // Check for floating-literal and integer-literal. Fail on anything else (i.e.
  // fixed-point literal, who needs them anyway??).
  if (literal.isFloatingLiteral()) {
    return ParseFloatingLiteral(literal, token);
  }
  if (literal.isIntegerLiteral()) {
    return ParseIntegerLiteral(literal, token);
  }

  // Don't care about anything else.
  BailOut("numeric-constant should be either float or integer literal: " +
              TokenDescription(token),
          token.getLocation());
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::ParseFloatingLiteral(clang::NumericLiteralParser& literal,
                                        clang::Token token) {
  const llvm::fltSemantics& format = literal.isFloat
                                         ? llvm::APFloat::IEEEsingle()
                                         : llvm::APFloat::IEEEdouble();
  llvm::APFloat raw_value(format);
  llvm::APFloat::opStatus result = literal.GetFloatValue(raw_value);

  // Overflow is always an error, but underflow is only an error if we
  // underflowed to zero (APFloat reports denormals as underflow).
  if ((result & llvm::APFloat::opOverflow) ||
      ((result & llvm::APFloat::opUnderflow) && raw_value.isZero())) {
    BailOut("float underflow/overflow happened: " + TokenDescription(token),
            token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  Scalar value = literal.isFloat ? Scalar(raw_value.convertToFloat())
                                 : Scalar(raw_value.convertToDouble());

  return std::make_unique<NumericLiteralNode>(value);
}

ExprResult Parser::ParseIntegerLiteral(clang::NumericLiteralParser& literal,
                                       clang::Token token) {
  // Create a value big enough to fit all valid numbers.
  llvm::APInt raw_value(TYPE_WIDTH(uintmax_t), 0);

  if (literal.GetIntegerValue(raw_value)) {
    BailOut(
        "integer literal is too large to be represented in any integer "
        "type: " +
            TokenDescription(token),
        token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  Scalar value;
  IntegerType int_type = PickIntegerType(literal, raw_value);

  if (int_type.is_unsigned) {
    uint64_t v = raw_value.getZExtValue();

    if (int_type.width == 32) {
      value.SetValueUInt32(static_cast<uint32_t>(v));
    } else if (int_type.width == 64) {
      value.SetValueUInt64(v);
    }
  } else {
    int64_t v = raw_value.getSExtValue();

    if (int_type.width == 32) {
      value.SetValueInt32(static_cast<int32_t>(v));
    } else if (int_type.width == 64) {
      value.SetValueInt64(v);
    }
  }

  if (value.type_ == Scalar::Type::INVALID) {
    BailOut("unexpected int width (" + std::to_string(int_type.width) +
                ") for numeric constant: " + TokenDescription(token),
            token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<NumericLiteralNode>(value);
}

}  // namespace lldb_eval
