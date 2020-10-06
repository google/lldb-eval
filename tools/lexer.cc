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

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/IdentifierTable.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/ModuleLoader.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/Token.h"
#include "llvm/Support/Host.h"

int main(int argc, char** argv) {
  (void)argc;

  // "parse" command line arguments.
  std::string expr = argv[1];

  clang::SourceManagerForFile smff("<expr>", expr);

  clang::SourceManager& sm = smff.get();
  clang::DiagnosticsEngine& de = sm.getDiagnostics();
  de.setClient(new clang::IgnoringDiagConsumer);

  // Create default target info.
  auto tOpts = std::make_shared<clang::TargetOptions>();
  tOpts->Triple = llvm::sys::getDefaultTargetTriple();
  std::unique_ptr<clang::TargetInfo> ti(
      clang::TargetInfo::CreateTargetInfo(de, tOpts));

  clang::LangOptions lang_opts;
  lang_opts.Bool = true;
  lang_opts.WChar = true;
  lang_opts.CPlusPlus = true;
  lang_opts.CPlusPlus11 = true;
  lang_opts.CPlusPlus14 = true;
  lang_opts.CPlusPlus17 = true;

  auto hOpts = std::make_shared<clang::HeaderSearchOptions>();
  clang::HeaderSearch hs(hOpts, sm, de, lang_opts, ti.get());
  clang::TrivialModuleLoader tml;

  auto pOpts = std::make_shared<clang::PreprocessorOptions>();
  clang::Preprocessor pp(pOpts, de, lang_opts, sm, hs, tml);

  pp.Initialize(*ti);
  pp.EnterMainSourceFile();

  clang::Token token;
  token.setKind(clang::tok::unknown);

  while (token.isNot(clang::tok::eof)) {
    pp.Lex(token);

    pp.DumpToken(token);
    std::cerr << std::endl;
  }

  return 0;
}
