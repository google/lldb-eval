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

#ifndef INCLUDE_AST_VISITOR_H
#define INCLUDE_AST_VISITOR_H

#include <variant>

#include "ast.h"

namespace fuzzer {

class Visitor {
 public:
  virtual ~Visitor() {}

  virtual void visit(const UnaryExpr& e) = 0;
  virtual void visit(const BinaryExpr& e) = 0;
  virtual void visit(const VariableExpr& e) = 0;
  virtual void visit(const IntegerConstant& e) = 0;
  virtual void visit(const DoubleConstant& e) = 0;

  void visit(const Expr& e) { std::visit(*this, e); }

  void operator()(const UnaryExpr& e) { visit(e); }
  void operator()(const BinaryExpr& e) { visit(e); }
  void operator()(const VariableExpr& e) { visit(e); }
  void operator()(const IntegerConstant& e) { visit(e); }
  void operator()(const DoubleConstant& e) { visit(e); }
};

}  // namespace fuzzer

#endif  // INCLUDE_AST_VISITOR_H
