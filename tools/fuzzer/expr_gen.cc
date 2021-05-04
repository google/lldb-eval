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

#include "tools/fuzzer/expr_gen.h"

#include <algorithm>
#include <array>
#include <cassert>
#include <functional>
#include <optional>
#include <random>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <variant>

#include "lldb-eval/defines.h"
#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/constraints.h"
#include "tools/fuzzer/enum_bitset.h"
#include "tools/fuzzer/symbol_table.h"

namespace fuzzer {

class Weights {
 public:
  using ExprWeightsArray = std::array<float, NUM_GEN_EXPR_KINDS>;
  using TypeWeightsArray = std::array<float, NUM_GEN_TYPE_KINDS>;

  ExprWeightsArray& expr_weights() { return expr_weights_; }
  const ExprWeightsArray& expr_weights() const { return expr_weights_; }

  TypeWeightsArray& type_weights() { return type_weights_; }
  const TypeWeightsArray& type_weights() const { return type_weights_; }

  float& operator[](ExprKind kind) { return expr_weights_[(size_t)kind]; }
  float& operator[](TypeKind kind) { return type_weights_[(size_t)kind]; }

  const float& operator[](ExprKind kind) const {
    return expr_weights_[(size_t)kind];
  }
  const float& operator[](TypeKind kind) const {
    return type_weights_[(size_t)kind];
  }

  int depth() const { return depth_; }
  void increment_depth() { depth_++; }

 private:
  std::array<float, NUM_GEN_EXPR_KINDS> expr_weights_;
  std::array<float, NUM_GEN_TYPE_KINDS> type_weights_;

  int depth_ = 0;
};

using ScalarMask = EnumBitset<ScalarType>;

int expr_precedence(const Expr& e) {
  return std::visit([](const auto& e) { return e.precedence(); }, e);
}

// A helper function that constructs a binary expression and wraps operands
// in parenthesis if necessary.
static Expr make_binary_expr(Expr lhs, BinOp op, Expr rhs) {
  // Rules for parenthesising the left hand side:
  // 1. If the left hand side has a strictly lower precedence than ours,
  //    then we will have to emit parens.
  //    Example: We emit `(3 + 4) * 5` instead of `3 + 4 * 5`.
  // 2. If the left hand side has the same precedence as we do, then we
  //    don't have to emit any parens. This is because all lldb-eval
  //    binary operators have left-to-right associativity.
  //    Example: We do not have to emit `(3 - 4) + 5`, `3 - 4 + 5` will
  //    also do.
  if (expr_precedence(lhs) > bin_op_precedence(op)) {
    lhs = ParenthesizedExpr(std::move(lhs));
  }

  // Rules for parenthesising the right hand side:
  // 1. If the right hand side has a strictly lower precedence than
  // ours,
  //    then we will have to emit parens.
  //    Example: We emit `5 * (3 + 4)` instead of `5 * 3 + 4`.
  // 2. If the right hand side has the same precedence as we do, then we
  //    should emit parens for good measure. This is because all
  //    lldb-eval binary operators have left-to-right associativity and
  //    we do not want to violate this with respect to the generated
  //    AST. Example: We emit `3 - (4 + 5)` instead of `3 - 4 + 5`. We
  //    also emit `3 + (4 + 5)` instead of `3 + 4 + 5`, even though both
  //    expressions are equivalent.
  if (expr_precedence(rhs) >= bin_op_precedence(op)) {
    rhs = ParenthesizedExpr(std::move(rhs));
  }

  return BinaryExpr(std::move(lhs), op, std::move(rhs));
}

std::optional<Expr> ExprGenerator::gen_boolean_constant_impl(
    const ExprConstraints& constraints) {
  const auto& type_constraints = constraints.type_constraints();

  if (constraints.must_be_lvalue() ||
      constraints.memory_constraints().must_be_valid() ||
      !type_constraints.allows_any_of(INT_TYPES | FLOAT_TYPES)) {
    return {};
  }

  return BooleanConstant(rng_->gen_boolean());
}

std::optional<Expr> ExprGenerator::gen_nullptr_constant(
    const ExprConstraints& constraints) {
  const auto& type_constraints = constraints.type_constraints();

  if (constraints.must_be_lvalue() ||
      constraints.memory_constraints().must_be_valid() ||
      !type_constraints.allows_nullptr()) {
    return {};
  }

  return NullptrConstant();
}

std::optional<Expr> ExprGenerator::gen_integer_constant_impl(
    const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue() ||
      constraints.memory_constraints().must_be_valid()) {
    return {};
  }

  const auto& type_constraints = constraints.type_constraints();

  // Integers can be generated in place of floats
  if (type_constraints.allows_any_of(INT_TYPES | FLOAT_TYPES)) {
    return rng_->gen_integer_constant(cfg_.int_const_min, cfg_.int_const_max);
  }

  if (type_constraints.allows_literal_zero()) {
    return IntegerConstant(0);
  }

  return {};
}

std::optional<Expr> ExprGenerator::gen_double_constant_impl(
    const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue()) {
    return {};
  }

  const auto& type_constraints = constraints.type_constraints();
  if (type_constraints.allows_any_of(FLOAT_TYPES)) {
    // Make sure a double constant isn't reached with valid pointer constraint.
    assert(!constraints.memory_constraints().must_be_valid() &&
           "Floating points shouldn't be implicitly converted to pointers!");

    return rng_->gen_double_constant(cfg_.double_constant_min,
                                     cfg_.double_constant_max);
  }

  return {};
}

std::optional<Expr> ExprGenerator::gen_enum_constant_impl(
    const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue()) {
    return {};
  }

  std::vector<std::reference_wrapper<const EnumConstant>> enums;
  for (const auto& [k, v] : symtab_.enums()) {
    if (constraints.type_constraints().allows_type(k)) {
      enums.insert(enums.end(), v.begin(), v.end());
    }
  }

  if (enums.empty()) {
    return {};
  }

  return rng_->pick_enum_literal(enums);
}

std::optional<Expr> ExprGenerator::gen_variable_expr_impl(
    const ExprConstraints& constraints) {
  const auto& type_constraints = constraints.type_constraints();
  const auto& memory_constraints = constraints.memory_constraints();

  std::vector<std::reference_wrapper<const VariableExpr>> vars;
  for (const auto& [k, v] : symtab_.vars()) {
    if (type_constraints.allows_type(k)) {
      for (const auto& var : v) {
        if (var.expr.name() == "this" && constraints.must_be_lvalue()) {
          // "this" is an rvalue.
          continue;
        }
        if (var.freedom_index >= memory_constraints.required_freedom_index()) {
          vars.emplace_back(var.expr);
        }
      }
    }
  }

  if (vars.empty()) {
    return {};
  }

  return rng_->pick_variable(vars);
}

std::optional<Expr> ExprGenerator::gen_binary_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue() ||
      constraints.memory_constraints().must_be_valid()) {
    return {};
  }

  const auto& type_constraints = constraints.type_constraints();

  BinOpMask mask = cfg_.bin_op_mask;

  ScalarMask default_type_mask;
  if (type_constraints.allows_any_of(INT_TYPES)) {
    default_type_mask |= INT_TYPES;
  }
  if (type_constraints.allows_any_of(FLOAT_TYPES)) {
    default_type_mask |= FLOAT_TYPES;
  }

  if (default_type_mask.none()) {
    constexpr BinOpMask PTR_OPS = {BinOp::Plus, BinOp::Minus};
    mask &= PTR_OPS;
  }

  while (mask.any()) {
    auto op = rng_->gen_bin_op(mask);

    SpecificTypes lhs_types;
    SpecificTypes rhs_types;

    switch (op) {
      case BinOp::Mult:
      case BinOp::Div:
        lhs_types = default_type_mask;
        rhs_types = default_type_mask;
        break;

      case BinOp::BitAnd:
      case BinOp::BitOr:
      case BinOp::BitXor:
      case BinOp::Shl:
      case BinOp::Shr:
      case BinOp::Mod:
        lhs_types = INT_TYPES;
        rhs_types = INT_TYPES;
        break;

      case BinOp::LogicalAnd:
      case BinOp::LogicalOr:
        lhs_types = SpecificTypes::all_in_bool_ctx();
        rhs_types = SpecificTypes::all_in_bool_ctx();
        break;

      case BinOp::Eq:
      case BinOp::Ne:
      case BinOp::Lt:
      case BinOp::Le:
      case BinOp::Gt:
      case BinOp::Ge: {
        lhs_types = INT_TYPES | FLOAT_TYPES;
        rhs_types = INT_TYPES | FLOAT_TYPES;

        // Try and see if we can generate a pointer or scoped enum type.
        // If not, we'll just compare scalars.
        bool gen_ptr_or_enum =
            rng_->gen_binop_ptr_or_enum(cfg_.binop_gen_ptr_or_enum_prob);
        if (gen_ptr_or_enum) {
          SpecificTypes types = SpecificTypes::all_in_pointer_ctx();
          types.allow_scoped_enums();
          auto maybe_type =
              gen_type(weights, types, /*allow_array_types*/ true);
          if (maybe_type.has_value()) {
            const auto& type = maybe_type.value();
            lhs_types = SpecificTypes(type);
            rhs_types = SpecificTypes(type);

            // `nullptr` is only allowed with equality and ineqality operators.
            if (op != BinOp::Eq && op != BinOp::Ne) {
              lhs_types.disallow_nullptr();
              rhs_types.disallow_nullptr();
            }
          }
        }
      } break;

      case BinOp::Plus:
      case BinOp::Minus: {
        bool allows_scalars = default_type_mask.any();
        bool allows_pointers = type_constraints.allows_pointer();

        if (allows_scalars && allows_pointers) {
          if (rng_->gen_binop_ptr_expr(cfg_.binop_gen_ptr_expr_prob)) {
            allows_scalars = false;
          } else {
            allows_pointers = false;
          }
        }

        if (!allows_scalars && !allows_pointers) {
          mask[op] = false;
          continue;
        }

        if (allows_pointers) {
          auto maybe_type = gen_type(weights, type_constraints);
          if (!maybe_type.has_value()) {
            mask[op] = false;
            continue;
          }
          const auto& type = maybe_type.value();
          const auto* ptr_type = std::get_if<PointerType>(&type);
          if (ptr_type == nullptr) {
            mask[op] = false;
            continue;
          }
          const auto* scalar_type =
              std::get_if<ScalarType>(&ptr_type->type().type());
          if (scalar_type != nullptr && *scalar_type == ScalarType::Void) {
            mask[op] = false;
            continue;
          }

          lhs_types = SpecificTypes(type);
          rhs_types = INT_TYPES;

          if (op == BinOp::Plus &&
              rng_->gen_binop_flip_operands(cfg_.binop_flip_operands_prob)) {
            std::swap(lhs_types, rhs_types);
          }
        } else {
          if (op == BinOp::Minus &&
              rng_->gen_binop_ptrdiff_expr(cfg_.binop_gen_ptrdiff_expr_prob)) {
            auto maybe_type = gen_type(weights, type_constraints);

            if (!maybe_type.has_value()) {
              mask[op] = false;
              continue;
            }
            const auto& type = maybe_type.value();
            const auto* ptr_type = std::get_if<PointerType>(&type);
            if (ptr_type == nullptr) {
              mask[op] = false;
              continue;
            }
            const auto* scalar_type =
                std::get_if<ScalarType>(&ptr_type->type().type());
            if (scalar_type != nullptr && *scalar_type == ScalarType::Void) {
              mask[op] = false;
              continue;
            }

            lhs_types = SpecificTypes(type);
            rhs_types = SpecificTypes(type);
          } else {
            lhs_types = default_type_mask;
            rhs_types = default_type_mask;
          }
        }
      } break;

      default:
        lldb_eval_unreachable(
            "Unhandled switch case, did you introduce a new binary "
            "operator?");
    }

    if (lhs_types.allows_any_of(INT_TYPES)) {
      lhs_types.allow_unscoped_enums();
    }

    if (rhs_types.allows_any_of(INT_TYPES)) {
      rhs_types.allow_unscoped_enums();
    }

    TypeConstraints lhs_constraints = std::move(lhs_types);
    TypeConstraints rhs_constraints = std::move(rhs_types);

    auto maybe_lhs = gen_with_weights(weights, std::move(lhs_constraints));
    if (!maybe_lhs.has_value()) {
      mask[op] = false;
      continue;
    }
    Expr lhs = std::move(maybe_lhs.value());

    auto maybe_rhs = gen_with_weights(weights, std::move(rhs_constraints));
    if (!maybe_rhs.has_value()) {
      mask[op] = false;
      continue;
    }
    Expr rhs = std::move(maybe_rhs.value());

    return make_binary_expr(std::move(lhs), op, std::move(rhs));
  }

  return {};
}

std::optional<Expr> ExprGenerator::gen_unary_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue() ||
      constraints.memory_constraints().must_be_valid()) {
    return {};
  }

  const auto& type_constraints = constraints.type_constraints();

  ScalarMask default_type_mask;
  if (type_constraints.allows_any_of(INT_TYPES)) {
    default_type_mask |= INT_TYPES;
  }
  if (type_constraints.allows_any_of(FLOAT_TYPES)) {
    default_type_mask |= FLOAT_TYPES;
  }

  if (default_type_mask.none()) {
    return {};
  }

  UnOpMask mask = cfg_.un_op_mask;
  while (mask.any()) {
    auto op = (UnOp)rng_->gen_un_op(mask);

    SpecificTypes expr_types;
    switch (op) {
      case UnOp::Plus:
      case UnOp::Neg:
        expr_types = default_type_mask;
        break;

      case UnOp::BitNot:
        expr_types = INT_TYPES;
        break;

      case UnOp::LogicalNot:
        expr_types = SpecificTypes::all_in_bool_ctx();
        break;

      default:
        lldb_eval_unreachable(
            "Unhandled switch case, did you introduce a new unary "
            "operator?");
    }

    auto maybe_expr =
        gen_with_weights(weights, TypeConstraints(std::move(expr_types)));
    if (!maybe_expr.has_value()) {
      mask[op] = false;
      continue;
    }
    Expr expr = std::move(maybe_expr.value());

    if (expr_precedence(expr) > UnaryExpr::PRECEDENCE) {
      expr = ParenthesizedExpr(expr);
    }

    return UnaryExpr(op, std::move(expr));
  }

  return {};
}

std::optional<Expr> ExprGenerator::gen_ternary_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  auto maybe_cond = gen_with_weights(
      weights, TypeConstraints(SpecificTypes::all_in_bool_ctx()));
  if (!maybe_cond.has_value()) {
    return {};
  }
  Expr cond = std::move(maybe_cond.value());

  const auto& type_constraints = constraints.type_constraints();
  auto maybe_type = gen_type(weights, type_constraints);
  if (!maybe_type.has_value()) {
    return {};
  }
  auto& type = maybe_type.value();

  ExprConstraints lhs_constraints;
  ExprConstraints rhs_constraints;

  if (constraints.must_be_lvalue()) {
    lhs_constraints =
        ExprConstraints(SpecificTypes(type), constraints.memory_constraints(),
                        ExprCategory::Lvalue);
    rhs_constraints = lhs_constraints;
  } else if (std::holds_alternative<ScalarType>(type)) {
    ScalarMask mask = INT_TYPES;
    if (type_constraints.allows_any_of(FLOAT_TYPES)) {
      mask |= FLOAT_TYPES;
    }

    SpecificTypes allowed_types = mask;
    if (allowed_types.allows_any_of(INT_TYPES)) {
      allowed_types.allow_unscoped_enums();
    }

    lhs_constraints =
        ExprConstraints(allowed_types, constraints.memory_constraints());
    rhs_constraints = lhs_constraints;
  } else {
    SpecificTypes allowed_types(type);
    lhs_constraints = ExprConstraints(TypeConstraints(allowed_types),
                                      constraints.memory_constraints());
    if (std::holds_alternative<PointerType>(type) ||
        std::holds_alternative<NullptrType>(type)) {
      // Disallow `0` literal in pointer context in one child expression. This
      // prevents invalid casts from `int` to pointer types, e.g.
      // `static_cast<int*>(cond ? 0 : 0)`.
      allowed_types.disallow_literal_zero();
    }
    rhs_constraints = ExprConstraints(TypeConstraints(allowed_types),
                                      constraints.memory_constraints());
    if (rng_->gen_binop_flip_operands(cfg_.binop_flip_operands_prob)) {
      std::swap(lhs_constraints, rhs_constraints);
    }
  }

  auto maybe_lhs = gen_with_weights(weights, lhs_constraints);
  if (!maybe_lhs.has_value()) {
    return {};
  }
  Expr lhs = std::move(maybe_lhs.value());

  auto maybe_rhs = gen_with_weights(weights, rhs_constraints);
  if (!maybe_rhs.has_value()) {
    return {};
  }
  Expr rhs = std::move(maybe_rhs.value());

  if (expr_precedence(cond) == TernaryExpr::PRECEDENCE) {
    cond = ParenthesizedExpr(cond);
  }

  return TernaryExpr(std::move(cond), std::move(lhs), std::move(rhs));
}

std::optional<Expr> ExprGenerator::gen_cast_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue()) {
    return {};
  }

  auto maybe_type = gen_type(weights, constraints.type_constraints());
  if (!maybe_type.has_value()) {
    return {};
  }
  Type type = std::move(maybe_type.value());

  CastKindMask mask = CastKindMask::all_set();
  if (cfg_.cv_qualifiers_enabled) {
    // C++ style casts are sensitive to casting away type qualifiers.
    // The fuzzer doesn't support this constraint currently.
    mask[CastExpr::Kind::StaticCast] = false;
    mask[CastExpr::Kind::ReinterpretCast] = false;
  }

  while (mask.any()) {
    SpecificTypes expr_types;
    auto kind = rng_->gen_cast_kind(mask);
    switch (kind) {
      case CastExpr::Kind::CStyleCast:
        expr_types = SpecificTypes::cast_to(type);
        break;
      case CastExpr::Kind::StaticCast:
        expr_types = SpecificTypes::static_cast_to(type);
        break;
      case CastExpr::Kind::ReinterpretCast:
        expr_types = SpecificTypes::reinterpret_cast_to(type);
        break;
    }

    if (!expr_types.satisfiable()) {
      mask[kind] = false;
      continue;
    }

    ExprConstraints new_constraints = ExprConstraints(
        std::move(expr_types), constraints.memory_constraints());
    auto maybe_expr = gen_with_weights(weights, new_constraints);

    if (!maybe_expr.has_value()) {
      mask[kind] = false;
      continue;
    }

    Expr expr = std::move(maybe_expr.value());
    if (kind == CastExpr::Kind::CStyleCast &&
        expr_precedence(expr) > cast_kind_precedence(kind)) {
      expr = ParenthesizedExpr(std::move(expr));
    }

    return CastExpr(kind, std::move(type), std::move(expr));
  }

  return {};
}

std::optional<Expr> ExprGenerator::gen_address_of_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  const auto& memory_constraints = constraints.memory_constraints();

  if (constraints.must_be_lvalue()) {
    return {};
  }

  if (memory_constraints.must_be_valid() &&
      memory_constraints.required_freedom_index() == 0) {
    return {};
  }

  TypeConstraints new_type_constraints =
      constraints.type_constraints().allowed_to_point_to();
  ExprConstraints new_constraints(std::move(new_type_constraints),
                                  memory_constraints.from_address_of(),
                                  ExprCategory::Lvalue);

  auto maybe_expr = gen_with_weights(weights, new_constraints);
  if (!maybe_expr.has_value()) {
    return {};
  }
  Expr expr = std::move(maybe_expr.value());

  if (expr_precedence(expr) > AddressOf::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return AddressOf(std::move(expr));
}

std::optional<Expr> ExprGenerator::gen_member_of_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  const auto& type_constraints = constraints.type_constraints();
  const auto& memory_constraints = constraints.memory_constraints();

  if (memory_constraints.required_freedom_index() > 0) {
    // TODO: Think about options to handle memory constraints for member-of
    // fields. Freedom index isn't tied to type field, but to the actual value.
    // Even if the required freedom index is 0, it can still be problematic to
    // access reference fields, e.g. in `(*(Type*)ptr_int).ref_field`.
    return {};
  }

  std::vector<std::reference_wrapper<const Field>> fields;
  for (const auto& [k, v] : symtab_.fields_by_type()) {
    if (type_constraints.allows_type(k)) {
      fields.insert(fields.end(), v.begin(), v.end());
    }
  }

  if (fields.empty()) {
    return {};
  }

  Field field = rng_->pick_field(fields);
  ExprConstraints new_constraints = ExprConstraints(
      SpecificTypes(field.containing_type()),
      memory_constraints.from_member_of(constraints.must_be_lvalue(), 0));
  auto maybe_expr = gen_with_weights(weights, std::move(new_constraints));
  if (!maybe_expr.has_value()) {
    return {};
  }
  Expr expr = std::move(maybe_expr.value());

  if (expr_precedence(expr) > MemberOf::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return MemberOf(std::move(expr), field.name());
}

std::optional<Expr> ExprGenerator::gen_member_of_ptr_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  const auto& type_constraints = constraints.type_constraints();
  const auto& memory_constraints = constraints.memory_constraints();

  if (memory_constraints.required_freedom_index() > 0) {
    // TODO: Think about options to handle memory constraints for member-of
    // fields.
    return {};
  }

  std::vector<std::reference_wrapper<const Field>> fields;

  for (const auto& [k, v] : symtab_.fields_by_type()) {
    if (type_constraints.allows_type(k)) {
      fields.insert(fields.end(), v.begin(), v.end());
    }
  }

  if (fields.empty()) {
    return {};
  }

  Field field = rng_->pick_field(fields);
  TypeConstraints new_type_constraints = SpecificTypes(field.containing_type());
  ExprConstraints new_constraints = ExprConstraints(
      new_type_constraints.make_pointer_constraints(),
      memory_constraints.from_member_of(constraints.must_be_lvalue(), 1));
  auto maybe_expr = gen_with_weights(weights, std::move(new_constraints));
  if (!maybe_expr.has_value()) {
    return {};
  }
  Expr expr = std::move(maybe_expr.value());

  if (expr_precedence(expr) > MemberOfPtr::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return MemberOfPtr(std::move(expr), field.name());
}

std::optional<Expr> ExprGenerator::gen_array_index_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  const auto& type_constraints = constraints.type_constraints();

  Expr lhs;
  Expr rhs;
  if (constraints.memory_constraints().must_be_valid() ||
      !constraints.must_be_lvalue()) {
    // If the expression that is being constructed is going to read from memory,
    // we are going to construct an expression in the form of
    // `array_type[int_expr % array_size]` (or its flipped alternative).
    // The reason for this special case is to reduce the "invalid memory access"
    // noise produced by the fuzzer in a general case (see the `else`
    // statement).

    auto maybe_type =
        gen_array_type(type_constraints.make_pointer_constraints());
    if (!maybe_type.has_value()) {
      return {};
    }

    const auto* array_type = std::get_if<ArrayType>(&maybe_type.value());
    assert(array_type != nullptr &&
           "Non-array type received by the gen_array_type!");

    auto array_size = IntegerConstant(
        array_type->size(), IntegerConstant::Base::Dec,
        IntegerConstant::Length::Int, IntegerConstant::Signedness::Unsigned);

    TypeConstraints lhs_constraints =
        SpecificTypes(std::move(maybe_type.value()));
    TypeConstraints rhs_constraints = SpecificTypes(INT_TYPES);

    bool flip_operands =
        rng_->gen_binop_flip_operands(cfg_.binop_flip_operands_prob);
    if (flip_operands) {
      std::swap(lhs_constraints, rhs_constraints);
    }

    auto maybe_lhs = gen_with_weights(weights, lhs_constraints);
    if (!maybe_lhs.has_value()) {
      return {};
    }
    lhs = std::move(maybe_lhs.value());

    auto maybe_rhs = gen_with_weights(weights, rhs_constraints);
    if (!maybe_rhs.has_value()) {
      return {};
    }
    rhs = std::move(maybe_rhs.value());

    Expr& index_expr = flip_operands ? lhs : rhs;
    index_expr = make_binary_expr(std::move(index_expr), BinOp::Mod,
                                  std::move(array_size));
  } else {
    // If we aren't going to read from memory (i.e. the parent is address-of),
    // any kind of array access expression is allowed.

    TypeConstraints lhs_constraints =
        constraints.type_constraints().make_pointer_constraints();
    TypeConstraints rhs_constraints = SpecificTypes(INT_TYPES);

    if (rng_->gen_binop_flip_operands(cfg_.binop_flip_operands_prob)) {
      std::swap(lhs_constraints, rhs_constraints);
    }

    auto maybe_lhs = gen_with_weights(weights, lhs_constraints);
    if (!maybe_lhs.has_value()) {
      return {};
    }
    lhs = std::move(maybe_lhs.value());

    auto maybe_rhs = gen_with_weights(weights, rhs_constraints);
    if (!maybe_rhs.has_value()) {
      return {};
    }
    rhs = std::move(maybe_rhs.value());
  }

  if (expr_precedence(lhs) > ArrayIndex::PRECEDENCE) {
    lhs = ParenthesizedExpr(std::move(lhs));
  }

  return ArrayIndex(std::move(lhs), std::move(rhs));
}

std::optional<Expr> ExprGenerator::gen_dereference_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  ExprConstraints new_constraints =
      ExprConstraints(constraints.type_constraints().make_pointer_constraints(),
                      constraints.memory_constraints().from_dereference_of(
                          constraints.must_be_lvalue()));

  auto maybe_expr = gen_with_weights(weights, new_constraints);
  if (!maybe_expr.has_value()) {
    return {};
  }
  Expr expr = std::move(maybe_expr.value());

  if (expr_precedence(expr) > DereferenceExpr::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return DereferenceExpr(std::move(expr));
}

std::optional<Expr> ExprGenerator::gen_function_call_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue()) {
    return {};
  }

  const auto& type_constraints = constraints.type_constraints();

  std::vector<std::reference_wrapper<const Function>> functions;
  for (const auto& [k, v] : symtab_.functions()) {
    if (type_constraints.allows_type(k)) {
      functions.insert(functions.end(), v.begin(), v.end());
    }
  }

  if (functions.empty()) {
    return {};
  }

  const auto& function = rng_->pick_function(functions);

  std::vector<std::shared_ptr<Expr>> args;
  for (const auto& argument_type : function.argument_types()) {
    TypeConstraints allowed_types =
        SpecificTypes::implicit_cast_to(argument_type);
    auto maybe_expr = gen_with_weights(weights, std::move(allowed_types));
    if (!maybe_expr.has_value()) {
      return {};
    }
    args.emplace_back(std::make_shared<Expr>(std::move(maybe_expr.value())));
  }

  return FunctionCallExpr(function.name(), std::move(args));
}

std::optional<Expr> ExprGenerator::gen_sizeof_expr_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  if (constraints.must_be_lvalue() ||
      !constraints.type_constraints().allows_any_of(INT_TYPES)) {
    return {};
  }

  bool gen_sizeof_type = rng_->gen_sizeof_type(cfg_.sizeof_gen_type_prob);
  if (gen_sizeof_type) {
    // `sizeof` can take any non-void type. However, it seems that LLDB can't
    // handle expressions such as `sizeof(StructType)` in all cases without
    // explicitly specifying `struct` or `class` keyword, e.g.
    // `sizeof(struct StructType)`. Because of that, we limit generation of
    // types only to pointers, scalar types and unscoped enums.
    SpecificTypes types = SpecificTypes::all_in_pointer_ctx();
    types.allow_scalar_types(INT_TYPES | FLOAT_TYPES);
    types.allow_unscoped_enums();
    auto maybe_type = gen_type(weights, types);
    if (!maybe_type.has_value()) {
      return {};
    }
    return SizeofExpr(std::move(maybe_type.value()));
  }

  auto maybe_expr = gen_with_weights(weights, TypeConstraints(AnyType()));
  if (!maybe_expr.has_value()) {
    return {};
  }

  auto expr = std::move(maybe_expr.value());
  if (expr_precedence(expr) > SizeofExpr::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  // C-style cast expression can't be a direct child of sizeof operator.
  // TODO: Split C-style cast and C++ casts in separate classes (precedence
  // determination and check will become easier).
  if (std::holds_alternative<CastExpr>(expr) &&
      expr_precedence(expr) == SizeofExpr::PRECEDENCE) {
    expr = ParenthesizedExpr(std::move(expr));
  }

  return SizeofExpr(std::move(expr));
}

std::optional<Expr> ExprGenerator::gen_with_weights_impl(
    const Weights& weights, const ExprConstraints& constraints) {
  Weights new_weights = weights;
  new_weights.increment_depth();
  if (new_weights.depth() == cfg_.max_depth) {
    return {};
  }

  ExprKindMask mask = cfg_.expr_kind_mask;
  while (mask.any()) {
    auto kind = rng_->gen_expr_kind(new_weights, mask);
    auto idx = (size_t)kind;

    auto old_weight = new_weights[kind];
    new_weights[kind] *= cfg_.expr_kind_weights[idx].dampening_factor;

    std::optional<Expr> maybe_expr;
    switch (kind) {
      case ExprKind::IntegerConstant:
        maybe_expr = gen_integer_constant(constraints);
        break;

      case ExprKind::DoubleConstant:
        maybe_expr = gen_double_constant(constraints);
        break;

      case ExprKind::VariableExpr:
        maybe_expr = gen_variable_expr(constraints);
        break;

      case ExprKind::BinaryExpr:
        maybe_expr = gen_binary_expr(new_weights, constraints);
        break;

      case ExprKind::UnaryExpr:
        maybe_expr = gen_unary_expr(new_weights, constraints);
        break;

      case ExprKind::TernaryExpr:
        maybe_expr = gen_ternary_expr(new_weights, constraints);
        break;

      case ExprKind::BooleanConstant:
        maybe_expr = gen_boolean_constant(constraints);
        break;

      case ExprKind::NullptrConstant:
        maybe_expr = gen_nullptr_constant(constraints);
        break;

      case ExprKind::EnumConstant:
        maybe_expr = gen_enum_constant(constraints);
        break;

      case ExprKind::CastExpr:
        maybe_expr = gen_cast_expr(new_weights, constraints);
        break;

      case ExprKind::DereferenceExpr:
        maybe_expr = gen_dereference_expr(new_weights, constraints);
        break;

      case ExprKind::AddressOf:
        maybe_expr = gen_address_of_expr(new_weights, constraints);
        break;

      case ExprKind::MemberOf:
        maybe_expr = gen_member_of_expr(new_weights, constraints);
        break;

      case ExprKind::MemberOfPtr:
        maybe_expr = gen_member_of_ptr_expr(new_weights, constraints);
        break;

      case ExprKind::ArrayIndex:
        maybe_expr = gen_array_index_expr(new_weights, constraints);
        break;

      case ExprKind::FunctionCallExpr:
        maybe_expr = gen_function_call_expr(new_weights, constraints);
        break;

      case ExprKind::SizeofExpr:
        maybe_expr = gen_sizeof_expr(new_weights, constraints);
        break;

      default:
        lldb_eval_unreachable("Unhandled expression generation case");
    }

    if (!maybe_expr.has_value()) {
      new_weights[kind] = old_weight;
      mask[kind] = false;

      continue;
    }

    return maybe_parenthesized(std::move(maybe_expr.value()));
  }

  return {};
}

std::optional<Expr> ExprGenerator::gen_expr(const GenerateExprFn& callback,
                                            std::string name) {
  auto node = std::make_shared<GenNode>(std::move(name), callback);
  if (!stack_.empty()) {
    stack_.top()->children_.emplace_back(node);
  }
  stack_.push(node);
  auto maybe_expr = callback(this);
  node->valid_ = maybe_expr.has_value();
  stack_.pop();
  node_ = std::move(node);
  return maybe_expr;
}

void ExprGenerator::on_consume_random(rand_t value) {
  assert(!stack_.empty() &&
         "Stack shouldn't be empty when consuming random value!");
  stack_.top()->children_.emplace_back(value);
}

#define DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(method)               \
  std::optional<Expr> ExprGenerator::method(                        \
      const Weights& weights, const ExprConstraints& constraints) { \
    auto callback = [weights, constraints](ExprGenerator* gen) {    \
      return gen->method##_impl(weights, constraints);              \
    };                                                              \
    return gen_expr(callback, __FUNCTION__);                        \
  }

#define DEFINE_GEN_METHOD_CONSTRAINTS(method)           \
  std::optional<Expr> ExprGenerator::method(            \
      const ExprConstraints& constraints) {             \
    auto callback = [constraints](ExprGenerator* gen) { \
      return gen->method##_impl(constraints);           \
    };                                                  \
    return gen_expr(callback, __FUNCTION__);            \
  }

DEFINE_GEN_METHOD_CONSTRAINTS(gen_boolean_constant)
DEFINE_GEN_METHOD_CONSTRAINTS(gen_integer_constant)
DEFINE_GEN_METHOD_CONSTRAINTS(gen_double_constant)
DEFINE_GEN_METHOD_CONSTRAINTS(gen_enum_constant)
DEFINE_GEN_METHOD_CONSTRAINTS(gen_variable_expr)

DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_with_weights)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_binary_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_unary_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_ternary_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_cast_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_dereference_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_address_of_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_member_of_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_member_of_ptr_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_array_index_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_function_call_expr)
DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS(gen_sizeof_expr)

#undef DEFINE_GEN_METHOD_WEIGHTS_CONSTRAINTS
#undef DEFINE_GEN_METHOD_CONSTRAINTS

Expr ExprGenerator::maybe_parenthesized(Expr expr) {
  if (rng_->gen_parenthesize(cfg_.parenthesize_prob)) {
    return ParenthesizedExpr(std::move(expr));
  }

  return expr;
}

std::optional<Type> ExprGenerator::gen_type(
    const Weights& weights, const TypeConstraints& type_constraints,
    bool allow_array_types) {
  if (!type_constraints.satisfiable()) {
    return {};
  }

  Weights new_weights = weights;
  new_weights.increment_depth();
  if (new_weights.depth() == cfg_.max_depth) {
    return {};
  }

  TypeKindMask mask = TypeKindMask::all_set();

  if (type_constraints.allowed_scalar_types().none()) {
    mask[TypeKind::ScalarType] = false;
  }
  if (!type_constraints.allows_tagged_types()) {
    mask[TypeKind::TaggedType] = false;
  }
  if (!type_constraints.allows_pointer()) {
    mask[TypeKind::PointerType] = false;
  }
  if (!type_constraints.allows_void_pointer()) {
    mask[TypeKind::VoidPointerType] = false;
  }
  if (!type_constraints.allows_nullptr()) {
    mask[TypeKind::NullptrType] = false;
  }
  if (!allow_array_types || !type_constraints.allows_array_types()) {
    mask[TypeKind::ArrayType] = false;
  }

  while (mask.any()) {
    auto choice = rng_->gen_type_kind(new_weights, mask);
    auto idx = (size_t)choice;

    auto& new_type_weights = new_weights.type_weights();
    auto old_weight = new_type_weights[idx];
    new_type_weights[idx] *= cfg_.type_kind_weights[idx].dampening_factor;

    std::optional<Type> maybe_type;
    switch (choice) {
      case TypeKind::ScalarType:
        maybe_type = gen_scalar_type(type_constraints);
        break;

      case TypeKind::TaggedType:
        maybe_type = gen_tagged_type(type_constraints);
        break;

      case TypeKind::PointerType:
        maybe_type =
            gen_pointer_type(new_weights, type_constraints, allow_array_types);
        break;

      case TypeKind::VoidPointerType:
        maybe_type = gen_void_pointer_type(type_constraints);
        break;

      case TypeKind::NullptrType:
        maybe_type = NullptrType{};
        break;

      case TypeKind::EnumType:
        maybe_type = gen_enum_type(type_constraints);
        break;

      case TypeKind::ArrayType:
        maybe_type = gen_array_type(type_constraints);
        break;
    }

    if (maybe_type.has_value()) {
      return maybe_type;
    }

    new_type_weights[idx] = old_weight;
    mask[choice] = false;
  }

  return {};
}

std::optional<QualifiedType> ExprGenerator::gen_qualified_type(
    const Weights& weights, const TypeConstraints& constraints,
    bool allow_array_types) {
  auto maybe_type = gen_type(weights, constraints, allow_array_types);
  if (!maybe_type.has_value()) {
    return {};
  }
  Type type = std::move(maybe_type.value());
  auto qualifiers = gen_cv_qualifiers();

  return QualifiedType(std::move(type), qualifiers);
}

std::optional<Type> ExprGenerator::gen_pointer_type(
    const Weights& weights, const TypeConstraints& constraints,
    bool allow_array_types) {
  if (!constraints.allows_pointer() && !constraints.allows_void_pointer()) {
    return {};
  }

  auto maybe_type = gen_qualified_type(
      weights, constraints.allowed_to_point_to(), allow_array_types);
  if (!maybe_type.has_value()) {
    return {};
  }

  return PointerType(std::move(maybe_type.value()));
}

std::optional<Type> ExprGenerator::gen_void_pointer_type(
    const TypeConstraints& constraints) {
  if (!constraints.allows_void_pointer()) {
    return {};
  }

  return PointerType(QualifiedType(ScalarType::Void, gen_cv_qualifiers()));
}

std::optional<Type> ExprGenerator::gen_tagged_type(
    const TypeConstraints& constraints) {
  if (!constraints.allows_tagged_types()) {
    return {};
  }

  const auto* constraint_tagged_types = constraints.allowed_tagged_types();
  const auto& tagged_type_set = constraint_tagged_types != nullptr
                                    ? *constraint_tagged_types
                                    : symtab_.tagged_types();
  std::vector<std::reference_wrapper<const TaggedType>> tagged_types(
      tagged_type_set.begin(), tagged_type_set.end());

  return rng_->pick_tagged_type(tagged_types);
}

std::optional<Type> ExprGenerator::gen_scalar_type(
    const TypeConstraints& constraints) {
  ScalarMask mask = constraints.allowed_scalar_types();
  if (mask.none()) {
    return {};
  }

  return rng_->gen_scalar_type(mask);
}

std::optional<Type> ExprGenerator::gen_enum_type(
    const TypeConstraints& constraints) {
  std::vector<std::reference_wrapper<const EnumType>> enum_types;
  for (const auto& [enum_type, _] : symtab_.enums()) {
    if (constraints.allows_type(enum_type)) {
      enum_types.emplace_back(enum_type);
    }
  }

  if (enum_types.empty()) {
    return {};
  }

  return rng_->pick_enum_type(enum_types);
}

std::optional<Type> ExprGenerator::gen_array_type(
    const TypeConstraints& constraints) {
  // Instead of constructing a random array type, we rely on set of
  // array types from symbol table. This will increase chances to match
  // variables of array types.
  std::vector<std::reference_wrapper<const ArrayType>> array_types;
  for (const auto& type : symtab_.array_types()) {
    if (constraints.allows_type(type)) {
      array_types.emplace_back(type);
    }
  }

  if (array_types.empty()) {
    return {};
  }

  return rng_->pick_array_type(array_types);
}

CvQualifiers ExprGenerator::gen_cv_qualifiers() {
  if (cfg_.cv_qualifiers_enabled) {
    return rng_->gen_cv_qualifiers(cfg_.const_prob, cfg_.volatile_prob);
  }
  return CvQualifiers();  // empty set
}

std::optional<Expr> ExprGenerator::generate() {
  Weights weights;

  auto& expr_weights = weights.expr_weights();
  for (size_t i = 0; i < expr_weights.size(); i++) {
    expr_weights[i] = cfg_.expr_kind_weights[i].initial_weight;
  }

  auto& type_weights = weights.type_weights();
  for (size_t i = 0; i < type_weights.size(); i++) {
    type_weights[i] = cfg_.type_kind_weights[i].initial_weight;
  }

  auto allowed_types = SpecificTypes::all_in_bool_ctx();
  allowed_types.allow_scoped_enums();

  return gen_with_weights(weights, TypeConstraints(allowed_types));
}

bool ExprGenerator::mutate_gen_node(std::shared_ptr<GenNode>& node) {
  // Don't mutate invalid nodes.
  if (node->is_valid()) {
    return false;
  }

  auto maybe_expr = gen_expr(node->callback_, node->name());
  if (!maybe_expr.has_value()) {
    return false;
  }

  // The mutated node is stored in `node_`.
  auto mutated_node = node_;
  assert(mutated_node->is_valid() && "The mutated node should be valid!");

  node->children_ = std::move(mutated_node->children_);
  return true;
}

template <typename Enum, typename Rng>
Enum pick_nth_set_bit(const EnumBitset<Enum> mask, Rng& rng) {
  // At least one bit needs to be set
  assert(mask.any() && "Mask must not be empty");

  std::uniform_int_distribution<size_t> distr(1, mask.count());
  size_t choice = distr(rng);

  size_t running_ones = 0;
  for (size_t i = 0; i < mask.size(); i++) {
    if (mask[i]) {
      running_ones++;
    }

    if (running_ones == choice) {
      return (Enum)i;
    }
  }

  // `choice` lies in the range `[1, mask.count()]`, `running_ones` will
  // always lie in the range `[0, mask.count()]` and is incremented at most
  // once per loop iteration. The only way for this assertion to fire is for
  // `mask` to be empty (which we have asserted beforehand).
  lldb_eval_unreachable("Mask has no bits set");
}

template <typename Enum, typename Rng, typename RealType>
Enum weighted_pick(
    const std::array<RealType, (size_t)Enum::EnumLast + 1>& array,
    const EnumBitset<Enum>& mask, Rng& rng) {
  static_assert(std::is_floating_point_v<RealType>,
                "Must be a floating point type");

  RealType sum = 0;
  for (size_t i = 0; i < array.size(); i++) {
    sum += mask[i] ? array[i] : 0;
  }

  std::uniform_real_distribution<RealType> distr(0, sum);
  RealType choice = distr(rng);

  RealType running_sum = 0;
  for (size_t i = 0; i < array.size(); i++) {
    running_sum += mask[i] ? array[i] : 0;
    if (choice < running_sum) {
      return (Enum)i;
    }
  }

  lldb_eval_unreachable("Could not pick an element; maybe sum is 0?");
}

template <typename T, typename Rng>
const T& pick_element(const std::vector<T>& vec, Rng& rng) {
  assert(!vec.empty() && "Can't pick an element out of an empty vector");

  std::uniform_int_distribution<size_t> distr(0, vec.size() - 1);
  auto choice = distr(rng);

  return vec[choice];
}

template <typename Rng>
BinOp DefaultGeneratorRng<Rng>::gen_bin_op(BinOpMask mask) {
  return pick_nth_set_bit(mask, rng_);
}

template <typename Rng>
UnOp DefaultGeneratorRng<Rng>::gen_un_op(UnOpMask mask) {
  return pick_nth_set_bit(mask, rng_);
}

template <typename Rng>
IntegerConstant DefaultGeneratorRng<Rng>::gen_integer_constant(uint64_t min,
                                                               uint64_t max) {
  using Base = IntegerConstant::Base;
  using Length = IntegerConstant::Length;
  using Signedness = IntegerConstant::Signedness;

  std::uniform_int_distribution<uint64_t> distr(min, max);
  auto value = distr(rng_);

  std::uniform_int_distribution<int> base_distr((int)Base::EnumFirst,
                                                (int)Base::EnumLast);
  auto base = (Base)base_distr(rng_);

  std::uniform_int_distribution<int> length_distr((int)Length::EnumFirst,
                                                  (int)Length::EnumLast);
  auto length = (Length)base_distr(rng_);

  std::uniform_int_distribution<int> sign_distr((int)Signedness::EnumFirst,
                                                (int)Signedness::EnumLast);
  auto signedness = (Signedness)base_distr(rng_);

  return IntegerConstant(value, base, length, signedness);
}

template <typename Rng>
DoubleConstant DefaultGeneratorRng<Rng>::gen_double_constant(double min,
                                                             double max) {
  using Format = DoubleConstant::Format;
  using Length = DoubleConstant::Length;

  std::uniform_real_distribution<double> distr(min, max);
  auto value = distr(rng_);

  std::uniform_int_distribution<int> format_distr((int)Format::EnumFirst,
                                                  (int)Format::EnumLast);
  auto format = (Format)format_distr(rng_);

  std::uniform_int_distribution<int> length_distr((int)Length::EnumFirst,
                                                  (int)Length::EnumLast);
  auto length = (Length)length_distr(rng_);

  return DoubleConstant(value, format, length);
}

template <typename Rng>
CvQualifiers DefaultGeneratorRng<Rng>::gen_cv_qualifiers(float const_prob,
                                                         float volatile_prob) {
  std::bernoulli_distribution const_distr(const_prob);
  std::bernoulli_distribution volatile_distr(volatile_prob);

  CvQualifiers retval;
  retval[CvQualifier::Const] = const_distr(rng_);
  retval[CvQualifier::Volatile] = volatile_distr(rng_);

  return retval;
}

template <typename Rng>
VariableExpr DefaultGeneratorRng<Rng>::pick_variable(
    const std::vector<std::reference_wrapper<const VariableExpr>>& vars) {
  return pick_element(vars, rng_);
}

template <typename Rng>
Field DefaultGeneratorRng<Rng>::pick_field(
    const std::vector<std::reference_wrapper<const Field>>& fields) {
  return pick_element(fields, rng_);
}

template <typename Rng>
TaggedType DefaultGeneratorRng<Rng>::pick_tagged_type(
    const std::vector<std::reference_wrapper<const TaggedType>>& types) {
  return pick_element(types, rng_);
}

template <typename Rng>
EnumType DefaultGeneratorRng<Rng>::pick_enum_type(
    const std::vector<std::reference_wrapper<const EnumType>>& types) {
  return pick_element(types, rng_);
}

template <typename Rng>
EnumConstant DefaultGeneratorRng<Rng>::pick_enum_literal(
    const std::vector<std::reference_wrapper<const EnumConstant>>& enums) {
  return pick_element(enums, rng_);
}

template <typename Rng>
Function DefaultGeneratorRng<Rng>::pick_function(
    const std::vector<std::reference_wrapper<const Function>>& functions) {
  return pick_element(functions, rng_);
}

template <typename Rng>
ArrayType DefaultGeneratorRng<Rng>::pick_array_type(
    const std::vector<std::reference_wrapper<const ArrayType>>& types) {
  return pick_element(types, rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_binop_ptr_expr(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_binop_flip_operands(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_binop_ptrdiff_expr(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_binop_ptr_or_enum(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_sizeof_type(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_parenthesize(float probability) {
  std::bernoulli_distribution distr(probability);
  return distr(rng_);
}

template <typename Rng>
bool DefaultGeneratorRng<Rng>::gen_boolean() {
  std::bernoulli_distribution distr;
  return distr(rng_);
}

template <typename Rng>
ExprKind DefaultGeneratorRng<Rng>::gen_expr_kind(const Weights& weights,
                                                 const ExprKindMask& mask) {
  return weighted_pick(weights.expr_weights(), mask, rng_);
}

template <typename Rng>
TypeKind DefaultGeneratorRng<Rng>::gen_type_kind(const Weights& weights,
                                                 const TypeKindMask& mask) {
  return weighted_pick(weights.type_weights(), mask, rng_);
}

template <typename Rng>
CastExpr::Kind DefaultGeneratorRng<Rng>::gen_cast_kind(
    const CastKindMask& mask) {
  return pick_nth_set_bit(mask, rng_);
}

template <typename Rng>
ScalarType DefaultGeneratorRng<Rng>::gen_scalar_type(ScalarMask mask) {
  return pick_nth_set_bit(mask, rng_);
}

template class DefaultGeneratorRng<Mt19937>;
template class DefaultGeneratorRng<FixedRng>;

}  // namespace fuzzer
