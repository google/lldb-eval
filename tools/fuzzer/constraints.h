#ifndef INCLUDE_CONSTRAINTS_H
#define INCLUDE_CONSTRAINTS_H

#include <cassert>
#include <memory>
#include <unordered_set>
#include <utility>
#include <variant>

#include "tools/fuzzer/ast.h"
#include "tools/fuzzer/enum_bitset.h"
#include "tools/fuzzer/expr_gen.h"

namespace fuzzer {

using ScalarMask = EnumBitset<ScalarType>;

class NoType {};
class AnyType {};

class TypeConstraints;

enum class VoidPointerConstraint : bool { Deny, Allow };
enum class ExprCategory : bool { LvalueOrRvalue, Lvalue };

class SpecificTypes {
 public:
  SpecificTypes() = default;
  SpecificTypes(ScalarMask scalar_types) : scalar_types_(scalar_types) {}
  SpecificTypes(std::unordered_set<TaggedType> tagged_types)
      : tagged_types_(std::move(tagged_types)) {}

  explicit SpecificTypes(const Type& type);

  static SpecificTypes all_in_bool_ctx() {
    SpecificTypes retval;
    retval.scalar_types_ = ~ScalarMask(ScalarType::Void);
    retval.ptr_types_ = AnyType{};
    retval.allows_void_pointer_ = true;

    return retval;
  }

  static SpecificTypes make_any_pointer_constraints() {
    SpecificTypes retval;
    retval.ptr_types_ = AnyType{};
    retval.allows_void_pointer_ = true;

    return retval;
  }

  static SpecificTypes make_any_non_void_pointer_constraints() {
    SpecificTypes retval;
    retval.ptr_types_ = AnyType{};

    return retval;
  }

  static SpecificTypes make_pointer_constraints(
      SpecificTypes constraints,
      VoidPointerConstraint void_ptr_constraint = VoidPointerConstraint::Deny);

  bool satisfiable() const {
    return scalar_types_.any() || !tagged_types_.empty() ||
           !std::holds_alternative<NoType>(ptr_types_) || allows_void_pointer_;
  }

  ScalarMask allowed_scalar_types() const { return scalar_types_; }

  const std::unordered_set<TaggedType>& allowed_tagged_types() const {
    return tagged_types_;
  }

  bool allows_any_of(ScalarMask mask) const {
    return (scalar_types_ & mask).any();
  }

  bool allows_non_void_pointer() const {
    return !std::holds_alternative<NoType>(ptr_types_);
  }

  bool allows_void_pointer() const { return allows_void_pointer_; }

  TypeConstraints allowed_to_point_to() const;

 private:
  ScalarMask scalar_types_;
  std::unordered_set<TaggedType> tagged_types_;
  std::variant<NoType, AnyType, std::shared_ptr<SpecificTypes>> ptr_types_;
  bool allows_void_pointer_ = false;
};

class TypeConstraints {
 public:
  TypeConstraints() = default;
  TypeConstraints(NoType) {}
  TypeConstraints(AnyType) : constraints_(AnyType()) {}
  TypeConstraints(SpecificTypes constraints) {
    if (constraints.satisfiable()) {
      constraints_ = std::move(constraints);
    }
  }

  static TypeConstraints all_in_bool_ctx() {
    return SpecificTypes::all_in_bool_ctx();
  }

  bool satisfiable() const {
    return !std::holds_alternative<NoType>(constraints_);
  }

  bool allows_any() const {
    return std::holds_alternative<AnyType>(constraints_);
  }

  const SpecificTypes* as_specific_types() const {
    return std::get_if<SpecificTypes>(&constraints_);
  }

  bool allows_any_of(ScalarMask mask) const {
    if (!satisfiable()) {
      return false;
    }

    if (allows_any()) {
      return true;
    }

    const auto* specific_types = as_specific_types();
    assert(specific_types != nullptr &&
           "Should never be null, did you introduce a new alternative?");

    return specific_types->allows_any_of(mask);
  }

  bool allows_tagged_types() const {
    if (!satisfiable()) {
      return false;
    }

    if (allows_any()) {
      return true;
    }

    const auto* specific_types = as_specific_types();
    assert(specific_types != nullptr && "Did you introduce a new alternative?");

    return !specific_types->allowed_tagged_types().empty();
  }

  ScalarMask allowed_scalar_types() const {
    if (!satisfiable()) {
      return ScalarMask();
    }

    if (allows_any()) {
      return ScalarMask::all_set();
    }

    const auto* specific_types = as_specific_types();
    assert(specific_types != nullptr && "Did you introduce a new alternative?");

    return specific_types->allowed_scalar_types();
  }

  const std::unordered_set<TaggedType>* allowed_tagged_types() const {
    const auto* specific_types = as_specific_types();
    if (specific_types == nullptr) {
      return nullptr;
    }
    return &specific_types->allowed_tagged_types();
  }

  TypeConstraints allowed_to_point_to() const;

  bool allows_void_pointer() const {
    if (!satisfiable()) {
      return false;
    }

    if (allows_any()) {
      return true;
    }

    const auto* specific_types = as_specific_types();
    assert(specific_types != nullptr && "Did you introduce a new alternative?");

    return specific_types->allows_void_pointer();
  }

  bool allows_pointer() const {
    if (!satisfiable()) {
      return false;
    }

    if (allows_any()) {
      return true;
    }

    const auto* specific_types = as_specific_types();
    assert(specific_types != nullptr && "Did you introduce a new alternative?");

    return specific_types->allows_void_pointer();
  }

  bool allows_type(const Type& type) const;

  bool allows_type(const QualifiedType& type) const {
    return allows_type(type.type());
  }

 private:
  std::variant<NoType, AnyType, SpecificTypes> constraints_;
};

class ExprConstraints {
 public:
  ExprConstraints() = default;

  // Allow implicit conversion from `TypeConstraints` for convenience (plus,
  // in most cases expressions don't have to be lvalues.
  ExprConstraints(TypeConstraints type_constraints,
                  ExprCategory category = ExprCategory::LvalueOrRvalue)
      : type_constraints_(std::move(type_constraints)),
        must_be_lvalue_((bool)category) {}

  ExprConstraints(ScalarMask mask,
                  ExprCategory category = ExprCategory::LvalueOrRvalue)
      : type_constraints_(TypeConstraints(mask)),
        must_be_lvalue_((bool)category) {}

  bool must_be_lvalue() const { return must_be_lvalue_; }
  const TypeConstraints& type_constraints() const { return type_constraints_; }

 private:
  TypeConstraints type_constraints_;
  bool must_be_lvalue_ = false;
};

inline constexpr ScalarMask INT_TYPES = {
    ScalarType::Bool,           ScalarType::Char,
    ScalarType::UnsignedChar,   ScalarType::SignedChar,
    ScalarType::SignedShort,    ScalarType::UnsignedShort,
    ScalarType::SignedInt,      ScalarType::UnsignedInt,
    ScalarType::SignedLong,     ScalarType::UnsignedLong,
    ScalarType::SignedLongLong, ScalarType::UnsignedLongLong,
};

inline constexpr ScalarMask FLOAT_TYPES = {
    ScalarType::Float,
    ScalarType::Double,
    ScalarType::LongDouble,
};

}  // namespace fuzzer

#endif  // INCLUDE_CONSTRAINTS_H
