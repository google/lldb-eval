#include "tools/fuzzer/constraints.h"

#include <variant>

#include "tools/fuzzer/ast.h"

namespace fuzzer {

SpecificTypes::SpecificTypes(const Type& type) {
  const auto* scalar_type = std::get_if<ScalarType>(&type);
  if (scalar_type != nullptr) {
    if (*scalar_type != ScalarType::Void) {
      scalar_types_[*scalar_type] = true;
    }
    return;
  }

  const auto* tagged_type = std::get_if<TaggedType>(&type);
  if (tagged_type != nullptr) {
    tagged_types_.insert(*tagged_type);
    return;
  }

  const auto* pointer_type = std::get_if<PointerType>(&type);
  if (pointer_type != nullptr) {
    const auto& inner = pointer_type->type().type();
    if (inner == Type(ScalarType::Void)) {
      allows_void_pointer_ = true;
    }

    ptr_types_ = std::make_shared<SpecificTypes>(inner);
    return;
  }
}

SpecificTypes SpecificTypes::make_pointer_constraints(
    SpecificTypes constraints, VoidPointerConstraint void_ptr_constraint) {
  SpecificTypes retval;
  if (constraints.scalar_types_[ScalarType::Void]) {
    constraints.scalar_types_[ScalarType::Void] = false;
    void_ptr_constraint = VoidPointerConstraint::Allow;
  }

  if (constraints.satisfiable()) {
    retval.ptr_types_ = std::make_shared<SpecificTypes>(std::move(constraints));
  }
  retval.allows_void_pointer_ = (bool)void_ptr_constraint;

  return retval;
}

TypeConstraints SpecificTypes::allowed_to_point_to() const {
  if (std::holds_alternative<NoType>(ptr_types_)) {
    return NoType();
  }

  if (std::holds_alternative<AnyType>(ptr_types_)) {
    return AnyType();
  }

  const auto* specific_types_ptr =
      std::get_if<std::shared_ptr<SpecificTypes>>(&ptr_types_);
  assert(specific_types_ptr != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  assert(
      *specific_types_ptr != nullptr &&
      "Should never be null, did you accidentally create a null shared_ptr?");

  return **specific_types_ptr;
}

TypeConstraints TypeConstraints::allowed_to_point_to() const {
  if (!satisfiable()) {
    return NoType();
  }

  if (allows_any()) {
    return AnyType();
  }

  const auto* specific_types = as_specific_types();
  assert(specific_types != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  return specific_types->allowed_to_point_to();
}

TypeConstraints TypeConstraints::make_pointer_constraints() const {
  if (!satisfiable()) {
    return NoType();
  }

  if (allows_any()) {
    return SpecificTypes::make_any_non_void_pointer_constraints();
  }

  const auto* specific_types = as_specific_types();
  assert(specific_types != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  return SpecificTypes::make_pointer_constraints(*specific_types);
}

bool TypeConstraints::allows_type(const Type& type) const {
  const auto* as_scalar = std::get_if<ScalarType>(&type);
  if (as_scalar != nullptr) {
    auto scalar_types = allowed_scalar_types();
    return scalar_types[*as_scalar];
  }

  const auto* as_tagged = std::get_if<TaggedType>(&type);
  if (as_tagged != nullptr) {
    if (!allows_tagged_types()) {
      return false;
    }

    const auto* tagged_types = allowed_tagged_types();
    if (tagged_types == nullptr) {
      return true;
    }

    return tagged_types->find(*as_tagged) != tagged_types->end();
  }

  const auto* as_ptr = std::get_if<PointerType>(&type);
  if (as_ptr != nullptr) {
    const auto& inner = as_ptr->type().type();
    const auto* as_scalar = std::get_if<ScalarType>(&inner);
    if (as_scalar != nullptr && *as_scalar == ScalarType::Void) {
      return allows_void_pointer();
    }

    const auto can_point_to = allowed_to_point_to();
    return can_point_to.allows_type(inner);
  }

  return false;
}

}  // namespace fuzzer
