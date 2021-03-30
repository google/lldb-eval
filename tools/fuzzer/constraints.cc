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

#include "tools/fuzzer/constraints.h"

#include <variant>

#include "tools/fuzzer/ast.h"

namespace fuzzer {

static bool is_void_pointer(const Type& type) {
  const auto* pointer_type = std::get_if<PointerType>(&type);
  if (pointer_type == nullptr) {
    return false;
  }
  const auto& inner = pointer_type->type().type();
  return inner == Type(ScalarType::Void);
}

SpecificTypes::SpecificTypes(const Type& type, bool allow_arrays_if_ptr) {
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
      allows_literal_zero_ = true;
    }

    ptr_types_ =
        std::make_shared<SpecificTypes>(inner, /*allow_arrays_if_ptr*/ false);

    if (allow_arrays_if_ptr) {
      array_types_ = ptr_types_;
    }
    return;
  }

  const auto* array_type = std::get_if<ArrayType>(&type);
  if (array_type != nullptr) {
    const auto& inner = array_type->type();
    array_types_ =
        std::make_shared<SpecificTypes>(inner, /*allow_arrays_if_ptr*/ false);
    array_size_ = array_type->size();
    return;
  }

  const auto* enum_type = std::get_if<EnumType>(&type);
  if (enum_type != nullptr) {
    if (enum_type->is_scoped()) {
      scoped_enum_types_ = *enum_type;
    } else {
      unscoped_enum_types_ = *enum_type;
    }
    return;
  }

  if (std::holds_alternative<NullptrType>(type)) {
    allows_nullptr_ = true;
    allows_literal_zero_ = true;
    return;
  }

  assert(false && "Did you introduce a new alternative?");
}

SpecificTypes SpecificTypes::make_pointer_constraints(
    SpecificTypes constraints, VoidPointerConstraint void_ptr_constraint) {
  SpecificTypes retval;
  if (constraints.scalar_types_[ScalarType::Void]) {
    constraints.scalar_types_[ScalarType::Void] = false;
    void_ptr_constraint = VoidPointerConstraint::Allow;
  }

  if (constraints.satisfiable()) {
    auto specific_types =
        std::make_shared<SpecificTypes>(std::move(constraints));
    retval.ptr_types_ = specific_types;
    retval.array_types_ = specific_types;
  }
  retval.allows_void_pointer_ = (bool)void_ptr_constraint;
  retval.allows_literal_zero_ = (bool)void_ptr_constraint;

  // We should never allow `nullptr` because dereferencing it is illegal.
  retval.allows_nullptr_ = false;

  return retval;
}

SpecificTypes SpecificTypes::cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<ArrayType>(type)) {
    return SpecificTypes();
  }

  if (std::holds_alternative<PointerType>(type)) {
    return SpecificTypes::all_in_pointer_ctx();
  }

  if (std::holds_alternative<NullptrType>(type)) {
    return SpecificTypes(type);
  }

  if (std::holds_alternative<EnumType>(type)) {
    // So far, casting other types to both (scoped and unscoped) enum types
    // didn't cause any problems. If it does, change this.
    SpecificTypes retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    retval.allow_scoped_enums();
    return retval;
  }

  const auto* scalar_type = std::get_if<ScalarType>(&type);
  if (scalar_type != nullptr) {
    SpecificTypes retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    retval.allow_scoped_enums();
    // Pointers can be casted to 8-bytes integer types.
    if (*scalar_type == ScalarType::SignedLongLong ||
        *scalar_type == ScalarType::UnsignedLongLong) {
      retval.ptr_types_ = AnyType{};
      retval.allows_void_pointer_ = true;
      retval.allows_nullptr_ = true;
    }
    return retval;
  }

  assert(false && "Did you introduce a new alternative?");
  return SpecificTypes();
}

SpecificTypes SpecificTypes::static_cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<ArrayType>(type)) {
    return SpecificTypes();
  }

  if (std::holds_alternative<PointerType>(type)) {
    if (is_void_pointer(type)) {
      return SpecificTypes::all_in_pointer_ctx();
    }
    SpecificTypes retval = SpecificTypes(type);
    retval.allows_nullptr_ = true;
    retval.allows_void_pointer_ = true;
    retval.allows_literal_zero_ = true;
    return retval;
  }

  if (std::holds_alternative<NullptrType>(type)) {
    return SpecificTypes(type);
  }

  if (std::holds_alternative<EnumType>(type) ||
      std::holds_alternative<ScalarType>(type)) {
    SpecificTypes retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    retval.allow_scoped_enums();
    return retval;
  }

  assert(false && "Did you introduce a new alternative?");
  return SpecificTypes();
}

SpecificTypes SpecificTypes::reinterpret_cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<NullptrType>(type) ||
      std::holds_alternative<ScalarType>(type) ||
      std::holds_alternative<ArrayType>(type)) {
    return SpecificTypes();
  }

  if (std::holds_alternative<PointerType>(type)) {
    SpecificTypes retval = SpecificTypes::all_in_pointer_ctx();
    retval.allows_nullptr_ = false;
    retval.allows_literal_zero_ = true;
    return retval;
  }

  if (std::holds_alternative<EnumType>(type)) {
    return SpecificTypes(type);
  }

  assert(false && "Did you introduce a new alternative?");
  return SpecificTypes();
}

SpecificTypes SpecificTypes::implicit_cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<PointerType>(type) ||
      std::holds_alternative<NullptrType>(type) ||
      std::holds_alternative<EnumType>(type)) {
    return SpecificTypes(type);
  }

  const auto* scalar_type = std::get_if<ScalarType>(&type);
  if (scalar_type != nullptr) {
    if (*scalar_type == ScalarType::Void) {
      return SpecificTypes(type);
    }

    SpecificTypes retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    return retval;
  }

  assert(false && "Did you introduce a new alternative?");
  return SpecificTypes();
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

bool SpecificTypes::allows_array_type(const ArrayType& array_type) const {
  if (array_size_.has_value() && array_size_.value() != array_type.size()) {
    return false;
  }

  if (std::holds_alternative<NoType>(array_types_)) {
    return false;
  }

  if (std::holds_alternative<AnyType>(array_types_)) {
    return true;
  }

  const auto* array_element_types =
      std::get_if<std::shared_ptr<SpecificTypes>>(&array_types_);
  assert(array_element_types != nullptr &&
         "Should never be null, did you introduce a new alternative?");
  assert(
      *array_element_types != nullptr &&
      "Should never be null, did you accidentally create a null shared_ptr?");

  return TypeConstraints(**array_element_types).allows_type(array_type.type());
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
    return SpecificTypes::all_in_non_void_pointer_ctx();
  }

  const auto* specific_types = as_specific_types();
  assert(specific_types != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  return SpecificTypes::make_pointer_constraints(*specific_types);
}

static bool allows_enum_type(const SpecificTypes& types, const EnumType& type) {
  std::variant<NoType, AnyType, EnumType> enum_types;
  if (type.is_scoped()) {
    enum_types = types.allowed_scoped_enum_types();
  } else {
    enum_types = types.allowed_unscoped_enum_types();
  }

  if (std::holds_alternative<NoType>(enum_types)) {
    return false;
  }

  if (std::holds_alternative<AnyType>(enum_types)) {
    return true;
  }

  const auto* as_enum = std::get_if<EnumType>(&enum_types);
  assert(as_enum != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  return *as_enum == type;
}

bool TypeConstraints::allows_type(const Type& type) const {
  if (!satisfiable()) {
    return false;
  }

  if (allows_any()) {
    return true;
  }

  const auto* specific_types = as_specific_types();
  assert(specific_types != nullptr && "Did you introduce a new alternative?");

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

  const auto* as_enum = std::get_if<EnumType>(&type);
  if (as_enum != nullptr) {
    return allows_enum_type(*specific_types, *as_enum);
  }

  const auto* as_array = std::get_if<ArrayType>(&type);
  if (as_array != nullptr) {
    return specific_types->allows_array_type(*as_array);
  }

  if (std::holds_alternative<NullptrType>(type)) {
    return allows_nullptr();
  }

  return false;
}

}  // namespace fuzzer
