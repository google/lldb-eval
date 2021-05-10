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

TypeConstraints::TypeConstraints(const Type& type, bool allow_arrays_if_ptr) {
  const auto* scalar_type = std::get_if<ScalarType>(&type);
  if (scalar_type != nullptr) {
    if (*scalar_type != ScalarType::Void) {
      scalar_types_[*scalar_type] = true;
    }
    return;
  }

  const auto* tagged_type = std::get_if<TaggedType>(&type);
  if (tagged_type != nullptr) {
    tagged_types_ = *tagged_type;
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
        std::make_shared<TypeConstraints>(inner, /*allow_arrays_if_ptr*/ false);

    if (allow_arrays_if_ptr) {
      array_types_ = ptr_types_;
    }
    return;
  }

  const auto* array_type = std::get_if<ArrayType>(&type);
  if (array_type != nullptr) {
    const auto& inner = array_type->type();
    array_types_ =
        std::make_shared<TypeConstraints>(inner, /*allow_arrays_if_ptr*/ false);
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

TypeConstraints TypeConstraints::make_pointer_constraints() const {
  TypeConstraints retval;

  if (satisfiable()) {
    auto specific_types = std::make_shared<TypeConstraints>(*this);
    retval.ptr_types_ = specific_types;
    retval.array_types_ = specific_types;
  }

  return retval;
}

TypeConstraints TypeConstraints::cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<ArrayType>(type)) {
    return TypeConstraints();
  }

  if (std::holds_alternative<PointerType>(type)) {
    return TypeConstraints::all_in_pointer_ctx();
  }

  if (std::holds_alternative<NullptrType>(type)) {
    return TypeConstraints(type);
  }

  if (std::holds_alternative<EnumType>(type)) {
    // So far, casting other types to both (scoped and unscoped) enum types
    // didn't cause any problems. If it does, change this.
    TypeConstraints retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    retval.allow_scoped_enums();
    return retval;
  }

  const auto* scalar_type = std::get_if<ScalarType>(&type);
  if (scalar_type != nullptr) {
    TypeConstraints retval = INT_TYPES | FLOAT_TYPES;
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
  return TypeConstraints();
}

TypeConstraints TypeConstraints::static_cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<ArrayType>(type)) {
    return TypeConstraints();
  }

  if (std::holds_alternative<PointerType>(type)) {
    if (is_void_pointer(type)) {
      return TypeConstraints::all_in_pointer_ctx();
    }
    TypeConstraints retval = TypeConstraints(type);
    retval.allows_nullptr_ = true;
    retval.allows_void_pointer_ = true;
    retval.allows_literal_zero_ = true;
    return retval;
  }

  if (std::holds_alternative<NullptrType>(type)) {
    return TypeConstraints(type);
  }

  if (std::holds_alternative<EnumType>(type) ||
      std::holds_alternative<ScalarType>(type)) {
    TypeConstraints retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    retval.allow_scoped_enums();
    return retval;
  }

  assert(false && "Did you introduce a new alternative?");
  return TypeConstraints();
}

TypeConstraints TypeConstraints::reinterpret_cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<NullptrType>(type) ||
      std::holds_alternative<ScalarType>(type) ||
      std::holds_alternative<ArrayType>(type)) {
    return TypeConstraints();
  }

  if (std::holds_alternative<PointerType>(type)) {
    TypeConstraints retval = TypeConstraints::all_in_pointer_ctx();
    retval.allows_nullptr_ = false;
    retval.allows_literal_zero_ = true;
    return retval;
  }

  if (std::holds_alternative<EnumType>(type)) {
    return TypeConstraints(type);
  }

  assert(false && "Did you introduce a new alternative?");
  return TypeConstraints();
}

TypeConstraints TypeConstraints::implicit_cast_to(const Type& type) {
  if (std::holds_alternative<TaggedType>(type) ||
      std::holds_alternative<PointerType>(type) ||
      std::holds_alternative<NullptrType>(type) ||
      std::holds_alternative<EnumType>(type)) {
    return TypeConstraints(type);
  }

  const auto* scalar_type = std::get_if<ScalarType>(&type);
  if (scalar_type != nullptr) {
    if (*scalar_type == ScalarType::Void) {
      return TypeConstraints(type);
    }

    TypeConstraints retval = INT_TYPES | FLOAT_TYPES;
    retval.allow_unscoped_enums();
    return retval;
  }

  assert(false && "Did you introduce a new alternative?");
  return TypeConstraints();
}

TypeConstraints TypeConstraints::allowed_to_point_to() const {
  if (std::holds_alternative<NoType>(ptr_types_)) {
    return TypeConstraints();
  }

  if (std::holds_alternative<AnyType>(ptr_types_)) {
    return TypeConstraints::all();
  }

  const auto* specific_types_ptr =
      std::get_if<std::shared_ptr<TypeConstraints>>(&ptr_types_);
  assert(specific_types_ptr != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  assert(
      *specific_types_ptr != nullptr &&
      "Should never be null, did you accidentally create a null shared_ptr?");

  return **specific_types_ptr;
}

bool TypeConstraints::allows_tagged_type(const TaggedType& tagged_type) const {
  if (std::holds_alternative<NoType>(tagged_types_)) {
    return false;
  }

  if (std::holds_alternative<AnyType>(tagged_types_)) {
    return true;
  }

  const auto* as_tagged = std::get_if<TaggedType>(&tagged_types_);
  assert(as_tagged != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  return *as_tagged == tagged_type;
}

bool TypeConstraints::allows_array_type(const ArrayType& array_type) const {
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
      std::get_if<std::shared_ptr<TypeConstraints>>(&array_types_);
  assert(array_element_types != nullptr &&
         "Should never be null, did you introduce a new alternative?");
  assert(
      *array_element_types != nullptr &&
      "Should never be null, did you accidentally create a null shared_ptr?");

  return (*array_element_types)->allows_type(array_type.type());
}

bool TypeConstraints::allows_enum_type(const EnumType& enum_type) const {
  const auto& enum_types =
      enum_type.is_scoped() ? scoped_enum_types_ : unscoped_enum_types_;

  if (std::holds_alternative<NoType>(enum_types)) {
    return false;
  }

  if (std::holds_alternative<AnyType>(enum_types)) {
    return true;
  }

  const auto* as_enum = std::get_if<EnumType>(&enum_types);
  assert(as_enum != nullptr &&
         "Should never be null, did you introduce a new alternative?");

  return *as_enum == enum_type;
}

bool TypeConstraints::allows_type(const Type& type) const {
  const auto* as_scalar = std::get_if<ScalarType>(&type);
  if (as_scalar != nullptr) {
    return scalar_types_[*as_scalar];
  }

  const auto* as_tagged = std::get_if<TaggedType>(&type);
  if (as_tagged != nullptr) {
    return allows_tagged_type(*as_tagged);
  }

  const auto* as_ptr = std::get_if<PointerType>(&type);
  if (as_ptr != nullptr) {
    const auto& inner = as_ptr->type().type();
    const auto* as_scalar = std::get_if<ScalarType>(&inner);
    if (as_scalar != nullptr && *as_scalar == ScalarType::Void) {
      return allows_void_pointer_;
    }

    const auto can_point_to = allowed_to_point_to();
    return can_point_to.allows_type(inner);
  }

  const auto* as_enum = std::get_if<EnumType>(&type);
  if (as_enum != nullptr) {
    return allows_enum_type(*as_enum);
  }

  const auto* as_array = std::get_if<ArrayType>(&type);
  if (as_array != nullptr) {
    return allows_array_type(*as_array);
  }

  if (std::holds_alternative<NullptrType>(type)) {
    return allows_nullptr_;
  }

  return false;
}

}  // namespace fuzzer
