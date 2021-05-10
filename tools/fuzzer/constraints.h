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

enum class ExprCategory : bool { LvalueOrRvalue, Lvalue };

// The type constraints an expression can have. This class represents the fact
// that an expression can be:
//
// - Of any type (AnyType)
// - Of no type at all (NoType) aka unsatisfiable
// - Of a specific type/specific set of types
class TypeConstraints {
 public:
  TypeConstraints() = default;
  TypeConstraints(ScalarMask scalar_types) : scalar_types_(scalar_types) {}
  TypeConstraints(TaggedType tagged_types)
      : tagged_types_(std::move(tagged_types)) {}

  // Constraints that only allow type `type`.
  // The only exception is if the `type` is a pointer type `T*` and
  // `allow_arrays_if_ptr` is set. In that case the array type `T[]` will also
  // be allowed. Note, however, that this apply only on the surface level and
  // won't be propagated further, i.e. if the `type` is of form `T**`, then
  // type `T*[]` will be allowed, but type `T[][]` will not be allowed.
  explicit TypeConstraints(const Type& type, bool allow_arrays_if_ptr = true);

  // Constraints corresponding to all types that can be used in a boolean
  // context, i.e. ternary expression condition, logical operators (`&&`, `||`,
  // `!`), etc. These types are:
  // - Integers
  // - Floats
  // - Void/non-void pointers or the null pointer constant `0`
  // - Unscoped enums
  // - Array types
  static TypeConstraints all_in_bool_ctx() {
    TypeConstraints retval;
    retval.scalar_types_ = ~ScalarMask(ScalarType::Void);
    retval.ptr_types_ = AnyType{};
    retval.array_types_ = AnyType{};
    retval.unscoped_enum_types_ = AnyType{};
    retval.scoped_enum_types_ = NoType{};
    retval.allows_void_pointer_ = true;
    retval.allows_nullptr_ = true;
    retval.allows_literal_zero_ = true;

    return retval;
  }

  // Return a set of constraints that allow any pointer type, including void
  // pointers, null pointers and array types.
  static TypeConstraints all_in_pointer_ctx() {
    TypeConstraints retval;
    retval.ptr_types_ = AnyType{};
    retval.array_types_ = AnyType{};
    retval.allows_void_pointer_ = true;
    retval.allows_nullptr_ = true;
    retval.allows_literal_zero_ = true;

    return retval;
  }

  static TypeConstraints all() {
    TypeConstraints retval;
    retval.scalar_types_ = ScalarMask::all_set();
    retval.tagged_types_ = AnyType{};
    retval.ptr_types_ = AnyType{};
    retval.array_types_ = AnyType{};
    retval.unscoped_enum_types_ = AnyType{};
    retval.scoped_enum_types_ = AnyType{};
    retval.allows_void_pointer_ = true;
    retval.allows_nullptr_ = true;
    retval.allows_literal_zero_ = true;

    return retval;
  }

  // Return a set of constraints that allow any non-void pointer type,
  // including array types.
  static TypeConstraints all_in_non_void_pointer_ctx() {
    TypeConstraints retval;
    retval.ptr_types_ = AnyType{};
    retval.array_types_ = AnyType{};

    return retval;
  }

  // Types that can be cast to the `type`, using a C-style cast.
  static TypeConstraints cast_to(const Type& type);

  // Types that can be cast to the `type`, using a `static_cast`.
  static TypeConstraints static_cast_to(const Type& type);

  // Types that can be cast to the `type`, using a `reinterpret_cast`.
  static TypeConstraints reinterpret_cast_to(const Type& type);

  // Types that can be implicitly cast to the `type`, e.g. when passing
  // arguments to function calls.
  static TypeConstraints implicit_cast_to(const Type& type);

  // Is there any type that satisfies these constraints?
  bool satisfiable() const {
    return scalar_types_.any() ||
           !std::holds_alternative<NoType>(unscoped_enum_types_) ||
           !std::holds_alternative<NoType>(scoped_enum_types_) ||
           !std::holds_alternative<NoType>(tagged_types_) ||
           !std::holds_alternative<NoType>(ptr_types_) ||
           !std::holds_alternative<NoType>(array_types_) ||
           allows_void_pointer_ || allows_nullptr_ || allows_literal_zero_;
  }

  // Scalar types allowed by these constraints.
  ScalarMask allowed_scalar_types() const { return scalar_types_; }

  // Tagged types allowed by these constraints.
  const std::variant<NoType, AnyType, TaggedType>& allowed_tagged_types()
      const {
    return tagged_types_;
  }

  // Scoped enum types allowed by these constraints.
  const std::variant<NoType, AnyType, EnumType>& allowed_scoped_enum_types()
      const {
    return scoped_enum_types_;
  }

  // Unscoped enum types allowed by these constraints.
  const std::variant<NoType, AnyType, EnumType>& allowed_unscoped_enum_types()
      const {
    return unscoped_enum_types_;
  }

  // Do these constraints allow any of the types in `mask`?
  bool allows_any_of(ScalarMask mask) const {
    return (scalar_types_ & mask).any();
  }

  // Do these constraints allow any kind of tagged types?
  bool allows_tagged_types() const {
    return !std::holds_alternative<NoType>(allowed_tagged_types());
  }

  // Do these constraints allow any kind of non-void pointer?
  bool allows_non_void_pointer() const {
    return !std::holds_alternative<NoType>(ptr_types_);
  }

  // Do these constraints allow any kind of array types?
  bool allows_array_types() const {
    return !std::holds_alternative<NoType>(array_types_);
  }

  // Do these constraints allow void pointers?
  bool allows_void_pointer() const { return allows_void_pointer_; }

  // Do these constraints allow `nullptr`?
  bool allows_nullptr() const { return allows_nullptr_; }

  // Do these constraints allow literal `0` in pointer context?
  // Typically literal `0` is allowed whenever void pointers and nullptr are
  // allowed, unless it is explicitly disallowed.
  bool allows_literal_zero() const { return allows_literal_zero_; }

  // Do these constraints allow a tagged type?
  bool allows_tagged_type(const TaggedType& tagged_type) const;

  // Do these constraints allow an enum type?
  bool allows_enum_type(const EnumType& enum_type) const;

  // Do these constraints allow an array type?
  bool allows_array_type(const ArrayType& array_type) const;

  // Do these constraints allow the `type`?
  bool allows_type(const Type& type) const;

  // Do these constraints allow the qualified `type`?
  bool allows_type(const QualifiedType& type) const {
    return allows_type(type.type());
  }

  // Allows specific set of scalar types.
  void allow_scalar_types(ScalarMask scalar_types) {
    scalar_types_ = scalar_types;
  }

  // Allows unscoped enum types.
  void allow_unscoped_enums() { unscoped_enum_types_ = AnyType{}; }

  // Allows scoped enum types.
  void allow_scoped_enums() { scoped_enum_types_ = AnyType{}; }

  // Allows `void *` type and literal `0`.
  void allow_void_pointer() {
    allows_void_pointer_ = true;
    allows_literal_zero_ = true;
  }

  // Disallows `nullptr`s.
  void disallow_nullptr() {
    allows_nullptr_ = false;
    if (!allows_void_pointer_) {
      allows_literal_zero_ = false;
    }
  }

  // Disallows `0` in pointer context.
  void disallow_literal_zero() { allows_literal_zero_ = false; }

  // Make a new set of pointer constraints. If the original constraints permit
  // type T, the new constraints will allow types `T*`, `const T*`, `volatile
  // T*`, `const volatile T*` and the array type `T[]`.
  TypeConstraints make_pointer_constraints() const;

  // What kind of types do these constraints allow a pointer to?
  TypeConstraints allowed_to_point_to() const;

 private:
  ScalarMask scalar_types_;
  std::variant<NoType, AnyType, EnumType> unscoped_enum_types_;
  std::variant<NoType, AnyType, EnumType> scoped_enum_types_;
  std::variant<NoType, AnyType, TaggedType> tagged_types_;
  std::variant<NoType, AnyType, std::shared_ptr<TypeConstraints>> ptr_types_;
  std::variant<NoType, AnyType, std::shared_ptr<TypeConstraints>> array_types_;
  bool allows_void_pointer_ = false;
  bool allows_nullptr_ = false;
  std::optional<size_t> array_size_ = std::nullopt;
  bool allows_literal_zero_ = false;
};

// Constraints that regulate memory access as an expression is being
// constructed.
class MemoryConstraints {
 public:
  MemoryConstraints(bool must_be_valid, int freedom)
      : must_be_valid_(must_be_valid),
        // Set required freedom index to 0 if invalid memory is allowed.
        required_freedom_index_(must_be_valid ? freedom : 0) {
    assert(required_freedom_index_ >= 0 &&
           "Required freedom index shouldn't be negative!");
  }

  // Constructs memory constraints from the required freedom index.
  // It's assumed that memory in this case should be valid.
  explicit MemoryConstraints(int freedom)
      : must_be_valid_(true), required_freedom_index_(freedom) {
    assert(required_freedom_index_ >= 0 &&
           "Required freedom index shouldn't be negative!");
  }

  // Empty constructor. Allows invalid memory.
  MemoryConstraints() = default;

  // Indicates whether the expression that is being constructed is going to
  // read from memory. If it's going to, it has to be valid to avoid illegal
  // memory access.
  bool must_be_valid() const { return must_be_valid_; }

  // Indicates a number of times the expression that is being constructed is
  // going to be dereferenced.
  int required_freedom_index() const { return required_freedom_index_; }

  // Creates new memory constraints assuming that current expression is an
  // address-of. It decreases required freedom index by 1.
  MemoryConstraints from_address_of() const {
    return MemoryConstraints(must_be_valid_, required_freedom_index_ - 1);
  }

  // Creates new memory constraints assuming that current expression is a
  // dereference-of. In most cases it means that from this point, memory
  // should be valid. The exception is if the parent was an address-of. In
  // that case, inherit validity from the current constraints, to allow
  // elimination of `&*`. It increases the required freedom index by 1.
  MemoryConstraints from_dereference_of(bool is_parent_address_of) const {
    return is_parent_address_of
               ? MemoryConstraints(must_be_valid_, required_freedom_index_ + 1)
               : MemoryConstraints(required_freedom_index_ + 1);
  }

  // Creates new memory constraints assuming that current expression is a
  // member-of. In the case that the parent expression is an address-of, it
  // inherits validity from the current constraints, to allow expression such
  // as `&(invalid_ptr)->field`. Required freedom index should be 1 for access
  // on pointers `->` and 0 for the dot `.` access.
  MemoryConstraints from_member_of(bool is_parent_address_of,
                                   int freedom) const {
    return is_parent_address_of ? MemoryConstraints(must_be_valid_, freedom)
                                : MemoryConstraints(freedom);
  }

 private:
  bool must_be_valid_ = false;
  int required_freedom_index_ = 0;
};

// The main class that deals with expression constraints.
class ExprConstraints {
 public:
  ExprConstraints() = default;

  // Allow implicit conversion from `TypeConstraints` for convenience (plus,
  // in most cases expressions don't have to be lvalues.
  ExprConstraints(TypeConstraints type_constraints,
                  MemoryConstraints memory_constraints = MemoryConstraints(),
                  ExprCategory category = ExprCategory::LvalueOrRvalue)
      : type_constraints_(std::move(type_constraints)),
        memory_constraints_(std::move(memory_constraints)),
        must_be_lvalue_((bool)category) {}

  ExprConstraints(ScalarMask mask,
                  MemoryConstraints memory_constraints = MemoryConstraints(),
                  ExprCategory category = ExprCategory::LvalueOrRvalue)
      : type_constraints_(TypeConstraints(mask)),
        memory_constraints_(std::move(memory_constraints)),
        must_be_lvalue_((bool)category) {}

  // Must the expression we generate be an lvalue?
  bool must_be_lvalue() const { return must_be_lvalue_; }

  // Type constraints of the expression to generate
  const TypeConstraints& type_constraints() const { return type_constraints_; }

  // Memory constraints of the expression to generate
  const MemoryConstraints& memory_constraints() const {
    return memory_constraints_;
  }

 private:
  TypeConstraints type_constraints_;
  MemoryConstraints memory_constraints_;
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
