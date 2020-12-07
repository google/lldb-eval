#include "tools/fuzzer/constraints.h"

#include "gmock/gmock.h"
#include "gtest/gtest.h"
#include "tools/fuzzer/ast.h"

using namespace fuzzer;
using namespace testing;

TEST(Constraints, ScalarValues) {
  SpecificTypes float_only = FLOAT_TYPES;
  SpecificTypes int_only = INT_TYPES;

  TypeConstraints float_constraints = float_only;
  TypeConstraints int_constraints = int_only;

  TypeConstraints any = AnyType();
  TypeConstraints none;

  PointerType void_ptr_type{QualifiedType(ScalarType::Void)};

  EXPECT_THAT(float_only.allows_any_of(ScalarType::Float), IsTrue());
  EXPECT_THAT(float_only.allows_any_of(ScalarType::Double), IsTrue());
  EXPECT_THAT(float_only.allows_any_of(ScalarType::LongDouble), IsTrue());

  EXPECT_THAT(float_only.allows_any_of(ScalarType::Void), IsFalse());
  EXPECT_THAT(float_only.allows_any_of(ScalarType::Bool), IsFalse());
  EXPECT_THAT(float_only.allows_any_of(ScalarType::SignedInt), IsFalse());
  EXPECT_THAT(float_only.allows_any_of(ScalarType::UnsignedLong), IsFalse());

  EXPECT_THAT(int_only.allowed_tagged_types(), IsEmpty());
  EXPECT_THAT(float_only.allowed_tagged_types(), IsEmpty());

  EXPECT_THAT(int_only.allows_non_void_pointer(), IsFalse());
  EXPECT_THAT(float_only.allows_non_void_pointer(), IsFalse());

  EXPECT_THAT(int_only.allows_void_pointer(), IsFalse());
  EXPECT_THAT(float_only.allows_void_pointer(), IsFalse());

  EXPECT_THAT(int_only.allows_any_of(ScalarType::Float), IsFalse());
  EXPECT_THAT(int_only.allows_any_of(ScalarType::Double), IsFalse());
  EXPECT_THAT(int_only.allows_any_of(ScalarType::LongDouble), IsFalse());

  EXPECT_THAT(int_only.allows_any_of(ScalarType::Void), IsFalse());
  EXPECT_THAT(int_only.allows_any_of(ScalarType::Bool), IsTrue());
  EXPECT_THAT(int_only.allows_any_of(ScalarType::SignedInt), IsTrue());
  EXPECT_THAT(int_only.allows_any_of(ScalarType::UnsignedLong), IsTrue());

  EXPECT_THAT(float_constraints.allows_type(ScalarType::Float), IsTrue());
  EXPECT_THAT(float_constraints.allows_type(ScalarType::SignedInt), IsFalse());
  EXPECT_THAT(float_constraints.allows_type(TaggedType("Test")), IsFalse());
  EXPECT_THAT(float_constraints.allows_type(void_ptr_type), IsFalse());

  EXPECT_THAT(int_constraints.allows_type(ScalarType::Float), IsFalse());
  EXPECT_THAT(int_constraints.allows_type(ScalarType::SignedInt), IsTrue());
  EXPECT_THAT(int_constraints.allows_type(TaggedType("Test")), IsFalse());
  EXPECT_THAT(int_constraints.allows_type(void_ptr_type), IsFalse());

  EXPECT_THAT(any.allows_type(ScalarType::Float), IsTrue());
  EXPECT_THAT(any.allows_type(ScalarType::SignedInt), IsTrue());

  EXPECT_THAT(none.allows_type(ScalarType::Float), IsFalse());
  EXPECT_THAT(none.allows_type(ScalarType::SignedInt), IsFalse());
}

TEST(Constraints, TaggedTypes) {
  SpecificTypes test_struct =
      std::unordered_set<TaggedType>{{TaggedType("TestStruct")}};

  EXPECT_THAT(test_struct.allows_any_of(ScalarMask::all_set()), IsFalse());
  EXPECT_THAT(test_struct.allowed_tagged_types(),
              UnorderedElementsAre(TaggedType("TestStruct")));
  EXPECT_THAT(test_struct.allows_non_void_pointer(), IsFalse());
  EXPECT_THAT(test_struct.allows_void_pointer(), IsFalse());

  TypeConstraints any = AnyType();
  TypeConstraints none;

  EXPECT_THAT(any.allows_tagged_types(), IsTrue());
  EXPECT_THAT(any.allowed_tagged_types(), IsNull());
  EXPECT_THAT(any.allows_type(TaggedType("TestStruct")), IsTrue());

  EXPECT_THAT(none.allows_tagged_types(), IsFalse());
  EXPECT_THAT(none.allowed_tagged_types(), IsNull());
  EXPECT_THAT(none.allows_type(TaggedType("TestStruct")), IsFalse());
}

TEST(Constraints, PointerTypes) {
  SpecificTypes int_ptr = SpecificTypes::make_pointer_constraints(
      SpecificTypes(ScalarMask{ScalarType::SignedInt}));
  SpecificTypes void_ptr = SpecificTypes::make_pointer_constraints(
      SpecificTypes(), VoidPointerConstraint::Allow);

  EXPECT_THAT(int_ptr.allows_any_of(ScalarMask::all_set()), IsFalse());
  EXPECT_THAT(int_ptr.allowed_tagged_types(), IsEmpty());
  EXPECT_THAT(int_ptr.allows_non_void_pointer(), IsTrue());
  EXPECT_THAT(int_ptr.allows_void_pointer(), IsFalse());

  EXPECT_THAT(void_ptr.allows_any_of(ScalarMask::all_set()), IsFalse());
  EXPECT_THAT(void_ptr.allowed_tagged_types(), IsEmpty());
  EXPECT_THAT(void_ptr.allows_non_void_pointer(), IsFalse());
  EXPECT_THAT(void_ptr.allows_void_pointer(), IsTrue());

  PointerType const_int_ptr{
      QualifiedType(ScalarType::SignedInt, CvQualifier::Const)};
  PointerType volatile_void_ptr{
      QualifiedType(ScalarType::Void, CvQualifier::Volatile)};

  TypeConstraints int_ptr_constraints = int_ptr;
  TypeConstraints void_ptr_constraints = void_ptr;

  TypeConstraints int_constraints = int_ptr.allowed_to_point_to();
  TypeConstraints void_constraints = void_ptr.allowed_to_point_to();

  EXPECT_THAT(int_ptr_constraints.allows_type(const_int_ptr), IsTrue());
  EXPECT_THAT(int_ptr_constraints.allows_type(volatile_void_ptr), IsFalse());
  EXPECT_THAT(int_constraints.allows_any_of(ScalarType::SignedInt), IsTrue());
  EXPECT_THAT(int_constraints.allows_any_of(ScalarType::Void), IsFalse());

  EXPECT_THAT(void_ptr_constraints.allows_type(const_int_ptr), IsFalse());
  EXPECT_THAT(void_ptr_constraints.allows_type(volatile_void_ptr), IsTrue());
  EXPECT_THAT(void_constraints.allows_any_of(ScalarType::SignedInt), IsFalse());

  // Due to the way we represent constraints, we cannot state that we support
  // void types :(
  EXPECT_THAT(void_constraints.allows_any_of(ScalarType::Void), IsFalse());

  TypeConstraints any = AnyType();
  TypeConstraints none;

  EXPECT_THAT(any.allows_type(const_int_ptr), IsTrue());
  EXPECT_THAT(any.allows_type(volatile_void_ptr), IsTrue());

  EXPECT_THAT(none.allows_type(const_int_ptr), IsFalse());
  EXPECT_THAT(none.allows_type(volatile_void_ptr), IsFalse());
}

TEST(Constraints, Unsatisfiability) {
  TypeConstraints default_ctor;
  TypeConstraints default_specific_types = SpecificTypes();

  EXPECT_THAT(default_ctor.satisfiable(), IsFalse());
  EXPECT_THAT(default_specific_types.satisfiable(), IsFalse());
}
