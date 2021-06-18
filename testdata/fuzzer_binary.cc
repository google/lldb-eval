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

#include <limits>

// This file _must not_ access the file system, since the current directory
// is specified in the fuzzer as just `./`.

// Returns the floor log base 2. This function is supported in lldb-eval and is
// defined here so it can be used via LLDB.
unsigned int __log2(unsigned int value) {
  unsigned int leading_zeros = 0;
  for (int bit = 31; bit >= 0; --bit) {
    if (value & (1U << bit)) {
      break;
    } else {
      leading_zeros++;
    }
  }
  return 31 - leading_zeros;
}

class MultiInheritBase1 {
 public:
  int f1 = 10;
};

class MultiInheritBase2 {
 public:
  int f2 = 20;
};

class MultiInheritDerived : public MultiInheritBase1, public MultiInheritBase2 {
 public:
  int f3 = 30;
};

class DeadlyDiamondBase {
 public:
  int f1 = 10;
};

class DeadlyDiamondDerived1 : public DeadlyDiamondBase {
 public:
  int f2 = 20;
};

class DeadlyDiamondDerived2 : public DeadlyDiamondBase {
 public:
  int f3 = 30;
};

class DeadlyDiamondSubclass : public DeadlyDiamondDerived1,
                              public DeadlyDiamondDerived2 {
 public:
  int f4 = 40;
};

class VirtualDiamondBase {
 public:
  int f1 = 10;
};

class VirtualDiamondDerived1 : public virtual VirtualDiamondBase {
 public:
  int f2 = 20;
};

class VirtualDiamondDerived2 : public virtual VirtualDiamondBase {
 public:
  int f3 = 30;
};

class VirtualDiamondSubclass : public VirtualDiamondDerived1,
                               public VirtualDiamondDerived2 {
 public:
  int f4 = 40;
};

class EmptyBase {};

class NonEmptyBase {
 public:
  int f2 = 10;
};

struct TestStruct {
  float flt_field = 0.5f;
  int int_field = 20;
  unsigned long long ull_field = -1ull;
  char ch_field = '/';
};

union TestUnion {
  unsigned int uint_field;
  unsigned char ch_field;
};

class NonEmptyDerived : public NonEmptyBase, public EmptyBase {
 public:
  EmptyBase base;
  int f1 = 10;
};

class StaticMember {
 public:
  static const int s1;
  static char s2;
  // TODO: Add static const members with inline assigned values and static
  // constexpr. In LLDB version 11, these cannot be accessed.
};
const int StaticMember::s1 = 10;
char StaticMember::s2 = 's';

class ClassWithNestedClass {
 public:
  class NestedClass {
   public:
    static const int s1;
    int f1 = 10;
  };

  NestedClass nested;
};
const int ClassWithNestedClass::NestedClass::s1 = 20;

enum CStyleEnum { VALUE1, VALUE2, VALUE3 };
enum class EnumClass { ZERO, ONE, TWO, THREE };

// Global variables
int global_int = 55;
int* global_ptr = &global_int;
int& global_ref = global_int;
TestStruct global_ts;

namespace ns {

class StaticMember {
 public:
  static const int s1;
};
const int StaticMember::s1 = 25;

enum CStyleEnum { V1, V2, V3 };
enum class EnumClass { ZERO, ONE, TWO, THREE };

// Global variables:
int global_int = 65;
int* global_ptr = &global_int;
int& global_ref = global_int;
TestStruct global_ts;

namespace nested_ns {

struct TestStruct {
  float flt_field = 3.14f;
  int int_field = 13;
  char ch_field = 'x';
};

// Global variables:
int global_int = 75;
TestStruct global_ts;

}  // namespace nested_ns
}  // namespace ns

int main() {
  auto char_min = std::numeric_limits<char>::min();
  auto char_max = std::numeric_limits<char>::max();
  (void)char_min, (void)char_max;

  auto uchar_min = std::numeric_limits<unsigned char>::min();
  auto uchar_max = std::numeric_limits<unsigned char>::max();
  (void)uchar_min, (void)uchar_max;

  auto schar_min = std::numeric_limits<signed char>::min();
  auto schar_max = std::numeric_limits<signed char>::max();
  (void)schar_min, (void)schar_max;

  auto short_min = std::numeric_limits<short>::min();
  auto short_max = std::numeric_limits<short>::max();
  (void)short_min, (void)short_max;

  auto ushort_min = std::numeric_limits<unsigned short>::min();
  auto ushort_max = std::numeric_limits<unsigned short>::max();
  (void)ushort_min, (void)ushort_max;

  auto int_min = std::numeric_limits<int>::min();
  auto int_max = std::numeric_limits<int>::max();
  (void)int_min, (void)int_max;

  auto uint_min = std::numeric_limits<unsigned int>::min();
  auto uint_max = std::numeric_limits<unsigned int>::max();
  (void)uint_min, (void)uint_max;

  auto long_min = std::numeric_limits<long>::min();
  auto long_max = std::numeric_limits<long>::max();
  (void)long_min, (void)long_max;

  auto ulong_min = std::numeric_limits<unsigned long>::min();
  auto ulong_max = std::numeric_limits<unsigned long>::max();
  (void)ulong_min, (void)ulong_max;

  auto llong_min = std::numeric_limits<long long>::min();
  auto llong_max = std::numeric_limits<long long>::max();
  (void)llong_min, (void)llong_max;

  auto ullong_min = std::numeric_limits<unsigned long long>::min();
  auto ullong_max = std::numeric_limits<unsigned long long>::max();
  (void)ullong_min, (void)ullong_max;

  auto finf = std::numeric_limits<float>::infinity();
  auto fnan = std::numeric_limits<float>::quiet_NaN();
  auto fsnan = std::numeric_limits<float>::signaling_NaN();
  auto fmax = std::numeric_limits<float>::max();
  // Smallest positive non-zero float denormal
  auto fdenorm = 0x0.1p-145f;
  (void)finf, (void)fnan, (void)fsnan, (void)fmax, (void)fdenorm;

  auto dinf = std::numeric_limits<double>::infinity();
  auto dnan = std::numeric_limits<double>::quiet_NaN();
  auto dsnan = std::numeric_limits<double>::signaling_NaN();
  auto dmax = std::numeric_limits<double>::max();
  // Smallest positive non-zero double denormal
  auto ddenorm = 0x0.1p-1070;
  (void)dinf, (void)dnan, (void)dsnan, (void)dmax, (void)ddenorm;

  auto ldinf = std::numeric_limits<long double>::infinity();
  auto ldnan = std::numeric_limits<long double>::quiet_NaN();
  auto ldsnan = std::numeric_limits<long double>::signaling_NaN();
  auto ldmax = std::numeric_limits<long double>::max();
  // Smallest positive non-zero long double denormal
#ifdef _WIN32
  // On Win32 `long double` is an alias for `double`.
  auto lddenorm = 0x0.1p-1070L;
#else
  auto lddenorm = 0x0.1p-16440L;
#endif

  (void)ldinf, (void)ldnan, (void)ldsnan, (void)ldmax, (void)lddenorm;

  int x = 42;
  int* p = &x;
  int** q = &p;
  int& ref = x;
  int* const* const& refp = &p;
  void* void_ptr = p;

  (void)x, (void)p, (void)q, (void)ref, (void)refp, (void)void_ptr;

  int array33[3][3] = {{0, 1, 2}, {3, 4, 5}, {6, 7, 8}};
  int array23[2][3] = {{1, 2, 3}, {4, 5, 6}};
  int array32[3][2] = {{1, 2}, {3, 4}, {5, 6}};
  float flt_array23[2][3] = {{1.0f, 2.0f, 3.0f}, {4.0f, 5.0f, 6.0f}};
  (void)array33, (void)array23, (void)array32, (void)flt_array23;

  int(*ptr_to_arr3)[3] = array33;
  (void)ptr_to_arr3;

  std::nullptr_t null_ptr = nullptr;
  std::nullptr_t* addr_null_ptr = &null_ptr;
  std::nullptr_t& ref_null_ptr = null_ptr;
  (void)null_ptr, (void)addr_null_ptr, (void)ref_null_ptr;

  MultiInheritDerived multi;
  DeadlyDiamondSubclass diamond;
  VirtualDiamondSubclass virtual_diamond;
  (void)multi, (void)diamond, (void)virtual_diamond;

  char* null_char_ptr = nullptr;
  const char* test_str = "Hee hee hee";
  char** addr_null_char_ptr = &null_char_ptr;
  (void)null_char_ptr, (void)test_str, (void)addr_null_char_ptr;

  NonEmptyDerived empty_base;
  (void)empty_base;

  TestStruct ts;
  TestUnion tu;
  tu.uint_field = 65;
  (void)ts, (void)tu;

  TestStruct ts_array[2];
  ts_array[0].int_field = -10;

  ns::nested_ns::TestStruct ns_ts;
  (void)ns_ts;

  ClassWithNestedClass with_nested;
  (void)with_nested;

  struct LocalStruct {
    int int_field;
    int& ref_field;
    int* ptr_field;
    int*& ptr_ref_field;
    double dbl_field;
  } ls{42, x, &x, p, -0.8};
  (void)ls;

  CStyleEnum c_enum = VALUE1;
  EnumClass enum_class = EnumClass::THREE;
  ns::CStyleEnum ns_enum = ns::V2;
  ns::EnumClass ns_enum_class = ns::EnumClass::TWO;
  (void)c_enum, (void)enum_class, (void)ns_enum, (void)ns_enum_class;

  // Modify values of global variables.
  global_ts.flt_field = 2.71f;
  global_ts.int_field = 1337;
  global_ts.ch_field = '*';
  global_ts.ull_field = 1LL << 40;

  // BREAK HERE

  return 0;
}
