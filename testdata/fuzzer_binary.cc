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
  const void* void_ptr = p;

  (void)x, (void)p, (void)q, (void)ref, (void)refp, (void)void_ptr;

  int array2d[3][3] = {{0, 1, 2}, {3, 4, 5}, {6, 7, 8}};
  (void)array2d;

  MultiInheritDerived multi;
  DeadlyDiamondSubclass diamond;
  VirtualDiamondSubclass virtual_diamond;
  (void)multi, (void)diamond, (void)diamond;

  const char* null_char_ptr = nullptr;
  const char* test_str = "Hee hee hee";
  (void)null_char_ptr, (void)test_str;

  NonEmptyDerived empty_base;
  (void)empty_base;

  TestStruct ts;
  TestUnion tu;
  tu.uint_field = 65;

  (void)ts, (void)tu;

  // BREAK HERE

  return 0;
}
