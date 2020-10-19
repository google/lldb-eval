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
  int x = 42;
  (void)x;

  int* p = &x;
  (void)p;

  int** q = &p;
  (void)q;

  int& ref = x;
  (void)ref;

  int* const* const& refp = &p;
  (void)refp;

  int array2d[3][3] = {{0, 1, 2}, {3, 4, 5}, {6, 7, 8}};
  (void)array2d;

  MultiInheritDerived multi;
  (void)multi;

  DeadlyDiamondSubclass diamond;
  (void)diamond;

  VirtualDiamondSubclass virtual_diamond;
  (void)diamond;

  const char* null_char_ptr = nullptr;
  (void)null_char_ptr;

  const char* test_str = "Hee hee hee";
  (void)test_str;

  NonEmptyDerived empty_base;
  (void)empty_base;

  TestStruct ts;
  (void)ts;

  TestUnion tu;
  tu.uint_field = 65;
  (void)tu;

  // BREAK HERE

  return 0;
}
