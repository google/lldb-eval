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

#include <limits>
#include <string>

static void TestArithmetic() {
  char c = 10;
  unsigned char uc = 1;
  int a = 1;
  int int_max = std::numeric_limits<int>::max();
  int int_min = std::numeric_limits<int>::min();
  unsigned int uint_max = std::numeric_limits<unsigned int>::max();
  unsigned int uint_zero = 0;
  long long ll_max = std::numeric_limits<long long>::max();
  long long ll_min = std::numeric_limits<long long>::min();
  unsigned long long ull_max = std::numeric_limits<unsigned long long>::max();
  unsigned long long ull_zero = 0;

  int x = 2;
  int& r = x;
  int* p = &x;

  typedef int& myr;
  myr my_r = x;

  auto fnan = std::numeric_limits<float>::quiet_NaN();
  auto fsnan = std::numeric_limits<float>::signaling_NaN();
  // Smallest positive non-zero float denormal
  auto fdenorm = 0x0.1p-145f;

  // BREAK(TestArithmetic)
  // BREAK(TestZeroDivision)
}

static void TestBitwiseOperators() {
  bool var_true = true;
  bool var_false = false;

  unsigned long long ull_max = std::numeric_limits<unsigned long long>::max();
  unsigned long long ull_zero = 0;

  struct S {
  } s;

  const char* p = nullptr;

  uint32_t mask_ff = 0xFF;

  // BREAK(TestBitwiseOperators)
}

static void TestPointerArithmetic() {
  const char* p_char1 = "hello";

  typedef const char* my_char_ptr;
  my_char_ptr my_p_char1 = p_char1;

  int offset = 5;
  int array[10];
  array[0] = 0;
  array[offset] = offset;

  int* p_int0 = &array[0];
  const int* cp_int0 = &array[0];
  const int* cp_int5 = &array[offset];

  typedef int* td_int_ptr_t;
  td_int_ptr_t td_int_ptr0 = &array[0];

  void* p_void = (void*)p_char1;
  void** pp_void0 = &p_void;
  void** pp_void1 = pp_void0 + 1;

  // BREAK(TestPointerArithmetic)
  // BREAK(PointerPointerComparison)
  // BREAK(PointerIntegerComparison)
}

static void TestLogicalOperators() {
  bool trueVar = true;
  bool falseVar = false;

  const char* p_ptr = "🦊";
  const char* p_nullptr = nullptr;

  struct S {
  } s;

  // BREAK(TestLogicalOperators)
}

static void TestLocalVariables() {
  int a = 1;
  int b = 2;

  char c = -3;
  unsigned short s = 4;

  // BREAK(TestLocalVariables)
}

static void TestMemberOf() {
  int x = 2;
  struct S {
    int x;
    int& r;
  } s{1, x};

  S& sr = s;
  S* sp = &s;

  // BREAK(TestMemberOf)
}

static void TestIndirection() {
  int val = 1;
  int* p = &val;

  typedef int* myp;
  myp my_p = &val;

  typedef int*& mypr;
  mypr my_pr = p;

  // BREAK(TestIndirection)
}

// Referenced by TestInstanceVariables
class C {
 public:
  int field_ = 1337;
};

// Referenced by TestAddressOf
int globalVar = 0xDEADBEEF;
extern int externGlobalVar;

class TestMethods {
 public:
  void TestInstanceVariables() {
    C c;
    c.field_ = -1;

    C& c_ref = c;
    C* c_ptr = &c;

    // BREAK(TestInstanceVariables)
  }

  void TestAddressOf(int param) {
    int x = 42;
    int& r = x;
    int* p = &x;
    int*& pr = p;

    typedef int*& mypr;
    mypr my_pr = p;

    std::string s = "hello";
    const char* s_str = s.c_str();

    // BREAK(TestAddressOf)
  }

 private:
  int field_ = 1;
};

static void TestSubscript() {
  const char* char_ptr = "lorem";
  const char char_arr[] = "ipsum";

  int int_arr[] = {1, 2, 3};

  C c_arr[2];
  c_arr[0].field_ = 0;
  c_arr[1].field_ = 1;

  C(&c_arr_ref)[2] = c_arr;

  int idx_1 = 1;
  const int& idx_1_ref = idx_1;

  typedef int td_int_t;
  typedef td_int_t td_td_int_t;
  typedef int* td_int_ptr_t;
  typedef int& td_int_ref_t;

  td_int_t td_int_idx_1 = 1;
  td_td_int_t td_td_int_idx_2 = 2;

  td_int_t td_int_arr[3] = {1, 2, 3};
  td_int_ptr_t td_int_ptr = td_int_arr;

  td_int_ref_t td_int_idx_1_ref = td_int_idx_1;
  td_int_t(&td_int_arr_ref)[3] = td_int_arr;

  unsigned char uchar_idx = std::numeric_limits<unsigned char>::max();
  uint8_t uint8_arr[256];
  uint8_arr[255] = 0xAB;
  uint8_t* uint8_ptr = uint8_arr;

  // BREAK(TestSubscript)
}

// Referenced by TestCStyleCast
namespace ns {

typedef int myint;

class Foo {};

namespace inner {

using mydouble = double;

class Foo {};

}  // namespace inner

}  // namespace ns

static void TestCStyleCast() {
  int a = 1;
  int* ap = &a;
  void* vp = &a;

  int na = -1;
  float f = 1.1;

  typedef int myint;

  myint myint_ = 1;
  ns::myint ns_myint_ = 2;
  ns::Foo ns_foo_;
  ns::Foo* ns_foo_ptr_ = &ns_foo_;

  ns::inner::mydouble ns_inner_mydouble_ = 1.2;
  ns::inner::Foo ns_inner_foo_;
  ns::inner::Foo* ns_inner_foo_ptr_ = &ns_inner_foo_;

  // BREAK(TestCStyleCastBasicType)
  // BREAK(TestCStyleCastPointer)
}

static void TestCStyleCastToReference() {
  struct InnerFoo {
    int a;
    int b;
  };

  InnerFoo ifoo;
  (void)ifoo;

  int arr[] = {1, 2, 3, 4};

  // BREAK(TestCStyleCastReference)
}

// Referenced by TestQualifiedId.
namespace ns {

int i = 1;

namespace ns {

int i = 2;

}  // namespace ns

}  // namespace ns

class Foo {
 public:
  static const int x = 42;
  static const int y;
};

const int Foo::y = 42;

static void TestQualifiedId() {
  // BREAK(TestQualifiedId)
}

// Referenced by TestTemplateTypes.
template <typename T>
struct T_1 {
  static const int cx;
  typedef double myint;

  T_1() {}
  T_1(T x) : x(x) {}
  T x;
};

template <typename T>
const int T_1<T>::cx = 42;

template <>
const int T_1<int>::cx = 24;

template <typename T1, typename T2>
struct T_2 {
  typedef float myint;

  T_2() {}
  T1 x;
  T2 y;
};

namespace ns {

template <typename T>
struct T_1 {
  static const int cx;
  typedef int myint;

  T_1() {}
  T_1(T x) : x(x) {}
  T x;
};

template <typename T>
const int T_1<T>::cx = 46;

template <>
const int T_1<int>::cx = 64;

}  // namespace ns

static void TestTemplateTypes() {
  int i;
  int* p = &i;

  { T_1<int> _; }
  { T_1<int*> _; }
  { T_1<int**> _; }
  { T_1<int&> _(i); }
  { T_1<int*&> _(p); }
  { T_1<double> _; }
  { T_2<int, char> _; }
  { T_2<char, int> _; }
  { T_2<T_1<int>, T_1<char>> _; }
  { T_2<T_1<T_1<int>>, T_1<char>> _; }

  { ns::T_1<int> _; }
  { ns::T_1<ns::T_1<int>> _; }

  { T_1<int>::myint _ = 0; }
  { T_1<int*>::myint _ = 0; }
  { T_1<int**>::myint _ = 0; }
  { T_1<int&>::myint _ = 0; }
  { T_1<int*&>::myint _ = 0; }
  { T_1<T_1<int>>::myint _ = 0; }
  { T_1<T_1<int*>>::myint _ = 0; }
  { T_1<T_1<int**>>::myint _ = 0; }
  { T_1<T_1<int&>>::myint _ = 0; }
  { T_1<T_1<int*&>>::myint _ = 0; }

  { T_2<int, char>::myint _ = 0; }
  { T_2<int*, char&>::myint _ = 0; }
  { T_2<int&, char*>::myint _ = 0; }
  { T_2<T_1<T_1<int>>, T_1<char>>::myint _ = 0; }

  { ns::T_1<int>::myint _ = 0; }
  { ns::T_1<int*>::myint _ = 0; }
  { ns::T_1<int**>::myint _ = 0; }
  { ns::T_1<int&>::myint _ = 0; }
  { ns::T_1<int*&>::myint _ = 0; }
  { ns::T_1<T_1<int>>::myint _ = 0; }
  { ns::T_1<T_1<int*>>::myint _ = 0; }
  { ns::T_1<T_1<int**>>::myint _ = 0; }
  { ns::T_1<T_1<int&>>::myint _ = 0; }
  { ns::T_1<T_1<int*&>>::myint _ = 0; }
  { ns::T_1<ns::T_1<int>>::myint _ = 0; }
  { ns::T_1<ns::T_1<int*>>::myint _ = 0; }
  { ns::T_1<ns::T_1<int**>>::myint _ = 0; }
  { ns::T_1<ns::T_1<int&>>::myint _ = 0; }
  { ns::T_1<ns::T_1<int*&>>::myint _ = 0; }

  (void)T_1<double>::cx;
  (void)ns::T_1<double>::cx;
  (void)ns::T_1<ns::T_1<int>>::cx;

  // BREAK(TestTemplateTypes)
}

void TestValueScope() {
  class Value {
   public:
    Value(int x, double y) : x_(x), y_(y) {}

   private:
    int x_;
    double y_;
  };

  Value var(1, 2.5);
  uint64_t z_ = 3;

  // BREAK(TestValueScope)
}

namespace test_binary {

void main() {
  // BREAK(TestSymbols)

  TestMethods tm;

  TestArithmetic();
  TestBitwiseOperators();
  TestPointerArithmetic();
  TestLogicalOperators();
  TestLocalVariables();
  TestMemberOf();
  tm.TestInstanceVariables();
  TestIndirection();
  tm.TestAddressOf(42);
  TestSubscript();
  TestCStyleCast();
  TestCStyleCastToReference();
  TestQualifiedId();
  TestTemplateTypes();
  TestValueScope();

  // break here
}

}  // namespace test_binary

int main() { test_binary::main(); }
