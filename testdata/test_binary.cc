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

#include <cstdint>
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
  int* p_null = nullptr;
  const char* p_char1 = "hello";

  typedef const char* my_char_ptr;
  my_char_ptr my_p_char1 = p_char1;

  int offset = 5;
  int array[10];
  array[0] = 0;
  array[offset] = offset;

  int* p_int0 = &array[0];
  int** pp_int0 = &p_int0;
  const int* cp_int0 = &array[0];
  const int* cp_int5 = &array[offset];

  typedef int* td_int_ptr_t;
  td_int_ptr_t td_int_ptr0 = &array[0];

  void* p_void = (void*)p_char1;
  void** pp_void0 = &p_void;
  void** pp_void1 = pp_void0 + 1;

  std::nullptr_t std_nullptr_t = nullptr;

  // BREAK(TestPointerArithmetic)
  // BREAK(PointerPointerArithmeticFloat)
  // BREAK(PointerPointerComparison)
  // BREAK(PointerIntegerComparison)
  // BREAK(TestPointerDereference)
}

static void TestLogicalOperators() {
  bool trueVar = true;
  bool falseVar = false;

  const char* p_ptr = "🦊";
  const char* p_nullptr = nullptr;

  int array[2] = {1, 2};

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
  struct Sx {
    int x;
    int& r;
    char y;
  } s{1, x, 2};

  Sx& sr = s;
  Sx* sp = &s;

  using SxAlias = Sx;
  SxAlias sa{3, x, 4};

  // BREAK(TestMemberOf)
}

static void TestMemberOfInheritance() {
  struct A {
    int a_;
  } a{1};

  struct B {
    int b_;
  } b{2};

  struct C : A, B {
    int c_;
  } c{{1}, {2}, 3};

  struct D : C {
    int d_;
    A fa_;
  } d{{{1}, {2}, 3}, 4, {5}};

  // Virtual inheritance example.
  struct Animal {
    virtual ~Animal() = default;
    int weight_;
  };
  struct Mammal : virtual Animal {};
  struct WingedAnimal : virtual Animal {};
  struct Bat : Mammal, WingedAnimal {
  } bat;
  bat.weight_ = 10;

  // Empty bases example.
  struct IPlugin {
    virtual ~IPlugin() {}
  };
  struct Plugin : public IPlugin {
    int x;
    int y;
  };
  Plugin plugin;
  plugin.x = 1;
  plugin.y = 2;

  struct ObjectBase {
    int x;
  };
  struct Object : ObjectBase {};
  struct Engine : Object {
    int y;
    int z;
  };

  Engine engine;
  engine.x = 1;
  engine.y = 2;
  engine.z = 3;

  // Empty multiple inheritance with empty base.
  struct Base {
    int x;
    int y;
    virtual void Do() = 0;
    virtual ~Base() {}
  };
  struct Mixin {};
  struct Parent : private Mixin, public Base {
    int z;
    virtual void Do(){};
  };
  Parent obj;
  obj.x = 1;
  obj.y = 2;
  obj.z = 3;
  Base* parent_base = &obj;
  Parent* parent = &obj;

  // BREAK(TestMemberOfInheritance)
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

int* globalPtr = &globalVar;
int& globalRef = globalVar;

namespace ns {
int globalVar = 13;
int* globalPtr = &globalVar;
int& globalRef = globalVar;
}  // namespace ns

void TestGlobalVariableLookup() {
  // BREAK(TestGlobalVariableLookup)
}

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

    char c = 1;

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

static void TestArrayDereference() {
  int arr_1d[2] = {1, 2};
  int arr_2d[2][3] = {{1, 2, 3}, {4, 5, 6}};

  // BREAK(TestArrayDereference)
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

  // BREAK(TestCStyleCastBuiltins)
  // BREAK(TestCStyleCastBasicType)
  // BREAK(TestCStyleCastPointer)
  // BREAK(TestCStyleCastNullptrType)

  struct InnerFoo {
    int a;
    int b;
  };

  InnerFoo ifoo;
  (void)ifoo;

  int arr_1d[] = {1, 2, 3, 4};
  int arr_2d[2][3] = {{1, 2, 3}, {4, 5, 6}};

  // BREAK(TestCStyleCastArray)
  // BREAK(TestCStyleCastReference)
}

// Referenced by TestCxxCast
struct CxxVirtualBase {
  int a;
  virtual ~CxxVirtualBase(){};
};
struct CxxVirtualParent : CxxVirtualBase {
  int b;
};

static void TestCxxCast() {
  struct CxxBase {
    int a;
    int b;
  };
  struct CxxParent : CxxBase {
    long long c;
    short d;
  };

  CxxParent parent;
  parent.a = 1;
  parent.b = 2;
  parent.c = 3;
  parent.d = 4;

  CxxBase* base = &parent;

  int arr[] = {1, 2, 3, 4, 5};

  // BREAK(TestCxxStaticCast)
  // BREAK(TestCxxReinterpretCast)

  CxxVirtualParent v_parent;
  v_parent.a = 1;
  v_parent.b = 2;
  CxxVirtualBase* v_base = &v_parent;

  // BREAK(TestCxxDynamicCast)
}

// Referenced by TestQualifiedId.
namespace ns {

int i = 1;

namespace ns {

int i = 2;

}  // namespace ns

}  // namespace ns

static void TestQualifiedId() {
  // BREAK(TestQualifiedId)
}

namespace outer {

namespace inner {

class Vars {
 public:
  inline static double inline_static = 1.5;
  static constexpr int static_constexpr = 2;
  static const unsigned int static_const;
};

const unsigned int Vars::static_const = 3;

}  // namespace inner

class Vars {
 public:
  inline static double inline_static = 4.5;
  static constexpr int static_constexpr = 5;
  static const unsigned int static_const;
};

const unsigned int Vars::static_const = 6;

}  // namespace outer

class Vars {
 public:
  inline static double inline_static = 7.5;
  static constexpr int static_constexpr = 8;
  static const unsigned int static_const;
};

const unsigned int Vars::static_const = 9;

static void TestStaticConst() {
  // BREAK(TestStaticConstDeclaredInline)
  // BREAK(TestStaticConstDeclaredOutsideTheClass)
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
  { T_1<T_1<T_1<int>>>::myint _ = 0; }
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

  int T_1 = 2;

  // BREAK(TestTemplateTypes)
  // BREAK(TestTemplateCpp11)
}

template <typename T, typename TAllocator>
struct TArray {
  using ElementType = T;
  T t_;
  TAllocator a_;
};

template <int Size>
struct Allocator {
  int size = Size;
};

void TestTemplateWithNumericArguments() {
  Allocator<4> a4;
  Allocator<8> a8;
  TArray<int, Allocator<4>> arr;
  decltype(arr)::ElementType* el = 0;

  // BREAK(TestTemplateWithNumericArguments)
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

void TestBitField() {
  struct BitFieldStruct {
    uint16_t a : 10;
    uint32_t b : 4;
    bool c : 1;
    bool d : 1;
  };

  BitFieldStruct bf;
  bf.a = 0b1111111111;
  bf.b = 0b1001;
  bf.c = 0b0;
  bf.d = 0b1;

  struct AlignedBitFieldStruct {
    uint16_t a : 10;
    uint8_t b : 4;
    unsigned char : 0;
    uint16_t c : 2;
  };

  uint32_t data = ~0;
  AlignedBitFieldStruct abf = (AlignedBitFieldStruct&)data;

  // BREAK(TestBitField)
  // BREAK(TestBitFieldPromotion)
}

void TestContextVariables() {
  struct Scope {
    int a = 10;
    const char* ptr = "hello";
  };

  Scope s;

  // BREAK(TestContextVariables)
  // BREAK(TestContextVariablesSubset)
}

// Referenced by TestScopedEnum.
enum class ScopedEnum { kFoo, kBar };
enum class ScopedEnumUInt8 : uint8_t { kFoo, kBar };

void TestScopedEnum() {
  auto enum_foo = ScopedEnum::kFoo;
  auto enum_bar = ScopedEnum::kBar;

  auto enum_u8_foo = ScopedEnumUInt8::kFoo;
  auto enum_u8_bar = ScopedEnumUInt8::kBar;

  // BREAK(TestScopedEnum)
  // BREAK(TestScopedEnumArithmetic)
  // BREAK(TestScopedEnumWithUnderlyingType)
}

enum UnscopedEnum { kZero, kOne, kTwo };
enum UnscopedEnumUInt8 : uint8_t { kZeroU8, kOneU8, kTwoU8 };
enum UnscopedEnumEmpty : uint8_t {};

// UnscopedEnum global_enum = UnscopedEnum::kOne;

void TestUnscopedEnum() {
  auto enum_zero = UnscopedEnum::kZero;
  auto enum_one = UnscopedEnum::kOne;
  auto enum_two = UnscopedEnum::kTwo;

  auto enum_zero_u8 = UnscopedEnumUInt8::kZeroU8;
  auto enum_one_u8 = UnscopedEnumUInt8::kOneU8;
  auto enum_two_u8 = UnscopedEnumUInt8::kTwoU8;

  UnscopedEnumEmpty enum_empty;

  // BREAK(TestUnscopedEnum)
  // BREAK(TestUnscopedEnumNegation)
  // BREAK(TestUnscopedEnumWithUnderlyingType)
  // BREAK(TestUnscopedEnumEmpty)
}

void TestTernaryOperator() {
  int i = 1;
  int* pi = &i;
  char c = 2;
  int array[2] = {1, 2};
  struct T {
  } t;
  // BREAK(TestTernaryOperator)
}

void TestSizeOf() {
  int i = 1;
  int* p = &i;
  int arr[] = {1, 2, 3};

  struct SizeOfFoo {
    int x, y;
  } foo;

  // BREAK(TestSizeOf)
}

void TestBuiltinFunction_Log2() {
  struct Foo {
  } foo;

  enum CEnum { kFoo = 129 } c_enum = kFoo;
  enum class CxxEnum { kFoo = 129 } cxx_enum = CxxEnum::kFoo;

  // BREAK(TestBuiltinFunction_Log2)
}

void TestPrefixIncDec() {
  auto enum_foo = ScopedEnum::kFoo;
  int i = 1;

  // BREAK(TestPrefixIncDec)
  // BREAK(TestPostfixIncDec)
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
  TestMemberOfInheritance();
  TestGlobalVariableLookup();
  tm.TestInstanceVariables();
  TestIndirection();
  tm.TestAddressOf(42);
  TestSubscript();
  TestCStyleCast();
  TestCxxCast();
  TestQualifiedId();
  TestStaticConst();
  TestTemplateTypes();
  TestTemplateWithNumericArguments();
  TestValueScope();
  TestBitField();
  TestContextVariables();
  TestPrefixIncDec();
  TestScopedEnum();
  TestUnscopedEnum();
  TestTernaryOperator();
  TestSizeOf();
  TestBuiltinFunction_Log2();
  TestArrayDereference();

  // BREAK HERE
}

}  // namespace test_binary

int main() { test_binary::main(); }
