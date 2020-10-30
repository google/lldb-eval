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

#include "lldb/lldb-enumerations.h"

template <typename T>
struct builtin_to_lldb_type;

#define BUILTIN_TO_LLDB_TYPE(T1, T2)         \
  template <>                                \
  struct builtin_to_lldb_type<T1> {          \
    const static lldb::BasicType value = T2; \
  };

BUILTIN_TO_LLDB_TYPE(int, lldb::eBasicTypeInt)
BUILTIN_TO_LLDB_TYPE(unsigned int, lldb::eBasicTypeUnsignedInt)
BUILTIN_TO_LLDB_TYPE(long, lldb::eBasicTypeLong)
BUILTIN_TO_LLDB_TYPE(unsigned long, lldb::eBasicTypeUnsignedLong)
BUILTIN_TO_LLDB_TYPE(long long, lldb::eBasicTypeLongLong)
BUILTIN_TO_LLDB_TYPE(unsigned long long, lldb::eBasicTypeUnsignedLongLong)

#undef BUILTIN_TO_LLDB_TYPE

#define LLDB_TYPE_BUILTIN_PROMOTABLE_INTEGER(V)  \
  V(lldb::eBasicTypeBool, bool)                  \
  V(lldb::eBasicTypeChar, char)                  \
  V(lldb::eBasicTypeSignedChar, signed char)     \
  V(lldb::eBasicTypeUnsignedChar, unsigned char) \
  V(lldb::eBasicTypeChar16, char16_t)            \
  V(lldb::eBasicTypeChar32, char32_t)            \
  V(lldb::eBasicTypeWChar, wchar_t)              \
  V(lldb::eBasicTypeSignedWChar, wchar_t)        \
  V(lldb::eBasicTypeUnsignedWChar, wchar_t)      \
  V(lldb::eBasicTypeShort, short)                \
  V(lldb::eBasicTypeUnsignedShort, unsigned short)

#define LLDB_TYPE_BUILTIN_INTEGRAL(V)            \
  LLDB_TYPE_BUILTIN_PROMOTABLE_INTEGER(V)        \
  V(lldb::eBasicTypeInt, int)                    \
  V(lldb::eBasicTypeUnsignedInt, unsigned int)   \
  V(lldb::eBasicTypeLong, long)                  \
  V(lldb::eBasicTypeUnsignedLong, unsigned long) \
  V(lldb::eBasicTypeLongLong, long long)         \
  V(lldb::eBasicTypeUnsignedLongLong, unsigned long long)

#define LLDB_TYPE_BUILTIN(V)      \
  LLDB_TYPE_BUILTIN_INTEGRAL(V)   \
  V(lldb::eBasicTypeFloat, float) \
  V(lldb::eBasicTypeDouble, double)
