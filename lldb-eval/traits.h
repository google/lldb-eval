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

template <int T>
struct lldb_type_to_builtin;

#define LLDB_TYPE_TO_BUILTIN(T1, T2) \
  template <>                        \
  struct lldb_type_to_builtin<T1> {  \
    using value = T2;                \
  };

LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeBool, bool)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeChar, char)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeSignedChar, signed char)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeUnsignedChar, unsigned char)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeChar16, char16_t)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeChar32, char32_t)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeWChar, wchar_t)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeSignedWChar, wchar_t)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeUnsignedWChar, wchar_t)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeShort, short)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeUnsignedShort, unsigned short)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeInt, int)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeUnsignedInt, unsigned int)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeLong, long)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeUnsignedLong, unsigned long)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeLongLong, long long)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeUnsignedLongLong, unsigned long long)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeFloat, float)
LLDB_TYPE_TO_BUILTIN(lldb::eBasicTypeDouble, double)

#undef LLDB_TYPE_TO_BUILTIN

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
