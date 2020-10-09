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

#include "lldb-eval/pointer.h"

#include "lldb-eval/scalar.h"
#include "lldb/API/SBValue.h"
#include "lldb/lldb-enumerations.h"

namespace lldb_eval {

bool Pointer::AsBool() const { return addr_ != 0; }

bool Pointer::IsPointerToVoid() {
  return type_.GetPointeeType().GetBasicType() == lldb::eBasicTypeVoid;
}

Pointer Pointer::Add(int64_t offset) {
  return Pointer(addr_ + offset * type_.GetPointeeType().GetByteSize(), type_);
}

Pointer Pointer::FromSbValue(lldb::SBValue value) {
  if (!value.GetType().GetCanonicalType().IsPointerType()) {
    return Pointer();
  }

  uint64_t base_addr = value.GetValueAsUnsigned();
  lldb::SBType item_type = value.GetType();

  return Pointer(base_addr, item_type);
}

}  // namespace lldb_eval
