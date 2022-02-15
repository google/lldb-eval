#include "lldb-eval/type.h"

#include "lldb-eval/traits.h"
#include "lldb/lldb-enumerations.h"
#include "llvm/Support/FormatAdapters.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Regex.h"

namespace lldb_eval {
namespace {
//
// template <typename T>
// bool IsScopedEnum_V(T type) {
//   // SBType::IsScopedEnumerationType was introduced in
//   // https://reviews.llvm.org/D93690. If it's not available yet, fallback to
//   the
//   // "default" implementation.
//   if constexpr (HAS_METHOD(T, IsScopedEnumerationType())) {
//     return type.IsScopedEnumerationType();
//   }
//   return false;
// }
}  // namespace
Type::~Type() = default;

bool Type::IsBasicType() {
  return GetCanonicalType()->GetBasicType() != lldb::eBasicTypeInvalid;
}

bool Type::IsBool() {
  return GetCanonicalType()->GetBasicType() == lldb::eBasicTypeBool;
}

bool Type::IsScalar() { return GetTypeFlags() & lldb::eTypeIsScalar; }

bool Type::IsInteger() { return GetTypeFlags() & lldb::eTypeIsInteger; }

bool Type::IsFloat() { return GetTypeFlags() & lldb::eTypeIsFloat; }

bool Type::IsPointerToVoid() {
  return IsPointerType() &&
         GetPointeeType()->GetBasicType() == lldb::eBasicTypeVoid;
}

bool Type::IsNullPtrType() {
  return GetCanonicalType()->GetBasicType() == lldb::eBasicTypeNullPtr;
}

bool Type::IsSigned() {
  if (IsEnum()) {
    return IsEnumerationIntegerTypeSigned();
  }
  return GetTypeFlags() & lldb::eTypeIsSigned;
}

bool Type::IsEnum() { return GetTypeFlags() & lldb::eTypeIsEnumeration; }

bool Type::IsUnscopedEnum() { return IsEnum() && !IsScopedEnum(); }

bool Type::IsScalarOrUnscopedEnum() { return IsScalar() || IsUnscopedEnum(); }

bool Type::IsIntegerOrUnscopedEnum() { return IsInteger() || IsUnscopedEnum(); }

bool Type::IsRecordType() {
  return GetCanonicalType()->GetTypeClass() &
         (lldb::eTypeClassClass | lldb::eTypeClassStruct |
          lldb::eTypeClassUnion);
}

bool Type::IsSmartPtrType() {
  // Regular expressions are mirrored from LLDB:
  // https://github.com/llvm/llvm-project/blob/release/13.x/lldb/source/Plugins/Language/CPlusPlus/CPlusPlusLanguage.cpp#L614-L634
  static llvm::Regex k_libcxx_std_unique_ptr_regex(
      "^std::__[[:alnum:]]+::unique_ptr<.+>(( )?&)?$");
  static llvm::Regex k_libcxx_std_shared_ptr_regex(
      "^std::__[[:alnum:]]+::shared_ptr<.+>(( )?&)?$");
  static llvm::Regex k_libcxx_std_weak_ptr_regex(
      "^std::__[[:alnum:]]+::weak_ptr<.+>(( )?&)?$");

  llvm::StringRef name = GetName();
  return k_libcxx_std_unique_ptr_regex.match(name) ||
         k_libcxx_std_shared_ptr_regex.match(name) ||
         k_libcxx_std_weak_ptr_regex.match(name);
}

bool Type::IsPromotableIntegerType() {
  // Unscoped enums are always considered as promotable, even if their
  // underlying type does not need to be promoted (e.g. "int").
  if (IsUnscopedEnum()) {
    return true;
  }

  switch (GetCanonicalType()->GetBasicType()) {
    case lldb::eBasicTypeBool:
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
    case lldb::eBasicTypeUnsignedChar:
    case lldb::eBasicTypeShort:
    case lldb::eBasicTypeUnsignedShort:
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeUnsignedWChar:
    case lldb::eBasicTypeChar16:
    case lldb::eBasicTypeChar32:
      return true;

    default:
      return false;
  }
}

bool Type::IsContextuallyConvertibleToBool() {
  return IsScalar() || IsUnscopedEnum() || IsPointerType() || IsNullPtrType() ||
         IsArrayType();
}

bool CompareTypes(TypeSP lhs, TypeSP rhs) {
  if (&lhs == &rhs) {
    return true;
  }

  return lhs->CompareTo(rhs);
}

std::string TypeDescription(TypeSP type) {
  auto name = type->GetName();
  auto canonical_name = type->GetCanonicalType()->GetName();
  if (name.empty() || canonical_name.empty()) {
    return "''";  // should not happen
  }
  if (name == canonical_name) {
    return llvm::formatv("'{0}'", name);
  }
  return llvm::formatv("'{0}' (aka '{1}')", name, canonical_name);
}

bool GetPathToBaseType(TypeSP type, TypeSP target_base,
                       std::vector<uint32_t>* path, uint64_t* offset) {
  if (CompareTypes(type, target_base)) {
    return true;
  }

  uint32_t num_non_empty_bases = 0;
  uint32_t num_direct_bases = type->GetNumberOfDirectBaseClasses();
  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    auto member = type->GetDirectBaseClassAtIndex(i);
    auto base = member.type;
    if (GetPathToBaseType(base, target_base, path, offset)) {
      if (path) {
        path->push_back(num_non_empty_bases);
      }
      if (offset) {
        *offset += member.offset;
      }
      return true;
    }
    if (base->GetNumberOfFields() > 0) {
      num_non_empty_bases++;
    }
  }

  return false;
}

}  // namespace lldb_eval
