#ifndef LLDB_EVAL_TYPE_H_
#define LLDB_EVAL_TYPE_H_

#include <cstdint>
#include <memory>

#include "lldb/lldb-enumerations.h"
#include "llvm/ADT/StringRef.h"
namespace lldb_eval {
class ParserContext;
class Type;

using TypeSP = std::shared_ptr<Type>;

class Type {
 public:
  virtual ~Type();
  virtual uint64_t GetByteSize() = 0;
  virtual uint32_t GetTypeFlags() = 0;
  virtual bool IsArrayType() = 0;
  virtual bool IsPointerType() = 0;
  virtual bool IsReferenceType() = 0;
  virtual bool IsPolymorphicClass() = 0;
  virtual bool IsScopedEnum() = 0;
  virtual bool IsAnonymousType() = 0;
  virtual llvm::StringRef GetName() = 0;
  virtual TypeSP GetArrayElementType() = 0;
  virtual TypeSP GetArrayType(uint64_t) = 0;
  virtual TypeSP GetEnumerationIntegerType(ParserContext&) = 0;
  virtual bool IsEnumerationIntegerTypeSigned() = 0;
  virtual TypeSP GetPointerType() = 0;
  virtual TypeSP GetPointeeType() = 0;
  virtual TypeSP GetReferenceType() = 0;
  virtual TypeSP GetDereferencedType() = 0;
  virtual TypeSP GetCanonicalType() = 0;
  virtual TypeSP GetUnqualifiedType() = 0;
  virtual lldb::BasicType GetBasicType() = 0;
  virtual lldb::TypeClass GetTypeClass() = 0;
  virtual uint32_t GetNumberOfDirectBaseClasses() = 0;
  virtual uint32_t GetNumberOfFields() = 0;
  virtual TypeSP GetSmartPtrPointeeType() = 0;  // std::unique_ptr<T> -> T

  struct BaseInfo {
    TypeSP type;
    uint64_t offset;
  };
  virtual BaseInfo GetDirectBaseClassAtIndex(uint32_t) = 0;
  virtual uint32_t GetNumberOfVirtualBaseClasses() = 0;
  virtual TypeSP GetVirtualBaseClassAtIndex(uint32_t) = 0;

  struct MemberInfo {
    llvm::Optional<std::string> name;
    TypeSP type;
    bool is_bitfield;
    uint32_t bitfield_size_in_bits;

    explicit operator bool() const { return type->IsValid(); }
  };
  virtual MemberInfo GetFieldAtIndex(uint32_t) = 0;
  virtual bool IsValid() const = 0;
  virtual bool CompareTo(TypeSP) = 0;

  bool IsBasicType();
  bool IsBool();
  bool IsScalar();
  bool IsInteger();
  bool IsFloat();
  bool IsPointerToVoid();
  bool IsSmartPtrType();
  bool IsNullPtrType();
  bool IsSigned();
  bool IsEnum();
  bool IsUnscopedEnum();
  bool IsScalarOrUnscopedEnum();
  bool IsIntegerOrUnscopedEnum();
  bool IsRecordType();
  bool IsPromotableIntegerType();
  bool IsContextuallyConvertibleToBool();
};

bool CompareTypes(TypeSP lhs, TypeSP rhs);
std::string TypeDescription(TypeSP type);

// Checks whether `target_base` is a direct or indirect base of `type`.
// If `path` is provided, it stores the sequence of direct base types from
// `target_base` to `type`.
bool GetPathToBaseType(TypeSP type, TypeSP target_base,
                       std::vector<uint32_t>* path, uint64_t* offset);

}  // namespace lldb_eval

#endif  // LLDB_EVAL_TYPE_H_
