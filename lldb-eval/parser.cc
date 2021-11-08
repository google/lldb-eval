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

#include "lldb-eval/parser.h"

#include <stdlib.h>

#include <cstdint>
#include <memory>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

#include "clang/Basic/Diagnostic.h"
#include "clang/Basic/LangOptions.h"
#include "clang/Basic/SourceManager.h"
#include "clang/Basic/TargetInfo.h"
#include "clang/Basic/TargetOptions.h"
#include "clang/Basic/TokenKinds.h"
#include "clang/Lex/HeaderSearch.h"
#include "clang/Lex/HeaderSearchOptions.h"
#include "clang/Lex/LiteralSupport.h"
#include "clang/Lex/ModuleLoader.h"
#include "clang/Lex/Preprocessor.h"
#include "clang/Lex/PreprocessorOptions.h"
#include "clang/Lex/Token.h"
#include "lldb-eval/ast.h"
#include "lldb-eval/defines.h"
#include "lldb-eval/value.h"
#include "lldb/API/SBType.h"
#include "lldb/API/SBValue.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/iterator_range.h"
#include "llvm/Support/FormatAdapters.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/Host.h"

namespace {

const lldb_eval::Type kInvalidType = lldb_eval::Type();

const char* kInvalidOperandsToUnaryExpression =
    "invalid argument type {0} to unary expression";

const char* kInvalidOperandsToBinaryExpression =
    "invalid operands to binary expression ({0} and {1})";

const char* kValueIsNotConvertibleToBool =
    "value of type {0} is not contextually convertible to 'bool'";

template <typename T>
constexpr unsigned type_width() {
  return static_cast<unsigned>(sizeof(T)) * CHAR_BIT;
}

inline void TokenKindsJoinImpl(std::ostringstream& os,
                               clang::tok::TokenKind k) {
  os << "'" << clang::tok::getTokenName(k) << "'";
}

template <typename... Ts>
inline void TokenKindsJoinImpl(std::ostringstream& os, clang::tok::TokenKind k,
                               Ts... ks) {
  TokenKindsJoinImpl(os, k);
  os << ", ";
  TokenKindsJoinImpl(os, ks...);
}

template <typename... Ts>
inline std::string TokenKindsJoin(clang::tok::TokenKind k, Ts... ks) {
  std::ostringstream os;
  TokenKindsJoinImpl(os, k, ks...);

  return os.str();
}

}  // namespace

namespace lldb_eval {

std::tuple<lldb::BasicType, bool> PickIntegerType(
    std::shared_ptr<Context> ctx, const clang::NumericLiteralParser& literal,
    const llvm::APInt& value) {
  unsigned int_size =
      ctx->GetBasicType(lldb::eBasicTypeInt).GetByteSize() * CHAR_BIT;
  unsigned long_size =
      ctx->GetBasicType(lldb::eBasicTypeLong).GetByteSize() * CHAR_BIT;
  unsigned long_long_size =
      ctx->GetBasicType(lldb::eBasicTypeLongLong).GetByteSize() * CHAR_BIT;

  // Binary, Octal, Hexadecimal and literals with a U suffix are allowed to be
  // an unsigned integer.
  bool unsigned_is_allowed = literal.isUnsigned || literal.getRadix() != 10;

  // Try int/unsigned int.
  if (!literal.isLong && !literal.isLongLong && value.isIntN(int_size)) {
    if (!literal.isUnsigned && value.isIntN(int_size - 1)) {
      return {lldb::eBasicTypeInt, false};
    }
    if (unsigned_is_allowed) {
      return {lldb::eBasicTypeUnsignedInt, true};
    }
  }
  // Try long/unsigned long.
  if (!literal.isLongLong && value.isIntN(long_size)) {
    if (!literal.isUnsigned && value.isIntN(long_size - 1)) {
      return {lldb::eBasicTypeLong, false};
    }
    if (unsigned_is_allowed) {
      return {lldb::eBasicTypeUnsignedLong, true};
    }
  }
  // Try long long/unsigned long long.
  if (value.isIntN(long_long_size)) {
    if (!literal.isUnsigned && value.isIntN(long_long_size - 1)) {
      return {lldb::eBasicTypeLongLong, false};
    }
    if (unsigned_is_allowed) {
      return {lldb::eBasicTypeUnsignedLongLong, true};
    }
  }

  // If we still couldn't decide a type, we probably have something that does
  // not fit in a signed long long, but has no U suffix. Also known as:
  //
  //  warning: integer literal is too large to be represented in a signed
  //  integer type, interpreting as unsigned [-Wimplicitly-unsigned-literal]
  //
  return {lldb::eBasicTypeUnsignedLongLong, true};
}

static bool TokenEndsTemplateArgumentList(const clang::Token& token) {
  // Note: in C++11 ">>" can be treated as "> >" and thus be a valid token
  // for the template argument list.
  return token.isOneOf(clang::tok::comma, clang::tok::greater,
                       clang::tok::greatergreater);
}

static ExprResult InsertSmartPtrToPointerConversion(ExprResult expr) {
  Type expr_type = expr->result_type_deref();

  assert(
      expr_type.IsSmartPtrType() &&
      "an argument to smart-ptr-to-pointer conversion must be a smart pointer");

  return std::make_unique<SmartPtrToPtrDecay>(
      expr->location(), expr_type.GetSmartPtrPointeeType().GetPointerType(),
      std::move(expr));
}

static ExprResult InsertArrayToPointerConversion(ExprResult expr) {
  assert(expr->result_type_deref().IsArrayType() &&
         "an argument to array-to-pointer conversion must be an array");

  // TODO(werat): Make this an explicit array-to-pointer conversion instead of
  // using a "generic" CStyleCastNode.
  return std::make_unique<CStyleCastNode>(
      expr->location(),
      expr->result_type_deref().GetArrayElementType().GetPointerType(),
      std::move(expr), CStyleCastKind::kPointer);
}

static lldb::SBType DoIntegralPromotion(std::shared_ptr<Context> ctx,
                                        Type from) {
  assert((from.IsInteger() || from.IsUnscopedEnum()) &&
         "Integral promotion works only for integers and unscoped enums.");

  // Don't do anything if the type doesn't need to be promoted.
  if (!from.IsPromotableIntegerType()) {
    return from;
  }

  if (from.IsUnscopedEnum()) {
    // Get the enumeration underlying type and promote it.
    return DoIntegralPromotion(ctx, from.GetEnumerationIntegerType(ctx));
  }

  // At this point the type should an integer.
  assert(from.IsInteger() && "invalid type: must be an integer");

  // Get the underlying builtin representation.
  lldb::BasicType builtin_type = from.GetCanonicalType().GetBasicType();

  if (builtin_type == lldb::eBasicTypeWChar ||
      builtin_type == lldb::eBasicTypeSignedWChar ||
      builtin_type == lldb::eBasicTypeUnsignedWChar ||
      builtin_type == lldb::eBasicTypeChar16 ||
      builtin_type == lldb::eBasicTypeChar32) {
    // Find the type that can hold the entire range of values for our type.
    bool is_signed = from.IsSigned();
    uint64_t from_size = from.GetByteSize();

    lldb::SBType promote_types[] = {
        ctx->GetBasicType(lldb::eBasicTypeInt),
        ctx->GetBasicType(lldb::eBasicTypeUnsignedInt),
        ctx->GetBasicType(lldb::eBasicTypeLong),
        ctx->GetBasicType(lldb::eBasicTypeUnsignedLong),
        ctx->GetBasicType(lldb::eBasicTypeLongLong),
        ctx->GetBasicType(lldb::eBasicTypeUnsignedLongLong),
    };
    for (lldb::SBType& type : promote_types) {
      if (from_size < type.GetByteSize() ||
          (from_size == type.GetByteSize() &&
           is_signed == (bool)(type.GetTypeFlags() & lldb::eTypeIsSigned))) {
        return type;
      }
    }

    lldb_eval_unreachable("char type should fit into long long");
  }

  // Here we can promote only to "int" or "unsigned int".
  lldb::SBType int_type = ctx->GetBasicType(lldb::eBasicTypeInt);

  // Signed integer types can be safely promoted to "int".
  if (from.IsSigned()) {
    return int_type;
  }
  // Unsigned integer types are promoted to "unsigned int" if "int" cannot hold
  // their entire value range.
  return (from.GetByteSize() == int_type.GetByteSize())
             ? ctx->GetBasicType(lldb::eBasicTypeUnsignedInt)
             : int_type;
}

static ExprResult UsualUnaryConversions(std::shared_ptr<Context> ctx,
                                        ExprResult expr) {
  // Perform usual conversions for unary operators. At the moment this includes
  // array-to-pointer and the integral promotion for eligible types.
  Type result_type = expr->result_type_deref();

  if (expr->is_bitfield()) {
    // Promote bitfields. If `int` can represent the bitfield value, it is
    // converted to `int`. Otherwise, if `unsigned int` can represent it, it
    // is converted to `unsigned int`. Otherwise, it is treated as its
    // underlying type.

    uint32_t bitfield_size = expr->bitfield_size();
    // Some bitfields have undefined size (e.g. result of ternary operation).
    // The AST's `bitfield_size` of those is 0, and no promotion takes place.
    if (bitfield_size > 0 && result_type.IsInteger()) {
      lldb::SBType int_type = ctx->GetBasicType(lldb::eBasicTypeInt);
      lldb::SBType uint_type = ctx->GetBasicType(lldb::eBasicTypeUnsignedInt);
      uint32_t int_bit_size = int_type.GetByteSize() * CHAR_BIT;
      if (bitfield_size < int_bit_size ||
          (result_type.IsSigned() && bitfield_size == int_bit_size)) {
        expr = std::make_unique<CStyleCastNode>(expr->location(), int_type,
                                                std::move(expr),
                                                CStyleCastKind::kArithmetic);
      } else if (bitfield_size <= uint_type.GetByteSize() * CHAR_BIT) {
        expr = std::make_unique<CStyleCastNode>(expr->location(), uint_type,
                                                std::move(expr),
                                                CStyleCastKind::kArithmetic);
      }
    }
  }

  if (result_type.IsArrayType()) {
    expr = InsertArrayToPointerConversion(std::move(expr));
  }

  if (result_type.IsInteger() || result_type.IsUnscopedEnum()) {
    lldb::SBType promoted_type = DoIntegralPromotion(ctx, result_type);

    // Insert a cast if the type promotion is happening.
    // TODO(werat): Make this an implicit static_cast.
    if (!CompareTypes(promoted_type, result_type)) {
      expr = std::make_unique<CStyleCastNode>(expr->location(), promoted_type,
                                              std::move(expr),
                                              CStyleCastKind::kArithmetic);
    }
  }

  return expr;
}

static size_t ConversionRank(lldb::SBType type) {
  // Get integer conversion rank
  // https://eel.is/c++draft/conv.rank
  switch (type.GetCanonicalType().GetBasicType()) {
    case lldb::eBasicTypeBool:
      return 1;
    case lldb::eBasicTypeChar:
    case lldb::eBasicTypeSignedChar:
    case lldb::eBasicTypeUnsignedChar:
      return 2;
    case lldb::eBasicTypeShort:
    case lldb::eBasicTypeUnsignedShort:
      return 3;
    case lldb::eBasicTypeInt:
    case lldb::eBasicTypeUnsignedInt:
      return 4;
    case lldb::eBasicTypeLong:
    case lldb::eBasicTypeUnsignedLong:
      return 5;
    case lldb::eBasicTypeLongLong:
    case lldb::eBasicTypeUnsignedLongLong:
      return 6;

      // TODO: The ranks of char16_t, char32_t, and wchar_t are equal to the
      // ranks of their underlying types.
    case lldb::eBasicTypeWChar:
    case lldb::eBasicTypeSignedWChar:
    case lldb::eBasicTypeUnsignedWChar:
      return 3;
    case lldb::eBasicTypeChar16:
      return 3;
    case lldb::eBasicTypeChar32:
      return 4;

    default:
      break;
  }
  return 0;
}

static lldb::BasicType BasicTypeToUnsigned(lldb::BasicType basic_type) {
  switch (basic_type) {
    case lldb::eBasicTypeInt:
      return lldb::eBasicTypeUnsignedInt;
    case lldb::eBasicTypeLong:
      return lldb::eBasicTypeUnsignedLong;
    case lldb::eBasicTypeLongLong:
      return lldb::eBasicTypeUnsignedLongLong;
    default:
      return basic_type;
  }
}

static void PerformIntegerConversions(std::shared_ptr<Context> ctx,
                                      ExprResult& l, ExprResult& r,
                                      bool convert_lhs, bool convert_rhs) {
  // Assert that rank(l) < rank(r).
  Type l_type = l->result_type_deref();
  Type r_type = r->result_type_deref();

  // if `r` is signed and `l` is unsigned, check whether it can represent all
  // of the values of the type of the `l`. If not, then promote `r` to the
  // unsigned version of its type.
  if (r_type.IsSigned() && !l_type.IsSigned()) {
    uint64_t l_size = l_type.GetByteSize();
    uint64_t r_size = r_type.GetByteSize();

    assert(l_size <= r_size && "left value must not be larger then the right!");

    if (r_size == l_size) {
      lldb::SBType r_type_unsigned = ctx->GetBasicType(
          BasicTypeToUnsigned(r_type.GetCanonicalType().GetBasicType()));
      if (convert_rhs) {
        r = std::make_unique<CStyleCastNode>(r->location(), r_type_unsigned,
                                             std::move(r),
                                             CStyleCastKind::kArithmetic);
      }
    }
  }

  if (convert_lhs) {
    l = std::make_unique<CStyleCastNode>(l->location(), r->result_type(),
                                         std::move(l),
                                         CStyleCastKind::kArithmetic);
  }
}

static lldb::SBType UsualArithmeticConversions(std::shared_ptr<Context> ctx,
                                               ExprResult& lhs, ExprResult& rhs,
                                               bool is_comp_assign = false) {
  // Apply unary conversions (e.g. intergal promotion) for both operands.
  // In case of a composite assignment operator LHS shouldn't get promoted.
  if (!is_comp_assign) {
    lhs = UsualUnaryConversions(ctx, std::move(lhs));
  }
  rhs = UsualUnaryConversions(ctx, std::move(rhs));

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  if (CompareTypes(lhs_type, rhs_type)) {
    return lhs_type;
  }

  // If either of the operands is not arithmetic (e.g. pointer), we're done.
  if (!lhs_type.IsScalar() || !rhs_type.IsScalar()) {
    return kInvalidType;
  }

  // Handle conversions for floating types (float, double).
  if (lhs_type.IsFloat() || rhs_type.IsFloat()) {
    // If both are floats, convert the smaller operand to the bigger.
    if (lhs_type.IsFloat() && rhs_type.IsFloat()) {
      int order = lhs_type.GetBasicType() - rhs_type.GetBasicType();
      if (order > 0) {
        rhs = std::make_unique<CStyleCastNode>(rhs->location(), lhs_type,
                                               std::move(rhs),
                                               CStyleCastKind::kArithmetic);
        return lhs_type;
      }
      assert(order < 0 && "illegal operands: must not be of the same type");
      if (!is_comp_assign) {
        lhs = std::make_unique<CStyleCastNode>(lhs->location(), rhs_type,
                                               std::move(lhs),
                                               CStyleCastKind::kArithmetic);
      }
      return rhs_type;
    }

    if (lhs_type.IsFloat()) {
      assert(rhs_type.IsInteger() && "illegal operand: must be an integer");
      rhs = std::make_unique<CStyleCastNode>(rhs->location(), lhs_type,
                                             std::move(rhs),
                                             CStyleCastKind::kArithmetic);
      return lhs_type;
    }
    assert(rhs_type.IsFloat() && "illegal operand: must be a float");
    if (!is_comp_assign) {
      lhs = std::make_unique<CStyleCastNode>(lhs->location(), rhs_type,
                                             std::move(lhs),
                                             CStyleCastKind::kArithmetic);
    }
    return rhs_type;
  }

  // Handle conversion for integer types.
  assert((lhs_type.IsInteger() && rhs_type.IsInteger()) &&
         "illegal operands: must be both integers");

  using Rank = std::tuple<size_t, bool>;
  Rank l_rank = {ConversionRank(lhs_type), !lhs_type.IsSigned()};
  Rank r_rank = {ConversionRank(rhs_type), !rhs_type.IsSigned()};

  if (l_rank < r_rank) {
    PerformIntegerConversions(ctx, lhs, rhs, !is_comp_assign, true);
  } else if (l_rank > r_rank) {
    PerformIntegerConversions(ctx, rhs, lhs, true, !is_comp_assign);
  }

  if (!is_comp_assign) {
    assert(CompareTypes(lhs->result_type_deref(), rhs->result_type_deref()) &&
           "integral promotion error: operands result types must be the same");
  }

  return lhs->result_type_deref().GetCanonicalType();
}

static uint32_t GetNumberOfNonEmptyBaseClasses(lldb::SBType type) {
  // Go through the base classes and count non-empty ones.
  uint32_t ret = 0;
  uint32_t num_direct_bases = type.GetNumberOfDirectBaseClasses();

  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    lldb::SBTypeMember base = type.GetDirectBaseClassAtIndex(i);
    lldb::SBType base_type = base.GetType();
    if (base_type.GetNumberOfFields() > 0 ||
        GetNumberOfNonEmptyBaseClasses(base_type) > 0) {
      ret += 1;
    }
  }
  return ret;
}

static lldb::SBTypeMember GetFieldWithNameIndexPath(
    lldb::SBType type, const std::string& name, std::vector<uint32_t>* idx) {
  // Go through the fields first.
  uint32_t num_fields = type.GetNumberOfFields();
  for (uint32_t i = 0; i < num_fields; ++i) {
    lldb::SBTypeMember field = type.GetFieldAtIndex(i);
    // Name can be null if this is a padding field.
    if (const char* field_name = field.GetName()) {
      if (field_name == name) {
        if (idx) {
          // Direct base classes are located before fields, so field members
          // needs to be offset by the number of base classes.
          idx->push_back(i + GetNumberOfNonEmptyBaseClasses(type));
        }
        return field;
      }
    } else if (field.GetType().IsAnonymousType()) {
      // Every member of an anonymous struct is considered to be a member of
      // the enclosing struct or union. This applies recursively if the
      // enclosing struct or union is also anonymous.
      //
      //  struct S {
      //    struct {
      //      int x;
      //    };
      //  } s;
      //
      //  s.x = 1;

      assert(field.GetName() == nullptr && "Field should be unnamed.");

      auto field_in_anon_type =
          GetFieldWithNameIndexPath(field.GetType(), name, idx);
      if (field_in_anon_type) {
        if (idx) {
          idx->push_back(i + GetNumberOfNonEmptyBaseClasses(type));
        }
        return field_in_anon_type;
      }
    }
  }

  // LLDB can't access inherited fields of anonymous struct members.
  if (type.IsAnonymousType()) {
    return lldb::SBTypeMember();
  }

  // Go through the base classes and look for the field there.
  uint32_t num_non_empty_bases = 0;
  uint32_t num_direct_bases = type.GetNumberOfDirectBaseClasses();
  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    lldb::SBType base = type.GetDirectBaseClassAtIndex(i).GetType();
    lldb::SBTypeMember field = GetFieldWithNameIndexPath(base, name, idx);
    if (field) {
      if (idx) {
        idx->push_back(num_non_empty_bases);
      }
      return field;
    }
    if (base.GetNumberOfFields() > 0) {
      num_non_empty_bases += 1;
    }
  }

  return lldb::SBTypeMember();
}

static std::tuple<lldb::SBTypeMember, std::vector<uint32_t>> GetFieldWithName(
    lldb::SBType type, const std::string& name) {
  std::vector<uint32_t> idx;
  lldb::SBTypeMember member = GetFieldWithNameIndexPath(type, name, &idx);
  std::reverse(idx.begin(), idx.end());
  return {member, std::move(idx)};
}

static const char* ToString(TypeDeclaration::TypeSpecifier type_spec) {
  using TypeSpecifier = TypeDeclaration::TypeSpecifier;
  switch (type_spec) {
      // clang-format off
    case TypeSpecifier::kVoid:       return "void";
    case TypeSpecifier::kBool:       return "bool";
    case TypeSpecifier::kChar:       return "char";
    case TypeSpecifier::kShort:      return "short";
    case TypeSpecifier::kInt:        return "int";
    case TypeSpecifier::kLong:       return "long";
    case TypeSpecifier::kLongLong:   return "long long";
    case TypeSpecifier::kFloat:      return "float";
    case TypeSpecifier::kDouble:     return "double";
    case TypeSpecifier::kLongDouble: return "long double";
    case TypeSpecifier::kWChar:      return "wchar_t";
    case TypeSpecifier::kChar16:     return "char16_t";
    case TypeSpecifier::kChar32:     return "char32_t";
      // clang-format on
    default:
      assert(false && "invalid type specifier");
      return nullptr;
  }
}

static const char* ToString(TypeDeclaration::SignSpecifier sign_spec) {
  using SignSpecifier = TypeDeclaration::SignSpecifier;
  switch (sign_spec) {
      // clang-format off
    case SignSpecifier::kSigned:   return "signed";
    case SignSpecifier::kUnsigned: return "unsigned";
      // clang-format on
    default:
      assert(false && "invalid sign specifier");
      return nullptr;
  }
}

static TypeDeclaration::TypeSpecifier ToTypeSpecifier(
    clang::tok::TokenKind kind) {
  using TypeSpecifier = TypeDeclaration::TypeSpecifier;
  switch (kind) {
      // clang-format off
    case clang::tok::kw_void:     return TypeSpecifier::kVoid;
    case clang::tok::kw_bool:     return TypeSpecifier::kBool;
    case clang::tok::kw_char:     return TypeSpecifier::kChar;
    case clang::tok::kw_short:    return TypeSpecifier::kShort;
    case clang::tok::kw_int:      return TypeSpecifier::kInt;
    case clang::tok::kw_long:     return TypeSpecifier::kLong;
    case clang::tok::kw_float:    return TypeSpecifier::kFloat;
    case clang::tok::kw_double:   return TypeSpecifier::kDouble;
    case clang::tok::kw_wchar_t:  return TypeSpecifier::kWChar;
    case clang::tok::kw_char16_t: return TypeSpecifier::kChar16;
    case clang::tok::kw_char32_t: return TypeSpecifier::kChar32;
      // clang-format on
    default:
      assert(false && "invalid type specifier token");
      return TypeSpecifier::kUnknown;
  }
}

lldb::BasicType TypeDeclaration::GetBasicType() const {
  assert(is_builtin_ && "type declaration doesn't describe a builtin type");

  if (sign_specifier_ == SignSpecifier::kSigned &&
      type_specifier_ == TypeSpecifier::kChar) {
    // "signed char" isn't the same as "char".
    return lldb::eBasicTypeSignedChar;
  }

  if (sign_specifier_ == SignSpecifier::kUnsigned) {
    switch (type_specifier_) {
      // clang-format off
      // "unsigned" is "unsigned int"
      case TypeSpecifier::kUnknown:  return lldb::eBasicTypeUnsignedInt;
      case TypeSpecifier::kChar:     return lldb::eBasicTypeUnsignedChar;
      case TypeSpecifier::kShort:    return lldb::eBasicTypeUnsignedShort;
      case TypeSpecifier::kInt:      return lldb::eBasicTypeUnsignedInt;
      case TypeSpecifier::kLong:     return lldb::eBasicTypeUnsignedLong;
      case TypeSpecifier::kLongLong: return lldb::eBasicTypeUnsignedLongLong;
      // clang-format on
      default:
        assert(false && "unknown unsigned basic type");
        return lldb::eBasicTypeInvalid;
    }
  }

  switch (type_specifier_) {
      // clang-format off
    case TypeSpecifier::kUnknown:
      // "signed" is "signed int"
      assert(sign_specifier_ == SignSpecifier::kSigned &&
             "invalid basic type declaration");
      return lldb::eBasicTypeInt;
    case TypeSpecifier::kVoid:       return lldb::eBasicTypeVoid;
    case TypeSpecifier::kBool:       return lldb::eBasicTypeBool;
    case TypeSpecifier::kChar:       return lldb::eBasicTypeChar;
    case TypeSpecifier::kShort:      return lldb::eBasicTypeShort;
    case TypeSpecifier::kInt:        return lldb::eBasicTypeInt;
    case TypeSpecifier::kLong:       return lldb::eBasicTypeLong;
    case TypeSpecifier::kLongLong:   return lldb::eBasicTypeLongLong;
    case TypeSpecifier::kFloat:      return lldb::eBasicTypeFloat;
    case TypeSpecifier::kDouble:     return lldb::eBasicTypeDouble;
    case TypeSpecifier::kLongDouble: return lldb::eBasicTypeLongDouble;
    case TypeSpecifier::kWChar:      return lldb::eBasicTypeWChar;
    case TypeSpecifier::kChar16:     return lldb::eBasicTypeChar16;
    case TypeSpecifier::kChar32:     return lldb::eBasicTypeChar32;
      // clang-format on
  }

  return lldb::eBasicTypeInvalid;
}

static std::unique_ptr<BuiltinFunctionDef> GetBuiltinFunctionDef(
    std::shared_ptr<Context> ctx, const std::string& identifier) {
  //
  // __log2(unsigned int x) -> unsigned int
  //
  //   Calculates the log2(x).
  //
  if (identifier == "__log2") {
    lldb::SBType return_type = ctx->GetBasicType(lldb::eBasicTypeUnsignedInt);
    std::vector<lldb::SBType> arguments = {
        ctx->GetBasicType(lldb::eBasicTypeUnsignedInt),
    };
    return std::make_unique<BuiltinFunctionDef>(identifier, return_type,
                                                std::move(arguments));
  }
  //
  // __findnonnull(T* ptr, long long buffer_size) -> int
  //
  //   Finds the first non-null object pointed by `ptr`. `ptr` is treated as an
  //   array of pointers of size `buffer_size`.
  //
  if (identifier == "__findnonnull") {
    lldb::SBType return_type = ctx->GetBasicType(lldb::eBasicTypeInt);
    std::vector<lldb::SBType> arguments = {
        // The first argument should actually be "T*", but we don't support
        // templates here.
        // HACK: Void means "any" and we'll check in runtime. The argument will
        // be passed as is without any conversions.
        ctx->GetBasicType(lldb::eBasicTypeVoid),
        ctx->GetBasicType(lldb::eBasicTypeLongLong),
    };
    return std::make_unique<BuiltinFunctionDef>(identifier, return_type,
                                                std::move(arguments));
  }
  // Not a builtin function.
  return nullptr;
}

static std::string TypeDescription(lldb::SBType type) {
  const char* name = type.GetName();
  const char* canonical_name = type.GetCanonicalType().GetName();
  if (name == nullptr || canonical_name == nullptr) {
    return "''";  // should not happen
  }
  if (strcmp(name, canonical_name) == 0) {
    return llvm::formatv("'{0}'", name);
  }
  return llvm::formatv("'{0}' (aka '{1}')", name, canonical_name);
}

// Checks whether `target_base` is a virtual base of `type` (direct or
// indirect). If it is, stores the first virtual base type on the path from
// `type` to `target_type`.
static bool IsVirtualBase(lldb::SBType type, lldb::SBType target_base,
                          lldb::SBType* virtual_base,
                          bool carry_virtual = false) {
  if (CompareTypes(type, target_base)) {
    return carry_virtual;
  }

  if (!carry_virtual) {
    uint32_t num_virtual_bases = type.GetNumberOfVirtualBaseClasses();
    for (uint32_t i = 0; i < num_virtual_bases; ++i) {
      lldb::SBType base = type.GetVirtualBaseClassAtIndex(i).GetType();
      if (IsVirtualBase(base, target_base, virtual_base,
                        /*carry_virtual*/ true)) {
        if (virtual_base) {
          *virtual_base = base;
        }
        return true;
      }
    }
  }

  uint32_t num_direct_bases = type.GetNumberOfDirectBaseClasses();
  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    lldb::SBType base = type.GetDirectBaseClassAtIndex(i).GetType();
    if (IsVirtualBase(base, target_base, virtual_base, carry_virtual)) {
      return true;
    }
  }

  return false;
}

// Checks whether `target_base` is a direct or indirect base of `type`.
// If `idx` is provided, it stores the sequence of direct base types from
// `target_base` to `type`. If `offset` is provided, it stores the positive
// offset of the inherited type in bytes.
static bool GetPathToBaseClass(lldb::SBType type, lldb::SBType target_base,
                               std::vector<uint32_t>* idx, uint64_t* offset) {
  if (CompareTypes(type, target_base)) {
    return true;
  }

  uint32_t num_non_empty_bases = 0;
  uint32_t num_direct_bases = type.GetNumberOfDirectBaseClasses();
  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    lldb::SBTypeMember member = type.GetDirectBaseClassAtIndex(i);
    lldb::SBType base = member.GetType();
    if (GetPathToBaseClass(base, target_base, idx, offset)) {
      if (idx) {
        idx->push_back(num_non_empty_bases);
      }
      if (offset) {
        *offset += member.GetOffsetInBytes();
      }
      return true;
    }
    if (base.GetNumberOfFields() > 0) {
      num_non_empty_bases++;
    }
  }

  return false;
}

Parser::Parser(std::shared_ptr<Context> ctx) : ctx_(std::move(ctx)) {
  target_ = ctx_->GetExecutionContext().GetTarget();

  clang::SourceManager& sm = ctx_->GetSourceManager();
  clang::DiagnosticsEngine& de = sm.getDiagnostics();

  auto tOpts = std::make_shared<clang::TargetOptions>();
  tOpts->Triple = llvm::sys::getDefaultTargetTriple();

  ti_.reset(clang::TargetInfo::CreateTargetInfo(de, tOpts));

  lang_opts_ = std::make_unique<clang::LangOptions>();
  lang_opts_->Bool = true;
  lang_opts_->WChar = true;
  lang_opts_->CPlusPlus = true;
  lang_opts_->CPlusPlus11 = true;
  lang_opts_->CPlusPlus14 = true;
  lang_opts_->CPlusPlus17 = true;

  tml_ = std::make_unique<clang::TrivialModuleLoader>();

  auto hOpts = std::make_shared<clang::HeaderSearchOptions>();
  hs_ = std::make_unique<clang::HeaderSearch>(hOpts, sm, de, *lang_opts_,
                                              ti_.get());

  auto pOpts = std::make_shared<clang::PreprocessorOptions>();
  pp_ = std::make_unique<clang::Preprocessor>(pOpts, de, *lang_opts_, sm, *hs_,
                                              *tml_);
  pp_->Initialize(*ti_);
  pp_->EnterMainSourceFile();

  // Initialize the token.
  token_.setKind(clang::tok::unknown);
}

ExprResult Parser::Run(Error& error) {
  ConsumeToken();
  auto expr = ParseExpression();
  Expect(clang::tok::eof);

  error = error_;
  error_.Clear();

  // Explicitly return ErrorNode if there was an error during the parsing. Some
  // routines raise an error, but don't change the return value (e.g. Expect).
  if (error) {
    return std::make_unique<ErrorNode>();
  }
  return expr;
}

std::string Parser::TokenDescription(const clang::Token& token) {
  const auto& spelling = pp_->getSpelling(token);
  const auto* kind_name = token.getName();
  return llvm::formatv("<'{0}' ({1})>", spelling, kind_name);
}

void Parser::Expect(clang::tok::TokenKind kind) {
  if (token_.isNot(kind)) {
    BailOut(ErrorCode::kUnknown,
            llvm::formatv("expected {0}, got: {1}", TokenKindsJoin(kind),
                          TokenDescription(token_)),
            token_.getLocation());
  }
}

template <typename... Ts>
void Parser::ExpectOneOf(clang::tok::TokenKind k, Ts... ks) {
  static_assert((std::is_same_v<Ts, clang::tok::TokenKind> && ...),
                "ExpectOneOf can be only called with values of type "
                "clang::tok::TokenKind");

  if (!token_.isOneOf(k, ks...)) {
    BailOut(ErrorCode::kUnknown,
            llvm::formatv("expected any of ({0}), got: {1}",
                          TokenKindsJoin(k, ks...), TokenDescription(token_)),
            token_.getLocation());
  }
}

void Parser::ConsumeToken() {
  if (token_.is(clang::tok::eof)) {
    // Don't do anything if we're already at eof. This can happen if an error
    // occurred during parsing and we're trying to bail out.
    return;
  }
  pp_->Lex(token_);
}

void Parser::BailOut(ErrorCode code, const std::string& error,
                     clang::SourceLocation loc) {
  if (error_) {
    // If error is already set, then the parser is in the "bail-out" mode. Don't
    // do anything and keep the original error.
    return;
  }

  error_.Set(code, FormatDiagnostics(ctx_->GetSourceManager(), error, loc));
  token_.setKind(clang::tok::eof);
}

// Parse an expression.
//
//  expression:
//    assignment_expression
//
ExprResult Parser::ParseExpression() { return ParseAssignmentExpression(); }

// Parse an assingment_expression.
//
//  assignment_expression:
//    conditional_expression
//    logical_or_expression assignment_operator assignment_expression
//
//  assignment_operator:
//    "="
//    "*="
//    "/="
//    "%="
//    "+="
//    "-="
//    ">>="
//    "<<="
//    "&="
//    "^="
//    "|="
//
//  conditional_expression:
//    logical_or_expression
//    logical_or_expression "?" expression ":" assignment_expression
//
ExprResult Parser::ParseAssignmentExpression() {
  auto lhs = ParseLogicalOrExpression();

  // Check if it's an assingment expression.
  if (token_.isOneOf(clang::tok::equal, clang::tok::starequal,
                     clang::tok::slashequal, clang::tok::percentequal,
                     clang::tok::plusequal, clang::tok::minusequal,
                     clang::tok::greatergreaterequal, clang::tok::lesslessequal,
                     clang::tok::ampequal, clang::tok::caretequal,
                     clang::tok::pipeequal)) {
    // That's an assingment!
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseAssignmentExpression();
    lhs = BuildBinaryOp(clang_token_kind_to_binary_op_kind(token.getKind()),
                        std::move(lhs), std::move(rhs), token.getLocation());
  }

  // Check if it's a conditional expression.
  if (token_.is(clang::tok::question)) {
    clang::Token token = token_;
    ConsumeToken();
    auto true_val = ParseExpression();
    Expect(clang::tok::colon);
    ConsumeToken();
    auto false_val = ParseAssignmentExpression();
    lhs = BuildTernaryOp(std::move(lhs), std::move(true_val),
                         std::move(false_val), token.getLocation());
  }

  return lhs;
}

// Parse a logical_or_expression.
//
//  logical_or_expression:
//    logical_and_expression {"||" logical_and_expression}
//
ExprResult Parser::ParseLogicalOrExpression() {
  auto lhs = ParseLogicalAndExpression();

  while (token_.is(clang::tok::pipepipe)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseLogicalAndExpression();
    lhs = BuildBinaryOp(BinaryOpKind::LOr, std::move(lhs), std::move(rhs),
                        token.getLocation());
  }

  return lhs;
}

// Parse a logical_and_expression.
//
//  logical_and_expression:
//    inclusive_or_expression {"&&" inclusive_or_expression}
//
ExprResult Parser::ParseLogicalAndExpression() {
  auto lhs = ParseInclusiveOrExpression();

  while (token_.is(clang::tok::ampamp)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseInclusiveOrExpression();
    lhs = BuildBinaryOp(BinaryOpKind::LAnd, std::move(lhs), std::move(rhs),
                        token.getLocation());
  }

  return lhs;
}

// Parse an inclusive_or_expression.
//
//  inclusive_or_expression:
//    exclusive_or_expression {"|" exclusive_or_expression}
//
ExprResult Parser::ParseInclusiveOrExpression() {
  auto lhs = ParseExclusiveOrExpression();

  while (token_.is(clang::tok::pipe)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseExclusiveOrExpression();
    lhs = BuildBinaryOp(BinaryOpKind::Or, std::move(lhs), std::move(rhs),
                        token.getLocation());
  }

  return lhs;
}

// Parse an exclusive_or_expression.
//
//  exclusive_or_expression:
//    and_expression {"^" and_expression}
//
ExprResult Parser::ParseExclusiveOrExpression() {
  auto lhs = ParseAndExpression();

  while (token_.is(clang::tok::caret)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseAndExpression();
    lhs = BuildBinaryOp(BinaryOpKind::Xor, std::move(lhs), std::move(rhs),
                        token.getLocation());
  }

  return lhs;
}

// Parse an and_expression.
//
//  and_expression:
//    equality_expression {"&" equality_expression}
//
ExprResult Parser::ParseAndExpression() {
  auto lhs = ParseEqualityExpression();

  while (token_.is(clang::tok::amp)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseEqualityExpression();
    lhs = BuildBinaryOp(BinaryOpKind::And, std::move(lhs), std::move(rhs),
                        token.getLocation());
  }

  return lhs;
}

// Parse an equality_expression.
//
//  equality_expression:
//    relational_expression {"==" relational_expression}
//    relational_expression {"!=" relational_expression}
//
ExprResult Parser::ParseEqualityExpression() {
  auto lhs = ParseRelationalExpression();

  while (token_.isOneOf(clang::tok::equalequal, clang::tok::exclaimequal)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseRelationalExpression();
    lhs = BuildBinaryOp(clang_token_kind_to_binary_op_kind(token.getKind()),
                        std::move(lhs), std::move(rhs), token.getLocation());
  }

  return lhs;
}

// Parse a relational_expression.
//
//  relational_expression:
//    shift_expression {"<" shift_expression}
//    shift_expression {">" shift_expression}
//    shift_expression {"<=" shift_expression}
//    shift_expression {">=" shift_expression}
//
ExprResult Parser::ParseRelationalExpression() {
  auto lhs = ParseShiftExpression();

  while (token_.isOneOf(clang::tok::less, clang::tok::greater,
                        clang::tok::lessequal, clang::tok::greaterequal)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseShiftExpression();
    lhs = BuildBinaryOp(clang_token_kind_to_binary_op_kind(token.getKind()),
                        std::move(lhs), std::move(rhs), token.getLocation());
  }

  return lhs;
}

// Parse a shift_expression.
//
//  shift_expression:
//    additive_expression {"<<" additive_expression}
//    additive_expression {">>" additive_expression}
//
ExprResult Parser::ParseShiftExpression() {
  auto lhs = ParseAdditiveExpression();

  while (token_.isOneOf(clang::tok::lessless, clang::tok::greatergreater)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseAdditiveExpression();
    lhs = BuildBinaryOp(clang_token_kind_to_binary_op_kind(token.getKind()),
                        std::move(lhs), std::move(rhs), token.getLocation());
  }

  return lhs;
}

// Parse an additive_expression.
//
//  additive_expression:
//    multiplicative_expression {"+" multiplicative_expression}
//    multiplicative_expression {"-" multiplicative_expression}
//
ExprResult Parser::ParseAdditiveExpression() {
  auto lhs = ParseMultiplicativeExpression();

  while (token_.isOneOf(clang::tok::plus, clang::tok::minus)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseMultiplicativeExpression();
    lhs = BuildBinaryOp(clang_token_kind_to_binary_op_kind(token.getKind()),
                        std::move(lhs), std::move(rhs), token.getLocation());
  }

  return lhs;
}

// Parse a multiplicative_expression.
//
//  multiplicative_expression:
//    cast_expression {"*" cast_expression}
//    cast_expression {"/" cast_expression}
//    cast_expression {"%" cast_expression}
//
ExprResult Parser::ParseMultiplicativeExpression() {
  auto lhs = ParseCastExpression();

  while (token_.isOneOf(clang::tok::star, clang::tok::slash,
                        clang::tok::percent)) {
    clang::Token token = token_;
    ConsumeToken();
    auto rhs = ParseCastExpression();
    lhs = BuildBinaryOp(clang_token_kind_to_binary_op_kind(token.getKind()),
                        std::move(lhs), std::move(rhs), token.getLocation());
  }

  return lhs;
}

// Parse a cast_expression.
//
//  cast_expression:
//    unary_expression
//    "(" type_id ")" cast_expression
//
ExprResult Parser::ParseCastExpression() {
  // This can be a C-style cast, try parsing the contents as a type declaration.
  if (token_.is(clang::tok::l_paren)) {
    clang::Token token = token_;

    // Enable lexer backtracking, so that we can rollback in case it's not
    // actually a type declaration.
    TentativeParsingAction tentative_parsing(this);

    // Consume the token only after enabling the backtracking.
    ConsumeToken();

    // Try parsing the type declaration. If the returned value is not valid,
    // then we should rollback and try parsing the expression.
    auto type_id = ParseTypeId();
    if (type_id) {
      // Successfully parsed the type declaration. Commit the backtracked
      // tokens and parse the cast_expression.
      tentative_parsing.Commit();

      if (!type_id.value()) {
        return std::make_unique<ErrorNode>();
      }

      Expect(clang::tok::r_paren);
      ConsumeToken();
      auto rhs = ParseCastExpression();

      return BuildCStyleCast(type_id.value(), std::move(rhs),
                             token.getLocation());
    }

    // Failed to parse the contents of the parentheses as a type declaration.
    // Rollback the lexer and try parsing it as unary_expression.
    tentative_parsing.Rollback();
  }

  return ParseUnaryExpression();
}

// Parse an unary_expression.
//
//  unary_expression:
//    postfix_expression
//    "++" cast_expression
//    "--" cast_expression
//    unary_operator cast_expression
//    sizeof unary_expression
//    sizeof "(" type_id ")"
//
//  unary_operator:
//    "&"
//    "*"
//    "+"
//    "-"
//    "~"
//    "!"
//
ExprResult Parser::ParseUnaryExpression() {
  if (token_.isOneOf(clang::tok::plusplus, clang::tok::minusminus,
                     clang::tok::star, clang::tok::amp, clang::tok::plus,
                     clang::tok::minus, clang::tok::exclaim,
                     clang::tok::tilde)) {
    clang::Token token = token_;
    clang::SourceLocation loc = token.getLocation();
    ConsumeToken();
    auto rhs = ParseCastExpression();

    switch (token.getKind()) {
      case clang::tok::plusplus:
        return BuildUnaryOp(UnaryOpKind::PreInc, std::move(rhs), loc);
      case clang::tok::minusminus:
        return BuildUnaryOp(UnaryOpKind::PreDec, std::move(rhs), loc);
      case clang::tok::star:
        return BuildUnaryOp(UnaryOpKind::Deref, std::move(rhs), loc);
      case clang::tok::amp:
        return BuildUnaryOp(UnaryOpKind::AddrOf, std::move(rhs), loc);
      case clang::tok::plus:
        return BuildUnaryOp(UnaryOpKind::Plus, std::move(rhs), loc);
      case clang::tok::minus:
        return BuildUnaryOp(UnaryOpKind::Minus, std::move(rhs), loc);
      case clang::tok::tilde:
        return BuildUnaryOp(UnaryOpKind::Not, std::move(rhs), loc);
      case clang::tok::exclaim:
        return BuildUnaryOp(UnaryOpKind::LNot, std::move(rhs), loc);

      default:
        lldb_eval_unreachable("invalid token kind");
    }
  }

  if (token_.is(clang::tok::kw_sizeof)) {
    clang::SourceLocation sizeof_loc = token_.getLocation();
    ConsumeToken();

    // [expr.sizeof](http://eel.is/c++draft/expr.sizeof#1)
    //
    // The operand is either an expression, which is an unevaluated operand,
    // or a parenthesized type-id.

    // Either operand itself (if it's a type_id), or an operand return type
    // (if it's an expression).
    Type operand;

    // `(` can mean either a type_id or a parenthesized expression.
    if (token_.is(clang::tok::l_paren)) {
      TentativeParsingAction tentative_parsing(this);

      Expect(clang::tok::l_paren);
      ConsumeToken();

      // Parse the type definition and resolve the type.
      auto type_id = ParseTypeId();
      if (type_id) {
        tentative_parsing.Commit();

        // type_id requires parentheses, so there must be a closing one.
        Expect(clang::tok::r_paren);
        ConsumeToken();

        operand = type_id.value();

      } else {
        tentative_parsing.Rollback();

        // Failed to parse type_id, fallback to parsing an unary_expression.
        operand = ParseUnaryExpression()->result_type_deref();
      }

    } else {
      // No opening parenthesis means this must be an unary_expression.
      operand = ParseUnaryExpression()->result_type_deref();
    }
    if (!operand) {
      return std::make_unique<ErrorNode>();
    }

    lldb::SBType result_type = ctx_->GetSizeType();
    return std::make_unique<SizeOfNode>(sizeof_loc, result_type, operand);
  }

  return ParsePostfixExpression();
}

// Parse a postfix_expression.
//
//  postfix_expression:
//    primary_expression
//    postfix_expression "[" expression "]"
//    postfix_expression "." id_expression
//    postfix_expression "->" id_expression
//    postfix_expression "++"
//    postfix_expression "--"
//    static_cast "<" type_id ">" "(" expression ")" ;
//    dynamic_cast "<" type_id ">" "(" expression ")" ;
//    reinterpret_cast "<" type_id ">" "(" expression ")" ;
//
ExprResult Parser::ParsePostfixExpression() {
  // Parse the first part of the postfix_expression. This could be either a
  // primary_expression, or a postfix_expression itself.
  ExprResult lhs;

  // C++-style cast.
  if (token_.isOneOf(clang::tok::kw_static_cast, clang::tok::kw_dynamic_cast,
                     clang::tok::kw_reinterpret_cast)) {
    clang::tok::TokenKind cast_kind = token_.getKind();
    clang::SourceLocation cast_loc = token_.getLocation();
    ConsumeToken();

    Expect(clang::tok::less);
    ConsumeToken();

    clang::SourceLocation loc = token_.getLocation();

    // Parse the type definition and resolve the type.
    auto type_id = ParseTypeId(/*must_be_type_id*/ true);
    if (!type_id) {
      BailOut(ErrorCode::kInvalidOperandType,
              "type name requires a specifier or qualifier", loc);
      return std::make_unique<ErrorNode>();
    }
    if (!type_id.value()) {
      return std::make_unique<ErrorNode>();
    }

    Expect(clang::tok::greater);
    ConsumeToken();

    Expect(clang::tok::l_paren);
    ConsumeToken();
    auto rhs = ParseExpression();
    Expect(clang::tok::r_paren);
    ConsumeToken();

    lhs = BuildCxxCast(cast_kind, type_id.value(), std::move(rhs), cast_loc);

  } else {
    // Otherwise it's a primary_expression.
    lhs = ParsePrimaryExpression();
  }
  assert(lhs && "LHS of the postfix_expression can't be NULL.");

  while (token_.isOneOf(clang::tok::l_square, clang::tok::period,
                        clang::tok::arrow, clang::tok::plusplus,
                        clang::tok::minusminus)) {
    clang::Token token = token_;
    switch (token.getKind()) {
      case clang::tok::period:
      case clang::tok::arrow: {
        ConsumeToken();
        clang::Token member_token = token_;
        auto member_id = ParseIdExpression();
        // Check if this is a function call.
        if (token_.is(clang::tok::l_paren)) {
          // TODO: Check if `member_id` is actually a member function of `lhs`.
          // If not, produce a more accurate diagnostic.
          BailOut(ErrorCode::kNotImplemented,
                  "member function calls are not supported",
                  token_.getLocation());
        }
        lhs = BuildMemberOf(std::move(lhs), std::move(member_id),
                            token.getKind() == clang::tok::arrow,
                            member_token.getLocation());
        break;
      }
      case clang::tok::plusplus: {
        ConsumeToken();
        return BuildUnaryOp(UnaryOpKind::PostInc, std::move(lhs),
                            token.getLocation());
      }
      case clang::tok::minusminus: {
        ConsumeToken();
        return BuildUnaryOp(UnaryOpKind::PostDec, std::move(lhs),
                            token.getLocation());
      }
      case clang::tok::l_square: {
        ConsumeToken();
        auto rhs = ParseExpression();
        Expect(clang::tok::r_square);
        ConsumeToken();
        lhs = BuildBinarySubscript(std::move(lhs), std::move(rhs),
                                   token.getLocation());
        break;
      }

      default:
        lldb_eval_unreachable("invalid token");
    }
  }

  return lhs;
}

// Parse a primary_expression.
//
//  primary_expression:
//    numeric_literal
//    boolean_literal
//    pointer_literal
//    id_expression
//    "this"
//    "(" expression ")"
//    builtin_func
//
ExprResult Parser::ParsePrimaryExpression() {
  if (token_.is(clang::tok::numeric_constant)) {
    return ParseNumericLiteral();
  } else if (token_.isOneOf(clang::tok::kw_true, clang::tok::kw_false)) {
    return ParseBooleanLiteral();
  } else if (token_.is(clang::tok::kw_nullptr)) {
    return ParsePointerLiteral();
  } else if (token_.isOneOf(clang::tok::coloncolon, clang::tok::identifier)) {
    // Save the source location for the diagnostics message.
    clang::SourceLocation loc = token_.getLocation();
    auto identifier = ParseIdExpression();
    // Check if this is a function call.
    if (token_.is(clang::tok::l_paren)) {
      auto func_def = GetBuiltinFunctionDef(ctx_, identifier);
      if (!func_def) {
        BailOut(
            ErrorCode::kNotImplemented,
            llvm::formatv("function '{0}' is not a supported builtin intrinsic",
                          identifier),
            loc);
        return std::make_unique<ErrorNode>();
      }
      return ParseBuiltinFunction(loc, std::move(func_def));
    }
    // Otherwise look for an identifier.
    // TODO: Handle bitfield identifiers when evaluating in the value context.
    auto value = ctx_->LookupIdentifier(identifier);
    if (!value) {
      BailOut(ErrorCode::kUndeclaredIdentifier,
              llvm::formatv("use of undeclared identifier '{0}'", identifier),
              loc);
      return std::make_unique<ErrorNode>();
    }
    return std::make_unique<IdentifierNode>(loc, identifier, Value(value),
                                            /*is_rvalue*/ false,
                                            ctx_->IsContextVar(identifier));
  } else if (token_.is(clang::tok::kw_this)) {
    // Save the source location for the diagnostics message.
    clang::SourceLocation loc = token_.getLocation();
    ConsumeToken();
    auto value = ctx_->LookupIdentifier("this");
    if (!value) {
      BailOut(ErrorCode::kUndeclaredIdentifier,
              "invalid use of 'this' outside of a non-static member function",
              loc);
      return std::make_unique<ErrorNode>();
    }
    // Special case for "this" pointer. As per C++ standard, it's a prvalue.
    return std::make_unique<IdentifierNode>(loc, "this", Value(value),
                                            /*is_rvalue*/ true,
                                            /*is_context_var*/ false);
  } else if (token_.is(clang::tok::l_paren)) {
    ConsumeToken();
    auto expr = ParseExpression();
    Expect(clang::tok::r_paren);
    ConsumeToken();
    return expr;
  }

  BailOut(ErrorCode::kInvalidExpressionSyntax,
          llvm::formatv("Unexpected token: {0}", TokenDescription(token_)),
          token_.getLocation());
  return std::make_unique<ErrorNode>();
}

// Parse a type_id.
//
//  type_id:
//    type_specifier_seq [abstract_declarator]
//
std::optional<lldb::SBType> Parser::ParseTypeId(bool must_be_type_id) {
  clang::SourceLocation type_loc = token_.getLocation();
  TypeDeclaration type_decl;

  // type_specifier_seq is required here, start with trying to parse it.
  ParseTypeSpecifierSeq(&type_decl);

  if (type_decl.IsEmpty()) {
    return {};
  }
  if (type_decl.has_error_) {
    return lldb::SBType();
  }

  // Try to resolve the base type.
  lldb::SBType type;
  if (type_decl.is_builtin_) {
    type = ctx_->GetBasicType(type_decl.GetBasicType());
    assert(type.IsValid() && "cannot resolve basic type");

  } else {
    assert(type_decl.is_user_type_ && "type_decl must be a user type");
    type = ctx_->ResolveTypeByName(type_decl.user_typename_);
    if (!type) {
      if (must_be_type_id) {
        BailOut(
            ErrorCode::kUndeclaredIdentifier,
            llvm::formatv("unknown type name '{0}'", type_decl.user_typename_),
            type_loc);
        return lldb::SBType();
      }
      return {};
    }
  }

  //
  //  abstract_declarator:
  //    ptr_operator [abstract_declarator]
  //
  std::vector<Parser::PtrOperator> ptr_operators;
  while (IsPtrOperator(token_)) {
    ptr_operators.push_back(ParsePtrOperator());
  }
  type = ResolveTypeDeclarators(type, ptr_operators);

  return type;
}

// Parse a type_specifier_seq.
//
//  type_specifier_seq:
//    type_specifier [type_specifier_seq]
//
void Parser::ParseTypeSpecifierSeq(TypeDeclaration* type_decl) {
  while (true) {
    bool type_specifier = ParseTypeSpecifier(type_decl);
    if (!type_specifier) {
      break;
    }
  }
}

// Parse a type_specifier.
//
//  type_specifier:
//    simple_type_specifier
//    cv_qualifier
//
//  simple_type_specifier:
//    ["::"] [nested_name_specifier] type_name
//    "char"
//    "char16_t"
//    "char32_t"
//    "wchar_t"
//    "bool"
//    "short"
//    "int"
//    "long"
//    "signed"
//    "unsigned"
//    "float"
//    "double"
//    "void"
//
// Returns TRUE if a type_specifier was successfully parsed at this location.
//
bool Parser::ParseTypeSpecifier(TypeDeclaration* type_decl) {
  if (IsCvQualifier(token_)) {
    // Just ignore CV quialifiers, we don't use them in type casting.
    ConsumeToken();
    return true;
  }

  if (IsSimpleTypeSpecifierKeyword(token_)) {
    // User-defined typenames can't be combined with builtin keywords.
    if (type_decl->is_user_type_) {
      BailOut(ErrorCode::kInvalidOperandType,
              "cannot combine with previous declaration specifier",
              token_.getLocation());
      type_decl->has_error_ = true;
      return false;
    }

    // From now on this type declaration must describe a builtin type.
    // TODO: Should this be allowed -- `unsigned myint`?
    type_decl->is_builtin_ = true;

    if (!HandleSimpleTypeSpecifier(type_decl)) {
      type_decl->has_error_ = true;
      return false;
    }
    ConsumeToken();
    return true;
  }

  // The type_specifier must be a user-defined type. Try parsing a
  // simple_type_specifier.
  {
    // Try parsing optional global scope operator.
    bool global_scope = false;
    if (token_.is(clang::tok::coloncolon)) {
      global_scope = true;
      ConsumeToken();
    }

    clang::SourceLocation loc = token_.getLocation();

    // Try parsing optional nested_name_specifier.
    auto nested_name_specifier = ParseNestedNameSpecifier();

    // Try parsing required type_name.
    auto type_name = ParseTypeName();

    // If there is a type_name, then this is indeed a simple_type_specifier.
    // Global and qualified (namespace/class) scopes can be empty, since they're
    // optional. In this case type_name is type we're looking for.
    if (!type_name.empty()) {
      // User-defined typenames can't be combined with builtin keywords.
      if (type_decl->is_builtin_) {
        BailOut(ErrorCode::kInvalidOperandType,
                "cannot combine with previous declaration specifier", loc);
        type_decl->has_error_ = true;
        return false;
      }
      // There should be only one user-defined typename.
      if (type_decl->is_user_type_) {
        BailOut(ErrorCode::kInvalidOperandType,
                "two or more data types in declaration of 'type name'", loc);
        type_decl->has_error_ = true;
        return false;
      }

      // Construct the fully qualified typename.
      type_decl->is_user_type_ = true;
      type_decl->user_typename_ =
          llvm::formatv("{0}{1}{2}", global_scope ? "::" : "",
                        nested_name_specifier, type_name);
      return true;
    }
  }

  // No type_specifier was found here.
  return false;
}

// Parse nested_name_specifier.
//
//  nested_name_specifier:
//    type_name "::"
//    namespace_name '::'
//    nested_name_specifier identifier "::"
//    nested_name_specifier simple_template_id "::"
//
std::string Parser::ParseNestedNameSpecifier() {
  // The first token in nested_name_specifier is always an identifier.
  if (token_.isNot(clang::tok::identifier)) {
    return "";
  }

  // If the next token is scope ("::"), then this is indeed a
  // nested_name_specifier
  if (pp_->LookAhead(0).is(clang::tok::coloncolon)) {
    // This nested_name_specifier is a single identifier.
    std::string identifier = pp_->getSpelling(token_);
    ConsumeToken();
    Expect(clang::tok::coloncolon);
    ConsumeToken();
    // Continue parsing the nested_name_specifier.
    return identifier + "::" + ParseNestedNameSpecifier();
  }

  // If the next token starts a template argument list, then we have a
  // simple_template_id here.
  if (pp_->LookAhead(0).is(clang::tok::less)) {
    // We don't know whether this will be a nested_name_identifier or just a
    // type_name. Prepare to rollback if this is not a nested_name_identifier.
    TentativeParsingAction tentative_parsing(this);

    // TODO(werat): Parse just the simple_template_id?
    auto type_name = ParseTypeName();

    // If we did parse the type_name successfully and it's followed by the scope
    // operator ("::"), then this is indeed a nested_name_specifier. Commit the
    // tentative parsing and continue parsing nested_name_specifier.
    if (!type_name.empty() && token_.is(clang::tok::coloncolon)) {
      tentative_parsing.Commit();
      ConsumeToken();
      // Continue parsing the nested_name_specifier.
      return type_name + "::" + ParseNestedNameSpecifier();
    }

    // Not a nested_name_specifier, but could be just a type_name or something
    // else entirely. Rollback the parser and try a different path.
    tentative_parsing.Rollback();
  }

  return "";
}

// Parse a type_name.
//
//  type_name:
//    class_name
//    enum_name
//    typedef_name
//    simple_template_id
//
//  class_name
//    identifier
//
//  enum_name
//    identifier
//
//  typedef_name
//    identifier
//
//  simple_template_id:
//    template_name "<" [template_argument_list] ">"
//
std::string Parser::ParseTypeName() {
  // Typename always starts with an identifier.
  if (token_.isNot(clang::tok::identifier)) {
    return "";
  }

  // If the next token starts a template argument list, parse this type_name as
  // a simple_template_id.
  if (pp_->LookAhead(0).is(clang::tok::less)) {
    // Parse the template_name. In this case it's just an identifier.
    std::string template_name = pp_->getSpelling(token_);
    ConsumeToken();
    // Consume the "<" token.
    ConsumeToken();

    // Short-circuit for missing template_argument_list.
    if (token_.is(clang::tok::greater)) {
      ConsumeToken();
      return llvm::formatv("{0}<>", template_name);
    }

    // Try parsing template_argument_list.
    auto template_argument_list = ParseTemplateArgumentList();

    if (token_.is(clang::tok::greater)) {
      // Single closing angle bracket is a valid end of the template argument
      // list, just consume it.
      ConsumeToken();

    } else if (token_.is(clang::tok::greatergreater)) {
      // C++11 allows using ">>" in nested template argument lists and C++-style
      // casts. In this case we alter change the token type to ">", but don't
      // consume it -- it will be done on the outer level when completing the
      // outer template argument list or C++-style cast.
      token_.setKind(clang::tok::greater);

    } else {
      // Not a valid end of the template argument list, failed to parse a
      // simple_template_id
      return "";
    }

    return llvm::formatv("{0}<{1}>", template_name, template_argument_list);
  }

  // Otherwise look for a class_name, enum_name or a typedef_name.
  std::string identifier = pp_->getSpelling(token_);
  ConsumeToken();

  return identifier;
}

// Parse a template_argument_list.
//
//  template_argument_list:
//    template_argument
//    template_argument_list "," template_argument
//
std::string Parser::ParseTemplateArgumentList() {
  // Parse template arguments one by one.
  std::vector<std::string> arguments;

  do {
    // Eat the comma if this is not the first iteration.
    if (arguments.size() > 0) {
      ConsumeToken();
    }

    // Try parsing a template_argument. If this fails, then this is actually not
    // a template_argument_list.
    auto argument = ParseTemplateArgument();
    if (argument.empty()) {
      return "";
    }

    arguments.push_back(argument);

  } while (token_.is(clang::tok::comma));

  // Internally in LLDB/Clang nested template type names have extra spaces to
  // avoid having ">>". Add the extra space before the closing ">" if the
  // template argument is also a template.
  if (arguments.back().back() == '>') {
    arguments.back().push_back(' ');
  }

  return llvm::formatv("{0:$[, ]}",
                       llvm::make_range(arguments.begin(), arguments.end()));
}

// Parse a template_argument.
//
//  template_argument:
//    type_id
//    numeric_literal
//    id_expression
//
std::string Parser::ParseTemplateArgument() {
  // There is no way to know at this point whether there is going to be a
  // type_id or something else. Try different options one by one.

  {
    // [temp.arg](http://eel.is/c++draft/temp.arg#2)
    //
    // In a template-argument, an ambiguity between a type-id and an expression
    // is resolved to a type-id, regardless of the form of the corresponding
    // template-parameter.

    // Therefore, first try parsing type_id.
    TentativeParsingAction tentative_parsing(this);

    auto type_id = ParseTypeId();
    if (type_id) {
      tentative_parsing.Commit();

      lldb::SBType& type = type_id.value();
      return type ? type.GetName() : "";

    } else {
      // Failed to parse a type_id. Rollback the parser and try something else.
      tentative_parsing.Rollback();
    }
  }

  {
    // The next candidate is a numeric_literal.
    TentativeParsingAction tentative_parsing(this);

    // Parse a numeric_literal.
    if (token_.is(clang::tok::numeric_constant)) {
      // TODO(werat): Actually parse the literal, check if it's valid and
      // canonize it (e.g. 8LL -> 8).
      std::string numeric_literal = pp_->getSpelling(token_);
      ConsumeToken();

      if (TokenEndsTemplateArgumentList(token_)) {
        tentative_parsing.Commit();
        return numeric_literal;
      }
    }

    // Failed to parse a numeric_literal.
    tentative_parsing.Rollback();
  }

  {
    // The next candidate is an id_expression.
    TentativeParsingAction tentative_parsing(this);

    // Parse an id_expression.
    auto id_expression = ParseIdExpression();

    // If we've parsed the id_expression successfully and the next token can
    // finish the template_argument, then we're done here.
    if (!id_expression.empty() && TokenEndsTemplateArgumentList(token_)) {
      tentative_parsing.Commit();
      return id_expression;
    }
    // Failed to parse a id_expression.
    tentative_parsing.Rollback();
  }

  // TODO(b/164399865): Another valid option here is a constant_expression, but
  // we definitely don't want to support constant arithmetic like "Foo<1+2>".
  // We can probably use ParsePrimaryExpression here, but need to figure out the
  // "stringification", since ParsePrimaryExpression returns ExprResult (and
  // potentially a whole expression, not just a single constant.)

  // This is not a template_argument.
  return "";
}

// Parse a ptr_operator.
//
//  ptr_operator:
//    "*" [cv_qualifier_seq]
//    "&"
//
Parser::PtrOperator Parser::ParsePtrOperator() {
  ExpectOneOf(clang::tok::star, clang::tok::amp);

  PtrOperator ptr_operator;
  if (token_.is(clang::tok::star)) {
    ptr_operator = std::make_tuple(clang::tok::star, token_.getLocation());
    ConsumeToken();

    //
    //  cv_qualifier_seq:
    //    cv_qualifier [cv_qualifier_seq]
    //
    //  cv_qualifier:
    //    "const"
    //    "volatile"
    //
    while (IsCvQualifier(token_)) {
      // Just ignore CV quialifiers, we don't use them in type casting.
      ConsumeToken();
    }

  } else if (token_.is(clang::tok::amp)) {
    ptr_operator = std::make_tuple(clang::tok::amp, token_.getLocation());
    ConsumeToken();
  }

  return ptr_operator;
}

lldb::SBType Parser::ResolveTypeDeclarators(
    lldb::SBType type, const std::vector<Parser::PtrOperator>& ptr_operators) {
  // Resolve pointers/references.
  for (auto& [tk, loc] : ptr_operators) {
    if (tk == clang::tok::star) {
      // Pointers to reference types are forbidden.
      if (type.IsReferenceType()) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv("'type name' declared as a pointer to a "
                              "reference of type {0}",
                              TypeDescription(type)),
                loc);
        return lldb::SBType();
      }
      // Get pointer type for the base type: e.g. int* -> int**.
      type = type.GetPointerType();

    } else if (tk == clang::tok::amp) {
      // References to references are forbidden.
      if (type.IsReferenceType()) {
        BailOut(ErrorCode::kInvalidOperandType,
                "type name declared as a reference to a reference", loc);
        return lldb::SBType();
      }
      // Get reference type for the base type: e.g. int -> int&.
      type = type.GetReferenceType();
    }
  }

  return type;
}

bool Parser::IsSimpleTypeSpecifierKeyword(clang::Token token) const {
  return token.isOneOf(
      clang::tok::kw_char, clang::tok::kw_char16_t, clang::tok::kw_char32_t,
      clang::tok::kw_wchar_t, clang::tok::kw_bool, clang::tok::kw_short,
      clang::tok::kw_int, clang::tok::kw_long, clang::tok::kw_signed,
      clang::tok::kw_unsigned, clang::tok::kw_float, clang::tok::kw_double,
      clang::tok::kw_void);
}

bool Parser::IsCvQualifier(clang::Token token) const {
  return token.isOneOf(clang::tok::kw_const, clang::tok::kw_volatile);
}

bool Parser::IsPtrOperator(clang::Token token) const {
  return token.isOneOf(clang::tok::star, clang::tok::amp);
}

bool Parser::HandleSimpleTypeSpecifier(TypeDeclaration* type_decl) {
  using TypeSpecifier = TypeDeclaration::TypeSpecifier;
  using SignSpecifier = TypeDeclaration::SignSpecifier;

  TypeSpecifier type_spec = type_decl->type_specifier_;
  clang::SourceLocation loc = token_.getLocation();
  clang::tok::TokenKind kind = token_.getKind();

  switch (kind) {
    case clang::tok::kw_int: {
      // "int" can have signedness and be combined with "short", "long" and
      // "long long" (but not with another "int").
      if (type_decl->has_int_specifier_) {
        BailOut(ErrorCode::kInvalidOperandType,
                "cannot combine with previous 'int' declaration specifier",
                loc);
        return false;
      }
      if (type_spec == TypeSpecifier::kShort ||
          type_spec == TypeSpecifier::kLong ||
          type_spec == TypeSpecifier::kLongLong) {
        type_decl->has_int_specifier_ = true;
        return true;
      } else if (type_spec == TypeSpecifier::kUnknown) {
        type_decl->type_specifier_ = TypeSpecifier::kInt;
        type_decl->has_int_specifier_ = true;
        return true;
      }
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv(
                  "cannot combine with previous '{0}' declaration specifier",
                  ToString(type_spec)),
              loc);
      return false;
    }

    case clang::tok::kw_long: {
      // "long" can have signedness and be combined with "int" or "long" to
      // form "long long".
      if (type_spec == TypeSpecifier::kUnknown ||
          type_spec == TypeSpecifier::kInt) {
        type_decl->type_specifier_ = TypeSpecifier::kLong;
        return true;
      } else if (type_spec == TypeSpecifier::kLong) {
        type_decl->type_specifier_ = TypeSpecifier::kLongLong;
        return true;
      } else if (type_spec == TypeSpecifier::kDouble) {
        type_decl->type_specifier_ = TypeSpecifier::kLongDouble;
        return true;
      }
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv(
                  "cannot combine with previous '{0}' declaration specifier",
                  ToString(type_spec)),
              loc);
      return false;
    }

    case clang::tok::kw_short: {
      // "short" can have signedness and be combined with "int".
      if (type_spec == TypeSpecifier::kUnknown ||
          type_spec == TypeSpecifier::kInt) {
        type_decl->type_specifier_ = TypeSpecifier::kShort;
        return true;
      }
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv(
                  "cannot combine with previous '{0}' declaration specifier",
                  ToString(type_spec)),
              loc);
      return false;
    }

    case clang::tok::kw_char: {
      // "char" can have signedness, but it cannot be combined with any other
      // type specifier.
      if (type_spec == TypeSpecifier::kUnknown) {
        type_decl->type_specifier_ = TypeSpecifier::kChar;
        return true;
      }
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv(
                  "cannot combine with previous '{0}' declaration specifier",
                  ToString(type_spec)),
              loc);
      return false;
    }

    case clang::tok::kw_double: {
      // "double" can be combined with "long" to form "long double", but it
      // cannot be combined with signedness specifier.
      if (type_decl->sign_specifier_ != SignSpecifier::kUnknown) {
        BailOut(ErrorCode::kInvalidOperandType,
                "'double' cannot be signed or unsigned", loc);
        return false;
      }
      if (type_spec == TypeSpecifier::kUnknown) {
        type_decl->type_specifier_ = TypeSpecifier::kDouble;
        return true;
      } else if (type_spec == TypeSpecifier::kLong) {
        type_decl->type_specifier_ = TypeSpecifier::kLongDouble;
        return true;
      }
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv(
                  "cannot combine with previous '{0}' declaration specifier",
                  ToString(type_spec)),
              loc);
      return false;
    }

    case clang::tok::kw_bool:
    case clang::tok::kw_void:
    case clang::tok::kw_float:
    case clang::tok::kw_wchar_t:
    case clang::tok::kw_char16_t:
    case clang::tok::kw_char32_t: {
      // These types cannot have signedness or be combined with any other type
      // specifiers.
      if (type_decl->sign_specifier_ != SignSpecifier::kUnknown) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv("'{0}' cannot be signed or unsigned",
                              ToString(ToTypeSpecifier(kind))),
                loc);
        return false;
      }
      if (type_spec != TypeSpecifier::kUnknown) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv(
                    "cannot combine with previous '{0}' declaration specifier",
                    ToString(type_spec)),
                loc);
      }
      type_decl->type_specifier_ = ToTypeSpecifier(kind);
      return true;
    }

    case clang::tok::kw_signed:
    case clang::tok::kw_unsigned: {
      // "signed" and "unsigned" cannot be combined with another signedness
      // specifier.
      if (type_decl->sign_specifier_ != SignSpecifier::kUnknown) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv(
                    "cannot combine with previous '{0}' declaration specifier",
                    ToString(type_decl->sign_specifier_)),
                loc);
        return false;
      }
      if (type_spec == TypeSpecifier::kVoid ||
          type_spec == TypeSpecifier::kBool ||
          type_spec == TypeSpecifier::kFloat ||
          type_spec == TypeSpecifier::kDouble ||
          type_spec == TypeSpecifier::kLongDouble ||
          type_spec == TypeSpecifier::kWChar ||
          type_spec == TypeSpecifier::kChar16 ||
          type_spec == TypeSpecifier::kChar32) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv("'{0}' cannot be signed or unsigned",
                              ToString(type_spec)),
                loc);
        return false;
      }

      type_decl->sign_specifier_ = (kind == clang::tok::kw_signed)
                                       ? SignSpecifier::kSigned
                                       : SignSpecifier::kUnsigned;
      return true;
    }

    default:
      assert(false && "invalid simple type specifier kind");
      return false;
  }
}

// Parse an id_expression.
//
//  id_expression:
//    unqualified_id
//    qualified_id
//
//  qualified_id:
//    ["::"] [nested_name_specifier] unqualified_id
//    ["::"] identifier
//
//  identifier:
//    ? clang::tok::identifier ?
//
std::string Parser::ParseIdExpression() {
  // Try parsing optional global scope operator.
  bool global_scope = false;
  if (token_.is(clang::tok::coloncolon)) {
    global_scope = true;
    ConsumeToken();
  }

  // Try parsing optional nested_name_specifier.
  auto nested_name_specifier = ParseNestedNameSpecifier();

  // If nested_name_specifier is present, then it's qualified_id production.
  // Follow the first production rule.
  if (!nested_name_specifier.empty()) {
    // Parse unqualified_id and construct a fully qualified id expression.
    auto unqualified_id = ParseUnqualifiedId();

    return llvm::formatv("{0}{1}{2}", global_scope ? "::" : "",
                         nested_name_specifier, unqualified_id);
  }

  // No nested_name_specifier, but with global scope -- this is also a
  // qualified_id production. Follow the second production rule.
  else if (global_scope) {
    Expect(clang::tok::identifier);
    std::string identifier = pp_->getSpelling(token_);
    ConsumeToken();
    return llvm::formatv("{0}{1}", global_scope ? "::" : "", identifier);
  }

  // This is unqualified_id production.
  return ParseUnqualifiedId();
}

// Parse an unqualified_id.
//
//  unqualified_id:
//    identifier
//
//  identifier:
//    ? clang::tok::identifier ?
//
std::string Parser::ParseUnqualifiedId() {
  Expect(clang::tok::identifier);
  std::string identifier = pp_->getSpelling(token_);
  ConsumeToken();
  return identifier;
}

// Parse a numeric_literal.
//
//  numeric_literal:
//    ? clang::tok::numeric_constant ?
//
ExprResult Parser::ParseNumericLiteral() {
  Expect(clang::tok::numeric_constant);
  ExprResult numeric_constant = ParseNumericConstant(token_);
  ConsumeToken();
  return numeric_constant;
}

// Parse an boolean_literal.
//
//  boolean_literal:
//    "true"
//    "false"
//
ExprResult Parser::ParseBooleanLiteral() {
  ExpectOneOf(clang::tok::kw_true, clang::tok::kw_false);
  clang::SourceLocation loc = token_.getLocation();
  bool literal_value = token_.is(clang::tok::kw_true);
  ConsumeToken();
  return std::make_unique<LiteralNode>(
      loc, CreateValueFromBool(target_, literal_value),
      /*is_literal_zero*/ false);
}

// Parse an pointer_literal.
//
//  pointer_literal:
//    "nullptr"
//
ExprResult Parser::ParsePointerLiteral() {
  Expect(clang::tok::kw_nullptr);
  clang::SourceLocation loc = token_.getLocation();
  ConsumeToken();
  return std::make_unique<LiteralNode>(
      loc,
      CreateValueNullptr(target_, ctx_->GetBasicType(lldb::eBasicTypeNullPtr)),
      /*is_literal_zero*/ false);
}

ExprResult Parser::ParseNumericConstant(clang::Token token) {
  // Parse numeric constant, it can be either integer or float.
  std::string tok_spelling = pp_->getSpelling(token);

  clang::NumericLiteralParser literal(
      tok_spelling, token.getLocation(), pp_->getSourceManager(),
      pp_->getLangOpts(), pp_->getTargetInfo(), pp_->getDiagnostics());

  if (literal.hadError) {
    BailOut(
        ErrorCode::kInvalidNumericLiteral,
        "Failed to parse token as numeric-constant: " + TokenDescription(token),
        token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  // Check for floating-literal and integer-literal. Fail on anything else (i.e.
  // fixed-point literal, who needs them anyway??).
  if (literal.isFloatingLiteral()) {
    return ParseFloatingLiteral(literal, token);
  }
  if (literal.isIntegerLiteral()) {
    return ParseIntegerLiteral(literal, token);
  }

  // Don't care about anything else.
  BailOut(ErrorCode::kInvalidNumericLiteral,
          "numeric-constant should be either float or integer literal: " +
              TokenDescription(token),
          token.getLocation());
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::ParseFloatingLiteral(clang::NumericLiteralParser& literal,
                                        clang::Token token) {
  const llvm::fltSemantics& format = literal.isFloat
                                         ? llvm::APFloat::IEEEsingle()
                                         : llvm::APFloat::IEEEdouble();
  llvm::APFloat raw_value(format);
  llvm::APFloat::opStatus result = literal.GetFloatValue(raw_value);

  // Overflow is always an error, but underflow is only an error if we
  // underflowed to zero (APFloat reports denormals as underflow).
  if ((result & llvm::APFloat::opOverflow) ||
      ((result & llvm::APFloat::opUnderflow) && raw_value.isZero())) {
    BailOut(ErrorCode::kInvalidNumericLiteral,
            llvm::formatv("float underflow/overflow happened: {0}",
                          TokenDescription(token)),
            token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  lldb::BasicType type =
      literal.isFloat ? lldb::eBasicTypeFloat : lldb::eBasicTypeDouble;

  Value value =
      CreateValueFromAPFloat(target_, raw_value, ctx_->GetBasicType(type));

  return std::make_unique<LiteralNode>(token.getLocation(), std::move(value),
                                       /*is_literal_zero*/ false);
}

ExprResult Parser::ParseIntegerLiteral(clang::NumericLiteralParser& literal,
                                       clang::Token token) {
  // Create a value big enough to fit all valid numbers.
  llvm::APInt raw_value(type_width<uintmax_t>(), 0);

  if (literal.GetIntegerValue(raw_value)) {
    BailOut(ErrorCode::kInvalidNumericLiteral,
            llvm::formatv("integer literal is too large to be represented in "
                          "any integer type: {0}",
                          TokenDescription(token)),
            token.getLocation());
    return std::make_unique<ErrorNode>();
  }

  auto [type, is_unsigned] = PickIntegerType(ctx_, literal, raw_value);

  Value value = CreateValueFromAPInt(
      target_, llvm::APSInt(raw_value, is_unsigned), ctx_->GetBasicType(type));

  return std::make_unique<LiteralNode>(
      token.getLocation(), std::move(value),
      /*is_literal_zero*/ raw_value.isNullValue());
}

// Parse a builtin_func.
//
//  builtin_func:
//    builtin_func_name "(" [builtin_func_argument_list] ")"
//
//  builtin_func_name:
//    "__log2"
//
//  builtin_func_argument_list:
//    builtin_func_argument
//    builtin_func_argument_list "," builtin_func_argument
//
//  builtin_func_argument:
//    expression
//
ExprResult Parser::ParseBuiltinFunction(
    clang::SourceLocation loc, std::unique_ptr<BuiltinFunctionDef> func_def) {
  Expect(clang::tok::l_paren);
  ConsumeToken();

  std::vector<ExprResult> arguments;

  if (token_.is(clang::tok::r_paren)) {
    // Empty argument list, nothing to do here.
    ConsumeToken();
  } else {
    // Non-empty argument list, parse all the arguments.
    do {
      // Eat the comma if this is not the first iteration.
      if (arguments.size() > 0) {
        ConsumeToken();
      }

      // Parse a builtin_func_argument. If failed to parse, bail out early and
      // don't try parsing the rest of the arguments.
      auto argument = ParseExpression();
      if (argument->is_error()) {
        return std::make_unique<ErrorNode>();
      }

      arguments.push_back(std::move(argument));

    } while (token_.is(clang::tok::comma));

    Expect(clang::tok::r_paren);
    ConsumeToken();
  }

  // Check we have the correct number of arguments.
  if (arguments.size() != func_def->arguments_.size()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv(
                "no matching function for call to '{0}': requires {1} "
                "argument(s), but {2} argument(s) were provided",
                func_def->name_, func_def->arguments_.size(), arguments.size()),
            loc);
    return std::make_unique<ErrorNode>();
  }

  // Now check that all arguments are correct types and perform implicit
  // conversions if possible.
  for (size_t i = 0; i < arguments.size(); ++i) {
    // HACK: Void means "any" and we'll check in runtime. The argument will be
    // passed as is without any conversions.
    if (func_def->arguments_[i].GetBasicType() == lldb::eBasicTypeVoid) {
      continue;
    }
    arguments[i] = InsertImplicitConversion(std::move(arguments[i]),
                                            func_def->arguments_[i]);
    if (arguments[i]->is_error()) {
      return std::make_unique<ErrorNode>();
    }
  }

  return std::make_unique<BuiltinFunctionCallNode>(
      loc, func_def->return_type_, func_def->name_, std::move(arguments));
}

ExprResult Parser::InsertImplicitConversion(ExprResult expr, Type type) {
  Type expr_type = expr->result_type_deref();

  // If the expression already has the required type, nothing to do here.
  if (CompareTypes(expr_type, type)) {
    return expr;
  }

  // Check if the implicit conversion is possible and insert a cast.
  if (ImplicitConversionIsAllowed(expr_type, type, expr->is_literal_zero())) {
    if (type.IsBasicType()) {
      return std::make_unique<CStyleCastNode>(
          expr->location(), type, std::move(expr), CStyleCastKind::kArithmetic);
    }

    if (type.IsPointerType()) {
      return std::make_unique<CStyleCastNode>(
          expr->location(), type, std::move(expr), CStyleCastKind::kPointer);
    }

    // TODO(werat): What about if the conversion is not `kArithmetic` or
    // `kPointer`?
    lldb_eval_unreachable("invalid implicit cast kind");
  }

  BailOut(ErrorCode::kInvalidOperandType,
          llvm::formatv("no known conversion from {0} to {1}",
                        TypeDescription(expr_type), TypeDescription(type)),
          expr->location());
  return std::make_unique<ErrorNode>();
}

bool Parser::ImplicitConversionIsAllowed(Type src, Type dst,
                                         bool is_src_literal_zero) {
  if (dst.IsInteger() || dst.IsFloat()) {
    // Arithmetic types and enumerations can be implicitly converted to integers
    // and floating point types.
    if (src.IsScalarOrUnscopedEnum() || src.IsScopedEnum()) {
      return true;
    }
  }

  if (dst.IsPointerType()) {
    // Literal zero, `nullptr_t` and arrays can be implicitly converted to
    // pointers.
    if (is_src_literal_zero || src.IsNullPtrType()) {
      return true;
    }
    if (src.IsArrayType() &&
        CompareTypes(src.GetArrayElementType(), dst.GetPointeeType())) {
      return true;
    }
  }

  return false;
}

ExprResult Parser::BuildCStyleCast(Type type, ExprResult rhs,
                                   clang::SourceLocation location) {
  CStyleCastKind kind;
  Type rhs_type = rhs->result_type_deref();

  // Cast to basic type (integer/float).
  if (type.IsScalar()) {
    // Before casting arrays to scalar types, array-to-pointer conversion
    // should be performed.
    if (rhs_type.IsArrayType()) {
      rhs = InsertArrayToPointerConversion(std::move(rhs));
      rhs_type = rhs->result_type_deref();
    }
    // Pointers can be cast to integers of the same or larger size.
    if (rhs_type.IsPointerType() || rhs_type.IsNullPtrType()) {
      // C-style cast from pointer to float/double is not allowed.
      if (type.IsFloat()) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv("C-style cast from {0} to {1} is not allowed",
                              TypeDescription(rhs_type), TypeDescription(type)),
                location);
        return std::make_unique<ErrorNode>();
      }
      // Casting pointer to bool is valid. Otherwise check if the result type
      // is at least as big as the pointer size.
      if (!type.IsBool() && type.GetByteSize() < rhs_type.GetByteSize()) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv(
                    "cast from pointer to smaller type {0} loses information",
                    TypeDescription(type)),
                location);
        return std::make_unique<ErrorNode>();
      }
    } else if (!rhs_type.IsScalar() && !rhs_type.IsEnum()) {
      // Otherwise accept only arithmetic types and enums.
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv(
                  "cannot convert {0} to {1} without a conversion operator",
                  TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
    kind = CStyleCastKind::kArithmetic;

  } else if (type.IsEnum()) {
    // Cast to enum type.
    if (!rhs_type.IsScalar() && !rhs_type.IsEnum()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("C-style cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
    kind = CStyleCastKind::kEnumeration;

  } else if (type.IsPointerType()) {
    // Cast to pointer type.
    if (!rhs_type.IsInteger() && !rhs_type.IsEnum() &&
        !rhs_type.IsArrayType() && !rhs_type.IsPointerType() &&
        !rhs_type.IsNullPtrType()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("cannot cast from type {0} to pointer type {1}",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
    kind = CStyleCastKind::kPointer;

  } else if (type.IsNullPtrType()) {
    // Cast to nullptr type.
    if (!rhs_type.IsNullPtrType() && !rhs->is_literal_zero()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("C-style cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
    kind = CStyleCastKind::kNullptr;

  } else if (type.IsReferenceType()) {
    // Cast to a reference type.
    if (rhs->is_rvalue()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("C-style cast from rvalue to reference type {0}",
                            TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
    kind = CStyleCastKind::kReference;

  } else {
    // Unsupported cast.
    BailOut(ErrorCode::kNotImplemented,
            llvm::formatv("casting of {0} to {1} is not implemented yet",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<CStyleCastNode>(location, type, std::move(rhs), kind);
}

ExprResult Parser::BuildCxxCast(clang::tok::TokenKind kind, Type type,
                                ExprResult rhs,
                                clang::SourceLocation location) {
  assert((kind == clang::tok::kw_static_cast ||
          kind == clang::tok::kw_dynamic_cast ||
          kind == clang::tok::kw_reinterpret_cast) &&
         "invalid C++-style cast type");

  // TODO(werat): Implement custom builders for all C++-style casts.
  if (kind == clang::tok::kw_dynamic_cast) {
    return BuildCxxDynamicCast(type, std::move(rhs), location);
  }
  if (kind == clang::tok::kw_reinterpret_cast) {
    return BuildCxxReinterpretCast(type, std::move(rhs), location);
  }
  if (kind == clang::tok::kw_static_cast) {
    return BuildCxxStaticCast(type, std::move(rhs), location);
  }
  return BuildCStyleCast(type, std::move(rhs), location);
}

ExprResult Parser::BuildCxxStaticCast(Type type, ExprResult rhs,
                                      clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();

  // Perform implicit array-to-pointer conversion.
  if (rhs_type.IsArrayType()) {
    rhs = InsertArrayToPointerConversion(std::move(rhs));
    rhs_type = rhs->result_type_deref();
  }

  if (CompareTypes(rhs_type, type)) {
    return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                               CxxStaticCastKind::kNoOp,
                                               /*is_rvalue*/ true);
  }

  if (type.IsScalar()) {
    return BuildCxxStaticCastToScalar(type, std::move(rhs), location);
  } else if (type.IsEnum()) {
    return BuildCxxStaticCastToEnum(type, std::move(rhs), location);
  } else if (type.IsPointerType()) {
    return BuildCxxStaticCastToPointer(type, std::move(rhs), location);
  } else if (type.IsNullPtrType()) {
    return BuildCxxStaticCastToNullPtr(type, std::move(rhs), location);
  } else if (type.IsReferenceType()) {
    return BuildCxxStaticCastToReference(type, std::move(rhs), location);
  }

  // Unsupported cast.
  BailOut(ErrorCode::kNotImplemented,
          llvm::formatv("casting of {0} to {1} is not implemented yet",
                        TypeDescription(rhs_type), TypeDescription(type)),
          location);
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::BuildCxxStaticCastToScalar(Type type, ExprResult rhs,
                                              clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();

  if (rhs_type.IsPointerType() || rhs_type.IsNullPtrType()) {
    // Pointers can be casted to bools.
    if (!type.IsBool()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("static_cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
  } else if (!rhs_type.IsScalar() && !rhs_type.IsEnum()) {
    // Otherwise accept only arithmetic types and enums.
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv("cannot convert {0} to {1} without a conversion operator",
                      TypeDescription(rhs_type), TypeDescription(type)),
        location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                             CxxStaticCastKind::kArithmetic,
                                             /*is_rvalue*/ true);
}

ExprResult Parser::BuildCxxStaticCastToEnum(Type type, ExprResult rhs,
                                            clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();

  if (!rhs_type.IsScalar() && !rhs_type.IsEnum()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("static_cast from {0} to {1} is not allowed",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                             CxxStaticCastKind::kEnumeration,
                                             /*is_rvalue*/ true);
}

ExprResult Parser::BuildCxxStaticCastToPointer(Type type, ExprResult rhs,
                                               clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();

  if (rhs_type.IsPointerType()) {
    Type type_pointee = type.GetPointeeType();
    Type rhs_type_pointee = rhs_type.GetPointeeType();

    if (type_pointee.IsRecordType() && rhs_type_pointee.IsRecordType()) {
      return BuildCxxStaticCastForInheritedTypes(type, std::move(rhs),
                                                 location);
    }

    if (!type.IsPointerToVoid() && !rhs_type.IsPointerToVoid()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("static_cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }
  } else if (!rhs_type.IsNullPtrType() && !rhs->is_literal_zero()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("cannot cast from type {0} to pointer type '{1}'",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                             CxxStaticCastKind::kPointer,
                                             /*is_rvalue*/ true);
}

ExprResult Parser::BuildCxxStaticCastToNullPtr(Type type, ExprResult rhs,
                                               clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();

  if (!rhs_type.IsNullPtrType() && !rhs->is_literal_zero()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("static_cast from {0} to {1} is not allowed",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                             CxxStaticCastKind::kNullptr,
                                             /*is_rvalue*/ true);
}

ExprResult Parser::BuildCxxStaticCastToReference(
    Type type, ExprResult rhs, clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();
  Type type_deref = type.GetDereferencedType();

  if (rhs->is_rvalue()) {
    BailOut(ErrorCode::kNotImplemented,
            llvm::formatv("static_cast from rvalue of type {0} to reference "
                          "type {1} is not implemented yet",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  if (CompareTypes(type_deref, rhs_type)) {
    return std::make_unique<CxxStaticCastNode>(
        location, type_deref, std::move(rhs), CxxStaticCastKind::kNoOp,
        /*is_rvalue*/ false);
  }

  if (type_deref.IsRecordType() && rhs_type.IsRecordType()) {
    return BuildCxxStaticCastForInheritedTypes(type, std::move(rhs), location);
  }

  BailOut(ErrorCode::kNotImplemented,
          llvm::formatv("static_cast from {0} to {1} is not implemented yet",
                        TypeDescription(rhs_type), TypeDescription(type)),
          location);
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::BuildCxxStaticCastForInheritedTypes(
    Type type, ExprResult rhs, clang::SourceLocation location) {
  assert((type.IsPointerType() || type.IsReferenceType()) &&
         "target type should either be a pointer or a reference");

  Type rhs_type = rhs->result_type_deref();
  Type record_type =
      type.IsPointerType() ? type.GetPointeeType() : type.GetDereferencedType();
  Type rhs_record_type =
      rhs_type.IsPointerType() ? rhs_type.GetPointeeType() : rhs_type;

  assert(record_type.IsRecordType() && rhs_record_type.IsRecordType() &&
         "underlying RHS and target types should be record types");
  assert(!CompareTypes(record_type, rhs_record_type) &&
         "underlying RHS and target types should be different");

  // Result of cast to reference type is an lvalue.
  bool is_rvalue = !type.IsReferenceType();

  // Handle derived-to-base conversion.
  std::vector<uint32_t> idx;
  if (GetPathToBaseClass(rhs_record_type, record_type, &idx,
                         /*offset*/ nullptr)) {
    std::reverse(idx.begin(), idx.end());
    // At this point `idx` represents indices of direct base classes on path
    // from the `rhs` type to the target `type`.
    return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                               std::move(idx), is_rvalue);
  }

  // Handle base-to-derived conversion.
  uint64_t offset = 0;
  if (GetPathToBaseClass(record_type, rhs_record_type, /*idx*/ nullptr,
                         &offset)) {
    lldb::SBType virtual_base;
    if (IsVirtualBase(record_type, rhs_record_type, &virtual_base)) {
      // Base-to-derived conversion isn't possible for virtually inherited
      // types (either directly or indirectly).
      assert(virtual_base.IsValid() && "virtual base should be valid");
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("cannot cast {0} to {1} via virtual base {2}",
                            TypeDescription(rhs_type), TypeDescription(type),
                            TypeDescription(virtual_base)),
              location);
      return std::make_unique<ErrorNode>();
    }

    return std::make_unique<CxxStaticCastNode>(location, type, std::move(rhs),
                                               offset, is_rvalue);
  }

  BailOut(ErrorCode::kInvalidOperandType,
          llvm::formatv("static_cast from {0} to {1}, which are not "
                        "related by inheritance, is not allowed",
                        TypeDescription(rhs_type), TypeDescription(type)),
          location);
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::BuildCxxReinterpretCast(Type type, ExprResult rhs,
                                           clang::SourceLocation location) {
  Type rhs_type = rhs->result_type_deref();
  bool is_rvalue = true;

  if (type.IsScalar()) {
    // reinterpret_cast doesn't support non-integral scalar types.
    if (!type.IsInteger()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("reinterpret_cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }

    // Perform implicit conversions.
    if (rhs_type.IsArrayType()) {
      rhs = InsertArrayToPointerConversion(std::move(rhs));
      rhs_type = rhs->result_type_deref();
    }

    if (rhs_type.IsPointerType() || rhs_type.IsNullPtrType()) {
      // A pointer can be converted to any integral type large enough to hold
      // its value.
      if (type.GetByteSize() < rhs_type.GetByteSize()) {
        BailOut(ErrorCode::kInvalidOperandType,
                llvm::formatv(
                    "cast from pointer to smaller type {0} loses information",
                    TypeDescription(type)),
                location);
        return std::make_unique<ErrorNode>();
      }
    } else if (!CompareTypes(type, rhs_type)) {
      // Integral type can be converted to its own type.
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("reinterpret_cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }

  } else if (type.IsEnum()) {
    // Enumeration type can be converted to its own type.
    if (!CompareTypes(type, rhs_type)) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("reinterpret_cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }

  } else if (type.IsPointerType()) {
    // Integral, enumeration and other pointer types can be converted to any
    // pointer type.
    // TODO: Implement an explicit node for array-to-pointer conversions.
    if (!rhs_type.IsInteger() && !rhs_type.IsEnum() &&
        !rhs_type.IsArrayType() && !rhs_type.IsPointerType()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("reinterpret_cast from {0} to {1} is not allowed",
                            TypeDescription(rhs_type), TypeDescription(type)),
              location);
      return std::make_unique<ErrorNode>();
    }

  } else if (type.IsNullPtrType()) {
    // reinterpret_cast to nullptr_t isn't allowed (even for nullptr_t).
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("reinterpret_cast from {0} to {1} is not allowed",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();

  } else if (type.IsReferenceType()) {
    // L-values can be converted to any reference type.
    if (rhs->is_rvalue()) {
      BailOut(
          ErrorCode::kInvalidOperandType,
          llvm::formatv("reinterpret_cast from rvalue to reference type {0}",
                        TypeDescription(type)),
          location);
      return std::make_unique<ErrorNode>();
    }
    // Casting to reference types gives an L-value result.
    is_rvalue = false;

  } else {
    // Unsupported cast.
    BailOut(ErrorCode::kNotImplemented,
            llvm::formatv("casting of {0} to {1} is not implemented yet",
                          TypeDescription(rhs_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<CxxReinterpretCastNode>(location, type,
                                                  std::move(rhs), is_rvalue);
}

ExprResult Parser::BuildCxxDynamicCast(Type type, ExprResult rhs,
                                       clang::SourceLocation location) {
  Type pointee_type;
  if (type.IsPointerType()) {
    pointee_type = type.GetPointeeType();
  } else if (type.IsReferenceType()) {
    pointee_type = type.GetDereferencedType();
  } else {
    // Dynamic casts are allowed only for pointers and references.
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv("invalid target type {0} for dynamic_cast; target type "
                      "must be a reference or pointer type to a defined class",
                      TypeDescription(type)),
        location);
    return std::make_unique<ErrorNode>();
  }
  // Dynamic casts are allowed only for record types.
  if (!pointee_type.IsRecordType()) {
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv("{0} is not a class type", TypeDescription(pointee_type)),
        location);
    return std::make_unique<ErrorNode>();
  }

  Type expr_type = rhs->result_type();
  if (expr_type.IsPointerType()) {
    expr_type = expr_type.GetPointeeType();
  } else if (expr_type.IsReferenceType()) {
    expr_type = expr_type.GetDereferencedType();
  } else {
    // Expression type must be a pointer or a reference.
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("cannot use dynamic_cast to convert from {0} to {1}",
                          TypeDescription(expr_type), TypeDescription(type)),
            location);
    return std::make_unique<ErrorNode>();
  }
  // Dynamic casts are allowed only for record types.
  if (!expr_type.IsRecordType()) {
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv("{0} is not a class type", TypeDescription(expr_type)),
        location);
    return std::make_unique<ErrorNode>();
  }

  // Expr type must be polymorphic.
  if (!expr_type.IsPolymorphicClass()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("{0} is not polymorphic", TypeDescription(expr_type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  // LLDB doesn't support dynamic_cast in the expression evaluator. We disable
  // it too to match the behaviour, but theoretically it can be implemented.
  BailOut(ErrorCode::kInvalidOperandType,
          "dynamic_cast is not supported in this context", location);
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::BuildUnaryOp(UnaryOpKind kind, ExprResult rhs,
                                clang::SourceLocation location) {
  lldb::SBType result_type;
  Type rhs_type = rhs->result_type_deref();

  switch (kind) {
    case UnaryOpKind::Deref: {
      if (rhs_type.IsPointerType()) {
        result_type = rhs_type.GetPointeeType();
      } else if (rhs_type.IsSmartPtrType()) {
        rhs = InsertSmartPtrToPointerConversion(std::move(rhs));
        result_type = rhs->result_type_deref().GetPointeeType();
      } else if (rhs_type.IsArrayType()) {
        rhs = InsertArrayToPointerConversion(std::move(rhs));
        result_type = rhs->result_type_deref().GetPointeeType();
      } else {
        BailOut(
            ErrorCode::kInvalidOperandType,
            llvm::formatv("indirection requires pointer operand ({0} invalid)",
                          TypeDescription(rhs_type)),
            location);
        return std::make_unique<ErrorNode>();
      }
      break;
    }
    case UnaryOpKind::AddrOf: {
      if (rhs->is_rvalue()) {
        BailOut(
            ErrorCode::kInvalidOperandType,
            llvm::formatv("cannot take the address of an rvalue of type {0}",
                          TypeDescription(rhs_type)),
            location);
        return std::make_unique<ErrorNode>();
      }
      if (rhs->is_bitfield()) {
        BailOut(ErrorCode::kInvalidOperandType,
                "address of bit-field requested", location);
        return std::make_unique<ErrorNode>();
      }
      result_type = rhs_type.GetPointerType();
      break;
    }
    case UnaryOpKind::Plus:
    case UnaryOpKind::Minus: {
      rhs = UsualUnaryConversions(ctx_, std::move(rhs));
      rhs_type = rhs->result_type_deref();
      if (rhs_type.IsScalar() ||
          // Unary plus is allowed for pointers.
          (kind == UnaryOpKind::Plus && rhs_type.IsPointerType())) {
        result_type = rhs->result_type();
      }
      break;
    }
    case UnaryOpKind::Not: {
      rhs = UsualUnaryConversions(ctx_, std::move(rhs));
      rhs_type = rhs->result_type_deref();
      if (rhs_type.IsInteger()) {
        result_type = rhs->result_type();
      }
      break;
    }
    case UnaryOpKind::LNot: {
      if (rhs_type.IsContextuallyConvertibleToBool()) {
        result_type = ctx_->GetBasicType(lldb::eBasicTypeBool);
      }
      break;
    }
    case UnaryOpKind::PostInc:
    case UnaryOpKind::PostDec:
    case UnaryOpKind::PreInc:
    case UnaryOpKind::PreDec: {
      return BuildIncrementDecrement(kind, std::move(rhs), location);
    }

    default:
      lldb_eval_unreachable("invalid unary op kind");
  }

  if (!result_type) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv(kInvalidOperandsToUnaryExpression,
                          TypeDescription(rhs_type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<UnaryOpNode>(location, result_type, kind,
                                       std::move(rhs));
}

ExprResult Parser::BuildIncrementDecrement(UnaryOpKind kind, ExprResult rhs,
                                           clang::SourceLocation location) {
  assert((kind == UnaryOpKind::PreInc || kind == UnaryOpKind::PreDec ||
          kind == UnaryOpKind::PostInc || kind == UnaryOpKind::PostDec) &&
         "illegal unary op kind, expected inc/dec");

  Type rhs_type = rhs->result_type_deref();

  // In C++ the requirement here is that the expression is "assignable". However
  // in the debugger context side-effects are not allowed and the only case
  // where increment/decrement are permitted is when modifying the "context
  // variable".
  // Technically, `++(++$var)` could be allowed too, since both increments
  // modify the context variable. However, MSVC debugger doesn't allow it, so we
  // don't implement it too.
  if (rhs->is_rvalue()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("expression is not assignable"), location);
    return std::make_unique<ErrorNode>();
  }
  if (!rhs->is_context_var() && !ctx_->AllowSideEffects()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("side effects are not supported in this context: "
                          "trying to modify data at the target process"),
            location);
    return std::make_unique<ErrorNode>();
  }
  llvm::StringRef op_name =
      (kind == UnaryOpKind::PreInc || kind == UnaryOpKind::PostInc)
          ? "increment"
          : "decrement";
  if (rhs_type.IsEnum()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("cannot {0} expression of enum type '{1}'", op_name,
                          rhs_type.GetName()),
            location);
    return std::make_unique<ErrorNode>();
  }
  if (!rhs_type.IsScalar() && !rhs_type.IsPointerType()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("cannot {0} value of type '{1}'", op_name,
                          rhs_type.GetName()),
            location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<UnaryOpNode>(location, rhs->result_type(), kind,
                                       std::move(rhs));
}

ExprResult Parser::BuildBinaryOp(BinaryOpKind kind, ExprResult lhs,
                                 ExprResult rhs,
                                 clang::SourceLocation location) {
  // TODO(werat): Get the "original" type (i.e. the one before implicit casts)
  // from the ExprResult.
  Type orig_lhs_type = lhs->result_type_deref();
  Type orig_rhs_type = rhs->result_type_deref();

  // Result type of the binary expression. For example, for `char + int` the
  // result type is `int`, but for `char += int` the result type is `char`.
  lldb::SBType result_type;

  // In case binary operation is a composite assignment, the type of the binary
  // expression _before_ the assignment. For example, for `char += int`
  // composite assignment type is `int`, because `char + int` is promoted to
  // `int + int`.
  lldb::SBType comp_assign_type;

  switch (kind) {
    case BinaryOpKind::Add:
      result_type =
          PrepareBinaryAddition(lhs, rhs, location, /*is_comp_assign*/ false);
      break;

    case BinaryOpKind::Sub:
      result_type = PrepareBinarySubtraction(lhs, rhs, location,
                                             /*is_comp_assign*/ false);
      break;

    case BinaryOpKind::Mul:
    case BinaryOpKind::Div:
      result_type = PrepareBinaryMulDiv(lhs, rhs,
                                        /*is_comp_assign*/ false);
      break;

    case BinaryOpKind::Rem:
      result_type = PrepareBinaryRemainder(lhs, rhs, /*is_comp_assign*/ false);
      break;

    case BinaryOpKind::And:
    case BinaryOpKind::Or:
    case BinaryOpKind::Xor:
      result_type = PrepareBinaryBitwise(lhs, rhs,
                                         /*is_comp_assign*/ false);
      break;
    case BinaryOpKind::Shl:
    case BinaryOpKind::Shr:
      result_type = PrepareBinaryShift(lhs, rhs,
                                       /*is_comp_assign*/ false);
      break;

    case BinaryOpKind::EQ:
    case BinaryOpKind::NE:
    case BinaryOpKind::LT:
    case BinaryOpKind::LE:
    case BinaryOpKind::GT:
    case BinaryOpKind::GE:
      result_type = PrepareBinaryComparison(kind, lhs, rhs, location);
      break;

    case BinaryOpKind::LAnd:
    case BinaryOpKind::LOr:
      result_type = PrepareBinaryLogical(lhs, rhs);
      break;

    case BinaryOpKind::Assign:
      // For plain assignment try to implicitly convert RHS to the type of LHS.
      // Later we'll check if the assignment is actually possible.
      rhs = InsertImplicitConversion(std::move(rhs), lhs->result_type_deref());
      // Shortcut for the case when the implicit conversion is not possible.
      if (rhs->is_error()) {
        return std::make_unique<ErrorNode>();
      }
      comp_assign_type = rhs->result_type_deref();
      break;

    case BinaryOpKind::AddAssign:
      comp_assign_type = PrepareBinaryAddition(lhs, rhs, location,
                                               /*is_comp_assign*/ true);
      break;

    case BinaryOpKind::SubAssign:
      comp_assign_type = PrepareBinarySubtraction(lhs, rhs, location,
                                                  /*is_comp_assign*/ true);
      break;

    case BinaryOpKind::MulAssign:
    case BinaryOpKind::DivAssign:
      comp_assign_type = PrepareBinaryMulDiv(lhs, rhs,
                                             /*is_comp_assign*/ true);
      break;

    case BinaryOpKind::RemAssign:
      comp_assign_type =
          PrepareBinaryRemainder(lhs, rhs, /*is_comp_assign*/ true);
      break;

    case BinaryOpKind::AndAssign:
    case BinaryOpKind::OrAssign:
    case BinaryOpKind::XorAssign:
      comp_assign_type = PrepareBinaryBitwise(lhs, rhs,
                                              /*is_comp_assign*/ true);
      break;
    case BinaryOpKind::ShlAssign:
    case BinaryOpKind::ShrAssign:
      comp_assign_type = PrepareBinaryShift(lhs, rhs,
                                            /*is_comp_assign*/ true);
      break;

    default:
      lldb_eval_unreachable("invalid binary op kind");
  }

  // If we're building a composite assignment, check for composite assignments
  // constraints: if the LHS is assignable, if the type of the binary operation
  // can be assigned to it, etc.
  if (comp_assign_type) {
    result_type = PrepareCompositeAssignment(comp_assign_type, lhs, location);
  }

  // If the result type is valid, then the binary operation is valid!
  if (result_type) {
    return std::make_unique<BinaryOpNode>(location, result_type, kind,
                                          std::move(lhs), std::move(rhs),
                                          comp_assign_type);
  }

  BailOut(ErrorCode::kInvalidOperandType,
          llvm::formatv(kInvalidOperandsToBinaryExpression,
                        TypeDescription(orig_lhs_type),
                        TypeDescription(orig_rhs_type)),
          location);
  return std::make_unique<ErrorNode>();
}

lldb::SBType Parser::PrepareBinaryAddition(ExprResult& lhs, ExprResult& rhs,
                                           clang::SourceLocation location,
                                           bool is_comp_assign) {
  // Operation '+' works for:
  //
  //  {scalar,unscoped_enum} <-> {scalar,unscoped_enum}
  //  {integer,unscoped_enum} <-> pointer
  //  pointer <-> {integer,unscoped_enum}

  Type result_type = UsualArithmeticConversions(ctx_, lhs, rhs, is_comp_assign);

  if (result_type.IsScalar()) {
    return result_type;
  }

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  // Check for pointer arithmetic operation.
  Type ptr_type, integer_type;

  if (lhs_type.IsPointerType()) {
    ptr_type = lhs_type;
    integer_type = rhs_type;
  } else if (rhs_type.IsPointerType()) {
    integer_type = lhs_type;
    ptr_type = rhs_type;
  }

  if (!ptr_type || !integer_type.IsInteger()) {
    // Invalid operands.
    return kInvalidType;
  }

  if (ptr_type.IsPointerToVoid()) {
    BailOut(ErrorCode::kInvalidOperandType, "arithmetic on a pointer to void",
            location);
    return kInvalidType;
  }

  return ptr_type;
}

lldb::SBType Parser::PrepareBinarySubtraction(ExprResult& lhs, ExprResult& rhs,
                                              clang::SourceLocation location,
                                              bool is_comp_assign) {
  // Operation '-' works for:
  //
  //  {scalar,unscoped_enum} <-> {scalar,unscoped_enum}
  //  pointer <-> {integer,unscoped_enum}
  //  pointer <-> pointer (if pointee types are compatible)

  Type result_type = UsualArithmeticConversions(ctx_, lhs, rhs, is_comp_assign);

  if (result_type.IsScalar()) {
    return result_type;
  }

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  if (lhs_type.IsPointerType() && rhs_type.IsInteger()) {
    if (lhs_type.IsPointerToVoid()) {
      BailOut(ErrorCode::kInvalidOperandType, "arithmetic on a pointer to void",
              location);
      return kInvalidType;
    }

    return lhs_type;
  }

  if (lhs_type.IsPointerType() && rhs_type.IsPointerType()) {
    if (lhs_type.IsPointerToVoid() && rhs_type.IsPointerToVoid()) {
      BailOut(ErrorCode::kInvalidOperandType, "arithmetic on pointers to void",
              location);
      return kInvalidType;
    }

    // Compare canonical unqualified pointer types.
    bool comparable =
        CompareTypes(lhs_type.GetCanonicalType().GetUnqualifiedType(),
                     rhs_type.GetCanonicalType().GetUnqualifiedType());

    if (!comparable) {
      BailOut(
          ErrorCode::kInvalidOperandType,
          llvm::formatv("{0} and {1} are not pointers to compatible types",
                        TypeDescription(lhs_type), TypeDescription(rhs_type)),
          location);
      return kInvalidType;
    }

    // Pointer difference is ptrdiff_t.
    return ctx_->GetPtrDiffType();
  }

  // Invalid operands.
  return kInvalidType;
}

lldb::SBType Parser::PrepareBinaryMulDiv(ExprResult& lhs, ExprResult& rhs,
                                         bool is_comp_assign) {
  // Operations {'*', '/'} work for:
  //
  //  {scalar,unscoped_enum} <-> {scalar,unscoped_enum}
  //

  Type result_type = UsualArithmeticConversions(ctx_, lhs, rhs, is_comp_assign);

  // TODO(werat): Check for arithmetic zero division.
  if (result_type.IsScalar()) {
    return result_type;
  }

  // Invalid operands.
  return kInvalidType;
}

lldb::SBType Parser::PrepareBinaryRemainder(ExprResult& lhs, ExprResult& rhs,
                                            bool is_comp_assign) {
  // Operation '%' works for:
  //
  //  {integer,unscoped_enum} <-> {integer,unscoped_enum}

  Type result_type = UsualArithmeticConversions(ctx_, lhs, rhs, is_comp_assign);

  // TODO(werat): Check for arithmetic zero division.
  if (result_type.IsInteger()) {
    return result_type;
  }

  // Invalid operands.
  return kInvalidType;
}

lldb::SBType Parser::PrepareBinaryBitwise(ExprResult& lhs, ExprResult& rhs,
                                          bool is_comp_assign) {
  // Operations {'&', '|', '^'} work for:
  //
  //  {integer,unscoped_enum} <-> {integer,unscoped_enum}
  //
  // Note that {'<<', '>>'} are handled in a separate method.

  Type result_type = UsualArithmeticConversions(ctx_, lhs, rhs, is_comp_assign);

  if (result_type.IsInteger()) {
    return result_type;
  }

  // Invalid operands.
  return kInvalidType;
}

lldb::SBType Parser::PrepareBinaryShift(ExprResult& lhs, ExprResult& rhs,
                                        bool is_comp_assign) {
  // Operations {'<<', '>>'} work for:
  //
  //  {integer,unscoped_enum} <-> {integer,unscoped_enum}

  if (!is_comp_assign) {
    lhs = UsualUnaryConversions(ctx_, std::move(lhs));
  }
  rhs = UsualUnaryConversions(ctx_, std::move(rhs));

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  if (!lhs_type.IsInteger() || !rhs_type.IsInteger()) {
    return kInvalidType;
  }

  // The type of the result is that of the promoted left operand.
  return DoIntegralPromotion(ctx_, lhs_type);
}

lldb::SBType Parser::PrepareBinaryComparison(BinaryOpKind kind, ExprResult& lhs,
                                             ExprResult& rhs,
                                             clang::SourceLocation location) {
  // Comparison works for:
  //
  //  {scalar,unscoped_enum} <-> {scalar,unscoped_enum}
  //  scoped_enum <-> scoped_enum (if the same type)
  //  pointer <-> pointer (if pointee types are compatible)
  //  pointer <-> {integer,unscoped_enum,nullptr_t}
  //  {integer,unscoped_enum,nullptr_t} <-> pointer
  //  nullptr_t <-> {nullptr_t,integer} (if integer is literal zero)
  //  {nullptr_t,integer} <-> nullptr_t (if integer is literal zero)

  // If the operands has arithmetic or enumeration type (scoped or unscoped),
  // usual arithmetic conversions are performed on both operands following the
  // rules for arithmetic operators.
  Type _ = UsualArithmeticConversions(ctx_, lhs, rhs);

  // Apply smart-pointer-to-pointer conversions.
  if (Type(lhs->result_type_deref()).IsSmartPtrType()) {
    lhs = InsertSmartPtrToPointerConversion(std::move(lhs));
  }
  if (Type(rhs->result_type_deref()).IsSmartPtrType()) {
    rhs = InsertSmartPtrToPointerConversion(std::move(rhs));
  }

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  // The result of the comparison is always bool.
  lldb::SBType boolean_ty = ctx_->GetBasicType(lldb::eBasicTypeBool);

  if (lhs_type.IsScalarOrUnscopedEnum() && rhs_type.IsScalarOrUnscopedEnum()) {
    return boolean_ty;
  }

  // Scoped enums can be compared only to the instances of the same type.
  if (lhs_type.IsScopedEnum() || rhs_type.IsScopedEnum()) {
    if (CompareTypes(lhs_type, rhs_type)) {
      return boolean_ty;
    }
    // Invalid operands.
    return kInvalidType;
  }

  bool is_ordered = (kind == BinaryOpKind::LT || kind == BinaryOpKind::LE ||
                     kind == BinaryOpKind::GT || kind == BinaryOpKind::GE);

  // Check if the value can be compared to a pointer. We allow all pointers,
  // integers, unscoped enumerations and a nullptr literal if it's an
  // equality/inequality comparison. For "pointer <-> integer" C++ allows only
  // equality/inequality comparison against literal zero and nullptr. However in
  // the debugger context it's often useful to compare a pointer with an integer
  // representing an address. That said, this also allows comparing nullptr and
  // any integer, not just literal zero, e.g. "nullptr == 1 -> false". C++
  // doesn't allow it, but we implement this for convenience.
  auto comparable_to_pointer = [&](Type t) {
    return t.IsPointerType() || t.IsInteger() || t.IsUnscopedEnum() ||
           (!is_ordered && t.IsNullPtrType());
  };

  if ((lhs_type.IsPointerType() && comparable_to_pointer(rhs_type)) ||
      (comparable_to_pointer(lhs_type) && rhs_type.IsPointerType())) {
    // If both are pointers, check if they have comparable types. Comparing
    // pointers to void is always allowed.
    if ((lhs_type.IsPointerType() && !lhs_type.IsPointerToVoid()) &&
        (rhs_type.IsPointerType() && !rhs_type.IsPointerToVoid())) {
      // Compare canonical unqualified pointer types.
      bool comparable =
          CompareTypes(lhs_type.GetCanonicalType().GetUnqualifiedType(),
                       rhs_type.GetCanonicalType().GetUnqualifiedType());

      if (!comparable) {
        BailOut(
            ErrorCode::kInvalidOperandType,
            llvm::formatv("comparison of distinct pointer types ({0} and {1})",
                          TypeDescription(lhs_type), TypeDescription(rhs_type)),
            location);
        return kInvalidType;
      }
    }

    return boolean_ty;
  }

  bool lhs_nullptr_or_zero = lhs_type.IsNullPtrType() || lhs->is_literal_zero();
  bool rhs_nullptr_or_zero = rhs_type.IsNullPtrType() || rhs->is_literal_zero();

  if (!is_ordered && ((lhs_type.IsNullPtrType() && rhs_nullptr_or_zero) ||
                      (lhs_nullptr_or_zero && rhs_type.IsNullPtrType()))) {
    return boolean_ty;
  }

  // Invalid operands.
  return kInvalidType;
}

lldb::SBType Parser::PrepareBinaryLogical(const ExprResult& lhs,
                                          const ExprResult& rhs) {
  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  if (!lhs_type.IsContextuallyConvertibleToBool()) {
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv(kValueIsNotConvertibleToBool, TypeDescription(lhs_type)),
        lhs->location());
    return kInvalidType;
  }

  if (!rhs_type.IsContextuallyConvertibleToBool()) {
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv(kValueIsNotConvertibleToBool, TypeDescription(rhs_type)),
        rhs->location());
    return kInvalidType;
  }

  // The result of the logical operator is always bool.
  return ctx_->GetBasicType(lldb::eBasicTypeBool);
}

ExprResult Parser::BuildBinarySubscript(ExprResult lhs, ExprResult rhs,
                                        clang::SourceLocation location) {
  // C99 6.5.2.1p2: the expression e1[e2] is by definition precisely
  // equivalent to the expression *((e1)+(e2)).
  // We need to figure out which expression is "base" and which is "index".

  ExprResult base;
  ExprResult index;

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  if (lhs_type.IsArrayType()) {
    base = InsertArrayToPointerConversion(std::move(lhs));
    index = std::move(rhs);
  } else if (lhs_type.IsPointerType()) {
    base = std::move(lhs);
    index = std::move(rhs);
  } else if (rhs_type.IsArrayType()) {
    base = InsertArrayToPointerConversion(std::move(rhs));
    index = std::move(lhs);
  } else if (rhs_type.IsPointerType()) {
    base = std::move(rhs);
    index = std::move(lhs);
  } else {
    BailOut(ErrorCode::kInvalidOperandType,
            "subscripted value is not an array or pointer", location);
    return std::make_unique<ErrorNode>();
  }

  // Index can be a typedef of a typedef of a typedef of a typedef...
  // Get canonical underlying type.
  Type index_type = index->result_type_deref();

  // Check if the index is of an integral type.
  if (!index_type.IsIntegerOrUnscopedEnum()) {
    BailOut(ErrorCode::kInvalidOperandType, "array subscript is not an integer",
            location);
    return std::make_unique<ErrorNode>();
  }

  Type base_type = base->result_type_deref();
  if (base_type.IsPointerToVoid()) {
    BailOut(ErrorCode::kInvalidOperandType,
            "subscript of pointer to incomplete type 'void'", location);
    return std::make_unique<ErrorNode>();
  }

  return std::make_unique<ArraySubscriptNode>(
      location, base->result_type_deref().GetPointeeType(), std::move(base),
      std::move(index));
}

lldb::SBType Parser::PrepareCompositeAssignment(
    Type comp_assign_type, const ExprResult& lhs,
    clang::SourceLocation location) {
  // In C++ the requirement here is that the expression is "assignable".
  // However in the debugger context side-effects are not allowed and the only
  // case where composite assignments are permitted is when modifying the
  // "context variable".
  // Technically, `($var += 1) += 1` could be allowed too, since both
  // operations modify the context variable. However, MSVC debugger doesn't
  // allow it, so we don't implement it too.
  if (lhs->is_rvalue()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("expression is not assignable"), location);
    return kInvalidType;
  }
  if (!lhs->is_context_var() && !ctx_->AllowSideEffects()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("side effects are not supported in this context: "
                          "trying to modify data at the target process"),
            location);
    return kInvalidType;
  }

  // Check if we can assign the result of the binary operation back to LHS.
  Type lhs_type = lhs->result_type_deref();

  if (CompareTypes(comp_assign_type, lhs_type) ||
      ImplicitConversionIsAllowed(comp_assign_type, lhs_type)) {
    return lhs_type;
  }

  BailOut(ErrorCode::kInvalidOperandType,
          llvm::formatv("no known conversion from {0} to {1}",
                        TypeDescription(comp_assign_type),
                        TypeDescription(lhs_type)),
          location);
  return kInvalidType;
}

ExprResult Parser::BuildTernaryOp(ExprResult cond, ExprResult lhs,
                                  ExprResult rhs,
                                  clang::SourceLocation location) {
  // First check if the condition contextually converted to bool.
  Type cond_type = cond->result_type_deref();
  if (!cond_type.IsContextuallyConvertibleToBool()) {
    BailOut(
        ErrorCode::kInvalidOperandType,
        llvm::formatv(kValueIsNotConvertibleToBool, TypeDescription(cond_type)),
        location);
    return std::make_unique<ErrorNode>();
  }

  Type lhs_type = lhs->result_type_deref();
  Type rhs_type = rhs->result_type_deref();

  // If operands have the same type, don't do any promotions.
  if (CompareTypes(lhs_type, rhs_type)) {
    return std::make_unique<TernaryOpNode>(location, lhs_type, std::move(cond),
                                           std::move(lhs), std::move(rhs));
  }

  // If both operands have arithmetic type, apply the usual arithmetic
  // conversions to bring them to a common type.
  if (lhs_type.IsScalarOrUnscopedEnum() && rhs_type.IsScalarOrUnscopedEnum()) {
    lldb::SBType result_type = UsualArithmeticConversions(ctx_, lhs, rhs);
    return std::make_unique<TernaryOpNode>(
        location, result_type, std::move(cond), std::move(lhs), std::move(rhs));
  }

  // Apply array-to-pointer implicit conversions.
  if (lhs_type.IsArrayType()) {
    lhs = InsertArrayToPointerConversion(std::move(lhs));
    lhs_type = lhs->result_type_deref();
  }
  if (rhs_type.IsArrayType()) {
    rhs = InsertArrayToPointerConversion(std::move(rhs));
    rhs_type = rhs->result_type_deref();
  }

  // Check if operands have the same pointer type.
  if (CompareTypes(lhs_type, rhs_type)) {
    return std::make_unique<TernaryOpNode>(location, lhs_type, std::move(cond),
                                           std::move(lhs), std::move(rhs));
  }

  // If one operand is a pointer and the other is a nullptr or literal zero,
  // convert the nullptr operand to pointer type.
  if (lhs_type.IsPointerType() &&
      (rhs->is_literal_zero() || rhs_type.IsNullPtrType())) {
    rhs = std::make_unique<CStyleCastNode>(
        rhs->location(), lhs_type, std::move(rhs), CStyleCastKind::kPointer);

    return std::make_unique<TernaryOpNode>(location, lhs_type, std::move(cond),
                                           std::move(lhs), std::move(rhs));
  }
  if ((lhs->is_literal_zero() || lhs_type.IsNullPtrType()) &&
      rhs_type.IsPointerType()) {
    lhs = std::make_unique<CStyleCastNode>(
        lhs->location(), rhs_type, std::move(lhs), CStyleCastKind::kPointer);

    return std::make_unique<TernaryOpNode>(location, rhs_type, std::move(cond),
                                           std::move(lhs), std::move(rhs));
  }

  // If one operand is nullptr and the other one is literal zero, convert
  // the literal zero to a nullptr type.
  if (lhs_type.IsNullPtrType() && rhs->is_literal_zero()) {
    rhs = std::make_unique<CStyleCastNode>(
        rhs->location(), lhs_type, std::move(rhs), CStyleCastKind::kNullptr);

    return std::make_unique<TernaryOpNode>(location, lhs_type, std::move(cond),
                                           std::move(lhs), std::move(rhs));
  }
  if (lhs->is_literal_zero() && rhs_type.IsNullPtrType()) {
    lhs = std::make_unique<CStyleCastNode>(
        lhs->location(), rhs_type, std::move(lhs), CStyleCastKind::kNullptr);

    return std::make_unique<TernaryOpNode>(location, rhs_type, std::move(cond),
                                           std::move(lhs), std::move(rhs));
  }

  BailOut(ErrorCode::kInvalidOperandType,
          llvm::formatv("incompatible operand types ({0} and {1})",
                        TypeDescription(lhs_type), TypeDescription(rhs_type)),
          location);
  return std::make_unique<ErrorNode>();
}

ExprResult Parser::BuildMemberOf(ExprResult lhs, std::string member_id,
                                 bool is_arrow,
                                 clang::SourceLocation location) {
  Type lhs_type = lhs->result_type_deref();

  if (is_arrow) {
    // "member of pointer" operator, check that LHS is a pointer and
    // dereference it.
    if (!lhs_type.IsPointerType() && !lhs_type.IsSmartPtrType() &&
        !lhs_type.IsArrayType()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("member reference type {0} is not a pointer; did "
                            "you mean to use '.'?",
                            TypeDescription(lhs_type)),
              location);
      return std::make_unique<ErrorNode>();
    }

    if (lhs_type.IsSmartPtrType()) {
      // If LHS is a smart pointer, decay it to an underlying object.
      lhs = InsertSmartPtrToPointerConversion(std::move(lhs));
      lhs_type = lhs->result_type_deref();
    } else if (lhs_type.IsArrayType()) {
      // If LHS is an array, convert it to pointer.
      lhs = InsertArrayToPointerConversion(std::move(lhs));
      lhs_type = lhs->result_type_deref();
    }

    lhs_type = lhs_type.GetPointeeType();
  } else {
    // "member of object" operator, check that LHS is an object.
    if (lhs_type.IsPointerType()) {
      BailOut(ErrorCode::kInvalidOperandType,
              llvm::formatv("member reference type {0} is a pointer; "
                            "did you mean to use '->'?",
                            TypeDescription(lhs_type)),
              location);
      return std::make_unique<ErrorNode>();
    }
  }

  // Check if LHS is a record type, i.e. class/struct or union.
  if (!lhs_type.IsRecordType()) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv(
                "member reference base type {0} is not a structure or union",
                TypeDescription(lhs_type)),
            location);
    return std::make_unique<ErrorNode>();
  }

  auto [member, idx] = GetFieldWithName(lhs_type, member_id);
  if (!member) {
    BailOut(ErrorCode::kInvalidOperandType,
            llvm::formatv("no member named '{0}' in {1}", member_id,
                          TypeDescription(lhs_type.GetUnqualifiedType())),
            location);
    return std::make_unique<ErrorNode>();
  }

  uint32_t bitfield_size =
      member.IsBitfield() ? member.GetBitfieldSizeInBits() : 0;
  if (bitfield_size > member.GetType().GetByteSize() * CHAR_BIT) {
    // If the declared bitfield size is exceeding the type size, shrink
    // the bitfield size to the size of the type in bits.
    bitfield_size = member.GetType().GetByteSize() * CHAR_BIT;
  }

  return std::make_unique<MemberOfNode>(
      location, member.GetType(), std::move(lhs), member.IsBitfield(),
      bitfield_size, std::move(idx), is_arrow);
}

}  // namespace lldb_eval
