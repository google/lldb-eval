#include "lldb-eval/parser_context.h"

#include "clang/Basic/SourceManager.h"
#include "llvm/Support/FormatAdapters.h"
#include "llvm/Support/FormatVariadic.h"

namespace lldb_eval {

static uint32_t GetNumberOfNonEmptyBaseClasses(TypeSP type) {
  // Go through the base classes and count non-empty ones.
  uint32_t ret = 0;
  uint32_t num_direct_bases = type->GetNumberOfDirectBaseClasses();

  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    TypeSP base_type = type->GetDirectBaseClassAtIndex(i).type;
    if (base_type->GetNumberOfFields() > 0 ||
        GetNumberOfNonEmptyBaseClasses(base_type) > 0) {
      ret += 1;
    }
  }
  return ret;
}

static Type::MemberInfo GetFieldWithNameIndexPath(TypeSP type,
                                                  const std::string& name,
                                                  std::vector<uint32_t>* idx,
                                                  TypeSP empty_type) {
  // Go through the fields first.
  uint32_t num_fields = type->GetNumberOfFields();
  for (uint32_t i = 0; i < num_fields; ++i) {
    auto field = type->GetFieldAtIndex(i);
    // Name can be null if this is a padding field.
    if (field.name == name) {
      if (idx) {
        assert(idx->empty());
        // Direct base classes are located before fields, so field members
        // needs to be offset by the number of base classes.
        idx->push_back(i + GetNumberOfNonEmptyBaseClasses(type));
      }
      return field;
    } else if (field.type->IsAnonymousType()) {
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

      assert(!field.name && "Field should be unnamed.");

      auto field_in_anon_type =
          GetFieldWithNameIndexPath(field.type, name, idx, empty_type);
      if (field_in_anon_type) {
        if (idx) {
          idx->push_back(i + GetNumberOfNonEmptyBaseClasses(type));
        }
        return field_in_anon_type;
      }
    }
  }

  // LLDB can't access inherited fields of anonymous struct members.
  if (type->IsAnonymousType()) {
    return {{}, empty_type, false, 0};
  }

  // Go through the base classes and look for the field there.
  uint32_t num_non_empty_bases = 0;
  uint32_t num_direct_bases = type->GetNumberOfDirectBaseClasses();
  for (uint32_t i = 0; i < num_direct_bases; ++i) {
    auto base = type->GetDirectBaseClassAtIndex(i).type;
    auto field = GetFieldWithNameIndexPath(base, name, idx, empty_type);
    if (field) {
      if (idx) {
        idx->push_back(num_non_empty_bases);
      }
      return field;
    }
    if (base->GetNumberOfFields() > 0) {
      num_non_empty_bases += 1;
    }
  }

  return {{}, empty_type, false, 0};
}

std::string FormatDiagnostics(clang::SourceManager& sm,
                              const std::string& message,
                              clang::SourceLocation loc) {
  // Get the source buffer and the location of the current token.
  llvm::StringRef text = sm.getBufferData(sm.getFileID(loc));
  size_t loc_offset = sm.getCharacterData(loc) - text.data();

  // Look for the start of the line.
  size_t line_start = text.rfind('\n', loc_offset);
  line_start = line_start == llvm::StringRef::npos ? 0 : line_start + 1;

  // Look for the end of the line.
  size_t line_end = text.find('\n', loc_offset);
  line_end = line_end == llvm::StringRef::npos ? text.size() : line_end;

  // Get a view of the current line in the source code and the position of the
  // diagnostics pointer.
  llvm::StringRef line = text.slice(line_start, line_end);
  int32_t arrow = sm.getPresumedColumnNumber(loc);

  // Calculate the padding in case we point outside of the expression (this can
  // happen if the parser expected something, but got EOF).
  size_t expr_rpad = std::max(0, arrow - static_cast<int32_t>(line.size()));
  size_t arrow_rpad = std::max(0, static_cast<int32_t>(line.size()) - arrow);

  return llvm::formatv("{0}: {1}\n{2}\n{3}", loc.printToString(sm), message,
                       llvm::fmt_pad(line, 0, expr_rpad),
                       llvm::fmt_pad("^", arrow - 1, arrow_rpad));
}

void ParserContext::SetAllowSideEffects(bool allow_side_effects) {
  allow_side_effects_ = allow_side_effects;
}

bool ParserContext::AllowSideEffects() const { return allow_side_effects_; }

std::tuple<Type::MemberInfo, std::vector<uint32_t>>
ParserContext::GetMemberInfo(TypeSP type, const std::string& name) const {
  std::vector<uint32_t> idx;
  auto member = GetFieldWithNameIndexPath(type, name, &idx, GetEmptyType());
  std::reverse(idx.begin(), idx.end());
  return {member, std::move(idx)};
}

}  // namespace lldb_eval
