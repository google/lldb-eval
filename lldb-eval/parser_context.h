#ifndef LLDB_EVAL_PARSER_CONTEXT_H_
#define LLDB_EVAL_PARSER_CONTEXT_H_

#include "clang/Basic/SourceManager.h"
#include "lldb-eval/type.h"
#include "lldb/lldb-enumerations.h"

namespace lldb_eval {

enum class ErrorCode : unsigned char {
  kOk = 0,
  kInvalidExpressionSyntax,
  kInvalidNumericLiteral,
  kInvalidOperandType,
  kUndeclaredIdentifier,
  kNotImplemented,
  kUnknown,
};

enum class UbStatus : unsigned char {
  kOk = 0,
  kDivisionByZero,
  // If "a / b" isn't representable in its result type, then results of "a / b"
  // and "a % b" are undefined behaviour. This happens when "a" is equal to the
  // minimum value of the result type and "b" is equal to -1.
  kDivisionByMinusOne,
  kInvalidCast,
  kInvalidShift,
  kNullptrArithmetic,
  kInvalidPtrDiff,
};

class Error {
 public:
  void Set(ErrorCode code, std::string message) {
    code_ = code;
    message_ = std::move(message);
  }
  void SetUbStatus(UbStatus status) { ub_status_ = status; }
  void Clear() { *this = {}; }

  ErrorCode code() const { return code_; }
  const std::string& message() const { return message_; }
  UbStatus ub_status() const { return ub_status_; }

  explicit operator bool() const { return code_ != ErrorCode::kOk; }

 private:
  ErrorCode code_ = ErrorCode::kOk;
  std::string message_;
  UbStatus ub_status_ = UbStatus::kOk;
};

class ParserContext {
 public:
  struct IdentifierInfo {
    virtual TypeSP GetType() = 0;
    virtual bool IsValid() const = 0;
    virtual ~IdentifierInfo() = default;
  };
  virtual clang::SourceManager& GetSourceManager() const = 0;
  virtual TypeSP ResolveTypeByName(const std::string& name) const = 0;
  virtual std::unique_ptr<IdentifierInfo> LookupIdentifier(
      const std::string& name) const = 0;
  virtual bool IsContextVar(const std::string& name) const = 0;
  virtual TypeSP GetBasicType(lldb::BasicType) = 0;
  virtual TypeSP GetEmptyType() = 0;
  virtual lldb::BasicType GetPtrDiffType() = 0;
  virtual lldb::BasicType GetSizeType() = 0;
  virtual ~ParserContext() = default;

  void SetAllowSideEffects(bool allow_side_effects);
  bool AllowSideEffects() const;

 private:
  // Whether side effects should be allowed.
  bool allow_side_effects_ = false;
};

std::string FormatDiagnostics(clang::SourceManager& sm,
                              const std::string& message,
                              clang::SourceLocation loc);

}  // namespace lldb_eval
#endif  // LLDB_EVAL_PARSER_CONTEXT_H_
