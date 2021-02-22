#ifndef EXPERIMENTAL_PROTO_TO_STR_H_
#define EXPERIMENTAL_PROTO_TO_STR_H_

#include <ostream>
#include <string>

#include "experimental/protobuf/expression.pb.h"

std::ostream& operator<<(std::ostream& os, const NumericalLiteral& expr);
std::ostream& operator<<(std::ostream& os, const ConditionalExpr& expr);
std::ostream& operator<<(std::ostream& os, const LogicalOrExpr& expr);
std::ostream& operator<<(std::ostream& os, const LogicalAndExpr& expr);
std::ostream& operator<<(std::ostream& os, const InclusiveOrExpr& expr);
std::ostream& operator<<(std::ostream& os, const ExclusiveOrExpr& expr);
std::ostream& operator<<(std::ostream& os, const BitwiseAndExpr& expr);
std::ostream& operator<<(std::ostream& os, const EqualityExpr& expr);
std::ostream& operator<<(std::ostream& os, const RelationalExpr& expr);
std::ostream& operator<<(std::ostream& os, const ShiftExpr& expr);
std::ostream& operator<<(std::ostream& os, const AdditiveExpr& expr);
std::ostream& operator<<(std::ostream& os, const MultiplicativeExpr& expr);
std::ostream& operator<<(std::ostream& os, const CastExpr& expr);
std::ostream& operator<<(std::ostream& os, const UnaryExpr& expr);
std::ostream& operator<<(std::ostream& os, const TypeId& expr);
std::ostream& operator<<(std::ostream& os, const PrimaryExpr& expr);
std::ostream& operator<<(std::ostream& os, const Expr& expr);

std::string DumpExpression(const Expr& expr);

#endif  // EXPERIMENTAL_PROTO_TO_STR_H_
