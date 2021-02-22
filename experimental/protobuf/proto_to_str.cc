#include "experimental/protobuf/proto_to_str.h"

#include <ostream>
#include <sstream>
#include <string>

#include <algorithm>
#include <vector>

#include "experimental/protobuf/expression.pb.h"

#define ADD_CASE(option, output) \
    case option:                 \
      os << output;              \
      break

std::ostream& operator<<(std::ostream& os, const NumericalLiteral& expr) {
  return os << expr.int32_value();
}

std::ostream& operator<<(std::ostream& os, const ConditionalExpr& expr) {
  os << expr.condition();
  if (expr.has_extended()) {
    os << " ? ";
    os << expr.extended().lhs();
    os << " : ";
    os << expr.extended().rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const LogicalOrExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    os << " || " << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const LogicalAndExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    os << " && " << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const InclusiveOrExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    os << " | " << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const ExclusiveOrExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    os << " ^ " << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const BitwiseAndExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    os << " & " << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const EqualityExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    switch (expr.operation()) {
      ADD_CASE(EqualityExpr::EQ, " == ");
      ADD_CASE(EqualityExpr::NE, " == ");

      default:
        assert(false);
    }
    os << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const RelationalExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    switch (expr.operation()) {
      ADD_CASE(RelationalExpr::LT, " < ");
      ADD_CASE(RelationalExpr::GT, " > ");
      ADD_CASE(RelationalExpr::LTE, " <= ");
      ADD_CASE(RelationalExpr::GTE, " >= ");

      default:
        assert(false);
    }
    os << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const ShiftExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    switch (expr.operation()) {
      ADD_CASE(ShiftExpr::LSH, " << ");
      ADD_CASE(ShiftExpr::RSH, " >> ");

      default:
        assert(false);
    }
    os << expr.rhs();
  }
  return os;
}


std::ostream& operator<<(std::ostream& os, const AdditiveExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    os << (expr.operation() == AdditiveExpr::PLUS ? " + " : " - ");
    os << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const MultiplicativeExpr& expr) {
  os << expr.lhs();
  if (expr.has_rhs()) {
    switch (expr.operation()) {
      ADD_CASE(MultiplicativeExpr::MUL, " * ");
      ADD_CASE(MultiplicativeExpr::DIV, " / ");
      ADD_CASE(MultiplicativeExpr::MOD, " % ");

      default:
        assert(false);
    }
    os << expr.rhs();
  }
  return os;
}

std::ostream& operator<<(std::ostream& os, const CastExpr& expr) {
  if (expr.has_cast_expr()) {
    return os << "(" << expr.cast_expr().type_id() << ")"
              << expr.cast_expr().expr();
  }
  return os << expr.unary_expr();
}

std::ostream& operator<<(std::ostream& os, const UnaryExpr& expr) {
  if (expr.has_unary_expr()) {
    os << " ";
    switch (expr.unary_expr().operation()) {
      ADD_CASE(UnaryExpr::PLUS, "+");
      ADD_CASE(UnaryExpr::MINUS, "-");
      ADD_CASE(UnaryExpr::LOGICAL_NOT, "!");
      ADD_CASE(UnaryExpr::BITWISE_NOT, "~");
      ADD_CASE(UnaryExpr::DEREFERNCE, "*");
      ADD_CASE(UnaryExpr::ADDRESS, "&");

      default:
        assert(false);
    }
    return os << expr.unary_expr().expr();
  }

  return os << expr.primary_expr();
}

std::ostream& operator<<(std::ostream& os, const TypeId& expr) {
  std::vector<std::string> options =
      {"int", "unsigned int", "char", "signed char", "unsigned char", "short",
       "unsigned short", "long", "unsigned long", "long long", "float",
       "double", "long double"};
  std::random_shuffle(options.begin(), options.end());
  return os << options[0];
}

std::ostream& operator<<(std::ostream& os, const PrimaryExpr& expr) {
  if (expr.has_literal()) return os << expr.literal();
  if (expr.has_expr()) return os << "(" << expr.expr() << ")";
  return os << expr.def_literal();
}

std::ostream& operator<<(std::ostream& os, const Expr& expr) {
  return os << expr.conditional_expr();
}

std::string DumpExpression(const Expr& expr) {
  std::ostringstream stream;
  stream << expr;
  return stream.str();
}
