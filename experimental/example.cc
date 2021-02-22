#include <variant>

class BinaryExpr;
class IntegerConstant;
class DoubleConstant;

using Expr = std::variant<BinaryExpr, IntegerConstant, DoubleConstant>;

enum class BinOp {
  ADD,
  SUB,
  MUL,
  DIV,
  MOD,
};

class BinaryExpr {
 public:
  BinaryExpr() = default;
  BinaryExpr(BinOp operation, const Expr& lhs, const Expr& rhs)
      : operation_(operation), lhs_(lhs), rhs_(rhs) {}

 private:

};

class GenWeightedNode {
 public:

};


struct Constraints {
  bool allows_float;
};

struct GrammarRule {
 public:
  
};

class GrammarNode {
 protected:
  void AddRule();

};


class Node {
 public:
  Node(const Expr& expr, const Constraints& constraints) {
    
  }


};