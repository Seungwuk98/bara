#include "bara/ast/AST.h"
#include "bara/interpreter/ExprInterpreter.h"
#include <variant>

namespace bara {

void LvExprInterpreter::visit(const IdentifierExpression &expr) {
  result = interpretIdentifier(&expr);
}

void LvExprInterpreter::visit(const IndexExpression &expr) {
  auto lv = interpretIndex(&expr);
  if (std::holds_alternative<char>(lv)) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::error_string_index_assignment);
    return;
  }
  result = std::get<Memory *>(lv);
}

#define EXPRESSION(Name)                                                       \
  void LvExprInterpreter::visit(const Name &expr) {                            \
    stmtInterpreter->report(                                                   \
        expr.getRange(),                                                       \
        InterpretDiagnostic::error_invalid_expression_for_assignment, #Name);  \
  }

EXPRESSION(MatchExpression)
EXPRESSION(LambdaExpression)
EXPRESSION(BinaryExpression)
EXPRESSION(UnaryExpression)
EXPRESSION(CallExpression)
EXPRESSION(ArrayExpression)
EXPRESSION(TupleExpression)
EXPRESSION(GroupExpression)
EXPRESSION(IntegerLiteral)
EXPRESSION(BooleanLiteral)
EXPRESSION(FloatLiteral)
EXPRESSION(StringLiteral)
EXPRESSION(NilLiteral)

#undef EXPRESSION

} // namespace bara
