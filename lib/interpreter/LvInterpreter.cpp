#include "bara/ast/AST.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/Memory.h"
#include <variant>

namespace bara {

void LvExprInterpreter::visit(const IdentifierExpression &expr) {
  result = interpretIdentifier(&expr);
}

void LvExprInterpreter::visit(const IndexExpression &expr) {
  auto lv = interpretIndex(&expr);
  if (diag.hasError())
    return;
  if (std::holds_alternative<char>(lv)) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::error_string_index_assignment);
    return;
  }
  if (std::holds_alternative<UniqueValue<Value>>(lv)) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::error_tuple_index_assignment);
    return;
  }
  result = std::get<Memory *>(lv);
}

void LvExprInterpreter::visit(const TupleExpression &expr) {
  SmallVector<Memory *> mems;
  for (auto *expr : expr.getExprs()) {
    expr->accept(*this);
    if (diag.hasError())
      return;
    mems.push_back(result);
  }
  result = TupleMemory::create(context, mems);
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
EXPRESSION(ConditionalExpression)
EXPRESSION(CallExpression)
EXPRESSION(ArrayExpression)
EXPRESSION(GroupExpression)
EXPRESSION(IntegerLiteral)
EXPRESSION(BooleanLiteral)
EXPRESSION(FloatLiteral)
EXPRESSION(StringLiteral)
EXPRESSION(NilLiteral)

#undef EXPRESSION

} // namespace bara
