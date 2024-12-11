#include "bara/ast/AST.h"
#include "bara/interpreter/ExprInterpreter.h"

namespace bara {

void RvExprInterpreter::visit(const IdentifierExpression &expr) {
  auto *memory = interpretIdentifier(&expr);
  if (diag.hasError())
    return;

  auto *valueMemory = memory->cast<ValueMemory>();
  result = valueMemory->view()->clone();
}

void RvExprInterpreter::visit(const IndexExpression &expr) {
  auto *memory = interpretIndex(&expr);
  if (diag.hasError())
    return;

  auto *valueMemory = memory->cast<ValueMemory>();
  result = valueMemory->view()->clone();
}

} // namespace bara
