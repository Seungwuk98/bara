#include "bara/ast/AST.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/Memory.h"
#include "llvm/ADT/SmallVectorExtras.h"
#include <variant>

namespace bara {

void LvExprInterpreter::visit(const IdentifierExpression &expr) {
  result = context->getGC()->registerRoot(interpretIdentifier(&expr));
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
  if (auto *reg = std::get_if<GC::RootRegister>(&lv)) {
    if (reg->hasValue()) {
      stmtInterpreter->report(
          expr.getRange(), InterpretDiagnostic::error_tuple_index_assignment);
    } else {
      result = std::move(*reg);
    }
    return;
  }
}

void LvExprInterpreter::visit(const TupleExpression &expr) {
  SmallVector<GC::RootRegister> regs;
  regs.reserve(expr.getExprs().size());
  for (auto *expr : expr.getExprs()) {
    expr->accept(*this);
    if (diag.hasError())
      return;
    regs.push_back(std::move(result));
  }
  GCSAFE(context->getGC()) {
    auto tupleM = TupleMemory::create(
        context, llvm::map_to_vector(regs, [](GC::RootRegister &reg) {
          return reg.getMemory();
        }));
    result = context->getGC()->registerRoot(tupleM);
  }
}

void LvExprInterpreter::visit(const StructAccessExpression &expr) {
  auto base = stmtInterpreter->rvInterpret(*expr.getBase());
  if (diag.hasError())
    return;

  if (!base.getValue()->isa<StructValue>()) {
    stmtInterpreter->report(expr.getBase()->getRange(),
                            InterpretDiagnostic::error_expected_struct,
                            base.getValue()->toString());
    return;
  }

  auto structV = base.getValue()->cast<StructValue>();
  const auto &idxMap =
      stmtInterpreter->structMemberMap.at(structV->getDeclaration());

  if (auto it = idxMap.find(expr.getFieldName()); it != idxMap.end()) {
    GCSAFE(context->getGC()) {
      auto memberV = structV->getMembers()[(it->second)];
      result = context->getGC()->registerRoot(memberV);
    }
  } else {
    stmtInterpreter->report(
        expr.getRange(), InterpretDiagnostic::error_unknown_struct_field,
        expr.getFieldName(), structV->getDeclaration()->getName());
    return;
  }
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
EXPRESSION(AssignmentExpression)
EXPRESSION(UnaryExpression)
EXPRESSION(ConditionalExpression)
EXPRESSION(CallExpression)
EXPRESSION(ArrayExpression)
EXPRESSION(GroupExpression)
EXPRESSION(CompoundExpression)
EXPRESSION(IntegerLiteral)
EXPRESSION(BooleanLiteral)
EXPRESSION(FloatLiteral)
EXPRESSION(StringLiteral)
EXPRESSION(NilLiteral)

#undef EXPRESSION

} // namespace bara
