#include "bara/ast/AST.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"
#include "llvm/ADT/TypeSwitch.h"

namespace bara {

StmtInterpreter::StmtInterpreter(MemoryContext *context, Diagnostic &diag)
    : context(context), diag(diag), env(),
      rvInterpreter(new RvExprInterpreter(this)),
      lvInterpreter(new LvExprInterpreter(this)) {}

StmtInterpreter::~StmtInterpreter() {
  delete rvInterpreter;
  delete lvInterpreter;
}

void StmtInterpreter::visit(const Program &stmt) {
  Environment::Scope scope(env);

  for (const auto &decl : stmt.getStmts()) {
    decl->accept(*this);
    if (isTerminated())
      break;
  }

  if (returnFlag) {
    report(returnFlag->getRange(),
           InterpretDiagnostic::error_unresolved_return_statement);
    return;
  }

  if (continueFlag) {
    report(continueFlag->getRange(),
           InterpretDiagnostic::error_unresolved_continue_statement);
    return;
  }

  if (breakFlag) {
    report(breakFlag->getRange(),
           InterpretDiagnostic::error_unresolved_break_statement);
  }
}

void StmtInterpreter::visit(const CompoundStatement &stmt) {
  Environment::Scope scope(env);
  for (const auto &decl : stmt.getStmts()) {
    decl->accept(*this);
    if (isTerminated())
      return;
  }
}

void StmtInterpreter::visit(const ExpressionStatement &stmt) {
  stmt.getExpr()->accept(*rvInterpreter);
}

void StmtInterpreter::visit(const IfStatement &stmt) {
  stmt.getCond()->accept(*rvInterpreter);
  if (isTerminated())
    return;
  auto condV = rvInterpret(*stmt.getCond());
  if (isTerminated())
    return;
  auto condOpt = condV->toBool();
  if (!condOpt) {
    report(stmt.getCond()->getRange(),
           InterpretDiagnostic::error_invalid_to_conver_boolean,
           condV->toString());
    return;
  }

  if (*condOpt) {
    for (const auto &thenStmt : stmt.getThenStmts()) {
      thenStmt->accept(*this);
      if (isTerminated())
        return;
    }
  } else if (stmt.hasElse()) {
    for (const auto &elseStmt : stmt.getElseStmts()) {
      elseStmt->accept(*this);
      if (isTerminated())
        return;
    }
  }
}

void StmtInterpreter::visit(const WhileStatement &stmt) {
  Environment::Scope scope(env);
  if (stmt.isDoWhile()) {
    bool cond;
    do {
      for (const auto &bodyStmt : stmt.getBody()) {
        bodyStmt->accept(*this);
        if (diag.hasError())
          return;
        else if (continueFlag) {
          continueFlag = nullptr;
          continue;
        } else if (breakFlag) {
          breakFlag = nullptr;
          break;
        } else if (returnValue) {
          return;
        }
      }

      auto condV = rvInterpret(*stmt.getCond());
      if (isTerminated())
        return;
      auto condOpt = condV->toBool();
      if (!condOpt) {
        report(stmt.getCond()->getRange(),
               InterpretDiagnostic::error_invalid_to_conver_boolean,
               condV->toString());
        return;
      }

      cond = *condOpt;
    } while (cond);

  } else {
    while (true) {
      auto condV = rvInterpret(*stmt.getCond());
      if (isTerminated())
        return;
      auto condOpt = condV->toBool();
      if (!condOpt) {
        report(stmt.getCond()->getRange(),
               InterpretDiagnostic::error_invalid_to_conver_boolean,
               condV->toString());
        return;
      }

      if (!*condOpt)
        break;

      for (const auto &bodyStmt : stmt.getBody()) {
        bodyStmt->accept(*this);
        if (diag.hasError())
          return;
        else if (continueFlag) {
          continueFlag = nullptr;
          continue;
        } else if (breakFlag) {
          breakFlag = nullptr;
          break;
        } else if (returnValue) {
          return;
        }
      }
    }
  }
}

void StmtInterpreter::visit(const ForStatement &stmt) {
  llvm_unreachable("Not implemented");
}

void StmtInterpreter::visit(const BreakStatement &stmt) {
  assert(!isTerminated() && "Already terminated");
  breakFlag = &stmt;
}

void StmtInterpreter::visit(const ContinueStatement &stmt) {
  assert(!isTerminated() && "Already terminated");
  continueFlag = &stmt;
}

void StmtInterpreter::visit(const ReturnStatement &stmt) {
  assert(!isTerminated() && "Already terminated");
  if (stmt.getExpr()) {
    returnValue = rvInterpret(**stmt.getExpr());
    if (isTerminated())
      return;
  }
  returnFlag = &stmt;
}

void StmtInterpreter::visit(const DeclarationStatement &stmt) {
  if (stmt.getInit()) {
    auto initV = rvInterpret(**stmt.getInit());
    if (isTerminated())
      return;
    if (!matchPattern(*stmt.getPattern(), initV.get())) {
      report(stmt.getRange(), InterpretDiagnostic::error_match_pattern_fail,
             stmt.getPattern()->toString(), initV->toString());
      return;
    }
  } else
    patternDeclaration(*stmt.getPattern());
}

void StmtInterpreter::visit(const AssignmentStatement &stmt) {
  auto *lv = lvInterpret(*stmt.getLhs());
  if (isTerminated())
    return;

  auto value = rvInterpret(*stmt.getRhs());
  if (isTerminated())
    return;

  if (!lv->assign(value.get())) {
    report(stmt.getRange(), InterpretDiagnostic::error_assignment_fail,
           stmt.getLhs()->toString(), value->toString());
  }
}

void StmtInterpreter::visit(const OperatorAssignmentStatement &stmt) {
  auto *lv = lvInterpret(*stmt.getLhs());
  if (isTerminated())
    return;
  if (!lv->isa<ValueMemory>()) {
    report(stmt.getLhs()->getRange(),
           InterpretDiagnostic::error_invalid_left_size_of_operator_assignment,
           operatorToString(stmt.getOperator()));
    return;
  }

  auto *valueMemory = lv->cast<ValueMemory>();
  auto lhsV = valueMemory->view();

  auto rhsV = rvInterpret(*stmt.getRhs());
  if (isTerminated())
    return;

  auto result = rvInterpreter->binaryOp(lhsV, rhsV.get(), stmt.getOperator());
  if (isTerminated())
    return;

  valueMemory->assign(std::move(result));
}

void StmtInterpreter::visit(const FunctionDeclaration &stmt) {
  auto functionV = FunctionValue::create(env, &stmt);
  auto functionName = stmt.getName();

  if (env.isDefinedCurrScope(functionName)) {
    report(stmt.getRange(), InterpretDiagnostic::error_redefinition_in_scope,
           functionName);
    return;
  }

  auto *newMem = ValueMemory::create(context, std::move(functionV));
  env.insert(functionName, newMem);
}

void StmtInterpreter::patternDeclaration(const Pattern &pattern) {
  llvm::TypeSwitch<const Pattern *>(&pattern)
      .Case([&](const IdentifierPattern *pattern) {
        auto ident = pattern->getName();
        if (env.isDefinedCurrScope(ident)) {
          report(pattern->getRange(),
                 InterpretDiagnostic::error_redefinition_in_scope, ident);
          return false;
        }
        auto *newMem = ValueMemory::create(context, NilValue::create());
        env.insert(ident, newMem);
        return true;
      })
      .Case([&](const TuplePattern *pattern) {
        auto patterns = pattern->getPatterns();

        for (auto *pattern : patterns) {
          patternDeclaration(*pattern);
          if (isTerminated())
            return;
        }
      })
      .Case([&](const GroupPattern *pattern) {
        patternDeclaration(*pattern->getPattern());
      })
      .Default([&](const Pattern *pattern) { return; });
}

bool StmtInterpreter::matchPattern(const Pattern &pattern, Value *value) {
  return llvm::TypeSwitch<const Pattern *, bool>(&pattern)
      .Case([&](const IdentifierPattern *pattern) {
        auto ident = pattern->getName();
        if (env.isDefinedCurrScope(ident)) {
          report(pattern->getRange(),
                 InterpretDiagnostic::error_redefinition_in_scope, ident);
          return false;
        }
        auto *newMem = ValueMemory::create(context, value->clone());
        env.insert(ident, newMem);
        return true;
      })
      .Case([&](const TuplePattern *pattern) {
        if (!value->isa<TupleValue>())
          return false;

        auto patterns = pattern->getPatterns();
        auto *tupleValue = value->cast<TupleValue>();
        auto memories = tupleValue->getMemories();
        if (patterns.size() != memories.size())
          return false;

        for (auto [pattern, memory] : llvm::zip(patterns, memories)) {
          if (!matchPattern(*pattern, memory->view()))
            return false;
        }
        return true;
      })
      .Case([&](const GroupPattern *pattern) {
        return matchPattern(*pattern->getPattern(), value);
      })
      .Case([&](const IntegerPattern *pattern) {
        if (!value->isa<IntegerValue>())
          return false;
        auto *intValue = value->cast<IntegerValue>();
        return intValue->getValue() == pattern->getValue();
      })
      .Case([&](const StringPattern *pattern) {
        if (!value->isa<StringValue>())
          return false;
        auto *stringValue = value->cast<StringValue>();
        return stringValue->getValue() == pattern->getValue();
      })
      .Case([&](const BooleanPattern *pattern) {
        if (!value->isa<BoolValue>())
          return false;
        auto *boolValue = value->cast<BoolValue>();
        return boolValue->getValue() == pattern->getValue();
      })
      .Case([&](const FloatPattern *pattern) {
        if (!value->isa<FloatValue>())
          return false;
        auto *floatValue = value->cast<FloatValue>();
        llvm::APFloat patternValue(llvm::APFloat::IEEEdouble(),
                                   pattern->getValue());
        return floatValue->getValue() == patternValue;
      })
      .Case([&](const EmptyPattern *pattern) { return true; })
      .Default([&](const Pattern *pattern) {
        assert(pattern->isa<NilPattern>());
        return value->isa<NilValue>();
      });
}
} // namespace bara
