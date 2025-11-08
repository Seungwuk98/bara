#include "bara/interpreter/StmtInterpreter.h"
#include "bara/ast/AST.h"
#include "bara/context/MemoryContext.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/Value.h"
#include "llvm/ADT/TypeSwitch.h"

namespace bara {

void interpret(const Program *program, MemoryContext *context,
               Diagnostic &diag) {
  StmtInterpreter interpreter(context, diag);
  program->accept(interpreter);
}

StmtInterpreter::StmtInterpreter(MemoryContext *context, Diagnostic &diag)
    : context(context), diag(diag), rvInterpreter(new RvExprInterpreter(this)),
      lvInterpreter(new LvExprInterpreter(this)) {
  context->getGC()->setInterpreter(this);
  stack.emplace_back(
      std::make_unique<Environment>(context->getBuiltinFuncTable()));
}

StmtInterpreter::~StmtInterpreter() {
  delete rvInterpreter;
  delete lvInterpreter;
}

GC::RootRegister StmtInterpreter::lvInterpret(const Expression &ast) {
  ast.accept(*lvInterpreter);
  return lvInterpreter->getResult();
}

GC::RootRegister StmtInterpreter::rvInterpret(const Expression &ast) {
  ast.accept(*rvInterpreter);
  return rvInterpreter->getResult();
}

void StmtInterpreter::visit(const Program &stmt) {
  Environment::Scope scope(getCurrEnv());

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

void StmtInterpreter::visit(const StructDeclaration &stmt) {
  auto structName = stmt.getName();

  if (getCurrEnv().isDefinedCurrScope(structName)) {
    report(stmt.getRange(), InterpretDiagnostic::error_redefinition_in_scope,
           structName);
    return;
  }

  GCSAFE(context->getGC()) {
    auto *structConV = BuiltinFunctionValue::create(
        context, structName, "Struct Constructor",
        [&](ArrayRef<Value *> args, Diagnostic &diag, SMRange range,
            MemoryContext *context) -> Value * {
          if (args.size() != stmt.getFieldSize()) {
            diag.report(
                range, llvm::SourceMgr::DK_Error,
                llvm::formatv(
                    "Struct '{0}' constructor expects {1} arguments, but got "
                    "{2}.",
                    structName, stmt.getFieldSize(), args.size())
                    .str());
            return nullptr;
          }

          GCSAFE(context->getGC()) {
            auto structV = StructValue::create(context, &stmt, args);
            return structV;
          }
        });
    Memory *newMem = ImmutableMemory::create(context, structConV);
    getCurrEnv().insert(structName, newMem);
  }
  DenseMap<StringRef, size_t> &idxMap = structMemberMap[&stmt];
  assert(idxMap.empty() && "Struct member map already exists");

  for (auto [idx, field] : llvm::enumerate(stmt.getFieldNames())) {
    idxMap[field] = idx;
  }
}

static void interpretCompoundStatement(ArrayRef<Statement *> stmt,
                                       StmtInterpreter &interpreter) {
  Environment::Scope scope(interpreter.getCurrEnv());
  for (const auto &decl : stmt) {
    decl->accept(interpreter);
    if (interpreter.isTerminated())
      return;
  }
}

void StmtInterpreter::visit(const ExpressionStatement &stmt) {
  stmt.getExpr()->accept(*rvInterpreter);
}

void StmtInterpreter::visit(const IfStatement &stmt) {
  auto condR = rvInterpret(*stmt.getCond());
  if (isTerminated())
    return;
  auto condOpt = condR.getValue()->toBool();
  if (!condOpt) {
    report(stmt.getCond()->getRange(),
           InterpretDiagnostic::error_invalid_to_conver_boolean,
           condR.getValue()->toString());
    return;
  }

  if (*condOpt)
    interpretCompoundStatement(stmt.getThenStmt(), *this);
  else if (stmt.hasElse())
    interpretCompoundStatement(stmt.getElseStmt(), *this);
}

void StmtInterpreter::visit(const WhileStatement &stmt) {
  Environment::Scope scope(getCurrEnv());
  if (stmt.isDoWhile()) {
    bool cond;
    do {
      interpretCompoundStatement(stmt.getBody(), *this);
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

      auto condR = rvInterpret(*stmt.getCond());
      if (isTerminated())
        return;
      auto condOpt = condR.getValue()->toBool();
      if (!condOpt) {
        report(stmt.getCond()->getRange(),
               InterpretDiagnostic::error_invalid_to_conver_boolean,
               condR.getValue()->toString());
        return;
      }

      cond = *condOpt;
    } while (cond);

  } else {
    while (true) {
      auto condV = rvInterpret(*stmt.getCond());
      if (isTerminated())
        return;
      auto condOpt = condV.getValue()->toBool();
      if (!condOpt) {
        report(stmt.getCond()->getRange(),
               InterpretDiagnostic::error_invalid_to_conver_boolean,
               condV.getValue()->toString());
        return;
      }

      if (!*condOpt)
        break;

      interpretCompoundStatement(stmt.getBody(), *this);
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

void StmtInterpreter::visit(const ForStatement &stmt) {
  Environment::Scope scope(getCurrEnv());
  auto declOpt = stmt.getDecl();

  if (declOpt) {
    (*declOpt)->accept(*this);
    if (isTerminated())
      return;
  }

  auto condOpt = stmt.getCond();
  auto stepOpt = stmt.getStep();
  while (true) {
    if (condOpt) {
      auto condR = rvInterpret(**condOpt);
      if (isTerminated())
        return;
      auto condBool = condR.getValue()->toBool();
      if (!condBool) {
        report((*condOpt)->getRange(),
               InterpretDiagnostic::error_invalid_to_conver_boolean,
               condR.getValue()->toString());
        return;
      }
      if (!*condBool)
        break;
    }
    Environment::Scope bodyScope(getCurrEnv());
    interpretCompoundStatement(stmt.getBody(), *this);
    if (diag.hasError())
      return;
    else if (continueFlag) {
      continueFlag = nullptr;
      continue;
    } else if (breakFlag) {
      breakFlag = nullptr;
      break;
    } else if (returnFlag) {
      return;
    }

    if (stepOpt) {
      rvInterpret(**stepOpt);
      if (isTerminated())
        return;
    }
  }
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
    auto reg = rvInterpret(**stmt.getExpr());
    if (isTerminated())
      return;
    returnValue = reg.getValue();
  }
  returnFlag = &stmt;
}

void StmtInterpreter::visit(const DeclarationStatement &stmt) {
  if (stmt.getInit()) {
    auto initR = rvInterpret(**stmt.getInit());
    if (isTerminated())
      return;
    if (!matchPattern(*stmt.getPattern(), initR.getValue())) {
      report(stmt.getRange(), InterpretDiagnostic::error_match_pattern_fail,
             stmt.getPattern()->toString(), initR.getValue()->toString());
      return;
    }
  } else
    patternDeclaration(*stmt.getPattern());
}

void StmtInterpreter::visit(const FunctionDeclaration &stmt) {
  GC::RootRegister functionR;
  FunctionValue *functionV;
  GCSAFE(context->getGC()) {
    functionV = FunctionValue::create(context, getCurrEnv(), &stmt);
    functionR = context->getGC()->registerRoot(functionV);
  }
  auto functionName = stmt.getName();

  if (getCurrEnv().isDefinedCurrScope(functionName)) {
    report(stmt.getRange(), InterpretDiagnostic::error_redefinition_in_scope,
           functionName);
    return;
  }

  GCSAFE(context->getGC()) {
    Memory *newMem = ImmutableMemory::create(context, std::move(functionV));
    getCurrEnv().insert(functionName, newMem);
  }
}

void StmtInterpreter::patternDeclaration(const Pattern &pattern) {
  llvm::TypeSwitch<const Pattern *>(&pattern)
      .Case([&](const IdentifierPattern *pattern) {
        auto ident = pattern->getName();
        if (getCurrEnv().isDefinedCurrScope(ident)) {
          report(pattern->getRange(),
                 InterpretDiagnostic::error_redefinition_in_scope, ident);
          return false;
        }

        GCSAFE(context->getGC()) {
          auto *newMem =
              ValueMemory::create(context, NilValue::create(context));
          getCurrEnv().insert(ident, newMem);
        }
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

static Value *
getValueFromVariant(const variant<Value *, ValueMemory *> &valueOrMemory) {
  return std::visit(
      [&]<typename T>(T *obj) -> Value * {
        using DecayT = std::remove_cvref_t<T>;
        if constexpr (std::is_same_v<ValueMemory, T>) {
          return obj->get();
        } else {
          return obj;
        }
      },
      valueOrMemory);
}

bool StmtInterpreter::matchPattern(
    const Pattern &pattern, variant<Value *, ValueMemory *> valueOrMemory) {
  return llvm::TypeSwitch<const Pattern *, bool>(&pattern)
      .Case([&](const IdentifierPattern *pattern) {
        auto ident = pattern->getName();
        if (getCurrEnv().isDefinedCurrScope(ident)) {
          report(pattern->getRange(),
                 InterpretDiagnostic::error_redefinition_in_scope, ident);
          return false;
        }
        GCSAFE(context->getGC()) {
          auto *newMem = std::visit(
              [&]<typename T>(T *obj) -> ValueMemory * {
                if constexpr (std::is_same_v<Value, T>) {
                  return ValueMemory::create(context, obj);
                } else {
                  return obj;
                }
              },
              valueOrMemory);
          getCurrEnv().insert(ident, newMem);
        }
        return true;
      })
      .Case([&](const StructPattern *pattern) -> bool {
        auto *value = getValueFromVariant(valueOrMemory);
        if (!value->isa<StructValue>())
          return false;

        StructValue *structV = value->cast<StructValue>();
        const auto &idxMap = structMemberMap.at(structV->getDeclaration());
        for (auto [isRef, fieldName, fieldPattern] : pattern->getFields()) {
          if (fieldName.empty()) {
            assert(fieldPattern->isa<IdentifierPattern>() &&
                   "Unnamed field must be identifier pattern and it is field "
                   "name");
            fieldName = fieldPattern->cast<IdentifierPattern>()->getName();
          }

          auto it = idxMap.find(fieldName);
          if (it == idxMap.end()) {
            report(pattern->getRange(),
                   InterpretDiagnostic::error_unknown_struct_field, fieldName,
                   structV->getDeclaration()->getName());
            return false;
          }
          size_t fieldIdx = it->second;
          ValueMemory *fieldMem =
              structV->getMembers()[fieldIdx]->cast<ValueMemory>();
          if (isRef) {
            if (!matchPattern(*fieldPattern, fieldMem))
              return false;
          } else {
            if (!matchPattern(*fieldPattern, fieldMem->get()))
              return false;
          }
        }
        return true;
      })
      .Case([&](const TuplePattern *pattern) {
        auto *value = getValueFromVariant(valueOrMemory);
        if (!value->isa<TupleValue>())
          return false;

        auto patterns = pattern->getPatterns();
        auto *tupleValue = value->cast<TupleValue>();
        auto elements = tupleValue->getValues();
        if (patterns.size() != elements.size())
          return false;

        for (auto [pattern, element] : llvm::zip(patterns, elements)) {
          if (!matchPattern(*pattern, element))
            return false;
        }
        return true;
      })
      .Case([&](const GroupPattern *pattern) {
        return matchPattern(*pattern->getPattern(), valueOrMemory);
      })
      .Case([&](const IntegerPattern *pattern) {
        auto *value = getValueFromVariant(valueOrMemory);
        if (!value->isa<IntegerValue>())
          return false;
        auto *intValue = value->cast<IntegerValue>();
        return intValue->getValue() == pattern->getValue();
      })
      .Case([&](const StringPattern *pattern) {
        auto *value = getValueFromVariant(valueOrMemory);
        if (!value->isa<StringValue>())
          return false;
        auto *stringValue = value->cast<StringValue>();
        auto evaledPattern = evalStringLiteral(pattern->getValue());
        return evaledPattern == stringValue->getValue();
      })
      .Case([&](const BooleanPattern *pattern) {
        auto *value = getValueFromVariant(valueOrMemory);
        if (!value->isa<BoolValue>())
          return false;
        auto *boolValue = value->cast<BoolValue>();
        return boolValue->getValue() == pattern->getValue();
      })
      .Case([&](const FloatPattern *pattern) {
        auto *value = getValueFromVariant(valueOrMemory);
        if (!value->isa<FloatValue>())
          return false;
        auto *floatValue = value->cast<FloatValue>();
        llvm::APFloat patternValue(llvm::APFloat::IEEEdouble(),
                                   pattern->getValue());
        return floatValue->getValue() == patternValue;
      })
      .Case([&](const EmptyPattern *pattern) { return true; })
      .Default([&](const Pattern *pattern) {
        auto *value = getValueFromVariant(valueOrMemory);
        assert(pattern->isa<NilPattern>());
        return value->isa<NilValue>();
      });
}

static raw_ostream *printOS = &outs();

void setPrintOS(raw_ostream &os) { printOS = &os; }
raw_ostream &getPrintOS() { return *printOS; }

static const char *interpreterDiagMsgs[] = {
#define DIAG(Name, Msg, Error) Msg,
#include "bara/interpreter/InterpreterDiagnostic.def"
};

static llvm::SourceMgr::DiagKind interpreterDiagKinds[] = {
#define DIAG(Name, Msg, Error) llvm::SourceMgr::DK_##Error,
#include "bara/interpreter/InterpreterDiagnostic.def"
};

const char *StmtInterpreter::InterpretDiagnostic::getMessage(Diag kind) {
  return interpreterDiagMsgs[kind];
}
llvm::SourceMgr::DiagKind
StmtInterpreter::InterpretDiagnostic::getDiagKind(Diag kind) {
  return interpreterDiagKinds[kind];
}
} // namespace bara
