#ifndef BARA_EXPR_INTERPRETER_H
#define BARA_EXPR_INTERPRETER_H

#include "bara/ast/AST.h"
#include "bara/context/GarbageCollector.h"
#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"
#include "llvm/ADT/TypeSwitch.h"
#include <variant>

namespace bara {

template <typename ConcreteType>
class CommonExprInterpreter {

public:
  using InterpretDiagnostic = StmtInterpreter::InterpretDiagnostic;

  CommonExprInterpreter(StmtInterpreter *stmtInterpreter)
      : diag(stmtInterpreter->diag), context(stmtInterpreter->context),
        stmtInterpreter(stmtInterpreter) {}

  Memory *interpretIdentifier(const IdentifierExpression *expr);
  std::variant<std::monostate, GC::RootRegister, char>
  interpretIndex(const IndexExpression *expr);

  Environment &getCurrEnv() { return stmtInterpreter->getCurrEnv(); }

protected:
  Diagnostic &diag;
  MemoryContext *context;
  StmtInterpreter *stmtInterpreter;
};

class RvExprInterpreter : public CommonExprInterpreter<RvExprInterpreter>,
                          public ConstASTVisitorBase<RvExprInterpreter
#define EXPRESSION(Name) , Name
#include "bara/ast/Expression.def"
                                                     > {

public:
  RvExprInterpreter(StmtInterpreter *stmtInterpreter)
      : CommonExprInterpreter(stmtInterpreter) {}

#define EXPRESSION(Name) void visit(const Name &expr);
#include "bara/ast/Expression.def"

  GC::RootRegister getResult() { return std::move(result); }

  optional<GC::RootRegister> binaryOp(SMRange range, const Value *l,
                                      const Value *r, Operator op);

private:
  friend class StmtInterpreter;

  GC::RootRegister result;
};

string evalStringLiteral(StringRef buffer);

class LvExprInterpreter : public CommonExprInterpreter<LvExprInterpreter>,
                          public ConstASTVisitorBase<LvExprInterpreter
#define EXPRESSION(Name) , Name
#include "bara/ast/Expression.def"
                                                     > {
public:
  LvExprInterpreter(StmtInterpreter *stmtInterpreter)
      : CommonExprInterpreter(stmtInterpreter) {}

#define EXPRESSION(Name) void visit(const Name &expr);
#include "bara/ast/Expression.def"

  GC::RootRegister getResult() { return std::move(result); }

private:
  friend class StmtInterpreter;

  GC::RootRegister result;
};

template <typename ConcreteType>
Memory *CommonExprInterpreter<ConcreteType>::interpretIdentifier(
    const IdentifierExpression *expr) {
  auto *memory = getCurrEnv().lookup(expr->getName());
  if (!memory) {
    stmtInterpreter->report(expr->getRange(),
                            InterpretDiagnostic::error_unknown_identifier,
                            expr->getName());
    return nullptr;
  }

  return memory;
}

template <typename ConcreteType>
std::variant<std::monostate, GC::RootRegister, char>
CommonExprInterpreter<ConcreteType>::interpretIndex(
    const IndexExpression *expr) {
  using IndexReturnTy = std::variant<std::monostate, GC::RootRegister, char>;
  auto value = stmtInterpreter->rvInterpret(*expr->getLhs());
  if (diag.hasError())
    return {};

  if (!value.getValue()->isa<ListValue, TupleValue, StringValue>()) {
    stmtInterpreter->report(expr->getLhs()->getRange(),
                            InterpretDiagnostic::error_invalid_type_to_access,
                            value.getValue()->toString());
    return {};
  }

  auto index = stmtInterpreter->rvInterpret(*expr->getRhs());
  if (diag.hasError())
    return {};

  if (!index.getValue()->isa<IntegerValue>()) {
    stmtInterpreter->report(expr->getRhs()->getRange(),
                            InterpretDiagnostic::error_invalid_type_for_access,
                            index.getValue()->toString());
    return {};
  }

  int64_t indexValue = index.getValue()->cast<IntegerValue>()->getValue();
  return llvm::TypeSwitch<Value *, IndexReturnTy>(value.getValue())
      .Case([&](const StringValue *str) -> IndexReturnTy {
        if (indexValue < 0 || indexValue >= str->getValue().size()) {
          stmtInterpreter->report(expr->getRange(),
                                  InterpretDiagnostic::error_out_of_range,
                                  indexValue, str->getValue().size());
          return {};
        }
        return str->getValue()[indexValue];
      })
      .Case([&](const ListValue *list) -> IndexReturnTy {
        if (indexValue < 0 || indexValue >= list->size()) {
          stmtInterpreter->report(expr->getRange(),
                                  InterpretDiagnostic::error_out_of_range,
                                  indexValue, list->size());
          return {};
        }
        return context->getGC()->registerRoot(list->get(indexValue));
      })
      .Default([&](const Value *value) -> IndexReturnTy {
        auto tuple = value->cast<TupleValue>();
        if (indexValue < 0 || indexValue >= tuple->size()) {
          stmtInterpreter->report(expr->getRange(),
                                  InterpretDiagnostic::error_out_of_range,
                                  indexValue, tuple->size());
          return {};
        }
        return context->getGC()->registerRoot(tuple->getElement(indexValue));
      });
}

} // namespace bara
#endif // BARA_EXPR_INTERPRETER_H
