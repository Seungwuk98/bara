#ifndef BARA_EXPR_INTERPRETER_H
#define BARA_EXPR_INTERPRETER_H

#include "bara/ast/AST.h"
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
  std::variant<std::monostate, Memory *, unique_ptr<Value>, char>
  interpretIndex(const IndexExpression *expr);

  Environment &getEnv() { return stmtInterpreter->env; }

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

  unique_ptr<Value> getResult() { return std::move(result); }

  unique_ptr<Value> binaryOp(SMRange range, const Value *l, const Value *r,
                             Operator op);

private:
  friend class StmtInterpreter;

  unique_ptr<Value> result = nullptr;
};

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

  Memory *getResult() { return result; }

private:
  friend class StmtInterpreter;

  Memory *result;
};

template <typename ConcreteType>
Memory *CommonExprInterpreter<ConcreteType>::interpretIdentifier(
    const IdentifierExpression *expr) {
  auto *memory = getEnv().lookup(expr->getName());
  if (!memory) {
    stmtInterpreter->report(expr->getRange(),
                            InterpretDiagnostic::error_unknown_identifier,
                            expr->getName());
    return nullptr;
  }
  return memory;
}

template <typename ConcreteType>
std::variant<std::monostate, Memory *, unique_ptr<Value>, char>
CommonExprInterpreter<ConcreteType>::interpretIndex(
    const IndexExpression *expr) {
  using IndexReturnTy =
      std::variant<std::monostate, Memory *, unique_ptr<Value>, char>;
  auto value = stmtInterpreter->rvInterpret(*expr->getLhs());
  if (diag.hasError())
    return {};

  if (!value->isa<ListValue, TupleValue, StringValue>()) {
    stmtInterpreter->report(expr->getLhs()->getRange(),
                            InterpretDiagnostic::error_invalid_type_to_access,
                            value->toString());
    return {};
  }

  auto index = stmtInterpreter->rvInterpret(*expr->getRhs());
  if (diag.hasError())
    return {};

  if (!index->isa<IntegerValue>()) {
    stmtInterpreter->report(expr->getRhs()->getRange(),
                            InterpretDiagnostic::error_invalid_type_for_access,
                            index->toString());
    return {};
  }

  int64_t indexValue = index->cast<IntegerValue>()->getValue();
  return llvm::TypeSwitch<Value *, IndexReturnTy>(value.get())
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
        return list->getElement(indexValue);
      })
      .Default([&](const Value *value) -> IndexReturnTy {
        auto tuple = value->cast<TupleValue>();
        if (indexValue < 0 || indexValue >= tuple->size()) {
          stmtInterpreter->report(expr->getRange(),
                                  InterpretDiagnostic::error_out_of_range,
                                  indexValue, tuple->size());
          return {};
        }
        return tuple->getElement(indexValue)->clone();
      });
}

} // namespace bara
#endif // BARA_EXPR_INTERPRETER_H
