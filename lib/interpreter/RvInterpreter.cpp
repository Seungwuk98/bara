#include "bara/ast/AST.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"
#include <limits>
#include <llvm-18/llvm/ADT/TypeSwitch.h>

namespace bara {

template <typename L, typename R>
optional<pair<const L *, const R *>> matchPair(const Value *l, const Value *r) {
  if (auto *l1 = l->dyn_cast<L>())
    if (auto *r1 = r->dyn_cast<R>())
      return std::make_pair(l1, r1);
  return nullopt;
}

namespace BinaryOp {
extern unique_ptr<Value> add(const Value *l, const Value *r);
extern unique_ptr<Value> sub(const Value *l, const Value *r);
extern unique_ptr<Value> mul(const Value *l, const Value *r);
extern unique_ptr<Value> div(const Value *l, const Value *r);
extern unique_ptr<Value> mod(const Value *l, const Value *r);
extern unique_ptr<Value> eq(const Value *l, const Value *r);
extern unique_ptr<Value> ne(const Value *l, const Value *r);
extern unique_ptr<Value> lt(const Value *l, const Value *r);
extern unique_ptr<Value> le(const Value *l, const Value *r);
extern unique_ptr<Value> gt(const Value *l, const Value *r);
extern unique_ptr<Value> ge(const Value *l, const Value *r);
extern unique_ptr<Value> logicalAnd(const Value *l, const Value *r);
extern unique_ptr<Value> logicalOr(const Value *l, const Value *r);
extern unique_ptr<Value> bitAnd(const Value *l, const Value *r);
extern unique_ptr<Value> bitOr(const Value *l, const Value *r);
extern unique_ptr<Value> bitXor(const Value *l, const Value *r);
extern unique_ptr<Value> shl(const Value *l, const Value *r);
extern unique_ptr<Value> shr(const Value *l, const Value *r);

} // namespace BinaryOp

unique_ptr<Value> RvExprInterpreter::binaryOp(SMRange range, const Value *l,
                                              const Value *r, Operator op) {
  switch (op) {
#define BINARY_OP(op, func)                                                    \
  case Operator::op: {                                                         \
    auto result = BinaryOp::func(l, r);                                        \
    if (result)                                                                \
      return result;                                                           \
    break;                                                                     \
  }
    BINARY_OP(Plus, add)
    BINARY_OP(Minus, sub)
    BINARY_OP(Mul, mul)
    BINARY_OP(Div, div)
    BINARY_OP(Mod, mod)
    BINARY_OP(Eq, eq)
    BINARY_OP(Ne, ne)
    BINARY_OP(Lt, lt)
    BINARY_OP(Le, le)
    BINARY_OP(Gt, gt)
    BINARY_OP(Ge, ge)
    BINARY_OP(BitAnd, bitAnd)
    BINARY_OP(BitOr, bitOr)
    BINARY_OP(BitXor, bitXor)
    BINARY_OP(Shl, shl)
    BINARY_OP(Shr, shr)

#undef BINARY_OP
  default:
    break;
  }
  stmtInterpreter->report(
      range, InterpretDiagnostic::error_invalid_operand_for_binary_operator,
      operatorToString(op), l->toString(), r->toString());
  return nullptr;
}

void RvExprInterpreter::visit(const IdentifierExpression &expr) {
  auto *memory = interpretIdentifier(&expr);
  if (diag.hasError())
    return;

  auto *valueMemory = memory->cast<ValueMemory>();
  result = valueMemory->view()->clone();
}

void RvExprInterpreter::visit(const IndexExpression &expr) {
  auto memoryOrChar = interpretIndex(&expr);
  if (diag.hasError())
    return;

  if (std::holds_alternative<char>(memoryOrChar)) {
    result = StringValue::create({&std::get<char>(memoryOrChar), 1});
    return;
  }

  auto *valueMemory = std::get<Memory *>(memoryOrChar)->cast<ValueMemory>();
  result = valueMemory->view()->clone();
}

void RvExprInterpreter::visit(const MatchExpression &expr) {
  llvm_unreachable("TODO");
}

void RvExprInterpreter::visit(const LambdaExpression &expr) {
  llvm_unreachable("TODO");
}

void RvExprInterpreter::visit(const BinaryExpression &expr) {
  expr.getLhs()->accept(*this);
  if (diag.hasError())
    return;
  auto lhs = std::move(result);

  expr.getRhs()->accept(*this);
  if (diag.hasError())
    return;
  auto rhs = std::move(result);

  result = binaryOp(expr.getRange(), lhs.get(), rhs.get(), expr.getOperator());
}

void RvExprInterpreter::visit(const UnaryExpression &expr) {
  expr.getExpr()->accept(*this);
  if (diag.hasError())
    return;
  auto operand = std::move(result);

  switch (expr.getOperator()) {
  case Operator::Plus:
    llvm::TypeSwitch<Value *>(operand.get())
        .Case(
            [&](const IntegerValue *integerV) { result = std::move(operand); })
        .Case([&](const FloatValue *floatV) { result = std::move(operand); })
        .Case([&](const BoolValue *boolV) {
          result = IntegerValue::create(boolV->getValue());
        })
        .Default([this, expr](const Value *value) {
          stmtInterpreter->report(
              expr.getRange(),
              InterpretDiagnostic::error_invalid_operand_for_unary_operator,
              operatorToString(expr.getOperator()), value->toString());
        });
    return;

  case Operator::Minus:
    llvm::TypeSwitch<Value *>(operand.get())
        .Case([&](const BoolValue *boolV) {
          result = IntegerValue::create(-boolV->getValue());
        })
        .Case([&](const IntegerValue *integerV) {
          result = IntegerValue::create(-integerV->getValue());
        })
        .Case([&](const FloatValue *floatV) {
          result = FloatValue::create(-floatV->getValue());
        })
        .Default([&](const Value *value) {
          stmtInterpreter->report(
              expr.getRange(),
              InterpretDiagnostic::error_invalid_operand_for_unary_operator,
              operatorToString(expr.getOperator()), value->toString());
        });
    return;

  case Operator::Not: {
    auto boolOpt = operand->toBool();
    if (!boolOpt) {
      stmtInterpreter->report(
          expr.getRange(),
          InterpretDiagnostic::error_invalid_operand_for_unary_operator,
          operatorToString(expr.getOperator()), operand->toString());
      return;
    }
    result = BoolValue::create(!*boolOpt);
    return;
  }

  case Operator::BitNot: {
    llvm::TypeSwitch<Value *>(operand.get())
        .Case([&](const BoolValue *boolV) {
          result = IntegerValue::create(!boolV->getValue());
        })
        .Case([&](const IntegerValue *integerV) {
          result = IntegerValue::create(~integerV->getValue());
        })
        .Default([&](const Value *value) {
          stmtInterpreter->report(
              expr.getRange(),
              InterpretDiagnostic::error_invalid_operand_for_unary_operator,
              operatorToString(expr.getOperator()), value->toString());
        });
  }
  default:
    llvm_unreachable("never used for unary operation");
  }
}

void RvExprInterpreter::visit(const CallExpression &expr) {
  expr.getCallee()->accept(*this);
  if (diag.hasError())
    return;
  auto callee = std::move(result);
  if (!callee->isa<FunctionValue, LambdaValue, BuiltinFunctionValue>()) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::error_invalid_callee,
                            callee->toString());
    return;
  }

  SmallVector<unique_ptr<Value>> args;
  args.reserve(expr.getArgs().size());
  for (auto *argExpr : expr.getArgs()) {
    argExpr->accept(*this);
    if (diag.hasError())
      return;
    args.emplace_back(std::move(result));
  }

  llvm::TypeSwitch<Value *, void>(callee.get())
      // User defined function
      .Case([&](FunctionValue *functionV) {
        StmtInterpreter::ReplaceEnvScope replace(*stmtInterpreter,
                                                 functionV->getEnvironment());
        Environment::Scope scope(getEnv());

        auto *funcDecl = functionV->getDeclaration();

        /// for recursive call
        getEnv().insert(funcDecl->getName(),
                        ImmutableMemory::create(context, functionV->clone()));
        auto params = funcDecl->getParams();

        if (params.size() != args.size()) {
          stmtInterpreter->report(
              expr.getRange(), InterpretDiagnostic::error_invalid_argument_size,
              params.size(), args.size());
          stmtInterpreter->report(funcDecl->getRange(),
                                  InterpretDiagnostic::note_function);
          return;
        }

        for (const auto &[pattern, value] : llvm::zip(params, args)) {
          if (!stmtInterpreter->matchPattern(*pattern, value.get())) {
            stmtInterpreter->report(
                expr.getRange(), InterpretDiagnostic::error_match_pattern_fail,
                pattern->toString(), value->toString());
            return;
          }
        }

        Environment::Scope bodyScope(getEnv());

        for (auto *body : funcDecl->getBody()) {
          body->accept(*stmtInterpreter);
          if (stmtInterpreter->isTerminated())
            break;
        }

        if (stmtInterpreter->continueFlag) {
          stmtInterpreter->report(
              stmtInterpreter->continueFlag->getRange(),
              InterpretDiagnostic::error_unresolved_continue_statement);
        } else if (stmtInterpreter->breakFlag) {
          stmtInterpreter->report(
              stmtInterpreter->breakFlag->getRange(),
              InterpretDiagnostic::error_unresolved_break_statement);
        } else if (stmtInterpreter->returnFlag) {
          result = std::move(stmtInterpreter->returnValue);
        } else {
          result = NilValue::create();
        }
      })
      // Lambda function
      .Case([&](LambdaValue *lambdaV) {
        StmtInterpreter::ReplaceEnvScope replaceScope(
            *stmtInterpreter, lambdaV->getEnvironment());
        Environment::Scope scope(getEnv());

        auto lambdaDecl = lambdaV->getExpression();

        auto params = lambdaDecl->getParams();
        if (params.size() != args.size()) {
          stmtInterpreter->report(
              expr.getRange(), InterpretDiagnostic::error_invalid_argument_size,
              params.size(), args.size());
          return;
        }

        for (const auto &[pattern, value] : llvm::zip(params, args)) {
          if (!stmtInterpreter->matchPattern(*pattern, value.get())) {
            stmtInterpreter->report(
                expr.getRange(), InterpretDiagnostic::error_match_pattern_fail,
                pattern->toString(), value->toString());
            return;
          }
        }

        Environment::Scope bodyScope(getEnv());

        if (lambdaDecl->isExprBody()) {
          auto *body = lambdaDecl->getExpr();
          body->accept(*this);
        } else {
          for (auto *body : lambdaDecl->getStmtBody()) {
            body->accept(*stmtInterpreter);
            if (stmtInterpreter->isTerminated())
              break;
          }

          if (stmtInterpreter->continueFlag) {
            stmtInterpreter->report(
                stmtInterpreter->continueFlag->getRange(),
                InterpretDiagnostic::error_unresolved_continue_statement);
          } else if (stmtInterpreter->breakFlag) {
            stmtInterpreter->report(
                stmtInterpreter->breakFlag->getRange(),
                InterpretDiagnostic::error_unresolved_break_statement);
          } else if (stmtInterpreter->returnFlag) {
            result = std::move(stmtInterpreter->returnValue);
          } else {
            result = NilValue::create();
          }
        }
      })
      .Case([&](BuiltinFunctionValue *builtinV) {
        auto func = builtinV->getFuncBody();
        result = func(args, diag);
      })
      .Default(
          [&](Value *) { llvm_unreachable("never used for call operation"); });
}

void RvExprInterpreter::visit(const ArrayExpression &expr) {
  SmallVector<ValueMemory *> memories;
  memories.reserve(expr.getSize());

  for (auto *elementExpr : expr.getArgs()) {
    elementExpr->accept(*this);
    if (diag.hasError())
      return;

    auto *valueMemory = ValueMemory::create(context, std::move(result));
    memories.emplace_back(valueMemory);
  }

  auto *vectorMemory = VectorMemory::create(context, memories);
  result = ListValue::create(vectorMemory);
}

void RvExprInterpreter::visit(const TupleExpression &expr) {
  SmallVector<ValueMemory *> memories;
  memories.reserve(expr.getSize());

  for (auto *elementExpr : expr.getExprs()) {
    elementExpr->accept(*this);
    if (diag.hasError())
      return;

    auto *valueMemory = ValueMemory::create(context, std::move(result));
    memories.emplace_back(valueMemory);
  }

  result = TupleValue::create(memories);
}

void RvExprInterpreter::visit(const GroupExpression &expr) {
  expr.getExpr()->accept(*this);
}

void RvExprInterpreter::visit(const IntegerLiteral &expr) {
  if (expr.getValue() >=
      static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::warning_integer_overflow);
  }

  result = IntegerValue::create(expr.getValue());
}

void RvExprInterpreter::visit(const BooleanLiteral &expr) {
  result = BoolValue::create(expr.getValue());
}

void RvExprInterpreter::visit(const FloatLiteral &expr) {
  result = FloatValue::create(expr.getValue());
}

void RvExprInterpreter::visit(const StringLiteral &expr) {
  string value;
  value.reserve(expr.getValue().size());
  raw_string_ostream os(value);

  StringRef buffer = expr.getValue();
  assert(expr.getValue().front() == '"');
  /// first string must be a double quote
  auto pos = 1;
  while (pos < buffer.size() - 1) {
    auto ch = buffer[pos++];
    if (ch == '\\') {
      /// This asser must be guaranteed by lexer
      assert(pos < buffer.size() - 1);
      ch = buffer[pos++];

      switch (ch) {
        // clang-format off
      case 'n':   os << '\n';   break;
      case 't':   os << '\t';   break;
      case 'f':   os << '\f';   break;
      case 'r':   os << '\r';   break;
      case 'v':   os << '\v';   break;
      case 'b':   os << '\b';   break;
      case '\'':  os << '\'';   break;
      case '\"':  os << '\"';   break;
      case 0:     os << '\0';   break;
      case '\\':  os << '\\';   break;
        // clang-format on
      case '\n':
        /// "abcd \
        ///       efg" => "abcdefg"
        while (pos < buffer.size() - 1 && std::isspace(buffer[pos]))
          pos++;
        break;

      default:
        os << ch;
      }
    } else {
      os << buffer[pos];
    }
  }

  result = StringValue::create(os.str());
}

void RvExprInterpreter::visit(const NilLiteral &expr) {
  result = NilValue::create();
}

} // namespace bara
