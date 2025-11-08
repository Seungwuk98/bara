#include "bara/ast/AST.h"
#include "bara/context/GarbageCollector.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"
#include "llvm/ADT/SmallVectorExtras.h"
#include "llvm/ADT/TypeSwitch.h"
#include <limits>
#include <variant>

namespace bara {

namespace BinaryOp {
extern Value *add(const Value *l, const Value *r);
extern Value *sub(const Value *l, const Value *r);
extern Value *mul(const Value *l, const Value *r);
extern Value *div(const Value *l, const Value *r);
extern Value *mod(const Value *l, const Value *r);
extern Value *eq(const Value *l, const Value *r);
extern Value *ne(const Value *l, const Value *r);
extern Value *lt(const Value *l, const Value *r);
extern Value *le(const Value *l, const Value *r);
extern Value *gt(const Value *l, const Value *r);
extern Value *ge(const Value *l, const Value *r);
extern Value *logicalAnd(const Value *l, const Value *r);
extern Value *logicalOr(const Value *l, const Value *r);
extern Value *bitAnd(const Value *l, const Value *r);
extern Value *bitOr(const Value *l, const Value *r);
extern Value *bitXor(const Value *l, const Value *r);
extern Value *shl(const Value *l, const Value *r);
extern Value *shr(const Value *l, const Value *r);

} // namespace BinaryOp

optional<GC::RootRegister> RvExprInterpreter::binaryOp(SMRange range,
                                                       const Value *l,
                                                       const Value *r,
                                                       Operator op) {
  switch (op) {
#define BINARY_OP(op, func)                                                    \
  case Operator::op: {                                                         \
    GCSAFE(context->getGC()) {                                                 \
      auto result = BinaryOp::func(l, r);                                      \
      if (result)                                                              \
        return context->getGC()->registerRoot(result);                         \
      break;                                                                   \
    }                                                                          \
  }
    BINARY_OP(Plus, add)
    BINARY_OP(Minus, sub)
    BINARY_OP(Mul, mul)
    BINARY_OP(Div, div)
    BINARY_OP(Mod, mod)
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
    llvm_unreachable("never used for binary operation");
  }
  return nullopt;
}

string evalStringLiteral(StringRef buffer) {
  string value;
  value.reserve(buffer.size());
  raw_string_ostream os(value);

  assert(buffer.front() == '"');
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
      os << ch;
    }
  }
  return os.str();
}

void RvExprInterpreter::visit(const IdentifierExpression &expr) {
  auto *memory = interpretIdentifier(&expr);
  if (isTerminated())
    return;

  if (auto *valueMemory = memory->dyn_cast<ValueMemory>())
    result = context->getGC()->registerRoot(valueMemory->get());
  else {
    auto immutMemory = memory->cast<ImmutableMemory>();
    result = context->getGC()->registerRoot(immutMemory->get());
  }
}

void RvExprInterpreter::visit(const IndexExpression &expr) {
  auto memoryOrValueOrChar = interpretIndex(&expr);
  if (isTerminated())
    return;

  if (std::holds_alternative<char>(memoryOrValueOrChar)) {
    GCSAFE(context->getGC()) {
      auto strValue = StringValue::create(
          context, {&std::get<char>(memoryOrValueOrChar), 1});
      result = context->getGC()->registerRoot(strValue);
    }
    return;
  }

  if (auto *gcRegistor = std::get_if<GC::RootRegister>(&memoryOrValueOrChar)) {
    if (gcRegistor->hasValue())
      result = std::move(*gcRegistor);
    else {
      auto *valueMemory = gcRegistor->getMemory()->cast<ValueMemory>();
      result = context->getGC()->registerRoot(valueMemory->get());
    }
    return;
  }

  llvm_unreachable("All cases should be handled");
}

void RvExprInterpreter::visit(const MatchExpression &expr) {
  expr.getExpr()->accept(*this);
  if (isTerminated())
    return;
  auto valueR = std::move(result);
  auto value = valueR.getValue();

  for (const auto &[pattern, expr] : expr.getMatchCases()) {
    Environment::Scope scope(getCurrEnv());
    if (stmtInterpreter->matchPattern(*pattern, value)) {
      expr->accept(*this);
      return;
    }
  }

  stmtInterpreter->report(expr.getRange(),
                          InterpretDiagnostic::error_unmatched_pattern,
                          value->toString());
}

void RvExprInterpreter::visit(const LambdaExpression &expr) {
  GCSAFE(context->getGC()) {
    auto lambdaV = LambdaValue::create(context, getCurrEnv(), &expr);
    result = context->getGC()->registerRoot(lambdaV);
  }
}

void RvExprInterpreter::visit(const StructAccessExpression &expr) {
  expr.getBase()->accept(*this);
  if (isTerminated())
    return;

  auto base = std::move(result);
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
      result =
          context->getGC()->registerRoot(memberV->cast<ValueMemory>()->get());
    }
  } else {
    stmtInterpreter->report(
        expr.getRange(), InterpretDiagnostic::error_unknown_struct_field,
        expr.getFieldName(), structV->getDeclaration()->getName());
    return;
  }
}

void RvExprInterpreter::visit(const BinaryExpression &expr) {
  switch (expr.getOperator()) {
  case Operator::And: {
    expr.getLhs()->accept(*this);
    if (isTerminated())
      return;
    auto lhs = std::move(result);
    auto lhsBoolOpt = lhs.getValue()->toBool();
    if (!lhsBoolOpt) {
      stmtInterpreter->report(
          expr.getLhs()->getRange(),
          InterpretDiagnostic::error_invalid_operand_for_binary_operator,
          operatorToString(expr.getOperator()), lhs.getValue()->toString());
      return;
    }

    if (!*lhsBoolOpt) {
      GCSAFE(context->getGC()) {
        auto boolV = BoolValue::create(context, false);
        result = context->getGC()->registerRoot(boolV);
      }
      return;
    }

    expr.getRhs()->accept(*this);
    if (isTerminated())
      return;
    auto rhs = std::move(result);
    auto rhsBoolOpt = rhs.getValue()->toBool();
    if (!rhsBoolOpt) {
      stmtInterpreter->report(
          expr.getRhs()->getRange(),
          InterpretDiagnostic::error_invalid_operand_for_binary_operator,
          operatorToString(expr.getOperator()), rhs.getValue()->toString());
      return;
    }

    GCSAFE(context->getGC()) {
      auto boolV = BoolValue::create(context, *rhsBoolOpt);
      result = context->getGC()->registerRoot(boolV);
    }
    return;
  }
  case Operator::Or: {
    expr.getLhs()->accept(*this);
    if (isTerminated())
      return;
    auto lhs = std::move(result);
    auto lhsBoolOpt = lhs.getValue()->toBool();
    if (!lhsBoolOpt) {
      stmtInterpreter->report(
          expr.getLhs()->getRange(),
          InterpretDiagnostic::error_invalid_operand_for_binary_operator,
          operatorToString(expr.getOperator()), lhs.getValue()->toString());
      return;
    }

    if (*lhsBoolOpt) {
      GCSAFE(context->getGC()) {
        auto boolV = BoolValue::create(context, true);
        result = context->getGC()->registerRoot(boolV);
      }
      return;
    }

    expr.getRhs()->accept(*this);
    if (isTerminated())
      return;
    auto rhs = std::move(result);
    auto rhsBoolOpt = rhs.getValue()->toBool();
    if (!rhsBoolOpt) {
      stmtInterpreter->report(
          expr.getRhs()->getRange(),
          InterpretDiagnostic::error_invalid_operand_for_binary_operator,
          operatorToString(expr.getOperator()), rhs.getValue()->toString());
      return;
    }

    GCSAFE(context->getGC()) {
      auto boolV = BoolValue::create(context, *rhsBoolOpt);
      result = context->getGC()->registerRoot(boolV);
    }
    return;
  }
  case Operator::Eq: {
    expr.getLhs()->accept(*this);
    if (isTerminated())
      return;
    auto lhs = std::move(result);

    expr.getRhs()->accept(*this);
    if (isTerminated())
      return;
    auto rhs = std::move(result);

    auto isSame = lhs.getValue()->isEqual(rhs.getValue());
    GCSAFE(context->getGC()) {
      auto boolV = BoolValue::create(context, isSame);
      result = context->getGC()->registerRoot(boolV);
    }
    return;
  }
  case Operator::Ne: {
    expr.getLhs()->accept(*this);
    if (isTerminated())
      return;
    auto lhs = std::move(result);

    expr.getRhs()->accept(*this);
    if (isTerminated())
      return;
    auto rhs = std::move(result);

    auto isSame = lhs.getValue()->isEqual(rhs.getValue());
    GCSAFE(context->getGC()) {
      auto boolV = BoolValue::create(context, !isSame);
      result = context->getGC()->registerRoot(boolV);
    }
    return;
  }
  default:
    expr.getLhs()->accept(*this);
    if (isTerminated())
      return;
    auto lhs = std::move(result);

    expr.getRhs()->accept(*this);
    if (isTerminated())
      return;
    auto rhs = std::move(result);

    auto resultOpt = binaryOp(expr.getRange(), lhs.getValue(), rhs.getValue(),
                              expr.getOperator());
    if (!resultOpt) {
      stmtInterpreter->report(
          expr.getRange(),
          InterpretDiagnostic::error_invalid_operand_for_binary_operator,
          operatorToString(expr.getOperator()), lhs.getValue()->toString(),
          rhs.getValue()->toString());
      return;
    }
    result = std::move(*resultOpt);
  }
}

void RvExprInterpreter::visit(const AssignmentExpression &stmt) {
  auto lR = stmtInterpreter->lvInterpret(*stmt.getLhs());
  if (isTerminated())
    return;
  if (stmt.getOperator() && !lR.getMemory()->isa<ValueMemory>()) {
    stmtInterpreter->report(
        stmt.getLhs()->getRange(),
        InterpretDiagnostic::error_invalid_left_size_of_operator_assignment,
        operatorToString(*stmt.getOperator()) + "=");
    return;
  }

  if (stmt.getOperator()) {
    auto *valueMemory = lR.getMemory()->cast<ValueMemory>();
    auto lhsV = valueMemory->get();

    auto rhsR = stmtInterpreter->rvInterpret(*stmt.getRhs());
    if (stmtInterpreter->isTerminated())
      return;
    auto rhsV = rhsR.getValue();

    optional<GC::RootRegister> assignValue;
    if (stmt.getOperator()) {
      assignValue = binaryOp(stmt.getRange(), lhsV, rhsV, *stmt.getOperator());
      if (isTerminated())
        return;
    } else {
      GCSAFE(context->getGC()) {
        assignValue = context->getGC()->registerRoot(rhsV);
      }
    }
    if (!assignValue) {
      stmtInterpreter->report(
          stmt.getRange(),
          InterpretDiagnostic::error_invalid_operand_for_binary_operator,
          (stmt.getOperator() ? operatorToString(*stmt.getOperator()) : "") +
              "=",
          lhsV->toString(), rhsV->toString());
      return;
    }

    valueMemory->assign(assignValue->getValue());
    result = std::move(*assignValue);
  } else {
    auto rhsR = stmtInterpreter->rvInterpret(*stmt.getRhs());
    if (stmtInterpreter->isTerminated())
      return;

    if (!lR.getMemory()->assign(rhsR.getValue())) {
      stmtInterpreter->report(stmt.getRange(),
                              InterpretDiagnostic::error_assignment_fail,
                              rhsR.getValue()->toString());
      return;
    }
    result = std::move(rhsR);
  }
}
void RvExprInterpreter::visit(const UnaryExpression &expr) {
  expr.getExpr()->accept(*this);
  if (isTerminated())
    return;
  auto operand = std::move(result);

  switch (expr.getOperator()) {
  case Operator::Plus:
    llvm::TypeSwitch<Value *>(operand.getValue())
        .Case(
            [&](const IntegerValue *integerV) { result = std::move(operand); })
        .Case([&](const FloatValue *floatV) { result = std::move(operand); })
        .Case([&](const BoolValue *boolV) {
          GCSAFE(context->getGC()) {
            auto intV = IntegerValue::create(context, boolV->getValue());
            result = context->getGC()->registerRoot(intV);
          }
        })
        .Case([&](const StringValue *strV) {
          if (strV->size() == 1) {
            GCSAFE(context->getGC()) {
              auto intV = IntegerValue::create(context, strV->getValue()[0]);
              result = context->getGC()->registerRoot(intV);
            }
          } else {
            stmtInterpreter->report(
                expr.getRange(),
                InterpretDiagnostic::error_invalid_operand_for_unary_operator,
                strV->toString());
          }
        })
        .Default([this, expr](const Value *value) {
          stmtInterpreter->report(
              expr.getRange(),
              InterpretDiagnostic::error_invalid_operand_for_unary_operator,
              operatorToString(expr.getOperator()), value->toString());
        });
    return;

  case Operator::Minus:
    GCSAFE(context->getGC()) {
      llvm::TypeSwitch<Value *>(operand.getValue())
          .Case([&](const BoolValue *boolV) {
            auto minusV = IntegerValue::create(context, -boolV->getValue());
            result = context->getGC()->registerRoot(minusV);
          })
          .Case([&](const IntegerValue *integerV) {
            auto minusV = IntegerValue::create(context, -integerV->getValue());
            result = context->getGC()->registerRoot(minusV);
          })
          .Case([&](const FloatValue *floatV) {
            auto minusV = FloatValue::create(context, -floatV->getValue());
            result = context->getGC()->registerRoot(minusV);
          })
          .Default([&](const Value *value) {
            stmtInterpreter->report(
                expr.getRange(),
                InterpretDiagnostic::error_invalid_operand_for_unary_operator,
                operatorToString(expr.getOperator()), value->toString());
          });
    }
    return;

  case Operator::Not: {
    auto boolOpt = operand.getValue()->toBool();
    if (!boolOpt) {
      stmtInterpreter->report(
          expr.getRange(),
          InterpretDiagnostic::error_invalid_operand_for_unary_operator,
          operatorToString(expr.getOperator()), operand.getValue()->toString());
      return;
    }
    GCSAFE(context->getGC()) {
      auto notV = BoolValue::create(context, !*boolOpt);
      result = context->getGC()->registerRoot(notV);
    }
    return;
  }

  case Operator::BitNot: {
    GCSAFE(context->getGC()) {
      llvm::TypeSwitch<Value *>(operand.getValue())
          .Case([&](const BoolValue *boolV) {
            auto notV = IntegerValue::create(context, !boolV->getValue());
            result = context->getGC()->registerRoot(notV);
          })
          .Case([&](const IntegerValue *integerV) {
            auto notV = IntegerValue::create(context, ~integerV->getValue());
            result = context->getGC()->registerRoot(notV);
          })
          .Default([&](const Value *value) {
            stmtInterpreter->report(
                expr.getRange(),
                InterpretDiagnostic::error_invalid_operand_for_unary_operator,
                operatorToString(expr.getOperator()), value->toString());
          });
    }
  }
  default:
    llvm_unreachable("never used for unary operation");
  }
}

void RvExprInterpreter::visit(const ConditionalExpression &expr) {
  expr.getCond()->accept(*this);
  if (isTerminated())
    return;
  auto cond = std::move(result);

  auto condBoolOpt = cond.getValue()->toBool();
  if (!condBoolOpt) {
    stmtInterpreter->report(
        expr.getCond()->getRange(),
        InterpretDiagnostic::error_invalid_to_conver_boolean,
        cond.getValue()->toString());
    return;
  }

  if (*condBoolOpt)
    expr.getThenExpr()->accept(*this);
  else
    expr.getElseExpr()->accept(*this);
}

void RvExprInterpreter::visit(const CallExpression &expr) {
  expr.getCallee()->accept(*this);
  if (isTerminated())
    return;

  auto callee = std::move(result);
  assert(callee.hasValue() && "Callee must have value");
  if (!callee.getValue()
           ->isa<FunctionValue, LambdaValue, BuiltinFunctionValue>()) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::error_invalid_callee,
                            callee.getValue()->toString());
    return;
  }

  SmallVector<GC::RootRegister> args;
  args.reserve(expr.getArgs().size());
  for (auto *argExpr : expr.getArgs()) {
    argExpr->accept(*this);
    if (isTerminated())
      return;
    args.emplace_back(std::move(result));
  }

  llvm::TypeSwitch<Value *, void>(callee.getValue())
      // User defined function
      .Case([&](FunctionValue *functionV) {
        Environment savedEnv(context->getBuiltinFuncTable(),
                             functionV->getEnvVars());
        StmtInterpreter::StackScope stackScope(*stmtInterpreter,
                                               std::move(savedEnv));
        Environment::Scope scope(getCurrEnv());

        auto *funcDecl = functionV->getDeclaration();

        /// for recursive call
        GCSAFE(context->getGC()) {
          getCurrEnv().insert(funcDecl->getName(),
                              ImmutableMemory::create(context, functionV));
        }
        auto params = funcDecl->getParams();

        if (params.size() != args.size()) {
          stmtInterpreter->report(
              expr.getRange(), InterpretDiagnostic::error_invalid_argument_size,
              params.size(), args.size());
          stmtInterpreter->report(funcDecl->getRange(),
                                  InterpretDiagnostic::note_function);
          return;
        }

        for (const auto &[ident, value] : llvm::zip(params, args)) {
          if (getCurrEnv().isDefinedCurrScope(ident)) {
            stmtInterpreter->report(
                expr.getRange(),
                InterpretDiagnostic::error_redefinition_in_scope, ident);
            return;
          }
          GCSAFE(context->getGC()) {
            getCurrEnv().insert(ident,
                                ValueMemory::create(context, value.getValue()));
          }
        }

        Environment::Scope bodyScope(getCurrEnv());

        for (auto *body : funcDecl->getBody()) {
          body->accept(*stmtInterpreter);
          if (stmtInterpreter->isTerminated())
            break;
        }
        if (diag.hasError()) {
          stmtInterpreter->report(
              expr.getRange(),
              InterpretDiagnostic::note_dump_function_call_stack,
              funcDecl->getName());
        } else if (stmtInterpreter->continueFlag) {
          stmtInterpreter->report(
              stmtInterpreter->continueFlag->getRange(),
              InterpretDiagnostic::error_unresolved_continue_statement);
        } else if (stmtInterpreter->breakFlag) {
          stmtInterpreter->report(
              stmtInterpreter->breakFlag->getRange(),
              InterpretDiagnostic::error_unresolved_break_statement);
        } else if (stmtInterpreter->returnFlag) {
          result = context->getGC()->registerRoot(stmtInterpreter->returnValue);
          stmtInterpreter->returnFlag = nullptr;
          stmtInterpreter->returnValue = nullptr;
        } else {
          GCSAFE(context->getGC()) {
            auto nilV = NilValue::create(context);
            result = context->getGC()->registerRoot(nilV);
          }
        }
      })
      // Lambda function
      .Case([&](LambdaValue *lambdaV) {
        Environment capturedEnv(context->getBuiltinFuncTable(),
                                lambdaV->getCaptures());
        StmtInterpreter::StackScope replaceScope(*stmtInterpreter,
                                                 std::move(capturedEnv));
        Environment::Scope scope(getCurrEnv());

        auto lambdaDecl = lambdaV->getExpression();

        auto params = lambdaDecl->getParams();
        if (params.size() != args.size()) {
          stmtInterpreter->report(
              expr.getRange(), InterpretDiagnostic::error_invalid_argument_size,
              params.size(), args.size());
          return;
        }

        for (const auto &[param, value] : llvm::zip(params, args)) {
          if (getCurrEnv().isDefinedCurrScope(param)) {
            stmtInterpreter->report(
                expr.getRange(),
                InterpretDiagnostic::error_redefinition_in_scope, param);
            return;
          }
          GCSAFE(context->getGC()) {
            getCurrEnv().insert(param,
                                ValueMemory::create(context, value.getValue()));
          }
        }

        Environment::Scope bodyScope(getCurrEnv());

        lambdaDecl->getExprBody()->accept(*this);

        if (diag.hasError()) {
          stmtInterpreter->report(
              lambdaDecl->getRange(),
              InterpretDiagnostic::note_dump_lambda_call_stack);
        } else if (stmtInterpreter->continueFlag) {
          stmtInterpreter->report(
              stmtInterpreter->continueFlag->getRange(),
              InterpretDiagnostic::error_unresolved_continue_statement);
        } else if (stmtInterpreter->breakFlag) {
          stmtInterpreter->report(
              stmtInterpreter->breakFlag->getRange(),
              InterpretDiagnostic::error_unresolved_break_statement);
        } else if (stmtInterpreter->returnFlag) {
          result = context->getGC()->registerRoot(stmtInterpreter->returnValue);
          stmtInterpreter->returnFlag = nullptr;
          stmtInterpreter->returnValue = nullptr;
        }
      })
      .Case([&](BuiltinFunctionValue *builtinV) {
        auto func = builtinV->getFuncBody();
        GCSAFE(context->getGC()) {
          auto resultV =
              func(llvm::map_to_vector(args,
                                       [&](const GC::RootRegister &reg) {
                                         return reg.getValue();
                                       }),
                   diag, expr.getRange(), context);
          if (resultV)
            result = context->getGC()->registerRoot(resultV);
        }
      })
      .Default(
          [&](Value *) { llvm_unreachable("never used for call operation"); });
}

void RvExprInterpreter::visit(const ArrayExpression &expr) {
  SmallVector<GC::RootRegister> registers;
  registers.reserve(expr.getSize());

  for (auto *elementExpr : expr.getArgs()) {
    elementExpr->accept(*this);
    if (isTerminated())
      return;
    registers.emplace_back(std::move(result));
  }

  GCSAFE(context->getGC()) {
    auto listV = ListValue::create(
        context, llvm::map_to_vector(
                     registers, [&](auto &reg) { return reg.getValue(); }));
    result = context->getGC()->registerRoot(listV);
  }
}

void RvExprInterpreter::visit(const TupleExpression &expr) {
  SmallVector<GC::RootRegister> values;
  values.reserve(expr.getSize());

  for (auto *elementExpr : expr.getExprs()) {
    elementExpr->accept(*this);
    if (isTerminated())
      return;

    values.emplace_back(std::move(result));
  }

  GCSAFE(context->getGC()) {
    auto tupleV = TupleValue::create(
        context,
        llvm::map_to_vector(values, [&](auto &reg) { return reg.getValue(); }));
    result = context->getGC()->registerRoot(tupleV);
  }
}

void RvExprInterpreter::visit(const GroupExpression &expr) {
  expr.getExpr()->accept(*this);
}

void RvExprInterpreter::visit(const CompoundExpression &expr) {
  Environment::Scope scope(getCurrEnv());
  for (auto *subStmt : expr.getStmts()) {
    subStmt->accept(*stmtInterpreter);
    if (stmtInterpreter->isTerminated())
      return;
  }
  // The result of a compound expression is the result of its last expression
  if (expr.getExpr()) {
    expr.getExpr()->accept(*this);
  } else {
    GCSAFE(context->getGC()) {
      auto nilV = NilValue::create(context);
      result = context->getGC()->registerRoot(nilV);
    }
  }
}

void RvExprInterpreter::visit(const IntegerLiteral &expr) {
  if (expr.getValue() >=
      static_cast<uint64_t>(std::numeric_limits<int64_t>::max())) {
    stmtInterpreter->report(expr.getRange(),
                            InterpretDiagnostic::warning_integer_overflow);
  }

  GCSAFE(context->getGC()) {
    auto intV = IntegerValue::create(context, expr.getValue());
    result = context->getGC()->registerRoot(intV);
  }
}

void RvExprInterpreter::visit(const BooleanLiteral &expr) {
  GCSAFE(context->getGC()) {
    auto boolV = BoolValue::create(context, expr.getValue());
    result = context->getGC()->registerRoot(boolV);
  }
}

void RvExprInterpreter::visit(const FloatLiteral &expr) {
  GCSAFE(context->getGC()) {
    auto floatV = FloatValue::create(context, expr.getValue());
    result = context->getGC()->registerRoot(floatV);
  }
}

void RvExprInterpreter::visit(const StringLiteral &expr) {
  GCSAFE(context->getGC()) {
    auto strV =
        StringValue::create(context, evalStringLiteral(expr.getValue()));
    result = context->getGC()->registerRoot(strV);
  }
}

void RvExprInterpreter::visit(const NilLiteral &expr) {
  GCSAFE(context->getGC()) {
    auto nilV = NilValue::create(context);
    result = context->getGC()->registerRoot(nilV);
  }
}

} // namespace bara
