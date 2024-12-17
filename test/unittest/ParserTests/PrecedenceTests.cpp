#include "bara/ast/AST.h"
#include "bara/parser/Parser.h"
#include "gtest/gtest.h"

namespace bara {

static bool runPrecedenceTest(ASTContext *context, Expression *expr) {
  llvm::SourceMgr srcMgr;
  Diagnostic diag(srcMgr);

  auto toStr = expr->toString();
  auto sourceIdx = srcMgr.AddNewSourceBuffer(
      llvm::MemoryBuffer::getMemBuffer(toStr, "expr"), llvm::SMLoc());
  auto bufferRef = srcMgr.getMemoryBuffer(sourceIdx)->getBuffer();
  Lexer lexer(diag, context, bufferRef);
  Parser parser(lexer);

  auto parsedExpr = parser.parseExpression();
  if (!lexer.peekToken()->is<Token::Tok_Eof>()) {
    diag.report(lexer.peekToken()->getRange(), llvm::SourceMgr::DK_Error,
                "Expression parsing is not ended");
    return false;
  }

  return expr->isEqual(parsedExpr);
}

TEST(ParserTests, PrecedenceTest) {
  ASTContext astContext;
  auto one = IntegerLiteral::create({}, &astContext, 1);
  auto two = IntegerLiteral::create({}, &astContext, 2);
  auto three = IntegerLiteral::create({}, &astContext, 3);

  auto Add = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Plus, rhs);
  };
  auto Sub = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Minus, rhs);
  };
  auto Mul = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Mul, rhs);
  };
  auto Div = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Div, rhs);
  };
  auto Mod = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Mod, rhs);
  };
  auto BitAnd = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::BitAnd,
                                    rhs);
  };
  auto BitOr = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::BitOr, rhs);
  };
  auto BitXor = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::BitXor,
                                    rhs);
  };
  auto BitShl = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Shl, rhs);
  };
  auto BitShr = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Shr, rhs);
  };
  auto LogAnd = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::And, rhs);
  };
  auto LogOr = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Or, rhs);
  };
  auto Eq = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Eq, rhs);
  };
  auto Ne = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Ne, rhs);
  };
  auto Lt = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Lt, rhs);
  };
  auto Le = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Le, rhs);
  };
  auto Gt = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Gt, rhs);
  };
  auto Ge = [&](Expression *lhs, Expression *rhs) {
    return BinaryExpression::create({}, &astContext, lhs, Operator::Ge, rhs);
  };

  SmallVector<pair<llvm::function_ref<Expression *(Expression *, Expression *)>,
                   uint16_t>>
      binaryBuilders = {
          {Mul, 3},    {Div, 3},     {Mod, 3},    {Add, 4},    {Sub, 4},
          {BitShl, 5}, {BitShr, 5},  {Lt, 6},     {Le, 6},     {Gt, 6},
          {Ge, 6},     {Eq, 7},      {Ne, 7},     {BitAnd, 8}, {BitXor, 9},
          {BitOr, 10}, {LogAnd, 11}, {LogOr, 12},
      };

  /// assume that all operations are left-associative
  for (auto l = 0; l < binaryBuilders.size(); ++l) {
    auto [lhsBuilder, lhsPrecedence] = binaryBuilders[l];
    for (auto r = l + 1; r < binaryBuilders.size(); ++r) {
      auto [rhsBuilder, rhsPrecedence] = binaryBuilders[r];
      Expression *expr;
      if (lhsPrecedence <= rhsPrecedence)
        /// 1 + 2 + 3
        /// 1 * 2 + 3
        expr = rhsBuilder(lhsBuilder(one, two), three);
      else
        /// 1 + 2 * 3
        expr = lhsBuilder(one, rhsBuilder(two, three));
      ASSERT_TRUE(runPrecedenceTest(&astContext, expr));
    }
  }

  auto UnaryPlus = [&](Expression *target) {
    return UnaryExpression::create({}, &astContext, Operator::Plus, target);
  };
  auto UnaryMinus = [&](Expression *target) {
    return UnaryExpression::create({}, &astContext, Operator::Minus, target);
  };
  auto BitNot = [&](Expression *target) {
    return UnaryExpression::create({}, &astContext, Operator::BitNot, target);
  };
  auto LogNot = [&](Expression *target) {
    return UnaryExpression::create({}, &astContext, Operator::Not, target);
  };

  SmallVector<pair<llvm::function_ref<Expression *(Expression *)>, uint16_t>>
      unaryBuilders = {
          {UnaryPlus, 2}, {UnaryMinus, 2}, {BitNot, 2}, {LogNot, 2}};

  for (auto l = 0; l < unaryBuilders.size(); ++l) {
    auto [lhsBuilder, lhsPrecedence] = unaryBuilders[l];
    for (auto r = 0; r < binaryBuilders.size(); ++r) {
      auto [rhsBuilder, rhsPrecedence] = binaryBuilders[r];
      Expression *expr;
      if (lhsPrecedence <= rhsPrecedence)
        /// -1 + 2
        expr = rhsBuilder(lhsBuilder(one), two);
      else
        /// no example yet
        expr = lhsBuilder(rhsBuilder(one, two));
      ASSERT_TRUE(runPrecedenceTest(&astContext, expr));
    }
  }
}

} // namespace bara
