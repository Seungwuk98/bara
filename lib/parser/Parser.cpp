#include "bara/parser/Parser.h"
#include "bara/ast/AST.h"
#include "llvm/Support/ErrorHandling.h"

namespace bara {

static optional<Operator> getOperator(Token::Kind kind);

Program *Parser::parse() { return parseProgram(); }

Program *Parser::parseProgram() {
  RangeCapture capture(*this);

  auto stmts = parseStatements();
  if (diag.hasError())
    return nullptr;

  return Program::create(capture.create(), context, stmts);
}

Statement *Parser::parseStatement() {
  auto *peekTok = peek();
  switch (peekTok->getKind()) {
  case Token::Tok_LBrace:
    return parseCompoundStatement();
  case Token::Tok_if:
    return parseIfStatement();
  case Token::Tok_while:
    return parseWhileStatement();
  case Token::Tok_do:
    return parseDoWhileStatement();
  case Token::Tok_for:
    return parseForStatement();
  case Token::Tok_return:
    return parseReturnStatement();
  case Token::Tok_break:
    return parseBreakStatement();
  case Token::Tok_continue:
    return parseContinueStatement();
  case Token::Tok_var:
    return parseDeclarationStatement();
  case Token::Tok_fn:
    return parseFunctionDeclaration();
  default:
    RangeCapture capture(*this);
    auto *expr = parseExpression();
    if (diag.hasError())
      return nullptr;
    peekTok = peek();
    switch (peekTok->getKind()) {
    case Token::Tok_Semicolon:
      skip();
      return ExpressionStatement::create(capture.create(), context, expr);
    case Token::Tok_Eof:
      report(peekTok->getRange(), ParseDiagnostic::error_unexpected_token,
             "semicolon", Token::getTokenString(peekTok->getKind()));
    case Token::Tok_Equal:
      return parseAssignmentStatement(expr);
    case Token::Tok_PlusEqual:
    case Token::Tok_MinusEqual:
    case Token::Tok_StarEqual:
    case Token::Tok_SlashEqual:
    case Token::Tok_PercentEqual:
    case Token::Tok_LShiftEqual:
    case Token::Tok_RShiftEqual:
    case Token::Tok_AmpersandEqual:
    case Token::Tok_VBarEqual:
    case Token::Tok_CaretEqual:
      return parseOperatorAssignmentStatement(expr);
    default:
      report(peekTok->getRange(), ParseDiagnostic::error_unparsable_token,
             "statement", Token::getTokenString(peekTok->getKind()));
      return nullptr;
    }
  }
}

CompoundStatement *Parser::parseCompoundStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_LBrace>())
    return nullptr;

  auto stmts = parseStatements();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_RBrace>())
    return nullptr;

  return CompoundStatement::create(capture.create(), context, stmts);
}

IfStatement *Parser::parseIfStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_if>())
    return nullptr;

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  auto thenStmt = parseCompoundStatement();
  if (diag.hasError())
    return nullptr;

  if (peekIs<Token::Tok_else>()) {
    skip();
    if (peekIs<Token::Tok_LBrace>()) {
      auto compoundStmt = parseCompoundStatement();
      if (diag.hasError())
        return nullptr;
      return IfStatement::create(capture.create(), context, expr, thenStmt,
                                 compoundStmt);
    } else {
      auto elseStmt = parseIfStatement();
      if (diag.hasError())
        return nullptr;
      return IfStatement::create(capture.create(), context, expr, thenStmt,
                                 elseStmt);
    }
  }

  return IfStatement::create(capture.create(), context, expr, thenStmt);
}

WhileStatement *Parser::parseWhileStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_while>())
    return nullptr;

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  auto stmt = parseCompoundStatement();
  if (diag.hasError())
    return nullptr;

  return WhileStatement::create(capture.create(), context, expr, stmt);
}

WhileStatement *Parser::parseDoWhileStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_do>())
    return nullptr;

  auto stmt = parseCompoundStatement();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_while>())
    return nullptr;

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return WhileStatement::create(capture.create(), context, expr, stmt, true);
}

ForStatement *Parser::parseForStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_for, Token::Tok_LParen>())
    return nullptr;

  optional<Statement *> decl;

  if (!peekIs<Token::Tok_Semicolon>()) {
    RangeCapture declCapture(*this);
    if (consume<Token::Tok_var>())
      return nullptr;

    auto *pattern = parsePattern();
    if (diag.hasError())
      return nullptr;

    if (consume<Token::Tok_Equal>())
      return nullptr;

    auto *expr = parseExpression();
    if (diag.hasError())
      return nullptr;

    decl = DeclarationStatement::create(declCapture.create(), context, pattern,
                                        expr);
  }

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  optional<Expression *> cond;
  if (!peekIs<Token::Tok_Semicolon>()) {
    auto *expr = parseExpression();
    if (diag.hasError())
      return nullptr;
    cond = expr;
  }

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  optional<Statement *> step;
  if (!peekIs<Token::Tok_RParen>()) {
    if (peekIs<Token::Tok_LBrace>()) {
      /// Compound Statement
      step = parseCompoundStatement();
      if (diag.hasError())
        return nullptr;
    } else {
      /// Assignment
      RangeCapture stepCapture(*this);

      auto *expr = parseExpression();
      if (diag.hasError())
        return nullptr;

      auto *nextTok = advance();
      if (nextTok->is<Token::Tok_Equal>()) {
        auto *rhs = parseExpression();
        if (diag.hasError())
          return nullptr;
        step = AssignmentStatement::create(stepCapture.create(), context, expr,
                                           rhs);
      } else {
        auto op = getOperator(nextTok->getKind());
        if (!op) {
          report(nextTok->getRange(), ParseDiagnostic::error_unparsable_token,
                 "operator", Token::getTokenString(nextTok->getKind()));
          return nullptr;
        }
        auto *rhs = parseExpression();
        if (diag.hasError())
          return nullptr;
        step = OperatorAssignmentStatement::create(stepCapture.create(),
                                                   context, expr, *op, rhs);
      }
    }
  }

  if (consume<Token::Tok_RParen>())
    return nullptr;

  auto stmt = parseCompoundStatement();
  if (diag.hasError())
    return nullptr;

  return ForStatement::create(capture.create(), context, decl, cond, step,
                              stmt);
}

ReturnStatement *Parser::parseReturnStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_return>())
    return nullptr;

  if (peekIs<Token::Tok_Semicolon>()) {
    skip();
    return ReturnStatement::create(capture.create(), context, nullopt);
  }

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return ReturnStatement::create(capture.create(), context, expr);
}

BreakStatement *Parser::parseBreakStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_break>())
    return nullptr;

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return BreakStatement::create(capture.create(), context);
}

ContinueStatement *Parser::parseContinueStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_continue>())
    return nullptr;

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return ContinueStatement::create(capture.create(), context);
}

DeclarationStatement *Parser::parseDeclarationStatement() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_var>())
    return nullptr;

  auto *pattern = parsePattern();
  if (diag.hasError())
    return nullptr;

  optional<Expression *> init;
  if (peekIs<Token::Tok_Equal>()) {
    skip();
    auto *expr = parseExpression();
    if (diag.hasError())
      return nullptr;
    init = expr;
  }

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return DeclarationStatement::create(capture.create(), context, pattern, init);
}

AssignmentStatement *Parser::parseAssignmentStatement(Expression *lhs) {
  RangeCapture capture(*this, lhs->getRange().Start);
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_Equal>())
    return nullptr;

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return AssignmentStatement::create(capture.create(), context, lhs, expr);
}

static optional<Operator> getOperator(Token::Kind kind) {
  switch (kind) {
  case Token::Tok_PlusEqual:
    return Operator::Plus;
  case Token::Tok_MinusEqual:
    return Operator::Minus;
  case Token::Tok_StarEqual:
    return Operator::Mul;
  case Token::Tok_SlashEqual:
    return Operator::Div;
  case Token::Tok_PercentEqual:
    return Operator::Mod;
  case Token::Tok_VBarEqual:
    return Operator::BitOr;
  case Token::Tok_AmpersandEqual:
    return Operator::BitAnd;
  case Token::Tok_CaretEqual:
    return Operator::BitXor;
  case Token::Tok_LShiftEqual:
    return Operator::Shl;
  case Token::Tok_RShiftEqual:
    return Operator::Shr;
  default:
    return nullopt;
  }
}

OperatorAssignmentStatement *
Parser::parseOperatorAssignmentStatement(Expression *lhs) {
  RangeCapture capture(*this, lhs->getRange().Start);
  if (diag.hasError())
    return nullptr;

  auto *nextTok = advance();
  optional<Operator> op = getOperator(nextTok->getKind());
  if (!op) {
    report(nextTok->getRange(), ParseDiagnostic::error_unexpected_token,
           "operator assignment", Token::getTokenString(nextTok->getKind()));
    return nullptr;
  }

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_Semicolon>())
    return nullptr;

  return OperatorAssignmentStatement::create(capture.create(), context, lhs,
                                             *op, expr);
}

FunctionDeclaration *Parser::parseFunctionDeclaration() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_fn>())
    return nullptr;

  auto *identTok = advance();
  if (expect<Token::Tok_Identifier>(identTok))
    return nullptr;

  auto functionName = identTok->getSymbol();
  if (consume<Token::Tok_LParen>())
    return nullptr;

  SmallVector<StringRef> params;
  if (!peekIs<Token::Tok_RParen>()) {
    auto *identTok = advance();
    if (expect<Token::Tok_Identifier>(identTok))
      return nullptr;
    params.push_back(identTok->getSymbol());

    while (peekIs<Token::Tok_Comma>()) {
      skip();
      identTok = advance();
      if (expect<Token::Tok_Identifier>(identTok))
        return nullptr;
      params.emplace_back(identTok->getSymbol());
    }
  }

  if (consume<Token::Tok_RParen>())
    return nullptr;

  if (consume<Token::Tok_LBrace>())
    return nullptr;

  auto stmts = parseStatements();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_RBrace>())
    return nullptr;

  return FunctionDeclaration::create(capture.create(), context, functionName,
                                     params, stmts);
}

Expression *Parser::parseExpression() { return parseConditionalExpression(); }

Expression *Parser::parseConditionalExpression() {
  RangeCapture capture(*this);
  auto *cond = parseLogicalOrExpression();
  if (diag.hasError())
    return nullptr;

  if (peekIs<Token::Tok_Question>()) {
    skip();
    auto *thenExpr = parseConditionalExpression();
    if (diag.hasError())
      return nullptr;

    if (consume<Token::Tok_Colon>())
      return nullptr;

    auto *elseExpr = parseConditionalExpression();
    if (diag.hasError())
      return nullptr;

    return ConditionalExpression::create(capture.create(), context, cond,
                                         thenExpr, elseExpr);
  }

  return cond;
}

Expression *Parser::parseLogicalOrExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseLogicalAndExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_LogicalOr>()) {
    skip();
    auto *rhs = parseLogicalAndExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs, Operator::Or,
                                   rhs);
  }

  return lhs;
}

Expression *Parser::parseLogicalAndExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseBitwiseOrExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_LogicalAnd>()) {
    skip();
    auto *rhs = parseBitwiseOrExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs,
                                   Operator::And, rhs);
  }

  return lhs;
}

Expression *Parser::parseBitwiseOrExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseBitwiseXorExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_VBar>()) {
    skip();
    auto *rhs = parseBitwiseXorExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs,
                                   Operator::BitOr, rhs);
  }

  return lhs;
}

Expression *Parser::parseBitwiseXorExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseBitwiseAndExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_Caret>()) {
    skip();
    auto *rhs = parseBitwiseAndExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs,
                                   Operator::BitXor, rhs);
  }

  return lhs;
}

Expression *Parser::parseBitwiseAndExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseEqualityExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_Ampersand>()) {
    skip();
    auto *rhs = parseEqualityExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs,
                                   Operator::BitAnd, rhs);
  }

  return lhs;
}

Expression *Parser::parseEqualityExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseComparisonExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_EqualEqual, Token::Tok_NotEqual>()) {
    auto op = peek()->getKind();
    skip();
    auto *rhs = parseComparisonExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(
        capture.create(), context, lhs,
        op == Token::Tok_EqualEqual ? Operator::Eq : Operator::Ne, rhs);
  }

  return lhs;
}

Expression *Parser::parseComparisonExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseBitwiseShiftExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_Lt, Token::Tok_Le, Token::Tok_Gt, Token::Tok_Ge>()) {
    auto op = peek()->getKind();
    skip();
    auto *rhs = parseBitwiseShiftExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs,
                                   op == Token::Tok_Lt   ? Operator::Lt
                                   : op == Token::Tok_Le ? Operator::Le
                                   : op == Token::Tok_Gt ? Operator::Gt
                                                         : Operator::Ge,
                                   rhs);
  }

  return lhs;
}

Expression *Parser::parseBitwiseShiftExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseAdditionExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_LShift, Token::Tok_RShift>()) {
    auto op = peek()->getKind();
    skip();
    auto *rhs = parseAdditionExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(
        capture.create(), context, lhs,
        op == Token::Tok_LShift ? Operator::Shl : Operator::Shr, rhs);
  }

  return lhs;
}

Expression *Parser::parseAdditionExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseMultiplicationExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_Plus, Token::Tok_Minus>()) {
    auto op = peek()->getKind();
    skip();
    auto *rhs = parseMultiplicationExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(
        capture.create(), context, lhs,
        op == Token::Tok_Plus ? Operator::Plus : Operator::Minus, rhs);
  }

  return lhs;
}

Expression *Parser::parseMultiplicationExpression() {
  RangeCapture capture(*this);
  auto *lhs = parseUnaryExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_Star, Token::Tok_Slash, Token::Tok_Percent>()) {
    auto op = peek()->getKind();
    skip();
    auto *rhs = parseUnaryExpression();
    if (diag.hasError())
      return nullptr;
    lhs = BinaryExpression::create(capture.create(), context, lhs,
                                   op == Token::Tok_Star    ? Operator::Mul
                                   : op == Token::Tok_Slash ? Operator::Div
                                                            : Operator::Mod,
                                   rhs);
  }

  return lhs;
}

Expression *Parser::parseUnaryExpression() {
  if (peekIs<Token::Tok_Plus, Token::Tok_Minus, Token::Tok_Bang,
             Token::Tok_Tilde>()) {
    RangeCapture capture(*this);
    auto op = peek()->getKind();
    skip();
    auto *expr = parseUnaryExpression();
    if (diag.hasError())
      return nullptr;
    return UnaryExpression::create(capture.create(), context,
                                   op == Token::Tok_Plus    ? Operator::Plus
                                   : op == Token::Tok_Minus ? Operator::Minus
                                   : op == Token::Tok_Bang  ? Operator::Not
                                                            : Operator::BitNot,
                                   expr);
  }
  return parseCallOrIndexExpression();
}

Expression *Parser::parseCallOrIndexExpression() {
  RangeCapture capture(*this);
  auto *lhs = parsePrimaryExpression();
  if (diag.hasError())
    return nullptr;

  while (peekIs<Token::Tok_LParen, Token::Tok_LBracket>()) {
    auto *peekTok = peek();
    if (peekTok->is<Token::Tok_LParen>()) {
      skip();
      SmallVector<Expression *> args;
      if (!peekIs<Token::Tok_RParen>()) {
        auto *arg = parseExpression();
        if (diag.hasError())
          return nullptr;
        args.emplace_back(arg);

        while (peekIs<Token::Tok_Comma>()) {
          skip();
          arg = parseExpression();
          if (diag.hasError())
            return nullptr;
          args.emplace_back(arg);
        }
      }
      if (consume<Token::Tok_RParen>())
        return nullptr;
      lhs = CallExpression::create(capture.create(), context, lhs, args);
    } else {
      skip();
      auto *index = parseExpression();
      if (diag.hasError())
        return nullptr;
      if (consume<Token::Tok_RBracket>())
        return nullptr;
      lhs = IndexExpression::create(capture.create(), context, lhs, index);
    }
  }

  return lhs;
}

Expression *Parser::parsePrimaryExpression() {
  auto peekTok = peek();

  switch (peekTok->getKind()) {
  case Token::Tok_Identifier:
    return parseIdentifierExpression();
  case Token::Tok_IntegerLiteral:
    return parseIntegerLiteral();
  case Token::Tok_FloatLiteral:
    return parseFloatLiteral();
  case Token::Tok_StringLiteral:
    return parseStringLiteral();
  case Token::Tok_true:
  case Token::Tok_false:
    return parseBooleanLiteral();
  case Token::Tok_LParen:
    return parseTupleOrGroupExpression();
  case Token::Tok_LBracket:
    return parseArrayExpression();
  case Token::Tok_match:
    return parseMatchExpression();
  case Token::Tok_BackSlash:
    return parseLambdaExpression();
  case Token::Tok_nil:
    return parseNilLiteral();
  default:
    report(peekTok->getRange(), ParseDiagnostic::error_unparsable_token,
           "primary expression", Token::getTokenString(peekTok->getKind()));
    return nullptr;
  }
}

MatchExpression *Parser::parseMatchExpression() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_match>())
    return nullptr;

  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  if (consume<Token::Tok_LBrace>())
    return nullptr;

  SmallVector<MatchExpression::MatchCase> cases;

  while (!peekIs<Token::Tok_RBrace, Token::Tok_Eof>()) {
    auto matchCase = parseMatchCase();
    if (diag.hasError())
      return nullptr;
    cases.push_back(matchCase);
  }

  if (consume<Token::Tok_RBrace>())
    return nullptr;

  return MatchExpression::create(capture.create(), context, expr, cases);
}

MatchExpression::MatchCase Parser::parseMatchCase() {
  if (consume<Token::Tok_BackSlash>())
    return {};

  auto *pattern = parsePattern();
  if (diag.hasError())
    return {};

  if (consume<Token::Tok_RightArrow>())
    return {};

  auto *expr = parseExpression();
  if (diag.hasError())
    return {};

  if (consume<Token::Tok_Semicolon>())
    return {};

  return {pattern, expr};
}

LambdaExpression *Parser::parseLambdaExpression() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_BackSlash>())
    return nullptr;

  SmallVector<StringRef> params;
  if (!peekIs<Token::Tok_RightArrow>()) {
    auto *identTok = advance();
    if (expect<Token::Tok_Identifier>(identTok))
      return nullptr;
    params.emplace_back(identTok->getSymbol());

    while (peekIs<Token::Tok_Comma>()) {
      skip();
      identTok = advance();
      if (expect<Token::Tok_Identifier>(identTok))
        return nullptr;
      params.emplace_back(identTok->getSymbol());
    }
  }

  if (consume<Token::Tok_RightArrow>())
    return nullptr;

  if (peekIs<Token::Tok_LBrace>()) {
    auto stmt = parseCompoundStatement();
    if (diag.hasError())
      return nullptr;
    return LambdaExpression::create(capture.create(), context, params, stmt);
  }
  auto *expr = parseExpression();
  if (diag.hasError())
    return nullptr;

  return LambdaExpression::create(capture.create(), context, params, expr);
}

IdentifierExpression *Parser::parseIdentifierExpression() {
  auto *identTok = advance();
  if (expect<Token::Tok_Identifier>(identTok))
    return nullptr;
  return IdentifierExpression::create(identTok->getRange(), context,
                                      identTok->getSymbol());
}

Expression *Parser::parseTupleOrGroupExpression() {
  RangeCapture capture(*this);
  if (consume<Token::Tok_LParen>())
    return nullptr;

  SmallVector<Expression *> exprs;
  bool isTuple = false;
  if (peekIs<Token::Tok_Comma>()) {
    skip();
    isTuple = true;
  } else if (peekIs<Token::Tok_RParen>()) {
    isTuple = true;
  } else {
    auto *expr = parseExpression();
    if (diag.hasError())
      return nullptr;
    exprs.emplace_back(expr);

    if (peekIs<Token::Tok_Comma>())
      isTuple = true;

    while (peekIs<Token::Tok_Comma>()) {
      skip();
      if (peekIs<Token::Tok_RParen>())
        break;
      expr = parseExpression();
      if (diag.hasError())
        return nullptr;
      exprs.emplace_back(expr);
    }
  }

  if (consume<Token::Tok_RParen>())
    return nullptr;

  if (isTuple)
    return TupleExpression::create(capture.create(), context, exprs);
  return GroupExpression::create(capture.create(), context, exprs[0]);
}

ArrayExpression *Parser::parseArrayExpression() {
  if (consume<Token::Tok_LBracket>())
    return nullptr;

  SmallVector<Expression *> exprs;
  if (peekIs<Token::Tok_Comma>()) {
    skip();
  } else if (!peekIs<Token::Tok_RBracket>()) {
    auto *expr = parseExpression();
    if (diag.hasError())
      return nullptr;
    exprs.emplace_back(expr);

    while (peekIs<Token::Tok_Comma>()) {
      skip();
      if (peekIs<Token::Tok_RBracket>())
        break;
      expr = parseExpression();
      if (diag.hasError())
        return nullptr;
      exprs.emplace_back(expr);
    }
  }

  if (consume<Token::Tok_RBracket>())
    return nullptr;

  return ArrayExpression::create(tok->getRange(), context, exprs);
}

IntegerLiteral *Parser::parseIntegerLiteral() {
  auto *intTok = advance();
  if (expect<Token::Tok_IntegerLiteral>(intTok))
    return nullptr;
  auto value = std::stoull(intTok->getSymbol().str());
  return IntegerLiteral::create(intTok->getRange(), context, value);
}

BooleanLiteral *Parser::parseBooleanLiteral() {
  auto *boolTok = advance();
  if (!boolTok->is<Token::Tok_true, Token::Tok_false>()) {
    report(boolTok->getRange(), ParseDiagnostic::error_unexpected_token,
           "boolean literal", Token::getTokenString(boolTok->getKind()));
    return nullptr;
  }

  auto value = boolTok->getKind() == Token::Tok_true;
  return BooleanLiteral::create(boolTok->getRange(), context, value);
}

FloatLiteral *Parser::parseFloatLiteral() {
  auto *floatTok = advance();
  if (expect<Token::Tok_FloatLiteral>(floatTok))
    return nullptr;
  return FloatLiteral::create(floatTok->getRange(), context,
                              floatTok->getSymbol());
}

StringLiteral *Parser::parseStringLiteral() {
  auto *stringTok = advance();
  if (expect<Token::Tok_StringLiteral>(stringTok))
    return nullptr;
  return StringLiteral::create(stringTok->getRange(), context,
                               stringTok->getSymbol());
}

NilLiteral *Parser::parseNilLiteral() {
  auto *nilTok = advance();
  if (expect<Token::Tok_nil>(nilTok))
    return nullptr;
  return NilLiteral::create(nilTok->getRange(), context);
}

Pattern *Parser::parsePattern() {
  auto peekTok = peek();

  switch (peekTok->getKind()) {
  case Token::Tok_Identifier:
    return parseIdentifierPattern();
  case Token::Tok_LParen:
    return parseTupleOrGroupPattern();
  case Token::Tok_IntegerLiteral:
    return parseIntegerPattern();
  case Token::Tok_FloatLiteral:
    return parseFloatPattern();
  case Token::Tok_true:
  case Token::Tok_false:
    return parseBooleanPattern();
  case Token::Tok_StringLiteral:
    return parseStringPattern();
  case Token::Tok_nil:
    return parseNilPattern();
  default:
    report(peekTok->getRange(), ParseDiagnostic::error_unparsable_token,
           "pattern", Token::getTokenString(peekTok->getKind()));
    return nullptr;
  }
}

Pattern *Parser::parseIdentifierPattern() {
  auto *identTok = advance();
  if (expect<Token::Tok_Identifier>(identTok))
    return nullptr;
  if (identTok->getSymbol() == "_")
    return EmptyPattern::create(identTok->getRange(), context);
  return IdentifierPattern::create(identTok->getRange(), context,
                                   identTok->getSymbol());
}

Pattern *Parser::parseTupleOrGroupPattern() {
  if (consume<Token::Tok_LParen>())
    return nullptr;

  SmallVector<Pattern *> patterns;
  bool isTuple = false;
  if (peekIs<Token::Tok_Comma>()) {
    skip();
    isTuple = true;
  } else if (peekIs<Token::Tok_RParen>()) {
    isTuple = true;
  } else {
    auto *pattern = parsePattern();
    if (diag.hasError())
      return nullptr;
    patterns.emplace_back(pattern);

    if (peekIs<Token::Tok_Comma>())
      isTuple = true;

    while (peekIs<Token::Tok_Comma>()) {
      skip();
      if (peekIs<Token::Tok_RParen>())
        break;
      pattern = parsePattern();
      if (diag.hasError())
        return nullptr;
      patterns.emplace_back(pattern);
    }
  }

  if (consume<Token::Tok_RParen>())
    return nullptr;

  if (isTuple)
    return TuplePattern::create(tok->getRange(), context, patterns);
  return GroupPattern::create(tok->getRange(), context, patterns[0]);
}

IntegerPattern *Parser::parseIntegerPattern() {
  auto *intTok = advance();
  if (expect<Token::Tok_IntegerLiteral>(intTok))
    return nullptr;
  auto value = std::stoull(intTok->getSymbol().str());
  return IntegerPattern::create(intTok->getRange(), context, value);
}

FloatPattern *Parser::parseFloatPattern() {
  auto *floatTok = advance();
  if (expect<Token::Tok_FloatLiteral>(floatTok))
    return nullptr;
  return FloatPattern::create(floatTok->getRange(), context,
                              floatTok->getSymbol());
}

BooleanPattern *Parser::parseBooleanPattern() {
  auto *boolTok = advance();
  if (!boolTok->is<Token::Tok_true, Token::Tok_false>()) {
    report(boolTok->getRange(), ParseDiagnostic::error_unexpected_token,
           "boolean pattern", Token::getTokenString(boolTok->getKind()));
    return nullptr;
  }

  auto value = boolTok->getKind() == Token::Tok_true;
  return BooleanPattern::create(boolTok->getRange(), context, value);
}

StringPattern *Parser::parseStringPattern() {
  auto *stringTok = advance();
  if (expect<Token::Tok_StringLiteral>(stringTok))
    return nullptr;
  return StringPattern::create(stringTok->getRange(), context,
                               stringTok->getSymbol());
}

NilPattern *Parser::parseNilPattern() {
  auto *nilTok = advance();
  if (expect<Token::Tok_nil>(nilTok))
    return nullptr;
  return NilPattern::create(nilTok->getRange(), context);
}

Token *Parser::advance() { return tok = lexer.getNextToken(); }

void Parser::skip() { advance(); }

Token *Parser::peek(size_t look) { return lexer.peekToken(look); }

void Parser::commonRecovery() {
  auto peekTok = peek();
  while (!peekTok->is<Token::Tok_LBrace, Token::Tok_if, Token::Tok_while,
                      Token::Tok_do, Token::Tok_for, Token::Tok_return,
                      Token::Tok_break, Token::Tok_continue, Token::Tok_var,
                      Token::Tok_fn, Token::Tok_Eof>()) {
    skip();
    if (peekTok->is<Token::Tok_Semicolon>())
      break;
    peekTok = peek();
  }
}

vector<Statement *> Parser::parseStatements() {
  vector<Statement *> stmts;
  while (!peekIs<Token::Tok_RBrace, Token::Tok_Eof>()) {
    auto *stmt = parseStatement();
    if (diag.hasError()) {
      commonRecovery();
      continue;
    }
    stmts.emplace_back(stmt);
  }
  return stmts;
}

const char *parserDiagMsgs[] = {
#define DIAG(Name, Msg, Error) Msg,
#include "bara/parser/ParserDiagnostic.def"
};

const llvm::SourceMgr::DiagKind parserDiagKinds[] = {
#define DIAG(Name, Msg, Error) llvm::SourceMgr::DK_##Error,
#include "bara/parser/ParserDiagnostic.def"
};

const char *Parser::ParseDiagnostic::getDiagMsg(Diag kind) {
  return parserDiagMsgs[kind];
}

llvm::SourceMgr::DiagKind Parser::ParseDiagnostic::getDiagKind(Diag kind) {
  return parserDiagKinds[kind];
}

} // namespace bara
