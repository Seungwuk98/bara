#ifndef BARA_PARSER_H
#define BARA_PARSER_H

#include "bara/ast/AST.h"
#include "bara/parser/Lexer.h"
namespace bara {

class Parser {
public:
  Parser(Lexer &lexer)
      : lexer(lexer), diag(lexer.getDiagnostic()),
        context(lexer.getASTContext()) {}

public:
  Program *parse();

  /// Expression ::= ConditionalExpression
  Expression *parseExpression();

  /// Statement ::=
  ///  CompoundStatement
  ///  | ExpressionStatement
  ///  | CompoundStatement
  ///  | IfStatement
  ///  | WhileStatement
  ///  | DoWhileStatement
  ///  | ForStatement
  ///  | ReturnStatement
  ///  | BreakStatement
  ///  | ContinueStatement
  ///  | DeclarationStatement
  ///  | AssignmentStatement
  ///  | OperatorAssignmentStatement
  ///  | FunctionDeclaration
  Statement *parseStatement();

private:
  /// Program ::= Statement*
  Program *parseProgram();

  /// CompoundStatement ::= '{' Statement* '}'
  CompoundStatement *parseCompoundStatement();

  /// IfStatement ::=
  ///   'if' Expression '{' Statement* '}' ('else' '{' Statement '}')?
  IfStatement *parseIfStatement();

  /// WhileStatement ::= 'while' Expression '{' Statement* '}'
  WhileStatement *parseWhileStatement();

  /// DoWhileStatement ::= 'do' '{' Statement* '}' 'while' Expression ';'
  WhileStatement *parseDoWhileStatement();

  /// ForStatement ::=
  /// 'for' '(' Declaration? ';' Expression? ';' Assignment | CompoundStatement
  /// ')' '{' Statement* '}'
  /// Declaration ::= 'var' Pattern ('=' Expression)?
  /// Assignment ::= Pattern
  ///   (
  ///     '=' | '+=' | '-=' | '*=' | '/=' | '%='
  ///     | '<<=' | '>>=' | '&=' | '|=' | '^='
  ///   ) Expression
  ForStatement *parseForStatement();

  /// BreakStatement ::= 'break' ';'
  BreakStatement *parseBreakStatement();

  /// ContinueStatement ::= 'continue' ';'
  ContinueStatement *parseContinueStatement();

  /// ReturnStatement ::= 'return' Expression? ';'
  ReturnStatement *parseReturnStatement();

  /// DeclarationStatement ::= 'var' Pattern ('=' Expression)? ';'
  DeclarationStatement *parseDeclarationStatement();

  /// AssignmentStatement ::= Pattern '=' Expression ';'
  AssignmentStatement *parseAssignmentStatement(Expression *lhs);

  /// OperatorAssignmentStatement ::= Identifier | IndexExpression
  ///       (
  ///         '+=' | '-=' | '*=' | '/=' | '%='
  ///         | '<<=' | '>>=' | '&=' | '|=' | '^='
  ///       ) Expression ';'
  OperatorAssignmentStatement *
  parseOperatorAssignmentStatement(Expression *rhs);

  /// FunctionDeclaration ::=
  ///   'fn' Identifier '(' ParameterList? ')' '{' Statement* '}'
  /// ParameterList ::= Parameter (',' Parameter)*
  /// Parameter ::= Pattern
  FunctionDeclaration *parseFunctionDeclaration();

  /// ConditionalExpression ::=
  ///   LogicalOrExpression
  ///     ('?' ConditionalExpression ':' ConditionalExpression)?
  Expression *parseConditionalExpression();

  /// LogicalOrExpression ::= LogicalAndExpression ('||' LogicalAndExpression)*
  Expression *parseLogicalOrExpression();

  /// LogicalAndExpression ::= BitwiseOrExpression ('&&' BitwiseOrExpression)*
  Expression *parseLogicalAndExpression();

  /// BitwiseOrExpression ::= BitwiseXorExpression ('|' BitwiseXorExpression)*
  Expression *parseBitwiseOrExpression();

  /// BitwiseXorExpression ::= BitwiseAndExpression ('^' BitwiseAndExpression)*
  Expression *parseBitwiseXorExpression();

  /// BitwiseAndExpression ::= EqualityExpression ('&' EqualityExpression)*
  Expression *parseBitwiseAndExpression();

  /// EqualityExpression ::= Comparison (('==' | '!=') Comparison)*
  Expression *parseEqualityExpression();

  /// Comparison ::= BitwiseShift (('<' | '<=' | '>' | '>=') BitwiseShift)*
  Expression *parseComparisonExpression();

  /// BitwiseShift ::= Addition (('<<' | '>>') Addition)*
  Expression *parseBitwiseShiftExpression();

  /// Addition ::= Multiplication (('+' | '-') Multiplication)*
  Expression *parseAdditionExpression();

  /// Multiplication ::= UnaryExpression (('*' | '/' | '%') UnaryExpression)*
  Expression *parseMultiplicationExpression();

  /// UnaryExpression ::=
  ///   ('+' | '-' | '!' | '~') UnaryExpression
  ///   | CallOrIndexExpression
  Expression *parseUnaryExpression();

  /// CallOrIndexExpression ::=
  ///   PrimaryExpression (('(' ArgumentList? ')') | ('[' Expression ']')) *
  Expression *parseCallOrIndexExpression();

  /// PrimaryExpression ::=
  ///   IdentifierExpression
  ///   | LiteralExpression
  ///   | GroupExpression
  ///   | TupleExpression
  ///   | ArrayExpression
  ///   | MatchExpression
  ///   | LambdaExpression
  Expression *parsePrimaryExpression();

  /// MatchExpression ::= 'match' Expression '{' MatchCase* '}'
  MatchExpression *parseMatchExpression();

  /// MatchCase ::= '\' Pattern '=>' Expression ';'
  MatchExpression::MatchCase parseMatchCase();

  /// LambdaExpression ::= '\' ParameterList? '=>' Expression
  LambdaExpression *parseLambdaExpression();

  /// ArrayExpression ::= '[' (Expression (',' Expression)* ','?)? ']'
  ArrayExpression *parseArrayExpression();

  /// IdentifierExpression ::= Identifier
  IdentifierExpression *parseIdentifierExpression();

  AST *parseStatementAllowedExpression();

  /// CompoundExpression ::= '{' Statement* Expression '}'
  CompoundExpression *parseCompoundExpression();

  /// TupleExpression ::=
  ///   '(' Expression? ',' ')'
  ///   | '(' Expression (',' Expression)+ ','? ')'
  /// GroupExpression ::= '(' Expression ')'
  Expression *parseTupleOrGroupExpression();

  IntegerLiteral *parseIntegerLiteral();

  BooleanLiteral *parseBooleanLiteral();

  FloatLiteral *parseFloatLiteral();

  StringLiteral *parseStringLiteral();

  NilLiteral *parseNilLiteral();

  /// Pattern ::= IdentifierPattern | TuplePattern | IntegerPattern |
  /// FloatPattern | StringPattern | GroupPattern | EmptyPattern
  Pattern *parsePattern();

  /// IdentifierPattern ::= Identifier
  Pattern *parseIdentifierPattern();

  /// TuplePattern ::=
  ///   '(' Pattern? ',' ')'
  ///   |'(' Pattern (',' Pattern)+ ','? ')'
  /// GroupPattern ::=
  ///   '(' Pattern ')'
  Pattern *parseTupleOrGroupPattern();

  IntegerPattern *parseIntegerPattern();

  FloatPattern *parseFloatPattern();

  BooleanPattern *parseBooleanPattern();

  StringPattern *parseStringPattern();

  NilPattern *parseNilPattern();

  vector<Statement *> parseStatements();

private:
  Token *advance();
  void skip();
  Token *peek(size_t look = 0);
  template <Token::Kind... Kind>
  bool peekIs();

  template <Token::Kind Kind>
  bool expect(Token *tok);

  template <Token::Kind... Kind>
  bool consume();

  void commonRecovery();

  struct ParseDiagnostic {
    enum Diag {
#define DIAG(Name, ...) Name,
#include "bara/parser/ParserDiagnostic.def"
    };

    static const char *getDiagMsg(Diag kind);
    static llvm::SourceMgr::DiagKind getDiagKind(Diag kind);
  };

  template <typename... Args>
  void report(SMRange range, ParseDiagnostic::Diag kind, Args &&...args) {
    diag.report(range, ParseDiagnostic::getDiagKind(kind),
                llvm::formatv(ParseDiagnostic::getDiagMsg(kind),
                              std::forward<Args>(args)...)
                    .str());
  }

  struct RangeCapture {
    RangeCapture(Parser &parser)
        : parser(parser), start(parser.peek()->getRange().Start) {}

    RangeCapture(Parser &parser, SMLoc start) : parser(parser), start(start) {}

    SMRange create() const {
      return SMRange(start, parser.tok->getRange().End);
    }

  private:
    Parser &parser;
    SMLoc start;
  };

  friend class PatternToExprVisitor;

private:
  Lexer &lexer;
  Diagnostic &diag;
  ASTContext *context;
  Token *tok;
};

template <Token::Kind... Kind>
bool Parser::peekIs() {
  return peek()->is<Kind...>();
}

template <Token::Kind Kind>
bool Parser::expect(Token *tok) {
  if (tok->getKind() != Kind) {
    report(tok->getRange(), ParseDiagnostic::error_unexpected_token,
           Token::getTokenString(Kind), Token::getTokenString(tok->getKind()));
    return true;
  }
  return false;
}

template <Token::Kind... Kind>
bool Parser::consume() {
  return (expect<Kind>(advance()) || ...);
}

} // namespace bara

#endif // BARA_PARSER_H
