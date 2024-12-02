#ifndef BARA_AST_H
#define BARA_AST_H

#include "bara/utils/LLVM.h"
#include <cstdint>
#include <optional>
#include <variant>

namespace bara {
class ASTContext;
class DeclarationStatement;
class AssignmentStatement;
class OperatorAssignmentStatement;

class AST {
public:
private:
};

class Statement : public AST {};

class Expression : public AST {};

/// Program ::= Statement*
class Program : public Statement, public TrailingObjects<Program, Statement *> {
  Program(std::size_t size) : size(size) {}

public:
  static Program *create(ASTContext *context, ArrayRef<Statement *> statements);

  std::size_t getSize() const { return size; }
  ArrayRef<Statement *> getStmts() const {
    return {getTrailingObjects<Statement *>(), size};
  }
  auto begin() const { return getStmts().begin(); }
  auto end() const { return getStmts().end(); }

private:
  std::size_t size;
};

/// CompoundStatement ::= '{' Statement* '}'
class CompoundStatement
    : public Statement,
      public TrailingObjects<CompoundStatement, Statement *> {
  CompoundStatement(std::size_t size) : size(size) {}

public:
  static CompoundStatement *create(ASTContext *context,
                                   ArrayRef<Statement *> statements);

  std::size_t getSize() const { return size; }
  ArrayRef<Statement *> getStmts() const {
    return {getTrailingObjects<Statement *>(), size};
  }
  auto begin() const { return getStmts().begin(); }
  auto end() const { return getStmts().end(); }

private:
  std::size_t size;
};

/// ExpressionStatement ::= Expression ';'
class ExpressionStatement : public Statement {
  ExpressionStatement(Expression *expr) : expr(expr) {}

public:
  static ExpressionStatement *create(ASTContext *context, Expression *expr);

  Expression *getExpr() const { return expr; }

private:
  Expression *expr;
};

/// IfStatement ::=
///   'if' Expression '{' Statement* '}' ('else' '{' Statement* '}')?
class IfStatement : public Statement,
                    public TrailingObjects<IfStatement, Statement *> {
  IfStatement(std::size_t thenSize, std::optional<std::size_t> elseSize)
      : thenSize(thenSize), elseSize(elseSize) {}

public:
  static IfStatement *
  create(ASTContext *context, Expression *cond, ArrayRef<Statement *> thenStmt,
         std::optional<ArrayRef<Statement *>> elseStmt = std::nullopt);

  std::size_t getThenSize() const { return thenSize; }
  ArrayRef<Statement *> getThenStmts() const {
    return {getTrailingObjects<Statement *>(), thenSize};
  }
  ArrayRef<Statement *> getElseStmts() const {
    if (elseSize)
      return {getTrailingObjects<Statement *>() + thenSize, *elseSize};
    return std::nullopt;
  }
  std::optional<std::size_t> getElseSize() const { return elseSize; }
  bool hasElse() const { return elseSize.has_value(); }

private:
  std::size_t thenSize;
  std::optional<std::size_t> elseSize;
};

/// WhileStatement ::= 'while' '(' Expression ')' '{' Statement* '}'
class WhileStatement : public Statement,
                       public TrailingObjects<WhileStatement, Statement *> {
  WhileStatement(Expression *cond, std::size_t bodySize, bool isDoWhile)
      : cond(cond), bodySize(bodySize), doWhile(isDoWhile) {}

public:
  static WhileStatement *create(ASTContext *context, Expression *cond,
                                ArrayRef<Statement *> body,
                                bool isDoWhile = false);

  Expression *getCond() const { return cond; }
  std::size_t getBodySize() const { return bodySize; }
  ArrayRef<Statement *> getBody() const {
    return {getTrailingObjects<Statement *>(), bodySize};
  }
  auto begin() const { return getBody().begin(); }
  auto end() const { return getBody().end(); }

private:
  Expression *cond;
  std::size_t bodySize;
  bool doWhile;
};

/// ForStatement ::=
/// 'for' '(' Declaration? ';' Expression? ';' Assignment | CompoundStatement')'
/// '{' Statement* '}'
/// Declaration ::= 'var' Pattern ('=' Expression)?
/// Assignment ::= Pattern
///   (
///     '=' | '+=' | '-=' | '*=' | '/=' | '%='
///     | '<<=' | '>>=' | '&=' | '|=' | '^='
///   ) Expression
class ForStatement : public Statement,
                     public TrailingObjects<ForStatement, Statement *> {
  ForStatement(std::optional<Expression *> cond, std::size_t bodySize);

public:
  static ForStatement *create(ASTContext *context,
                              std::optional<DeclarationStatement *> decl,
                              std::optional<Expression *> cond,
                              std::optional<OperatorAssignmentStatement *> step,
                              ArrayRef<Statement *> body);

  std::optional<DeclarationStatement *> getDecl() const;
  std::optional<Expression *> getCond() const;
  std::optional<OperatorAssignmentStatement *> getStep() const;
  std::size_t getBodySize() const;
  ArrayRef<Statement *> getBody() const;

private:
  bool hasDecl;
  std::optional<Expression *> cond;
  bool hasStep;
  std::size_t bodySize;
};

/// BreakStatement ::= 'break' ';'
class BreakStatement : public Statement {
public:
  static BreakStatement *create(ASTContext *context);
};

/// ContinueStatement ::= 'continue' ';'
class ContinueStatement : public Statement {
public:
  static ContinueStatement *create(ASTContext *context);
};

/// ReturnStatement ::= 'return' Expression? ';'
class ReturnStatement : public Statement {
  ReturnStatement(std::optional<Expression *> expr) : expr(expr) {}

public:
  static ReturnStatement *create(ASTContext *context,
                                 std::optional<Expression *> expr);

  std::optional<Expression *> getExpr() const { return expr; }

private:
  std::optional<Expression *> expr;
};

/// DeclarationStatement ::= 'var' Pattern ('=' Expression)? ';'
class DeclarationStatement : public Statement {};

/// AssignmentStatement ::= Pattern '=' Expression ';'
class AssignmentStatement : public Statement {};

/// OperatorAssignmentStatement ::= Identifier
///       (
///         '+=' | '-=' | '*=' | '/=' | '%='
///         | '<<=' | '>>=' | '&=' | '|=' | '^='
///       ) Expression ';'
class OperatorAssignmentStatement : public Statement {};

/// FunctionDeclaration ::=
/// 'fn' Identifier '(' ParameterList? ')' '{' Statement* '}'
/// ParameterList ::= Parameter (',' Parameter)*
/// Parameter ::= Pattern
class FunctionDeclaration : public Statement {};

/// MatchExpression ::= 'match' '(' Expression ')' '{' MatchCase* '}'
/// MatchCase ::= 'case' Pattern ':' Statement*
class MatchExpression : public Expression {};

/// BinaryExpression ::= Expression BinaryOperator Expression
/// BinaryOperator ::= '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '<='
/// |
///                    '>' | '>=' | '&&' | '||' | '&' | '|' | '^' | '<<' |
///                    '>>'
class BinaryExpression : public Expression {};

/// UnaryExpression ::= UnaryOperator Expression
/// UnaryOperator ::= '+' | '-' | '!' | '~'
class UnaryExpression : public Expression {};

/// CallExpression ::= Expression '(' ArgumentList? ')'
/// ArgumentList ::= Expression (',' Expression)*
class CallExpression : public Expression {};

/// ArrayExpression ::= '[' (Expression (',' Expression)*)? ']'
class ArrayExpression : public Expression {};

/// IndexExpression ::= Expression '[' Expression ']'
class IndexExpression : public Expression {};

/// IdentifierExpression ::= Identifier
class IdentifierExpression : public Expression {};

/// IntegerLiteral
class IntegerLiteral : public Expression {};

/// BooleanLiteral ::= 'true' | 'false'
class BooleanLiteral : public Expression {};

/// FloatLiteral
class FloatLiteral : public Expression {};

/// StringLiteral
class StringLiteral : public Expression {};

/// NilLiteral ::= 'nil'
class NilLiteral : public Expression {};

} // namespace bara

#endif // BARA_AST_H
