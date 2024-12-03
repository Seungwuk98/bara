#ifndef BARA_AST_H
#define BARA_AST_H

#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {
class ASTContext;
class DeclarationStatement;
class AssignmentStatement;
class OperatorAssignmentStatement;

enum class ASTKind {
  Program,
  CompoundStatement,
  ExpressionStatement,
  IfStatement,
  WhileStatement,
  ForStatement,
  BreakStatement,
  ContinueStatement,
  ReturnStatement,
  DeclarationStatement,
  AssignmentStatement,
  OperatorAssignmentStatement,
  FunctionDeclaration,
  MatchExpression,
  BinaryExpression,
  UnaryExpression,
  CallExpression,
  ArrayExpression,
  IndexExpression,
  IdentifierExpression,
  TupleExpression,
  GroupExpression,
  IntegerLiteral,
  BooleanLiteral,
  FloatLiteral,
  StringLiteral,
  NilLiteral,
  IdentifierPattern,
  TuplePattern,
};

class AST {
public:
  template <typename U> bool isa(ASTKind kind) const { return isa<U>(this); }
  template <typename U> U *cast(ASTKind kind) const { return cast<U>(this); }
  template <typename U> U *dyn_cast(ASTKind kind) const {
    return dyn_cast<U>(this);
  }

protected:
  AST(SMRange range, ASTKind kind) : range(range), kind(kind) {};

private:
  SMRange range;
  ASTKind kind;
};

class Statement : public AST {
protected:
  using AST::AST;

public:
  static bool classof(const AST *ast);
};

class Expression : public AST {
protected:
  using AST::AST;

public:
  static bool classof(const AST *ast);
};

class Pattern : public AST {
protected:
  using AST::AST;

public:
  static bool classof(const AST *ast);
};

enum class Operator {
  Plus,   // +
  Minus,  // -
  Mul,    // *
  Div,    // /
  Mod,    // %
  Eq,     // ==
  Ne,     // !=
  Lt,     // <
  Le,     // <=
  Gt,     // >
  Ge,     // >=
  And,    // &&
  Or,     // ||
  BitAnd, // &
  BitOr,  // |
  BitXor, // ^
  Shl,    // <<
  Shr,    // >>
};

/// Program ::= Statement*
class Program final : public AST, public TrailingObjects<Program, Statement *> {
  Program(SMRange range, size_t size)
      : AST(range, ASTKind::Program), size(size) {}

public:
  static Program *create(SMRange range, ASTContext *context,
                         ArrayRef<Statement *> statements);

  size_t getSize() const { return size; }
  ArrayRef<Statement *> getStmts() const {
    return {getTrailingObjects<Statement *>(), size};
  }
  auto begin() const { return getStmts().begin(); }
  auto end() const { return getStmts().end(); }

private:
  size_t size;
};

/// CompoundStatement ::= '{' Statement* '}'
class CompoundStatement final
    : public Statement,
      public TrailingObjects<CompoundStatement, Statement *> {
  CompoundStatement(SMRange range, size_t size)
      : Statement(range, ASTKind::CompoundStatement), size(size) {}

public:
  static CompoundStatement *create(SMRange range, ASTContext *context,
                                   ArrayRef<Statement *> statements);

  size_t getSize() const { return size; }
  ArrayRef<Statement *> getStmts() const {
    return {getTrailingObjects<Statement *>(), size};
  }
  auto begin() const { return getStmts().begin(); }
  auto end() const { return getStmts().end(); }

private:
  size_t size;
};

/// ExpressionStatement ::= Expression ';'
class ExpressionStatement final : public Statement {
  ExpressionStatement(SMRange range, Expression *expr)
      : Statement(range, ASTKind::ExpressionStatement), expr(expr) {}

public:
  static ExpressionStatement *create(SMRange range, ASTContext *context,
                                     Expression *expr);

  Expression *getExpr() const { return expr; }

private:
  Expression *expr;
};

/// IfStatement ::=
///   'if' Expression '{' Statement* '}' ('else' '{' Statement* '}')?
class IfStatement final : public Statement,
                          public TrailingObjects<IfStatement, Statement *> {
  IfStatement(SMRange range, size_t thenSize, optional<size_t> elseSize)
      : Statement(range, ASTKind::IfStatement), thenSize(thenSize),
        elseSize(elseSize) {}

public:
  static IfStatement *
  create(SMRange range, ASTContext *context, Expression *cond,
         ArrayRef<Statement *> thenStmt,
         optional<ArrayRef<Statement *>> elseStmt = nullopt);

  size_t getThenSize() const { return thenSize; }
  ArrayRef<Statement *> getThenStmts() const {
    return {getTrailingObjects<Statement *>(), thenSize};
  }
  ArrayRef<Statement *> getElseStmts() const {
    if (elseSize)
      return {getTrailingObjects<Statement *>() + thenSize, *elseSize};
    return nullopt;
  }
  optional<size_t> getElseSize() const { return elseSize; }
  bool hasElse() const { return elseSize.has_value(); }

private:
  size_t thenSize;
  optional<size_t> elseSize;
};

/// WhileStatement ::= 'while' '(' Expression ')' '{' Statement* '}'
class WhileStatement final
    : public Statement,
      public TrailingObjects<WhileStatement, Statement *> {
  WhileStatement(SMRange range, Expression *cond, size_t bodySize,
                 bool isDoWhile)
      : Statement(range, ASTKind::WhileStatement), cond(cond),
        bodySize(bodySize), doWhile(isDoWhile) {}

public:
  static WhileStatement *create(SMRange range, ASTContext *context,
                                Expression *cond, ArrayRef<Statement *> body,
                                bool isDoWhile = false);

  Expression *getCond() const { return cond; }
  size_t getBodySize() const { return bodySize; }
  ArrayRef<Statement *> getBody() const {
    return {getTrailingObjects<Statement *>(), bodySize};
  }
  auto begin() const { return getBody().begin(); }
  auto end() const { return getBody().end(); }

private:
  Expression *cond;
  size_t bodySize;
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
class ForStatement final : public Statement,
                           public TrailingObjects<ForStatement, Statement *> {
  ForStatement(SMRange range, bool hasDecl, optional<Expression *> cond,
               bool hasStep, size_t bodySize)
      : Statement(range, ASTKind::ForStatement), hasDecl(hasDecl), cond(cond),
        hasStep(hasStep), bodySize(bodySize) {}

public:
  static ForStatement *create(SMRange range, ASTContext *context,
                              optional<Statement *> decl,
                              optional<Expression *> cond,
                              optional<Statement *> step,
                              ArrayRef<Statement *> body);

  optional<DeclarationStatement *> getDecl() const;
  optional<Expression *> getCond() const;
  optional<Statement *> getStep() const;
  size_t getBodySize() const;
  ArrayRef<Statement *> getBody() const;

private:
  bool hasDecl;
  optional<Expression *> cond;
  bool hasStep;
  size_t bodySize;
};

/// BreakStatement ::= 'break' ';'
class BreakStatement final : public Statement {
  friend class ASTContext;
  BreakStatement(SMRange range) : Statement(range, ASTKind::BreakStatement) {}

public:
  static BreakStatement *create(SMRange range, ASTContext *context);
};

/// ContinueStatement ::= 'continue' ';'
class ContinueStatement final : public Statement {
  friend class ASTContext;
  ContinueStatement(SMRange range)
      : Statement(range, ASTKind::ContinueStatement) {}

public:
  static ContinueStatement *create(SMRange range, ASTContext *context);
};

/// ReturnStatement ::= 'return' Expression? ';'
class ReturnStatement final : public Statement {
  friend class ASTContext;
  ReturnStatement(SMRange range, optional<Expression *> expr)
      : Statement(range, ASTKind::ReturnStatement), expr(expr) {}

public:
  static ReturnStatement *create(SMRange range, ASTContext *context,
                                 optional<Expression *> expr);

  optional<Expression *> getExpr() const { return expr; }

private:
  optional<Expression *> expr;
};

/// DeclarationStatement ::= 'var' Pattern ('=' Expression)? ';'
class DeclarationStatement final : public Statement {
  friend class ASTContext;
  DeclarationStatement(SMRange range, Pattern *pattern,
                       optional<Expression *> init)
      : Statement(range, ASTKind::DeclarationStatement), pattern(pattern),
        init(init) {}

public:
  static DeclarationStatement *create(SMRange range, ASTContext *context,
                                      Pattern *pattern,
                                      optional<Expression *> init);

  Pattern *getPattern() const { return pattern; }
  optional<Expression *> getInit() const { return init; }

private:
  Pattern *pattern;
  optional<Expression *> init;
};

/// AssignmentStatement ::= Pattern '=' Expression ';'
class AssignmentStatement final : public Statement {
  friend class ASTContext;
  AssignmentStatement(SMRange range, Pattern *pattern, Expression *expr)
      : Statement(range, ASTKind::AssignmentStatement), pattern(pattern),
        expr(expr) {}

public:
  static AssignmentStatement *create(SMRange range, ASTContext *context,
                                     Pattern *pattern, Expression *expr);

  Pattern *getPattern() const { return pattern; }
  Expression *getExpr() const { return expr; }

private:
  Pattern *pattern;
  Expression *expr;
};

/// OperatorAssignmentStatement ::= Identifier | IndexExpression
///       (
///         '+=' | '-=' | '*=' | '/=' | '%='
///         | '<<=' | '>>=' | '&=' | '|=' | '^='
///       ) Expression ';'
class OperatorAssignmentStatement final : public Statement {
  friend class ASTContext;
  OperatorAssignmentStatement(SMRange range, Expression *lhs, Operator op,
                              Expression *rhs)
      : Statement(range, ASTKind::OperatorAssignmentStatement), lhs(lhs),
        op(op), rhs(rhs) {}

public:
  static OperatorAssignmentStatement *create(SMRange range, ASTContext *context,
                                             Expression *lhs, Operator op,
                                             Expression *rhs);

  Expression *getLhs() const { return lhs; }
  Operator getOperator() const { return op; }
  Expression *getRhs() const { return rhs; }

private:
  Expression *lhs;
  Operator op;
  Expression *rhs;
};

/// FunctionDeclaration ::=
/// 'fn' Identifier '(' ParameterList? ')' '{' Statement* '}'
/// ParameterList ::= Parameter (',' Parameter)*
/// Parameter ::= Pattern
class FunctionDeclaration final
    : public Statement,
      public TrailingObjects<FunctionDeclaration, Pattern *, Statement *> {
  friend class TrailingObjects;
  FunctionDeclaration(SMRange range, StringRef name, size_t paramSize,
                      size_t bodySize)
      : Statement(range, ASTKind::FunctionDeclaration), name(name),
        paramSize(paramSize), bodySize(bodySize) {}

  size_t numTrailingObjects(OverloadToken<Pattern *>) const {
    return paramSize;
  }

public:
  static FunctionDeclaration *create(SMRange range, ASTContext *context,
                                     StringRef name, ArrayRef<Pattern *> params,
                                     ArrayRef<Statement *> body);

  StringRef getName() const { return name; }
  size_t getParamSize() const { return paramSize; }
  size_t getBodySize() const { return bodySize; }
  ArrayRef<Pattern *> getParams() const {
    return {getTrailingObjects<Pattern *>(), paramSize};
  }
  ArrayRef<Statement *> getBody() const {
    return {getTrailingObjects<Statement *>(), bodySize};
  }

private:
  string name;
  size_t paramSize;
  size_t bodySize;
};

/// MatchExpression ::= 'match' '(' Expression ')' '{' MatchCase* '}'
/// MatchCase ::= 'case' Pattern ':' Statement*
class MatchExpression final
    : public Expression,
      public TrailingObjects<MatchExpression,
                             std::pair<Pattern *, Statement *>> {
  MatchExpression(SMRange range, Expression *expr, size_t matchCaseSize)
      : Expression(range, ASTKind::MatchExpression), expr(expr),
        matchCaseSize(matchCaseSize) {}

public:
  using MatchCase = std::pair<Pattern *, Statement *>;
  static MatchExpression *
  create(SMRange range, ASTContext *context, Expression *expr,
         ArrayRef<std::pair<Pattern *, Statement *>> cases);

  Expression *getExpr() const { return expr; }
  size_t getMatchCaseSize() const { return matchCaseSize; }
  ArrayRef<std::pair<Pattern *, Statement *>> getMatchCases() const {
    return {getTrailingObjects<std::pair<Pattern *, Statement *>>(),
            matchCaseSize};
  }

private:
  Expression *expr;
  size_t matchCaseSize;
};

/// BinaryExpression ::= Expression BinaryOperator Expression
/// BinaryOperator ::= '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '<='
/// | '>' | '>=' | '&&' | '||' | '&' | '|' | '^' | '<<' | '>>'
class BinaryExpression final : public Expression {
  friend class ASTContext;
  BinaryExpression(SMRange range, Expression *lhs, Operator op, Expression *rhs)
      : Expression(range, ASTKind::BinaryExpression), lhs(lhs), op(op),
        rhs(rhs) {}

public:
  static BinaryExpression *create(SMRange range, ASTContext *context,
                                  Expression *lhs, Operator op,
                                  Expression *rhs);

  Expression *getLhs() const { return lhs; }
  Operator getOperator() const { return op; }
  Expression *getRhs() const { return rhs; }

private:
  Expression *lhs;
  Operator op;
  Expression *rhs;
};

/// UnaryExpression ::= UnaryOperator Expression
/// UnaryOperator ::= '+' | '-' | '!' | '~'
class UnaryExpression final : public Expression {
  friend class ASTContext;
  UnaryExpression(SMRange range, Operator op, Expression *expr)
      : Expression(range, ASTKind::UnaryExpression), op(op), expr(expr) {}

public:
  static UnaryExpression *create(SMRange range, ASTContext *context,
                                 Operator op, Expression *expr);

  Operator getOperator() const { return op; }
  Expression *getExpr() const { return expr; }

private:
  Operator op;
  Expression *expr;
};

/// CallExpression ::= Expression '(' ArgumentList? ')'
/// ArgumentList ::= Expression (',' Expression)*
class CallExpression final
    : public Expression,
      public TrailingObjects<CallExpression, Expression *> {
  CallExpression(SMRange range, size_t argSize)
      : Expression(range, ASTKind::CallExpression), argSize(argSize) {}

public:
  static CallExpression *create(SMRange range, ASTContext *context,
                                Expression *callee,
                                ArrayRef<Expression *> args);

  size_t getArgSize() const { return argSize; }
  Expression *getCallee() const { return *getTrailingObjects<Expression *>(); }
  ArrayRef<Expression *> getArgs() const {
    return {getTrailingObjects<Expression *>() + 1, argSize};
  }

private:
  size_t argSize;
};

/// ArrayExpression ::= '[' ((Expression (',' Expression)*)? ','? )?']'
class ArrayExpression final
    : public Expression,
      public TrailingObjects<ArrayExpression, Expression *> {
  ArrayExpression(SMRange range, size_t size)
      : Expression(range, ASTKind::ArrayExpression), size(size) {}

public:
  static ArrayExpression *create(SMRange range, ASTContext *context,
                                 ArrayRef<Expression *> args);

  size_t getSize() const { return size; }
  ArrayRef<Expression *> getArgs() const {
    return {getTrailingObjects<Expression *>(), size};
  }
  auto begin() const { return getArgs().begin(); }
  auto end() const { return getArgs().end(); }

private:
  size_t size;
};

/// IndexExpression ::= Expression '[' Expression ']'
class IndexExpression final : public Expression {
  friend class ASTContext;
  IndexExpression(SMRange range, Expression *lhs, Expression *rhs)
      : Expression(range, ASTKind::IndexExpression), lhs(lhs), rhs(rhs) {}

public:
  static IndexExpression *create(SMRange range, ASTContext *context,
                                 Expression *lhs, Expression *rhs);

  Expression *getLhs() const { return lhs; }
  Expression *getRhs() const { return rhs; }

private:
  Expression *lhs;
  Expression *rhs;
};

/// IdentifierExpression ::= Identifier
class IdentifierExpression final : public Expression {
  friend class ASTContext;
  IdentifierExpression(SMRange range, StringRef name)
      : Expression(range, ASTKind::IdentifierPattern), name(name) {}

public:
  static IdentifierExpression *create(SMRange range, ASTContext *context,
                                      StringRef name);

  StringRef getName() const { return name; }

private:
  string name;
};

/// TupleExpression ::= '(' Expression (',' Expression)* ','? ')'
class TupleExpression final
    : public Expression,
      public TrailingObjects<TupleExpression, Expression *> {
  TupleExpression(SMRange range, size_t size)
      : Expression(range, ASTKind::TupleExpression), size(size) {}

public:
  static TupleExpression *create(SMRange range, ASTContext *context,
                                 ArrayRef<Expression *> exprs);

  size_t getSize() const { return size; }
  ArrayRef<Expression *> getExprs() const {
    return {getTrailingObjects<Expression *>(), size};
  }

private:
  size_t size;
};

/// GroupExpression ::= '(' Expression ')'
class GroupExpression final : public Expression {
  friend class ASTContext;
  GroupExpression(SMRange range, Expression *expr)
      : Expression(range, ASTKind::GroupExpression), expr(expr) {}

public:
  static GroupExpression *create(SMRange range, ASTContext *context,
                                 Expression *expr);

  Expression *getExpr() const { return expr; }

private:
  Expression *expr;
};

/// IntegerLiteral
class IntegerLiteral final : public Expression {
  friend class ASTContext;
  IntegerLiteral(SMRange range, uint64_t value)
      : Expression(range, ASTKind::IntegerLiteral), value(value) {}

public:
  static IntegerLiteral *create(SMRange range, ASTContext *context,
                                uint64_t value);

  uint64_t getValue() const { return value; }

private:
  uint64_t value;
};

/// BooleanLiteral ::= 'true' | 'false'
class BooleanLiteral final : public Expression {
  friend class ASTContext;
  BooleanLiteral(SMRange range, bool value)
      : Expression(range, ASTKind::BooleanLiteral), value(value) {}

public:
  static BooleanLiteral *create(SMRange range, ASTContext *context, bool value);

  bool getValue() const { return value; }

private:
  bool value;
};

/// FloatLiteral
class FloatLiteral final : public Expression {
  friend class ASTContext;
  FloatLiteral(SMRange range, StringRef value)
      : Expression(range, ASTKind::FloatLiteral), value(value) {}

public:
  static FloatLiteral *create(SMRange range, ASTContext *context,
                              StringRef value);

  StringRef getValue() const { return value; }

private:
  string value;
};

/// StringLiteral
class StringLiteral final : public Expression {
  friend class ASTContext;
  StringLiteral(SMRange range, StringRef value)
      : Expression(range, ASTKind::StringLiteral), value(value) {}

public:
  static StringLiteral *create(SMRange range, ASTContext *context,
                               StringRef value);

  StringRef getValue() const { return value; }

private:
  string value;
};

/// NilLiteral ::= 'nil'
class NilLiteral final : public Expression {
  friend class ASTContext;
  NilLiteral(SMRange range) : Expression(range, ASTKind::NilLiteral) {}

public:
  static NilLiteral *create(SMRange range, ASTContext *context);
};

/// IdentifierPattern ::= Identifier
class IdentifierPattern final : public Pattern {
  friend class ASTContext;
  IdentifierPattern(SMRange range, StringRef name)
      : Pattern(range, ASTKind::IdentifierPattern), name(name) {}

public:
  static IdentifierPattern *create(SMRange range, ASTContext *context,
                                   StringRef name);

  StringRef getName() const { return name; }

private:
  string name;
};

/// TuplePattern ::= '(' Pattern (',' Pattern)* ','? ')'
class TuplePattern final : public Pattern,
                           public TrailingObjects<TuplePattern, Pattern *> {
  TuplePattern(SMRange range, size_t size)
      : Pattern(range, ASTKind::TuplePattern), size(size) {}

public:
  static TuplePattern *create(SMRange range, ASTContext *context,
                              ArrayRef<Pattern *> patterns);

  size_t getSize() const { return size; }
  ArrayRef<Pattern *> getPatterns() const {
    return {getTrailingObjects<Pattern *>(), size};
  }
  auto begin() const { return getPatterns().begin(); }
  auto end() const { return getPatterns().end(); }

private:
  size_t size;
};

} // namespace bara

#endif // BARA_AST_H
