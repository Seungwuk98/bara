#ifndef BARA_AST_H
#define BARA_AST_H

#include "bara/ast/ASTPrinter.h"
#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {

class Visitor;
class ConstVisitor;

#define AST_KIND(NAME) class NAME;
#include "bara/ast/AST.def"

class ASTContext;

enum class ASTKind : uint16_t {
#define AST_KIND(NAME) NAME,
#include "bara/ast/AST.def"

  NUM_OF_AST,
};

namespace _inner {
template <typename T>
struct ASTKindMap {};

#define AST_KIND(NAME)                                                         \
  template <>                                                                  \
  struct ASTKindMap<NAME> {                                                    \
    static constexpr ASTKind value = ASTKind::NAME;                            \
  };
#include "bara/ast/AST.def"
} // namespace _inner

class AST {
public:
  template <typename... U>
  bool isa() const {
    return llvm::isa<U...>(this);
  }
  template <typename U>
  const U *cast() const {
    return llvm::cast<U>(this);
  }
  template <typename U>
  U *cast() {
    return llvm::cast<U>(this);
  }
  template <typename U>
  const U *dyn_cast() const {
    return llvm::dyn_cast<U>(this);
  }
  template <typename U>
  U *dyn_cast() {
    return llvm::dyn_cast<U>(this);
  }

  ASTKind getKind() const { return kind; }

  void accept(Visitor &visitor);
  void accept(ConstVisitor &visitor) const;

  string toString() const {
    string str;
    raw_string_ostream os(str);
    ASTPrinter printer(os);
    print(printer);
    return os.str();
  }

  void print(ASTPrinter &printer) const;

  void print() const {
    ASTPrinter printer(errs());
    print(printer);
  }

  bool isEqual(const AST *other) const;

  SMRange getRange() const { return range; }

protected:
  AST(SMRange range, ASTKind kind) : range(range), kind(kind) {};

private:
  SMRange range;
  ASTKind kind;
};

template <typename ConcreteType, typename ParentType>
class ASTBase : public ParentType {
public:
  using ParentType::ParentType;

  static bool classof(const AST *ast) {
    return ast->getKind() == _inner::ASTKindMap<ConcreteType>::value;
  }
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
  Assign, // =
  Eq,     // ==
  Ne,     // !=
  Lt,     // <
  Le,     // <=
  Gt,     // >
  Ge,     // >=
  And,    // &&
  Or,     // ||
  Not,    // !
  BitAnd, // &
  BitOr,  // |
  BitXor, // ^
  BitNot, // ~
  Shl,    // <<
  Shr,    // >>
};

/// Program ::= Statement*
class Program final : public ASTBase<Program, AST>,
                      public TrailingObjects<Program, Statement *> {
  Program(SMRange range, size_t size)
      : ASTBase(range, ASTKind::Program), size(size) {}

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
    : public ASTBase<CompoundStatement, Statement>,
      public TrailingObjects<CompoundStatement, Statement *> {
  CompoundStatement(SMRange range, size_t size)
      : ASTBase(range, ASTKind::CompoundStatement), size(size) {}

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
class ExpressionStatement final
    : public ASTBase<ExpressionStatement, Statement> {
  ExpressionStatement(SMRange range, Expression *expr)
      : ASTBase(range, ASTKind::ExpressionStatement), expr(expr) {}

public:
  static ExpressionStatement *create(SMRange range, ASTContext *context,
                                     Expression *expr);

  Expression *getExpr() const { return expr; }

private:
  Expression *expr;
};

/// IfStatement ::=
///   'if' Expression '{' Statement* '}' ('else' '{' Statement* '}')?
class IfStatement final : public ASTBase<IfStatement, Statement>,
                          public TrailingObjects<IfStatement, Statement *> {
  IfStatement(SMRange range, Expression *cond, size_t thenSize,
              optional<size_t> elseSize)
      : ASTBase(range, ASTKind::IfStatement), cond(cond), thenSize(thenSize),
        elseSize(elseSize) {}

public:
  static IfStatement *
  create(SMRange range, ASTContext *context, Expression *cond,
         ArrayRef<Statement *> thenStmt,
         optional<ArrayRef<Statement *>> elseStmt = nullopt);

  Expression *getCond() const { return cond; }
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
  Expression *cond;
  size_t thenSize;
  optional<size_t> elseSize;
};

/// WhileStatement ::= 'while' Expression '{' Statement* '}'
class WhileStatement final
    : public ASTBase<WhileStatement, Statement>,
      public TrailingObjects<WhileStatement, Statement *> {
  WhileStatement(SMRange range, Expression *cond, size_t bodySize,
                 bool isDoWhile)
      : ASTBase(range, ASTKind::WhileStatement), cond(cond), bodySize(bodySize),
        doWhile(isDoWhile) {}

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
  bool isDoWhile() const { return doWhile; }

private:
  Expression *cond;
  size_t bodySize;
  bool doWhile;
};

/// ForStatement ::=
/// 'for' '(' Declaration? ';' Expression? ';' Assignment | CompoundStatement
/// ')' '{' Statement* '}'
/// Declaration ::= 'var' Pattern ('=' Expression)?
/// Assignment ::= Pattern
///   (
///     '=' | '+=' | '-=' | '*=' | '/=' | '%='
///     | '<<=' | '>>=' | '&=' | '|=' | '^='
///   ) Expression
class ForStatement final : public ASTBase<ForStatement, Statement>,
                           public TrailingObjects<ForStatement, Statement *> {
  ForStatement(SMRange range, bool hasDecl, optional<Expression *> cond,
               bool hasStep, size_t bodySize)
      : ASTBase(range, ASTKind::ForStatement), hasDecl(hasDecl), cond(cond),
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
class BreakStatement final : public ASTBase<BreakStatement, Statement> {
  friend class ASTContext;
  BreakStatement(SMRange range) : ASTBase(range, ASTKind::BreakStatement) {}

public:
  static BreakStatement *create(SMRange range, ASTContext *context);
};

/// ContinueStatement ::= 'continue' ';'
class ContinueStatement final : public ASTBase<ContinueStatement, Statement> {
  friend class ASTContext;
  ContinueStatement(SMRange range)
      : ASTBase(range, ASTKind::ContinueStatement) {}

public:
  static ContinueStatement *create(SMRange range, ASTContext *context);
};

/// ReturnStatement ::= 'return' Expression? ';'
class ReturnStatement final : public ASTBase<ReturnStatement, Statement> {
  friend class ASTContext;
  ReturnStatement(SMRange range, optional<Expression *> expr)
      : ASTBase(range, ASTKind::ReturnStatement), expr(expr) {}

public:
  static ReturnStatement *create(SMRange range, ASTContext *context,
                                 optional<Expression *> expr);

  optional<Expression *> getExpr() const { return expr; }

private:
  optional<Expression *> expr;
};

/// DeclarationStatement ::= 'var' Pattern ('=' Expression)? ';'
class DeclarationStatement final
    : public ASTBase<DeclarationStatement, Statement> {
  friend class ASTContext;
  DeclarationStatement(SMRange range, Pattern *pattern,
                       optional<Expression *> init)
      : ASTBase(range, ASTKind::DeclarationStatement), pattern(pattern),
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

/// AssignmentStatement ::= Expression '=' Expression ';'
class AssignmentStatement final
    : public ASTBase<AssignmentStatement, Statement> {
  friend class ASTContext;
  AssignmentStatement(SMRange range, Expression *lhs, Expression *rhs)
      : ASTBase(range, ASTKind::AssignmentStatement), lhs(lhs), rhs(rhs) {}

public:
  static AssignmentStatement *create(SMRange range, ASTContext *context,
                                     Expression *lhs, Expression *rhs);

  Expression *getLhs() const { return lhs; }
  Expression *getRhs() const { return rhs; }

private:
  Expression *lhs;
  Expression *rhs;
};

/// OperatorAssignmentStatement ::= Identifier | IndexExpression
///       (
///         '+=' | '-=' | '*=' | '/=' | '%='
///         | '<<=' | '>>=' | '&=' | '|=' | '^='
///       ) Expression ';'
class OperatorAssignmentStatement final
    : public ASTBase<OperatorAssignmentStatement, Statement> {
  friend class ASTContext;
  OperatorAssignmentStatement(SMRange range, Expression *lhs, Operator op,
                              Expression *rhs)
      : ASTBase(range, ASTKind::OperatorAssignmentStatement), lhs(lhs), op(op),
        rhs(rhs) {}

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
///   'fn' Identifier '(' ParameterList? ')' '{' Statement* '}'
/// ParameterList ::= Parameter (',' Parameter)*
/// Parameter ::= Pattern
class FunctionDeclaration final
    : public ASTBase<FunctionDeclaration, Statement>,
      public TrailingObjects<FunctionDeclaration, Pattern *, Statement *> {
  friend class TrailingObjects;
  FunctionDeclaration(SMRange range, StringRef name, size_t paramSize,
                      size_t bodySize)
      : ASTBase(range, ASTKind::FunctionDeclaration), name(name),
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
/// MatchCase ::= '\' Pattern '=>' Expression ';'
class MatchExpression final
    : public ASTBase<MatchExpression, Expression>,
      public TrailingObjects<MatchExpression,
                             std::pair<Pattern *, Expression *>> {
  MatchExpression(SMRange range, Expression *expr, size_t matchCaseSize)
      : ASTBase(range, ASTKind::MatchExpression), expr(expr),
        matchCaseSize(matchCaseSize) {}

public:
  using MatchCase = std::pair<Pattern *, Expression *>;
  static MatchExpression *create(SMRange range, ASTContext *context,
                                 Expression *expr, ArrayRef<MatchCase> cases);

  Expression *getExpr() const { return expr; }
  size_t getMatchCaseSize() const { return matchCaseSize; }
  ArrayRef<MatchCase> getMatchCases() const {
    return {getTrailingObjects<MatchCase>(), matchCaseSize};
  }

private:
  Expression *expr;
  size_t matchCaseSize;
};

/// LambdaExpression ::=
///   '\' ParamList? '=>' (Expression | '{' Statement* '}')
class LambdaExpression final
    : public ASTBase<LambdaExpression, Expression>,
      public TrailingObjects<LambdaExpression, Pattern *, AST *> {
  friend class ASTContext;
  friend class TrailingObjects;

  LambdaExpression(SMRange range, size_t paramSize, bool isExpr,
                   size_t bodySize)
      : ASTBase(range, ASTKind::LambdaExpression), paramSize(paramSize),
        isExpr(isExpr), bodySize(bodySize) {}

  size_t numTrailingObjects(OverloadToken<Pattern *>) const {
    return paramSize;
  }

public:
  static LambdaExpression *create(SMRange range, ASTContext *context,
                                  ArrayRef<Pattern *> params, Expression *expr);

  static LambdaExpression *create(SMRange range, ASTContext *context,
                                  ArrayRef<Pattern *> params,
                                  ArrayRef<Statement *> body);

  size_t getParamSize() const { return paramSize; }
  bool isExprBody() const { return isExpr; }
  Expression *getExpr() const;
  ArrayRef<Pattern *> getParams() const {
    return {getTrailingObjects<Pattern *>(), paramSize};
  }
  ArrayRef<Statement *> getStmtBody() const;
  size_t getBodySize() const { return bodySize; }

private:
  size_t paramSize;
  bool isExpr;
  size_t bodySize;
};

/// BinaryExpression ::= Expression BinaryOperator Expression
/// BinaryOperator ::= '+' | '-' | '*' | '/' | '%' | '==' | '!=' | '<' | '<='
/// | '>' | '>=' | '&&' | '||' | '&' | '|' | '^' | '<<' | '>>'
class BinaryExpression final : public ASTBase<BinaryExpression, Expression> {
  friend class ASTContext;
  BinaryExpression(SMRange range, Expression *lhs, Operator op, Expression *rhs)
      : ASTBase(range, ASTKind::BinaryExpression), lhs(lhs), op(op), rhs(rhs) {}

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
class UnaryExpression final : public ASTBase<UnaryExpression, Expression> {
  friend class ASTContext;
  UnaryExpression(SMRange range, Operator op, Expression *expr)
      : ASTBase(range, ASTKind::UnaryExpression), op(op), expr(expr) {}

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
    : public ASTBase<CallExpression, Expression>,
      public TrailingObjects<CallExpression, Expression *> {
  CallExpression(SMRange range, size_t argSize)
      : ASTBase(range, ASTKind::CallExpression), argSize(argSize) {}

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
    : public ASTBase<ArrayExpression, Expression>,
      public TrailingObjects<ArrayExpression, Expression *> {
  ArrayExpression(SMRange range, size_t size)
      : ASTBase(range, ASTKind::ArrayExpression), size(size) {}

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
class IndexExpression final : public ASTBase<IndexExpression, Expression> {
  friend class ASTContext;
  IndexExpression(SMRange range, Expression *lhs, Expression *rhs)
      : ASTBase(range, ASTKind::IndexExpression), lhs(lhs), rhs(rhs) {}

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
class IdentifierExpression final
    : public ASTBase<IdentifierExpression, Expression> {
  friend class ASTContext;
  IdentifierExpression(SMRange range, StringRef name)
      : ASTBase(range, ASTKind::IdentifierPattern), name(name) {}

public:
  static IdentifierExpression *create(SMRange range, ASTContext *context,
                                      StringRef name);

  StringRef getName() const { return name; }

private:
  StringRef name;
};

/// TupleExpression ::=
///   | '(' ')'
///   | '(' Expression? ',' ')'
///   | '(' Expression (',' Expression)+ ','? ')'
class TupleExpression final
    : public ASTBase<TupleExpression, Expression>,
      public TrailingObjects<TupleExpression, Expression *> {
  TupleExpression(SMRange range, size_t size)
      : ASTBase(range, ASTKind::TupleExpression), size(size) {}

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
class GroupExpression final : public ASTBase<GroupExpression, Expression> {
  friend class ASTContext;
  GroupExpression(SMRange range, Expression *expr)
      : ASTBase(range, ASTKind::GroupExpression), expr(expr) {}

public:
  static GroupExpression *create(SMRange range, ASTContext *context,
                                 Expression *expr);

  Expression *getExpr() const { return expr; }

private:
  Expression *expr;
};

/// IntegerLiteral
class IntegerLiteral final : public ASTBase<IntegerLiteral, Expression> {
  friend class ASTContext;
  IntegerLiteral(SMRange range, uint64_t value)
      : ASTBase(range, ASTKind::IntegerLiteral), value(value) {}

public:
  static IntegerLiteral *create(SMRange range, ASTContext *context,
                                uint64_t value);

  uint64_t getValue() const { return value; }

private:
  uint64_t value;
};

/// BooleanLiteral ::= 'true' | 'false'
class BooleanLiteral final : public ASTBase<BooleanLiteral, Expression> {
  friend class ASTContext;
  BooleanLiteral(SMRange range, bool value)
      : ASTBase(range, ASTKind::BooleanLiteral), value(value) {}

public:
  static BooleanLiteral *create(SMRange range, ASTContext *context, bool value);

  bool getValue() const { return value; }

private:
  bool value;
};

/// FloatLiteral
class FloatLiteral final : public ASTBase<FloatLiteral, Expression> {
  friend class ASTContext;
  FloatLiteral(SMRange range, StringRef value)
      : ASTBase(range, ASTKind::FloatLiteral), value(value) {}

public:
  static FloatLiteral *create(SMRange range, ASTContext *context,
                              StringRef value);

  StringRef getValue() const { return value; }

private:
  string value;
};

/// StringLiteral
class StringLiteral final : public ASTBase<StringLiteral, Expression> {
  friend class ASTContext;
  StringLiteral(SMRange range, StringRef value)
      : ASTBase(range, ASTKind::StringLiteral), value(value) {}

public:
  static StringLiteral *create(SMRange range, ASTContext *context,
                               StringRef value);

  StringRef getValue() const { return value; }

private:
  string value;
};

/// NilLiteral ::= 'nil'
class NilLiteral final : public ASTBase<NilLiteral, Expression> {
  friend class ASTContext;
  NilLiteral(SMRange range) : ASTBase(range, ASTKind::NilLiteral) {}

public:
  static NilLiteral *create(SMRange range, ASTContext *context);
};

/// IdentifierPattern ::= Identifier
class IdentifierPattern final : public ASTBase<IdentifierPattern, Pattern> {
  friend class ASTContext;
  IdentifierPattern(SMRange range, StringRef name)
      : ASTBase(range, ASTKind::IdentifierPattern), name(name) {}

public:
  static IdentifierPattern *create(SMRange range, ASTContext *context,
                                   StringRef name);

  StringRef getName() const { return name; }

private:
  StringRef name;
};

/// TuplePattern ::=
///   | '(' ')'
///   | '(' Pattern? ',' ')'
///   | '(' Pattern (',' Pattern)+ ','? ')'
class TuplePattern final : public ASTBase<TuplePattern, Pattern>,
                           public TrailingObjects<TuplePattern, Pattern *> {
  TuplePattern(SMRange range, size_t size)
      : ASTBase(range, ASTKind::TuplePattern), size(size) {}

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

class GroupPattern final : public ASTBase<GroupPattern, Pattern> {
  friend class ASTContext;
  GroupPattern(SMRange range, Pattern *pattern)
      : ASTBase(range, ASTKind::GroupPattern), pattern(pattern) {}

public:
  static GroupPattern *create(SMRange range, ASTContext *context,
                              Pattern *pattern);

  Pattern *getPattern() const { return pattern; }

private:
  Pattern *pattern;
};

class IntegerPattern final : public ASTBase<IntegerPattern, Pattern> {
  friend class ASTContext;
  IntegerPattern(SMRange range, uint64_t value)
      : ASTBase(range, ASTKind::IntegerPattern), value(value) {}

public:
  static IntegerPattern *create(SMRange range, ASTContext *context,
                                uint64_t value);

  uint64_t getValue() const { return value; }

private:
  uint64_t value;
};

class FloatPattern final : public ASTBase<FloatPattern, Pattern> {
  friend class ASTContext;
  FloatPattern(SMRange range, StringRef value)
      : ASTBase(range, ASTKind::FloatPattern), value(value) {}

public:
  static FloatPattern *create(SMRange range, ASTContext *context,
                              StringRef value);

  StringRef getValue() const { return value; }

private:
  StringRef value;
};

class StringPattern final : public ASTBase<StringPattern, Pattern> {
  friend class ASTContext;
  StringPattern(SMRange range, StringRef value)
      : ASTBase(range, ASTKind::StringPattern), value(value) {}

public:
  static StringPattern *create(SMRange range, ASTContext *context,
                               StringRef value);

  StringRef getValue() const { return value; }

private:
  StringRef value;
};

class BooleanPattern final : public ASTBase<BooleanPattern, Pattern> {
  friend class ASTContext;
  BooleanPattern(SMRange range, bool value)
      : ASTBase(range, ASTKind::BooleanPattern), value(value) {}

public:
  static BooleanPattern *create(SMRange range, ASTContext *context, bool value);

  bool getValue() const { return value; }

private:
  bool value;
};

class EmptyPattern final : public ASTBase<EmptyPattern, Pattern> {
  friend class ASTContext;
  EmptyPattern(SMRange range) : ASTBase(range, ASTKind::EmptyPattern) {}

public:
  static EmptyPattern *create(SMRange range, ASTContext *context);
};

class NilPattern final : public ASTBase<NilPattern, Pattern> {
  friend class ASTContext;
  NilPattern(SMRange range) : ASTBase(range, ASTKind::NilPattern) {}

public:
  static NilPattern *create(SMRange range, ASTContext *context);
};

} // namespace bara

#endif // BARA_AST_H
