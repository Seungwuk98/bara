#include "bara/ast/AST.h"
#include "bara/context/ASTContext.h"

namespace bara {

Program *Program::create(SMRange range, ASTContext *context,
                         ArrayRef<Statement *> statements) {
  auto allocSize = totalSizeToAlloc<Statement *>(statements.size());
  void *mem = context->alloc(allocSize);
  auto *program = new (mem) Program(range, statements.size());
  std::uninitialized_copy(statements.begin(), statements.end(),
                          program->getTrailingObjects<Statement *>());
  return program;
}

CompoundStatement *CompoundStatement::create(SMRange range, ASTContext *context,
                                             ArrayRef<Statement *> statements) {
  auto allocSize = totalSizeToAlloc<Statement *>(statements.size());
  void *mem = context->alloc(allocSize);
  auto *stmt = new (mem) CompoundStatement(range, statements.size());
  std::uninitialized_copy(statements.begin(), statements.end(),
                          stmt->getTrailingObjects<Statement *>());
  return stmt;
}

ExpressionStatement *ExpressionStatement::create(SMRange range,
                                                 ASTContext *context,
                                                 Expression *expr) {
  void *mem = context->alloc(sizeof(ExpressionStatement));
  return new (mem) ExpressionStatement(range, expr);
}

IfStatement *
IfStatement::create(SMRange range, ASTContext *context, Expression *cond,
                    ArrayRef<Statement *> thenStmt,
                    std::optional<ArrayRef<Statement *>> elseStmt) {
  auto totalCnt = thenStmt.size() + (elseStmt ? elseStmt->size() : 0);
  auto allocSize = totalSizeToAlloc<Statement *>(totalCnt);
  void *mem = context->alloc(allocSize);

  auto ifStmt = new (mem)
      IfStatement(range, thenStmt.size(),
                  elseStmt ? std::optional(elseStmt->size()) : std::nullopt);
  std::uninitialized_copy(thenStmt.begin(), thenStmt.end(),
                          ifStmt->getTrailingObjects<Statement *>());
  if (elseStmt) {
    std::uninitialized_copy(elseStmt->begin(), elseStmt->end(),
                            ifStmt->getTrailingObjects<Statement *>() +
                                thenStmt.size());
  }
  return ifStmt;
}

WhileStatement *WhileStatement::create(SMRange range, ASTContext *context,
                                       Expression *cond,
                                       ArrayRef<Statement *> body,
                                       bool isDoWhile) {
  auto allocSize = totalSizeToAlloc<Statement *>(body.size());
  void *mem = context->alloc(allocSize);
  auto whileStmt =
      new (mem) WhileStatement(range, cond, body.size(), isDoWhile);
  std::uninitialized_copy(body.begin(), body.end(),
                          whileStmt->getTrailingObjects<Statement *>());
  return whileStmt;
}

ForStatement *ForStatement::create(SMRange range, ASTContext *context,
                                   std::optional<Statement *> decl,
                                   std::optional<Expression *> cond,
                                   std::optional<Statement *> step,
                                   ArrayRef<Statement *> body) {
  auto totalCnt = bool(decl) + bool(step) + body.size();
  auto allocSize = totalSizeToAlloc<Statement *>(totalCnt);
  void *mem = context->alloc(allocSize);
  auto *forStmt = new (mem) ForStatement(range, decl.has_value(), cond,
                                         step.has_value(), body.size());
  std::size_t idx = 0;
  auto *stmts = forStmt->getTrailingObjects<Statement *>();
  if (decl)
    stmts[idx++] = static_cast<Statement *>(*decl);
  if (step)
    stmts[idx++] = static_cast<Statement *>(*step);
  std::uninitialized_copy(body.begin(), body.end(), stmts + idx);
  return forStmt;
}

std::optional<DeclarationStatement *> ForStatement::getDecl() const {
  if (hasDecl)
    return static_cast<DeclarationStatement *>(
        getTrailingObjects<Statement *>()[0]);
  return std::nullopt;
}
std::optional<Expression *> ForStatement::getCond() const { return cond; }
std::optional<Statement *> ForStatement::getStep() const {
  if (hasStep)
    return static_cast<OperatorAssignmentStatement *>(
        getTrailingObjects<Statement *>()[hasDecl]);
  return std::nullopt;
}
std::size_t ForStatement::getBodySize() const { return bodySize; }
ArrayRef<Statement *> ForStatement::getBody() const {
  return {getTrailingObjects<Statement *>() + hasDecl + hasStep, bodySize};
}

BreakStatement *BreakStatement::create(SMRange range, ASTContext *context) {
  return context->make<BreakStatement>(range);
}

ContinueStatement *ContinueStatement::create(SMRange range,
                                             ASTContext *context) {
  return context->make<ContinueStatement>(range);
}

ReturnStatement *ReturnStatement::create(SMRange range, ASTContext *context,
                                         std::optional<Expression *> expr) {
  return context->make<ReturnStatement>(range, expr);
}

DeclarationStatement *
DeclarationStatement::create(SMRange range, ASTContext *context,
                             Pattern *pattern, optional<Expression *> init) {
  return context->make<DeclarationStatement>(range, pattern, init);
}

AssignmentStatement *AssignmentStatement::create(SMRange range,
                                                 ASTContext *context,
                                                 Pattern *pattern,
                                                 Expression *init) {
  return context->make<AssignmentStatement>(range, pattern, init);
}

OperatorAssignmentStatement *
OperatorAssignmentStatement::create(SMRange range, ASTContext *context,
                                    Expression *lhs, Operator op,
                                    Expression *rhs) {
  return context->make<OperatorAssignmentStatement>(range, lhs, op, rhs);
}

FunctionDeclaration *FunctionDeclaration::create(SMRange range,
                                                 ASTContext *context,
                                                 StringRef name,
                                                 ArrayRef<Pattern *> params,
                                                 ArrayRef<Statement *> body) {
  auto allocSize =
      totalSizeToAlloc<Pattern *, Statement *>(params.size(), body.size());
  void *mem = context->alloc(allocSize);
  auto *funcDecl =
      new (mem) FunctionDeclaration(range, name, params.size(), body.size());

  std::uninitialized_copy(params.begin(), params.end(),
                          funcDecl->getTrailingObjects<Pattern *>());
  std::uninitialized_copy(body.begin(), body.end(),
                          funcDecl->getTrailingObjects<Statement *>());
  return funcDecl;
}

MatchExpression *
MatchExpression::create(SMRange range, ASTContext *context, Expression *expr,
                        ArrayRef<std::pair<Pattern *, Statement *>> cases) {
  auto allocSize = totalSizeToAlloc<MatchCase>(cases.size());
  void *mem = context->alloc(allocSize);
  auto *matchExpr = new (mem) MatchExpression(range, expr, cases.size());
  std::uninitialized_copy(cases.begin(), cases.end(),
                          matchExpr->getTrailingObjects<MatchCase>());
  return matchExpr;
}

BinaryExpression *BinaryExpression::create(SMRange range, ASTContext *context,
                                           Expression *lhs, Operator op,
                                           Expression *rhs) {
  return context->make<BinaryExpression>(range, lhs, op, rhs);
}

UnaryExpression *UnaryExpression::create(SMRange range, ASTContext *context,
                                         Operator op, Expression *expr) {
  return context->make<UnaryExpression>(range, op, expr);
}

CallExpression *CallExpression::create(SMRange range, ASTContext *context,
                                       Expression *callee,
                                       ArrayRef<Expression *> args) {
  auto allocSize = totalSizeToAlloc<Expression *>(args.size());
  void *mem = context->alloc(allocSize);
  auto *callExpr = new (mem) CallExpression(range, args.size());
  auto exprs = callExpr->getTrailingObjects<Expression *>();
  exprs[0] = callee;
  std::uninitialized_copy(args.begin(), args.end(),
                          callExpr->getTrailingObjects<Expression *>() + 1);
  return callExpr;
}

ArrayExpression *ArrayExpression::create(SMRange range, ASTContext *context,
                                         ArrayRef<Expression *> elements) {
  auto allocSize = totalSizeToAlloc<Expression *>(elements.size());
  void *mem = context->alloc(allocSize);
  auto *arrayExpr = new (mem) ArrayExpression(range, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          arrayExpr->getTrailingObjects<Expression *>());
  return arrayExpr;
}

IndexExpression *IndexExpression::create(SMRange range, ASTContext *context,
                                         Expression *lhs, Expression *index) {
  return context->make<IndexExpression>(range, lhs, index);
}

IdentifierExpression *IdentifierExpression::create(SMRange range,
                                                   ASTContext *context,
                                                   StringRef name) {
  return context->make<IdentifierExpression>(range, name);
}

TupleExpression *TupleExpression::create(SMRange range, ASTContext *context,
                                         ArrayRef<Expression *> elements) {
  auto allocSize = totalSizeToAlloc<Expression *>(elements.size());
  void *mem = context->alloc(allocSize);
  auto *tupleExpr = new (mem) TupleExpression(range, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          tupleExpr->getTrailingObjects<Expression *>());
  return tupleExpr;
}

GroupExpression *GroupExpression::create(SMRange range, ASTContext *context,
                                         Expression *expr) {
  return context->make<GroupExpression>(range, expr);
}

IntegerLiteral *IntegerLiteral::create(SMRange range, ASTContext *context,
                                       uint64_t value) {
  return context->make<IntegerLiteral>(range, value);
}

BooleanLiteral *BooleanLiteral::create(SMRange range, ASTContext *context,
                                       bool value) {
  return context->make<BooleanLiteral>(range, value);
}

FloatLiteral *FloatLiteral::create(SMRange range, ASTContext *context,
                                   StringRef value) {
  return context->make<FloatLiteral>(range, value);
}

StringLiteral *StringLiteral::create(SMRange range, ASTContext *context,
                                     StringRef value) {
  return context->make<StringLiteral>(range, value);
}

NilLiteral *NilLiteral::create(SMRange range, ASTContext *context) {
  return context->make<NilLiteral>(range);
}

IdentifierPattern *IdentifierPattern::create(SMRange range, ASTContext *context,
                                             StringRef name) {
  return context->make<IdentifierPattern>(range, name);
}

TuplePattern *TuplePattern::create(SMRange range, ASTContext *context,
                                   ArrayRef<Pattern *> elements) {
  auto allocSize = totalSizeToAlloc<Pattern *>(elements.size());
  void *mem = context->alloc(allocSize);
  auto *tuplePattern = new (mem) TuplePattern(range, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          tuplePattern->getTrailingObjects<Pattern *>());
  return tuplePattern;
}

} // namespace bara
