#include "bara/ast/AST.h"
#include "bara/context/ASTContext.h"

namespace bara {

Program *Program::create(ASTContext *context,
                         ArrayRef<Statement *> statements) {
  auto allocSize = totalSizeToAlloc<Statement *>(statements.size());
  void *mem = context->alloc(allocSize);
  auto *program = new (mem) Program(statements.size());
  std::uninitialized_copy(statements.begin(), statements.end(),
                          program->getTrailingObjects<Statement *>());
  return program;
}

CompoundStatement *CompoundStatement::create(ASTContext *context,
                                             ArrayRef<Statement *> statements) {
  auto allocSize = totalSizeToAlloc<Statement *>(statements.size());
  void *mem = context->alloc(allocSize);
  auto *stmt = new (mem) CompoundStatement(statements.size());
  std::uninitialized_copy(statements.begin(), statements.end(),
                          stmt->getTrailingObjects<Statement *>());
  return stmt;
}

ExpressionStatement *ExpressionStatement::create(ASTContext *context,
                                                 Expression *expr) {
  void *mem = context->alloc(sizeof(ExpressionStatement));
  return new (mem) ExpressionStatement(expr);
}

IfStatement *
IfStatement::create(ASTContext *context, Expression *cond,
                    ArrayRef<Statement *> thenStmt,
                    std::optional<ArrayRef<Statement *>> elseStmt) {
  auto totalCnt = thenStmt.size() + (elseStmt ? elseStmt->size() : 0);
  auto allocSize = totalSizeToAlloc<Statement *>(totalCnt);
  void *mem = context->alloc(allocSize);

  auto ifStmt = new (mem)
      IfStatement(thenStmt.size(),
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

WhileStatement *WhileStatement::create(ASTContext *context, Expression *cond,
                                       ArrayRef<Statement *> body,
                                       bool isDoWhile) {
  auto allocSize = totalSizeToAlloc<Statement *>(body.size());
  void *mem = context->alloc(allocSize);
  auto whileStmt = new (mem) WhileStatement(cond, body.size(), isDoWhile);
  std::uninitialized_copy(body.begin(), body.end(),
                          whileStmt->getTrailingObjects<Statement *>());
  return whileStmt;
}

ForStatement *
ForStatement::create(ASTContext *context,
                     std::optional<DeclarationStatement *> decl,
                     std::optional<Expression *> cond,
                     std::optional<OperatorAssignmentStatement *> step,
                     ArrayRef<Statement *> body) {
  auto totalCnt = bool(decl) + bool(step) + body.size();
  auto allocSize = totalSizeToAlloc<Statement *>(totalCnt);
  void *mem = context->alloc(allocSize);
  auto *forStmt = new (mem) ForStatement(cond, body.size());
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
std::optional<OperatorAssignmentStatement *> ForStatement::getStep() const {
  if (hasStep)
    return static_cast<OperatorAssignmentStatement *>(
        getTrailingObjects<Statement *>()[hasDecl]);
  return std::nullopt;
}
std::size_t ForStatement::getBodySize() const { return bodySize; }
ArrayRef<Statement *> ForStatement::getBody() const {
  return {getTrailingObjects<Statement *>() + hasDecl + hasStep, bodySize};
}

BreakStatement *BreakStatement::create(ASTContext *context) {
  return context->getBreakStmt();
}

ContinueStatement *ContinueStatement::create(ASTContext *context) {
  return context->getContinueStmt();
}

ReturnStatement *ReturnStatement::create(ASTContext *context,
                                         std::optional<Expression *> expr) {
  return context->make<ReturnStatement>(expr);
}
} // namespace bara
