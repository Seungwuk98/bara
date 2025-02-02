#include "bara/ast/AST.h"
#include "bara/context/ASTContext.h"
#include <cassert>

namespace bara {

bool Statement::classof(const AST *ast) {
  return false
#define STATEMENT(NAME) || ast->isa<NAME>()
#include "bara/ast/Statement.def"
      ;
}

bool Expression::classof(const AST *ast) {
  return false
#define EXPRESSION(NAME) || ast->isa<NAME>()
#include "bara/ast/Expression.def"
      ;
}

class ASTPrintVisitor : public ConstASTVisitorBase<ASTPrintVisitor
#define AST_KIND(NAME) , NAME
#include "bara/ast/AST.def"
                                                   > {
public:
  ASTPrintVisitor(ASTPrinter &printer) : printer(printer) {}

#define AST_KIND(NAME) void visit(const NAME &ast);
#include "bara/ast/AST.def"

private:
  ASTPrinter &printer;
};

void printOperator(raw_ostream &os, Operator op) {
  switch (op) {
  case Operator::Plus:
    os << '+';
    break;
  case Operator::Minus:
    os << '-';
    break;
  case Operator::Mul:
    os << '*';
    break;
  case Operator::Div:
    os << '/';
    break;
  case Operator::Mod:
    os << '%';
    break;
  case Operator::Assign:
    os << '=';
    break;
  case Operator::Eq:
    os << "==";
    break;
  case Operator::Ne:
    os << "!=";
    break;
  case Operator::Lt:
    os << '<';
    break;
  case Operator::Le:
    os << "<=";
    break;
  case Operator::Gt:
    os << '>';
    break;
  case Operator::Ge:
    os << ">=";
    break;
  case Operator::And:
    os << "&&";
    break;
  case Operator::Or:
    os << "||";
    break;
  case Operator::Not:
    os << "!";
    break;
  case Operator::BitAnd:
    os << "&";
    break;
  case Operator::BitOr:
    os << "|";
    break;
  case Operator::BitXor:
    os << "^";
    break;
  case Operator::BitNot:
    os << "~";
    break;
  case Operator::Shl:
    os << "<<";
    break;
  case Operator::Shr:
    os << ">>";
    break;
  }
}

string operatorToString(Operator op) {
  string str;
  raw_string_ostream os(str);
  printOperator(os, op);
  return os.str();
}

class ASTEqualVisitor : public ConstASTVisitorBase<ASTEqualVisitor
#define AST_KIND(NAME) , NAME
#include "bara/ast/AST.def"
                                                   > {
public:
  ASTEqualVisitor(const AST *thisAST) : thisAST(thisAST) {}
#define AST_KIND(NAME) void visit(const NAME &other);
#include "bara/ast/AST.def"

  bool isEqual() const { return equal; }

  template <typename T>
  bool isEqualASTArray(ArrayRef<T> lhs, ArrayRef<T> rhs) {
    if (lhs.size() != rhs.size())
      return false;
    for (auto [l, r] : llvm::zip(lhs, rhs)) {
      if (!l->isEqual(r))
        return false;
    }
    return true;
  }

  template <typename T>
  bool isEqualOptionalAST(optional<T> lhs, optional<T> rhs) {
    if (lhs.has_value() != rhs.has_value())
      return false;
    if (!lhs.has_value())
      return true;
    return lhs.value()->isEqual(rhs.value());
  }

private:
  const AST *thisAST;
  bool equal = true;
};

void AST::accept(ASTVisitor &visitor) { visitor.visit(*this); }
void AST::accept(ConstASTVisitor &visitor) const { visitor.visit(*this); }

void AST::print(ASTPrinter &printer) const {
  ASTPrintVisitor printVisitor(printer);
  this->accept(printVisitor);
}

bool AST::isEqual(const AST *other) const {
  ASTEqualVisitor equalVisitor(this);
  this->accept(equalVisitor);
  return equalVisitor.isEqual();
}

//===----------------------------------------------------------------------===//
// Program
//===----------------------------------------------------------------------===//

Program *Program::create(SMRange range, ASTContext *context,
                         ArrayRef<Statement *> statements) {
  auto allocSize = totalSizeToAlloc<Statement *>(statements.size());
  void *mem = context->alloc(allocSize);
  auto *program = new (mem) Program(range, statements.size());
  std::uninitialized_copy(statements.begin(), statements.end(),
                          program->getTrailingObjects<Statement *>());
  return program;
}

void ASTPrintVisitor::visit(const Program &ast) {
  for (auto stmt : ast.getStmts()) {
    stmt->accept(*this);
    printer.ln();
  }
}

void ASTEqualVisitor::visit(const Program &other) {
  if (!thisAST->isa<Program>()) {
    equal = false;
    return;
  }

  auto *thisProgram = thisAST->cast<Program>();
  equal =
      isEqualASTArray<Statement *>(thisProgram->getStmts(), other.getStmts());
}

//===----------------------------------------------------------------------===//
// CompoundStatement
//===----------------------------------------------------------------------===//
CompoundStatement *CompoundStatement::create(SMRange range, ASTContext *context,
                                             ArrayRef<Statement *> statements) {
  auto allocSize = totalSizeToAlloc<Statement *>(statements.size());
  void *mem = context->alloc(allocSize);
  auto *stmt = new (mem) CompoundStatement(range, statements.size());
  std::uninitialized_copy(statements.begin(), statements.end(),
                          stmt->getTrailingObjects<Statement *>());
  return stmt;
}

void ASTPrintVisitor::visit(const CompoundStatement &ast) {
  printer << "{";
  {
    ASTPrinter::AddIndentScope scope(printer);
    for (auto stmt : ast.getStmts()) {
      printer.ln();
      stmt->accept(*this);
    }
  }
  printer.ln() << "}";
}

void ASTEqualVisitor::visit(const CompoundStatement &other) {
  if (!thisAST->isa<CompoundStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<CompoundStatement>();
  equal = isEqualASTArray<Statement *>(thisStmt->getStmts(), other.getStmts());
}

//===----------------------------------------------------------------------===//
// ExpressionStatement
//===----------------------------------------------------------------------===//

ExpressionStatement *ExpressionStatement::create(SMRange range,
                                                 ASTContext *context,
                                                 Expression *expr) {
  void *mem = context->alloc(sizeof(ExpressionStatement));
  return new (mem) ExpressionStatement(range, expr);
}

void ASTPrintVisitor::visit(const ExpressionStatement &ast) {
  ast.getExpr()->accept(*this);
  printer << ';';
}

void ASTEqualVisitor::visit(const ExpressionStatement &other) {
  if (!thisAST->isa<ExpressionStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<ExpressionStatement>();
  auto *otherExpr = other.getExpr();
  if (!thisStmt->getExpr()->isEqual(otherExpr)) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
// IfStatement
//===----------------------------------------------------------------------===//

IfStatement *IfStatement::create(SMRange range, ASTContext *context,
                                 Expression *cond, CompoundStatement *thenStmt,
                                 CompoundStatement *elseStmt) {
  return context->make<IfStatement>(range, cond, thenStmt, elseStmt);
}

IfStatement *IfStatement::create(SMRange range, ASTContext *context,
                                 Expression *cond, CompoundStatement *thenStmt,
                                 IfStatement *elseStmt) {
  return context->make<IfStatement>(range, cond, thenStmt, elseStmt);
}

IfStatement *IfStatement::create(SMRange range, ASTContext *context,
                                 Expression *cond,
                                 CompoundStatement *thenStmt) {
  return context->make<IfStatement>(range, cond, thenStmt, nullopt);
}

void ASTPrintVisitor::visit(const IfStatement &ast) {
  printer << "if ";
  ast.getCond()->accept(*this);
  ast.getThenStmt()->accept(*this);

  if (ast.hasElse()) {
    printer << " else ";
    ast.getElseStmt()->accept(*this);
  }
}

void ASTEqualVisitor::visit(const IfStatement &other) {
  if (!thisAST->isa<IfStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<IfStatement>();
  if (!thisStmt->getCond()->isEqual(other.getCond())) {
    equal = false;
    return;
  }

  if (!thisStmt->getThenStmt()->isEqual(other.getThenStmt())) {
    equal = false;
    return;
  }

  if (thisStmt->hasElse() != other.hasElse()) {
    equal = false;
    return;
  }

  equal = !thisStmt->hasElse() ||
          thisStmt->getElseStmt()->isEqual(other.getElseStmt());
}

//===----------------------------------------------------------------------===//
// WhileStatement
//===----------------------------------------------------------------------===//

WhileStatement *WhileStatement::create(SMRange range, ASTContext *context,
                                       Expression *cond,
                                       CompoundStatement *body,
                                       bool isDoWhile) {
  return context->make<WhileStatement>(range, cond, body, isDoWhile);
}

void ASTPrintVisitor::visit(const WhileStatement &ast) {
  if (ast.isDoWhile()) {
    printer << "do ";
    ast.getBody()->accept(*this);
    printer << " while ";
    ast.getCond()->accept(*this);
    printer << ";";
  } else {
    printer << "while ";
    ast.getCond()->accept(*this);
    ast.getBody()->accept(*this);
  }
}

void ASTEqualVisitor::visit(const WhileStatement &other) {
  if (!thisAST->isa<WhileStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<WhileStatement>();
  if (thisStmt->isDoWhile() != other.isDoWhile()) {
    equal = false;
    return;
  }

  if (!thisStmt->getCond()->isEqual(other.getCond())) {
    equal = false;
    return;
  }

  auto bodyEq =
      isEqualASTArray<Statement *>(thisStmt->getBody(), other.getBody());
  if (!bodyEq) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
// ForStatement
//===----------------------------------------------------------------------===//

ForStatement *ForStatement::create(SMRange range, ASTContext *context,
                                   std::optional<Statement *> decl,
                                   std::optional<Expression *> cond,
                                   std::optional<Statement *> step,
                                   CompoundStatement *body) {
  auto hasDecl = decl.has_value();
  auto hasStep = step.has_value();
  auto totalCnt = hasDecl + hasStep + 1;
  auto allocSize = totalSizeToAlloc<Statement *>(totalCnt);
  void *mem = context->alloc(allocSize);
  auto *forStmt = new (mem) ForStatement(range, hasDecl, cond, hasStep);
  std::size_t idx = 0;
  auto *stmts = forStmt->getTrailingObjects<Statement *>();
  if (hasDecl)
    stmts[idx++] = static_cast<Statement *>(*decl);
  if (hasStep)
    stmts[idx++] = static_cast<Statement *>(*step);
  stmts[idx] = body;
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
CompoundStatement *ForStatement::getBody() const {
  return (*(getTrailingObjects<Statement *>() + hasDecl + hasStep))
      ->cast<CompoundStatement>();
}

void ASTPrintVisitor::visit(const ForStatement &ast) {
  printer << "for (";
  if (ast.getDecl()) {
    auto *decl = ast.getDecl().value();
    printer << "var ";
    decl->getPattern()->accept(*this);
    if (decl->getInit()) {
      printer << " = ";
      decl->getInit().value()->accept(*this);
    }
  }
  printer << "; ";

  if (ast.getCond()) {
    ast.getCond().value()->accept(*this);
  }
  printer << "; ";

  if (ast.getStep()) {
    auto *step = *ast.getStep();
    assert((step->isa<OperatorAssignmentStatement, AssignmentStatement,
                      CompoundStatement>()));
    if (auto *opAssign = step->dyn_cast<OperatorAssignmentStatement>()) {
      opAssign->getLhs()->accept(*this);
      printer << " ";
      printOperator(printer.getOS(), opAssign->getOperator());
      printer << "= ";
      opAssign->getRhs()->accept(*this);
    } else if (auto *assign = step->dyn_cast<AssignmentStatement>()) {
      assign->getLhs()->accept(*this);
      printer << " = ";
      assign->getRhs()->accept(*this);
    } else {
      step->accept(*this);
    }
  }

  printer << ") ";
  ast.getBody()->accept(*this);
}

void ASTEqualVisitor::visit(const ForStatement &other) {
  if (!thisAST->isa<ForStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<ForStatement>();
  if (!isEqualOptionalAST(thisStmt->getDecl(), other.getDecl())) {
    equal = false;
    return;
  }

  if (!isEqualOptionalAST(thisStmt->getCond(), other.getCond())) {
    equal = false;
    return;
  }

  if (!isEqualOptionalAST(thisStmt->getStep(), other.getStep())) {
    equal = false;
    return;
  }

  equal = isEqualASTArray<Statement *>(thisStmt->getBody(), other.getBody());
}

//===----------------------------------------------------------------------===//
/// BreakStatement
//===----------------------------------------------------------------------===//

BreakStatement *BreakStatement::create(SMRange range, ASTContext *context) {
  return context->make<BreakStatement>(range);
}

void ASTPrintVisitor::visit(const BreakStatement &ast) { printer << "break;"; }

void ASTEqualVisitor::visit(const BreakStatement &other) {
  equal = thisAST->isa<BreakStatement>();
}

//===----------------------------------------------------------------------===//
/// ContinueStatement
//===----------------------------------------------------------------------===//

ContinueStatement *ContinueStatement::create(SMRange range,
                                             ASTContext *context) {
  return context->make<ContinueStatement>(range);
}

void ASTPrintVisitor::visit(const ContinueStatement &ast) {
  printer << "continue;";
}

void ASTEqualVisitor::visit(const ContinueStatement &other) {
  equal = thisAST->isa<ContinueStatement>();
}

//===----------------------------------------------------------------------===//
/// ReturnStatement
//===----------------------------------------------------------------------===//

ReturnStatement *ReturnStatement::create(SMRange range, ASTContext *context,
                                         std::optional<Expression *> expr) {
  return context->make<ReturnStatement>(range, expr);
}

void ASTPrintVisitor::visit(const ReturnStatement &ast) {
  printer << "return";
  if (ast.getExpr()) {
    printer << " ";
    ast.getExpr().value()->accept(*this);
  }
  printer << ";";
}

void ASTEqualVisitor::visit(const ReturnStatement &other) {
  if (!thisAST->isa<ReturnStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<ReturnStatement>();
  equal = isEqualOptionalAST(thisStmt->getExpr(), other.getExpr());
}

//===----------------------------------------------------------------------===//
/// DeclarationStatement
//===----------------------------------------------------------------------===//

DeclarationStatement *
DeclarationStatement::create(SMRange range, ASTContext *context,
                             Pattern *pattern, optional<Expression *> init) {
  return context->make<DeclarationStatement>(range, pattern, init);
}

void ASTPrintVisitor::visit(const DeclarationStatement &ast) {
  printer << "var ";
  ast.getPattern()->accept(*this);
  if (ast.getInit()) {
    printer << " = ";
    ast.getInit().value()->accept(*this);
  }
  printer << ";";
}

void ASTEqualVisitor::visit(const DeclarationStatement &other) {
  if (!thisAST->isa<DeclarationStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<DeclarationStatement>();
  if (!thisStmt->getPattern()->isEqual(other.getPattern())) {
    equal = false;
    return;
  }

  equal = isEqualOptionalAST(thisStmt->getInit(), other.getInit());
}

//===----------------------------------------------------------------------===//
/// AssignmentStatement
//===----------------------------------------------------------------------===//

AssignmentStatement *AssignmentStatement::create(SMRange range,
                                                 ASTContext *context,
                                                 Expression *lhs,
                                                 Expression *rhs) {
  return context->make<AssignmentStatement>(range, lhs, rhs);
}

void ASTPrintVisitor::visit(const AssignmentStatement &ast) {
  ast.getLhs()->accept(*this);
  printer << " = ";
  ast.getRhs()->accept(*this);
  printer << ";";
}

void ASTEqualVisitor::visit(const AssignmentStatement &other) {
  if (!thisAST->isa<AssignmentStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<AssignmentStatement>();
  if (!thisStmt->getLhs()->isEqual(other.getLhs())) {
    equal = false;
    return;
  }

  if (!thisStmt->getRhs()->isEqual(other.getRhs())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// OperatorAssignmentStatement
//===----------------------------------------------------------------------===//

OperatorAssignmentStatement *
OperatorAssignmentStatement::create(SMRange range, ASTContext *context,
                                    Expression *lhs, Operator op,
                                    Expression *rhs) {
  return context->make<OperatorAssignmentStatement>(range, lhs, op, rhs);
}

void ASTPrintVisitor::visit(const OperatorAssignmentStatement &ast) {
  ast.getLhs()->accept(*this);
  printer << " ";
  printOperator(printer.getOS(), ast.getOperator());
  printer << "= ";
  ast.getRhs()->accept(*this);
  printer << ";";
}

void ASTEqualVisitor::visit(const OperatorAssignmentStatement &other) {
  if (!thisAST->isa<OperatorAssignmentStatement>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<OperatorAssignmentStatement>();
  if (!thisStmt->getLhs()->isEqual(other.getLhs())) {
    equal = false;
    return;
  }

  if (thisStmt->getOperator() != other.getOperator()) {
    equal = false;
    return;
  }

  if (!thisStmt->getRhs()->isEqual(other.getRhs())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// FunctionDeclaration
//===----------------------------------------------------------------------===//

FunctionDeclaration *FunctionDeclaration::create(SMRange range,
                                                 ASTContext *context,
                                                 StringRef name,
                                                 ArrayRef<StringRef> params,
                                                 ArrayRef<Statement *> body) {
  auto allocSize =
      totalSizeToAlloc<StringRef, Statement *>(params.size(), body.size());
  void *mem = context->alloc(allocSize);
  auto *funcDecl =
      new (mem) FunctionDeclaration(range, name, params.size(), body.size());

  std::uninitialized_copy(params.begin(), params.end(),
                          funcDecl->getTrailingObjects<StringRef>());
  std::uninitialized_copy(body.begin(), body.end(),
                          funcDecl->getTrailingObjects<Statement *>());
  return funcDecl;
}

void ASTPrintVisitor::visit(const FunctionDeclaration &ast) {
  printer << "fn " << ast.getName() << "(";
  for (auto [idx, param] : llvm::enumerate(ast.getParams())) {
    printer << param;
    if (idx != ast.getParams().size() - 1)
      printer << ", ";
  }
  printer << ") {";
  {
    ASTPrinter::AddIndentScope scope(printer);
    for (auto stmt : ast.getBody()) {
      printer.ln();
      stmt->accept(*this);
    }
  }
  printer.ln() << "}";
}

void ASTEqualVisitor::visit(const FunctionDeclaration &other) {
  if (!thisAST->isa<FunctionDeclaration>()) {
    equal = false;
    return;
  }

  auto *thisDecl = thisAST->cast<FunctionDeclaration>();
  if (thisDecl->getName() != other.getName()) {
    equal = false;
    return;
  }

  if (thisDecl->getParams().size() != other.getParams().size()) {
    equal = false;
    return;
  }

  for (auto [l, r] : llvm::zip(thisDecl->getParams(), other.getParams())) {
    if (l != r) {
      equal = false;
      return;
    }
  }

  equal = isEqualASTArray<Statement *>(thisDecl->getBody(), other.getBody());
}

//===----------------------------------------------------------------------===//
/// MatchExpression
//===----------------------------------------------------------------------===//

MatchExpression *MatchExpression::create(SMRange range, ASTContext *context,
                                         Expression *expr,
                                         ArrayRef<MatchCase> cases) {
  auto allocSize = totalSizeToAlloc<MatchCase>(cases.size());
  void *mem = context->alloc(allocSize);
  auto *matchExpr = new (mem) MatchExpression(range, expr, cases.size());
  std::uninitialized_copy(cases.begin(), cases.end(),
                          matchExpr->getTrailingObjects<MatchCase>());
  return matchExpr;
}

void ASTPrintVisitor::visit(const MatchExpression &ast) {
  printer << "match ";
  ast.getExpr()->accept(*this);
  printer << " {";
  {
    ASTPrinter::AddIndentScope scope(printer);
    for (auto matchCase : ast.getMatchCases()) {
      printer.ln() << '\\';
      matchCase.first->accept(*this);
      printer << " => ";
      matchCase.second->accept(*this);
      printer << ';';
    }
  }
  printer.ln() << "}";
}

void ASTEqualVisitor::visit(const MatchExpression &other) {
  if (!thisAST->isa<MatchExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<MatchExpression>();
  if (!thisExpr->getExpr()->isEqual(other.getExpr())) {
    equal = false;
    return;
  }

  if (thisAST->cast<MatchExpression>()->getMatchCases().size() !=
      other.getMatchCases().size()) {
    equal = false;
    return;
  }

  for (auto [l, r] :
       llvm::zip(thisExpr->getMatchCases(), other.getMatchCases())) {
    if (!l.first->isEqual(r.first) || !l.second->isEqual(r.second)) {
      equal = false;
      return;
    }
  }
}

//===----------------------------------------------------------------------===//
/// LambdaExpression
//===----------------------------------------------------------------------===//

LambdaExpression *LambdaExpression::create(SMRange range, ASTContext *context,
                                           ArrayRef<StringRef> params,
                                           Expression *body) {
  auto allocSize = totalSizeToAlloc<StringRef>(params.size());
  void *mem = context->alloc(allocSize);
  auto *lambdaExpr = new (mem) LambdaExpression(range, params.size(), body);
  std::uninitialized_copy(params.begin(), params.end(),
                          lambdaExpr->getTrailingObjects<StringRef>());
  return lambdaExpr;
}

LambdaExpression *LambdaExpression::create(SMRange range, ASTContext *context,
                                           ArrayRef<StringRef> params,
                                           CompoundStatement *stmt) {
  auto allocSize = totalSizeToAlloc<StringRef>(params.size());
  void *mem = context->alloc(allocSize);
  auto *lambdaExpr = new (mem) LambdaExpression(range, params.size(), stmt);
  std::uninitialized_copy(params.begin(), params.end(),
                          lambdaExpr->getTrailingObjects<StringRef>());
  return lambdaExpr;
}

Expression *LambdaExpression::getExprBody() const {
  assert(isExprBody());
  return body->cast<Expression>();
}

CompoundStatement *LambdaExpression::getStmtBody() const {
  assert(!isExprBody());
  return body->cast<CompoundStatement>();
}

void ASTPrintVisitor::visit(const LambdaExpression &ast) {
  printer << "\\";
  for (auto [idx, param] : llvm::enumerate(ast.getParams())) {
    printer << param;
    if (idx != ast.getParams().size() - 1)
      printer << ", ";
  }
  printer << " => ";
  if (ast.isExprBody())
    ast.getExprBody()->accept(*this);
  else
    ast.getStmtBody()->accept(*this);
}

void ASTEqualVisitor::visit(const LambdaExpression &other) {
  if (!thisAST->isa<LambdaExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<LambdaExpression>();
  if (thisExpr->getParams().size() != other.getParams().size()) {
    equal = false;
    return;
  }

  for (auto [l, r] : llvm::zip(thisExpr->getParams(), other.getParams())) {
    if (l != r) {
      equal = false;
      return;
    }
  }

  if (thisExpr->isExprBody() != other.isExprBody()) {
    equal = false;
    return;
  }

  equal = thisExpr->isExprBody()
              ? thisExpr->getExprBody()->isEqual(other.getExprBody())
              : thisExpr->getStmtBody()->isEqual(other.getStmtBody());
}

//===----------------------------------------------------------------------===//
/// BinaryExpression
//===----------------------------------------------------------------------===//

BinaryExpression *BinaryExpression::create(SMRange range, ASTContext *context,
                                           Expression *lhs, Operator op,
                                           Expression *rhs) {
  return context->make<BinaryExpression>(range, lhs, op, rhs);
}

void ASTPrintVisitor::visit(const BinaryExpression &ast) {
  ast.getLhs()->accept(*this);
  printer << " ";
  printOperator(printer.getOS(), ast.getOperator());
  printer << " ";
  ast.getRhs()->accept(*this);
}

void ASTEqualVisitor::visit(const BinaryExpression &other) {
  if (!thisAST->isa<BinaryExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<BinaryExpression>();
  if (!thisExpr->getLhs()->isEqual(other.getLhs())) {
    equal = false;
    return;
  }

  if (thisExpr->getOperator() != other.getOperator()) {
    equal = false;
    return;
  }

  if (!thisExpr->getRhs()->isEqual(other.getRhs())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// BinaryExpression
//===----------------------------------------------------------------------===//

UnaryExpression *UnaryExpression::create(SMRange range, ASTContext *context,
                                         Operator op, Expression *expr) {
  return context->make<UnaryExpression>(range, op, expr);
}

void ASTPrintVisitor::visit(const UnaryExpression &ast) {
  printOperator(printer.getOS(), ast.getOperator());
  ast.getExpr()->accept(*this);
}

void ASTEqualVisitor::visit(const UnaryExpression &other) {
  if (!thisAST->isa<UnaryExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<UnaryExpression>();
  if (thisExpr->getOperator() != other.getOperator()) {
    equal = false;
    return;
  }

  if (!thisExpr->getExpr()->isEqual(other.getExpr())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// ConditionalExpression
//===----------------------------------------------------------------------===//

ConditionalExpression *ConditionalExpression::create(SMRange range,
                                                     ASTContext *context,
                                                     Expression *cond,
                                                     Expression *lhs,
                                                     Expression *rhs) {
  return context->make<ConditionalExpression>(range, cond, lhs, rhs);
}

void ASTPrintVisitor::visit(const ConditionalExpression &ast) {
  ast.getCond()->accept(*this);
  printer << " ? ";
  ast.getThenExpr()->accept(*this);
  printer << " : ";
  ast.getElseExpr()->accept(*this);
}

void ASTEqualVisitor::visit(const ConditionalExpression &other) {
  if (!thisAST->isa<ConditionalExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<ConditionalExpression>();
  if (!thisExpr->getCond()->isEqual(other.getCond())) {
    equal = false;
    return;
  }

  if (!thisExpr->getThenExpr()->isEqual(other.getThenExpr())) {
    equal = false;
    return;
  }

  if (!thisExpr->getElseExpr()->isEqual(other.getElseExpr())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// CallExpression
//===----------------------------------------------------------------------===//

CallExpression *CallExpression::create(SMRange range, ASTContext *context,
                                       Expression *callee,
                                       ArrayRef<Expression *> args) {
  auto allocSize = totalSizeToAlloc<Expression *>(args.size() + 1);
  void *mem = context->alloc(allocSize);
  auto *callExpr = new (mem) CallExpression(range, args.size());
  auto exprs = callExpr->getTrailingObjects<Expression *>();
  exprs[0] = callee;
  std::uninitialized_copy(args.begin(), args.end(),
                          callExpr->getTrailingObjects<Expression *>() + 1);
  return callExpr;
}

void ASTPrintVisitor::visit(const CallExpression &ast) {
  ast.getCallee()->accept(*this);
  printer << "(";
  for (auto [idx, arg] : llvm::enumerate(ast.getArgs())) {
    arg->accept(*this);
    if (idx != ast.getArgs().size() - 1)
      printer << ", ";
  }
  printer << ")";
}

void ASTEqualVisitor::visit(const CallExpression &other) {
  if (!thisAST->isa<CallExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<CallExpression>();
  if (!thisExpr->getCallee()->isEqual(other.getCallee())) {
    equal = false;
    return;
  }

  equal = isEqualASTArray<Expression *>(thisExpr->getArgs(), other.getArgs());
}

//===----------------------------------------------------------------------===//
/// ArrayExpression
//===----------------------------------------------------------------------===//

ArrayExpression *ArrayExpression::create(SMRange range, ASTContext *context,
                                         ArrayRef<Expression *> elements) {
  auto allocSize = totalSizeToAlloc<Expression *>(elements.size());
  void *mem = context->alloc(allocSize);
  auto *arrayExpr = new (mem) ArrayExpression(range, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          arrayExpr->getTrailingObjects<Expression *>());
  return arrayExpr;
}

void ASTPrintVisitor::visit(const ArrayExpression &ast) {
  printer << "[";
  for (auto [idx, elem] : llvm::enumerate(ast.getArgs())) {
    elem->accept(*this);
    if (idx != ast.getArgs().size() - 1)
      printer << ", ";
  }
  printer << "]";
}

void ASTEqualVisitor::visit(const ArrayExpression &other) {
  if (!thisAST->isa<ArrayExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<ArrayExpression>();
  equal = isEqualASTArray<Expression *>(thisExpr->getArgs(), other.getArgs());
}

//===----------------------------------------------------------------------===//
/// IndexExpression
//===----------------------------------------------------------------------===//

IndexExpression *IndexExpression::create(SMRange range, ASTContext *context,
                                         Expression *lhs, Expression *index) {
  return context->make<IndexExpression>(range, lhs, index);
}

void ASTPrintVisitor::visit(const IndexExpression &ast) {
  ast.getLhs()->accept(*this);
  printer << "[";
  ast.getRhs()->accept(*this);
  printer << "]";
}

void ASTEqualVisitor::visit(const IndexExpression &other) {
  if (!thisAST->isa<IndexExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<IndexExpression>();
  if (!thisExpr->getLhs()->isEqual(other.getLhs())) {
    equal = false;
    return;
  }

  if (!thisExpr->getRhs()->isEqual(other.getRhs())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// IdentifierExpression
//===----------------------------------------------------------------------===//

IdentifierExpression *IdentifierExpression::create(SMRange range,
                                                   ASTContext *context,
                                                   StringRef name) {
  return context->make<IdentifierExpression>(range, name);
}

void ASTPrintVisitor::visit(const IdentifierExpression &ast) {
  printer << ast.getName();
}

void ASTEqualVisitor::visit(const IdentifierExpression &other) {
  if (!thisAST->isa<IdentifierExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<IdentifierExpression>();
  equal = thisExpr->getName() == other.getName();
}

//===----------------------------------------------------------------------===//
/// TupleExpression
//===----------------------------------------------------------------------===//

TupleExpression *TupleExpression::create(SMRange range, ASTContext *context,
                                         ArrayRef<Expression *> elements) {
  auto allocSize = totalSizeToAlloc<Expression *>(elements.size());
  void *mem = context->alloc(allocSize);
  auto *tupleExpr = new (mem) TupleExpression(range, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          tupleExpr->getTrailingObjects<Expression *>());
  return tupleExpr;
}

void ASTPrintVisitor::visit(const TupleExpression &ast) {
  printer << "(";
  for (auto [idx, elem] : llvm::enumerate(ast.getExprs())) {
    elem->accept(*this);
    if (idx != ast.getExprs().size() - 1)
      printer << ", ";
  }
  if (ast.getExprs().size() == 1)
    printer << ",";
  printer << ")";
}

void ASTEqualVisitor::visit(const TupleExpression &other) {
  if (!thisAST->isa<TupleExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<TupleExpression>();
  equal = isEqualASTArray<Expression *>(thisExpr->getExprs(), other.getExprs());
}

//===----------------------------------------------------------------------===//
/// GroupExpression
//===----------------------------------------------------------------------===//

GroupExpression *GroupExpression::create(SMRange range, ASTContext *context,
                                         Expression *expr) {
  return context->make<GroupExpression>(range, expr);
}

void ASTPrintVisitor::visit(const GroupExpression &ast) {
  printer << "(";
  ast.getExpr()->accept(*this);
  printer << ")";
}

void ASTEqualVisitor::visit(const GroupExpression &other) {
  if (!thisAST->isa<GroupExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<GroupExpression>();
  equal = thisExpr->getExpr()->isEqual(other.getExpr());
}

//===----------------------------------------------------------------------===//
/// IntegerLiteral
//===----------------------------------------------------------------------===//

IntegerLiteral *IntegerLiteral::create(SMRange range, ASTContext *context,
                                       uint64_t value) {
  return context->make<IntegerLiteral>(range, value);
}

void ASTPrintVisitor::visit(const IntegerLiteral &ast) {
  printer << ast.getValue();
}

void ASTEqualVisitor::visit(const IntegerLiteral &other) {
  if (!thisAST->isa<IntegerLiteral>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<IntegerLiteral>();
  equal = thisExpr->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// BooleanLiteral
//===----------------------------------------------------------------------===//

BooleanLiteral *BooleanLiteral::create(SMRange range, ASTContext *context,
                                       bool value) {
  return context->make<BooleanLiteral>(range, value);
}

void ASTPrintVisitor::visit(const BooleanLiteral &ast) {
  printer << (ast.getValue() ? "true" : "false");
}

void ASTEqualVisitor::visit(const BooleanLiteral &other) {
  if (!thisAST->isa<BooleanLiteral>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<BooleanLiteral>();
  equal = thisExpr->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// FloatLiteral
//===----------------------------------------------------------------------===//

FloatLiteral *FloatLiteral::create(SMRange range, ASTContext *context,
                                   StringRef value) {
  return context->make<FloatLiteral>(range, value);
}

void ASTPrintVisitor::visit(const FloatLiteral &ast) {
  printer << ast.getValue();
}

void ASTEqualVisitor::visit(const FloatLiteral &other) {
  if (!thisAST->isa<FloatLiteral>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<FloatLiteral>();
  equal = thisExpr->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// StringLiteral
//===----------------------------------------------------------------------===//

StringLiteral *StringLiteral::create(SMRange range, ASTContext *context,
                                     StringRef value) {
  return context->make<StringLiteral>(range, value);
}

void ASTPrintVisitor::visit(const StringLiteral &ast) {
  printer << ast.getValue();
}

void ASTEqualVisitor::visit(const StringLiteral &other) {
  if (!thisAST->isa<StringLiteral>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<StringLiteral>();
  equal = thisExpr->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// NilLiteral
//===----------------------------------------------------------------------===//

NilLiteral *NilLiteral::create(SMRange range, ASTContext *context) {
  return context->make<NilLiteral>(range);
}

void ASTPrintVisitor::visit(const NilLiteral &ast) { printer << "nil"; }

void ASTEqualVisitor::visit(const NilLiteral &other) {
  equal = thisAST->isa<NilLiteral>();
}

//===----------------------------------------------------------------------===//
/// IdentifierPattern
//===----------------------------------------------------------------------===//

IdentifierPattern *IdentifierPattern::create(SMRange range, ASTContext *context,
                                             StringRef name) {
  return context->make<IdentifierPattern>(range, name);
}

void ASTPrintVisitor::visit(const IdentifierPattern &ast) {
  printer << ast.getName();
}

void ASTEqualVisitor::visit(const IdentifierPattern &other) {
  if (!thisAST->isa<IdentifierPattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<IdentifierPattern>();
  equal = thisPattern->getName() == other.getName();
}

//===----------------------------------------------------------------------===//
/// TuplePattern
//===----------------------------------------------------------------------===//

TuplePattern *TuplePattern::create(SMRange range, ASTContext *context,
                                   ArrayRef<Pattern *> elements) {
  auto allocSize = totalSizeToAlloc<Pattern *>(elements.size());
  void *mem = context->alloc(allocSize);
  auto *tuplePattern = new (mem) TuplePattern(range, elements.size());
  std::uninitialized_copy(elements.begin(), elements.end(),
                          tuplePattern->getTrailingObjects<Pattern *>());
  return tuplePattern;
}

void ASTPrintVisitor::visit(const TuplePattern &ast) {
  printer << "(";
  for (auto [idx, elem] : llvm::enumerate(ast.getPatterns())) {
    elem->accept(*this);
    if (idx != ast.getPatterns().size() - 1)
      printer << ", ";
  }
  if (ast.getPatterns().size() == 1)
    printer << ",";
  printer << ")";
}

void ASTEqualVisitor::visit(const TuplePattern &other) {
  if (!thisAST->isa<TuplePattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<TuplePattern>();
  equal = isEqualASTArray<Pattern *>(thisPattern->getPatterns(),
                                     other.getPatterns());
}

//===----------------------------------------------------------------------===//
/// TuplePattern
//===----------------------------------------------------------------------===//

GroupPattern *GroupPattern::create(SMRange range, ASTContext *context,
                                   Pattern *pattern) {
  return context->make<GroupPattern>(range, pattern);
}

void ASTPrintVisitor::visit(const GroupPattern &ast) {
  printer << "(";
  ast.getPattern()->accept(*this);
  printer << ")";
}

void ASTEqualVisitor::visit(const GroupPattern &other) {
  if (!thisAST->isa<GroupPattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<GroupPattern>();
  equal = thisPattern->getPattern()->isEqual(other.getPattern());
}

//===----------------------------------------------------------------------===//
/// IntegerPattern
//===----------------------------------------------------------------------===//

IntegerPattern *IntegerPattern::create(SMRange range, ASTContext *context,
                                       uint64_t value) {
  return context->make<IntegerPattern>(range, value);
}

void ASTPrintVisitor::visit(const IntegerPattern &ast) {
  printer << ast.getValue();
}

void ASTEqualVisitor::visit(const IntegerPattern &other) {
  if (!thisAST->isa<IntegerPattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<IntegerPattern>();
  equal = thisPattern->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// FloatPattern
//===----------------------------------------------------------------------===//

FloatPattern *FloatPattern::create(SMRange range, ASTContext *context,
                                   StringRef value) {
  return context->make<FloatPattern>(range, value);
}

void ASTPrintVisitor::visit(const FloatPattern &ast) {
  printer << ast.getValue();
}

void ASTEqualVisitor::visit(const FloatPattern &other) {
  if (!thisAST->isa<FloatPattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<FloatPattern>();
  equal = thisPattern->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// StringPattern
//===----------------------------------------------------------------------===//

StringPattern *StringPattern::create(SMRange range, ASTContext *context,
                                     StringRef value) {
  return context->make<StringPattern>(range, value);
}

void ASTPrintVisitor::visit(const StringPattern &ast) {
  printer << ast.getValue();
}

void ASTEqualVisitor::visit(const StringPattern &other) {
  if (!thisAST->isa<StringPattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<StringPattern>();
  equal = thisPattern->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// BooleanPattern
//===----------------------------------------------------------------------===//

BooleanPattern *BooleanPattern::create(SMRange range, ASTContext *context,
                                       bool value) {
  return context->make<BooleanPattern>(range, value);
}

void ASTPrintVisitor::visit(const BooleanPattern &ast) {
  printer << (ast.getValue() ? "true" : "false");
}

void ASTEqualVisitor::visit(const BooleanPattern &other) {
  if (!thisAST->isa<BooleanPattern>()) {
    equal = false;
    return;
  }

  auto *thisPattern = thisAST->cast<BooleanPattern>();
  equal = thisPattern->getValue() == other.getValue();
}

//===----------------------------------------------------------------------===//
/// EmptyPattern
//===----------------------------------------------------------------------===//

EmptyPattern *EmptyPattern::create(SMRange range, ASTContext *context) {
  return context->make<EmptyPattern>(range);
}

void ASTPrintVisitor::visit(const EmptyPattern &ast) { printer << "_"; }

void ASTEqualVisitor::visit(const EmptyPattern &other) {
  equal = thisAST->isa<EmptyPattern>();
}

//===----------------------------------------------------------------------===//
/// NilPattern
//===----------------------------------------------------------------------===//

NilPattern *NilPattern::create(SMRange range, ASTContext *context) {
  return context->make<NilPattern>(range);
}

void ASTPrintVisitor::visit(const NilPattern &ast) { printer << "nil"; }

void ASTEqualVisitor::visit(const NilPattern &other) {
  equal = thisAST->isa<NilPattern>();
}

} // namespace bara
