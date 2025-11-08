#include "bara/ast/AST.h"
#include "bara/ast/ASTPrinter.h"
#include "bara/context/ASTContext.h"
#include <cassert>
#include <memory>

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
// StructDeclaration
//===----------------------------------------------------------------------===//

StructDeclaration *StructDeclaration::create(SMRange range, ASTContext *context,
                                             StringRef name,
                                             ArrayRef<StringRef> fields) {
  auto allocSize = totalSizeToAlloc<StringRef>(fields.size());
  void *mem = context->alloc(allocSize);
  auto *structDecl = new (mem) StructDeclaration(range, name, fields.size());
  std::uninitialized_copy(fields.begin(), fields.end(),
                          structDecl->getTrailingObjects<StringRef>());
  return structDecl;
}

void ASTPrintVisitor::visit(const StructDeclaration &ast) {
  printer << "struct " << ast.getName() << "(";
  for (auto [idx, field] : llvm::enumerate(ast.getFieldNames())) {
    printer << field;
    if (idx != ast.getFieldNames().size() - 1)
      printer << ", ";
  }
  printer << ");";
}

void ASTEqualVisitor::visit(const StructDeclaration &other) {
  if (!thisAST->isa<StructDeclaration>()) {
    equal = false;
    return;
  }

  auto *thisDecl = thisAST->cast<StructDeclaration>();
  if (thisDecl->getName() != other.getName()) {
    equal = false;
    return;
  }

  if (thisDecl->getFieldNames().size() != other.getFieldNames().size()) {
    equal = false;
    return;
  }

  for (auto [l, r] :
       llvm::zip(thisDecl->getFieldNames(), other.getFieldNames())) {
    if (l != r) {
      equal = false;
      return;
    }
  }
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
                                 Expression *cond,
                                 ArrayRef<Statement *> thenStmt,
                                 optional<ArrayRef<Statement *>> elseStmt) {
  size_t stmtSize = thenStmt.size();
  if (elseStmt.has_value()) {
    stmtSize += elseStmt->size();
  }

  auto allocSize = totalSizeToAlloc<Statement *>(stmtSize);
  void *mem = context->alloc(allocSize);
  optional<size_t> elseSize;
  if (elseStmt.has_value())
    elseSize = elseStmt->size();

  auto *ifStmt =
      new (mem) IfStatement(range, cond, thenStmt.size(), elseSize, false);

  auto *stmts = ifStmt->getTrailingObjects<Statement *>();
  std::uninitialized_copy(thenStmt.begin(), thenStmt.end(), stmts);
  if (elseStmt.has_value()) {
    std::uninitialized_copy(elseStmt->begin(), elseStmt->end(),
                            stmts + thenStmt.size());
  }
  return ifStmt;
}

IfStatement *IfStatement::create(SMRange range, ASTContext *context,
                                 Expression *cond,
                                 ArrayRef<Statement *> thenStmt,
                                 IfStatement *elseStmt) {
  size_t stmtSize = thenStmt.size() + 1;
  auto allocSize = totalSizeToAlloc<Statement *>(stmtSize);
  void *mem = context->alloc(allocSize);
  auto *ifStmt = new (mem) IfStatement(range, cond, thenStmt.size(), 1, true);
  auto *stmts = ifStmt->getTrailingObjects<Statement *>();
  std::uninitialized_copy(thenStmt.begin(), thenStmt.end(), stmts);
  stmts[thenStmt.size()] = elseStmt;
  return ifStmt;
}

void ASTPrintVisitor::visit(const IfStatement &ast) {
  printer << "if ";
  ast.getCond()->accept(*this);
  printer << " {";
  {
    ASTPrinter::AddIndentScope scope(printer);
    for (auto stmt : ast.getThenStmt()) {
      printer.ln();
      stmt->accept(*this);
    }
  }
  printer.ln() << "}";

  if (ast.hasElse()) {
    printer << " else ";
    if (ast.isElseIfStmt()) {
      ast.getElseStmt()[0]->accept(*this);
    } else {
      printer << "{";
      {
        ASTPrinter::AddIndentScope scope(printer);
        for (auto stmt : ast.getElseStmt()) {
          printer.ln();
          stmt->accept(*this);
        }
      }
      printer.ln() << "}";
    }
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

  auto thenEq = isEqualASTArray<Statement *>(thisStmt->getThenStmt(),
                                             other.getThenStmt());
  if (!thenEq) {
    equal = false;
    return;
  }

  if (thisStmt->hasElse() != other.hasElse()) {
    equal = false;
    return;
  }

  if (thisStmt->isElseIfStmt() != other.isElseIfStmt()) {
    equal = false;
    return;
  }

  equal = !thisStmt->hasElse() ||
          isEqualASTArray<Statement *>(thisStmt->getElseStmt(),
                                       other.getElseStmt());
}

//===----------------------------------------------------------------------===//
// WhileStatement
//===----------------------------------------------------------------------===//

WhileStatement *WhileStatement::create(SMRange range, ASTContext *context,
                                       Expression *cond,
                                       ArrayRef<Statement *> body,
                                       bool isDoWhile) {
  auto allocSize = totalSizeToAlloc<Statement *>(body.size());
  void *mem = context->alloc(allocSize);
  auto *whileStmt =
      new (mem) WhileStatement(range, cond, body.size(), isDoWhile);
  std::uninitialized_copy(body.begin(), body.end(),
                          whileStmt->getTrailingObjects<Statement *>());
  return whileStmt;
}

void ASTPrintVisitor::visit(const WhileStatement &ast) {
  if (ast.isDoWhile()) {
    printer << "do ";
    printer << "{";
    {
      ASTPrinter::AddIndentScope scope(printer);
      for (auto stmt : ast.getBody()) {
        printer.ln();
        stmt->accept(*this);
      }
    }
    printer.ln() << "}"
                 << " while ";
    ast.getCond()->accept(*this);
    printer << ";";
  } else {
    printer << "while ";
    ast.getCond()->accept(*this);
    printer << " {";
    {
      ASTPrinter::AddIndentScope scope(printer);
      for (auto stmt : ast.getBody()) {
        printer.ln();
        stmt->accept(*this);
      }
    }
    printer.ln() << "}";
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
                                   std::optional<Expression *> step,
                                   ArrayRef<Statement *> body) {
  auto hasDecl = decl.has_value();
  auto hasStep = step.has_value();
  auto hasCond = cond.has_value();
  auto stmtCnt = hasDecl + body.size();
  auto exprCnt = hasCond + hasStep;
  auto allocSize =
      totalSizeToAlloc<Expression *, Statement *>(exprCnt, stmtCnt);
  void *mem = context->alloc(allocSize);
  auto *forStmt =
      new (mem) ForStatement(range, hasDecl, hasCond, hasStep, body.size());
  std::size_t idx = 0;
  auto *stmts = forStmt->getTrailingObjects<Statement *>();
  if (hasDecl)
    stmts[idx++] = *decl;
  std::uninitialized_copy(body.begin(), body.end(), stmts + idx);

  auto *exprs = forStmt->getTrailingObjects<Expression *>();
  idx = 0;
  if (hasCond)
    exprs[idx++] = *cond;
  if (hasStep)
    exprs[idx++] = *step;
  return forStmt;
}

std::optional<DeclarationStatement *> ForStatement::getDecl() const {
  if (hasDecl)
    return static_cast<DeclarationStatement *>(
        getTrailingObjects<Statement *>()[0]);
  return std::nullopt;
}
std::optional<Expression *> ForStatement::getCond() const {
  if (hasCond)
    return *(getTrailingObjects<Expression *>());
  return std::nullopt;
}
std::optional<Expression *> ForStatement::getStep() const {
  if (hasStep)
    return *(getTrailingObjects<Expression *>() + hasCond);
  return std::nullopt;
}
ArrayRef<Statement *> ForStatement::getBody() const {
  return {getTrailingObjects<Statement *>() + hasDecl, bodySize};
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
  printer << ";";

  if (ast.getCond()) {
    printer << ' ';
    ast.getCond().value()->accept(*this);
  }
  printer << ";";

  if (ast.getStep()) {
    printer << ' ';
    auto *step = *ast.getStep();
    step->accept(*this);
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
      printer.ln();
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

Expression *LambdaExpression::getExprBody() const {
  return body->cast<Expression>();
}

void ASTPrintVisitor::visit(const LambdaExpression &ast) {
  printer << "\\";
  for (auto [idx, param] : llvm::enumerate(ast.getParams())) {
    printer << param;
    if (idx != ast.getParams().size() - 1)
      printer << ", ";
  }
  printer << " => ";
  ast.getExprBody()->accept(*this);
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

  if (!thisExpr->getExprBody()->isEqual(other.getExprBody())) {
    equal = false;
    return;
  }
}

//===----------------------------------------------------------------------===//
/// StructAccess
//===----------------------------------------------------------------------===//

StructAccessExpression *StructAccessExpression::create(SMRange range,
                                                       ASTContext *context,
                                                       Expression *base,
                                                       StringRef fieldName) {
  return context->make<StructAccessExpression>(range, base, fieldName);
}

void ASTPrintVisitor::visit(const StructAccessExpression &ast) {
  ast.getBase()->accept(*this);
  printer << '.' << ast.getFieldName();
}

void ASTEqualVisitor::visit(const StructAccessExpression &other) {
  if (!thisAST->isa<StructAccessExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<StructAccessExpression>();
  if (!thisExpr->getBase()->isEqual(other.getBase())) {
    equal = false;
    return;
  }

  if (thisExpr->getFieldName() != other.getFieldName()) {
    equal = false;
    return;
  }
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
/// AssignmentExpression
//===----------------------------------------------------------------------===//

AssignmentExpression *AssignmentExpression::create(SMRange range,
                                                   ASTContext *context,
                                                   Expression *lhs,
                                                   optional<Operator> op,
                                                   Expression *rhs) {
  return context->make<AssignmentExpression>(range, lhs, op, rhs);
}

void ASTPrintVisitor::visit(const AssignmentExpression &ast) {
  ast.getLhs()->accept(*this);
  printer << " ";
  if (ast.getOperator())
    printOperator(printer.getOS(), *ast.getOperator());
  printer << "= ";
  ast.getRhs()->accept(*this);
}

void ASTEqualVisitor::visit(const AssignmentExpression &other) {
  if (!thisAST->isa<AssignmentExpression>()) {
    equal = false;
    return;
  }

  auto *thisStmt = thisAST->cast<AssignmentExpression>();
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
/// UnaryExpression
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
/// CompoundExpression
//===----------------------------------------------------------------------===//

CompoundExpression *CompoundExpression::create(SMRange range,
                                               ASTContext *context,
                                               ArrayRef<Statement *> stmts,
                                               Expression *expr) {
  auto allocSize = totalSizeToAlloc<Statement *>(stmts.size());
  void *mem = context->alloc(allocSize);
  auto *compoundExpr = new (mem) CompoundExpression(range, expr, stmts.size());
  std::uninitialized_copy(stmts.begin(), stmts.end(),
                          compoundExpr->getTrailingObjects<Statement *>());
  return compoundExpr;
}

void ASTPrintVisitor::visit(const CompoundExpression &ast) {
  printer << "{";
  {
    ASTPrinter::AddIndentScope scope(printer);
    for (auto stmt : ast.getStmts()) {
      printer.ln();
      stmt->accept(*this);
    }
    if (ast.getExpr()) {
      printer.ln();
      ast.getExpr()->accept(*this);
    }
  }
  printer.ln() << "}";
}

void ASTEqualVisitor::visit(const CompoundExpression &other) {
  if (!thisAST->isa<CompoundExpression>()) {
    equal = false;
    return;
  }

  auto *thisExpr = thisAST->cast<CompoundExpression>();
  if (!isEqualASTArray<Statement *>(thisExpr->getStmts(), other.getStmts())) {
    equal = false;
    return;
  }

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
/// GroupPattern
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

//===----------------------------------------------------------------------===//
/// StructPattern
//===----------------------------------------------------------------------===//

StructPattern *StructPattern::create(SMRange range, ASTContext *context,
                                     StringRef name, ArrayRef<Field> fields) {
  auto allocSize = totalSizeToAlloc<Field>(fields.size());
  void *mem = context->alloc(allocSize);
  StructPattern *structPattern =
      new (mem) StructPattern(range, name, fields.size());

  auto *fieldMem = structPattern->getTrailingObjects<Field>();
  std::uninitialized_copy(fields.begin(), fields.end(), fieldMem);
  return structPattern;
}

void ASTPrintVisitor::visit(const StructPattern &ast) {
  printer << ast.getName() << '(';

  for (auto I = ast.getFields().begin(), E = ast.getFields().end(); I != E;
       ++I) {
    if (I != ast.getFields().begin())
      printer << ", ";

    const auto &[ref, field, pattern] = *I;
    if (ref)
      printer << 'ref ';
    if (field.empty()) {
      pattern->accept(*this);
    } else {
      printer << field << ": ";
      pattern->accept(*this);
    }
  }

  printer << ')';
}

void ASTEqualVisitor::visit(const StructPattern &ast) {
  if (!thisAST->isa<StructPattern>()) {
    equal = false;
    return;
  }

  const StructPattern *structP = thisAST->cast<StructPattern>();
  if (structP->getName() != ast.getName()) {
    equal = false;
    return;
  }

  if (structP->getFields().size() != ast.getFields().size()) {
    equal = false;
    return;
  }

  for (auto [l, r] : llvm::zip(structP->getFields(), ast.getFields())) {
    const auto &[lRef, lField, lPattern] = l;
    const auto &[rRef, rField, rPattern] = r;

    if (lRef != rRef) {
      equal = false;
      return;
    }

    if (lField != rField) {
      equal = false;
      return;
    }

    if (!lPattern->isEqual(rPattern)) {
      equal = false;
      return;
    }
  }
}

} // namespace bara
