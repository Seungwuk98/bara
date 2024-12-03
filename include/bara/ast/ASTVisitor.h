#ifndef BARA_AST_VISITOR_H
#define BARA_AST_VISITOR_H

#include "bara/ast/AST.h"

namespace bara {

class Visitor {
public:
  void visit(AST &ast) { visitFn[static_cast<uint16_t>(ast.getKind())](&ast); }

protected:
  void setVisitFn(ASTKind kind, function<void(AST *)> fn) {
    visitFn[static_cast<uint16_t>(kind)] = std::move(fn);
  }

private:
  array<function<void(AST *)>, static_cast<uint16_t>(ASTKind::NUM_OF_AST)>
      visitFn;
};

template <typename ConcreteType, typename... ASTTypes>
class VisitorBase : public Visitor {
public:
  VisitorBase() {
    (setVisitFn(_inner::ASTKindMap<ASTTypes>::value,
                [this](AST *ast) {
                  static_cast<ConcreteType *>(this)->visit(
                      *ast->cast<ASTTypes>());
                }),
     ...);
  }
};

class ConstVisitor {
public:
  void visit(const AST &ast) {
    visitFn[static_cast<uint16_t>(ast.getKind())](&ast);
  }

protected:
  void setVisitFn(ASTKind kind, function<void(const AST *)> fn) {
    visitFn[static_cast<uint16_t>(kind)] = std::move(fn);
  }

private:
  array<function<void(const AST *)>, static_cast<uint16_t>(ASTKind::NUM_OF_AST)>
      visitFn;
};

template <typename ConcreteType, typename... ASTTypes>
class ConstVisitorBase : public ConstVisitor {
public:
  ConstVisitorBase() {
    (setVisitFn(_inner::ASTKindMap<ASTTypes>::value,
                [this](const AST *ast) {
                  static_cast<ConcreteType *>(this)->visit(
                      *ast->cast<ASTTypes>());
                }),
     ...);
  }
};

} // namespace bara

#endif // BARA_AST_VISITOR_H
