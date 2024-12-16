#ifndef BARA_UTILS_VISITOR_BASE_H
#define BARA_UTILS_VISITOR_BASE_H

#include "bara/utils/STL.h"
#include <type_traits>

namespace bara::utils {

template <typename BaseTy, bool IsConst = false>
class Visitor {
public:
  using Kind = BaseTy::KindTy;
  using VisitorTy = Visitor<BaseTy, IsConst>;
  using VisitTy = std::conditional_t<IsConst, const BaseTy, BaseTy>;
  using FunctionArrayTy = std::function<void(VisitorTy *, VisitTy *)> *;

  void visit(VisitTy &ast) {
    assert(visitFn[static_cast<uint16_t>(ast.getKind())] &&
           "No visit function for this AST node.");
    visitFn[static_cast<uint16_t>(ast.getKind())](this, &ast);
  }

protected:
  Visitor(FunctionArrayTy visitFn) : visitFn(visitFn) {}

private:
  FunctionArrayTy visitFn;
};

template <typename ConcreteType, typename BaseTy, bool IsConst,
          template <typename> typename KindMapper, typename... VisitTypes>
class VisitorBase : public Visitor<BaseTy, IsConst> {
public:
  using ParentTy = Visitor<BaseTy, IsConst>;
  using VisitTy = ParentTy::VisitTy;
  using VisitorTy = ParentTy::VisitorTy;
  using Kind = BaseTy::KindTy;
  using FunctionArrayTy = ParentTy::FunctionArrayTy;

  VisitorBase() : Visitor<BaseTy, IsConst>(getVisitFn()) {}

private:
  FunctionArrayTy getVisitFn() {
    static function<void(VisitorTy *, VisitTy *)> visitFn[static_cast<uint16_t>(
        Kind::NUM)] = {[static_cast<uint16_t>(KindMapper<VisitTypes>::value)] =
                           [](VisitorTy *visitor, VisitTy *ast) {
                             static_cast<ConcreteType *>(visitor)->visit(
                                 *ast->template cast<VisitTypes>());
                           }...};
    return visitFn;
  }
};

} // namespace bara::utils

#endif // BARA_UTILS_VISITOR_BASE_H
