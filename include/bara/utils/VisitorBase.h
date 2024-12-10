#ifndef BARA_UTILS_VISITOR_BASE_H
#define BARA_UTILS_VISITOR_BASE_H

#include "bara/utils/STL.h"
#include <type_traits>

namespace bara::utils {

template <typename BaseTy, bool IsConst = false>
class Visitor {
public:
  using Kind = BaseTy::KindTy;
  using VisitTy = std::conditional_t<IsConst, const BaseTy, BaseTy>;

  void visit(VisitTy &ast) {
    visitFn[static_cast<uint16_t>(ast.getKind())](&ast);
  }

protected:
  void setVisitFn(Kind kind, function<void(VisitTy *)> fn) {
    visitFn[static_cast<uint16_t>(kind)] = std::move(fn);
  }

private:
  array<function<void(VisitTy *)>, static_cast<uint16_t>(Kind::NUM)> visitFn;
};

template <typename ConcreteType, typename BaseTy, bool IsConst,
          template <typename> typename KindMapper, typename... VisitTypes>
class VisitorBase : public Visitor<BaseTy, IsConst> {
public:
  using ParentTy = Visitor<BaseTy, IsConst>;
  using VisitTy = ParentTy::VisitTy;

  VisitorBase() {
    (ParentTy::setVisitFn(KindMapper<VisitTypes>::value,
                          [this](VisitTy *obj) {
                            static_cast<ConcreteType *>(this)->visit(
                                *obj->template cast<VisitTypes>());
                          }),
     ...);
  }
};
} // namespace bara::utils

#endif // BARA_UTILS_VISITOR_BASE_H
