#ifndef BARA_AST_CONTEXT_H
#define BARA_AST_CONTEXT_H

#include "bara/ast/AST.h"
#include <cstdlib>
#include <unordered_set>

namespace bara {

class ASTContext {
public:
  ASTContext() = default;

  ~ASTContext();

  void *alloc(std::size_t size);

  template <typename T, typename... Ts>
  T *make(Ts &&...args) {
    return new (alloc(sizeof(T))) T(std::forward<Ts>(args)...);
  }

private:
  vector<void *> allocations{};
};
} // namespace bara

#endif // BARA_AST_CONTEXT_H
