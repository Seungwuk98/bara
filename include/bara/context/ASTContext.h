#ifndef BARA_AST_CONTEXT_H
#define BARA_AST_CONTEXT_H

#include "bara/ast/AST.h"
#include <cstdlib>
#include <vector>

namespace bara {

class ASTContext {
public:
  ASTContext();

  ~ASTContext() {
    for (auto ptr : allocations) {
      free(ptr);
    }
  }

  void *alloc(std::size_t size) {
    void *ptr = malloc(size);
    allocations.push_back(ptr);
    return ptr;
  }

  template <typename T, typename... Ts> T *make(Ts &&...args) {
    return new (alloc(sizeof(T))) T(std::forward<Ts>(args)...);
  }

  BreakStatement *getBreakStmt() const { return breakStmt; }
  ContinueStatement *getContinueStmt() const { return continueStmt; }
  BooleanLiteral *getTrueStmt() const { return trueStmt; }
  BooleanLiteral *getFalseStmt() const { return falseStmt; }

private:
  BreakStatement *breakStmt;
  ContinueStatement *continueStmt;
  BooleanLiteral *trueStmt;
  BooleanLiteral *falseStmt;
  std::vector<void *> allocations;
};
} // namespace bara

#endif // BARA_AST_CONTEXT_H
