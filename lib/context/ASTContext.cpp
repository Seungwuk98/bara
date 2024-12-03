#include "bara/context/ASTContext.h"
#include "bara/ast/AST.h"

namespace bara {

ASTContext::~ASTContext() {
  for (auto ptr : allocations) {
    free(ptr);
  }
}

void *ASTContext::alloc(std::size_t size) {
  void *ptr = malloc(size);
  allocations.push_back(ptr);
  return ptr;
}

} // namespace bara
