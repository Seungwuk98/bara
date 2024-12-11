#ifndef BARA_MEMORY_CONTEXT_H
#define BARA_MEMORY_CONTEXT_H

#include "bara/utils/STL.h"

namespace bara {
class MemoryContext {
public:
  ~MemoryContext();

  template <typename DestructTy>
  void *alloc(size_t size) {
    void *ptr = malloc(size);
    allocations.emplace_back(ptr, [](void *ptr) {
      static_cast<DestructTy *>(ptr)->~DestructTy();
      free(ptr);
    });
    return ptr;
  }

private:
  vector<pair<void *, function<void(void *)>>> allocations;
};
} // namespace bara

#endif // BARA_MEMORY_CONTEXT_H
