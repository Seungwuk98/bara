#ifndef BARA_MEMORY_CONTEXT_H
#define BARA_MEMORY_CONTEXT_H

#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {
class ImmutableMemory;

class MemoryContext {
public:
  MemoryContext();
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

  DenseMap<StringRef, ImmutableMemory *> *getBuiltinFuncTable();

private:
  vector<pair<void *, function<void(void *)>>> allocations;
  DenseMap<StringRef, ImmutableMemory *> builtinFuncTable;
};
} // namespace bara

#endif // BARA_MEMORY_CONTEXT_H
