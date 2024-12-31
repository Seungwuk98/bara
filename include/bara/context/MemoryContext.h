#ifndef BARA_MEMORY_CONTEXT_H
#define BARA_MEMORY_CONTEXT_H

#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {
class ImmutableMemory;
class GC;
class GCTarget;

class MemoryContext {
public:
  using MarkFn = function<bool(void *)>;

  MemoryContext();
  ~MemoryContext();

  GCTarget *alloc(size_t size);

  const DenseMap<StringRef, ImmutableMemory *> *getBuiltinFuncTable() const {
    return &builtinFuncTable;
  }

  GC *getGC() const { return gc; }

  void setGCThreshold(size_t threshold) { gcThreshold = threshold; }

private:
  vector<pair<GCTarget *, size_t>> allocations;
  DenseMap<StringRef, ImmutableMemory *> builtinFuncTable;

  size_t totalAllocated = 0;
  size_t gcThreshold = 1024 * 1024 * 10; // 10MB

  friend class GC;
  GC *gc;
};

} // namespace bara

#endif // BARA_MEMORY_CONTEXT_H
