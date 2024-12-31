#include "bara/context/MemoryContext.h"
#include "bara/context/GarbageCollector.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/Value.h"
#include "llvm/Support/MemAlloc.h"

namespace bara {
namespace BuiltinFn {
#define BUILTIN_FUNC(Name, Identifier, Help)                                   \
  extern Value *Name(ArrayRef<Value *>, Diagnostic &, SMRange, MemoryContext *);
#include "bara/interpreter/BuiltinFunctions.def"

} // namespace BuiltinFn

MemoryContext::MemoryContext() : gc(new GC(this)) {
#define BUILTIN_FUNC(Name, Identifier, Help)                                   \
  do {                                                                         \
    auto builtinFn =                                                           \
        BuiltinFunctionValue::create(this, Identifier, Help, BuiltinFn::Name); \
    auto memory = ImmutableMemory::create(this, std::move(builtinFn));         \
    auto inserted = builtinFuncTable.try_emplace(Identifier, memory).second;   \
    assert(inserted);                                                          \
    (void)inserted;                                                            \
  } while (false);

  GCSAFE(gc) {
#include "bara/interpreter/BuiltinFunctions.def"
  }
}

MemoryContext::~MemoryContext() {
  for (const auto &[ptr, size] : allocations)
    free(ptr);
  delete gc;
}

GCTarget *MemoryContext::alloc(size_t size) {
  assert(gc->isLocked() && "GC must be locked");
  auto *ptr = static_cast<GCTarget *>(llvm::safe_malloc(size));
  totalAllocated += size;
  if (totalAllocated >= gcThreshold) {
    while (totalAllocated >= gcThreshold)
      gcThreshold <<= 1;
    gc->trigger();
  }
  allocations.emplace_back(ptr, size);
  return ptr;
}

} // namespace bara
