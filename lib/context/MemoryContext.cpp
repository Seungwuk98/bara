#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/Value.h"

namespace bara {
namespace BuiltinFn {

#define BUILTIN_FUNC(Name, Identifier, Help)                                   \
  extern unique_ptr<Value> Name(ArrayRef<unique_ptr<Value>>, Diagnostic &,     \
                                SMRange, MemoryContext *);
#include "bara/interpreter/BuiltinFunctions.def"

} // namespace BuiltinFn

MemoryContext::MemoryContext() {
#define BUILTIN_FUNC(Name, Identifier, Help)                                   \
  do {                                                                         \
    auto builtinFn =                                                           \
        BuiltinFunctionValue::create(Identifier, Help, BuiltinFn::Name);       \
                                                                               \
    auto memory = ImmutableMemory::create(this, std::move(builtinFn));         \
    auto inserted = builtinFuncTable.try_emplace(Identifier, memory).second;   \
    assert(inserted);                                                          \
    (void)inserted;                                                            \
  } while (false);
#include "bara/interpreter/BuiltinFunctions.def"
}

MemoryContext::~MemoryContext() {
  for (const auto &[ptr, destructFn] : allocations)
    destructFn(ptr);
}

} // namespace bara
