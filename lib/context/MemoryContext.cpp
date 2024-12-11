#include "bara/context/MemoryContext.h"

namespace bara {
MemoryContext::~MemoryContext() {
  for (const auto &[ptr, destructFn] : allocations)
    destructFn(ptr);
}

} // namespace bara
