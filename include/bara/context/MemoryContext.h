#ifndef BARA_MEMORY_CONTEXT_H
#define BARA_MEMORY_CONTEXT_H

#include "bara/utils/STL.h"

namespace bara {
class MemoryContext {
public:
  void *alloc(size_t size);

private:
};
} // namespace bara

#endif // BARA_MEMORY_CONTEXT_H
