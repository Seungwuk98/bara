#include "bara/interpreter/Memory.h"
#include "bara/context/GarbageCollector.h"
#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Value.h"

namespace bara {

class AssignVisitor : public MemoryVisitorBase<AssignVisitor> {
public:
  AssignVisitor(Value *value) : value(value) {}

  bool getFail() const { return fail; }

#define MEMORY(Name) void visit(Name##Memory &mem);
#include "bara/interpreter/Memory.def"

private:
  Value *value;
  bool fail = false;
};

void Memory::accept(MemoryVisitor &visitor) { visitor.visit(*this); }

bool Memory::assign(Value *value) {
  AssignVisitor visitor(value);
  accept(visitor);
  return !visitor.getFail();
}

ImmutableMemory *ImmutableMemory::create(MemoryContext *context, Value *value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto *mem = context->alloc(sizeof(ImmutableMemory));
  return new (mem) ImmutableMemory(context, value);
}

void AssignVisitor::visit(ImmutableMemory &mem) { fail = true; }

ValueMemory *ValueMemory::create(MemoryContext *context, Value *value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto *mem = context->alloc(sizeof(ValueMemory));
  return new (mem) ValueMemory(context, value);
}

void AssignVisitor::visit(ValueMemory &mem) { mem.assign(value); }

TupleMemory *TupleMemory::create(MemoryContext *context,
                                 ArrayRef<Memory *> mems) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto allocSize = totalSizeToAlloc<Memory *>(mems.size());
  auto *mem = context->alloc(allocSize);
  auto *tupleMem = new (mem) TupleMemory(context, mems.size());
  std::uninitialized_copy(mems.begin(), mems.end(),
                          tupleMem->getTrailingObjects<Memory *>());
  return tupleMem;
}

void AssignVisitor::visit(TupleMemory &mem) {
  if (!value->isa<TupleValue>())
    return;

  auto *tupleValue = value->cast<TupleValue>();
  auto mems = mem.getMemories();
  if (mems.size() != tupleValue->size()) {
    fail = true;
    return;
  }

  for (auto [mem, valueView] : llvm::zip(mems, tupleValue->getValues())) {
    if (!mem->assign(valueView)) {
      fail = true;
      return;
    }
  }
}

VectorMemory *VectorMemory::create(MemoryContext *context,
                                   ArrayRef<ValueMemory *> values,
                                   size_t capacity) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  assert(values.size() <= capacity && "Values exceed capacity");

  auto allocSize = totalSizeToAlloc<ValueMemory *>(capacity);
  auto *mem = context->alloc(allocSize);
  auto *vectorMem = new (mem) VectorMemory(context, capacity);
  std::uninitialized_copy(values.begin(), values.end(),
                          vectorMem->getTrailingObjects<ValueMemory *>());

  for (auto idx = values.size(); idx < capacity; ++idx) {
    vectorMem->getTrailingObjects<ValueMemory *>()[idx] =
        ValueMemory::create(context, nullptr);
  }
  return vectorMem;
}

void AssignVisitor::visit(VectorMemory &mem) {
  llvm_unreachable("VectorMemory should not be assigned");
}

} // namespace bara
