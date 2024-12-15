#include "bara/interpreter/Memory.h"
#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Value.h"
#include <llvm-18/llvm/Support/ErrorHandling.h>

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

ImmutableMemory::~ImmutableMemory() = default;

ImmutableMemory *ImmutableMemory::create(MemoryContext *context,
                                         unique_ptr<Value> value) {
  auto *mem = context->alloc<ImmutableMemory>(sizeof(ImmutableMemory));
  return new (mem) ImmutableMemory(context, std::move(value));
}

void AssignVisitor::visit(ImmutableMemory &mem) { fail = true; }

ValueMemory::~ValueMemory() = default;

ValueMemory *ValueMemory::create(MemoryContext *context,
                                 unique_ptr<Value> value) {
  auto *mem = context->alloc<ValueMemory>(sizeof(ValueMemory));
  return new (mem) ValueMemory(context, std::move(value));
}

void AssignVisitor::visit(ValueMemory &mem) { mem.assign(value->clone()); }

TupleMemory *TupleMemory::create(MemoryContext *context,
                                 ArrayRef<Memory *> mems) {
  auto allocSize = totalSizeToAlloc<Memory *>(mems.size());
  auto *mem = context->alloc<TupleMemory>(allocSize);
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

  for (auto [mem, valueMemory] : llvm::zip(mems, tupleValue->getMemories())) {
    if (!mem->assign(valueMemory->view())) {
      fail = true;
      return;
    }
  }
}

VectorMemory *VectorMemory::create(MemoryContext *context,
                                   ArrayRef<ValueMemory *> mems) {
  auto *mem = context->alloc<VectorMemory>(sizeof(VectorMemory));
  return new (mem) VectorMemory(context, mems);
}

void AssignVisitor::visit(VectorMemory &mem) {
  llvm_unreachable("Vector memory is not designed for assignment");
}

} // namespace bara
