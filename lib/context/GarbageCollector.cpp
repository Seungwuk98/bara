#include "bara/context/GarbageCollector.h"
#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"

namespace bara {

bool GC::trigger() {
  if (lock) {
    triggered = true;
    return false;
  } else {
    collect();
    return true;
  }
}

class MemoryMarker : public MemoryVisitorBase<MemoryMarker> {
public:
#define MEMORY(Name) void visit(Name##Memory &mem);
#include "bara/interpreter/Memory.def"
};

class ValueMarker : public ValueVisitorBase<ValueMarker> {
public:
#define VALUE(Name) void visit(Name##Value &val);
#include "bara/interpreter/Value.def"
};

static MemoryMarker memoryMarker;
static ValueMarker valueMarker;

void MemoryMarker::visit(ValueMemory &mem) {
  if (mem.isMarked())
    return;
  mem.mark();
  if (mem.get())
    mem.get()->accept(valueMarker);
}

void MemoryMarker::visit(TupleMemory &mem) {
  if (mem.isMarked())
    return;
  mem.mark();
  for (auto &memory : mem.getMemories())
    memory->accept(*this);
}

void MemoryMarker::visit(ImmutableMemory &mem) {
  if (mem.isMarked())
    return;
  mem.mark();
  if (mem.get())
    mem.get()->accept(valueMarker);
}

void MemoryMarker::visit(VectorMemory &mem) {
  if (mem.isMarked())
    return;
  mem.mark();
  for (auto &memory : mem.getMemories())
    memory->accept(*this);
}

void ValueMarker::visit(IntegerValue &val) { val.mark(); }
void ValueMarker::visit(FloatValue &val) { val.mark(); }
void ValueMarker::visit(BoolValue &val) { val.mark(); }
void ValueMarker::visit(StringValue &val) { val.mark(); }
void ValueMarker::visit(ListValue &val) {
  if (val.isMarked())
    return;
  val.mark();
  val.getVectorMemory()->accept(memoryMarker);
}
void ValueMarker::visit(TupleValue &val) {
  if (val.isMarked())
    return;
  val.mark();
  for (auto &element : val.getValues())
    element->accept(*this);
}
void ValueMarker::visit(NilValue &val) { val.mark(); }
void ValueMarker::visit(FunctionValue &val) {
  if (val.isMarked())
    return;
  val.mark();
  for (const auto &[_, memory] : val.getEnvVars())
    memory->accept(memoryMarker);
}
void ValueMarker::visit(LambdaValue &val) {
  if (val.isMarked())
    return;
  val.mark();
  for (const auto &[_, memory] : val.getCaptures())
    memory->accept(memoryMarker);
}
void ValueMarker::visit(BuiltinFunctionValue &val) { val.mark(); }

void ValueMarker::visit(StructValue &val) {
  if (val.isMarked())
    return;
  val.mark();
  for (const auto memory : val.getMembers())
    memory->accept(memoryMarker);
}

void GC::collect() {
  /// stmt interpreter return value
  if (interpreter->returnValue)
    interpreter->returnValue->accept(valueMarker);

  /// environment
  for (const auto &env : interpreter->stack)
    for (const auto &scope : env->scopes)
      for (const auto &[name, memory] : scope)
        memory->accept(memoryMarker);

  /// builtin functions
  for (const auto &[_, memory] : *context->getBuiltinFuncTable())
    memory->accept(memoryMarker);

  SmallVector<
      std::pair<variant<std::monostate, Value *, Memory *>, RootRegister *>>
      newRoots;
  /// root registers
  for (const auto &[root, reg] : roots)
    std::visit(
        [&]<typename T>(T obj) {
          if constexpr (std::is_same_v<T, Value *>) {
            obj->accept(valueMarker);
          } else if constexpr (std::is_same_v<T, Memory *>) {
            obj->accept(memoryMarker);
          } else {
            return;
          }
          reg->setIndex(newRoots.size());
          newRoots.emplace_back(root, reg);
        },
        root);

  roots = std::move(newRoots);

  release();
  /// update threshold
  /// After GC, if the unreleased memory is more than 90% of the threshold,
  /// double the threshold.
  /// This is to avoid frequent GC and increase memory usage.
  /// 90% is an empirical number.
  while (context->gcThreshold < (context->totalAllocated * 10 / 9)) {
    context->gcThreshold <<= 1;
  }
}

void GC::release() {
  vector<pair<GCTarget *, size_t>> unReleased;
  size_t unReleasedSize = 0;
  for (const auto &[ptr, size] : context->allocations) {
    if (ptr->isMarked()) {
      unReleased.emplace_back(ptr, size);
      ptr->unmark();
      unReleasedSize += size;
    } else {
      free(ptr);
    }
  }
  context->allocations = std::move(unReleased);
  context->totalAllocated = unReleasedSize;
}

} // namespace bara
