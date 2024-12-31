#ifndef BARA_MEMORY_H
#define BARA_MEMORY_H

#include "bara/context/GarbageCollector.h"
#include "bara/context/MemoryContext.h"
#include "bara/utils/LLVM.h"
#include "bara/utils/VisitorBase.h"

namespace bara {
class Value;
class Memory;

enum class MemoryKind {
#define MEMORY(Name) Name,
#include "bara/interpreter/Memory.def"
  NUM,
};

#define MEMORY(Name) class Name##Memory;
#include "bara/interpreter/Memory.def"

namespace _inner {
template <typename T>
struct MemoryKindMapper {};

#define MEMORY(Name)                                                           \
  template <>                                                                  \
  struct MemoryKindMapper<Name##Memory> {                                      \
    static const MemoryKind value = MemoryKind::Name;                          \
  };
#include "bara/interpreter/Memory.def"

} // namespace _inner

using MemoryVisitor = utils::Visitor<Memory>;
template <typename ConcreteType>
using MemoryVisitorBase =
    utils::VisitorBase<ConcreteType, Memory, false, _inner::MemoryKindMapper
#define MEMORY(Name) , Name##Memory
#include "bara/interpreter/Memory.def"
                       >;

class Memory : public GCTarget {
public:
  using KindTy = MemoryKind;

  template <typename... U>
  bool isa() const {
    return llvm::isa<U...>(this);
  }
  template <typename U>
  const U *cast() const {
    return llvm::cast<U>(this);
  }
  template <typename U>
  U *cast() {
    return llvm::cast<U>(this);
  }
  template <typename U>
  const U *dyn_cast() const {
    return llvm::dyn_cast<U>(this);
  }
  template <typename U>
  U *dyn_cast() {
    return llvm::dyn_cast<U>(this);
  }

  void accept(MemoryVisitor &visitor);

  MemoryKind getKind() const { return kind; }

  bool assign(Value *value);

  MemoryContext *getContext() const { return context; }

  static bool classof(const GCTarget *target);

protected:
  Memory(MemoryContext *context, MemoryKind kind)
      : context(context), kind(kind) {}

private:
  MemoryContext *context;
  MemoryKind kind;
};

class ImmutableMemory final : public Memory {
  ImmutableMemory(MemoryContext *context, Value *value)
      : Memory(context, MemoryKind::Immutable), value(std::move(value)) {}

public:
  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Immutable;
  }

  Value *get() const { return value; }

  static ImmutableMemory *create(MemoryContext *context, Value *value);

private:
  Value *value;
};

class ValueMemory final : public Memory {
  ValueMemory(MemoryContext *context, Value *value)
      : Memory(context, MemoryKind::Value), value(std::move(value)) {}

public:
  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Value;
  }

  Value *get() const { return value; }
  void assign(Value *value) { this->value = value; }

  static ValueMemory *create(MemoryContext *context, Value *value);

private:
  Value *value;
};

class TupleMemory final : public Memory,
                          public TrailingObjects<TupleMemory, Memory *> {
  TupleMemory(MemoryContext *context, size_t size)
      : Memory(context, MemoryKind::Tuple), size(size) {}

public:
  static TupleMemory *create(MemoryContext *context, ArrayRef<Memory *> mems);

  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Tuple;
  }

  ArrayRef<Memory *> getMemories() const {
    return {getTrailingObjects<Memory *>(), size};
  }

private:
  size_t size;
};

class VectorMemory final : public Memory,
                           public TrailingObjects<VectorMemory, ValueMemory *> {
  VectorMemory(MemoryContext *context, size_t capacity)
      : Memory(context, MemoryKind::Vector), capacity(capacity) {}

public:
  static VectorMemory *create(MemoryContext *context,
                              ArrayRef<ValueMemory *> values, size_t capacity);

  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Vector;
  }

  ValueMemory *get(size_t index) const {
    assert(index < capacity && "index out of bounds");
    return getTrailingObjects<ValueMemory *>()[index];
  }

  ArrayRef<ValueMemory *> getMemories() const {
    return {getTrailingObjects<ValueMemory *>(), capacity};
  }
  size_t getCapacity() const { return capacity; }

private:
  size_t capacity;
};

} // namespace bara

#endif // BARA_MEMORY_H
