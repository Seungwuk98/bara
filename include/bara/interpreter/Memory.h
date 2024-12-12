#ifndef BARA_MEMORY_H
#define BARA_MEMORY_H

#include "bara/context/MemoryContext.h"
#include "bara/utils/LLVM.h"
#include "bara/utils/VisitorBase.h"
#include <memory>

namespace bara {
class Value;
class Memory;

using std::unique_ptr;

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

class Memory {
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

  MemoryKind getKind() const { return kind; }

  bool assign(Value *value);

protected:
  Memory(MemoryKind kind) : kind(kind) {}

private:
  MemoryKind kind;
};

class ImmutableMemory final : public Memory {
  ImmutableMemory(unique_ptr<Value> value)
      : Memory(MemoryKind::Immutable), value(std::move(value)) {}

public:
  ~ImmutableMemory();
  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Immutable;
  }

  Value *view() const { return value.get(); }

  static ImmutableMemory *create(MemoryContext *context,
                                 unique_ptr<Value> value);

private:
  unique_ptr<Value> value;
};

class ValueMemory final : public Memory {
  ValueMemory(unique_ptr<Value> value)
      : Memory(MemoryKind::Value), value(std::move(value)) {}

public:
  ~ValueMemory();

  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Value;
  }

  Value *view() const { return value.get(); }
  void assign(unique_ptr<Value> value) { this->value = std::move(value); }

  static ValueMemory *create(MemoryContext *context, unique_ptr<Value> value);

private:
  unique_ptr<Value> value;
};

class TupleMemory final : public Memory,
                          public TrailingObjects<TupleMemory, Memory *> {
  TupleMemory(size_t size) : Memory(MemoryKind::Tuple), size(size) {}

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

class VectorMemory final : public Memory {
  VectorMemory(ArrayRef<ValueMemory *> mems)
      : Memory(MemoryKind::Vector), mems(mems) {}

public:
  static VectorMemory *create(MemoryContext *context,
                              ArrayRef<ValueMemory *> mems);

  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Vector;
  }

  Memory *get(size_t index) const { return mems[index]; }
  size_t size() const { return mems.size(); }
  bool empty() const { return mems.empty(); }
  auto begin() const { return mems.begin(); }
  auto end() const { return mems.end(); }

  void reserve(size_t size) { mems.reserve(size); }
  void push(ValueMemory *mem) { mems.emplace_back(mem); }
  void pop() { mems.pop_back(); }

private:
  vector<ValueMemory *> mems;
};

} // namespace bara

#endif // BARA_MEMORY_H
