#ifndef BARA_MEMORY_H
#define BARA_MEMORY_H

#include "bara/context/MemoryContext.h"
#include "bara/utils/LLVM.h"
#include <memory>

namespace bara {
class Value;

using std::unique_ptr;

enum class MemoryKind {
  Value,
  Tuple,
  Vector,
};

class Memory {
public:
  MemoryKind getKind() const { return kind; }

protected:
  Memory(MemoryKind kind) : kind(kind) {}

private:
  MemoryKind kind;
};

class ValueMemory final : public Memory {
  ValueMemory(unique_ptr<Value> value)
      : Memory(MemoryKind::Value), value(std::move(value)) {}

  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Value;
  }

public:
  Value *view() const { return value.get(); }
  unique_ptr<Value> load() { return std::move(value); }
  void assign(unique_ptr<Value> value) { this->value = std::move(value); }

  static ValueMemory *create(MemoryContext *context, unique_ptr<Value> value);

private:
  unique_ptr<Value> value;
};

class TupleMemory final : public Memory,
                          public TrailingObjects<TupleMemory, Memory *> {
public:
  static TupleMemory *create(MemoryContext *context, ArrayRef<Memory *> mems);

  static bool classof(const Memory *mem) {
    return mem->getKind() == MemoryKind::Tuple;
  }

private:
  size_t size;
};

class VectorMemory final : public Memory {

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
