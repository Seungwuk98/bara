#ifndef BARA_GARBAGE_COLLECTOR_H
#define BARA_GARBAGE_COLLECTOR_H

#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {

class MemoryContext;
class StmtInterpreter;
class Value;
class Memory;

class GC {
public:
  GC(MemoryContext *context) : context(context) {}

  bool trigger();
  bool isLocked() const { return lock; }

  struct SafeGuard {
    SafeGuard(const SafeGuard &) = delete;
    SafeGuard &operator=(const SafeGuard &) = delete;

    SafeGuard(GC *gc) : gc(gc), savedStatus(gc->lock) {
      assert(!gc->triggered && "GC is already triggered");
      gc->lock = true;
    }

    ~SafeGuard() {
      if (!savedStatus && gc->triggered) {
        gc->collect();
        gc->triggered = false;
      }
      gc->lock = savedStatus;
    }

  private:
    GC *gc;
    bool savedStatus;
  };

  struct RootRegister {
    RootRegister() : gc(nullptr) {}
    RootRegister(const RootRegister &) = delete;
    RootRegister &operator=(const RootRegister &) = delete;
    RootRegister(RootRegister &&other) {
      gc = other.gc;
      root = other.root;
      index = other.index;
      if (gc)
        gc->roots[index].second = this;
      other.gc = nullptr;
      other.root = {};
      other.index = -1;
    }
    RootRegister &operator=(RootRegister &&other) {
      gc = other.gc;
      root = other.root;
      index = other.index;
      if (gc)
        gc->roots[index].second = this;
      other.gc = nullptr;
      other.root = {};
      other.index = -1;
      return *this;
    }

    RootRegister(GC *gc, variant<std::monostate, Value *, Memory *> root)
        : gc(gc), root(root), index(gc->roots.size()) {
      gc->roots.emplace_back(root, this);
    }

    ~RootRegister() {
      if (gc) {
        gc->roots[index] = {std::monostate(), nullptr};
      }
    }

    bool hasValue() const { return std::holds_alternative<Value *>(root); }
    Value *getValue() const { return std::get<Value *>(root); }
    Memory *getMemory() const { return std::get<Memory *>(root); }
    void setIndex(size_t index) { this->index = index; }

  private:
    GC *gc;
    variant<std::monostate, Value *, Memory *> root;
    size_t index;
  };

  RootRegister registerRoot(variant<std::monostate, Value *, Memory *> value) {
    return RootRegister(this, value);
  }

  void setInterpreter(StmtInterpreter *interpreter) {
    this->interpreter = interpreter;
  }

private:
  void collect();
  void release();

private:
  MemoryContext *context;

  /// Environment and result are candidates for inspection
  StmtInterpreter *interpreter;

  SmallVector<
      std::pair<variant<std::monostate, Value *, Memory *>, RootRegister *>>
      roots;

  bool lock = false;
  bool triggered = false;
};

#define GCSAFE(gc) if (GC::SafeGuard _safe_guard(gc); true)

class GCTarget {
public:
  bool isMarked() const { return marked; }
  void unmark() { marked = false; }
  void mark() { marked = true; }

private:
  friend class GC;
  bool marked = false;
};

} // namespace bara

#endif // BARA_GARBAGE_COLLECTOR_H
