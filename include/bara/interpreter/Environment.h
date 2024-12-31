#ifndef BARA_ENVIRONMENT_H
#define BARA_ENVIRONMENT_H

#include "bara/interpreter/Memory.h"

namespace bara {

class Environment {
public:
  Environment(const DenseMap<StringRef, ImmutableMemory *> *builtinFuncTable)
      : builtinFuncTable(builtinFuncTable) {}

  Environment(const DenseMap<StringRef, ImmutableMemory *> *builtinFuncTable,
              ArrayRef<pair<StringRef, Memory *>> memories)
      : builtinFuncTable(builtinFuncTable) {
    scopes.emplace_back();
    for (const auto &[name, memory] : memories)
      insert(name, memory);
  }

  Memory *lookup(StringRef name) const;
  bool isDefinedCurrScope(StringRef name) const;
  void insert(StringRef name, Memory *memory);

  SmallVector<pair<StringRef, Memory *>> shallowCopy() const;
  SmallVector<pair<StringRef, Memory *>> capture(MemoryContext *context) const;

  struct Scope {
    Scope(const Scope &) = delete;
    Scope &operator=(const Scope &) = delete;
    Scope(Scope &&) = delete;
    Scope &operator=(Scope &&) = delete;

    Scope(Environment &env) : env(env) { env.scopes.emplace_back(); }

    ~Scope() { env.scopes.pop_back(); }

  private:
    Environment &env;
  };

  void dump() const;

private:
  friend class GC;
  const DenseMap<StringRef, ImmutableMemory *> *builtinFuncTable;
  SmallVector<DenseMap<StringRef, Memory *>> scopes;
};
} // namespace bara

#endif // BARA_ENVIRONMENT_H
