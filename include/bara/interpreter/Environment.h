#ifndef BARA_ENVIRONMENT_H
#define BARA_ENVIRONMENT_H

#include "bara/interpreter/Memory.h"
#include "llvm/ADT/DenseMap.h"

namespace bara {

class Environment {
public:
  Environment(const DenseMap<StringRef, ImmutableMemory *> *builtinFuncTable)
      : builtinFuncTable(builtinFuncTable) {}

  Memory *lookup(StringRef name) const;
  bool isDefinedCurrScope(StringRef name) const;
  void insert(StringRef name, Memory *memory);

  struct Scope {
    Scope(Environment &env) : env(env) { env.scopes.emplace_back(); }

    ~Scope() { env.scopes.pop_back(); }

  private:
    Environment &env;
  };

private:
  const DenseMap<StringRef, ImmutableMemory *> *builtinFuncTable;
  SmallVector<DenseMap<StringRef, Memory *>> scopes;
};
} // namespace bara

#endif // BARA_ENVIRONMENT_H
