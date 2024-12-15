#include "bara/interpreter/Environment.h"
#include "bara/interpreter/Value.h"

namespace bara {

Memory *Environment::lookup(StringRef name) const {
  for (const auto &scope : llvm::reverse(scopes)) {
    auto it = scope.find(name);
    if (it != scope.end())
      return it->second;
  }

  if (auto it = builtinFuncTable->find(name); it != builtinFuncTable->end())
    return it->second;
  return nullptr;
}

bool Environment::isDefinedCurrScope(StringRef name) const {
  assert(!scopes.empty() && "No scope to check");
  return scopes.back().count(name);
}

void Environment::insert(StringRef name, Memory *memory) {
  assert(!scopes.empty() && "No scope to insert");
  assert(!isDefinedCurrScope(name) && "Name already defined in current scope");
  scopes.back().try_emplace(name, memory);
}

} // namespace bara
