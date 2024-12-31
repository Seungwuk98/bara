#include "bara/interpreter/Environment.h"
#include "bara/context/GarbageCollector.h"
#include "bara/interpreter/Memory.h"
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

SmallVector<pair<StringRef, Memory *>>
Environment::capture(MemoryContext *context) const {
  assert(context->getGC()->isLocked() && "GC must be locked");
  DenseMap<StringRef, Memory *> captureScope;
  for (const auto &scope : scopes)
    for (const auto &[name, memory] : scope)
      captureScope[name] = memory;

  SmallVector<pair<StringRef, Memory *>> result;
  result.reserve(captureScope.size());
  for (auto &[name, memory] : captureScope) {
    Memory *newMemory;
    if (auto *immut = memory->dyn_cast<ImmutableMemory>())
      newMemory = ImmutableMemory::create(context, immut->get());
    else
      newMemory =
          ValueMemory::create(context, memory->cast<ValueMemory>()->get());
    result.emplace_back(name, newMemory);
  }

  return result;
}

SmallVector<pair<StringRef, Memory *>> Environment::shallowCopy() const {
  DenseMap<StringRef, Memory *> shallowScope;
  for (const auto &scope : scopes)
    for (const auto &[name, memory] : scope)
      shallowScope[name] = memory;

  SmallVector<pair<StringRef, Memory *>> result;
  result.reserve(shallowScope.size());
  for (auto &[name, memory] : shallowScope)
    result.emplace_back(name, memory);
  return result;
}

void Environment::dump() const {
  for (const auto &scope : llvm::reverse(scopes)) {
    errs() << "{\n";
    for (const auto &[name, memory] : scope) {
      llvm::errs() << " " << name << " -> ";
      if (auto valueM = memory->dyn_cast<ValueMemory>())
        errs() << valueM->get()->toString();
      else
        errs() << memory->cast<ImmutableMemory>()->get()->toString();
    }
    errs() << "}\n";
  }
}
} // namespace bara
