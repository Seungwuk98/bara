#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Value.h"

namespace bara {
namespace BuiltinFn {

extern unique_ptr<Value> print(ArrayRef<unique_ptr<Value>> args,
                               Diagnostic &diag);
extern unique_ptr<Value> help(ArrayRef<unique_ptr<Value>> args,
                              Diagnostic &diag);
extern unique_ptr<Value> push(ArrayRef<unique_ptr<Value>> args,
                              Diagnostic &diag);
extern unique_ptr<Value> pop(ArrayRef<unique_ptr<Value>> args,
                             Diagnostic &diag);
extern unique_ptr<Value> str(ArrayRef<unique_ptr<Value>> args,
                             Diagnostic &diag);
extern unique_ptr<Value> len(ArrayRef<unique_ptr<Value>> args,
                             Diagnostic &diag);
extern unique_ptr<Value> intCast(ArrayRef<unique_ptr<Value>> args,
                                 Diagnostic &diag);
extern unique_ptr<Value> floatCast(ArrayRef<unique_ptr<Value>> args,
                                   Diagnostic &diag);
extern unique_ptr<Value> boolCast(ArrayRef<unique_ptr<Value>> args,
                                  Diagnostic &diag);
extern unique_ptr<Value> type(ArrayRef<unique_ptr<Value>> args,
                              Diagnostic &diag);

} // namespace BuiltinFn

MemoryContext::MemoryContext() {}

MemoryContext::~MemoryContext() {
  for (const auto &[ptr, destructFn] : allocations)
    destructFn(ptr);
}

} // namespace bara
