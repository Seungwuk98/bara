#include "bara/interpreter/Value.h"
#include "bara/ast/AST.h"
#include "llvm/ADT/SmallVectorExtras.h"

namespace bara {

unique_ptr<IntegerValue> IntegerValue::create(int64_t value) {
  auto *mem = new IntegerValue(value);
  return unique_ptr<IntegerValue>(mem);
}

unique_ptr<BoolValue> BoolValue::create(bool value) {
  auto *mem = new BoolValue(value);
  return unique_ptr<BoolValue>(mem);
}

unique_ptr<FloatValue> FloatValue::create(StringRef value) {
  auto *mem = new FloatValue(value);
  return unique_ptr<FloatValue>(mem);
}

unique_ptr<StringValue> StringValue::create(StringRef value) {
  auto *mem = new StringValue(value);
  return unique_ptr<StringValue>(mem);
}

unique_ptr<ListValue>
ListValue::create(MemoryContext *context,
                  MutableArrayRef<unique_ptr<Value>> value) {
  auto valueMemorys = llvm::map_to_vector(
      value, [context](unique_ptr<Value> &value) -> Memory * {
        return ValueMemory::create(context, std::move(value));
      });

  auto *vectorMemory = VectorMemory::create(context, valueMemorys);

  return unique_ptr<ListValue>(new ListValue(vectorMemory));
}

unique_ptr<TupleValue> TupleValue::create(ArrayRef<Memory *> mems) {
  auto *mem = new TupleValue(mems);
  return unique_ptr<TupleValue>(mem);
}

unique_ptr<NilValue> NilValue::create() {
  auto *mem = new NilValue();
  return unique_ptr<NilValue>(mem);
}

unique_ptr<FunctionValue> FunctionValue::create(const Environment &env,
                                                FunctionDeclaration *decl) {
  auto *mem = new FunctionValue(env, decl);
  return unique_ptr<FunctionValue>(mem);
}

unique_ptr<LambdaValue> LambdaValue::create(const Environment &env,
                                            LambdaExpression *decl) {
  auto *mem = new LambdaValue(env, decl);
  return unique_ptr<LambdaValue>(mem);
}

} // namespace bara
