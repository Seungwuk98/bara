#include "bara/interpreter/Value.h"
#include "bara/ast/AST.h"
#include "bara/context/GarbageCollector.h"
#include "bara/context/MemoryContext.h"
#include "llvm/ADT/SmallVectorExtras.h"
#include "llvm/Support/ManagedStatic.h"

namespace bara {

class ValuePrintVisitor : public ConstValueVisitorBase<ValuePrintVisitor> {
public:
  ValuePrintVisitor(ASTPrinter &printer) : printer(printer) {}

#define VALUE(Name) void visit(const Name##Value &value);
#include "bara/interpreter/Value.def"

private:
  ASTPrinter &printer;
};

class ToBoolVisitor : public ConstValueVisitorBase<ToBoolVisitor> {
public:
  ToBoolVisitor() : result(nullopt) {}

  void init() { result = nullopt; }

  std::optional<bool> getResult() const { return result; }

#define VALUE(Name) void visit(const Name##Value &value);
#include "bara/interpreter/Value.def"

private:
  optional<bool> result;
};

llvm::ManagedStatic<ToBoolVisitor> toBoolVisitor;

class ValueEqVisitor : public ConstValueVisitorBase<ValueEqVisitor> {
public:
  ValueEqVisitor(const Value *r) : r(r), result(false) {}

#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"

  bool getResult() const { return result; }

private:
  const Value *r;
  bool result;
};

void Value::accept(ConstValueVisitor &visitor) const { visitor.visit(*this); }
void Value::accept(ValueVisitor &visitor) { visitor.visit(*this); }

string Value::toString() const {
  string str;
  raw_string_ostream os(str);
  ASTPrinter printer(os);
  ValuePrintVisitor printVisitor(printer);
  accept(printVisitor);
  return os.str();
}

optional<bool> Value::toBool() const {
  toBoolVisitor->init();
  accept(*toBoolVisitor);
  return toBoolVisitor->getResult();
}

bool Value::isEqual(const Value *other) const {
  ValueEqVisitor visitor(other);
  accept(visitor);
  return visitor.getResult();
}

//===----------------------------------------------------------------------===//
/// IntegerValue
//===----------------------------------------------------------------------===//

IntegerValue *IntegerValue::create(MemoryContext *context, int64_t value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  return new (context->alloc(sizeof(IntegerValue)))
      IntegerValue(context, value);
}

void ValuePrintVisitor::visit(const IntegerValue &value) {
  printer << value.getValue();
}

void ToBoolVisitor::visit(const IntegerValue &value) {
  result = value.getValue() != 0;
}

void ValueEqVisitor::visit(const IntegerValue &l) {
  auto value = false;
  if (const auto *intR = r->dyn_cast<IntegerValue>())
    value = l.getValue() == intR->getValue();
  result = value;
}

//===----------------------------------------------------------------------===//
/// BoolValue
//===----------------------------------------------------------------------===//

BoolValue *BoolValue::create(MemoryContext *context, bool value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  return new (context->alloc(sizeof(BoolValue))) BoolValue(context, value);
}

void ToBoolVisitor::visit(const BoolValue &value) { result = value.getValue(); }

void ValuePrintVisitor::visit(const BoolValue &value) {
  printer << (value.getValue() ? "true" : "false");
}

void ValueEqVisitor::visit(const BoolValue &l) {
  auto value = false;
  if (const auto *boolR = r->dyn_cast<BoolValue>())
    value = l.getValue() == boolR->getValue();
  result = value;
}

//===----------------------------------------------------------------------===//
/// FloatValue
//===----------------------------------------------------------------------===//

FloatValue *FloatValue::create(MemoryContext *context, StringRef value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  return new (context->alloc(sizeof(FloatValue))) FloatValue(context, value);
}

FloatValue *FloatValue::create(MemoryContext *context, const APFloat &value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  return new (context->alloc(sizeof(FloatValue))) FloatValue(context, value);
}

void ValuePrintVisitor::visit(const FloatValue &value) {
  SmallVector<char> buffer;
  value.getValue().toString(buffer);
  printer << buffer;
}

void ToBoolVisitor::visit(const FloatValue &value) {
  result = value.getValue().convertToDouble() != 0.0;
}

void ValueEqVisitor::visit(const FloatValue &l) {
  auto value = false;
  if (const auto *floatR = r->dyn_cast<FloatValue>())
    value = l.getValue() == floatR->getValue();
  result = value;
}

//===----------------------------------------------------------------------===//
/// StringValue
//===----------------------------------------------------------------------===//

StringValue *StringValue::create(MemoryContext *context, StringRef value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto sizeToAlloc = totalSizeToAlloc<char>(value.size());
  auto *stringValue =
      new (context->alloc(sizeToAlloc)) StringValue(context, value.size());
  std::uninitialized_copy(value.begin(), value.end(),
                          stringValue->getTrailingObjects<char>());
  return stringValue;
}

void ValuePrintVisitor::visit(const StringValue &value) {
  printer << value.getValue();
}

void ToBoolVisitor::visit(const StringValue &value) {
  result = !value.getValue().empty();
}

void ValueEqVisitor::visit(const StringValue &l) {
  auto value = false;
  if (const auto *strR = r->dyn_cast<StringValue>())
    value = l.getValue() == strR->getValue();
  result = value;
}

//===----------------------------------------------------------------------===//
/// ListValue
//===----------------------------------------------------------------------===//

ListValue *ListValue::create(MemoryContext *context, ArrayRef<Value *> value) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto valueMemories = llvm::map_to_vector(value, [context](Value *value) {
    return ValueMemory::create(context, value);
  });

  VectorMemory *vectorMemory;
  if (value.size() <= 4)
    vectorMemory = VectorMemory::create(context, valueMemories, 4);
  else
    vectorMemory = VectorMemory::create(context, valueMemories, value.size());

  return new (context->alloc(sizeof(ListValue)))
      ListValue(context, value.size(), vectorMemory);
}

void ListValue::reallocate(size_t newCapacity) {
  assert(getContext()->getGC()->isLocked() && "GC must be locked");
  auto values = memories->getMemories().slice(0, length);
  auto *newMemories = VectorMemory::create(getContext(), values, newCapacity);
  memories = newMemories;
}

void ListValue::push(Value *value) {
  assert(value && "Value must not be null");
  GCSAFE(getContext()->getGC()) {
    if (length == memories->getCapacity())
      reallocate(memories->getCapacity() << 1);
    memories->get(length++)->assign(value);
  }
}

Value *ListValue::pop() {
  if (length == 0)
    return nullptr;

  /// Poped value is unreacheable by this ListValue.
  /// So, it can be a target of GC
  auto popedMemory = memories->get(--length);
  auto *value = popedMemory->get();
  popedMemory->assign(nullptr);
  return value;
}

ValueMemory *ListValue::get(size_t index) const {
  if (index >= length)
    return nullptr;
  return memories->get(index);
}

void ValuePrintVisitor::visit(const ListValue &value) {
  printer << '[';
  for (auto [idx, mem] : llvm::enumerate(value.getMemories())) {
    auto *view = mem->get();
    printer << view->toString();
    if (idx != value.size() - 1)
      printer << ", ";
  }
  printer << ']';
}

void ToBoolVisitor::visit(const ListValue &value) { result = !value.empty(); }

void ValueEqVisitor::visit(const ListValue &l) {
  auto value = false;
  if (const auto *listR = r->dyn_cast<ListValue>())
    value = l.getVectorMemory() == listR->getVectorMemory();
  result = value;
}

//===----------------------------------------------------------------------===//
/// TupleValue
//===----------------------------------------------------------------------===//

TupleValue *TupleValue::create(MemoryContext *context, ArrayRef<Value *> mems) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto sizeToAlloc = totalSizeToAlloc<Value *>(mems.size());
  auto *mem =
      new (context->alloc(sizeToAlloc)) TupleValue(context, mems.size());
  std::uninitialized_move(mems.begin(), mems.end(),
                          mem->getTrailingObjects<Value *>());

  return mem;
}

void ValuePrintVisitor::visit(const TupleValue &value) {
  printer << '(';
  for (auto [idx, element] : llvm::enumerate(value.getValues())) {
    printer << element->toString();
    if (idx != value.getValues().size() - 1)
      printer << ", ";
  }
  if (value.size() == 1)
    printer << ',';
  printer << ')';
}

void ToBoolVisitor::visit(const TupleValue &value) { result = !value.empty(); }

void ValueEqVisitor::visit(const TupleValue &l) {
  auto value = false;
  if (const auto *tupleR = r->dyn_cast<TupleValue>()) {
    if (l.size() == tupleR->size()) {
      value = true;
      for (auto idx = 0; idx < l.size(); ++idx) {
        auto *lV = l.getElement(idx);
        auto *rV = tupleR->getElement(idx);
        if (lV->isEqual(rV)) {
          value = false;
          break;
        }
      }
    }
  }
  result = value;
}

//===----------------------------------------------------------------------===//
/// NilValue
//===----------------------------------------------------------------------===//

NilValue *NilValue::create(MemoryContext *context) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  return new (context->alloc(sizeof(NilValue))) NilValue(context);
}

void ValuePrintVisitor::visit(const NilValue &value) { printer << "nil"; }

void ToBoolVisitor::visit(const NilValue &value) { result = false; }

void ValueEqVisitor::visit(const NilValue &l) { result = r->isa<NilValue>(); }

//===----------------------------------------------------------------------===//
/// FunctionValue
//===----------------------------------------------------------------------===//

FunctionValue *FunctionValue::create(MemoryContext *context,
                                     const Environment &env,
                                     const FunctionDeclaration *decl) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto shallowCopy = env.shallowCopy();
  auto sizeToAlloc =
      totalSizeToAlloc<pair<StringRef, Memory *>>(shallowCopy.size());
  auto *mem = new (context->alloc(sizeToAlloc))
      FunctionValue(context, shallowCopy.size(), decl);
  std::uninitialized_copy(shallowCopy.begin(), shallowCopy.end(),
                          mem->getTrailingObjects<pair<StringRef, Memory *>>());
  return mem;
}

void ValuePrintVisitor::visit(const FunctionValue &value) {
  printer << "<function>";
  {
    ASTPrinter::AddIndentScope scope(printer);
    printer << value.getDeclaration()->toString();
  }
  printer.ln();
}

void ToBoolVisitor::visit(const FunctionValue &value) { result = nullopt; }

void ValueEqVisitor::visit(const FunctionValue &l) {
  auto value = false;
  if (const auto *funcR = r->dyn_cast<FunctionValue>())
    value = l.getDeclaration() == funcR->getDeclaration();
  result = value;
}

//===----------------------------------------------------------------------===//
/// LambdaValue
//===----------------------------------------------------------------------===//

LambdaValue *LambdaValue::create(MemoryContext *context, const Environment &env,
                                 const LambdaExpression *decl) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  auto capture = env.capture(context);
  auto sizeToAlloc =
      totalSizeToAlloc<pair<StringRef, Memory *>>(capture.size());
  auto *mem = new (context->alloc(sizeToAlloc))
      LambdaValue(context, capture.size(), decl);
  std::uninitialized_copy(capture.begin(), capture.end(),
                          mem->getTrailingObjects<pair<StringRef, Memory *>>());
  return mem;
}

void ValuePrintVisitor::visit(const LambdaValue &value) {
  printer << "<lambda>";
  {
    ASTPrinter::AddIndentScope scope(printer);
    printer << value.getExpression()->toString();
  }
  printer.ln();
}

void ToBoolVisitor::visit(const LambdaValue &value) { result = nullopt; }

void ValueEqVisitor::visit(const LambdaValue &l) {
  auto value = false;
  if (const auto *lambdaR = r->dyn_cast<LambdaValue>())
    value = l.getExpression() == lambdaR->getExpression();
  result = value;
}

//===----------------------------------------------------------------------===//
/// BuiltinFunctionValue
//===----------------------------------------------------------------------===//

BuiltinFunctionValue *BuiltinFunctionValue::create(MemoryContext *context,
                                                   StringRef name,
                                                   StringRef helpMsg,
                                                   funcBodyType fn) {
  assert(context->getGC()->isLocked() && "GC must be locked");
  return new (context->alloc(sizeof(BuiltinFunctionValue)))
      BuiltinFunctionValue(context, name, helpMsg, fn);
}

void ValuePrintVisitor::visit(const BuiltinFunctionValue &value) {
  printer << "<builtin function " << '\'' << value.getName() << "'>";
}

void ToBoolVisitor::visit(const BuiltinFunctionValue &value) {
  result = nullopt;
}

void ValueEqVisitor::visit(const BuiltinFunctionValue &l) {
  auto value = false;
  if (const auto *builtinR = r->dyn_cast<BuiltinFunctionValue>())
    value = l.getName() == builtinR->getName();
  result = value;
}
} // namespace bara
