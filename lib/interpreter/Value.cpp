#include "bara/interpreter/Value.h"
#include "bara/ast/AST.h"
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

class CloneVisitor : public ConstValueVisitorBase<CloneVisitor> {
public:
  CloneVisitor() : result(nullptr) {}

  void init() { result = nullptr; }

#define VALUE(Name) void visit(const Name##Value &value);
#include "bara/interpreter/Value.def"

  UniqueValue<Value> getResult() { return std::move(result); }

private:
  UniqueValue<Value> result;
};

llvm::ManagedStatic<CloneVisitor> cloneVisitor;

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

UniqueValue<Value> Value::clone() const {
  cloneVisitor->init();
  accept(*cloneVisitor);
  return cloneVisitor->getResult();
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

UniqueValue<IntegerValue> IntegerValue::create(int64_t value) {
  auto *mem = new (malloc(sizeof(IntegerValue))) IntegerValue(value);
  return UniqueValue<IntegerValue>(mem);
}

void ValuePrintVisitor::visit(const IntegerValue &value) {
  printer << value.getValue();
}

void ToBoolVisitor::visit(const IntegerValue &value) {
  result = value.getValue() != 0;
}

void CloneVisitor::visit(const IntegerValue &value) {
  result = IntegerValue::create(value.getValue());
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

UniqueValue<BoolValue> BoolValue::create(bool value) {
  auto *mem = new (malloc(sizeof(BoolValue))) BoolValue(value);
  return UniqueValue<BoolValue>(mem);
}

void ToBoolVisitor::visit(const BoolValue &value) { result = value.getValue(); }

void ValuePrintVisitor::visit(const BoolValue &value) {
  printer << (value.getValue() ? "true" : "false");
}

void CloneVisitor::visit(const BoolValue &value) {
  result = BoolValue::create(value.getValue());
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

UniqueValue<FloatValue> FloatValue::create(StringRef value) {
  auto *mem = new (malloc(sizeof(FloatValue))) FloatValue(value);
  return UniqueValue<FloatValue>(mem);
}

UniqueValue<FloatValue> FloatValue::create(const APFloat &value) {
  auto *mem = new (malloc(sizeof(FloatValue))) FloatValue(value);
  return UniqueValue<FloatValue>(mem);
}

void ValuePrintVisitor::visit(const FloatValue &value) {
  SmallVector<char> buffer;
  value.getValue().toString(buffer);
  printer << buffer;
}

void ToBoolVisitor::visit(const FloatValue &value) {
  result = value.getValue().convertToDouble() != 0.0;
}

void CloneVisitor::visit(const FloatValue &value) {
  SmallVector<char> buffer;
  value.getValue().toString(buffer);
  result = FloatValue::create(StringRef{buffer.data(), buffer.size()});
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

UniqueValue<StringValue> StringValue::create(StringRef value) {
  auto sizeToAlloc = totalSizeToAlloc<char>(value.size());
  auto *stringValue = new (malloc(sizeToAlloc)) StringValue(value.size());
  std::uninitialized_copy(value.begin(), value.end(),
                          stringValue->getTrailingObjects<char>());
  return UniqueValue<StringValue>(stringValue);
}

void ValuePrintVisitor::visit(const StringValue &value) {
  printer << value.getValue();
}

void ToBoolVisitor::visit(const StringValue &value) {
  result = !value.getValue().empty();
}

void CloneVisitor::visit(const StringValue &value) {
  result = StringValue::create(value.getValue());
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

UniqueValue<ListValue>
ListValue::create(MemoryContext *context,
                  MutableArrayRef<UniqueValue<Value>> value) {
  auto valueMemorys =
      llvm::map_to_vector(value, [context](UniqueValue<Value> &value) {
        return ValueMemory::create(context, std::move(value));
      });

  auto *vectorMemory = VectorMemory::create(context, valueMemorys);

  return UniqueValue<ListValue>(new (malloc(sizeof(ListValue)))
                                    ListValue(vectorMemory));
}

UniqueValue<ListValue> ListValue::create(VectorMemory *memory) {
  auto *mem = new (malloc(sizeof(ListValue))) ListValue(memory);
  return UniqueValue<ListValue>(mem);
}

void ValuePrintVisitor::visit(const ListValue &value) {
  printer << '[';
  for (auto [idx, mem] : llvm::enumerate(*value.getVectorMemory())) {
    auto *view = mem->view();
    printer << view->toString();
    if (idx != value.size() - 1)
      printer << ", ";
  }
  printer << ']';
}

void ToBoolVisitor::visit(const ListValue &value) { result = !value.empty(); }

void CloneVisitor::visit(const ListValue &value) {
  result = ListValue::create(value.getVectorMemory());
}

void ValueEqVisitor::visit(const ListValue &l) {
  auto value = false;
  if (const auto *listR = r->dyn_cast<ListValue>())
    value = l.getVectorMemory() == listR->getVectorMemory();
  result = value;
}

//===----------------------------------------------------------------------===//
/// TupleValue
//===----------------------------------------------------------------------===//

UniqueValue<TupleValue>
TupleValue::create(SmallVector<UniqueValue<Value>> mems) {
  auto sizeToAlloc = totalSizeToAlloc<UniqueValue<Value>>(mems.size());
  auto *mem = new (malloc(sizeToAlloc)) TupleValue(mems.size());
  std::uninitialized_move(mems.begin(), mems.end(),
                          mem->getTrailingObjects<UniqueValue<Value>>());

  return UniqueValue<TupleValue>(mem);
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

void CloneVisitor::visit(const TupleValue &value) {
  auto values = llvm::map_to_vector(
      value.getValues(), [](const auto &element) { return element->clone(); });
  result = TupleValue::create(std::move(values));
}

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

UniqueValue<NilValue> NilValue::create() {
  auto *mem = new (malloc(sizeof(NilValue))) NilValue();
  return UniqueValue<NilValue>(mem);
}

void ValuePrintVisitor::visit(const NilValue &value) { printer << "nil"; }

void ToBoolVisitor::visit(const NilValue &value) { result = false; }

void CloneVisitor::visit(const NilValue &value) { result = NilValue::create(); }

void ValueEqVisitor::visit(const NilValue &l) { result = r->isa<NilValue>(); }

//===----------------------------------------------------------------------===//
/// FunctionValue
//===----------------------------------------------------------------------===//

UniqueValue<FunctionValue>
FunctionValue::create(const Environment &env, const FunctionDeclaration *decl) {
  auto *mem = new (malloc(sizeof(FunctionValue))) FunctionValue(env, decl);
  return UniqueValue<FunctionValue>(mem);
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

void CloneVisitor::visit(const FunctionValue &value) {
  result =
      FunctionValue::create(value.getEnvironment(), value.getDeclaration());
}

void ValueEqVisitor::visit(const FunctionValue &l) {
  auto value = false;
  if (const auto *funcR = r->dyn_cast<FunctionValue>())
    value = l.getDeclaration() == funcR->getDeclaration();
  result = value;
}

//===----------------------------------------------------------------------===//
/// LambdaValue
//===----------------------------------------------------------------------===//

UniqueValue<LambdaValue> LambdaValue::create(const Environment &env,
                                             const LambdaExpression *decl) {
  auto *mem = new (malloc(sizeof(LambdaValue))) LambdaValue(env, decl);
  return UniqueValue<LambdaValue>(mem);
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

void CloneVisitor::visit(const LambdaValue &value) {
  result = LambdaValue::create(value.getEnvironment(), value.getExpression());
}

void ValueEqVisitor::visit(const LambdaValue &l) {
  auto value = false;
  if (const auto *lambdaR = r->dyn_cast<LambdaValue>())
    value = l.getExpression() == lambdaR->getExpression();
  result = value;
}

//===----------------------------------------------------------------------===//
/// BuiltinFunctionValue
//===----------------------------------------------------------------------===//

UniqueValue<BuiltinFunctionValue>
BuiltinFunctionValue::create(StringRef name, StringRef helpMsg,
                             funcBodyType fn) {
  auto *mem = new (malloc(sizeof(BuiltinFunctionValue)))
      BuiltinFunctionValue(name, helpMsg, fn);
  return UniqueValue<BuiltinFunctionValue>(mem);
}

void ValuePrintVisitor::visit(const BuiltinFunctionValue &value) {
  printer << "<builtin function" << '\'' << value.getName() << "'>";
}

void ToBoolVisitor::visit(const BuiltinFunctionValue &value) {
  result = nullopt;
}

void CloneVisitor::visit(const BuiltinFunctionValue &value) {
  result = BuiltinFunctionValue::create(value.getName(), value.getHelp(),
                                        value.getFuncBody());
}

void ValueEqVisitor::visit(const BuiltinFunctionValue &l) {
  auto value = false;
  if (const auto *builtinR = r->dyn_cast<BuiltinFunctionValue>())
    value = l.getName() == builtinR->getName();
  result = value;
}
} // namespace bara
