#include "bara/interpreter/Value.h"
#include "bara/ast/AST.h"
#include "llvm/ADT/SmallVectorExtras.h"
#include "llvm/Support/ManagedStatic.h"

namespace bara {

class ValuePrintVisitor : public ConstValueVisitorBase<ValuePrintVisitor> {
public:
  void setPrinter(ASTPrinter *printer) { this->printer = printer; }

#define VALUE(Name) void visit(const Name##Value &value);
#include "bara/interpreter/Value.def"

private:
  ASTPrinter *printer;
};

llvm::ManagedStatic<ValuePrintVisitor> printVisitor;

class ToBoolVisitor : public ConstValueVisitorBase<ToBoolVisitor> {
public:
  ToBoolVisitor() : value(nullptr), result(nullopt) {}

  void init(const Value *value) {
    this->value = value;
    result = nullopt;
  }

  std::optional<bool> getResult() const { return result; }

#define VALUE(Name) void visit(const Name##Value &value);
#include "bara/interpreter/Value.def"

private:
  const Value *value;
  optional<bool> result;
};

llvm::ManagedStatic<ToBoolVisitor> toBoolVisitor;

string Value::toString() const {
  string str;
  raw_string_ostream os(str);
  ASTPrinter printer(os);
  printVisitor->setPrinter(&printer);
  accept(*printVisitor);
  return os.str();
}

optional<bool> Value::toBool() const {
  toBoolVisitor->init(this);
  accept(*toBoolVisitor);
  return toBoolVisitor->getResult();
}

unique_ptr<IntegerValue> IntegerValue::create(int64_t value) {
  auto *mem = new IntegerValue(value);
  return unique_ptr<IntegerValue>(mem);
}

void ValuePrintVisitor::visit(const IntegerValue &value) {
  *printer << value.getValue();
}

void ToBoolVisitor::visit(const IntegerValue &value) {
  result = value.getValue() != 0;
}

unique_ptr<BoolValue> BoolValue::create(bool value) {
  auto *mem = new BoolValue(value);
  return unique_ptr<BoolValue>(mem);
}

void ToBoolVisitor::visit(const BoolValue &value) { result = value.getValue(); }

void ValuePrintVisitor::visit(const BoolValue &value) {
  *printer << (value.getValue() ? "true" : "false");
}

unique_ptr<FloatValue> FloatValue::create(StringRef value) {
  auto *mem = new FloatValue(value);
  return unique_ptr<FloatValue>(mem);
}

void ValuePrintVisitor::visit(const FloatValue &value) {
  SmallVector<char> buffer;
  value.getValue().toString(buffer);
  *printer << buffer;
}

void ToBoolVisitor::visit(const FloatValue &value) {
  result = value.getValue().convertToDouble() != 0.0;
}

unique_ptr<StringValue> StringValue::create(StringRef value) {
  auto *mem = new StringValue(value);
  return unique_ptr<StringValue>(mem);
}

void ValuePrintVisitor::visit(const StringValue &value) {
  *printer << '"' << value.getValue() << '"';
}

void ToBoolVisitor::visit(const StringValue &value) {
  result = !value.getValue().empty();
}

unique_ptr<ListValue>
ListValue::create(MemoryContext *context,
                  MutableArrayRef<unique_ptr<Value>> value) {
  auto valueMemorys =
      llvm::map_to_vector(value, [context](unique_ptr<Value> &value) {
        return ValueMemory::create(context, std::move(value));
      });

  auto *vectorMemory = VectorMemory::create(context, valueMemorys);

  return unique_ptr<ListValue>(new ListValue(vectorMemory));
}

void ValuePrintVisitor::visit(const ListValue &value) {
  *printer << '[';
  for (auto [idx, mem] : llvm::enumerate(*value.getVectorMemory())) {
    auto *view = mem->view();
    *printer << view->toString();
    if (idx != value.size() - 1)
      *printer << ", ";
  }
  *printer << ']';
}

void ToBoolVisitor::visit(const ListValue &value) { result = !value.empty(); }

unique_ptr<TupleValue> TupleValue::create(ArrayRef<ValueMemory *> mems) {
  auto *mem = new TupleValue(mems);
  return unique_ptr<TupleValue>(mem);
}

void ValuePrintVisitor::visit(const TupleValue &value) {
  *printer << '(';
  for (auto [idx, mem] : llvm::enumerate(value.getMemories())) {
    auto *view = mem->view();
    *printer << view->toString();
    if (idx != value.getMemories().size() - 1)
      *printer << ", ";
  }
  if (value.size() == 1)
    *printer << ',';
  *printer << ')';
}

void ToBoolVisitor::visit(const TupleValue &value) { result = !value.empty(); }

unique_ptr<NilValue> NilValue::create() {
  auto *mem = new NilValue();
  return unique_ptr<NilValue>(mem);
}

void ValuePrintVisitor::visit(const NilValue &value) { *printer << "nil"; }

void ToBoolVisitor::visit(const NilValue &value) { result = false; }

unique_ptr<FunctionValue> FunctionValue::create(const Environment &env,
                                                FunctionDeclaration *decl) {
  auto *mem = new FunctionValue(env, decl);
  return unique_ptr<FunctionValue>(mem);
}

void ValuePrintVisitor::visit(const FunctionValue &value) {
  *printer << "<function>";
  {
    ASTPrinter::AddIndentScope scope(*printer);
    *printer << value.getDeclaration()->toString();
  }
  printer->ln();
}

void ToBoolVisitor::visit(const FunctionValue &value) { result = nullopt; }

unique_ptr<LambdaValue> LambdaValue::create(const Environment &env,
                                            LambdaExpression *decl) {
  auto *mem = new LambdaValue(env, decl);
  return unique_ptr<LambdaValue>(mem);
}

void ValuePrintVisitor::visit(const LambdaValue &value) {
  *printer << "<lambda>";
  {
    ASTPrinter::AddIndentScope scope(*printer);
    *printer << value.getExpression()->toString();
  }
  printer->ln();
}

void ToBoolVisitor::visit(const LambdaValue &value) { result = nullopt; }

} // namespace bara
