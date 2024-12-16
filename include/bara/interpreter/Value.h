#ifndef BARA_VALUE_H
#define BARA_VALUE_H

#include "bara/ast/AST.h"
#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/Environment.h"
#include "bara/interpreter/Memory.h"
#include "bara/utils/VisitorBase.h"
#include "llvm/ADT/APFloat.h"

namespace bara {

using llvm::APFloat;

enum class ValueKind {
#define VALUE(Name) Name,
#include "bara/interpreter/Value.def"
  NUM
};

#define VALUE(Name) class Name##Value;
#include "bara/interpreter/Value.def"

namespace _inner {
template <typename T>
struct ValueKindMapper {};

#define VALUE(Name)                                                            \
  template <>                                                                  \
  struct ValueKindMapper<Name##Value> {                                        \
    static const ValueKind value = ValueKind::Name;                            \
  };
#include "bara/interpreter/Value.def"

} // namespace _inner

using ConstValueVisitor = utils::Visitor<Value, true>;
template <typename ConcreteType>
using ConstValueVisitorBase =
    utils::VisitorBase<ConcreteType, Value, true, _inner::ValueKindMapper
#define VALUE(Name) , Name##Value
#include "bara/interpreter/Value.def"
                       >;

class Value {
public:
  virtual ~Value() = default;

  using KindTy = ValueKind;

  template <typename... U>
  bool isa() const {
    return llvm::isa<U...>(this);
  }
  template <typename U>
  const U *cast() const {
    return llvm::cast<U>(this);
  }
  template <typename U>
  U *cast() {
    return llvm::cast<U>(this);
  }
  template <typename U>
  const U *dyn_cast() const {
    return llvm::dyn_cast<U>(this);
  }
  template <typename U>
  U *dyn_cast() {
    return llvm::dyn_cast<U>(this);
  }

  ValueKind getKind() const { return kind; }

  string toString() const;
  unique_ptr<Value> clone() const;
  optional<bool> toBool() const;
  bool isEqual(const Value *value) const;

  void accept(ConstValueVisitor &visitor) const;

protected:
  Value(ValueKind kind) : kind(kind) {}

private:
  ValueKind kind;
};

class IntegerValue final : public Value {
  IntegerValue(uint64_t value) : Value(ValueKind::Integer), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Integer;
  }

  static unique_ptr<IntegerValue> create(int64_t value);
  int64_t getValue() const { return value; }

private:
  int64_t value;
};

class BoolValue final : public Value {
  BoolValue(bool value) : Value(ValueKind::Bool), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Bool;
  }

  static unique_ptr<BoolValue> create(bool value);
  bool getValue() const { return value; }

private:
  bool value;
};

class FloatValue final : public Value {
  FloatValue(StringRef value)
      : Value(ValueKind::Float), value(APFloat::IEEEdouble(), value) {}
  FloatValue(const APFloat &value) : Value(ValueKind::Float), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Float;
  }

  static unique_ptr<FloatValue> create(StringRef value);
  static unique_ptr<FloatValue> create(const APFloat &value);
  const APFloat &getValue() const { return value; }

private:
  APFloat value;
};

class StringValue final : public Value {
  StringValue(StringRef value) : Value(ValueKind::String), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::String;
  }

  static unique_ptr<StringValue> create(StringRef value);
  StringRef getValue() const { return value; }

private:
  string value;
};

class ListValue final : public Value {
  ListValue(VectorMemory *memory) : Value(ValueKind::List), memory(memory) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::List;
  }

  static unique_ptr<ListValue>
  create(MemoryContext *context, MutableArrayRef<unique_ptr<Value>> values);

  static unique_ptr<ListValue> create(VectorMemory *memory);

  VectorMemory *getVectorMemory() const { return memory; }
  ValueMemory *getElement(size_t index) const { return memory->get(index); }
  size_t size() const { return memory->size(); }
  bool empty() const { return memory->empty(); }

private:
  VectorMemory *memory;
};

class TupleValue final : public Value {
  TupleValue(SmallVector<unique_ptr<Value>> mems)
      : Value(ValueKind::Tuple), mems(std::move(mems)) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Tuple;
  }

  static unique_ptr<TupleValue> create(SmallVector<unique_ptr<Value>> mems);

  ArrayRef<unique_ptr<Value>> getValues() const { return mems; }
  Value *getElement(size_t index) const { return mems[index].get(); }
  size_t size() const { return mems.size(); }
  bool empty() const { return mems.empty(); }

private:
  const SmallVector<unique_ptr<Value>> mems;
};

class NilValue final : public Value {
public:
  NilValue() : Value(ValueKind::Nil) {}

  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Nil;
  }

  static unique_ptr<NilValue> create();
};

class FunctionValue final : public Value {
  FunctionValue(const Environment &env, const FunctionDeclaration *decl)
      : Value(ValueKind::Function), env(env), decl(decl) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Function;
  }

  static unique_ptr<FunctionValue> create(const Environment &env,
                                          const FunctionDeclaration *decl);

  const Environment &getEnvironment() const { return env; }
  const FunctionDeclaration *getDeclaration() const { return decl; }

private:
  Environment env;
  const FunctionDeclaration *decl;
};

class LambdaValue final : public Value {
  LambdaValue(Environment env, const LambdaExpression *expr)
      : Value(ValueKind::Lambda), env(env), expr(expr) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Lambda;
  }

  static unique_ptr<LambdaValue> create(const Environment &env,
                                        const LambdaExpression *expr);

  const Environment &getEnvironment() const { return env; }
  const LambdaExpression *getExpression() const { return expr; }

private:
  Environment env;
  const LambdaExpression *expr;
};

class BuiltinFunctionValue final : public Value {
  using funcBodyType = llvm::function_ref<unique_ptr<Value>(
      ArrayRef<unique_ptr<Value>>, Diagnostic &, SMRange)>;

  BuiltinFunctionValue(StringRef name, StringRef helpMsg, funcBodyType func)
      : Value(ValueKind::BuiltinFunction), name(name), helpMsg(helpMsg),
        func(func) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::BuiltinFunction;
  }

  StringRef getName() const { return name; }
  StringRef getHelp() const { return helpMsg; }

  funcBodyType getFuncBody() const { return func; }

  static unique_ptr<BuiltinFunctionValue>
  create(StringRef name, StringRef helpMsg, funcBodyType func);

private:
  StringRef name;
  StringRef helpMsg;
  funcBodyType func;
};

} // namespace bara

#endif // BARA_VALUE_H
