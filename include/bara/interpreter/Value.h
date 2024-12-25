#ifndef BARA_VALUE_H
#define BARA_VALUE_H

#include "bara/ast/AST.h"
#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/Environment.h"
#include "bara/interpreter/Memory.h"
#include "bara/interpreter/ValueDeleter.h"
#include "bara/utils/VisitorBase.h"
#include "llvm/ADT/APFloat.h"
#include <llvm-18/llvm/Support/TrailingObjects.h>

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
using ValueVisitor = utils::Visitor<Value, false>;
template <typename ConcreteType>
using ValueVisitorBase =
    utils::VisitorBase<ConcreteType, Value, false, _inner::ValueKindMapper
#define VALUE(Name) , Name##Value
#include "bara/interpreter/Value.def"
                       >;

class Value {
public:
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
  UniqueValue<Value> clone() const;
  optional<bool> toBool() const;
  bool isEqual(const Value *value) const;

  void accept(ConstValueVisitor &visitor) const;
  void accept(ValueVisitor &visitor);

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

  static UniqueValue<IntegerValue> create(int64_t value);
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

  static UniqueValue<BoolValue> create(bool value);
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

  static UniqueValue<FloatValue> create(StringRef value);
  static UniqueValue<FloatValue> create(const APFloat &value);
  const APFloat &getValue() const { return value; }

private:
  APFloat value;
};

class StringValue final : public Value,
                          public TrailingObjects<StringValue, char> {
  StringValue(size_t length) : Value(ValueKind::String), length(length) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::String;
  }

  static UniqueValue<StringValue> create(StringRef value);
  StringRef getValue() const { return {getTrailingObjects<char>(), length}; }
  size_t size() const { return length; }

private:
  size_t length;
};

class ListValue final : public Value {
  ListValue(VectorMemory *memory) : Value(ValueKind::List), memory(memory) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::List;
  }

  static UniqueValue<ListValue>
  create(MemoryContext *context, MutableArrayRef<UniqueValue<Value>> values);

  static UniqueValue<ListValue> create(VectorMemory *memory);

  VectorMemory *getVectorMemory() const { return memory; }
  ValueMemory *getElement(size_t index) const { return memory->get(index); }
  size_t size() const { return memory->size(); }
  bool empty() const { return memory->empty(); }

private:
  VectorMemory *memory;
};

class TupleValue final
    : public Value,
      public TrailingObjects<TupleValue, UniqueValue<Value>> {
  friend class ValueEraser;
  TupleValue(size_t length) : Value(ValueKind::Tuple), length(length) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Tuple;
  }

  static UniqueValue<TupleValue> create(SmallVector<UniqueValue<Value>> mems);

  ArrayRef<UniqueValue<Value>> getValues() const {
    return {getTrailingObjects<UniqueValue<Value>>(), length};
  }
  Value *getElement(size_t index) const { return getValues()[index].get(); }
  size_t size() const { return length; }
  bool empty() const { return length == 0; }

private:
  size_t length;
};

class NilValue final : public Value {
public:
  NilValue() : Value(ValueKind::Nil) {}

  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Nil;
  }

  static UniqueValue<NilValue> create();
};

class FunctionValue final : public Value {
  friend class ValueEraser;
  FunctionValue(const Environment &env, const FunctionDeclaration *decl)
      : Value(ValueKind::Function), env(env), decl(decl) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Function;
  }

  static UniqueValue<FunctionValue> create(const Environment &env,
                                           const FunctionDeclaration *decl);

  const Environment &getEnvironment() const { return env; }
  const FunctionDeclaration *getDeclaration() const { return decl; }

private:
  Environment env;
  const FunctionDeclaration *decl;
};

class LambdaValue final : public Value {
  friend class ValueEraser;
  LambdaValue(Environment env, const LambdaExpression *expr)
      : Value(ValueKind::Lambda), env(env), expr(expr) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Lambda;
  }

  static UniqueValue<LambdaValue> create(const Environment &env,
                                         const LambdaExpression *expr);

  const Environment &getEnvironment() const { return env; }
  const LambdaExpression *getExpression() const { return expr; }

private:
  Environment env;
  const LambdaExpression *expr;
};

class BuiltinFunctionValue final : public Value {
  using funcBodyType = llvm::function_ref<UniqueValue<Value>(
      ArrayRef<UniqueValue<Value>>, Diagnostic &, SMRange, MemoryContext *)>;

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

  static UniqueValue<BuiltinFunctionValue>
  create(StringRef name, StringRef helpMsg, funcBodyType func);

private:
  StringRef name;
  StringRef helpMsg;
  funcBodyType func;
};

} // namespace bara

#endif // BARA_VALUE_H
