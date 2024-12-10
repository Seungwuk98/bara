#ifndef BARA_VALUE_H
#define BARA_VALUE_H

#include "bara/ast/AST.h"
#include "bara/interpreter/Environment.h"
#include "bara/interpreter/Memory.h"
#include "bara/utils/VisitorBase.h"
#include "llvm/ADT/APFloat.h"

namespace bara {

using llvm::APFloat;

enum class ValueKind {
#define VALUE(Name) Name,
#include "bara/interpreter/Value.def"
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
  ValueKind getKind() const { return kind; }

  string toString() const;
  unique_ptr<Value> clone() const;
  optional<bool> toBool() const;

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
  uint64_t getValue() const { return value; }

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

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Float;
  }

  static unique_ptr<FloatValue> create(StringRef value);
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

  VectorMemory *getVectorMemory() { return memory; }
  Memory *getElement(size_t index) { return memory->get(index); }

private:
  VectorMemory *memory;
};

class TupleValue final : public Value {
  TupleValue(ArrayRef<Memory *> mems) : Value(ValueKind::Tuple), mems(mems) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Tuple;
  }

  static unique_ptr<TupleValue> create(ArrayRef<Memory *> mems);

  ArrayRef<Memory *> getMems() const { return mems; }
  Memory *getElement(size_t index) const { return mems[index]; }

private:
  const vector<Memory *> mems;
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
  FunctionValue(const Environment &env, FunctionDeclaration *decl)
      : Value(ValueKind::Function), env(env), decl(decl) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Function;
  }

  static unique_ptr<FunctionValue> create(const Environment &env,
                                          FunctionDeclaration *decl);

  Environment &getEnvironment() { return env; }
  FunctionDeclaration *getDeclaration() { return decl; }

private:
  Environment env;
  FunctionDeclaration *decl;
};

class LambdaValue final : public Value {
  LambdaValue(Environment env, LambdaExpression *expr)
      : Value(ValueKind::Lambda), env(env), expr(expr) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Lambda;
  }

  static unique_ptr<LambdaValue> create(const Environment &env,
                                        LambdaExpression *expr);

  Environment &getEnvironment() { return env; }
  LambdaExpression *getExpression() { return expr; }

private:
  Environment env;
  LambdaExpression *expr;
};

} // namespace bara

#endif // BARA_VALUE_H
