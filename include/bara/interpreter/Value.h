#ifndef BARA_VALUE_H
#define BARA_VALUE_H

#include "bara/ast/AST.h"
#include "bara/context/GarbageCollector.h"
#include "bara/context/MemoryContext.h"
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
using ValueVisitor = utils::Visitor<Value, false>;
template <typename ConcreteType>
using ValueVisitorBase =
    utils::VisitorBase<ConcreteType, Value, false, _inner::ValueKindMapper
#define VALUE(Name) , Name##Value
#include "bara/interpreter/Value.def"
                       >;

class Value : public GCTarget {
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
  optional<bool> toBool() const;
  bool isEqual(const Value *value) const;

  void accept(ConstValueVisitor &visitor) const;
  void accept(ValueVisitor &visitor);

  MemoryContext *getContext() const { return context; }

protected:
  Value(MemoryContext *context, ValueKind kind)
      : context(context), kind(kind) {}

private:
  MemoryContext *context;
  ValueKind kind;
};

class IntegerValue final : public Value {
  IntegerValue(MemoryContext *context, uint64_t value)
      : Value(context, ValueKind::Integer), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Integer;
  }

  static IntegerValue *create(MemoryContext *ctx, int64_t value);
  int64_t getValue() const { return value; }

private:
  int64_t value;
};

class BoolValue final : public Value {
  BoolValue(MemoryContext *context, bool value)
      : Value(context, ValueKind::Bool), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Bool;
  }

  static BoolValue *create(MemoryContext *ctx, bool value);
  bool getValue() const { return value; }

private:
  bool value;
};

class FloatValue final : public Value {
  FloatValue(MemoryContext *context, StringRef value)
      : Value(context, ValueKind::Float), value(APFloat::IEEEdouble(), value) {}
  FloatValue(MemoryContext *context, const APFloat &value)
      : Value(context, ValueKind::Float), value(value) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Float;
  }

  static FloatValue *create(MemoryContext *ctx, StringRef value);
  static FloatValue *create(MemoryContext *ctx, const APFloat &value);
  const APFloat &getValue() const { return value; }

private:
  APFloat value;
};

class StringValue final : public Value,
                          public TrailingObjects<StringValue, char> {
  StringValue(MemoryContext *context, size_t length)
      : Value(context, ValueKind::String), length(length) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::String;
  }

  static StringValue *create(MemoryContext *ctx, StringRef value);
  StringRef getValue() const { return {getTrailingObjects<char>(), length}; }
  size_t size() const { return length; }

private:
  size_t length;
};

class ListValue final : public Value {
  ListValue(MemoryContext *context, size_t length, VectorMemory *memories)
      : Value(context, ValueKind::List), length(length), memories(memories) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::List;
  }

  void reallocate(size_t newCapacity);
  void push(Value *value);
  Value *pop();
  ValueMemory *get(size_t index) const;
  size_t size() const { return length; }
  bool empty() const { return length == 0; }
  VectorMemory *getVectorMemory() const { return memories; }
  ArrayRef<ValueMemory *> getMemories() const {
    return memories->getMemories().slice(0, length);
  }

  static ListValue *create(MemoryContext *context, ArrayRef<Value *> values);

private:
  size_t length;
  VectorMemory *memories;
};

class TupleValue final : public Value,
                         public TrailingObjects<TupleValue, Value *> {
  friend class ValueEraser;
  TupleValue(MemoryContext *context, size_t length)
      : Value(context, ValueKind::Tuple), length(length) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Tuple;
  }

  static TupleValue *create(MemoryContext *ctx, ArrayRef<Value *> mems);

  ArrayRef<Value *> getValues() const {
    return {getTrailingObjects<Value *>(), length};
  }
  Value *getElement(size_t index) const { return getValues()[index]; }
  size_t size() const { return length; }
  bool empty() const { return length == 0; }

private:
  size_t length;
};

class NilValue final : public Value {
public:
  NilValue(MemoryContext *context) : Value(context, ValueKind::Nil) {}

  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Nil;
  }

  static NilValue *create(MemoryContext *ctx);
};

class FunctionValue final
    : public Value,
      public TrailingObjects<FunctionValue, pair<StringRef, Memory *>> {
  friend class ValueEraser;
  FunctionValue(MemoryContext *context, size_t envVarLength,
                const FunctionDeclaration *decl)
      : Value(context, ValueKind::Function), envVarLength(envVarLength),
        decl(decl) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Function;
  }

  static FunctionValue *create(MemoryContext *ctx, const Environment &env,
                               const FunctionDeclaration *decl);

  const FunctionDeclaration *getDeclaration() const { return decl; }
  ArrayRef<pair<StringRef, Memory *>> getEnvVars() const {
    return {getTrailingObjects<pair<StringRef, Memory *>>(), envVarLength};
  }

private:
  size_t envVarLength;
  const FunctionDeclaration *decl;
};

class LambdaValue final
    : public Value,
      public TrailingObjects<LambdaValue, pair<StringRef, Memory *>> {
  friend class ValueEraser;
  LambdaValue(MemoryContext *context, size_t captureLength,
              const LambdaExpression *expr)
      : Value(context, ValueKind::Lambda), captureLength(captureLength),
        expr(expr) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::Lambda;
  }

  static LambdaValue *create(MemoryContext *ctx, const Environment &env,
                             const LambdaExpression *expr);

  const LambdaExpression *getExpression() const { return expr; }
  ArrayRef<pair<StringRef, Memory *>> getCaptures() const {
    return {getTrailingObjects<pair<StringRef, Memory *>>(), captureLength};
  }

private:
  size_t captureLength;
  const LambdaExpression *expr;
};

class BuiltinFunctionValue final : public Value {
  using funcBodyType = llvm::function_ref<Value *(
      ArrayRef<Value *>, Diagnostic &, SMRange, MemoryContext *)>;

  BuiltinFunctionValue(MemoryContext *context, StringRef name,
                       StringRef helpMsg, funcBodyType func)
      : Value(context, ValueKind::BuiltinFunction), name(name),
        helpMsg(helpMsg), func(func) {}

public:
  static bool classof(const Value *value) {
    return value->getKind() == ValueKind::BuiltinFunction;
  }

  StringRef getName() const { return name; }
  StringRef getHelp() const { return helpMsg; }

  funcBodyType getFuncBody() const { return func; }

  static BuiltinFunctionValue *create(MemoryContext *ctx, StringRef name,
                                      StringRef helpMsg, funcBodyType func);

private:
  StringRef name;
  StringRef helpMsg;
  funcBodyType func;
};

} // namespace bara

#endif // BARA_VALUE_H
