#include "bara/context/GarbageCollector.h"
#include "bara/interpreter/Value.h"
#include "bara/utils/LLVM.h"
#include "llvm/ADT/APFloat.h"
#include "llvm/ADT/TypeSwitch.h"

namespace bara {

namespace {
static Value *null(const Value *) { return nullptr; }
} // namespace

template <typename ConcreteVisitor>
class BinaryOpVisitorImpl : public ConstValueVisitorBase<ConcreteVisitor> {
public:
  BinaryOpVisitorImpl(const Value *r) : r(r), result(nullptr) {}

  Value *getResult() { return std::move(result); }

protected:
  const Value *r;
  Value *result;
};

using ValueSwitch = llvm::TypeSwitch<const Value *, Value *>;

class AddVisitor : public BinaryOpVisitorImpl<AddVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

namespace {
static Value *add(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() + r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *add(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() + r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *add(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() + r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *add(const IntegerValue *l, const FloatValue *r) {
  auto apFloat = r->getValue();
  auto newValue =
      apFloat + llvm::APFloat(llvm::APFloat::IEEEdouble(), l->getValue());
  return FloatValue::create(l->getContext(), newValue);
}
static Value *add(const FloatValue *l, const FloatValue *r) {
  auto value = l->getValue() + r->getValue();
  return FloatValue::create(l->getContext(), value);
}
static Value *add(const StringValue *l, const StringValue *r) {
  auto value = l->getValue() + r->getValue();
  return StringValue::create(l->getContext(), value.str());
}
static Value *add(const ListValue *l, const ListValue *r) {
  auto *context = l->getContext();
  vector<Value *> newValues;
  newValues.reserve(l->size() + r->size());
  for (auto idx = 0; idx < l->size(); ++idx)
    newValues.emplace_back(l->get(idx)->get());
  for (auto idx = 0; idx < r->size(); ++idx)
    newValues.emplace_back(r->get(idx)->get());
  return ListValue::create(context, newValues);
}

} // namespace

void AddVisitor::visit(const IntegerValue &l) {
  result =
      ValueSwitch(r)
          .Case<IntegerValue>([&](const IntegerValue *r) { return add(&l, r); })
          .Case<BoolValue>([&](const BoolValue *r) { return add(&l, r); })
          .Case<FloatValue>([&](const FloatValue *r) { return add(&l, r); })
          .Default(null);
}
void AddVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return add(r, &l); })
               .Case([&](const BoolValue *r) { return add(&l, r); })
               .Default(null);
}
void AddVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return add(r, &l); })
               .Case([&](const FloatValue *r) { return add(&l, r); })
               .Default(null);
}
void AddVisitor::visit(const StringValue &l) {
  if (const auto *strR = r->dyn_cast<StringValue>())
    result = add(&l, strR);
  else
    result = nullptr;
}
void AddVisitor::visit(const ListValue &l) {
  if (const auto *listR = r->dyn_cast<ListValue>())
    result = add(&l, listR);
  else
    result = nullptr;
}
void AddVisitor::visit(const TupleValue &l) { result = nullptr; }
void AddVisitor::visit(const NilValue &l) { result = nullptr; }
void AddVisitor::visit(const FunctionValue &l) { result = nullptr; }
void AddVisitor::visit(const LambdaValue &l) { result = nullptr; }
void AddVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void AddVisitor::visit(const StructValue &l) { result = nullptr; }

class SubVisitor : public BinaryOpVisitorImpl<SubVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

void SubVisitor::visit(const IntegerValue &l) {
  result =
      ValueSwitch(r)
          .Case([&](const IntegerValue *r) {
            auto value = l.getValue() - r->getValue();
            return IntegerValue::create(l.getContext(), value);
          })
          .Case([&](const FloatValue *r) {
            auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
            return FloatValue::create(l.getContext(), floatL - r->getValue());
          })
          .Case([&](const BoolValue *r) {
            return IntegerValue::create(l.getContext(),
                                        l.getValue() - r->getValue());
          })
          .Default(null);
}
void SubVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() - r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Case([&](const BoolValue *r) {
                 return IntegerValue::create(l.getContext(),
                                             l.getValue() - r->getValue());
               })
               .Default(null);
}
void SubVisitor::visit(const FloatValue &l) {
  result =
      ValueSwitch(r)
          .Case([&](const IntegerValue *r) {
            auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
            return FloatValue::create(l.getContext(), l.getValue() - floatR);
          })
          .Case([&](const FloatValue *r) {
            auto value = l.getValue() - r->getValue();
            return FloatValue::create(l.getContext(), value);
          })
          .Default(null);
}
void SubVisitor::visit(const StringValue &l) { result = nullptr; }
void SubVisitor::visit(const ListValue &l) { result = nullptr; }
void SubVisitor::visit(const TupleValue &l) { result = nullptr; }
void SubVisitor::visit(const NilValue &l) { result = nullptr; }
void SubVisitor::visit(const FunctionValue &l) { result = nullptr; }
void SubVisitor::visit(const LambdaValue &l) { result = nullptr; }
void SubVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void SubVisitor::visit(const StructValue &l) { result = nullptr; }

class MulVisitor : public BinaryOpVisitorImpl<MulVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

namespace {
static Value *mul(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() * r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *mul(const IntegerValue *l, const FloatValue *r) {
  auto floatL = APFloat(APFloat::IEEEdouble(), l->getValue());
  return FloatValue::create(l->getContext(), floatL * r->getValue());
}
static Value *mul(const FloatValue *l, const FloatValue *r) {
  auto value = l->getValue() * r->getValue();
  return FloatValue::create(l->getContext(), value);
}
static Value *mul(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() * r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *mul(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() * r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *mul(const IntegerValue *l, const ListValue *r) {
  if (l->getValue() < 0)
    return nullptr;
  auto context = r->getContext();
  vector<Value *> newValues;
  auto newSize = l->getValue() * r->size();
  newValues.reserve(newSize);

  for (auto cnt = 0; cnt < l->getValue(); ++cnt) {
    for (auto idx = 0; idx < r->size(); ++idx) {
      newValues.emplace_back(r->get(idx)->get());
    }
  }

  return ListValue::create(context, newValues);
}
static Value *mul(const IntegerValue *l, const StringValue *r) {
  if (l->getValue() < 0)
    return nullptr;
  string newStr;
  newStr.reserve(l->getValue() * r->getValue().size());
  raw_string_ostream os(newStr);

  for (auto cnt = 0; cnt < l->getValue(); ++cnt)
    os << r->getValue();
  return StringValue::create(l->getContext(), os.str());
}
} // namespace

void MulVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return mul(&l, r); })
               .Case([&](const BoolValue *r) { return mul(&l, r); })
               .Case([&](const FloatValue *r) { return mul(&l, r); })
               .Case([&](const ListValue *r) { return mul(&l, r); })
               .Case([&](const StringValue *r) { return mul(&l, r); })
               .Default(null);
}
void MulVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return mul(r, &l); })
               .Case([&](const BoolValue *r) { return mul(&l, r); })
               .Default(null);
}
void MulVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return mul(r, &l); })
               .Case([&](const FloatValue *r) { return mul(&l, r); })
               .Default(null);
}
void MulVisitor::visit(const StringValue &l) {
  if (const IntegerValue *intV = r->dyn_cast<IntegerValue>())
    result = mul(intV, &l);
  else
    result = nullptr;
}
void MulVisitor::visit(const ListValue &l) {
  if (const IntegerValue *intV = r->dyn_cast<IntegerValue>())
    result = mul(intV, &l);
  else
    result = nullptr;
}
void MulVisitor::visit(const TupleValue &l) { result = nullptr; }
void MulVisitor::visit(const NilValue &l) { result = nullptr; }
void MulVisitor::visit(const FunctionValue &l) { result = nullptr; }
void MulVisitor::visit(const LambdaValue &l) { result = nullptr; }
void MulVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void MulVisitor::visit(const StructValue &l) { result = nullptr; }

class DivVisitor : public BinaryOpVisitorImpl<DivVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

void DivVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> Value * {
                 if (r->getValue() == 0)
                   return nullptr;
                 auto value = l.getValue() / r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 auto value = floatL / r->getValue();
                 return FloatValue::create(l.getContext(), value);
               })
               .Default(null);
}
void DivVisitor::visit(const BoolValue &l) { result = nullptr; }
void DivVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 auto value = l.getValue() / floatR;
                 return FloatValue::create(l.getContext(), value);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() / r->getValue();
                 return FloatValue::create(l.getContext(), value);
               })
               .Default(null);
}
void DivVisitor::visit(const StringValue &l) { result = nullptr; }
void DivVisitor::visit(const ListValue &l) { result = nullptr; }
void DivVisitor::visit(const TupleValue &l) { result = nullptr; }
void DivVisitor::visit(const NilValue &l) { result = nullptr; }
void DivVisitor::visit(const FunctionValue &l) { result = nullptr; }
void DivVisitor::visit(const LambdaValue &l) { result = nullptr; }
void DivVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void DivVisitor::visit(const StructValue &l) { result = nullptr; }

class ModVisitor : public BinaryOpVisitorImpl<ModVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

void ModVisitor::visit(const IntegerValue &l) {
  if (const IntegerValue *intR = r->dyn_cast<IntegerValue>()) {
    if (intR->getValue() == 0)
      result = nullptr;
    auto value = l.getValue() % intR->getValue();
    result = IntegerValue::create(l.getContext(), value);
  } else
    result = nullptr;
}

void ModVisitor::visit(const BoolValue &l) { result = nullptr; }
void ModVisitor::visit(const FloatValue &l) { result = nullptr; }
void ModVisitor::visit(const StringValue &l) { result = nullptr; }
void ModVisitor::visit(const ListValue &l) { result = nullptr; }
void ModVisitor::visit(const TupleValue &l) { result = nullptr; }
void ModVisitor::visit(const NilValue &l) { result = nullptr; }
void ModVisitor::visit(const FunctionValue &l) { result = nullptr; }
void ModVisitor::visit(const LambdaValue &l) { result = nullptr; }
void ModVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void ModVisitor::visit(const StructValue &l) { result = nullptr; }

namespace {

static int compare(const IntegerValue *l, const IntegerValue *r) {
  return l->getValue() - r->getValue();
}
static optional<int> compare(const IntegerValue *l, const FloatValue *r) {
  auto floatL = APFloat(APFloat::IEEEdouble(), l->getValue());
  auto cmp = floatL.compare(r->getValue());
  if (cmp == APFloat::cmpLessThan)
    return -1;
  if (cmp == APFloat::cmpGreaterThan)
    return 1;
  if (cmp == APFloat::cmpUnordered)
    return nullopt;
  return 0;
}
static optional<int> compare(const FloatValue *l, const IntegerValue *r) {
  auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
  auto cmpt = l->getValue().compare(floatR);
  if (cmpt == APFloat::cmpLessThan)
    return -1;
  if (cmpt == APFloat::cmpGreaterThan)
    return 1;
  if (cmpt == APFloat::cmpUnordered)
    return nullopt;
  return 0;
}
static optional<int> compare(const FloatValue *l, const FloatValue *r) {
  auto cmp = l->getValue().compare(r->getValue());
  if (cmp == APFloat::cmpLessThan)
    return -1;
  if (cmp == APFloat::cmpGreaterThan)
    return 1;
  if (cmp == APFloat::cmpUnordered)
    return nullopt;
  return 0;
}
static int compare(const StringValue *l, const StringValue *r) {
  return l->getValue().compare(r->getValue());
}

} // namespace

class Comparator : public ConstValueVisitorBase<Comparator> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"

  Comparator(const Value *r) : r(r) {}

  optional<int> getResult() { return result; }

  bool hasError() { return err; }

private:
  const Value *r;
  bool err = false;
  optional<int> result;
};

void Comparator::visit(const IntegerValue &l) {
  result = llvm::TypeSwitch<const Value *, optional<int>>(r)
               .Case([&](const IntegerValue *r) { return compare(&l, r); })
               .Case([&](const FloatValue *r) { return compare(&l, r); })
               .Default([&](const Value *) { return nullopt; });
}
void Comparator::visit(const BoolValue &l) { err = true; }
void Comparator::visit(const FloatValue &l) {
  result = llvm::TypeSwitch<const Value *, optional<int>>(r)
               .Case([&](const IntegerValue *r) { return compare(&l, r); })
               .Case([&](const FloatValue *r) { return compare(&l, r); })
               .Default([&](const Value *) { return nullopt; });
}
void Comparator::visit(const StringValue &l) {
  if (const auto *strR = r->dyn_cast<StringValue>())
    result = compare(&l, strR);
  else
    err = true;
}
void Comparator::visit(const ListValue &l) {
  if (const auto *listR = r->dyn_cast<ListValue>()) {
    auto lsize = l.size();
    auto rsize = listR->size();
    auto size = std::min(lsize, rsize);

    for (auto idx = 0; idx < size; ++idx) {
      Comparator cmp(listR->get(idx)->get());
      l.get(idx)->get()->accept(cmp);
      if (cmp.hasError()) {
        err = true;
        return;
      }
      auto cmpResult = cmp.getResult();
      if (!cmpResult || *cmpResult != 0) {
        result = cmpResult;
        return;
      }
    }
    result = lsize - rsize;
  } else
    err = true;
}
void Comparator::visit(const TupleValue &l) {
  if (const auto *tupleR = r->dyn_cast<TupleValue>()) {
    auto lsize = l.size();
    auto rsize = tupleR->size();
    auto size = std::min(lsize, rsize);

    for (auto idx = 0; idx < size; ++idx) {
      Comparator cmp(tupleR->getElement(idx));
      l.getElement(idx)->accept(cmp);
      if (cmp.hasError()) {
        err = true;
        return;
      }
      auto cmpResult = cmp.getResult();
      if (!cmpResult || *cmpResult != 0) {
        result = cmpResult;
        return;
      }
    }
    result = lsize - rsize;
  } else
    err = true;
}
void Comparator::visit(const NilValue &l) { err = true; }
void Comparator::visit(const FunctionValue &l) { err = true; }
void Comparator::visit(const LambdaValue &l) { err = true; }
void Comparator::visit(const BuiltinFunctionValue &l) { err = true; }
void Comparator::visit(const StructValue &l) { err = true; }

class BitAndVisitor : public BinaryOpVisitorImpl<BitAndVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

namespace {
static Value *bitAnd(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() & r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *bitAnd(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() & r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *bitAnd(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() & r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
} // namespace
void BitAndVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return bitAnd(&l, r); })
               .Case([&](const BoolValue *r) { return bitAnd(&l, r); })
               .Default(null);
}
void BitAndVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return bitAnd(r, &l); })
               .Case([&](const BoolValue *r) { return bitAnd(&l, r); })
               .Default(null);
}
void BitAndVisitor::visit(const FloatValue &l) { result = nullptr; }
void BitAndVisitor::visit(const StringValue &l) { result = nullptr; }
void BitAndVisitor::visit(const ListValue &l) { result = nullptr; }
void BitAndVisitor::visit(const TupleValue &l) { result = nullptr; }
void BitAndVisitor::visit(const NilValue &l) { result = nullptr; }
void BitAndVisitor::visit(const FunctionValue &l) { result = nullptr; }
void BitAndVisitor::visit(const LambdaValue &l) { result = nullptr; }
void BitAndVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void BitAndVisitor::visit(const StructValue &l) { result = nullptr; }

class BitOrVisitor : public BinaryOpVisitorImpl<BitOrVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
namespace {
static Value *bitOr(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() | r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *bitOr(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() | r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *bitOr(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() | r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
} // namespace

void BitOrVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return bitOr(&l, r); })
               .Case([&](const BoolValue *r) { return bitOr(&l, r); })
               .Default(null);
}
void BitOrVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return bitOr(r, &l); })
               .Case([&](const BoolValue *r) { return bitOr(&l, r); })
               .Default(null);
}
void BitOrVisitor::visit(const FloatValue &l) { result = nullptr; }
void BitOrVisitor::visit(const StringValue &l) { result = nullptr; }
void BitOrVisitor::visit(const ListValue &l) { result = nullptr; }
void BitOrVisitor::visit(const TupleValue &l) { result = nullptr; }
void BitOrVisitor::visit(const NilValue &l) { result = nullptr; }
void BitOrVisitor::visit(const FunctionValue &l) { result = nullptr; }
void BitOrVisitor::visit(const LambdaValue &l) { result = nullptr; }
void BitOrVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void BitOrVisitor::visit(const StructValue &l) { result = nullptr; }

class BitXorVisitor : public BinaryOpVisitorImpl<BitXorVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
namespace {
static Value *bitXor(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() ^ r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *bitXor(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() ^ r->getValue();
  return IntegerValue::create(l->getContext(), value);
}
static Value *bitXor(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() ^ r->getValue();
  return IntegerValue::create(l->getContext(), value);
}

} // namespace
void BitXorVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return bitXor(&l, r); })
               .Case([&](const BoolValue *r) { return bitXor(&l, r); })
               .Default(null);
}
void BitXorVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { return bitXor(r, &l); })
               .Case([&](const BoolValue *r) { return bitXor(&l, r); })
               .Default(null);
}
void BitXorVisitor::visit(const FloatValue &l) { result = nullptr; }
void BitXorVisitor::visit(const StringValue &l) { result = nullptr; }
void BitXorVisitor::visit(const ListValue &l) { result = nullptr; }
void BitXorVisitor::visit(const TupleValue &l) { result = nullptr; }
void BitXorVisitor::visit(const NilValue &l) { result = nullptr; }
void BitXorVisitor::visit(const FunctionValue &l) { result = nullptr; }
void BitXorVisitor::visit(const LambdaValue &l) { result = nullptr; }
void BitXorVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void BitXorVisitor::visit(const StructValue &l) { result = nullptr; }

class ShlVisitor : public BinaryOpVisitorImpl<ShlVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void ShlVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> Value * {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Default(null);
}
void ShlVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> Value * {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Default(null);
}
void ShlVisitor::visit(const FloatValue &l) { result = nullptr; }
void ShlVisitor::visit(const StringValue &l) { result = nullptr; }
void ShlVisitor::visit(const ListValue &l) { result = nullptr; }
void ShlVisitor::visit(const TupleValue &l) { result = nullptr; }
void ShlVisitor::visit(const NilValue &l) { result = nullptr; }
void ShlVisitor::visit(const FunctionValue &l) { result = nullptr; }
void ShlVisitor::visit(const LambdaValue &l) { result = nullptr; }
void ShlVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void ShlVisitor::visit(const StructValue &l) { result = nullptr; }

class ShrVisitor : public BinaryOpVisitorImpl<ShrVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void ShrVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> Value * {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Default(null);
}
void ShrVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> Value * {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(l.getContext(), value);
               })
               .Default(null);
}
void ShrVisitor::visit(const FloatValue &l) { result = nullptr; }
void ShrVisitor::visit(const StringValue &l) { result = nullptr; }
void ShrVisitor::visit(const ListValue &l) { result = nullptr; }
void ShrVisitor::visit(const TupleValue &l) { result = nullptr; }
void ShrVisitor::visit(const NilValue &l) { result = nullptr; }
void ShrVisitor::visit(const FunctionValue &l) { result = nullptr; }
void ShrVisitor::visit(const LambdaValue &l) { result = nullptr; }
void ShrVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }
void ShrVisitor::visit(const StructValue &l) { result = nullptr; }

namespace BinaryOp {
#define BINARY_FUNC(funcName, VisitorName)                                     \
  Value *funcName(const Value *l, const Value *r) {                            \
    VisitorName visitor(r);                                                    \
    l->accept(visitor);                                                        \
    return visitor.getResult();                                                \
  }
BINARY_FUNC(add, AddVisitor)
BINARY_FUNC(sub, SubVisitor)
BINARY_FUNC(mul, MulVisitor)
BINARY_FUNC(div, DivVisitor)
BINARY_FUNC(mod, ModVisitor)
BINARY_FUNC(bitAnd, BitAndVisitor)
BINARY_FUNC(bitOr, BitOrVisitor)
BINARY_FUNC(bitXor, BitXorVisitor)
BINARY_FUNC(shl, ShlVisitor)
BINARY_FUNC(shr, ShrVisitor)

#undef BINARY_FUNC

#define COMPARE_FUNC(funcName, op)                                             \
  Value *funcName(const Value *l, const Value *r) {                            \
    Comparator cmp(r);                                                         \
    GCSAFE(l->getContext()->getGC()) { l->accept(cmp); }                       \
    if (cmp.hasError())                                                        \
      return nullptr;                                                          \
    auto result = cmp.getResult();                                             \
    if (!result)                                                               \
      return BoolValue::create(l->getContext(), false);                        \
    return BoolValue::create(l->getContext(), *result op 0);                   \
  }

COMPARE_FUNC(gt, >)
COMPARE_FUNC(ge, >=)
COMPARE_FUNC(lt, <)
COMPARE_FUNC(le, <=)

#undef COMPARE_FUNC

} // namespace BinaryOp
} // namespace bara
