#include "bara/interpreter/Value.h"
#include "bara/utils/LLVM.h"
#include "llvm/ADT/TypeSwitch.h"
#include <llvm-18/llvm/ADT/APFloat.h>

namespace bara {

namespace {
static unique_ptr<Value> null(const Value *) { return nullptr; }
} // namespace

template <typename ConcreteVisitor>
class BinaryOpVisitorImpl : public ConstValueVisitorBase<ConcreteVisitor> {
public:
  void init(const Value *r) {
    this->r = r;
    result.reset();
  }

  std::unique_ptr<Value> getResult() { return std::move(result); }

protected:
  const Value *r;
  std::unique_ptr<Value> result;
};

using ValueSwitch = llvm::TypeSwitch<const Value *, unique_ptr<Value>>;

class AddVisitor : public BinaryOpVisitorImpl<AddVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

namespace {
static unique_ptr<Value> add(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() + r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> add(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() + r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> add(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() + r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> add(const IntegerValue *l, const FloatValue *r) {
  auto apFloat = r->getValue();
  auto newValue =
      apFloat + llvm::APFloat(llvm::APFloat::IEEEdouble(), l->getValue());
  return FloatValue::create(newValue);
}
static unique_ptr<Value> add(const FloatValue *l, const FloatValue *r) {
  auto value = l->getValue() + r->getValue();
  return FloatValue::create(value);
}
static unique_ptr<Value> add(const StringValue *l, const StringValue *r) {
  auto value = l->getValue() + r->getValue();
  return StringValue::create(value.str());
}
static unique_ptr<Value> add(const ListValue *l, const ListValue *r) {
  auto *context = l->getVectorMemory()->getContext();
  assert(context == r->getVectorMemory()->getContext());
  vector<unique_ptr<Value>> newValues;
  newValues.reserve(l->size() + r->size());
  for (auto idx = 0; idx < l->size(); ++idx)
    newValues.emplace_back(l->getElement(idx)->view()->clone());
  for (auto idx = 0; idx < r->size(); ++idx)
    newValues.emplace_back(r->getElement(idx)->view()->clone());
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
void AddVisitor::visit(const ListValue &l) {
  if (const auto *listL = r->dyn_cast<ListValue>())
    result = add(listL, &l);
  else
    result = nullptr;
}
void AddVisitor::visit(const TupleValue &l) { result = nullptr; }
void AddVisitor::visit(const NilValue &l) { result = nullptr; }
void AddVisitor::visit(const FunctionValue &l) { result = nullptr; }
void AddVisitor::visit(const LambdaValue &l) { result = nullptr; }
void AddVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class SubVisitor : public BinaryOpVisitorImpl<SubVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

void SubVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() - r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 return FloatValue::create(floatL - r->getValue());
               })
               .Case([&](const BoolValue *r) {
                 return IntegerValue::create(l.getValue() - r->getValue());
               })
               .Default(null);
}
void SubVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() - r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const BoolValue *r) {
                 return IntegerValue::create(l.getValue() - r->getValue());
               })
               .Default(null);
}
void SubVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 return FloatValue::create(l.getValue() - floatR);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() - r->getValue();
                 return FloatValue::create(value);
               })
               .Default(null);
}
void SubVisitor::visit(const ListValue &l) { result = nullptr; }
void SubVisitor::visit(const TupleValue &l) { result = nullptr; }
void SubVisitor::visit(const NilValue &l) { result = nullptr; }
void SubVisitor::visit(const FunctionValue &l) { result = nullptr; }
void SubVisitor::visit(const LambdaValue &l) { result = nullptr; }
void SubVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class MulVisitor : public BinaryOpVisitorImpl<MulVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

namespace {
static unique_ptr<Value> mul(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() * r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> mul(const IntegerValue *l, const FloatValue *r) {
  auto floatL = APFloat(APFloat::IEEEdouble(), l->getValue());
  return FloatValue::create(floatL * r->getValue());
}
static unique_ptr<Value> mul(const FloatValue *l, const FloatValue *r) {
  auto value = l->getValue() * r->getValue();
  return FloatValue::create(value);
}
static unique_ptr<Value> mul(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() * r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> mul(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() * r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> mul(const IntegerValue *l, const ListValue *r) {
  if (l->getValue() < 0)
    return nullptr;
  auto context = r->getVectorMemory()->getContext();
  vector<unique_ptr<Value>> newValues;
  auto newSize = l->getValue() * r->size();
  newValues.reserve(newSize);

  for (auto cnt = 0; cnt < l->getValue(); ++cnt) {
    for (auto idx = 0; idx < r->size(); ++idx) {
      newValues.emplace_back(r->getElement(idx)->view()->clone());
    }
  }

  return ListValue::create(context, newValues);
}
static unique_ptr<Value> mul(const IntegerValue *l, const StringValue *r) {
  if (l->getValue() < 0)
    return nullptr;
  string newStr;
  newStr.reserve(l->getValue() * r->getValue().size());
  raw_string_ostream os(newStr);

  for (auto cnt = 0; cnt < l->getValue(); ++cnt)
    os << r->getValue();
  return StringValue::create(os.str());
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

class DivVisitor : public BinaryOpVisitorImpl<DivVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

void DivVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> unique_ptr<Value> {
                 if (r->getValue() == 0)
                   return nullptr;
                 auto value = l.getValue() / r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 auto value = floatL / r->getValue();
                 return FloatValue::create(value);
               })
               .Default(null);
}
void DivVisitor::visit(const BoolValue &l) { result = nullptr; }
void DivVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 auto value = l.getValue() / floatR;
                 return FloatValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() / r->getValue();
                 return FloatValue::create(value);
               })
               .Default(null);
}
void DivVisitor::visit(const ListValue &l) { result = nullptr; }
void DivVisitor::visit(const TupleValue &l) { result = nullptr; }
void DivVisitor::visit(const NilValue &l) { result = nullptr; }
void DivVisitor::visit(const FunctionValue &l) { result = nullptr; }
void DivVisitor::visit(const LambdaValue &l) { result = nullptr; }
void DivVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class ModVisitor : public BinaryOpVisitorImpl<ModVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

void ModVisitor::visit(const IntegerValue &l) {
  if (const IntegerValue *intR = r->dyn_cast<IntegerValue>()) {
    auto value = l.getValue() % intR->getValue();
    result = IntegerValue::create(value);
  } else
    result = nullptr;
}

void ModVisitor::visit(const BoolValue &l) { result = nullptr; }
void ModVisitor::visit(const FloatValue &l) { result = nullptr; }
void ModVisitor::visit(const ListValue &l) { result = nullptr; }
void ModVisitor::visit(const TupleValue &l) { result = nullptr; }
void ModVisitor::visit(const NilValue &l) { result = nullptr; }
void ModVisitor::visit(const FunctionValue &l) { result = nullptr; }
void ModVisitor::visit(const LambdaValue &l) { result = nullptr; }
void ModVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class LtVisitor : public BinaryOpVisitorImpl<LtVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void LtVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() < r->getValue();
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 auto value = floatL < r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void LtVisitor::visit(const BoolValue &l) { result = nullptr; }
void LtVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 auto value = l.getValue() < floatR;
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() < r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void LtVisitor::visit(const ListValue &l) { result = nullptr; }
void LtVisitor::visit(const TupleValue &l) { result = nullptr; }
void LtVisitor::visit(const NilValue &l) { result = nullptr; }
void LtVisitor::visit(const FunctionValue &l) { result = nullptr; }
void LtVisitor::visit(const LambdaValue &l) { result = nullptr; }
void LtVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class LeVisitor : public BinaryOpVisitorImpl<LeVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void LeVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() <= r->getValue();
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 auto value = floatL <= r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void LeVisitor::visit(const BoolValue &l) { result = nullptr; }
void LeVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 auto value = l.getValue() <= floatR;
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() <= r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void LeVisitor::visit(const ListValue &l) { result = nullptr; }
void LeVisitor::visit(const TupleValue &l) { result = nullptr; }
void LeVisitor::visit(const NilValue &l) { result = nullptr; }
void LeVisitor::visit(const FunctionValue &l) { result = nullptr; }
void LeVisitor::visit(const LambdaValue &l) { result = nullptr; }
void LeVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class GtVisitor : public BinaryOpVisitorImpl<GtVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void GtVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() > r->getValue();
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 auto value = floatL > r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void GtVisitor::visit(const BoolValue &l) { result = nullptr; }
void GtVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 auto value = l.getValue() > floatR;
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() > r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void GtVisitor::visit(const ListValue &l) { result = nullptr; }
void GtVisitor::visit(const TupleValue &l) { result = nullptr; }
void GtVisitor::visit(const NilValue &l) { result = nullptr; }
void GtVisitor::visit(const FunctionValue &l) { result = nullptr; }
void GtVisitor::visit(const LambdaValue &l) { result = nullptr; }
void GtVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class GeVisitor : public BinaryOpVisitorImpl<GeVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void GeVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto value = l.getValue() >= r->getValue();
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto floatL = APFloat(APFloat::IEEEdouble(), l.getValue());
                 auto value = floatL >= r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void GeVisitor::visit(const BoolValue &l) { result = nullptr; }
void GeVisitor::visit(const FloatValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) {
                 auto floatR = APFloat(APFloat::IEEEdouble(), r->getValue());
                 auto value = l.getValue() >= floatR;
                 return BoolValue::create(value);
               })
               .Case([&](const FloatValue *r) {
                 auto value = l.getValue() >= r->getValue();
                 return BoolValue::create(value);
               })
               .Default(null);
}
void GeVisitor::visit(const ListValue &l) { result = nullptr; }
void GeVisitor::visit(const TupleValue &l) { result = nullptr; }
void GeVisitor::visit(const NilValue &l) { result = nullptr; }
void GeVisitor::visit(const FunctionValue &l) { result = nullptr; }
void GeVisitor::visit(const LambdaValue &l) { result = nullptr; }
void GeVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class BitAndVisitor : public BinaryOpVisitorImpl<BitAndVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};

namespace {
static unique_ptr<Value> bitAnd(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() & r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> bitAnd(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() & r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> bitAnd(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() & r->getValue();
  return IntegerValue::create(value);
}
} // namespace
void BitAndVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { bitAnd(&l, r); })
               .Case([&](const BoolValue *r) { bitAnd(&l, r); })
               .Default(null);
}
void BitAndVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { bitAnd(r, &l); })
               .Case([&](const BoolValue *r) { bitAnd(&l, r); })
               .Default(null);
}
void BitAndVisitor::visit(const FloatValue &l) { result = nullptr; }
void BitAndVisitor::visit(const ListValue &l) { result = nullptr; }
void BitAndVisitor::visit(const TupleValue &l) { result = nullptr; }
void BitAndVisitor::visit(const NilValue &l) { result = nullptr; }
void BitAndVisitor::visit(const FunctionValue &l) { result = nullptr; }
void BitAndVisitor::visit(const LambdaValue &l) { result = nullptr; }
void BitAndVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class BitOrVisitor : public BinaryOpVisitorImpl<BitOrVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
namespace {
static unique_ptr<Value> bitOr(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() | r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> bitOr(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() | r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> bitOr(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() | r->getValue();
  return IntegerValue::create(value);
}
} // namespace

void BitOrVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { bitOr(&l, r); })
               .Case([&](const BoolValue *r) { bitOr(&l, r); })
               .Default(null);
}
void BitOrVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { bitOr(r, &l); })
               .Case([&](const BoolValue *r) { bitOr(&l, r); })
               .Default(null);
}
void BitOrVisitor::visit(const FloatValue &l) { result = nullptr; }
void BitOrVisitor::visit(const ListValue &l) { result = nullptr; }
void BitOrVisitor::visit(const TupleValue &l) { result = nullptr; }
void BitOrVisitor::visit(const NilValue &l) { result = nullptr; }
void BitOrVisitor::visit(const FunctionValue &l) { result = nullptr; }
void BitOrVisitor::visit(const LambdaValue &l) { result = nullptr; }
void BitOrVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class BitXorVisitor : public BinaryOpVisitorImpl<BitXorVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
namespace {
static unique_ptr<Value> bitXor(const IntegerValue *l, const IntegerValue *r) {
  auto value = l->getValue() ^ r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> bitXor(const IntegerValue *l, const BoolValue *r) {
  auto value = l->getValue() ^ r->getValue();
  return IntegerValue::create(value);
}
static unique_ptr<Value> bitXor(const BoolValue *l, const BoolValue *r) {
  auto value = l->getValue() ^ r->getValue();
  return IntegerValue::create(value);
}

} // namespace
void BitXorVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { bitXor(&l, r); })
               .Case([&](const BoolValue *r) { bitXor(&l, r); })
               .Default(null);
}
void BitXorVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) { bitXor(r, &l); })
               .Case([&](const BoolValue *r) { bitXor(&l, r); })
               .Default(null);
}
void BitXorVisitor::visit(const FloatValue &l) { result = nullptr; }
void BitXorVisitor::visit(const ListValue &l) { result = nullptr; }
void BitXorVisitor::visit(const TupleValue &l) { result = nullptr; }
void BitXorVisitor::visit(const NilValue &l) { result = nullptr; }
void BitXorVisitor::visit(const FunctionValue &l) { result = nullptr; }
void BitXorVisitor::visit(const LambdaValue &l) { result = nullptr; }
void BitXorVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class ShlVisitor : public BinaryOpVisitorImpl<ShlVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void ShlVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> unique_ptr<Value> {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(value);
               })
               .Default(null);
}
void ShlVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> unique_ptr<Value> {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() << r->getValue();
                 return IntegerValue::create(value);
               })
               .Default(null);
}
void ShlVisitor::visit(const FloatValue &l) { result = nullptr; }
void ShlVisitor::visit(const ListValue &l) { result = nullptr; }
void ShlVisitor::visit(const TupleValue &l) { result = nullptr; }
void ShlVisitor::visit(const NilValue &l) { result = nullptr; }
void ShlVisitor::visit(const FunctionValue &l) { result = nullptr; }
void ShlVisitor::visit(const LambdaValue &l) { result = nullptr; }
void ShlVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

class ShrVisitor : public BinaryOpVisitorImpl<ShrVisitor> {
public:
#define VALUE(Name) void visit(const Name##Value &l);
#include "bara/interpreter/Value.def"
};
void ShrVisitor::visit(const IntegerValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> unique_ptr<Value> {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(value);
               })
               .Default(null);
}
void ShrVisitor::visit(const BoolValue &l) {
  result = ValueSwitch(r)
               .Case([&](const IntegerValue *r) -> unique_ptr<Value> {
                 if (r->getValue() < 0)
                   return nullptr;
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(value);
               })
               .Case([&](const BoolValue *r) {
                 auto value = l.getValue() >> r->getValue();
                 return IntegerValue::create(value);
               })
               .Default(null);
}
void ShrVisitor::visit(const FloatValue &l) { result = nullptr; }
void ShrVisitor::visit(const ListValue &l) { result = nullptr; }
void ShrVisitor::visit(const TupleValue &l) { result = nullptr; }
void ShrVisitor::visit(const NilValue &l) { result = nullptr; }
void ShrVisitor::visit(const FunctionValue &l) { result = nullptr; }
void ShrVisitor::visit(const LambdaValue &l) { result = nullptr; }
void ShrVisitor::visit(const BuiltinFunctionValue &l) { result = nullptr; }

namespace BinaryOp {
#define BINARY_FUNC(funcName, VisitorName)                                     \
  unique_ptr<Value> funcName(const Value *l, const Value *r) {                 \
    VisitorName visitor;                                                       \
    visitor.init(r);                                                           \
    l->accept(visitor);                                                        \
    return visitor.getResult();                                                \
  }
BINARY_FUNC(add, AddVisitor)
BINARY_FUNC(sub, SubVisitor)
BINARY_FUNC(mul, MulVisitor)
BINARY_FUNC(div, DivVisitor)
BINARY_FUNC(mod, ModVisitor)
BINARY_FUNC(lt, LtVisitor)
BINARY_FUNC(le, LeVisitor)
BINARY_FUNC(gt, GtVisitor)
BINARY_FUNC(ge, GeVisitor)
BINARY_FUNC(bitAnd, BitAndVisitor)
BINARY_FUNC(bitOr, BitOrVisitor)
BINARY_FUNC(bitXor, BitXorVisitor)
BINARY_FUNC(shk, ShlVisitor)
BINARY_FUNC(shr, ShrVisitor)

} // namespace BinaryOp
} // namespace bara
