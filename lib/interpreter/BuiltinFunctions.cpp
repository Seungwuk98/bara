#include "bara/context/MemoryContext.h"
#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"
#include "llvm/ADT/APSInt.h"
#include "llvm/ADT/FloatingPointMode.h"
#include "llvm/ADT/SmallVectorExtras.h"
#include "llvm/ADT/StringExtras.h"
#include "llvm/ADT/TypeSwitch.h"
#include "llvm/Support/FormatVariadic.h"
#include <iostream>
#include <random>

namespace bara {

namespace BuiltinFn {

enum Diag {
#define DIAG(Name, ...) Name,
#include "BuiltinFunctionDiagnostic.def"
};

const char *diagMsgs[] = {
#define DIAG(Name, Msg, Error) Msg,
#include "BuiltinFunctionDiagnostic.def"
};

llvm::SourceMgr::DiagKind diagKinds[] = {
#define DIAG(Name, Msg, Error) llvm::SourceMgr::DK_##Error,
#include "BuiltinFunctionDiagnostic.def"
};

template <typename... Args>
void report(SMRange range, Diagnostic &diag, Diag kind, Args &&...args) {
  auto msg = llvm::formatv(diagMsgs[kind], std::forward<Args>(args)...).str();
  diag.report(range, diagKinds[kind], msg);
}

#define DECL(Name)                                                             \
  Value *Name(ArrayRef<Value *> args, Diagnostic &diag, SMRange range,         \
              MemoryContext *context)

DECL(print) {
  for (const auto &[idx, arg] : llvm::enumerate(args)) {
    getPrintOS() << arg->toString();
    if (idx != args.size() - 1)
      getPrintOS() << " ";
  }
  getPrintOS() << "\n";
  return NilValue::create(context);
}

DECL(help) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "help", 1, args.size());
    return nullptr;
  }

  auto value = args[0];
  if (!value->isa<BuiltinFunctionValue>()) {
    report(range, diag, error_unexpected_type, "builtin function",
           value->toString());
    return nullptr;
  }

  auto funcValue = value->cast<BuiltinFunctionValue>();
  outs() << funcValue->getHelp();
  return NilValue::create(context);
}

DECL(push) {
  if (args.size() != 2) {
    report(range, diag, error_invalid_argument_size, "push", 2, args.size());
    return nullptr;
  }

  auto value = args[0];
  if (!value->isa<ListValue>()) {
    report(range, diag, error_unexpected_type, "list value", value->toString());
    return nullptr;
  }

  auto listValue = value->cast<ListValue>();
  listValue->push(args[1]);
  return NilValue::create(context);
}

DECL(pop) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "pop", 1, args.size());
    return nullptr;
  }

  auto value = args[0];
  if (!value->isa<ListValue>()) {
    report(range, diag, error_unexpected_type, "list value", value->toString());
    return nullptr;
  }

  auto listValue = value->cast<ListValue>();
  if (listValue->empty()) {
    report(range, diag, error_empty_list);
    return nullptr;
  }

  auto *poped = listValue->pop();
  return poped;
}

DECL(str) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "str", 1, args.size());
    return nullptr;
  }
  auto value = args[0]->toString();
  return StringValue::create(context, value);
}

DECL(format) {
  auto first = args[0];
  if (!first->isa<StringValue>()) {
    report(range, diag, error_unexpected_type, "str value", first->toString());
    return nullptr;
  }

  auto format = first->cast<StringValue>()->getValue();
  auto pos = 0;
  string str;
  raw_string_ostream os(str);

  auto argIdx = 1;
  while (pos < format.size()) {
    auto formatIdx = format.find("{}", pos);
    os << format.slice(pos, formatIdx);
    if (formatIdx == StringRef::npos)
      break;

    if (argIdx < args.size())
      os << args[argIdx++]->toString();
    else
      os << "{}";
    pos = formatIdx + 2;
  }

  return StringValue::create(context, os.str());
}

using ValueSwitch = llvm::TypeSwitch<Value *, Value *>;

DECL(len) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "len", 1, args.size());
    return nullptr;
  }

  auto value = args[0];
  return ValueSwitch(value)
      .Case([&](const ListValue *listV) {
        return IntegerValue::create(context, listV->size());
      })
      .Case([&](const TupleValue *tupleV) {
        return IntegerValue::create(context, tupleV->size());
      })
      .Default([&](Value *) -> Value * {
        report(range, diag, error_unexpected_type, "list or tuple",
               value->toString());
        return nullptr;
      });
}

DECL(intCast) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "int cast", 1,
           args.size());
    return nullptr;
  }

  auto value = args[0];
  return ValueSwitch(value)
      .Case([&](IntegerValue *intV) { return intV; })
      .Case([&](const FloatValue *floatV) -> Value * {
        llvm::APSInt apsInt(64, false);
        bool isExact;
        auto status = floatV->getValue().convertToInteger(
            apsInt, llvm::RoundingMode::TowardNegative, &isExact);
        if (status == llvm::APFloat::opInvalidOp ||
            status == llvm::APFloat::opDivByZero) {
          report(range, diag, error_cast, floatV->toString(), "int cast");
          return nullptr;
        }

        return IntegerValue::create(context, apsInt.getSExtValue());
      })
      .Case([&](const BoolValue *boolV) {
        return IntegerValue::create(context, boolV->getValue());
      })
      .Case([&](const StringValue *strV) -> Value * {
        auto value = strV->getValue();
        int64_t result;
        auto fail = value.getAsInteger(10, result);
        if (fail) {
          report(range, diag, error_cast, strV->toString(), "int cast");
          return nullptr;
        }
        return IntegerValue::create(context, result);
      })
      .Default([&](const Value *) {
        report(range, diag, error_cast, value->toString(), "int cast");
        return nullptr;
      });
}

DECL(floatCast) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "float cast", 1,
           args.size());
    return nullptr;
  }

  auto value = args[0];
  return ValueSwitch(value)
      .Case([&](const IntegerValue *intV) {
        return FloatValue::create(
            context, APFloat(APFloat::IEEEdouble(), intV->getValue()));
      })
      .Case([&](FloatValue *floatV) { return floatV; })
      .Case([&](const StringValue *strV) -> Value * {
        auto value = strV->getValue();
        double result;
        auto fail = value.getAsDouble(result);
        if (fail) {
          report(range, diag, error_cast, strV->toString(), "float");
          return nullptr;
        }
        return FloatValue::create(context, APFloat(result));
      })
      .Default([&](const Value *) {
        report(range, diag, error_cast, value->toString(), "float");
        return nullptr;
      });
}

DECL(boolCast) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "bool cast", 1,
           args.size());
    return nullptr;
  }

  auto value = args[0];
  return ValueSwitch(value)
      .Case([&](const IntegerValue *intV) {
        return BoolValue::create(context, intV->getValue());
      })
      .Case([&](BoolValue *boolV) { return boolV; })
      .Case([&](const StringValue *strV) -> Value * {
        auto value = llvm::StringSwitch<optional<bool>>(strV->getValue())
                         .Case("true", true)
                         .Case("false", false)
                         .Default(nullopt);
        if (!value) {
          report(range, diag, error_cast, strV->toString(), "bool");
          return nullptr;
        }
        return BoolValue::create(context, *value);
      })
      .Default([&](const Value *) {
        report(range, diag, error_cast, value->toString(), "bool");
        return nullptr;
      });
}

DECL(type) {
  if (args.size() != 1) {
    report(range, diag, error_invalid_argument_size, "type", 1, args.size());
    return nullptr;
  }

  auto value = args[0];
  return ValueSwitch(value)
      .Case([&](const IntegerValue *) {
        return StringValue::create(context, "int");
      })
      .Case([&](const FloatValue *) {
        return StringValue::create(context, "float");
      })
      .Case([&](const BoolValue *) {
        return StringValue::create(context, "bool");
      })
      .Case([&](const StringValue *) {
        return StringValue::create(context, "str");
      })
      .Case([&](const ListValue *) {
        return StringValue::create(context, "list");
      })
      .Case([&](const TupleValue *) {
        return StringValue::create(context, "tuple");
      })
      .Case(
          [&](const NilValue *) { return StringValue::create(context, "nil"); })
      .Case([&](const FunctionValue *) {
        return StringValue::create(context, "function");
      })
      .Case([&](const LambdaValue *) {
        return StringValue::create(context, "lambda");
      })
      .Case([&](const BuiltinFunctionValue *) {
        return StringValue::create(context, "builtin function");
      })

      .Default([&](const Value *) -> Value * {
        llvm_unreachable("all type of value is handled");
      });
}

DECL(input) {
  if (args.size() != 0) {
    report(range, diag, error_invalid_argument_size, "input", 0, args.size());
    return nullptr;
  }

  string input;

  char ch;
  if (!std::getline(std::cin, input) && input.empty()) {
    report(range, diag, error_input);
    return nullptr;
  }

  return StringValue::create(context, input);
}

DECL(split) {
  if (args.empty() || args.size() > 2) {
    report(range, diag, error_invalid_argument_size, "split", "1 or 2",
           args.size());
    return nullptr;
  }

  auto value = args[0];
  if (!value->isa<StringValue>()) {
    report(range, diag, error_unexpected_type, "str", value->toString());
    return nullptr;
  }

  auto strValue = value->cast<StringValue>();
  StringRef sep = " ";
  if (args.size() == 2) {
    auto sepValue = args[1];
    if (!sepValue->isa<StringValue>()) {
      report(range, diag, error_unexpected_type, "str", sepValue->toString());
      return nullptr;
    }
    sep = sepValue->cast<StringValue>()->getValue();
  }

  auto str = strValue->getValue();
  auto splitResult = llvm::split(str, sep);
  SmallVector<Value *> newValues =
      llvm::map_to_vector(splitResult, [&](StringRef element) -> Value * {
        return StringValue::create(context, element);
      });
  return ListValue::create(context, newValues);
}

static std::random_device rd;
static std::mt19937 gen(rd());
static std::uniform_int_distribution<int64_t>
    dis(std::numeric_limits<int64_t>::min(),
        std::numeric_limits<int64_t>::max());

DECL(random) {
  if (args.size() != 0 && args.size() != 2) {
    report(range, diag, error_invalid_argument_size, "random", 0, args.size());
    return nullptr;
  }

  auto randValue = dis(gen);
  if (args.size() == 2) {
    auto start = args[0];
    if (!start->isa<IntegerValue>()) {
      report(range, diag, error_unexpected_type, "int", start->toString());
      return nullptr;
    }
    auto end = args[1];
    if (!end->isa<IntegerValue>()) {
      report(range, diag, error_unexpected_type, "int", end->toString());
      return nullptr;
    }

    auto startValue = start->cast<IntegerValue>()->getValue();
    auto endValue = end->cast<IntegerValue>()->getValue();

    if (startValue >= endValue) {
      report(range, diag, error_invalid_random_range, startValue, endValue);
      return nullptr;
    }

    randValue = randValue % (endValue - startValue);
    if (randValue < 0)
      randValue = -randValue;
    randValue += startValue;
  }

  return IntegerValue::create(context, randValue);
}

#undef DECL

} // namespace BuiltinFn

} // namespace bara
