#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/Value.h"
#include "llvm/Support/FormatVariadic.h"

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
  unique_ptr<Value> Name(ArrayRef<unique_ptr<Value>> args, Diagnostic &diag)

DECL(print) {
  for (const auto &[idx, arg] : llvm::enumerate(args)) {
    outs() << arg->toString();
    if (idx != args.size() - 1)
      outs() << " ";
  }
  outs() << "\n";
  return NilValue::create();
}

DECL(help) {
  if (args.size() != 1) {
  }
}

#undef DECL

} // namespace BuiltinFn

} // namespace bara
