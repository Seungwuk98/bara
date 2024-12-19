#ifndef BARA_REPL_H
#define BARA_REPL_H

#include "bara/context/ASTContext.h"
#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Environment.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "llvm/Support/SourceMgr.h"

namespace bara {

class REPL {
public:
  REPL(raw_ostream &os = outs(), raw_ostream &err = errs(),
       raw_ostream &printOS = outs())
      : os(os), diag(srcMgr, err), astContext(), memoryContext(),
        interpreter(&memoryContext, diag), scope(interpreter.getEnv()) {
    setPrintOS(printOS);
  }

  bool evalSTDIN();

  void eval(StringRef line);

  void show(const Value *value);

private:
  raw_ostream &os;

  llvm::SourceMgr srcMgr;
  Diagnostic diag;
  ASTContext astContext;
  MemoryContext memoryContext;
  StmtInterpreter interpreter;

  Environment::Scope scope;

  vector<string> lines;

  friend int runREPLMain();
};

int runREPLMain();

} // namespace bara

#endif // BARA_REPL_H
