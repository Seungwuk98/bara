#ifndef BARA_INTERPRETER_H
#define BARA_INTERPRETER_H

#include "bara/ast/AST.h"
#include "bara/context/MemoryContext.h"
#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/Environment.h"
#include "bara/interpreter/Memory.h"
#include "llvm/Support/FormatVariadic.h"

namespace bara {

class RvExprInterpreter;
class LvExprInterpreter;

class StmtInterpreter : public ConstASTVisitorBase<StmtInterpreter
#define STATEMENT(Name) , Name
#include "bara/ast/Statement.def"
                                                   > {

public:
  StmtInterpreter(MemoryContext *context, Diagnostic &diag);
  ~StmtInterpreter();

#define STATEMENT(Name) void visit(const Name &stmt);
#include "bara/ast/Statement.def"

  Memory *lvInterpret(const Expression &ast);
  unique_ptr<Value> rvInterpret(const Expression &ast);

private:
  struct InterpretDiagnostic {
    enum Diag {
#define DIAG(Name, Msg, Error) Name,
#include "bara/interpreter/InterpreterDiagnostic.def"
    };

    static const char *getMessage(Diag kind);
    static llvm::SourceMgr::DiagKind getDiagKind(Diag kind);
  };

  template <typename... Args>
  void report(SMRange range, InterpretDiagnostic::Diag kind, Args &&...args) {
    diag.report(range, InterpretDiagnostic::getDiagKind(kind),
                llvm::formatv(InterpretDiagnostic::getMessage(kind),
                              std::forward<Args>(args)...)
                    .str());
  }

  struct ReplaceEnvScope {
    ReplaceEnvScope(StmtInterpreter &interpreter, const Environment &env)
        : interpreter(interpreter), savedEnv(std::move(interpreter.env)) {
      interpreter.env = env;
    }

  private:
    StmtInterpreter &interpreter;
    Environment savedEnv;
  };

  void patternDeclaration(const Pattern &pattern);
  bool matchPattern(const Pattern &pattern, Value *value);

  template <typename ConcreteType>
  friend class CommonExprInterpreter;
  friend class RvExprInterpreter;
  friend class LvExprInterpreter;

  MemoryContext *context;
  Diagnostic &diag;

  Environment env;
  RvExprInterpreter *rvInterpreter;
  LvExprInterpreter *lvInterpreter;

  bool isTerminated() const {
    return diag.hasError() || continueFlag || breakFlag || returnFlag;
  }

  const ContinueStatement *continueFlag = nullptr;
  const BreakStatement *breakFlag = nullptr;
  const ReturnStatement *returnFlag = nullptr;
  unique_ptr<Value> returnValue = nullptr;
};

} // namespace bara

#endif // BARA_INTERPRETER_H
