#ifndef BARA_INTERPRETER_H
#define BARA_INTERPRETER_H

#include "bara/ast/AST.h"
#include "bara/context/GarbageCollector.h"
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

  GC::RootRegister lvInterpret(const Expression &ast);
  GC::RootRegister rvInterpret(const Expression &ast);
  Environment &getCurrEnv() { return *stack.back(); }
  bool isTerminated() const {
    return diag.hasError() || continueFlag || breakFlag || returnFlag;
  }

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

  struct StackScope {
    StackScope(const StackScope &) = delete;
    StackScope &operator=(const StackScope &) = delete;
    StackScope(StackScope &&) = delete;
    StackScope &operator=(StackScope &&) = delete;

    StackScope(StmtInterpreter &interpreter, const Environment &env)
        : interpreter(interpreter) {
      interpreter.stack.emplace_back(std::make_unique<Environment>(env));
    }

    ~StackScope() { interpreter.stack.pop_back(); }

  private:
    StmtInterpreter &interpreter;
  };

  void patternDeclaration(const Pattern &pattern);
  bool matchPattern(const Pattern &pattern,
                    variant<Value *, ValueMemory *> value);

  template <typename ConcreteType>
  friend class CommonExprInterpreter;
  friend class RvExprInterpreter;
  friend class LvExprInterpreter;
  friend class GC;

  MemoryContext *context;
  Diagnostic &diag;

  SmallVector<std::unique_ptr<Environment>> stack;
  RvExprInterpreter *rvInterpreter;
  LvExprInterpreter *lvInterpreter;

  DenseMap<const StructDeclaration *, DenseMap<StringRef, size_t>>
      structMemberMap;

  const ContinueStatement *continueFlag = nullptr;
  const BreakStatement *breakFlag = nullptr;
  const ReturnStatement *returnFlag = nullptr;
  Value *returnValue = nullptr;
};

void interpret(const Program *program, MemoryContext *context,
               Diagnostic &diag);

void setPrintOS(raw_ostream &os);
raw_ostream &getPrintOS();

} // namespace bara

#endif // BARA_INTERPRETER_H
