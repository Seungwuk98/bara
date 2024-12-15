#include "bara/context/MemoryContext.h"
#include "bara/interpreter/ExprInterpreter.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/parser/Lexer.h"
#include "bara/parser/Parser.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"

#define INTERPRETER_TEST_SUITE InterpreterTests
namespace bara {
class InterpreterTests {
public:
  InterpreterTests() : diag(srcMgr), interpreter(&memoryContext, diag) {}

  Expression *parseExpression(StringRef source) {
    auto sourceIdx =
        srcMgr.AddNewSourceBuffer(llvm::MemoryBuffer::getMemBuffer(
                                      source, "expr" + std::to_string(cnt++)),
                                  llvm::SMLoc());
    auto bufferRef = srcMgr.getMemoryBuffer(sourceIdx)->getBuffer();
    Lexer lexer(diag, &astContext, bufferRef);
    Parser parser(lexer);
    auto *expr = parser.parseExpression();
    if (!lexer.peekToken()->is<Token::Tok_Eof>()) {
      diag.report(lexer.peekToken()->getRange(), llvm::SourceMgr::DK_Error,
                  "Expression parsing is not ended");
      return nullptr;
    }
    return expr;
  }

  unique_ptr<Value> eval(StringRef source) {
    auto expr = parseExpression(source);
    if (diag.hasError())
      return nullptr;
    return eval(expr);
  }

  unique_ptr<Value> eval(Expression *expr) {
    return interpreter.rvInterpret(*expr);
  }

  bool hasError() const { return diag.hasError(); }

  ASTContext *getASTContext() { return &astContext; }
  MemoryContext *getMemoryContext() { return &memoryContext; }

private:
  ASTContext astContext;
  MemoryContext memoryContext;
  llvm::SourceMgr srcMgr;
  Diagnostic diag;

  StmtInterpreter interpreter;

  size_t cnt = 0;
};

} // namespace bara
