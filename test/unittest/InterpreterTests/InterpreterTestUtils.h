#include "bara/context/MemoryContext.h"
#include "bara/interpreter/Environment.h"
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
  InterpreterTests(raw_ostream &os = errs())
      : diag(srcMgr, os), interpreter(&memoryContext, diag) {}

  Expression *parseExpression(StringRef source) {
    auto sourceIdx = srcMgr.AddNewSourceBuffer(
        llvm::MemoryBuffer::getMemBuffer(source,
                                         "expr" + std::to_string(exprCnt++)),
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

  Statement *parseStatement(StringRef source) {
    auto sourceIdx = srcMgr.AddNewSourceBuffer(
        llvm::MemoryBuffer::getMemBuffer(source,
                                         "expr" + std::to_string(stmtCnt++)),
        llvm::SMLoc());
    auto bufferRef = srcMgr.getMemoryBuffer(sourceIdx)->getBuffer();
    Lexer lexer(diag, &astContext, bufferRef);
    Parser parser(lexer);
    auto *stmt = parser.parseStatement();
    if (!lexer.peekToken()->is<Token::Tok_Eof>()) {
      diag.report(lexer.peekToken()->getRange(), llvm::SourceMgr::DK_Error,
                  "Expression parsing is not ended");
      return nullptr;
    }
    return stmt;
  }

  UniqueValue<Value> eval(StringRef source) {
    auto expr = parseExpression(source);
    if (diag.hasError())
      return nullptr;
    return eval(expr);
  }

  UniqueValue<Value> eval(Expression *expr) {
    return interpreter.rvInterpret(*expr);
  }

  bool hasError() const { return diag.hasError(); }

  ASTContext *getASTContext() { return &astContext; }
  MemoryContext *getMemoryContext() { return &memoryContext; }

  class TestProgram {
  public:
    TestProgram(InterpreterTests *tests)
        : tests(tests), scope(tests->interpreter.getEnv()) {}

    TestProgram &statement(StringRef statement) {
      auto stmt = tests->parseStatement(statement);
      if (tests->hasError())
        return *this;
      stmt->accept(tests->interpreter);
      return *this;
    }

    UniqueValue<Value> eval(StringRef expr) { return tests->eval(expr); }

    bool hasError() const { return tests->hasError(); }

  private:
    InterpreterTests *tests;
    Environment::Scope scope;
  };

private:
  ASTContext astContext;
  MemoryContext memoryContext;
  llvm::SourceMgr srcMgr;
  Diagnostic diag;

  StmtInterpreter interpreter;

  size_t stmtCnt = 0;
  size_t exprCnt = 0;
};

} // namespace bara
