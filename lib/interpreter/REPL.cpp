#include "bara/interpreter/REPL.h"
#include "bara/ast/AST.h"
#include "bara/interpreter/Value.h"
#include "bara/parser/Parser.h"
#include "llvm/Support/SourceMgr.h"
#include <iostream>

namespace bara {

bool REPL::evalSTDIN() {
  string line;
  os << "> ";
  if (!std::getline(std::cin, line) && line.empty()) {
    errs() << "Error reading from stdin\n";
    return true;
  }
  if (line == "exit" || line == "quit")
    return true;
  eval(line);
  return false;
}

void REPL::eval(StringRef line) {
  diag.reset();
  lines.emplace_back(line);
  auto lineNumber = lines.size();
  auto lineBuffer = llvm::MemoryBuffer::getMemBuffer(
      lines.back(), (llvm::formatv("<REPL line:{0}>", lineNumber).str()));
  auto idx = srcMgr.AddNewSourceBuffer(std::move(lineBuffer), SMLoc());
  auto buffer = srcMgr.getMemoryBuffer(idx);

  Lexer lexer(diag, &astContext, buffer->getBuffer());
  Parser parser(lexer);

  Statement *stmt = parser.parseStatement();
  if (diag.hasError())
    return;

  if (!lexer.peekToken()->is<Token::Tok_Eof>()) {
    diag.report(lexer.peekToken()->getRange(), llvm::SourceMgr::DK_Error,
                "Expected one statement per line");
    return;
  }

  if (auto *exprExpr = stmt->dyn_cast<ExpressionStatement>()) {
    auto *expr = exprExpr->getExpr();
    auto value = interpreter.rvInterpret(*expr);
    if (diag.hasError())
      return;
    show(value.get());
  } else
    stmt->accept(interpreter);
}

class REPLShowVisitor : public ConstValueVisitorBase<REPLShowVisitor> {
public:
  REPLShowVisitor(raw_ostream &os) : os(os) {}
#define VALUE(Name) void visit(const Name##Value &value);
#include "bara/interpreter/Value.def"

private:
  raw_ostream &os;
};

void REPL::show(const Value *value) {
  REPLShowVisitor visitor(os);
  value->accept(visitor);
  os << '\n';
}

void REPLShowVisitor::visit(const IntegerValue &value) {
  os << value.getValue();
}

void REPLShowVisitor::visit(const FloatValue &value) {
  SmallVector<char> buffer;
  value.getValue().toString(buffer);
  os << buffer;
}

void REPLShowVisitor::visit(const BoolValue &value) {
  os << (value.getValue() ? "true" : "false");
}

void REPLShowVisitor::visit(const StringValue &value) {
  os << '"' << value.getValue() << '"';
}

void REPLShowVisitor::visit(const ListValue &value) {
  os << '[';
  for (auto idx = 0; idx < value.size(); ++idx) {
    value.getElement(idx)->view()->accept(*this);
    if (idx != value.size() - 1)
      os << ", ";
  }
  os << ']';
}

void REPLShowVisitor::visit(const TupleValue &value) {
  os << '(';
  for (auto idx = 0; idx < value.size(); ++idx) {
    value.getElement(idx)->accept(*this);
    if (idx != value.size() - 1)
      os << ", ";
  }
  os << ')';
}

void REPLShowVisitor::visit(const NilValue &value) { os << "nil"; }

void REPLShowVisitor::visit(const FunctionValue &value) {
  os << "<function " << value.getDeclaration()->getName() << ">";
}

void REPLShowVisitor::visit(const LambdaValue &value) { os << "<lambda>"; }

void REPLShowVisitor::visit(const BuiltinFunctionValue &value) {
  os << "<builtin function " << value.getName() << ">";
}

int runREPLMain() {
  REPL repl;
  repl.os << "bara REPL\n";

  while (!repl.evalSTDIN())
    ;

  return 0;
}

} // namespace bara
