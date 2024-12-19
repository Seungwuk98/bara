#include "bara/diagnostic/Diagnostic.h"
#include "bara/interpreter/REPL.h"
#include "bara/interpreter/StmtInterpreter.h"
#include "bara/interpreter/Value.h"
#include "bara/parser/Parser.h"
#include "bara/utils/STL.h"
#include "llvm/Support/CommandLine.h"
#include "llvm/Support/InitLLVM.h"
#include "llvm/Support/raw_ostream.h"

namespace bara::cl {

enum Action {
  ASTPrint,
  Interpret,
};

llvm::cl::opt<string> input(llvm::cl::Positional, llvm::cl::init("-"),
                            llvm::cl::desc("<input file>"));

llvm::cl::opt<string> output("o", llvm::cl::init("-"),
                             llvm::cl::desc("Specify output file"),
                             llvm::cl::value_desc("filename"));

llvm::cl::opt<Action>
    action(llvm::cl::desc("Action to perform:"), llvm::cl::init(Interpret),
           llvm::cl::values(clEnumValN(ASTPrint, "ast-print", "Print AST"),
                            clEnumValN(Interpret, "interpret", "Interpret")));

} // namespace bara::cl

namespace bara {

void dump(llvm::function_ref<void(raw_ostream &)> fn) {
  if (bara::cl::output == "-") {
    fn(llvm::outs());
  } else {
    std::error_code ec;
    llvm::raw_fd_ostream os(bara::cl::output, ec);
    if (ec) {
      llvm::errs() << "invalid output file " << cl::output << " : "
                   << ec.message() << "\n";
      return;
    }
    fn(os);
  }
}

int baraMain() {
  llvm::SourceMgr sourceMgr;
  Diagnostic diag(sourceMgr);

  if (cl::input == "-" && bara::cl::action == cl::Interpret)
    return runREPLMain();

  auto inputBufferOrErr = llvm::MemoryBuffer::getFileOrSTDIN(bara::cl::input);
  if (inputBufferOrErr.getError()) {

    llvm::errs() << ("read file error: " +
                     inputBufferOrErr.getError().message());
    return 1;
  }

  auto index =
      sourceMgr.AddNewSourceBuffer(std::move(inputBufferOrErr.get()), SMLoc());
  auto inputBufferRef = sourceMgr.getMemoryBuffer(index);

  ASTContext context;

  Lexer lexer(diag, &context, inputBufferRef->getBuffer());
  Parser parser(lexer);

  auto *program = parser.parse();
  if (diag.hasError())
    return 1;

  if (bara::cl::action == cl::ASTPrint) {
    dump([program](raw_ostream &os) { os << program->toString(); });
    return 0;
  }

  if (bara::cl::action == cl::Interpret) {
    MemoryContext memoryContext;
    interpret(program, &memoryContext, diag);
    return diag.hasError();
  }

  return 0;
}

} // namespace bara

int main(int argc, const char **argv) {
  llvm::InitLLVM x(argc, argv);

  llvm::cl::ParseCommandLineOptions(argc, argv, "bara\n");

  return bara::baraMain();
}
