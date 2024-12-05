#ifndef BARA_LEXER_H
#define BARA_LEXER_H

#include "bara/diagnostic/Diagnostic.h"
#include "bara/parser/Token.h"
#include "llvm/Support/FormatVariadic.h"
#include "llvm/Support/SourceMgr.h"

namespace bara {

class Lexer {
public:
  Lexer(Diagnostic &reporter, ASTContext *context, StringRef buffer)
      : buffer(buffer), reporter(reporter), context(context) {}

  Diagnostic &getDiagnostic() { return reporter; }
  ASTContext *getASTContext() { return context; }

  Token *getNextToken(size_t index = 0);
  Token *peekToken(size_t index = 0);
  ArrayRef<Token *> getTokens() const { return tokens; }

  static constexpr char eof = static_cast<char>(EOF);

private:
  void lex();
  void capture();
  char advance();
  char peek();
  void skip();
  Token *lexToTokenPos(size_t pos);
  void lexIdentifier();
  void lexNumber();
  void lexInteger();
  void lexFloat();
  void lexString();
  void skipWhitespace();
  SMRange createRange() const;
  Token *createToken();

  struct LexDiagnostic {
    enum Diag {
#define DIAG(Name, ...) Name,
#include "bara/parser/LexerDiagnostic.def"
    };

    static const char *getDiagMsg(Diag kind);
    static llvm::SourceMgr::DiagKind getDiagKind(Diag kind);
  };

  template <typename... Args>
  void report(SMRange range, LexDiagnostic::Diag diag, Args &&...args) {
    reporter.report(range, LexDiagnostic::getDiagKind(diag),
                    llvm::formatv(LexDiagnostic::getDiagMsg(diag),
                                  std::forward<Args>(args)...)
                        .str());
  }

private:
  StringRef buffer;
  size_t pos = 0;
  size_t row = 1;
  size_t col = 1;
  Token::Kind kind = Token::Kind::Tok_Unknown;
  size_t lastPos = 0;
  size_t lastRow = 0;
  size_t lastCol = 0;

  Diagnostic &reporter;
  ASTContext *context;

  vector<Token *> tokens;
  size_t tokPos = 0;
};

} // namespace bara

#endif // BARA_LEXER_H
