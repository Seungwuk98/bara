#ifndef BARA_LEXER_H
#define BARA_LEXER_H

#include "bara/parser/Token.h"
#include "llvm/Support/SourceMgr.h"

namespace bara {

class Lexer {
public:
  Lexer(llvm::SourceMgr &sourceMgr, StringRef buffer)
      : sourceMgr(sourceMgr), buffer(buffer) {}

  Token *getNextToken(size_t index = 0);
  Token *peekToken(size_t index = 0);

  static constexpr char eof = static_cast<char>(EOF);

private:
  void lex();
  void capture();
  char advance();
  char peek();
  void skip();
  void lexToTokenPos(size_t pos);
  void lexIdentifier();
  void lexNumber();
  void lexInteger();
  void lexFloat();
  void lexString();
  void skipWhitespace();
  Token *createToken();

private:
  StringRef buffer;
  size_t pos = 0;
  size_t row = 0;
  size_t col = 0;
  Token::Kind kind = Token::Kind::Tok_Unknown;
  size_t lastPos = 0;
  size_t lastRow = 0;
  size_t lastCol = 0;

  llvm::SourceMgr &sourceMgr;
  ASTContext *context;

  vector<Token *> tokens;
  size_t tokPos = 0;
};

} // namespace bara

#endif // BARA_LEXER_H
