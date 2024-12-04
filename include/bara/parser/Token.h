#ifndef BARA_TOKEN_H
#define BARA_TOKEN_H

#include "bara/context/ASTContext.h"
#include "bara/utils/LLVM.h"
#include "bara/utils/STL.h"

namespace bara {

class Token {
public:
  enum Kind {

#define TOKEN(NAME) Tok_##NAME,
#include "bara/parser/Token.def"

    NUM_OF_TOKEN
  };

  Token(SMRange range, Kind kind, StringRef symbol, size_t row, size_t col,
        size_t index)
      : range(range), kind(kind), symbol(symbol), row(row), col(col),
        index(index) {}

  friend class Lexer;

  template <Kind... Kinds>
  bool is() const {
    return ((kind == Kinds) || ...);
  }

  static Token *create(ASTContext *context, SMRange range, Kind kind,
                       StringRef symbol, size_t row, size_t col, size_t index) {
    auto *token = context->alloc(sizeof(Token));
    return new (token) Token(range, kind, symbol, row, col, index);
  }

  SMRange getRange() const { return range; }
  Kind getKind() const { return kind; }
  StringRef getSymbol() const { return symbol; }
  size_t getRow() const { return row; }
  size_t getCol() const { return col; }
  size_t getIndex() const { return index; }

  bool operator==(const Token &other) const {
    return kind == other.kind && symbol == other.symbol && row == other.row &&
           col == other.col && index == other.index;
  }

  static const char *getTokenString(Kind kind);
  string toString() const;

private:
  SMRange range;
  Kind kind;
  StringRef symbol;
  size_t row;
  size_t col;
  size_t index;
};

} // namespace bara

#endif // BARA_TOKEN_H
