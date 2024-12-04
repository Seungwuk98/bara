#include "bara/parser/Token.h"
#include <format>

namespace bara {

const char *tokenNames[] = {
#define TOKEN(NAME) #NAME,
#include "bara/parser/Token.def"
};

const char *Token::getTokenString(Kind kind) { return tokenNames[kind]; }

std::string Token::toString() const {
  return std::format("{}:{}:{}:{}:{}", getTokenString(getKind()),
                     getSymbol().str(), getRow(), getCol(), getIndex());
}

} // namespace bara
