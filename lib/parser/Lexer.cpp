#include "bara/parser/Lexer.h"

namespace bara {

void Lexer::lex() {
  skipWhitespace();
  capture();
  auto ch = advance();
  switch (ch) {
    // clang-format off
  case eof:
    kind = Token::Tok_Eof; return;
  case '+':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_PlusEqual;
    } else
      kind = Token::Tok_Plus;
    return;

  case '-':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_MinusEqual;
    } else
      kind = Token::Tok_Minus;
    return;

  case '*':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_StarEqual;
    } else
      kind = Token::Tok_Star;
    return;

  case '/':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_SlashEqual;
    } else if (ch == '/') {
      while (peek() != '\n')
        skip();
      lex();
    } else 
      kind = Token::Tok_Slash;
    return;

  case '%':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_PercentEqual;
    } else
      kind = Token::Tok_Percent;
    return;

  case '=':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_EqualEqual;
    } else
      kind = Token::Tok_Equal;
    return;

  case '&':
    ch = peek();
    if (ch == '&') {
      skip();
      kind = Token::Tok_LogicalAnd;
    } else if (ch == '=') {
      skip();
      kind = Token::Tok_AmpersandEqual;
    } else 
      kind = Token::Tok_Ampersand;
    return;
  
  case '^':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_CaretEqual;
    } else
      kind = Token::Tok_Caret;
    return;

  case '<':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_Le;
    } else if (ch == '<') {
      skip();
      ch = peek();
      if (ch == '=') {
        skip();
        kind = Token::Tok_LShiftEqual;
      } else
        kind = Token::Tok_LShift;
    } else
      kind = Token::Tok_Lt;

  case '>':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_Ge;
    } else if (ch == '>') {
      skip();
      ch = peek();
      if (ch == '=') {
        skip();
        kind = Token::Tok_RShiftEqual;
      } else
        kind = Token::Tok_RShift;
    } else
      kind = Token::Tok_Gt;
    return;

  case '|':
    ch = peek();
    if (ch == '|') {
      skip();
      kind = Token::Tok_LogicalOr;
    } else if (ch == '=') {
      skip();
      kind = Token::Tok_VBarEqual;
    } else
      kind = Token::Tok_VBar;
    return;

  case ',':
    kind = Token::Tok_Comma;      return;
  case ';':
    kind = Token::Tok_Semicolon;  return;
  case '{':
    kind = Token::Tok_LBrace;     return;
  case '}':
    kind = Token::Tok_RBrace;     return;
  case '(':
    kind = Token::Tok_LParen;     return;
  case ')':
    kind = Token::Tok_RParen;     return;
  case '[':
    kind = Token::Tok_LBracket;   return;
  case ']':
    kind = Token::Tok_RBracket;   return;
  case '!':
    kind = Token::Tok_Bang;       return;
  case '~':
    kind = Token::Tok_Tilde;      return;

  case '"':
    lexString();                  return;

    // clang-format on
  default:
    if (isdigit(ch)) {
      lexNumber();
    } else if (isalpha(ch) || ch == '_') {
      lexIdentifier();
    } else {
      while (!std::isspace(peek()) && peek() != eof)
        skip();
      kind = Token::Tok_Unknown;
    }

    return;
  }
}

void Lexer::capture() {
  lastPos = pos;
  lastRow = row;
  lastCol = col;
  kind = Token::Kind::Tok_Unknown;
}

char Lexer::advance() {
  if (pos == buffer.size())
    return eof;

  auto ch = buffer[pos++];
  if (ch == '\n') {
    row++;
    col = 0;
  } else {
    col++;
  }
  return ch;
}

char Lexer::peek() {
  if (pos == buffer.size())
    return eof;
  return buffer[pos];
}

void Lexer::skip() { advance(); }

void Lexer::lexToTokenPos(size_t pos) {
  while (pos >= tokens.size()) {
    if (!tokens.empty() && tokens.back()->is<Token::Tok_Eof>())
      return;
    lex();
    createToken();
  }
}

void Lexer::skipWhitespace() {
  while (std::isspace(peek()))
    skip();
}

Token *Lexer::createToken() {
  auto start = SMLoc::getFromPointer(buffer.data() + lastPos);
  auto end = SMLoc::getFromPointer(buffer.data() + pos);
  auto range = SMRange(start, end);
  auto symbol = buffer.slice(lastPos, pos);
  auto token = Token::create(context, range, kind, symbol, lastRow, lastCol,
                             tokens.size());
  tokens.emplace_back(token);
  return token;
}

} // namespace bara
