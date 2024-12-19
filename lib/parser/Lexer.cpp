#include "bara/parser/Lexer.h"
#include "llvm/ADT/StringSwitch.h"

namespace bara {

Token *Lexer::getNextToken(size_t index) {
  auto tokenPos = tokPos + index;
  auto nextTok = lexToTokenPos(tokenPos);
  tokPos = nextTok->getIndex() + 1;
  return nextTok;
}

Token *Lexer::peekToken(size_t index) {
  auto tokenPos = tokPos + index;
  return lexToTokenPos(tokenPos);
}

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
      while (peek() != '\n' && peek() != eof)
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
    } else if (ch == '>') {
      skip();
      kind = Token::Tok_RightArrow;
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
    return;

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

  case '!':
    ch = peek();
    if (ch == '=') {
      skip();
      kind = Token::Tok_NotEqual;
    } else
      kind = Token::Tok_Bang;       
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
  case '~':
    kind = Token::Tok_Tilde;      return;
  case '?':
    kind = Token::Tok_Question;   return;
  case ':':
    kind = Token::Tok_Colon;      return;
  case '\\':
    kind = Token::Tok_BackSlash;  return;

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

void Lexer::lexIdentifier() {
  while (isalnum(peek()) || peek() == '_')
    skip();
  auto symbol = buffer.slice(lastPos, pos);
  kind = llvm::StringSwitch<Token::Kind>(symbol)
#define KEYWORD(NAME) .Case(#NAME, Token::Tok_##NAME)
#include "bara/parser/Token.def"
             .Default(Token::Tok_Identifier);
}

void Lexer::lexNumber() {
  while (std::isdigit(peek()))
    skip();
  auto isFloat = false;
  if (peek() == '.') {
    skip();
    isFloat = true;
    while (std::isdigit(peek()))
      skip();
  }
  kind = isFloat ? Token::Tok_FloatLiteral : Token::Tok_IntegerLiteral;
}

void Lexer::lexString() {
  while (peek() != '"' && peek() != eof) {
    auto ch = peek();
    if (ch == '\\') {
      skip();
    } else if (ch == '\n') {
      report(createRange(), LexDiagnostic::error_unexpected_newline);
      kind = Token::Tok_Unknown;
      return;
    }
    skip();
  }
  if (peek() == eof) {
    report(createRange(), LexDiagnostic::error_unclosed_string);
    kind = Token::Tok_Unknown;
    return;
  }
  skip();
  kind = Token::Tok_StringLiteral;
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
    col = 1;
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

Token *Lexer::lexToTokenPos(size_t pos) {
  while (pos >= tokens.size()) {
    if (!tokens.empty() && tokens.back()->is<Token::Tok_Eof>())
      return tokens.back();
    lex();
    createToken();
  }
  return tokens[pos];
}

void Lexer::skipWhitespace() {
  while (std::isspace(peek()))
    skip();
}

SMRange Lexer::createRange() const {
  auto start = SMLoc::getFromPointer(buffer.data() + lastPos);
  auto end = SMLoc::getFromPointer(buffer.data() + pos);
  return SMRange(start, end);
}

Token *Lexer::createToken() {
  auto symbol = buffer.slice(lastPos, pos);
  auto range = createRange();
  auto token = Token::create(context, range, kind, symbol, lastRow, lastCol,
                             tokens.size());
  tokens.emplace_back(token);
  return token;
}

const char *lexDiagMsg[] = {
#define DIAG(Name, Msg, Error) Msg,
#include "bara/parser/LexerDiagnostic.def"
};

llvm::SourceMgr::DiagKind lexDiagKind[] = {
#define DIAG(Name, Msg, Error) llvm::SourceMgr::DiagKind::DK_##Error,
#include "bara/parser/LexerDiagnostic.def"
};

const char *Lexer::LexDiagnostic::getDiagMsg(Diag kind) {
  return lexDiagMsg[kind];
}

llvm::SourceMgr::DiagKind Lexer::LexDiagnostic::getDiagKind(Diag kind) {
  return lexDiagKind[kind];
}

} // namespace bara
