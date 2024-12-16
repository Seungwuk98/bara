#include "bara/diagnostic/Diagnostic.h"
#include "bara/parser/Lexer.h"
#include "llvm/Support/MemoryBuffer.h"
#include "llvm/Support/SourceMgr.h"
#include "gtest/gtest.h"
#include <format>

namespace bara {

bool runTest(
    StringRef code,
    vector<std::tuple<Token::Kind, StringRef, size_t, size_t>> expect) {
  llvm::SourceMgr sourceMgr;
  auto buffer = llvm::MemoryBuffer::getMemBuffer(code, "source");
  auto index = sourceMgr.AddNewSourceBuffer(std::move(buffer), llvm::SMLoc());
  auto bufferRef = sourceMgr.getMemoryBuffer(index)->getBuffer();

  ASTContext context;
  Diagnostic reporter(sourceMgr);

  Lexer lexer(reporter, &context, bufferRef);
  while (!lexer.peekToken()->is<Token::Tok_Eof>())
    lexer.getNextToken();

  auto tokens = lexer.getTokens();
  if (tokens.size() != expect.size()) {
    for (auto token : tokens)
      errs() << token->toString() << "\n";
    return false;
  }

  for (auto [index, token, expected] : llvm::enumerate(tokens, expect)) {
    const auto &[kind, symbol, row, col] = expected;

    if (auto expectedToken = Token({}, kind, symbol, row, col, index);
        *token != expectedToken) {
      sourceMgr.PrintMessage(token->getRange().Start, llvm::SourceMgr::DK_Error,
                             std::format("got: {}, expected: {} ",
                                         token->toString(),
                                         expectedToken.toString()),
                             {token->getRange()});
      return false;
    }
  }

  return true;
}

TEST(ParserTests, LexerTest) {

  ASSERT_TRUE(runTest(
      R"(
fn main() {
  var (a, b, c) = (1, 2, 3);
}
  )",
      {

          {Token::Tok_fn, "fn", 2, 1},
          {Token::Tok_Identifier, "main", 2, 4},
          {Token::Tok_LParen, "(", 2, 8},
          {Token::Tok_RParen, ")", 2, 9},
          {Token::Tok_LBrace, "{", 2, 11},
          {Token::Tok_var, "var", 3, 3},
          {Token::Tok_LParen, "(", 3, 7},
          {Token::Tok_Identifier, "a", 3, 8},
          {Token::Tok_Comma, ",", 3, 9},
          {Token::Tok_Identifier, "b", 3, 11},
          {Token::Tok_Comma, ",", 3, 12},
          {Token::Tok_Identifier, "c", 3, 14},
          {Token::Tok_RParen, ")", 3, 15},
          {Token::Tok_Equal, "=", 3, 17},
          {Token::Tok_LParen, "(", 3, 19},
          {Token::Tok_IntegerLiteral, "1", 3, 20},
          {Token::Tok_Comma, ",", 3, 21},
          {Token::Tok_IntegerLiteral, "2", 3, 23},
          {Token::Tok_Comma, ",", 3, 24},
          {Token::Tok_IntegerLiteral, "3", 3, 26},
          {Token::Tok_RParen, ")", 3, 27},
          {Token::Tok_Semicolon, ";", 3, 28},
          {Token::Tok_RBrace, "}", 4, 1},
          {Token::Tok_Eof, "", 5, 3}

      }));

  ASSERT_TRUE(runTest(
      R"(
10;
10.0;
"hello";
"hello \
    world";
)",
      {{Token::Tok_IntegerLiteral, "10", 2, 1},
       {Token::Tok_Semicolon, ";", 2, 3},
       {Token::Tok_FloatLiteral, "10.0", 3, 1},
       {Token::Tok_Semicolon, ";", 3, 5},
       {Token::Tok_StringLiteral, "\"hello\"", 4, 1},
       {Token::Tok_Semicolon, ";", 4, 8},
       {Token::Tok_StringLiteral, "\"hello \\\n    world\"", 5, 1},
       {Token::Tok_Semicolon, ";", 6, 11},
       {Token::Tok_Eof, "", 7, 1}

      }));

  ASSERT_TRUE(runTest(
      R"(
for var a = 10; a < 20; a += 1 {
  print(10)
}
)",
      {{Token::Tok_for, "for", 2, 1},
       {Token::Tok_var, "var", 2, 5},
       {Token::Tok_Identifier, "a", 2, 9},
       {Token::Tok_Equal, "=", 2, 11},
       {Token::Tok_IntegerLiteral, "10", 2, 13},
       {Token::Tok_Semicolon, ";", 2, 15},
       {Token::Tok_Identifier, "a", 2, 17},
       {Token::Tok_Lt, "<", 2, 19},
       {Token::Tok_IntegerLiteral, "20", 2, 21},
       {Token::Tok_Semicolon, ";", 2, 23},
       {Token::Tok_Identifier, "a", 2, 25},
       {Token::Tok_PlusEqual, "+=", 2, 27},
       {Token::Tok_IntegerLiteral, "1", 2, 30},
       {Token::Tok_LBrace, "{", 2, 32},
       {Token::Tok_Identifier, "print", 3, 3},
       {Token::Tok_LParen, "(", 3, 8},
       {Token::Tok_IntegerLiteral, "10", 3, 9},
       {Token::Tok_RParen, ")", 3, 11},
       {Token::Tok_RBrace, "}", 4, 1},
       {Token::Tok_Eof, "", 5, 1}

      }));
}

} // namespace bara
