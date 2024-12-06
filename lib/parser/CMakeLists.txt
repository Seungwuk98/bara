add_library(
  BaraParser
  STATIC
  Lexer.cpp
  Token.cpp
  Parser.cpp
)

target_link_libraries(
  BaraParser 
  PRIVATE
  BaraAST
  BaraContext
  ${llvm_libs}
)
