add_library(
  BaraAST
  STATIC 
  AST.cpp
)

target_link_libraries(
  BaraAST
  PRIVATE 
  BaraContext
)

