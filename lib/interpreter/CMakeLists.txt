add_library(
  BaraInterpreter
  STATIC
  Interpreter.cpp 
  Memory.cpp
  Value.cpp
)

target_link_libraries(
  BaraInterpreter
  PUBLIC
  BaraAST
  BaraContext
)
