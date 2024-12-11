add_library(
  BaraInterpreter
  STATIC
  Interpreter.cpp 
  Memory.cpp
  Value.cpp
  RvInterpreter.cpp
  LvInterpreter.cpp
)

target_link_libraries(
  BaraInterpreter
  PUBLIC
  BaraAST
  BaraContext
)
