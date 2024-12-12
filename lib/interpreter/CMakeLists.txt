add_library(
  BaraInterpreter
  STATIC
  StmtInterpreter.cpp 
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
