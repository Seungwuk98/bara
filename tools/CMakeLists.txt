add_executable(bara bara.cpp)

target_link_libraries(
  bara
  PRIVATE
  BaraAST
  BaraContext
  BaraParser
  ${llvm_libs}
)
