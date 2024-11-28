add_executable(bara bara.cpp)

target_link_libraries(
  bara
  PRIVATE
  ${llvm_libs}
)
