function(add_unit_test test_name)
  cmake_parse_arguments(ARG "" "" "PRIVATE;PUBLIC" ${ARGN})
  message(STATUS "ARG_UNPARSED_ARGUMENTS: ${ARG_UNPARSED_ARGUMENTS}")
  message(STATUS "ARG_PRIVATE: ${ARG_PRIVATE}")
  add_executable(${test_name} ${ARG_UNPARSED_ARGUMENTS})
  target_link_libraries(${test_name} 
    PRIVATE 
    gtest gtest_main
    ${ARG_PRIVATE}
    PUBLIC
    ${ARG_PUBLIC}
  )
  add_test(${test_name} ${test_name})
endfunction()

add_subdirectory(ASTTests)
add_subdirectory(ParserTests)
