set(TEST_EXECUTION_PY ${CMAKE_CURRENT_SOURCE_DIR}/test_algorithm.py)

find_program(PYTHON python3 REQUIRED)
message(STATUS "Found Python3: ${PYTHON}")

function(add_algorithm algorithm)
  set(algorithm "test-${algorithm}")

  cmake_parse_arguments(ALG "" "ANSWER;BARA;TEST_CASE;CHECKER;ITERATION" "" ${ARGN})
  set(test_name "${algorithm}-${ALG_TEST_CASE}")

  set(case_output_dir "${CMAKE_CURRENT_BINARY_DIR}/case/${ALG_TEST_CASE}")
  file(MAKE_DIRECTORY ${case_output_dir}/output/)

  add_custom_target(
    ${test_name} 
    COMMAND ${PYTHON} ${TEST_EXECUTION_PY}  
             --case-dir=${case_output_dir} 
             --bara-interpreter=$<TARGET_FILE:bara> 
             --bara-file=${ALG_BARA} 
             --checker-file=$<TARGET_FILE:${ALG_CHECKER}>
             --iteration=${ALG_ITERATION}
    DEPENDS ${ALG_ANSWER} ${ALG_CHECKER} ${case_output_dir}/output/
    COMMENT "Running test:${test_name}"
  )
endfunction()

function(generator algorithm) 
  cmake_parse_arguments(GEN "" "FILE;NAME" "" ${ARGN})
  set(generator "test-${algorithm}-${GEN_NAME}")
  add_executable(${generator} ${GEN_FILE})
endfunction()

function(create_test_case algorithm)
  set(algorithm "test-${algorithm}")
  cmake_parse_arguments(GEN "" "SCRIPTS;NAME;ANSWER" "" ${ARGN})
  file(READ ${GEN_SCRIPTS} FILE_CONTENTS)
  string(REGEX MATCHALL "[^\r\n]+" FILE_LINES ${FILE_CONTENTS})
  set(case_output_dir "${CMAKE_CURRENT_BINARY_DIR}/case/${GEN_NAME}")
  file(MAKE_DIRECTORY ${case_output_dir})

  foreach(LINE ${FILE_LINES})
    string(REGEX MATCH "^([^ ]+) (.+)" _match ${LINE})
    set(GENERATOR "${algorithm}-${CMAKE_MATCH_1}")
    set(COMMANDS ${CMAKE_MATCH_2})
    string(REGEX MATCH "([^>]+)>(.*)" _match ${COMMANDS})
    set(COMMANDS ${CMAKE_MATCH_1})
    set(OUTPUT ${CMAKE_MATCH_2})
    string(REGEX MATCHALL "[^ ]+" COMMANDS ${COMMANDS})
    string(STRIP ${OUTPUT} OUTPUT)
    set(COMMANDS ${GENERATOR};${COMMANDS})
  

    add_custom_command(
      TARGET "${GENERATOR}"
      POST_BUILD
      COMMAND ${COMMANDS} > ${case_output_dir}/${OUTPUT}
      DEPENDS ${case_output_dir}/${OUTPUT}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Generating case:${case_name} ${OUTPUT}"
    )

    set(INPUT "${case_output_dir}/${OUTPUT}")
    set(OUTPUT "${case_output_dir}/${OUTPUT}.a")

    add_dependencies(${GEN_ANSWER} ${GENERATOR})
    add_custom_command(
      TARGET ${GEN_ANSWER} 
      POST_BUILD
      COMMAND $<TARGET_FILE_NAME:${GEN_ANSWER}> < ${INPUT} > ${OUTPUT}
      DEPENDS ${INPUT} ${GENERATOR}
      WORKING_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}
      COMMENT "Generating answer:${case_name} ${OUTPUT}"
    )
  endforeach()
endfunction()



add_subdirectory(MST)
add_subdirectory(PrefixSum)
add_subdirectory(Sort)

