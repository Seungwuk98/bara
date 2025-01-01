set(TEST_EXECUTION_PY ${CMAKE_CURRENT_SOURCE_DIR}/test_algorithm.py)
set(TEST_VALGRIND_PY ${CMAKE_CURRENT_SOURCE_DIR}/test_valgrind.py)
set(TEST_CASE_GEN_PY ${CMAKE_CURRENT_SOURCE_DIR}/generate.py)

find_program(PYTHON python3 REQUIRED)
message(STATUS "Found Python3: ${PYTHON}")

if (BARA_VALGRIND_TEST)
  find_program(VALGRIND valgrind REQUIRED)
  message(STATUS "Found Valgrind: ${VALGRIND}")
endif()

function(add_algorithm algorithm)
  set(algorithm "test-${algorithm}")
  cmake_parse_arguments(ALG "" "ANSWER;BARA;TEST_CASE;CHECKER;ITERATION" "GC_THRESHOLDS" ${ARGN})
  
  set(test_name "${algorithm}-${ALG_TEST_CASE}")

  set(case_output_dir "${CMAKE_CURRENT_BINARY_DIR}/case/${ALG_TEST_CASE}")
  file(MAKE_DIRECTORY ${case_output_dir}/output/)

  add_custom_target(
    ${test_name} 
    COMMAND  ${CMAKE_COMMAND} -E make_directory ${case_output_dir}/output/
             &&
             ${PYTHON} ${TEST_EXECUTION_PY}  
             --case-dir=${case_output_dir} 
             --bara-interpreter=$<TARGET_FILE:bara> 
             --bara-file=${ALG_BARA} 
             --checker-file=$<TARGET_FILE:${ALG_CHECKER}>
    DEPENDS ${ALG_ANSWER} ${ALG_CHECKER} ${case_output_dir}/output/ ${test_name}_gen-test-case
    COMMENT "Running test:${test_name}"
  )

  if (BARA_VALGRIND_TEST)
    file(MAKE_DIRECTORY ${case_output_dir}/valgrind/)
    add_custom_target(
      ${test_name}_valgrind
      COMMAND  ${CMAKE_COMMAND} -E make_directory ${case_output_dir}/valgrind/
               &&
               ${PYTHON} ${TEST_VALGRIND_PY}  
               --case-dir=${case_output_dir} 
               --bara-interpreter=$<TARGET_FILE:bara> 
               --bara-file=${ALG_BARA} 
               --valgrind=${VALGRIND}
               --gc-threshold="${ALG_GC_THRESHOLDS}"
      DEPENDS ${ALG_ANSWER} ${ALG_CHECKER} ${test_name}_gen-test-case
      BYPRODUCTS ${case_output_dir}/valgrind/*
      COMMENT "Running valgrind test:${test_name}"
    )
  endif()

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

  set(GENERATORS "")
  set(GENERATOR_FILES "")
  set(GEN_INPUTS "")
  set(GEN_OUTPUTS "")
  set(GEN_COMMANDS "")
  foreach(LINE ${FILE_LINES})
    string(REGEX MATCH "^([^ ]+) (.+)" _match ${LINE})
    set(GENERATOR "${algorithm}-${CMAKE_MATCH_1}")

    list(APPEND GENERATORS ${GENERATOR})
    list(APPEND GENERATOR_FILES $<TARGET_FILE:${GENERATOR}>)

    set(COMMANDS ${CMAKE_MATCH_2})
    string(REGEX MATCH "([^>]+)>(.*)" _match ${COMMANDS})
    set(COMMANDS ${CMAKE_MATCH_1})
    set(OUTPUT ${CMAKE_MATCH_2})
    string(REGEX MATCHALL "[^ ]+" COMMANDS ${COMMANDS})
    string(STRIP ${OUTPUT} OUTPUT)

    list(APPEND GEN_INPUTS ${case_output_dir}/${OUTPUT})
    list(APPEND GEN_OUTPUTS ${case_output_dir}/${OUTPUT}.a)
    list(APPEND GEN_COMMANDS "{${COMMANDS}}")
  
    set(INPUT "${case_output_dir}/${OUTPUT}")
    set(OUTPUT "${case_output_dir}/${OUTPUT}.a")

    add_dependencies(${GEN_ANSWER} ${GENERATOR})
  endforeach()
  add_custom_target(
    "${algorithm}-${GEN_NAME}_gen-test-case"
    DEPENDS ${GENERATORS} ${GEN_ANSWER} ${GEN_SCRIPTS}
    BYPRODUCTS ${GEN_INPUTS} ${GEN_OUTPUTS}
    COMMAND ${CMAKE_COMMAND} -E make_directory ${case_output_dir}
            &&
            ${CMAKE_COMMAND} -E rm -rf ${case_output_dir}/*
            &&
            ${PYTHON} ${TEST_CASE_GEN_PY}
            --arguments="${GEN_COMMANDS}"
            --generators="${GENERATOR_FILES}"
            --answer=$<TARGET_FILE:${GEN_ANSWER}>
            --input-gen-files="${GEN_INPUTS}"
            --output-gen-files="${GEN_OUTPUTS}"

    COMMENT "Generating test case:${GEN_NAME}"
  )
endfunction()



add_subdirectory(MST)
add_subdirectory(PrefixSum)
add_subdirectory(Sort)

