add_executable("test-mst_answer" sol.cpp)
add_executable("test-mst_checker" checker.cpp)

generator(
  "mst"
  NAME "gen"
  FILE "generator/gen.cpp"
)

create_test_case(
  "mst"
  NAME "all"
  SCRIPTS "generator/case.txt"
  ANSWER "test-mst_answer"
)

add_algorithm(
  "mst"
  TEST_CASE "all"
  CHECKER "test-mst_checker"
  ANSWER "test-mst_answer"
  BARA "${CMAKE_CURRENT_SOURCE_DIR}/MST.bara"
  ITERATION 10
  GC_THRESHOLDS "1024;2048;4096;8192"
)
