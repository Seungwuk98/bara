add_executable("test-sort_answer" sol.cpp)
add_executable("test-sort_checker" checker.cpp)



generator(
  "sort"
  NAME "gen_sort"
  FILE "generator/gen_sort.cpp"
)

generator(
  "sort"
  NAME "gen"
  FILE "generator/gen.cpp"
)

generator(
  "sort"
  NAME "big_gen_sort"
  FILE "generator/big_gen_sort.cpp"
)

generator(
  "sort"
  NAME "big_gen"
  FILE "generator/big_gen.cpp"
)

create_test_case(
  "sort"
  NAME "small"
  SCRIPTS "generator/small.txt"
  ANSWER "test-sort_answer"
)

create_test_case(
  "sort"
  NAME "big"
  SCRIPTS "generator/big.txt"
  ANSWER "test-sort_answer"
)


add_algorithm(
  "sort"
  TEST_CASE "small"
  CHECKER "test-sort_checker"
  ANSWER "test-sort_answer"
  BARA "${CMAKE_CURRENT_SOURCE_DIR}/MergeSort.bara"
)
#
# add_algorithm(
#   "Sort"
#   "big"
#   GENERATOR "gen" "gen_sort"
#   ANSWER "sol.cpp"
#   CHECKER "checker.cpp"
#   CASE "big"
#   BARA "MergeSort.bara"
# )
