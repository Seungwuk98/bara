add_subdirectory(unittest)
add_subdirectory(lit)

if (CMAKE_BUILD_TYPE STREQUAL "Release")
  add_subdirectory(benchmark)
endif()
