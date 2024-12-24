add_subdirectory(googletest)

if (BARA_OPTIMIZED_BENCHMARK) 
  set(CMAKE_BUILD_TYPE Release)
  add_subdirectory(benchmark)
else()
  add_subdirectory(benchmark)
endif()


