cmake_minimum_required(VERSION 3.28)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_CXX_STANDARD 20)
project(bara)

find_package(LLVM REQUIRED CONFIG)
list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
include(AddLLVM)

llvm_map_components_to_libnames(llvm_libs support core irreader)

add_subdirectory(third-party)
include_directories(${PROJECT_SOURCE_DIR}/third-party/googletest/googletest/include)

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(include)
add_subdirectory(lib)
add_subdirectory(tools)
add_subdirectory(test)

