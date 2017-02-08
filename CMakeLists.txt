cmake_minimum_required(VERSION 2.8)

project (sft)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)
set(PRERELEASE ${CMAKE_BINARY_DIR})

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
add_library(sft SHARED src/assert.f95 src/suite.f95 src/utils.f95)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/examples)

install(TARGETS sft
        LIBRARY DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/lib    
        INCLUDES DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/include
)

