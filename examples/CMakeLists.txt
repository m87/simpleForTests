cmake_minimum_required(VERSION 2.8)

project (examples)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

LINK_DIRECTORIES(${PRERELEASE})
include_directories(${PRERELEASE})      

add_executable(assert.example assert.f95)
target_link_libraries(assert.example sft)

set(UNIT_TEST assert.example)
add_custom_command(
    TARGET ${UNIT_TEST}
    COMMENT "Run tests"
    POST_BUILD 
    COMMAND ${UNIT_TEST}
)
