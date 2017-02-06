cmake_minimum_required(VERSION 2.8)

project (sft)
set(CMAKE_BUILD_TYPE Release)
enable_language (Fortran)

include_directories(${CMAKE_CURRENT_SOURCE_DIR}/include)
add_library(sft SHARED src/assert.f95 src/suit.f95 src/utils.f95)

add_subdirectory(${CMAKE_CURRENT_SOURCE_DIR}/examples)

add_custom_command(
    TARGET sft POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${CMAKE_SOURCE_DIR}/release/*.mod
    ${CMAKE_CURRENT_SOURCE_DIR}/include
)
add_custom_command(
    TARGET sft POST_BUILD
    COMMAND ${CMAKE_COMMAND} -E copy
    ${CMAKE_SOURCE_DIR}/release/*.so
    ${CMAKE_CURRENT_SOURCE_DIR}/lib
)

install(TARGETS sft
        LIBRARY DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/lib    
        INCLUDES DESTINATION ${CMAKE_CURRENT_SOURCE_DIR}/include
)

