cmake_minimum_required(VERSION 3.2)

project(ast)

set(PACKCC packcc CACHE FILEPATH "Specify file path of packcc command.")

add_custom_command(
    OUTPUT parser.c parser.h
    COMMAND ${PACKCC} ARGS -o parser ${CMAKE_CURRENT_SOURCE_DIR}/parser.peg
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/parser.peg
    VERBATIM
)

add_executable(ast main.c parser.c system.c utility.c)

target_compile_features(ast PRIVATE c_std_99)
target_include_directories(ast BEFORE PRIVATE ${CMAKE_CURRENT_SOURCE_DIR} ${CMAKE_CURRENT_BINARY_DIR})
