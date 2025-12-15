cmake_minimum_required(VERSION 4.1.2)

set(TARGET crafting-interpreters)

project(${TARGET} VERSION 1.0 LANGUAGES C)

set(CMAKE_C_STANDARD 23)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

add_executable(${TARGET} 
    main.c
)

target_include_directories(${TARGET} PRIVATE "${CMAKE_SOURCE_DIR}/include")

target_compile_options(${TARGET} PRIVATE
  -Wall
  -Wextra
  -Wpedantic
  -Wno-gnu-zero-variadic-macro-arguments

  $<$<CONFIG:Debug>:-O0;-g3;-fsanitize=address,undefined;-fno-omit-frame-pointer>
  $<$<CONFIG:Release>:-O2;-DNDEBUG>
)

target_link_options(${TARGET} PRIVATE
  $<$<CONFIG:Debug>:-fsanitize=address,undefined>
)

include(CTest)

find_program(NODE_EXECUTABLE node REQUIRED)

if(NODE_EXECUTABLE)
    add_test(NAME parser COMMAND ${NODE_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/tests/run-tests.js" parser)
    add_test(NAME eval COMMAND ${NODE_EXECUTABLE} "${CMAKE_CURRENT_SOURCE_DIR}/tests/run-tests.js" eval)
else()
    message(FATAL_ERROR "Node executable not found")
endif()
