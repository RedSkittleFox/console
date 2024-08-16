cmake_minimum_required(VERSION 3.21)

set(sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/fox/console.hpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_library(
    console
    INTERFACE
    ${sources}
)

target_include_directories(
    console
    INTERFACE
    ${CMAKE_CURRENT_SOURCE_DIR}
)

if(FOX_CONSOLE_ENABLE_EXCEPTIONS)
    target_compile_definitions(console INTERFACE FOX_CONSOLE_ENABLE_EXCEPTIONS)
endif()

if(FOX_CONSOLE_THREAD_SAFE)
    target_compile_definitions(console INTERFACE FOX_CONSOLE_THREAD_SAFE)
endif()