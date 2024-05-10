cmake_minimum_required(VERSION 3.21)

set(sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/main.cpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_executable(
    convar-demo
    ${sources}
)

if(MSVC)
    target_compile_options(
	convar-demo
        PRIVATE /WX # all warnings as errors 
        PRIVATE /MP # multi-processor compilation
    )
endif()

target_include_directories(
    convar-demo
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(
    convar-demo
    convar
)