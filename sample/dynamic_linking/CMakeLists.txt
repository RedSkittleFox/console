cmake_minimum_required(VERSION 3.21)

set(sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/main.cpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_executable(
    console-sample-dynamic-linking-exe
    ${sources}
)

if(MSVC)
    target_compile_options(
    	console-sample-dynamic-linking-exe
	PRIVATE /W4 
	PRIVATE /MP 
	PRIVATE /arch:AVX2
	PRIVATE /WX
    )
endif()

target_include_directories(
    console-sample-dynamic-linking-exe
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(
    console-sample-dynamic-linking-exe
    fox::console
)

set(lib_sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/main_lib.cpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${lib_sources})

add_library(
    console-sample-dynamic-linking-lib
    SHARED
    ${lib_sources}
)

if(MSVC)
    target_compile_options(
        console-sample-dynamic-linking-lib
	PRIVATE /W4 
	PRIVATE /MP 
	PRIVATE /arch:AVX2
	PRIVATE /WX
    )
endif()

target_include_directories(
    console-sample-dynamic-linking-lib
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(
    console-sample-dynamic-linking-lib
    fox::console
)

target_link_libraries(
    console-sample-dynamic-linking-exe
    console-sample-dynamic-linking-lib
)