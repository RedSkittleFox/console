cmake_minimum_required(VERSION 3.21)

set(sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/main.cpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_executable(
    console-sample-hello-console
    ${sources}
)

if(MSVC)
    target_compile_options(
    	console-sample-hello-console
	PRIVATE /W4 
	PRIVATE /MP 
	PRIVATE /arch:AVX2
	PRIVATE /WX
    )
endif()

target_include_directories(
    console-sample-hello-console
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(
    console-sample-hello-console
    fox::console
)