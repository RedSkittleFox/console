cmake_minimum_required(VERSION 3.21)

set(sources 
    "${CMAKE_CURRENT_SOURCE_DIR}/main.cpp"
)

source_group(TREE ${CMAKE_CURRENT_SOURCE_DIR} FILES ${sources})

add_executable(
    console-sample-critical-section
    ${sources}
)

if(MSVC)
    target_compile_options(
    	console-sample-critical-section
	PRIVATE /W4 
	PRIVATE /MP 
	PRIVATE /arch:AVX2
	PRIVATE /WX
    )
endif()

target_include_directories(
    console-sample-critical-section
    PUBLIC
    ${CMAKE_CURRENT_SOURCE_DIR}
)

target_link_libraries(
    console-sample-critical-section
    fox::console
)