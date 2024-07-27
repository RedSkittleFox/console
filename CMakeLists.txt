cmake_minimum_required(VERSION 3.21)

project(console)

if (PROJECT_IS_TOP_LEVEL)
    set_property(GLOBAL PROPERTY USE_FOLDERS ON)
    	
    set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin-lib")
    set(CMAKE_LIBRARY_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin-etc")
    set(CMAKE_RUNTIME_OUTPUT_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/bin")
    
    set(CMAKE_CXX_STANDARD 23)
    set(CMAKE_CXX_STANDARD_REQUIRED)

    option(
    	FOX_CONVAR_BUILD_SAMPLES
        "If samples are built."
    	ON
    )

    option(
	    FOX_CONVAR_BUILD_TESTS
        "If unit tests are built"
	    ON
    )
else()
    option(
    	FOX_CONVAR_BUILD_SAMPLES
        "If samples are built."
    	OFF
    )

    option(
	    FOX_CONVAR_BUILD_TESTS
        "If unit tests are built"
	    OFF
    )
endif()
    
set_property(GLOBAL PROPERTY USE_FOLDERS ON)

include(FetchContent)
add_subdirectory("include")
add_library(fox::console ALIAS console)

if (FOX_CONVAR_BUILD_SAMPLES)
	add_subdirectory("sample")
endif()

if (FOX_CONVAR_BUILD_TESTS)
	enable_testing()
	add_subdirectory("test")
endif()