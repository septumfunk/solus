cmake_minimum_required(VERSION 3.28)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
set(LIBRARY_TYPE STATIC)

project(solus C)
add_library(${PROJECT_NAME} ${LIBRARY_TYPE}
    src/bytecode.c
    src/solc.c
    src/std.c
    src/syntax.c
    src/vm.c
)
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)

# CLI
set(CLI_TARGET solus_cli)
add_executable(${CLI_TARGET}
    src/cli.c
)
set_target_properties(${CLI_TARGET} PROPERTIES OUTPUT_NAME "solus")
target_link_libraries(${CLI_TARGET} PRIVATE ${PROJECT_NAME})

option(ENABLE_SANITIZERS "Enable sanitizers" OFF)
if (ENABLE_SANITIZERS AND NOT MSVC)
    message(STATUS "Sanitizers enabled")
    set(SANITIZER_FLAGS
        -fsanitize=address
        -fsanitize=undefined
        -fno-omit-frame-pointer
        -g
    )
    target_compile_options(${PROJECT_NAME} PUBLIC ${SANITIZER_FLAGS})
    target_link_options(${PROJECT_NAME} PUBLIC ${SANITIZER_FLAGS})
    target_compile_definitions(${PROJECT_NAME} PUBLIC USE_SANITIZERS)
    set(SANITIZER_COMPILE_FLAGS ${SANITIZER_FLAGS})
    set(SANITIZER_DEFINE USE_SANITIZERS)
endif()

add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
if (MSVC)
    set(COMPILE_OPTIONS /W4 /WX /permissive- /sdl /wd4068)
else()
    set(COMPILE_OPTIONS
        -Wall -Wextra -Werror

        -Wpedantic -Wconversion -Wuninitialized
        -Wsign-conversion -Wnull-dereference
    )
endif()
target_compile_options(${PROJECT_NAME} PUBLIC ${COMPILE_OPTIONS})
target_compile_options(${PROJECT_NAME} PUBLIC ${COMPILE_OPTIONS})

if (WIN32)
    if (BUILD_SHARED_LIBS)
        set(CMAKE_SHARED_LIBRARY_PREFIX "")
        target_compile_definitions(${PROJECT_NAME} PUBLIC LIB_DYNAMIC)
        target_compile_definitions(${PROJECT_NAME} PRIVATE LIB_EXPORTS)
    endif()
endif()

# Fetch Dependencies
include(FetchContent)
FetchContent_Declare(
        sf-std
        GIT_REPOSITORY https://github.com/septumfunk/sf-std.git
        GIT_TAG master
)
FetchContent_MakeAvailable(sf-std)
target_link_libraries(${PROJECT_NAME} PUBLIC sf-std)

# CTest
if (PROJECT_IS_TOP_LEVEL)
    enable_testing()
    file(GLOB TEST_SRCS tests/*.c)
    foreach(TEST_SRC ${TEST_SRCS})
        get_filename_component(TEST_NAME ${TEST_SRC} NAME_WE)
        add_executable(${TEST_NAME} ${TEST_SRC})
        target_link_libraries(${TEST_NAME} PRIVATE ${PROJECT_NAME})
        target_compile_options(${TEST_NAME} PUBLIC ${COMPILE_OPTIONS})
        if (ENABLE_SANITIZERS AND NOT MSVC)
            target_compile_options(${TEST_NAME} PUBLIC ${SANITIZER_COMPILE_FLAGS})
            target_link_options(${TEST_NAME} PUBLIC ${SANITIZER_COMPILE_FLAGS})
            target_compile_definitions(${TEST_NAME} PUBLIC ${SANITIZER_DEFINE})
        endif()
        set_target_properties(${TEST_NAME} PROPERTIES
            RUNTIME_OUTPUT_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/tests
        )
        add_test(NAME ${TEST_NAME} COMMAND ${TEST_NAME})
        set_tests_properties(${TEST_NAME} PROPERTIES WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR} LABELS "Test;Fucker")
    endforeach()
endif()
