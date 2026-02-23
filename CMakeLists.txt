cmake_minimum_required(VERSION 3.28)

set(CMAKE_EXPORT_COMPILE_COMMANDS ON)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_C_EXTENSIONS OFF)
set(LIBRARY_TYPE STATIC)

# LIB
project(solus C)
add_library(${PROJECT_NAME} ${LIBRARY_TYPE}
    src/bytecode.c
    src/compiler.c
    src/syntax.c
    src/vm.c
    src/val.c

    src/std/builtin.c
    src/std/io.c
    src/std/string.c
    src/std/obj.c
    src/std/_math.c # It has to be this way for headers
    src/std/gc.c
)
target_include_directories(${PROJECT_NAME} PUBLIC ${CMAKE_CURRENT_SOURCE_DIR}/include)
target_compile_definitions(${PROJECT_NAME} PUBLIC _CRT_SECURE_NO_WARNINGS)


add_compile_definitions(_CRT_SECURE_NO_WARNINGS)
if (MSVC) # Good luck if you're on MSVC
    add_compile_options(/W4 /WX)
elseif(UNIX)
    add_compile_options(
        -Wall -Wextra -Werror
        -Wpedantic -Wconversion -Wuninitialized
        -Wsign-conversion -Wnull-dereference
    )
endif()
target_compile_options(${PROJECT_NAME} PUBLIC ${COMPILE_OPTIONS})

if (PROJECT_IS_TOP_LEVEL)
    # CLI
    set(CLI_TARGET solus_cli)
    set(CLI_SOURCES src/cli/cli.c)
    if (WIN32 OR UNIX) # ncurses/pdcurses required
        list(APPEND CLI_SOURCES src/cli/dbg.c)
    endif()
    add_executable(${CLI_TARGET} ${CLI_SOURCES})
    set_target_properties(${CLI_TARGET} PROPERTIES OUTPUT_NAME "solus")
    target_compile_definitions(${CLI_TARGET} PUBLIC _CRT_SECURE_NO_WARNINGS)
    if(WIN32)
        find_package(unofficial-pdcurses CONFIG REQUIRED)
        target_link_libraries(${CLI_TARGET} PRIVATE ${PROJECT_NAME} unofficial::pdcurses::pdcurses)
    elseif(UNIX)
        find_package(Curses REQUIRED)
        target_link_libraries(${CLI_TARGET} PRIVATE ${PROJECT_NAME} ${CURSES_LIBRARIES})
    else()
        target_link_libraries(${CLI_TARGET} PRIVATE ${PROJECT_NAME})
    endif()
    target_compile_options(${CLI_TARGET} PUBLIC ${COMPILE_OPTIONS})
endif()


# Sanitizer
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

if (WIN32)
    if (BUILD_SHARED_LIBS)
        set(CMAKE_SHARED_LIBRARY_PREFIX "")
        target_compile_definitions(${PROJECT_NAME} PUBLIC LIB_DYNAMIC)
        target_compile_definitions(${PROJECT_NAME} PRIVATE LIB_EXPORTS)
    endif()
    target_link_libraries(${PROJECT_NAME} PRIVATE Ws2_32)
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
