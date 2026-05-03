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
    src/api.c
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
        $<$<CONFIG:Release>:-O3>
    )
endif()

# CLI
if (PROJECT_IS_TOP_LEVEL)
    set(CLI_TARGET solus_cli)
    add_executable(${CLI_TARGET} src/cli.c)
    set_target_properties(${CLI_TARGET} PROPERTIES OUTPUT_NAME "solus")
    target_compile_definitions(${CLI_TARGET} PUBLIC _CRT_SECURE_NO_WARNINGS)
    target_link_libraries(${CLI_TARGET} PRIVATE ${PROJECT_NAME})
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

option(ENABLE_SANITIZERS "Enable sanitizers" OFF)
if (ENABLE_SANITIZERS)
    message(STATUS "Sanitizers enabled")
    set(SANITIZER_COMPILE_FLAGS
        -fsanitize=address,undefined
        -fno-omit-frame-pointer
        -g
        -O0
    )

    target_compile_options(sf-std PRIVATE ${SANITIZER_COMPILE_FLAGS})
    target_compile_options(${PROJECT_NAME} PRIVATE ${SANITIZER_COMPILE_FLAGS})
    if (PROJECT_IS_TOP_LEVEL)
        target_compile_options(${CLI_TARGET} PRIVATE ${SANITIZER_COMPILE_FLAGS})
        target_link_options(${CLI_TARGET} PRIVATE -fsanitize=address,undefined)
    endif()
endif()

if (WIN32)
    if (BUILD_SHARED_LIBS)
        set(CMAKE_SHARED_LIBRARY_PREFIX "")
        target_compile_definitions(${PROJECT_NAME} PUBLIC LIB_DYNAMIC)
        target_compile_definitions(${PROJECT_NAME} PRIVATE LIB_EXPORTS)
    endif()
    target_link_libraries(${PROJECT_NAME} PRIVATE Ws2_32)
endif()
