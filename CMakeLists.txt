cmake_minimum_required(VERSION 3.15)
project(luau-format LANGUAGES CXX)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)

set(LUAU_BUILD_CLI OFF CACHE BOOL "" FORCE)
set(LUAU_BUILD_TESTS OFF CACHE BOOL "" FORCE)

include(FetchContent)
FetchContent_Declare(
    luau
    GIT_REPOSITORY https://github.com/luau-lang/luau.git
    GIT_TAG        0.630
)
FetchContent_MakeAvailable(luau)

add_executable(luau-format
    "${CMAKE_CURRENT_SOURCE_DIR}/src/cli.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/simplifier.cpp"
    "${CMAKE_CURRENT_SOURCE_DIR}/src/formatter.cpp"

    "${CMAKE_CURRENT_SOURCE_DIR}/main.cpp"
)

target_include_directories(luau-format PUBLIC "${CMAKE_CURRENT_SOURCE_DIR}/include")

target_link_libraries(luau-format PRIVATE
    Luau.Analysis
    Luau.Ast
    Luau.Compiler
    Luau.VM
)

target_compile_options(Luau.Analysis PRIVATE
    -include cstdint
)
