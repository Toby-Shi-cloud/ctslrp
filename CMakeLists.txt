cmake_minimum_required(VERSION 3.25)

project(ctslrp)

set(CMAKE_CXX_STANDARD 20)

include_directories(lib/ctre/single-header)
include_directories(include)

add_executable(test test/test.cpp)
