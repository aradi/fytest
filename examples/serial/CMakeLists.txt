cmake_minimum_required(VERSION 3.16)

project(FyTest_serial_example_simple LANGUAGES Fortran)

# Build the library
add_subdirectory(lib)

# Build and add tests
enable_testing()
add_subdirectory(test)
