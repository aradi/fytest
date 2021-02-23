set(sources-f90
  mymath.f90)

add_library(mymath ${sources-f90})

set(includedir ${CMAKE_CURRENT_BINARY_DIR}/include)
set_target_properties(mymath PROPERTIES Fortran_MODULE_DIRECTORY ${includedir})
target_include_directories(mymath PUBLIC ${includedir})
