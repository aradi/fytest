# Add function to handle preprocessing via Fypp
function(fytest_fypp_preprocess preprocopts oldfiles newfiles)
  set(_newfiles)
  get_property(fypp GLOBAL PROPERTY FYTEST_FYPP_PROGRAM)
  get_property(flags GLOBAL PROPERTY FYTEST_FYPP_FLAGS)
  foreach(oldfile IN LISTS oldfiles)
    set(newfile "${oldfile}.f90")
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${newfile}
      COMMAND ${fypp} ${preprocopts} ${flags} ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile} ${CMAKE_CURRENT_BINARY_DIR}/${newfile}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile})
    list(APPEND _newfiles ${CMAKE_CURRENT_BINARY_DIR}/${newfile})
  endforeach()
  set(${newfiles} ${_newfiles} PARENT_SCOPE)
endfunction()

find_program(FYPP fypp REQUIRED)
if (NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found!")
endif()
set_property(GLOBAL PROPERTY FYTEST_FYPP_PROGRAM "${FYPP}")

set(fytest_fypp_flags "-I${CMAKE_CURRENT_SOURCE_DIR}")
if(FYTEST_WITH_MPI)
  list(APPEND fytest_fypp_flags "-Dfytest_with_mpi=True")
else()
  list(APPEND fytest_fypp_flags "-Dfytest_with_mpi=False")
endif()
set_property(GLOBAL PROPERTY FYTEST_FYPP_FLAGS "${fytest_fypp_flags}")

set(sources-fpp fytest.fpp)
fytest_fypp_preprocess("" "${sources-fpp}" sources-f90)
add_library(fytest ${sources-f90})

add_library(FyTest::FyTest INTERFACE IMPORTED GLOBAL)
target_link_libraries(FyTest::FyTest INTERFACE fytest)
target_include_directories(FyTest::FyTest INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
