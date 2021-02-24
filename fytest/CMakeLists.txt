find_program(FYPP fypp REQUIRED)
if (NOT FYPP)
  message(FATAL_ERROR "Preprocessor fypp not found!")
endif()

set(FYPP ${FYPP} PARENT_SCOPE)

set(FYTEST_FYPP_FLAGS "-I${CMAKE_CURRENT_SOURCE_DIR}")
if(FYTEST_WITH_MPI)
  list(APPEND FYTEST_FYPP_FLAGS "-Dfytest_with_mpi=True")
else()
  list(APPEND FYTEST_FYPP_FLAGS "-Dfytest_with_mpi=False")
endif()
set(FYTEST_FYPP_FLAGS "${FYTEST_FYPP_FLAGS}" PARENT_SCOPE)

# Add function to handle preprocessing via Fypp
function(fypp_preprocess preprocopts oldfiles newfiles)
  set(_newfiles)
  foreach(oldfile IN LISTS oldfiles)
    set(newfile "${oldfile}.f90")
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${newfile}
      COMMAND ${FYPP} ${preprocopts} ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile} ${CMAKE_CURRENT_BINARY_DIR}/${newfile}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile})
    list(APPEND _newfiles ${CMAKE_CURRENT_BINARY_DIR}/${newfile})
  endforeach()
  set(${newfiles} ${_newfiles} PARENT_SCOPE)
endfunction()

set(sources-fpp fytest.fpp)
fypp_preprocess("${FYPP_FLAGS};${FYTEST_FYPP_FLAGS}" "${sources-fpp}" sources-f90)
add_library(fytest ${sources-f90})

add_library(FyTest::FyTest INTERFACE IMPORTED GLOBAL)
target_link_libraries(FyTest::FyTest INTERFACE fytest)
target_include_directories(FyTest::FyTest INTERFACE ${CMAKE_CURRENT_BINARY_DIR})
