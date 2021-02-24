# Build FyTest library
add_subdirectory(fytest)

# Build individual tests
set(tests
  failing_fixture
  failing_suite
  fixture
  full
  parameterized
  parameterized2
  simple)

foreach(test IN LISTS tests)
  fytest_fypp_preprocess("" "test_${test}.fpp" sources-f90)
  add_executable(test_${test} ${sources-f90})
  target_link_libraries(test_${test} mymath FyTest::FyTest)
  add_test(NAME ${test} COMMAND test_${test})
endforeach()
