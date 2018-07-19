#:include 'fytest.fypp'

#:block TEST_MODULE('test_factorial')
  use factorial_module
  implicit none

  integer :: special_value = 1

#:contains

  #:block TEST_CASE('factorial_5')
    @:ASSERT(factorial(5) == 120)
  #:endblock TEST_CASE


  #:block TEST_CASE('factorial_1')
    @:ASSERT(factorial(1) == 0)
  #:endblock TEST_CASE

  #:block TEST_CASE('factorial_special')
    @:ASSERT(factorial(special_value) == 1)
  #:endblock TEST_CASE

#:endblock TEST_MODULE


#:block TEST_PROGRAM('test_driver')
#:endblock TEST_PROGRAM
