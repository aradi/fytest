#:include 'fytest.fypp'

#:block TEST_SUITE('test_factorial')
  use factorial_module
  implicit none

  integer :: special_value = 1

#:contains

  #:block TEST('factorial_5')
    @:REQUIRE(factorial(5) == 120)
  #:endblock TEST


  #:block TEST('factorial_1')
    @:REQUIRE(factorial(1) == 0)
  #:endblock TEST


#!  #:block TEST_FIXTURE('special_cases')
#!
#!    integer :: special_result
#!
#!  #:contains
#!
#!    #:block TEST_INITIALIZER
#!      special_result = 1
#!      print *, 'TEST_INITIALIZER'
#!    #:endblock TEST_INITIALIZER
#!
#!    #:block TEST_FINALIZER
#!      print *, "TEST_FINALIZER"
#!    #:endblock TEST_FINALIZER
#!
#!  #:endblock TEST_FIXTURE
#!
#!
#!  #:block TEST('factorial_special_1', fixture='special_cases')
#!      @:ASSERT(factorial(special_value) == special_result)
#!  #:endblock TEST
#!
#!
#!  #:block TEST('factorial_special_0', fixture='special_cases')
#!      @:ASSERT(factorial(0) == special_result)
#!  #:endblock TEST


#:endblock TEST_SUITE


#:block TEST_DRIVER()
#:endblock TEST_DRIVER
