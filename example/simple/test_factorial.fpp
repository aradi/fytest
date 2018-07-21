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


  #:block TEST_FIXTURE('special_cases',&
      & PARAMETERS=[('param1', 'paramarray'), ('param2', 'paramarray')])

    integer :: param1, param2
    integer, parameter :: paramarray(3) = [9, -4, 3]
    integer :: special_result

  #:contains

    subroutine initializer_helper()
      special_result = 1
      print *, 'TEST_INITIALIZER, param1, param2:', param1, param2
    end subroutine initializer_helper

    #:block TEST_INITIALIZER
      call initializer_helper()
    #:endblock TEST_INITIALIZER

    #:block TEST_FINALIZER
      call finalizer_helper()
    #:endblock TEST_FINALIZER

    subroutine finalizer_helper()
      print *, "TEST_FINALIZER"
    end subroutine finalizer_helper

  #:endblock TEST_FIXTURE


  #:block TEST('factorial_special_1', FIXTURE='special_cases')
    @:REQUIRE(factorial(special_value) == special_result)
  #:endblock TEST


  #:block TEST('factorial_special_0', FIXTURE='special_cases')
    @:REQUIRE(factorial(0) == special_result)
  #:endblock TEST


#:endblock TEST_SUITE


#:block TEST_DRIVER()
#:endblock TEST_DRIVER
