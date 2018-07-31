#:include 'fytest.fypp'

#:block TEST_SUITE('factorial')
  use factorial_module
  implicit none

  integer :: special_value = 5

#:contains

  #:block TEST_SUITE_INITIALIZER
    special_value = 1
  #:endblock TEST_SUITE_INITIALIZER


  #:block TEST('5')
    @:REQUIRE(factorial(5) == 120)
  #:endblock TEST


  #:block TEST('1')
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
    end subroutine initializer_helper

    #:block TEST_FIXTURE_INITIALIZER
      call initializer_helper()
    #:endblock TEST_FIXTURE_INITIALIZER

    #:block TEST_FIXTURE_FINALIZER
      call finalizer_helper()
    #:endblock TEST_FIXTURE_FINALIZER

    subroutine finalizer_helper()
      continue
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
