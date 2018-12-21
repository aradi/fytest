#:include 'fytest.fypp'

#:block TEST_SUITE('factorial')
  use factorial_module
  implicit none

  integer :: special_value = 5
  integer, parameter :: paramarray(3) = [9, -4, 3]

#:contains

  #:block TEST_SUITE_INIT
    special_value = 1
  #:endblock TEST_SUITE_INIT


  #:block TEST('5')
    @:ASSERT(factorial(5) == 120)
  #:endblock TEST


  #:block TEST('1')
    @:ASSERT(factorial(1) == 0)
  #:endblock TEST


  #:block TEST_FIXTURE('special_cases',&
      & ITERATORS=[('param1', 'paramarray'), ('param2', 'paramarray')])

    integer :: param1, param2
    integer :: special_result

  #:contains

    subroutine initializer_helper()
      special_result = 1
    end subroutine initializer_helper

    #:block TEST_FIXTURE_INIT
      call initializer_helper()
    #:endblock TEST_FIXTURE_INIT

    #:block TEST_FIXTURE_FINAL
      call finalizer_helper()
    #:endblock TEST_FIXTURE_FINAL

    subroutine finalizer_helper()
      continue
    end subroutine finalizer_helper

    #:block TEST('factorial_special_1')
      @:ASSERT(factorial(special_value) == special_result)
    #:endblock TEST

    #:block TEST('factorial_special_0')
      @:ASSERT(factorial(0) == special_result)
    #:endblock TEST

  #:endblock TEST_FIXTURE


  #:block TEST('6')
    @:ASSERT(factorial(6) == 720)
  #:endblock TEST


#:endblock TEST_SUITE


#:block TEST_DRIVER()
#:endblock TEST_DRIVER
