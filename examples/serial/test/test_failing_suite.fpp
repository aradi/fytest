#! Demonstrates a failing test suite initializer

#:include 'fytest.fypp'

#:block TEST_SUITE('failing_suite')
  use mymath
  implicit none

#:contains

  #! Using the test suite initializer to initialize suite.
  #! Since it will fail, none of the tests in the suite will be run.
  #:block TEST_SUITE_INIT
    call random_seed()
    @:ASSERT(.false.)
    print *, "Error: this line should be never reached"
    stop
  #:endblock


  #:block TEST_SUITE_FINAL
    print *, 'Error: This should be never reached, since init has failed'
    stop
  #:endblock


  #! Testing for various special factorial values

  #:block TEST_FIXTURE('random', RENDERER='render')

    #! Variables defined here can be accessed by each unit within the fixture
    integer :: curval

  #:contains

    #! Test fixture initializer is called each time before a test starts.
    #! A separate scope is created for each test.
    #:block TEST_FIXTURE_INIT
      real :: rand

      call random_number(rand)
      curval = int(rand * 10.0) + 1
    #:endblock

    #! Tests can access the fixture scope

    #:block TEST('recursion_up')
      @:ASSERT(factorial(curval) * (curval + 1) == factorial(curval + 1))
    #:endblock

    #:block TEST('recursion_down')
      @:ASSERT(factorial(curval) == curval * factorial(curval - 1))
    #:endblock

    #! We define a renderer to show the random number used in a given fixture
    #! A renderer must have no arguments and return a string containing a human
    #! readable representation of the fixture.
    function render() result(str)
      character(:), allocatable :: str

      character(10) :: buffer

      write(buffer, "(A,I0)") 'curval=', curval
      str = trim(buffer)

    end function render

  #:endblock TEST_FIXTURE

#:endblock TEST_SUITE


#:block TEST_DRIVER()
#:endblock TEST_DRIVER
