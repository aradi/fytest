#! Demonstrates failing asserts in fixture initialization and finalization

#:include 'fytest.fypp'

#:block TEST_SUITE('failing_fixture')
  use mymath
  implicit none

#:contains

  #! We use the test suite initializer to initialize the seed
  #! Note: This routine is called once before any tests are executed.
  #:block TEST_SUITE_INIT
    call random_seed()
  #:endblock


  #! Parameterized test, iterator runs over the range from 1 to 10.

  #:block TEST_FIXTURE('random', ITERATORS=[('iter', 3)], RENDERER='render')

    #! Iterator variables must be declared in the fixture scope
    integer :: iter

    #! Some other fixture variable
    integer :: curval

  #:contains

    #! Test fixture initializer is called each time before a test starts.
    #! If an ASSERT() within the initialization fails, the test is considered
    #! as failed and the test execution is stopped immediately.
    #:block TEST_FIXTURE_INIT

      real :: rand

      call random_number(rand)
      curval = int(rand * 10.0) + 1
      #! All initialiazations except the first one will fail.
      @:ASSERT(iter <= 1)

    #:endblock


    #! Test fixture finalizer is called each time after the test has finished.
    #! If an ASSERT() within the finalization fails, the test execution is stopped immediately,
    #! but the test status won't be changed (if the test was successfull, it remains successful.).
    #:block TEST_FIXTURE_FINAL

      @:ASSERT(.false.)
      #! This point should be never reached
      print *, "ERROR: reached a point beyond assert"
      stop

    #:endblock


    #! Tests can access the fixture scope

    #:block TEST('recursion_up')
      @:ASSERT_EQ(factorial(curval) * (curval + 1), factorial(curval + 1))
    #:endblock

    #:block TEST('recursion_down')
      @:ASSERT_EQ(factorial(curval), curval * factorial(curval - 1))
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
