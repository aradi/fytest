#! Demonstrates the usage of parameterized test fixtures.

#:include 'fytest.fypp'

#:block TEST_SUITE('parameterized2')
  use mymath
  implicit none

  type :: fact_calc_t
    integer :: val
    integer :: expresult
  end type fact_calc_t

  #! This will contain the parameters of the tests, once TEST_SUITE_INIT() has been executed.
  type(fact_calc_t), allocatable :: factcalcs(:)

#:contains

  #! Initializes global test suite variables

  #:block TEST_SUITE_INIT()

    integer :: fd
    integer :: icalc, ncalc

    open(newunit=fd, file="factcalcs.dat", action="read", form="formatted")
    read(fd, *) ncalc
    allocate(factcalcs(ncalc))
    do icalc = 1, ncalc
      read(fd, *) factcalcs(icalc)%val, factcalcs(icalc)%expresult
    end do

  #:endblock


  #! Parameterized test, iterator runs over a given array. The array must be either a constant
  #! array, or must be initialized in the TEST_SUITE_INIT() routine.

  #:block TEST_FIXTURE('special', ITERATORS=[('factcalc', 'factcalcs')], RENDERER='render')

    type(fact_calc_t) :: factcalc

  #:contains

    #! Tests can access the fixture scope

    #:block TEST('testval')
      @:ASSERT(factorial(factcalc%val) == factcalc%expresult)
    #:endblock

    #! We define a renderer to show the number used in a given fixture
    #! A renderer must have no arguments and return a string containing a human
    #! readable representation of the fixture.
    function render() result(str)
      character(:), allocatable :: str

      character(10) :: buffer

      write(buffer, "(A,I0)") 'curval=', factcalc%val
      str = trim(buffer)

    end function render

  #:endblock TEST_FIXTURE

#:endblock TEST_SUITE


#:block TEST_DRIVER()
#:endblock TEST_DRIVER
