#! Demonstrates the usage of simple tests.

#:include 'fytest.fypp'

#:block TEST_SUITE('simple')
  use mymath
  implicit none

#:contains

  #! Testing for various special factorial values

  #:block TEST('1')
    @:ASSERT(factorial(1) == 1)
  #:endblock

  #:block TEST('5')
    @:ASSERT(factorial(5) == 120)
  #:endblock

  #:block TEST('6')
    @:ASSERT(factorial(6) == 720)
  #:endblock

  #! This one will fail in order to demonstrate test failing

  #:block TEST('0_failing')
    @:ASSERT(factorial(0) == 0)
  #:endblock

  #! This should pass again

  #:block TEST('7')
    @:ASSERT(factorial(7) == 5040)
  #:endblock

#:endblock TEST_SUITE


@:TEST_DRIVER()
