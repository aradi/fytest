#! Demonstrates the usage of simple tests.

#:include 'fytest.fypp'

#:block TEST_SUITE('simple')
  use factorial_module
  implicit none

#:contains

  #! Testing for various special factorial values

  #:block TEST('1')
    @:ASSERT_EQ(factorial(1), 1)
  #:endblock

  #:block TEST('5')
    @:ASSERT_EQ(factorial(5), 120)
  #:endblock

  #:block TEST('6')
    @:ASSERT_EQ(factorial(6), 720)
  #:endblock

  #! This one will fail in order to demonstrate test failing

  #:block TEST('0_failing')
    @:ASSERT_EQ(factorial(0), 0)
  #:endblock

  #! This should pass again

  #:block TEST('7')
    @:ASSERT(factorial(7) == 5040)
  #:endblock

#:endblock TEST_SUITE


#:block TEST_DRIVER()
#:endblock TEST_DRIVER
