#! Demonstrates the usage of simple mpi tests.

#:include 'fytest.fypp'

#:block MPI_TEST_SUITE('simple')
  use mpi
  implicit none

  integer :: mycomm, myrank

#:contains

  #:block MPI_TEST_SUITE_INIT
    integer :: error

    mycomm = MPI_COMM_WORLD
    call mpi_comm_rank(mycomm, myrank, error)
    @:MPI_ASSERT_EQ(error, 0)

  #:endblock


  #! Testing whether broadcast from master arrives on all processes

  #:block MPI_TEST('bcast_const')

    integer :: localval
    integer :: error

    localval = 0
    @:MPI_ASSERT_EQ(localval, 0)
    if (myrank == 0) then
      localval = 1
    end if
    call mpi_bcast(localval, 1, MPI_INTEGER, 0, mycomm, error)
    @:MPI_ASSERT_EQ(error, 0)
    @:MPI_ASSERT_EQ(localval, 1)

  #:endblock


  #! Failing test for demonstration purpose

  #:block MPI_TEST('bcast_const_failing')

    integer :: localval
    integer :: error

    localval = mod(myrank, 2)
    #! This assert fails on odd nodes, but not on master and any other even node.
    @:MPI_ASSERT_EQ(localval, 0)
    print *, 'You should never see this line on any of the nodes!'

  #:endblock

#:endblock MPI_TEST_SUITE


#:block MPI_TEST_DRIVER()
#:endblock MPI_TEST_DRIVER
