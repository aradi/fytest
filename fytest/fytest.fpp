module fytest
  use, intrinsic :: iso_fortran_env
  #:if fytest_with_mpi
    use mpi
  #:endif
  implicit none

  private
  public :: fytest_GlobalEnv, fytest_GlobalEnv_init, fytest_GlobalEnv_final
  public :: fytest_SuiteContainer, fytest_Suite
  public :: fytest_Fixture
  public :: fytest_TestContainer, fytest_TestStatus
  public :: fytest_TestContext, fytest_TestPhases
  public :: fytest_Timer
  public :: fytest_TestLogger, fytest_TestLogger_init
  public :: fytest_initSuites, fytest_finalSuites, fytest_getSuites
  public :: fytest_getFlatTestArray, fytest_runTests
  public :: fytest_generateFinalReport
  public :: fytest_nTestStatus
  public :: fytest_lib_mode

  #:if fytest_with_mpi
    character(*), parameter :: fytest_lib_mode = 'mpi'
  #:else
    character(*), parameter :: fytest_lib_mode = 'serial'
  #:endif


  #:if fytest_with_mpi
    type :: fytest_MpiEnv
      integer :: comm
      integer :: commSize, myRank, rootRank
    end type fytest_MpiEnv
  #:endif


  type :: fytest_GlobalEnv
    logical :: isMaster = .true.
    #:if fytest_with_mpi
      type(fytest_MpiEnv) :: mpi
    #:endif
  contains
    procedure :: synchronize => fytest_GlobalEnv_synchronize
  end type fytest_GlobalEnv


  type :: TestStatusHelper
    integer :: testSucceded = 1
    integer :: testFailed = 2
  end type TestStatusHelper

  integer, parameter :: fytest_nTestStatus = 2

  type(TestStatusHelper), parameter :: fytest_TestStatus = TestStatusHelper()

  character(10), parameter :: statusChars(fytest_nTestStatus) = [&
      & character(10) :: "Success", "FAILED"]


  type :: TestPhasesHelper
    integer :: uninit = 0
    integer :: init = 1
    integer :: test = 2
    integer :: final = 3
    integer :: done = 4
  end type TestPhasesHelper

  type(TestPhasesHelper), parameter :: fytest_TestPhases = TestPhasesHelper()


  type :: fytest_TestException
    character(:), allocatable :: file
    integer :: line
    character(:), allocatable :: message
    #:if fytest_with_mpi
      integer :: node
    #:endif
  contains
    #:if fytest_with_mpi
      procedure :: mpiSend => fytest_TestException_mpiSend
      procedure :: mpiRecv => fytest_TestException_mpiRecv
    #:endif
  end type fytest_TestException


  type :: fytest_TestContext
    type(fytest_GlobalEnv) :: env
    integer :: status = fytest_TestStatus%testSucceded
    integer :: statusGlobal = fytest_TestStatus%testSucceded
    integer :: phase = fytest_TestPhases%uninit
    character(:), allocatable :: textRepr
    type(fytest_TestException), allocatable :: exceptions(:)
  contains
    procedure :: setPhase => fytest_TestContext_setPhase
    procedure :: setTextRepr => fytest_TestContext_setTextRepr
    procedure :: assertFailed => fytest_TestContext_assertFailed
    procedure :: signalizeFailure => fytest_TestContext_signalizeFailure
    procedure :: isValid => fytest_TestContext_isValid
    procedure :: synchronizeExceptions => fytest_TestContext_synchronizeExceptions
  end type fytest_TestContext


  abstract interface
    subroutine testRunnerIface(testName, testParams, testContext)
      import :: fytest_TestContext
      character(*), intent(in) :: testName
      integer, intent(in) :: testParams(:)
      type(fytest_TestContext), intent(inout) :: testContext
    end subroutine testRunnerIface
  end interface


  type :: VarChar
    character(:), allocatable :: str
  end type VarChar


  type :: fytest_Fixture
    character(:), allocatable :: name
    type(VarChar), allocatable :: tests(:)
    integer, allocatable :: paramBounds(:,:)
    procedure(testRunnerIface), pointer, nopass :: runTest => null()
  end type fytest_Fixture


  type :: fytest_Suite
    character(:), allocatable :: name
    character(:), allocatable :: fileName
    type(fytest_Fixture), allocatable :: fixtures(:)
  end type fytest_Suite


  abstract interface
    subroutine suiteInitIface(context)
      import :: fytest_TestContext
      type(fytest_TestContext), intent(inout) :: context
    end subroutine suiteInitIface

    subroutine suiteFinalIface(context)
      import :: fytest_TestContext
      type(fytest_TestContext), intent(inout) :: context
    end subroutine suiteFinalIface

    subroutine getSuiteIface(suite)
      import :: fytest_Suite
      type(fytest_Suite), intent(out) :: suite
    end subroutine getSuiteIface
  end interface


  type :: fytest_SuiteContainer
    character(:), allocatable :: name
    type(fytest_TestContext) :: suiteContext
    procedure(suiteInitIface), pointer, nopass :: initSuite => null()
    procedure(suiteFinalIface), pointer, nopass :: finalSuite => null()
    procedure(getSuiteIface), pointer, nopass :: getSuite => null()
  end type fytest_SuiteContainer


  type :: fytest_TestContainer
    character(:), allocatable :: fileName, suiteName, fixtureName, testName
    integer, allocatable :: testParams(:)
    procedure(testRunnerIface), pointer, nopass :: runTest => null()
    type(fytest_TestContext) :: testContext
  end type fytest_TestContainer


  type :: fytest_TestLogger
    integer :: fdOut = output_unit
  contains
    procedure :: writeError => fytest_TestLogger_writeError
    procedure :: logSuiteInit => fytest_TestLogger_logSuiteInit
    procedure :: logSuiteInitResult => fytest_TestLogger_logSuiteInitResult
    procedure :: logFailedSuiteInit => fytest_TestLogger_logFailedSuiteInit
    procedure :: logTestRun => fytest_TestLogger_logTestRun
    procedure :: logTestResult => fytest_TestLogger_logTestResult
    procedure :: logFailedTest => fytest_TestLogger_logFailedTest
    procedure :: logStatus => fytest_TestLogger_logStatus
    procedure :: logTestStatistics => fytest_TestLogger_logTestStatistics
  end type fytest_TestLogger


  type :: fytest_Timer
    integer :: startCount, endCount, countRate
    real :: startTime, endTime
  contains
    procedure :: start => fytest_Timer_start
    procedure :: stop => fytest_Timer_stop
    procedure :: getCpuTime => fytest_Timer_getCpuTime
    procedure :: getWallClockTime => fytest_Timer_getWallClockTime
  end type fytest_Timer


contains

  subroutine fytest_GlobalEnv_init(this)
    type(fytest_GlobalEnv), intent(out) :: this

    #:if fytest_with_mpi
      integer :: error
      integer :: myRank
    #:endif

    #:if fytest_with_mpi
      call mpi_init(error)
      if (error /= 0) then
        stop "Could not initialize MPI framework"
      end if
      this%mpi%comm = MPI_COMM_WORLD
      call mpi_comm_size(this%mpi%comm, this%mpi%commSize, error)
      if (error /= 0) then
        stop "Could not initialize MPI framework"
      end if
      call mpi_comm_rank(this%mpi%comm, this%mpi%myRank, error)
      if (error /= 0) then
        stop "Could not initialize MPI framework"
      end if
      this%mpi%rootRank = 0
      this%isMaster = (this%mpi%myRank == this%mpi%rootRank)
    #:endif

  end subroutine fytest_GlobalEnv_init


  subroutine fytest_GlobalEnv_final(this)
    type(fytest_GlobalEnv), intent(inout) :: this

    #:if fytest_with_mpi
      integer :: error
    #:endif

    #:if fytest_with_mpi
      call mpi_barrier(this%mpi%comm, error)
      call mpi_finalize(error)
    #:endif

  end subroutine fytest_GlobalEnv_final


  subroutine fytest_GlobalEnv_synchronize(this)
    class(fytest_GlobalEnv), intent(inout) :: this

    #:if fytest_with_mpi
      integer :: error
    #:endif

    #:if fytest_with_mpi
      call mpi_barrier(this%mpi%comm, error)
    #:endif

  end subroutine fytest_GlobalEnv_synchronize


  subroutine fytest_initSuites(suiteContainers, logger)
    type(fytest_SuiteContainer), intent(inout) :: suiteContainers(:)
    type(fytest_TestLogger), optional, intent(inout) :: logger

    integer :: iSuite

    do iSuite = 1, size(suiteContainers)
      if (associated(suiteContainers(iSuite)%initSuite)) then
        call suiteContainers(iSuite)%suiteContext%setPhase(fytest_TestPhases%init)
        if (present(logger)) then
          call logger%logSuiteInit(suiteContainers(iSuite))
        end if
        call suiteContainers(iSuite)%initSuite(suiteContainers(iSuite)%suiteContext)
        if (present(logger)) then
          call logger%logSuiteInitResult(suiteContainers(iSuite))
        end if
        if (.not. suiteContainers(iSuite)%suiteContext%isValid()) then
          cycle
        end if
        call suiteContainers(iSuite)%suiteContext%setPhase(fytest_TestPhases%test)
      end if
    end do

  end subroutine fytest_initSuites


  subroutine fytest_finalSuites(suiteContainers)
    type(fytest_SuiteContainer), intent(inout) :: suiteContainers(:)

    integer :: iSuite

    do iSuite = 1, size(suiteContainers)
      if (.not. suiteContainers(iSuite)%suiteContext%isValid()) then
        cycle
      end if
      if (associated(suiteContainers(iSuite)%finalSuite)) then
        call suiteContainers(iSuite)%suiteContext%setPhase(fytest_TestPhases%final)
        call suiteContainers(iSuite)%finalSuite(suiteContainers(iSuite)%suiteContext)
      end if
    end do

  end subroutine fytest_finalSuites


  subroutine fytest_getSuites(suiteContainers, suites)
    type(fytest_SuiteContainer), intent(in) :: suiteContainers(:)
    type(fytest_Suite), allocatable, intent(out) :: suites(:)

    integer :: iSuite, nSuites, ind

    nSuites = 0
    do iSuite = 1, size(suiteContainers)
      if (suiteContainers(iSuite)%suiteContext%isValid()) then
        nSuites = nSuites + 1
      end if
    end do
    allocate(suites(nSuites))
    ind = 1
    do iSuite = 1, size(suiteContainers)
      if (suiteContainers(iSuite)%suiteContext%isValid()) then
        call suiteContainers(iSuite)%getSuite(suites(ind))
        ind = ind + 1
      end if
    end do

  end subroutine fytest_getSuites


  subroutine fytest_TestContainer_initialize(this, suite, fixture, iTest, iParamTest)
    type(fytest_TestContainer), intent(out) :: this
    type(fytest_Suite), intent(in) :: suite
    type(fytest_Fixture), intent(in) :: fixture
    integer, intent(in) :: iTest
    integer, intent(in) :: iParamTest

    this%fileName = suite%fileName
    this%suiteName = suite%name
    this%fixtureName = fixture%name
    this%testName = fixture%tests(iTest)%str
    this%runTest => fixture%runTest
    if (iParamTest > 0) then
      this%testParams = getParams(iParamTest, fixture%paramBounds)
    else
      this%testParams = [integer ::]
    end if

  end subroutine fytest_TestContainer_initialize


  subroutine fytest_TestContext_setPhase(this, phase)
    class(fytest_TestContext), intent(inout) :: this
    integer, intent(in) :: phase

    this%phase = phase

  end subroutine fytest_TestContext_setPhase


  subroutine fytest_TestContext_setTextRepr(this, textRepr)
    class(fytest_TestContext), intent(inout) :: this
    character(*), intent(in) :: textRepr

    this%textRepr = textRepr

  end subroutine fytest_TestContext_setTextRepr


  function fytest_TestContext_assertFailed(this, cond, file, line, msg) result(failedGlobal)
    class(fytest_TestContext), intent(inout) :: this
    logical, intent(in) :: cond
    character(*), intent(in) :: file
    integer, intent(in) :: line
    character(*), intent(in) :: msg
    logical :: failedGlobal

    logical :: condGlobal, failed
    integer :: error

    failed = .not. cond
    #:if fytest_with_mpi
      call mpi_allreduce(cond, condGlobal, 1, MPI_LOGICAL, MPI_LAND, this%env%mpi%comm, error)
      failedGlobal = .not. condGlobal
    #:else
      failedGlobal = failed
    #:endif
    call this%signalizeFailure(failed, failedGlobal, file, line, msg)

  end function fytest_TestContext_assertFailed


  subroutine fytest_TestContext_signalizeFailure(this, failed, failedGlobal, file, line, msg)
    class(fytest_TestContext), intent(inout) :: this
    logical, intent(in) :: failed
    logical, intent(in) :: failedGlobal
    character(*), intent(in) :: file
    integer, intent(in) :: line
    character(*), intent(in) :: msg

    type(fytest_TestException) :: exception

    #! Assertions during finalization are recorded but not considered as failure
    if (this%phase /= fytest_TestPhases%final) then
      if (failed) then
        this%status = fytest_TestStatus%testFailed
      end if
      if (failedGlobal) then
        this%statusGlobal = fytest_TestStatus%testFailed
      end if
    end if
    if (failed) then
      #:if fytest_with_mpi
        exception = fytest_TestException(file, line, msg, this%env%mpi%myRank)
      #:else
        exception = fytest_TestException(file, line, msg)
      #:endif
      this%exceptions = [exception]
    end if
    if (failedGlobal) then
      call this%synchronizeExceptions()
    end if

  end subroutine fytest_TestContext_signalizeFailure


  subroutine fytest_TestContext_synchronizeExceptions(this)
    class(fytest_TestContext), intent(inout) :: this

    #:if fytest_with_mpi
      integer :: iProc
      type(fytest_TestException) :: exception
      integer, allocatable :: nExceptions(:)
      integer :: nException, iExc
      integer :: error
    #:endif

    #:if fytest_with_mpi
      allocate(nExceptions(0 : this%env%mpi%commSize - 1))
      if (allocated(this%exceptions)) then
        nException = size(this%exceptions)
      else
        nException = 0
      end if
      call mpi_gather(nException, 1, MPI_INTEGER, nExceptions, 1, MPI_INTEGER, 0,&
          & this%env%mpi%comm, error)
      if (this%env%isMaster) then
        if (.not. allocated(this%exceptions)) then
          allocate(this%exceptions(0))
        end if
        do iProc = 1, this%env%mpi%commSize - 1
          do iExc = 1, nExceptions(iProc)
            call exception%mpiRecv(this%env, iProc)
            this%exceptions = [this%exceptions, exception]
          end do
        end do
      else
        do iExc = 1, nException
          call this%exceptions(iExc)%mpiSend(this%env, 0)
        end do
      end if
    #:endif

  end subroutine fytest_TestContext_synchronizeExceptions


  function fytest_TestContext_isValid(this) result(isValid)
    class(fytest_TestContext), intent(in) :: this
    logical :: isValid

    isValid = (this%status /= fytest_TestStatus%testFailed)

  end function fytest_TestContext_isValid


  subroutine fytest_getFlatTestArray(suites, testContainers)
    type(fytest_Suite), intent(in) :: suites(:)
    type(fytest_TestContainer), allocatable, intent(out) :: testContainers(:)

    integer :: iSuite, iFixture, iTest, nParamTests, iParam, ind
    integer :: nAllTests

    nAllTests = 0
    do iSuite = 1, size(suites)
      associate (suite => suites(iSuite))
        do iFixture = 1, size(suite%fixtures)
          associate (fixture => suite%fixtures(iFixture))
            do iTest = 1, size(fixture%tests)
              if (size(fixture%paramBounds) > 0) then
                nParamTests = product(fixture%paramBounds(2, :) - fixture%paramBounds(1, :) + 1)
              else
                nParamTests = 1
              end if
              nAllTests = nAllTests + nParamTests
            end do
          end associate
        end do
      end associate
    end do

    allocate(testContainers(nAllTests))

    ind = 1
    do iSuite = 1, size(suites)
      associate (suite => suites(iSuite))
        do iFixture = 1, size(suite%fixtures)
          associate (fixture => suite%fixtures(iFixture))
            do iTest = 1, size(fixture%tests)
              if (size(fixture%paramBounds) > 0) then
                nParamTests = product(fixture%paramBounds(2, :) - fixture%paramBounds(1, :) + 1)
                do iParam = 1, nParamTests
                  call fytest_TestContainer_initialize(testContainers(ind), suite, fixture,&
                      & iTest, iParam)
                  ind = ind + 1
                end do
              else
                call fytest_TestContainer_initialize(testContainers(ind), suite, fixture, iTest, 0)
                ind = ind + 1
              end if
            end do
          end associate
        end do
      end associate
    end do

  end subroutine fytest_getFlatTestArray


  subroutine fytest_runTests(env, testContainers, logger)
    type(fytest_GlobalEnv), intent(inout) :: env
    type(fytest_TestContainer), intent(inout) :: testContainers(:)
    type(fytest_TestLogger), optional, intent(inout) :: logger

    integer :: iTest

    do iTest = 1, size(testContainers)
      if (present(logger)) then
        call logger%logTestRun(testContainers(iTest))
      end if
      testContainers(iTest)%testContext%env = env
      call testContainers(iTest)%runTest(&
          & testContainers(iTest)%testName, testContainers(iTest)%testParams,&
          & testContainers(iTest)%testContext)
      if (present(logger)) then
        call logger%logTestResult(testContainers(iTest))
      end if
    end do

  end subroutine fytest_runTests


  subroutine fytest_generateFinalReport(suiteContainers, testContainers, timer, totals, hasErrors,&
      & logger)
    type(fytest_SuiteContainer), intent(in) :: suiteContainers(:)
    type(fytest_TestContainer), intent(in) :: testContainers(:)
    type(fytest_Timer), intent(in) :: timer
    integer, intent(out) :: totals(fytest_nTestStatus)
    logical, intent(out) :: hasErrors
    type(fytest_TestLogger), optional, intent(inout) :: logger

    integer :: iSuite, iTest

    hasErrors = .false.

    do iSuite = 1, size(suiteContainers)
      associate (suite => suiteContainers(iSuite))
        if (suite%suiteContext%status == fytest_TestStatus%testFailed) then
          if (present(logger)) then
            call logger%logFailedSuiteInit(suite)
          end if
          hasErrors = .true.
        end if
      end associate
    end do

    totals(:) = 0
    do iTest = 1, size(testContainers)
      associate (test => testContainers(iTest))
        totals(test%testContext%statusGlobal) = totals(test%testContext%statusGlobal) + 1
        if (test%testContext%statusGlobal == fytest_TestStatus%testFailed) then
          if (present(logger)) then
            call logger%logFailedTest(test)
          end if
          hasErrors = .true.
        end if
      end associate
    end do
    if (present(logger)) then
      call logger%logTestStatistics(totals, timer)
    end if

  end subroutine fytest_generateFinalReport


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  TestLogger
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  subroutine fytest_TestLogger_init(this)
    type(fytest_TestLogger), intent(out) :: this

  end subroutine fytest_TestLogger_init


  subroutine fytest_TestLogger_writeError(this, msg)
    class(fytest_TestLogger), intent(inout) :: this
    character(*), intent(in) :: msg

    write(this%fdOut, "(A,A)") "ERROR: ", msg

  end subroutine fytest_TestLogger_writeError


  subroutine fytest_TestLogger_logStatus(this, status, msg)
    class(fytest_TestLogger), intent(in) :: this
    character(*), intent(in) :: status
    character(*), intent(in) :: msg

    write(this%fdOut, "('[ ', A, T11, ' ] ', A)") trim(status), trim(msg)

  end subroutine fytest_TestLogger_logStatus


  subroutine fytest_TestLogger_logSuiteInit(this, container)
    class(fytest_TestLogger), intent(in) :: this
    class(fytest_SuiteContainer), intent(in) :: container

    call this%logStatus('Init', container%name)

  end subroutine fytest_TestLogger_logSuiteInit


  subroutine fytest_TestLogger_logSuiteInitResult(this, container)
    class(fytest_TestLogger), intent(in) :: this
    class(fytest_SuiteContainer), intent(in) :: container

    call this%logStatus(trim(statusChars(container%suiteContext%status)), container%name)

  end subroutine fytest_TestLogger_logSuiteInitResult


  subroutine fytest_TestLogger_logFailedSuiteInit(this, container)
    class(fytest_TestLogger), intent(in) :: this
    type(fytest_SuiteContainer), intent(in) :: container

    type(fytest_TestException) :: exc

    write(this%fdOut, "(/, A, 1X, T20, A)") 'FAILING SUITE:', container%name
    exc = container%suiteContext%exceptions(1)
    write(this%fdOut, "(A, T20, 2A, I0)") 'Failed at: ', exc%file, ':', exc%line
    write(this%fdOut, "(A)") exc%message

  end subroutine fytest_TestLogger_logFailedSuiteInit


  subroutine fytest_TestLogger_logTestRun(this, container)
    class(fytest_TestLogger), intent(in) :: this
    type(fytest_TestContainer), intent(in) :: container

    call this%logStatus('Running',&
        & trim(getFullTestName(container%suiteName, container%fixtureName, container%testName,&
        & container%testParams)))

  end subroutine fytest_TestLogger_logTestRun


  subroutine fytest_TestLogger_logTestResult(this, container)
    class(fytest_TestLogger), intent(in) :: this
    type(fytest_TestContainer), intent(in) :: container

    call this%logStatus(trim(statusChars(container%testContext%statusGlobal)),&
        & trim(getFullTestName(container%suiteName, container%fixtureName, container%testName,&
        & container%testParams, container%testContext%textRepr)))

  end subroutine fytest_TestLogger_logTestResult


  subroutine fytest_TestLogger_logFailedTest(this, container)
    class(fytest_TestLogger), intent(in) :: this
    type(fytest_TestContainer), intent(in) :: container

    type(fytest_TestException) :: exc
    integer :: iExc

    write(this%fdOut, "(/, A, 1X, T20, A)") 'FAILING TEST:',&
        & trim(getFullTestName(container%suiteName, container%fixtureName, container%testName,&
        & container%testParams, container%testContext%textRepr))
    do iExc = 1, size(container%testContext%exceptions)
      exc = container%testContext%exceptions(iExc)
      #:if fytest_with_mpi
        write(this%fdOut, "(/, A, T20, 2A, I0, 1X, A, I0, A)") 'Failed at: ', exc%file, ':',&
            & exc%line, "[process ", exc%node, "]"
      #:else
        write(this%fdOut, "(/, A, T20, 2A, I0)") 'Failed at: ', exc%file, ':', exc%line
      #:endif
      write(this%fdOut, "(A)") exc%message
    end do

  end subroutine fytest_TestLogger_logFailedTest


  subroutine fytest_TestLogger_logTestStatistics(this, totals, timer)
    class(fytest_TestLogger), intent(in) :: this
    integer, intent(in) :: totals(:)
    class(fytest_Timer), intent(in) :: timer

    real :: wallClockTime, cpuTime

    wallClockTime = timer%getWallClockTime()
    cpuTime = timer%getCpuTime()

    if (wallClockTime < 1.0) then
      write(this%fdOut, "(/, A, I0, A, I0, A)") 'Run ', sum(totals), ' tests in ',&
          & int(wallClockTime * 1000.0), ' ms.'
    else
      write(this%fdOut, "(/, A, I0, A, F0.3, A)") 'Run ', sum(totals), ' tests in ',&
          & wallClockTime, ' s'
    end if
    write(this%fdOut, "(2(A, I0), A)") 'Passed: ', totals(fytest_TestStatus%testSucceded),&
        & ', Failed: ', totals(fytest_TestStatus%testFailed), '.'

  end subroutine fytest_TestLogger_logTestStatistics


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Timer
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


  !> Starts the timer.
  subroutine fytest_Timer_start(this)
    class(fytest_Timer), intent(inout) :: this

    call cpu_time(this%startTime)
    call system_clock(count=this%startCount, count_rate=this%countRate)

  end subroutine fytest_Timer_start


  !> Stops the timer.
  subroutine fytest_Timer_stop(this)
    class(fytest_Timer), intent(inout) :: this

    call cpu_time(this%endTime)
    call system_clock(count=this%endCount)

  end subroutine fytest_Timer_stop


  !> Returns the measured CPU time
  function fytest_Timer_getCpuTime(this) result(cpuTime)
    class(fytest_Timer), intent(in) :: this
    real :: cpuTime

    cpuTime = this%endTime - this%startTime

  end function fytest_Timer_getCpuTime


  !> Returns the measured wall clock time.
  function fytest_Timer_getWallClockTime(this) result(wallClockTime)
    class(fytest_Timer), intent(in) :: this
    real :: wallClockTime

    if (this%countRate == 0) then
      wallClockTime = 0.0
    else
      wallClockTime = real(this%endCount - this%startCount) / real(this%countRate)
    end if

  end function fytest_Timer_getWallClockTime


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!  Helper functions
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

  function getParams(ind, paramBounds) result(params)
    integer, intent(in) :: ind
    integer, intent(in) :: paramBounds(:,:)
    integer :: params(size(paramBounds, dim=2))

    integer :: nParams(size(paramBounds, dim=2))
    integer :: ind0, divisor, iParam

    nParams(:) = paramBounds(2, :) - paramBounds(1, :) + 1
    ind0 = ind - 1
    divisor = product(nParams)
    do iParam = size(paramBounds, dim=2), 2, -1
      divisor = divisor / nParams(iParam)
      params(iParam) = ind0 / divisor
      ind0 = ind0 - divisor * params(iParam)
    end do
    params(1) = ind0
    params(:) = params + 1

  end function getParams


  function getTestParamStr(testParams) result(paramStr)
    integer, intent(in) :: testParams(:)
    character(100) :: paramStr

    character(100) :: formatStr

    if (size(testParams) == 1) then
      write(paramStr, "(I0)") testParams(1)
    else
      write(formatStr, "(A,I0,A)") "(", size(testParams) - 1, "(I0,','),I0)"
      write(paramStr, formatStr) testParams
    end if

  end function getTestParamStr


  function getFullTestName(suiteName, fixtureName, testName, testParams, textRepr)&
      & result(fullTestName)
    character(*), intent(in) :: suiteName
    character(*), intent(in) :: fixtureName
    character(*), intent(in) :: testName
    integer, intent(in) :: testParams(:)
    character(:), allocatable, intent(in), optional :: textRepr
    character(1024) :: fullTestName

    character(:), allocatable :: textReprSuffix
    character(1024) :: prefix

    textReprSuffix = ''
    if (present(textRepr)) then
      if (len(textRepr) > 0) then
        textReprSuffix = ' {' // textRepr // '}'
      end if
    end if
    if (len_trim(fixtureName) > 0) then
      write(prefix, "(A, '/', A)") trim(suiteName), trim(fixtureName)
    else
      write(prefix, "(A)") trim(suiteName)
    end if
    if (size(testParams) > 0) then
      write(fullTestName, "(A, '(', A, ')/', A, A)")  trim(prefix),&
          & trim(getTestParamStr(testParams)), trim(testName), textReprSuffix
    else
      write(fullTestName, "(A, '/', A, A)") trim(prefix), trim(testName), textReprSuffix
    end if

  end function getFullTestName


  #:if fytest_with_mpi

    subroutine fytest_TestException_mpiSend(this, env, dest)
      class(fytest_TestException), intent(in) :: this
      class(fytest_GlobalEnv), intent(inout) :: env
      integer, intent(in) :: dest

      integer :: bufferLen
      integer :: error

      bufferLen = len(this%file)
      call mpi_send(bufferLen, 1, MPI_INTEGER, dest, 0, env%mpi%comm, error)
      call mpi_send(this%file, bufferLen, MPI_CHARACTER, dest, 0, env%mpi%comm, error)
      call mpi_send(this%line, 1, MPI_INTEGER, dest, 0, env%mpi%comm, error)
      bufferLen = len(this%message)
      call mpi_send(bufferLen, 1, MPI_INTEGER, dest, 0, env%mpi%comm, error)
      call mpi_send(this%message, bufferLen, MPI_CHARACTER, dest, 0, env%mpi%comm, error)
      call mpi_send(this%node, 1, MPI_INTEGER, dest, 0, env%mpi%comm, error)

    end subroutine fytest_TestException_mpiSend



    subroutine fytest_TestException_mpiRecv(this, env, source)
      class(fytest_TestException), intent(out) :: this
      class(fytest_GlobalEnv), intent(inout) :: env
      integer, intent(in) :: source

      integer :: status(MPI_STATUS_SIZE)
      integer :: bufferLen, error

      call mpi_recv(bufferLen, 1, MPI_INTEGER, source, 0, env%mpi%comm, status, error)
      allocate(character(bufferLen) :: this%file)
      call mpi_recv(this%file, bufferLen, MPI_CHARACTER, source, 0, env%mpi%comm, status, error)
      call mpi_recv(this%line, 1, MPI_INTEGER, source, 0, env%mpi%comm, status, error)
      call mpi_recv(bufferLen, 1, MPI_INTEGER, source, 0, env%mpi%comm, status, error)
      allocate(character(bufferLen) :: this%message)
      call mpi_recv(this%message, bufferLen, MPI_CHARACTER, source, 0, env%mpi%comm, status, error)
      call mpi_recv(this%node, 1, MPI_INTEGER, source, 0, env%mpi%comm, status, error)

    end subroutine fytest_TestException_mpiRecv

  #:endif

end module fytest
