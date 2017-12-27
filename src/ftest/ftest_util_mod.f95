module ftest_util_mod

    use iso_fortran_env, only: i1 => int8, &
                               i2 => int16, &
                               i4 => int32, &
                               i8 => int64, &
                               r4 => real32, &
                               r8 => real64, &
                               r16 => real128

    implicit none
    private

    public :: equal

    interface equal
        module procedure i1_equal
        module procedure i2_equal
        module procedure i4_equal
        module procedure i8_equal
        module procedure r4_equal
        module procedure r8_equal
        module procedure r16_equal
        module procedure logical_equal
        module procedure string_equal
    end interface

contains

    function r4_equal(a,b) result(equal)
        real(r4), intent(in) :: a
        real(r4), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
            return
        else if (abs(a-b) < epsilon(a)) then
            equal = .true.
        end if
    end function

    function r8_equal(a,b) result(equal)
        real(r8), intent(in) :: a
        real(r8), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
            return
        else if (abs(a-b) < epsilon(a)) then
            equal = .true.
        end if
    end function

    function r16_equal(a,b) result(equal)
        real(r16), intent(in) :: a
        real(r16), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
            return
        else if (abs(a-b) < epsilon(a)) then
            equal = .true.
            return
        end if
    end function

    function i1_equal(a,b) result(equal)
        integer(i1), intent(in) :: a
        integer(i1), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
        end if
    end function

    function i2_equal(a,b) result(equal)
        integer(i2), intent(in) :: a
        integer(i2), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
        end if
    end function

    function i4_equal(a,b) result(equal)
        integer(i4), intent(in) :: a
        integer(i4), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
        end if
    end function

    function i8_equal(a,b) result(equal)
        integer(i8), intent(in) :: a
        integer(i8), intent(in) :: b
        logical :: equal

        equal = .false.
        if (a == b) then
            equal = .true.
        end if
    end function

    function logical_equal(l1,l2) result(equal)
        logical, intent(in) :: l1
        logical, intent(in) :: l2
        logical :: equal

        equal = .false.
        if (l1 .eqv. l2) then
            equal = .true.
        end if
    end function

    function string_equal(str1,str2) result(equal)
        character(len=*), intent(in) :: str1
        character(len=*), intent(in) :: str2
        logical :: equal

        equal = .false.
        if (str1 .eq. str2) then
            equal = .true.
        end if
    end function

end module


module ftest

    use ftest_util_mod, only: equal
    implicit none
    private

    ! public :: assert, initialize_tests, report_tests

    
    ! ftest routines
    public :: ftest_init, ftest_run, ftest_finish

    ! Assertions
    public :: assert_equal, &
              assert_not_equal, &
              assert_true, &
              assert_false

    ! Test 
    public :: test 

    integer, parameter :: STDOUT = 6
    integer, parameter :: STDERR = 0

    integer, private, save :: nsuccess
    integer, private, save :: nfailure
    integer, private, save :: ntest
    integer, private, save :: last_test
    integer, private, save :: nruns

    integer, private, save :: successful_tests
    integer, private, save :: failed_tests

    logical, private, save :: call_final = .true.
    character(len=*), parameter :: TEST_FILE = "ftest.lst"

    interface test
        module procedure test_named
        module procedure test_unnamed
    end interface

    interface assert_equal
        module procedure assert_equal_integer
        module procedure assert_equal_logical
    end interface

    interface assert_not_equal
        module procedure assert_not_equal_integer
        module procedure assert_not_equal_logical
    end interface

contains

    subroutine ftest_init
        call_final = .false.
    end subroutine

    subroutine ftest_run(tests,tests_failed)
        interface
            subroutine tests
            end subroutine tests
        end interface
        logical, intent(out), optional :: tests_failed

        integer :: unit
        integer :: ierr
        logical :: tests_failed_

        last_test = 0
        nfailure  = 0
        nsuccess  = 0
        nruns = 0
        ntest = 0

        successful_tests = 0
        failed_tests = 0

        if (file_exists_(TEST_FILE)) then
            open(newunit=unit,file=TEST_FILE,iostat=ierr)
            if (ierr == 0) then
                read(unit, *, iostat=ierr) last_test, nfailure, nsuccess, nruns
                if ( ierr /= 0 ) then
                    last_test = 0
                    nfailure = 0
                    nsuccess = 0
                    nruns = 0
                endif
                close(unit)
            endif
        endif

        nruns = nruns + 1

        call tests

        if (call_final) then
            call ftest_finish(tests_failed_)
            if (present(tests_failed)) tests_failed = tests_failed_
        endif
    end subroutine

!>  Check if a file exists.
    function file_exists_(file) result(exists)
        character(len=*), intent(in) :: file
        logical :: exists
        inquire(file=file,exist=exists)
    end function

!>  Routine to run a named test.
    subroutine test_named(tcase,tname)
        interface
            subroutine tcase() 
                !! A subroutine implementing a test.
            end subroutine
        end interface
        character(len=*), intent(in) :: tname !! The name of the test.

        integer :: init_nfailure
        integer :: init_nsuccess
        integer :: unit

        init_nfailure = nfailure
        init_nsuccess = nsuccess

        ! Check if we should run the test
        ntest = ntest + 1
        if ( ntest <= last_test ) then
            return
        endif

        ! Record the fact that we started the test
        open(newunit=unit,file='ftest.lst' )
        write(unit,*) ntest, nfailure, nsuccess, nruns
        close(unit)

        ! Run the test
        write( *,fmt='(2a)',advance="no") 'Running test: ', trim(tname)
        
        call tcase ! Hopefully the program doesn't chrash here...

        if (nfailure > init_nfailure) then
            failed_tests = failed_tests + 1
            write(*,*) 'test failed'
        else
            successful_tests = successful_tests + 1
            write(*,*) 'test passed'
        end if

        ! Record results
        open(newunit=unit,file='ftest.lst' )
        write(unit,*) ntest, nfailure, nsuccess, nruns
        close(unit)
    end subroutine

!>  Routine to run an unnamed test.
    subroutine test_unnamed(tcase)
        interface
            subroutine tcase() 
                !! A subroutine implementing a test.
            end subroutine
        end interface
        call test(tcase,"_unnamed_")
    end subroutine


!== ASSERTIONS =================================================================

!>  Asserts if two integers ARE equal.
    subroutine assert_equal_integer(ia,ib,text)
        integer, intent(in) :: ia
        integer, intent(in) :: ib
        character(len=*), intent(in), optional :: text

        if (.not. equal(ia,ib)) then
            call fail_
        else
            call success_
        end if
    end subroutine


!>  Asserts if two integers are NOT equal.
    subroutine assert_not_equal_integer(ia,ib,text)
        integer, intent(in) :: ia
        integer, intent(in) :: ib
        character(len=*), intent(in), optional :: text

        if (equal(ia,ib)) then
            call fail_
        else
            call success_
        end if
    end subroutine


!>  Asserts if two booleans ARE equal.
    subroutine assert_equal_logical(la,lb,text)
        logical, intent(in) :: la
        logical, intent(in) :: lb
        character(len=*), intent(in), optional :: text
 
        if (.not.equal(la,lb)) then
            call fail_
        else
            call success_
        end if
    end subroutine


!>  Asserts if two booleans are NOT equal.
    subroutine assert_not_equal_logical(la,lb,text)
        logical, intent(in) :: la
        logical, intent(in) :: lb
        character(len=*), intent(in), optional :: text

        if (equal(la,lb)) then
            call fail_
        else
            call success_
        end if
    end subroutine


!>  Assert a condition is `.true.`.
    subroutine assert_true(condition,text)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: text
    
        if (.not. condition) then
            call fail_            
        else
            call success_
        end if
    end subroutine


!>  Assert a condition is `.false.`.
    subroutine assert_false(condition,text)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: text

        if (condition) then
            call fail_
            write(*,*) "[assert_false] Expected ",.false.," got ", .true.          
        else
            call success_
        end if
    end subroutine

!== END ASSERTIONS =============================================================


!>  Increase the number of failed assertions.
    subroutine fail_
        nfailure = nfailure + 1
    end subroutine

!>  Increase the number of successful assertions.
    subroutine success_
        nsuccess = nsuccess + 1
    end subroutine




!>  Asserts a given condition and prints the result.
!   
!   TODO: use colorize function
    logical function assert(condition,test_name)
        ! PARAMETERS
        logical, intent(in) :: condition !! The condition to be asserted.
        character(len=*), intent(in) :: test_name !! The name of the test.
        
        character(len=69) :: output_test_name
        assert = condition
        output_test_name = test_name
        if (assert) then
            write(unit=STDOUT,fmt='(a)')'test '//output_test_name//': '//&
                    char(27)//'[32mPASS'//char(27)//'[0m'
        else
            write(unit=STDOUT,fmt='(a)')'test '//output_test_name//': '//&
                    char(27)//'[31mFAIL'//char(27)//'[0m'
      end if
    end function

!>  Initialize a set of tests.
    subroutine initialize_tests(tests,ntests)
        ! PARAMETERS
        logical, allocatable, intent(inout) :: tests(:)
            !! An array of tests.
        integer, intent(in) :: ntests
            !! The number of tests we want.
      
        if (allocated(tests)) then
            deallocate(tests)
        end if
        allocate(tests(ntests))
    end subroutine

!>  Prints a report of test results.
    subroutine report_tests(tests,test_failed)
        ! PARAMETERS
        logical, intent(in) :: tests(:) 
            !! An array of tests.
        logical, optional, intent(out) :: test_failed 
            !! Wheather the tests failed.
        
        integer :: n,ntests,nsuccess,nfailure
        
        ntests = size(tests)
        nsuccess = 0
        nfailure = 0
        
        do n = 1, ntests
            if (tests(n)) then
                nsuccess = nsuccess + 1
            else
                nfailure = nfailure + 1
            endif
        end do
        
        write(unit=STDOUT,fmt='(a,i3,a)')'Ran a total of ',ntests,' tests.'
        write(unit=STDOUT,fmt='(i3,a,i3,a)') nsuccess,' tests PASSED, ',&
                                             nfailure,' tests FAILED.'
        if (present(test_failed)) then
            test_failed = .false.
            if (.not. (nfailure == 0) ) test_failed = .true.
        endif
    end subroutine

    subroutine ftest_finish(tests_failed)
        logical, intent(out), optional :: tests_failed
        write(*,*)
        write(*,*)
        write(*,*) '-- Start of ftest summary --'
        write(*,*)
        

        if (failed_tests > 0) then
           write (stdout,*) 'Some tests failed!'
           if (present(tests_failed)) tests_failed = .true.
        else
           write (stdout,*) 'ALL TESTS WERE SUCCESSFUL!'
           if (present(tests_failed)) tests_failed = .false.
        end if

        if (nfailure + nsuccess /= 0) then
            call ftest_summary_(nsuccess,nfailure,successful_tests,failed_tests)
        end if
        write(*,*) '-- End of ftest summary --'
    end subroutine
    
    subroutine ftest_summary_(succ_assert,fail_assert,succ_case,fail_case)
        integer, intent(in) :: succ_assert
        integer, intent(in) :: fail_assert
        integer, intent(in) :: succ_case
        integer, intent(in) :: fail_case

        write(*,*) 'Total number of asserts :   ', succ_assert + fail_assert
        write(*,*) 'Successful asserts      :   ', succ_assert
        write(*,*) 'Failed asserts          :   ', fail_assert
        write(*,*) 'Successful asserts / total asserts : ', succ_assert, '/', succ_assert + fail_assert

        write(*,*)
        write(*,*) 'Total number of cases   :   ', succ_case + fail_case
        write(*,*) 'Successful test cases   :   ', succ_case
        write(*,*) 'Failed test cases       :   ', fail_case
        write(*,*) 'Successful cases   / total cases   : ', succ_case, '/', succ_case + fail_case
    end subroutine

end module

module test_tests
    
    use ftest, only: test, assert_equal, assert_not_equal, assert_true, assert_false

    implicit none
    private

    public :: all_tests

contains

    subroutine all_tests
        call test(bogus_integer,"bogus_integer")
        call test(bogus_logical,"bogus_logical")
        call test(bogus_condition,"bogus_condition")
    end subroutine

    subroutine bogus_integer
        integer :: a = 2
        integer :: b = 2
        call assert_equal(a,b)
    end subroutine

    subroutine bogus_logical
        logical :: a = .true.
        logical :: b = .false.
        call assert_equal(a,b,"This should fail!")
        call assert_not_equal(a,b)
    end subroutine

    subroutine bogus_condition
        logical :: cond1, cond2
        cond1 = .true.
        cond2 = .false.
        call assert_true(cond1)
        call assert_false(.not.  cond2)
    end subroutine
end module

program test_driver
    use ftest
    use test_tests, only: all_tests
    
    implicit none

    logical :: result
    
    ! call ftest_init
    call ftest_run(all_tests,result)
    ! call ftest_finish(result)

    if (.not. result) stop "TESTS FAILED"

end program
! program test

!     use ftest_util_mod

!     implicit none

!     real(r8) :: a = 1.e-9_r8
!     real(r8) :: b = 1.e-9_r8+epsilon(a)

!     print *, equal(a,b)
!     print *, a, b


!     print *, abs(a-b)/max(abs(a),abs(b)) <= epsilon(a)
! end program