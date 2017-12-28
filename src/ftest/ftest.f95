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
              assert_false, &
              assert_equivalent, &
              assert_lt, &
              assert_gt, &
              assert_lte, &
              assert_gte

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
        module procedure assert_equal_string
    end interface

    interface assert_not_equal
        module procedure assert_not_equal_integer
        module procedure assert_not_equal_logical
        module procedure assert_not_equal_string
    end interface

    interface assert_equivalent
        module procedure assert_equal_logical
    end interface

    interface assert_lt
        module procedure assert_lt_integer
    end interface

    interface assert_gt
        module procedure assert_gt_integer
    end interface

    interface assert_lte
        module procedure assert_lte_integer
    end interface

    interface assert_gte
        module procedure assert_gte_integer
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
            call ftest_remove_file(TEST_FILE)
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
        write( *,fmt='(2a)') 'Running test: ', trim(tname)
        
        call tcase ! Hopefully the program doesn't chrash here...

        if (nfailure > init_nfailure) then
            failed_tests = failed_tests + 1
            write(*,*) red_('FAILED')
        else
            successful_tests = successful_tests + 1
            write(*,*) green_('PASSED')
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


    subroutine ftest_finish(tests_failed)
        logical, intent(out), optional :: tests_failed
        write(STDOUT,*)
        write(STDOUT,*)
        write(STDOUT,*) '-- Start of ftest summary --'
        write(STDOUT,*)
        

        if (failed_tests > 0) then
           write(STDOUT,*) red_('Some tests failed!')
           if (present(tests_failed)) tests_failed = .true.
        else
           write(STDOUT,*) green_('ALL TESTS WERE SUCCESSFUL!')
           if (present(tests_failed)) tests_failed = .false.
        end if
        write (STDOUT,*)

        if (nfailure + nsuccess /= 0) then
            call ftest_summary_(nsuccess,nfailure,successful_tests,failed_tests)
        end if
        write(STDOUT,*)
        write(STDOUT,*) '-- End of ftest summary --'
    end subroutine

    function green_(text)
        character(len=*), intent(in) :: text
        character(len=len(text)+15) :: green_ 

        green_ = char(27)//'[32m'//text//char(27)//'[0m'
    end function

    function red_(text)
        character(len=*), intent(in) :: text
        character(len=len(text)+15) :: red_ 

        red_ = char(27)//'[31m'//text//char(27)//'[0m'
    end function

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


    subroutine ftest_remove_file( filename )
        character(len=*), intent(in) :: filename

        integer                      :: unit
        integer                      :: ierr

        open(newunit=unit,file=filename,iostat=ierr)
        if (ierr/=0) then
            write(*,*) '    Could not open file for removal: ', trim(filename)
        else
            close(unit,status='delete')
            if (file_exists_(filename)) then
                write(*,*) '    Removal of file unsuccssful: ', trim(filename)
            endif
        endif
    end subroutine


!== ASSERTIONS =================================================================

!>  Asserts if two integers ARE equal.
    subroutine assert_equal_integer(a,b,text)
        integer, intent(in) :: a
        integer, intent(in) :: b
        character(len=*), intent(in), optional :: text

        if (.not. equal(a,b)) then
            call fail_
            write(STDOUT,'(a,i5,a,i5,a)') red_('[assert_equal_integer]')//' Expected ', a,', found ', b, ". "//text
        else
            call success_
        end if
    end subroutine


!>  Asserts if two integers are NOT equal.
    subroutine assert_not_equal_integer(a,b,text)
        integer, intent(in) :: a
        integer, intent(in) :: b
        character(len=*), intent(in), optional :: text

        if (equal(a,b)) then
            call fail_
            write(STDOUT,'(a,i5,a,i5,a)') red_('[assert_not_equal_integer]')//' Expected ', a,', found ', b, ". "//text
        else
            call success_
        end if
    end subroutine


!>  Asserts if two booleans ARE equal.
    subroutine assert_equal_logical(a,b,text)
        logical, intent(in) :: a
        logical, intent(in) :: b
        character(len=*), intent(in), optional :: text
 
        if (.not.equal(a,b)) then
            call fail_
            write(STDOUT,'(a,l1,a,l1,a)') red_('[assert_equal_logical]')//' Expected ', a,', found ', b, ". "//text
        else
            call success_
        end if
    end subroutine


!>  Asserts if two booleans are NOT equal.
    subroutine assert_not_equal_logical(a,b,text)
        logical, intent(in) :: a
        logical, intent(in) :: b
        character(len=*), intent(in), optional :: text

        if (equal(a,b)) then
            call fail_
            write(STDOUT,'(a,l1,a,l1,a)') red_('[assert_not_equal_logical]')//' Expected ', a,', found ', b, ". "//text
        else
            call success_
        end if
    end subroutine

!>  Asserts if two strings ARE equal.
    subroutine assert_equal_string(a,b,text)
        character(len=*), intent(in) :: a
        character(len=*), intent(in) :: b
        character(len=*), intent(in), optional :: text

        if (.not. equal(a,b)) then
            call fail_
        else
            call success_
        end if
    end subroutine

!>  Asserts if two strings are NOT equal.
    subroutine assert_not_equal_string(a,b,text)
        character(len=*), intent(in) :: a
        character(len=*), intent(in) :: b
        character(len=*), intent(in), optional :: text

        if (equal(a,b)) then
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
            write(STDOUT,'(a,l1,a)') red_('[assert_true]')//' Expected T, found ', condition, ". "//text        
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
            write(STDOUT,'(a,l1,a)') red_('[assert_true]')//' Expected F, found ', condition, ". "//text
        else
            call success_
        end if
    end subroutine

!>  Assert an integer is lower than another integer.
    subroutine assert_lt_integer(a,b,text)
        integer, intent(in) :: a
        integer, intent(in) :: b
        character(len=*), intent(in), optional :: text
        
        call assert_true(a < b,text)
    end subroutine

!>  Assert an integer is greater than another integer.
    subroutine assert_gt_integer(a,b,text)
        integer, intent(in) :: a
        integer, intent(in) :: b
        character(len=*), intent(in), optional :: text
        
        call assert_true(a > b,text)
    end subroutine

!>  Assert an integer is lower than or equal to another integer.
    subroutine assert_lte_integer(a,b,text)
        integer, intent(in) :: a
        integer, intent(in) :: b
        character(len=*), intent(in), optional :: text
        
        call assert_true(a <= b,text)
    end subroutine

!>  Assert an integer is greater than or equal to another integer.
    subroutine assert_gte_integer(a,b,text)
        integer, intent(in) :: a
        integer, intent(in) :: b
        character(len=*), intent(in), optional :: text
        
        call assert_true(a >= b,text)
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
end module