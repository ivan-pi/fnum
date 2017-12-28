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


! module test_tests
    
!     use ftest, only: test, assert_equal, assert_not_equal, assert_true, assert_false

!     implicit none
!     private

!     public :: all_tests

! contains

!     subroutine all_tests
!         call test(bogus_integer,"bogus_integer")
!         call test(bogus_logical,"bogus_logical")
!         call test(bogus_condition,"bogus_condition")
!     end subroutine

!     subroutine bogus_integer
!         integer :: a = 2
!         integer :: b = 3
!         call assert_equal(a,b, "hi ivan")
!     end subroutine

!     subroutine bogus_logical
!         logical :: a = .true.
!         logical :: b = .false.
!         call assert_equal(a,b,"This should fail!")
!         ! call assert_not_equal(a,b)
!     end subroutine

!     subroutine bogus_condition
!         logical :: cond1, cond2
!         cond1 = .false.
!         cond2 = .false.
!         call assert_true(cond1)
!         call assert_false(cond2)
!     end subroutine
! end module

! program test_driver
!     use ftest
!     use test_tests, only: all_tests
    
!     implicit none

!     logical :: result
    
!     ! call ftest_init
!     call ftest_run(all_tests,result)
!     ! call ftest_finish(result)

!     if (.not. result) stop "TESTS FAILED"

! end program
