
! # define assert_associated(a,text) call assert_true(associated(a),__LINE__//" ."//text)

# define assert_associated(a) call assert_true(associated(a),line=__LINE__)
! # define assert_associated(a,text) call assert_true(associated(a),text, line =__LINE__)

program test_preproc

    real(8), target :: a(5)
    class(*), pointer :: b => null()

    a = [1.0,2.0,3.0,4.0,5.0]

    print *, __LINE__
    assert_associated(b)

    call assert_assoc(b,__LINE__)
contains

    subroutine assert_true(condition,text,line)
        logical, intent(in) :: condition
        character(len=*), intent(in), optional :: text
        integer, intent(in), optional :: line

        if (.not. condition) then
            print *, "[assert_true] failed at line ", line, __FILE__
        end if
    end subroutine

    subroutine assert_assoc(a,line)
        class(*), intent(in), pointer :: a
        integer, intent(in) :: line

        if (.not. associated(a)) then
            print *, "assert_assoc failed at line", line
        end if
    end subroutine
end program