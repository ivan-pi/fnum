module utils_mod
    
    implicit none

    private
    public str

    interface str
        module procedure str_int
    end interface

contains

    pure integer function str_int_len(i) result(sz)
    ! Returns the length of the string representation of 'i'
    integer, intent(in) :: i
    integer, parameter :: MAX_STR = 100
    character(len=MAX_STR) :: s
    ! If 's' is too short (MAX_STR too small), Fortan will abort with:
    ! "Fortran runtime error: End of record"
    write(s, '(i0)') i
    sz = len_trim(s)
    end function

    pure function str_int(i) result(s)
        ! Converts integer "i" to string
        integer, intent(in) :: i
        character(len=str_int_len(i)) :: s
        write(s, '(i0)') i
    end function

end module