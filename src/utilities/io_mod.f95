module io_module

    use precision, only: wp

    implicit none
    private

contains

!===============================================================================
!>  Returns the lowest available i/o unit.
!
!   Searches through the range of possible units defined by `umin` and `umax`,
!   and checks if they are already open. The lowest available avaiable unit is
!   returned by the function and also through the optional `unit` parameter. This
!   allows the function to be used directly in an `open(...)` statement, and
!   optionally save the result in a local variable. If no units are available 
!   the function returns `-1`.
!
!### Example
!```
! integer :: u
! open(unit=newunit(u), file="log.txt", status="old")
! read(u, *) a, b
! close(u)
!```
!   @note This function is meant to be used older compilers that do not
!   support the `newunit` specifier available in Fortran 2008!
!

    function newunit(unit) result(new)
 
        ! PARAMETERS
        integer, intent(out), optional :: unit
            !! If present, contains the `newunit` value.

        ! RETURNS
        integer :: new
            !! An available unit. Returns -1 in case no units are available.

        logical :: open
        integer, parameter :: umin = 10 ! Avoid lower numbers.
        integer, parameter :: umax = 999 ! May be system-dependent.
        integer :: u
    !---------------------------------------------------------------------------
        new = -1    ! No available units.

        ! Loop through range of units.
        do u = umin, umax
            ! Check if available.
            inquire(unit=u,opened=open)
            if (.not. open) then
                unit = u
                exit
            end if
        end do
        if (present(unit)) unit = new
    end function
!===============================================================================


!===============================================================================
end module