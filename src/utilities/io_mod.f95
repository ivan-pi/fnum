module io_mod

    ! use precision, only: wp

    implicit none
    private

    public :: newunit

contains


! !>  Wrapper function for the intrinsic `open` statement. 
!     function open(iostat,file,status,access,form,recl,blank,position,action,delim,pad) result(unit)
        
!         ! PARAMETERS
!         integer, intent(out), optional :: iostat
!         character(len=*), intent(in), optional :: file !! The file to open.
!         character(len=*), intent(in), optional :: status
!         character(len=*), intent(in), optional :: access
!         character(len=*), intent(in), optional :: form
!         integer, intent(in), optional :: recl
!         character(len=*), intent(in), optional :: blank
!         character(len=*), intent(in), optional :: position
!         character(len=*), intent(in), optional :: action
!         character(len=*), intent(in), optional :: delim
!         character(len=*), intent(in), optional :: pad
        
!         ! RETURNS
!         integer :: unit

!         character(len=80) :: file_, status_, access_, form_, blank_, &
!                              position_, action_, delim_, pad_
!         integer :: iostat_, recl_

!         ! IOSTAT
!         iostat_ = 0

!         ! ACCESS
!         access_ = "sequential" ! default value
!         if (present(access)) access_ = access

!         ! FORM
!         if (.not. present(form)) then
!             form_ = "formatted" ! default value
!             if (access_ == "direct") form_ = "unformatted" ! default value
!         else
!             form_ = form ! user
!         end if


!         ! STATUS
!         if (.not. present(status)) then
!             status_ = "unknown"
!             if (.not. present(file)) status_ = "scratch"
!         else
!             status_ = status
!             if (.not. present(file) .and. status_ == "new") then
!                 print *, "[open]: missing file specifier with status =", status_
!                 stop
!             end if
!             if (.not. present(file) .and. status_ == "replace") then
!                 print *, "[open]: missing file specifier with status =", status_
!             end if
!         end if

!         ! RECL (must be present for "direct" access)
!         if (access == "direct" .and. (.not. present(recl))) then
!             print *, "recl must be present for direct access"
!             iostat = 1
!             unit = -1
!             return
!         end if

!         ! BLANK (should only appear for formatted i/o)
!         blank_ = "null"
!         if (present(blank)) blank_ = blank

!         ! POSITION (only necessary for sequential access)
!         position_ = "asis"
!         if (present(position)) position_ = position

!         ! ACTION
!         action_ = "readwrite" ! true default should be processor dependent
!         if (present(action)) action_ = action

!         ! DELIM (should only appear for formatted i/o)
!         delim_ = "none"
!         if (present(delim)) delim_ = delim

!         ! PAD
!         pad_ = "yes"
!         if (present(pad)) pad_ = pad

!         ! FILE
!         if (present(file)) then
!             file_ = file
!             ! open(newunit=unit,iostat=iostat_,file=file_,status=status_,access=access_,form=form_,&
!                 ! blank=blank_)
!         else
!             status_ = "scratch"
!             ! omit the file specifier
!             ! open(newunit=unit,iostat=iostat_,status=status_,access=access_,form=form_)
!         end if
        
!         ! IOSTAT
!         if (present(iostat)) iostat = iostat_
!     end function

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
!   support the `newunit` specifier available in Fortran 2008! On new compilers
!   you can use the ```fortran open(newunit=u,file=...)``` statement.
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

! program test_open

!     use io_mod


!     integer :: unit

!     ! unit = open(file="myfile.txt",status="new")
!     print *, unit

!     write(unit,*) "hello IVan"

!     close(unit)
!     print *, "  blue" == "blue     "

!     open(30,status="scratch",access="direct")
! end program