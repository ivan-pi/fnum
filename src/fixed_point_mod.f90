module fixed_point_mod
    
    ! use precision, only: wp

    ! use user_function_mod, only: user_function

    implicit none
    private

    public :: fixed_point, wp, user_function

    integer, parameter :: wp = kind(2.0d0) 

    interface 
        real(wp) function user_function(x)
            import :: wp
            real(wp), intent(in) :: x
        end function
    end interface

contains

    pure real(wp) function relerr_(actual,desired)
        real(wp), intent(in) :: actual, desired

        relerr_ = (actual - desired)/desired
    end function


    pure real(wp) function del2_(p0,p1,d)
        real(wp), intent(in) :: p0, p1, d

        del2_ = p0 - (p1 - p0)**2/d
    end function


    pure real(wp) function lazywhere(cond,p,p0)
        logical, intent(in) :: cond
        real(wp), intent(in) :: p, p0

        if (cond) then
            lazywhere = p
        else
            lazywhere = p0
        end if
    end function


    real(wp) function fixed_point(func,x0,xtol,maxiter,accel) result(p)

        interface
            real(wp) function func(x)
                import :: wp
                real(wp), intent(in) :: x
            end function
        end interface
        real(wp), intent(in) :: x0
        real(wp), intent(in), optional :: xtol
        integer, intent(in), optional :: maxiter
        logical, intent(in), optional :: accel

        real(wp) :: xtol_, p0, p1, p2, relerr, d
        integer :: maxiter_, i
        logical :: accel_

        xtol_ = 1.0e-8_wp
        if (present(xtol)) xtol_ = xtol

        maxiter_ = 400
        if (present(maxiter)) maxiter_ = maxiter

        accel_ = .true.
        if (present(accel)) accel_ = accel


        p0 = x0


        do i = 1, maxiter_

            ! evaluate function
            p1 = func(p0)

            if (accel_) then
                p2 = func(p1)
                d = p2 - 2.0_wp*p1 + p0 ! calculate denominator
                
                p = lazywhere(d /= 0,del2_(p0,p1,d),p2)
            else
                p = p1
            end if


            ! calculate relative error
            relerr = lazywhere(p0 /= 0,relerr_(p,p0),p-p0)

            ! check tolerance
            if (abs(relerr) < xtol_) then
                return
            end if

            p0 = p

        end do
    end function


end module


module myfunc_mod

    use fixed_point_mod, only: wp

    implicit none
    private

    public :: myfunc

contains

    real(wp) function myfunc(x)
        real(wp), intent(in) :: x
        myfunc = sqrt(10._wp/(x+4._wp))
    end function

end module

program test_fixed_point

    use fixed_point_mod
    use myfunc_mod
    implicit none

    real(wp) :: x0

    x0 = 1.5_wp
    print *, fixed_point(myfunc,x0)

! contains

!     real(wp) function myfunc(x)
!         real(wp), intent(in) :: x
!         myfunc = sqrt(10._wp/(x+4._wp))
!     end function
    
end program