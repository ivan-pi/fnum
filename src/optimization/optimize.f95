module optimize_mod

    use precision, only : wp
    use utils_mod, only: assert, stop_error

    implicit none
    private

    public :: func, bisection

    real(dp), parameter :: T_ = 1.0e-7_dp
    integer, parameter :: MAXIT_ = 150

    interface
        function user_function(x)
            import :: wp
            real(wp) :: user_function
            real(wp), intent(in) :: x
        end function
    end interface

contains

!>
!   Finds a root of a function using bisection.
!
!   
    pure function bisection(f,a,b,tol) result(c)
        ! PARAMETERS
        procedure(user_function) :: f
            !! A user function with the interface shown above.
        real(dp), intent(in) :: a
            !! The left value of the bracketed interval.
        real(dp), intent(in) :: b
            !! The right value of the bracketed interval.
        real(dp), intent(in), optional :: tol
            !! A prescribed tolerance (default value is 1.e-7).
        
        ! RETURNS
        real(dp) :: c
            !! A root of the function within the bracketed interval.

        real(dp) :: tol_
        real(dp) :: a_, b_, fa, fb, fc

        ! Set tolerabce
        tol_ = 1._wp-7 
        if (present(tol)) tol_ = tol

        ! Initialize bracket and function values
        a_ = a
        b_ = b
        fa = f(a_)
        fb = f(b_)

        call assert(fa*fb < 0, "[bisection] f(a) and f(b) must have opposite signs!")

        do while (abs(b_ - a_) > tol_)
            c = 0.5_dp*(a_ + b_)
            fc = f(c)
            if (abs(fc) < tiny(1.0_dp)) return ! zero found
            if (fa*fc < 0) then
                b_ = c
                fb = fc
            else
                a_ = c
                fa = fc
            end if
        end do

        c = 0.5_dp*(a_ + b_)
    end function


    ! function newton(f,df,x0,tol,maxiter)

    ! end function

    ! function secant(f,x0,tol,maxiter)
    ! end function


end module