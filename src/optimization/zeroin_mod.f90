module optimize_mod

    implicit none
    private

    public :: zeroin, fmin

    interface
        real function zeroin(ax,bx,f,tol)
            real, intent(in) :: ax
            real, intent(in) :: bx
            interface
                real function f(x)
                    real, intent(in) :: x
                end function
            end interface   
            real, intent(in) :: tol
        end function
    end interface


    interface
        real function fmin(ax,bx,f,tol)
            real, intent(in) :: ax
            real, intent(in) :: bx
            interface
                real function f(x)
                    real, intent(in) :: x
                end function
            end interface   
            real, intent(in) :: tol
        end function
    end interface
contains


end module

program test
    use optimize_mod, only: zeroin, fmin

    implicit none

    real :: z
    real :: a = 2.0
    real :: b = 3.0
    real :: tol = 1.0e-10


    z = zeroin(a,b,f,tol)

    write(*,'(a,f13.10)') "root z =", z


    z = fmin(0.0,1.0,f,1.0e-5)

    write(*,'(a,f12.5)') "minimum z =", z
contains

    real function f(x)
        real, intent(in) :: x
        f = x*(x*x - 2.) - 5.
    end function

end program