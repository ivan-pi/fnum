module interpolation_mod

    use precision, only: wp

    implicit none
    private

    public :: wp, polyval, poly_stupid
    public :: neville, polint

    interface polyval
        module procedure polyval
        module procedure polyval_array
    end interface

    interface neville
        module procedure neville
        module procedure neville_err
    end interface
contains

    pure function polyval(a,x) result(p)
        ! integer, intent(in) :: n
        real(wp), intent(in) :: a(:)
        real(wp), intent(in) :: x

        real(wp) :: p

        integer :: i

        p = a(size(a))
        do i = size(a)-1, 1, -1
            p = a(i) + p*x  
        end do
    end function

    pure function polyval_array(a,x) result(p)
        real(wp), intent(in) :: a(:)
        real(wp), intent(in) :: x(:)

        real(wp) :: p(size(x))
        integer :: i
        do i = 1, size(x)
            p(i) = polyval(a,x(i))
        end do
    end function


    function poly_stupid(a,x) result(p)
        real(wp), intent(in) :: a(:)
        real(wp), intent(in) :: x

        real(wp) :: p

        integer :: i 

        p = 0._wp
        do i = 1, size(a)
            p = p + a(i)*x**(i-1)
        end do
    end function

!>  Evaluate a polynomial from a discrete set of points.
!
!### References
! https://s3.amazonaws.com/torkian/torkian/Site/Research/Entries/2008/2/29_Nevilles_algorithm_Java_Code.html
    pure function neville(n,xi,yi,x) result(px)

        ! PARAMETERS
        integer, intent(in) :: n        !! Number of interpolating points.
        real(wp), intent(in) :: xi(n)   !! Array containing the abscissas.
        real(wp), intent(in) :: yi(n)   !! Array containing the ordinates.
        real(wp), intent(in) :: x       !! Abscissa at which to evaluate the interpolating polynomial.

        ! RETURNS
        real(wp) :: px      !! The value of the polynomial at `x`.

        ! Local variables
        real(wp) :: P(n), denom, a, b
        integer :: m, i

        ! copy values
        P = yi

        do m = 1, n-1
            do i = 1, n - m
                a = xi(i) - x
                b = xi(i+m) - x
                denom = a - b
                P(i) = (a*P(i+1) - b*P(i))/denom
            end do
        end do

        px = P(1)
    end function


    function neville_err(n,xi,yi,x,dy) result(y)
        integer, intent(in) :: n
        real(wp), intent(in) :: xi(n)
        real(wp), intent(in) :: yi(n)
        real(wp), intent(in) :: x
        real(wp), intent(out) :: dy

        real(wp) :: y
        real(wp) :: C(n), D(n), a, b, denom, w, dy_
        integer :: ns, m, i

        ns = minloc(abs(x-xi),dim=1)
        C = yi
        D = yi

        dy_ = 0.0_wp
        y = yi(ns) ! The initial approximation

        ns = ns - 1! ????

        do m = 1, n - 1
            do i = 1, n - m
                a = xi(i) - x
                b = xi(i+m) - x
                denom = a - b
                 ! if (denom == 0) ... two xi points are equal
                w = C(i+1) - D(i)
                C(i) = a*w/denom
                D(i) = b*w/denom
            end do

            if (2*ns < n-m) then ! ???
                dy_ = C(ns+1)
            else
                dy_ = D(ns)
                ns = ns - 1 
            end if

            y = y + dy_
        end do
        dy = dy_
    end function

    pure subroutine polint(n,xi,yi,x,y,dy)
        integer, intent(in) :: n
        real(wp), intent(in) :: xi(n)
        real(wp), intent(in) :: yi(n)
        real(wp), intent(in) :: x
        real(wp), intent(out) :: y
        real(wp), intent(out), optional :: dy

        real(wp) :: C(n), D(n), a, b, denom, w, dy_
        integer :: ns, m, i, ierr_

        ns = minloc(abs(x-xi),dim=1)
        C = yi
        D = yi

        dy_ = 0.0_wp
        y = yi(ns) ! The initial approximation

        ns = ns - 1! ????

        do m = 1, n - 1
            do i = 1, n - m
                a = xi(i) - x
                b = xi(i+m) - x
                denom = a - b
                 ! if (denom == 0) ... two xi points are equal
                w = C(i+1) - D(i)
                C(i) = a*w/denom
                D(i) = b*w/denom
            end do

            if (2*ns < n-m) then ! ???
                dy_ = C(ns+1)
            else
                dy_ = D(ns)
                ns = ns - 1 
            end if

            y = y + dy_
        end do
        if (present(dy)) dy = dy_
    end subroutine
end module

program test_poly
    use interpolation_mod

    implicit none

    real(wp), allocatable :: a(:), x(:), y(:), xx(:)
    real(wp) :: p, dy

    integer :: i
    integer, parameter :: n = 11

    a = [1._wp,2._wp,3._wp,3.25_wp]

    p = polyval(a,0.5_wp)

    x = [(real(i-1,wp)*0.1_wp,i=1,n)]

    y = polyval(a,x)

    ! print *, a
    ! print *, p, poly_stupid(a,0.5_wp), neville(size(x),x,y,0.5_wp)

    call polint(n,x,y,0.51_wp,p,dy)
    ! print *, dy
    print *, p, neville(n,x,y,0.51_wp), polyval(a,0.51_wp)
    print *, dy

    ! xx = linspace(0._wp,1._wp,100)

    ! do i = 1, 100
    !     print *, xx(i), polyval(a,xx(i)), neville(11,x,y,xx(i))
    ! end do


contains

    function linspace(a,b,n)
        real(wp), intent(in) :: a, b
        integer, intent(in) :: n

        real(wp), allocatable :: linspace(:)

        real(wp) :: step

        step = (b-a)/real(n-1,wp)
        linspace = [(a + real(i-1,wp)*step,i=1,n)]
    end function
end program


