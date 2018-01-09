module spline_module

    use precision, only : wp

    implicit none
    private

    public :: spline, seval, binary_search


contains


!>  Compute the coefficients of a cubic interpolating spline.
!   
!   Computes the coefficients \(b_i\), \(c_i\) and \(d_i\) for \(i=1,2,\dots,n\)
!   that define a cubic interpolating spline
!   \[s(x) = y_i + b_i(x-x_i) + c_i(x-x_i)^2+d_i(x-x_i)^3\]
!   for \(x_i \leq x \leq x_{i+1}\).
!
!   Using the subscript \(_x\) to denote differentiation with respect to \(x\)
!   the coefficients are defined as:
!   \[y_i = s(x_i)\]
!   \[b_i = s_x(x_i)\]
!   \[c_i = s_{xx}(x_i)/2\]
!   \[d_i = s_{xxx}(x_i)/6\]
!
!   @note The accompanying subroutine `seval` can be used to evaluate the spline.
    subroutine spline(n,x,y,b,c,d)

        ! PARAMETERS
        integer, intent(in) :: n
            !! The number of data points or knots (`n >= 2`).
        real(wp), intent(in) :: x(n)
            !! The abscissas of the knots in strictly increasing order.
        real(wp), intent(in) :: y(n)
            !! The ordinates of the knots.

        ! RETURNS
        real(wp), intent(out) :: b(n), c(n), d(n)
            !! Array of spline coefficients as defined in the description.

        integer :: i, j
        real(wp) :: t
        !-------------------------------------

        if (n < 2) return

        ! linear interpolation
        if (n < 3) then
            b(1) = (y(2)-y(1))/(x(2)-x(1))
            c(1) = 0._wp
            d(1) = 0._wp
            b(2) = b(1)
            c(2) = 0._wp
            d(2) = 0._wp
        else
            ! Set up tridiagonal system
            d(1) = x(2) - x(1)
            c(2) = (y(2) - y(1))/d(1)
            do i = 2, n - 1
                d(i) = x(i+1) - x(i)
                b(i) = 2._wp*(d(i-1) + d(i))
                c(i+1) = (y(i+1) - y(i))/d(i)
                c(i) = c(i+1) - c(i)
            end do        

            ! End conditions. Third derivates at x(1) and x(n) are
            ! obtained from divided differences.

            b(1) = -d(1)
            b(n) = -d(n-1)
            c(1) = 0.0_wp
            c(n) = 0.0_wp

            ! Skip this if n = 3
            if (n /= 3) then
                c(1) = c(3)/(x(4)-x(2)) - c(2)/(x(3)-x(1))
                c(n) = c(n-1)/(x(n) - x(n-2)) - c(n-2)/(x(n-1) - x(n-3))
                c(1) = c(1)*d(1)**2/(x(4)-x(1))
                c(n) = -c(n)*d(n-1)**2/(x(n)-x(n-3))
            end if

            ! Forward elimination
            do i = 2, n
                t = d(i-1)/b(i-1)
                b(i) = b(i) - t*d(i-1)
                c(i) = c(i) - t*c(i-1)
            end do

            ! Back substitution
            c(n) = c(n)/b(n)
            do j = i, n - 1
                i = n - j
                c(i) = (c(i) - d(i)*c(i+1))/b(i)
            end do
            ! c now contains the second derivative coefficients

            ! Compute polynomial coefficients
            b(n) = (y(n) - y(n-1))/d(n-1) + d(n-1)*(c(n-1) + 2._wp*c(n))
            do i = 1, n - 1
                b(i) = (y(i+1) - y(i))/d(i) - d(i)*(c(i+1) + 2._wp*c(i))
                d(i) = (c(i+1) - c(i))/d(i)
                c(i) = 3._wp*c(i)
            end do

            c(n) = 3._wp*c(n)
            d(n) = d(n-1)
        end if
    end subroutine

!>
!@note The accompanying function `spline` can be used to calculate the coefficients
!   of the cubic spline.
    function seval(n,u,x,y,b,c,d) result(s)
        
        ! PARAMETERS
        integer, intent(in) :: n
            !! The number of data points.
        real(wp), intent(in) :: u
            !! The abscissa at which the spline is to be evaluated.
        real(wp), intent(in) :: x(n)
            !! The array of data abscissas.
        real(wp), intent(in) :: y(n)
            !! The array of data ordinates.
        real(wp), intent(in) :: b(n), c(n), d(n)
            !! Array of spline coefficients computed by `spline`.

        ! RETURNS
        real(wp) :: s
            !! The value of the cubic spline evaluated at the given abscissa `u`.
        
        integer, save :: i = 1
        real(wp) :: dx

        if (u >= x(i+1)) then
            print *, u, "performed search"
            i = binary_search(x,u)
        end if

        if (i == 0) then
            i = 1
        else if (i == n) then
            i = n - 1
        end if

        ! Evaluate spline
        dx = u - x(i)
        s = y(i) + dx*(b(i) + dx*(c(i) + dx*d(i)))
    end function

!>  Find the place of a value in an ordered table. 
!
!   Returns a value `i` such that the value `x` lies between the elements `samples(i)` and `samples(i+1)` of a table.
!   A return value equal to `0` or the size of the table `size(samples)` means the value `x` lies out of the range.
!   @warning The `samples` array must be either  monotonically increasing or monotonically decreasing.
!
!### References
!   * Numerical recipes
    pure function binary_search(samples,x) result(i)

        ! PARAMETERS
        real(wp), intent(in) :: samples(:)
            !! A 1D array of tabulated values (must be monotonic). 
        real(wp), intent(in) ::  x
            !! The value of which position we are searching for in `samples`.
        
        ! RETURNS
        integer :: i
            !! An integer `i` so that `samples(i) <= x <= samples(i+1)` is true.

        integer :: n, lower, middle, upper
        logical :: global_order, local_order

        n = size(samples)

        ! Set limits of binary search process
        lower = 0
        upper = n + 1

        ! Check if samples are increasing or decreasing
        ! If .true. samples are increasing, if .false. decreasing
        global_order = samples(n) >= samples(1)

        ! Perform bisection to find interval
         do while (upper - lower > 1)
            middle = (lower + upper)/2
            local_order = x >= samples(middle)
            if (local_order .eqv. global_order) then
                lower = middle
            else
                upper = middle
            end if
        end do

        ! Assign value to index
        if (x == samples(1)) then
            i = 1
        else if (x == samples(n)) then
            i = n - 1
        else
            i = lower
        end if
    end function

end module


program test_spline
    use precision, only : wp
    use spline_module
    use polynomial_module, only : linspace

    integer, parameter :: n = 21
    real(wp) :: x(n), y(n), b(n), c(n), d(n)
    real(wp), allocatable :: xx(:), s(:)

    integer :: i

    x = linspace(-1._wp,1._wp,n)
    y = 1._wp/(1._wp + 25._wp*x**2)

    call spline(n,x,y,b,c,d)

    xx = linspace(-1._wp,1._wp,201)
    s = [(seval(n,xx(i),x,y,b,c,d),i=1,201)]
    ! stop

    do i = 1, 201
        print *, xx(i), s(i)
    end do
    print*, (size(xx))

    print *,
    print *,

    do i = 1, n
        print *, x(i), y(i)
    end do
end program