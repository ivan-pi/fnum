!>
!  author: Ivan Pribec
!  date: 23/12/2016
!
!  An object-oriented implementation of a lagrange polynomial.

module lagrange_polynomial_module

    use precision, only: wp

    implicit none
    private

    public :: LagrangePolynomial, linspace, from_optional

    type, public :: LagrangePolynomial
        !! Class for interpolation using Lagrange polynomials
        private
        integer :: n !! Number of points
        real(wp), allocatable :: x(:) !! The point \(x\)-values
        real(wp), allocatable :: y(:) !! The function values \(f(x)\)
    contains
            procedure :: val !! Evaluates the polynomial at a position.
            procedure :: d_dx
    end type

    interface LagrangePolynomial
        module procedure new_LagrangePolynomial
    end interface

contains

!>  Sets a default or optional value.
    function from_optional(opt,default)
        
        ! PARAMETERS
        integer, intent(in), optional :: opt
            !! An optional integer value.
        integer, intent(in) :: default
            !! The default value.
        
        ! RETURNS
        integer :: from_optional
            !! The value that will be set.

        from_optional = default
        if (present(opt)) from_optional = opt
    end function


  
!>  
!   Return evenly spaced numbers over a specified interval.
!
!   Returns `num` evenly spaced samples, calculated over the interval `[start, stop]`. 
!
!### References
!   See the numpy version of this routine.
!
! author: Ivan Pribec
    function linspace(start,end,num,endpoint,step) result(samples)
        
        ! PARAMETERS
        real(wp), intent(in) :: start 
            !! The starting value of the sequence.
        real(wp), intent(in) :: end
            !! The end value of the sequence, unless `endpoint` is set to `.false.`. 
            !! In that case, the sequence consists of all but the last of `num + 1` 
            !! evenly spaced samples, so that `end` is excluded. Note that the 
            !! step size changes when `endpoint` is `.false.`.
        integer, intent(in), optional :: num
            !! Number of samples to generate. Default value is 50.
        logical, intent(in), optional :: endpoint
            !! If `.true.`, `end` is the last sample. Otherwise, it is not included. Default is `.true.`.
        real(wp), intent(out), optional :: step
            !! If present, `step` is the size of spacing between samples.

        ! RETURNS
        real(wp), allocatable :: samples(:)
            !! There are `num` equally spaced samples in the closed interval `[start, stop]` or 
            !! the half-open interval `[start, stop)` (depending on whether `endpoint` is `.true.` or `.false.`).

        integer :: num_, i
        logical :: endpoint_
        real(wp) :: step_

        num_ = from_optional(num,50)

        if (present(endpoint)) endpoint_ = endpoint

        ! find step size
        if (endpoint_) then
            step_ = (end - start)/real(num_-1,wp)
        else
            step_ = (end - start)/real(num_,wp)
        end if

        if (present(step)) step = step_

        samples = [(start + real(i-1,wp)*step_, i = 1, num_)]
    end function linspace


!>
!   Calculates the value of the interpolation polynomial as the linear
!   combination \[L(x) = \sum^n_{i=1} y_i l_i(x)\]
!   of Lagrange basis polynomials
!   \[l_j(x) = \prod_{1 \leq m \leq n, m \neq j} \frac{x-x_m}{x_j-x_m}\]
!   where \(1 \leq i \leq n\).

    elemental function val(poly,x) result(value)

        ! Parameters
        class(LagrangePolynomial), intent(in) :: poly
            !! An object representing the polynomial \(p\).
        real(wp), intent(in) :: x
            !! The \(x\)-value at which to evaluate \(p\).

        ! Returns
        real(wp) :: value
            !! The value of \(p(x)\).

        real(wp) :: product
        integer :: i, j

        value = 0.0_wp
        do i = 1, poly%n
            product = 1.0_wp
            do j = 1, poly%n
                if (i /= j) then
                    product = product*(x - poly%x(j))/(poly%x(i) - poly%x(j))
                end if
            end do
            value = value + poly%y(i)*product
        end do
    end function
    !*


    pure elemental function d_dx(this,x) result(dp)
        class(LagrangePolynomial), intent(in) :: this
        real(wp), intent(in) :: x
        real(wp) :: sum, prod
        real(wp) :: dp
        integer :: j, l, m

        dp = 0.0_wp
        do j = 1, this%n
            sum = 0.0_wp
            do l = 1, this%n
                prod = 1.0_wp
                do m = 1, this%n
                    if (m==l .or. m == j) then
                        continue
                    else
                        prod = prod*(x - this%x(m))/(this%x(j)-this%x(m))
                    end if
                end do
                if (l == j) then
                    continue
                else
                    sum = sum + prod/(this%x(j)-this%x(l))
                end if
            end do

            dp = dp + sum*this%y(j)
        end do
    end function

!>  Constructor for the Lagrange polynomial.
!
!   Constructs a Lagrange polynomial from a set of points \((x_j,y_j)\) where 
!   \(1 \leq j \leq n\).
!
!   @note The arrays `x` and `y` should be of length `n` and have no duplicate points.
!   display: public
    pure function new_LagrangePolynomial(n,x,y) result(poly)

        ! PARAMETERS
        integer, intent(in) :: n
            !! The number of points.
        real(wp), intent(in) :: x(n)
            !! A set of \(x_j\) values.
        real(wp), intent(in) :: y(n)
            !! A set of \(y_j\) values.

        ! RETURNS
        type(LagrangePolynomial) :: poly
            !! An object representing the Lagrange polynomial.

        poly%n = n
        poly%x = x
        poly%y = y
    end function
end module