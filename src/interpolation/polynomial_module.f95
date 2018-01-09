module polynomial_module
    
    use precision, only : wp

    implicit none
    private

    public :: polyval, linspace

!>  
!   Evaluate a polynomial at a specific value.    
!
!   If `a` is coefficent vector of length `n`, this function returns the value
!   `p = a(1) + a(2)*x**2 + a(3)*x**3 + ... + a(n)*x**n`.
!
!   If `x` is a sequence, then *p(x)* is returned for each element of `x`.
    interface polyval
        module procedure :: polyval_scalar
        module procedure :: polyval_array
    end interface

contains


    function polyval_scalar(a,x,b) result(p)

        ! PARAMETERS
        real(wp), intent(in) :: a(:)
            !! 1D array of polynomial coefficients \(a_j\) (including those equal to zero) from 
            !! the constant term to the coefficients of highest degree.
        real(wp), intent(in) :: x
            !! The \(x\) value where we would like to evaluate the polynomial.
        real(wp), intent(out), optional :: b(:)
            !! The divided coefficent vector.

        ! RETURNS
        real(wp) :: p

        integer :: i, n
        real(wp), allocatable :: b_(:)

        n = size(a)
        allocate(b_(n))

        b_(n) = a(n)
        do i = n-1, 1, -1
            b_(i) = a(i) + b_(i+1)*x
        end do
        p = b_(1)

        if (present(b)) b = b_
    end function

    function polyval_array(a,x) result(p)

        ! PARAMETERS
        real(wp), intent(in) :: a(:)
            !! The coefficients \(a_j\) of the polynomial.
        real(wp), intent(in) :: x(:)
            !! An array of \(x_j\) values at which we would like to evaluate the polynomial.

        ! RETURNS
        real(wp) :: p(size(x))
            !! The array of evaluated polynomial values.

        integer :: i

        p = [(polyval_scalar(a,x(i)),i=1,size(x))]
    end function

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

        endpoint_ = .true.
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
end module




! program test_polynomial
!     use precision, only : wp
!     use polynomial_module, only : polyval, linspace

!     real(wp) :: xx = 2.0_wp

!     real(wp), allocatable :: a(:), x(:), p(:)
!     real(wp) :: b(3)


!     x = linspace(0._wp,4._wp,200)

!     ! 1 + 2x + 3x^2
!     a = [1._wp,2._wp,3._wp]

!     ! print *, 3*xx**2+2*xx+1
!     ! print *, polyval(a,xx,b)
!     ! print *, b

!     p = polyval(a,x)
!     do i = 1, size(x)
!         print *, x(i), p(i)
!     end do
! end program