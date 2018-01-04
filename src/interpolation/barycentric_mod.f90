module barycentric_mod
    
    use precision, only : wp

    implicit none
    private

    public :: barycentric_weights, barycentric_val
    public :: barycentric_derivative

    public :: barycentric_interpolant

    type :: barycentric_interpolant
        integer :: n = 0
        real(wp), allocatable :: x(:)
        real(wp), allocatable :: y(:)
        real(wp), allocatable :: w(:)
        logical :: alloc = .false.    
    contains
        procedure :: dx => der_matrix
    end type

    interface barycentric_interpolant
        module procedure new_barycentric_interpolant
        module procedure new_barycentric_interpolant_implicit
    end interface

    interface barycentric_val
        module procedure barycentric_val
        module procedure barycentric_val_array
    end interface

contains

    pure function new_barycentric_interpolant(n,x,y) result(this)
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(in) :: y(n)

        type(barycentric_interpolant) :: this

        this%n = n
        this%x = x
        this%y = y
        this%w = barycentric_weights(n,x,y)
        this%alloc = .true.
    end function


    pure function new_barycentric_interpolant_implicit(x,y) result(this)
        real(wp), intent(in) :: x(:)
        real(wp), intent(in) :: y(:)
        type(barycentric_interpolant) :: this

        integer :: n

        n = min(size(x),size(y))
        this = new_barycentric_interpolant(n,x(1:n),y(1:n))
    end function


    pure function barycentric_weights(n,x,y) result(w)
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(in) :: y(n)

        real(wp) :: w(n)

        real(wp) :: prod
        integer :: i, j

        do i = 1, n
            prod = 1._wp
            do j = 1, n
                if (j == i) cycle
                prod = prod*(x(j) - x(i))
            end do
            w(i) = 1._wp/prod
        end do
    end function


    pure function barycentric_val(n,x,y,w,xx) result(p)
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(in) :: y(n)
        real(wp), intent(in) :: w(n)
        real(wp), intent(in) :: xx

        real(wp) :: p
        real(wp) :: numer, denom, xdiff, temp

        integer :: i

        numer = 0
        denom = 0

        do i = 1, n
            xdiff = xx - x(i)
            if (xdiff == 0) then
                p = y(i)
                return
            end if
            temp = w(i)/xdiff
            numer = numer + temp*y(i)
            denom = denom + temp
        end do
        p = numer/denom
    end function

    function barycentric_val_array(n,x,y,w,xx) result(p)
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(in) :: y(n)
        real(wp), intent(in) :: w(n)
        real(wp), intent(in) :: xx(:)

        real(wp), allocatable :: p(:)
        real(wp), allocatable :: numer(:), denom(:), xdiff(:), temp(:)
        integer, allocatable :: exact(:)
        integer :: i, m

        logical, allocatable :: mask(:)

        m = size(xx)
        allocate(p(m),numer(m), denom(m), xdiff(m),temp(m),exact(m))
        numer = 0._wp
        denom = 0._wp
        exact = 0

        do i = 1, n
            xdiff = xx - x(i)
            temp = w(i)/xdiff
            numer = numer + temp*y(i)
            denom = denom + temp
            where (xdiff == 0) exact = i
        end do
        p = numer/denom

        do i = 1, m
            if (exact(i) > 0) then
                p(i) = y(exact(i))
            end if 
        end do

        ! TODO: Use ifort findloc routine
    end function


    function barycentric_derivative(n,x,y,w,k) result(pf)
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(in) :: y(n)
        real(wp), intent(in) :: w(n)
        integer, intent(in) :: k

        real(wp) :: pf
        real(wp) :: numer, denom, xdiff, temp

        integer :: j

        real(wp) :: l(n)
        numer = 0
        denom = 0
        
        l = 0._wp

        do j = 1, n
            xdiff = x(k) - x(j)
            if (j == k) cycle
            l(j) = w(j)/(w(k)*xdiff)
        end do
        l(k) = -sum(l)

        pf = dot_product(l,y)        
    end function

    function der_matrix(this,d) result(M)
        class(barycentric_interpolant), intent(in) :: this
        integer, intent(in) :: d

        real(wp) :: M(this%n,this%n), xdiff
        integer :: i, j

        M = 0
        select case(d)
        case(1)
        do i = 1, this%n
            do j = 1,this%n
                xdiff = this%x(i) - this%x(j)
                if (j == i) cycle
                M(i,j) = this%w(j)/(this%w(i)*xdiff)
            end do
            M(i,i) = -sum(M(i,:))
        end do
        case(2)
            print *, "not implemented yet"
        case default
            print *, "should not be here"
        end select
    end function
end module


program test_barycentric

    use precision, only: wp
    use barycentric_mod
    use interpolation_mod, only: polyval

    implicit none

    integer, parameter :: n = 11
    integer :: i

    real(wp), allocatable :: x(:), y(:), w(:), a(:), x2(:), y2(:),yd(:)
    type(barycentric_interpolant) :: b

    a = [1._wp,2._wp,3._wp]

    x = [(real(i-1,wp)*0.1_wp,i=1,n)]
    y = polyval(a,x)

    b = barycentric_interpolant(x,y)

    w = barycentric_weights(n,x,y)


    ! print *, polyval(a,0.1_wp), barycentric_val(n,x,y,w,0.1_wp)

    ! print *, polyval([2._wp,6._wp],x(n-1)), barycentric_derivative(n,x,y,w,n-1)

    x2 = [(real(i-1,wp)*0.01_wp,i=1,101)]

    y2 = barycentric_val(n,x,y,w,x2)

    yd = matmul(b%dx(1),y)

    do i = 1, 101
        print *, x2(i), y2(i),yd(i)
    end do
    print *,
    print *,
    do i = 1, n
        print *, x(i),y(i),yd(i)
    end do
end program