module tridiag_matrix_mod
    
    use precision, only: wp
    
    implicit none
    private

    public :: tridiag_matrix
    public :: thomas, solve, transpose, size, allocated
    public :: operator(*), operator(.T.)

!>  A tridiagonal matrix type.
!
!
    type :: tridiag_matrix
        private
        integer :: n = 0    !! The size of the matrix (length of the main diagonal).
        real(wp), allocatable :: lo(:) !! The lower diagonal.
        real(wp), allocatable :: ma(:) !! The main diagonal.
        real(wp), allocatable :: up(:) !! The upper diagonal.
    contains
        procedure :: print => tridiag_print             !! Print the tridiagonal matrix.
        procedure :: free => tridiag_free               !! Deallocate the tridiagonal matrix.
        procedure, nopass :: solve => tridiag_solve
        procedure :: lower => tridiag_lower             !! Accessor function of the lower diagonal.
        procedure :: main => tridiag_main               !! Accessor function of the main diagonal.
        procedure :: upper => tridiag_upper             !! Accessor function of the upper diagonal.
        procedure :: size => tridiag_size               !! Accessor function for the size.
        procedure :: allocated => tridiag_allocated     !! Check if the TDM is allocated.
        procedure :: set_lower => tridiag_set_lower     !! Set the lower diagonal.
        procedure :: set_main => tridiag_set_main       !! Set the main diagonal.
        procedure :: set_upper => tridiag_set_upper     !! Set the upper diagonal.
        procedure :: transpose => tridiag_transpose     !! Return the transpose.
        procedure :: allocate => tridiag_allocate       !! Allocate the tridiagonal matrix.
    end type


    !> Tridiagonal matrix constructor.
    interface tridiag_matrix
        module procedure tridiag_alloc
        module procedure tridiag_scalars
        module procedure tridiag_vectors
        module procedure tridiag_array
    end interface


    !> Transpose function.
    interface transpose
        module procedure tridiag_transpose
    end interface

    !> Transpose operator.
    interface operator(.T.)
        module procedure tridiag_transpose
    end interface

    !> Multiply operator.
    interface operator (*)
        module procedure tridiag_by_vector
        module procedure tridiag_by_scalar
    end interface


    interface solve
        module procedure tridiag_solve
    end interface

    interface size
        module procedure tridiag_size
    end interface

    interface allocated
        module procedure tridiag_allocated
    end interface

contains


!>  Returns the transpose of a tridiagonal matrix
    pure function tridiag_transpose(T) result(S)

        ! PARAMETERS
        class(tridiag_matrix), intent(in) :: T   !! A tridiagonal matrix.
        
        ! RETURNS
        type(tridiag_matrix) :: S   !! The transpose of the given matrix.

        S = tridiag_alloc(T%n)
        S%up = T%lo
        S%ma = T%ma
        S%lo = T%up
    end function


!>  Allocates a tridiagonal matrix.
    pure function tridiag_alloc(n) result(T)

        ! PARAMETERS
        integer, intent(in) :: n
            !! Size of the tridiagonal matrix.

        ! RETURNS
        type(tridiag_matrix) :: T
            !! An empty tridiagonal matrix.

        T%n = n
        allocate(T%lo(n-1))
        allocate(T%ma(n))
        allocate(T%up(n-1))
    end function

!>  Checks if the passed tridiagonal matrix is allocated.
    pure logical function tridiag_allocated(T)
        class(tridiag_matrix), intent(in) :: T
        logical :: lo, ma, up

        tridiag_allocated = .false.

        lo = allocated(T%lo)
        ma = allocated(T%ma)
        up = allocated(T%up)

        if (lo .and. ma .and. up) tridiag_allocated = .true.
    end function

    subroutine tridiag_allocate(T,n)
        class(tridiag_matrix), intent(inout) :: T
        integer, intent(in) :: n

        if (T%allocated()) then
            call T%free()
        else
            T%n = n
            allocate(T%lo(n-1))
            allocate(T%ma(n))
            allocate(T%up(n-1))
        end if
    end subroutine


!>   Prints the tridiagonal matrix to the screen.
    subroutine tridiag_print(T)

        ! PARAMETERS
        class(tridiag_matrix), intent(in) :: T !! A tridiagonal matrix
        
        integer :: n, i

        n = T%size()

        ! First row
        print *, 1, 0.0_wp, T%ma(1), T%up(1)

        ! Middle rows
        if (n > 2) then
            do i = 2, n - 1
                print*, i, T%lo(i-1), T%ma(i), T%up(i)
            end do
        end if

        ! Last row
        print *, n, T%lo(n-1), T%ma(n), 0.0_wp
    end subroutine


!>  Deallocates the diagonals of a tridiagonal matrix.
    subroutine tridiag_free(T)
        
        ! PARAMETERS
        class(tridiag_matrix), intent(inout) :: T
            !! The tridiagonal matrix to be deallocated.

        T%n = 0
        deallocate(T%lo)
        deallocate(T%ma)
        deallocate(T%up)
    end subroutine


!>  Creates a tridiagonal matrix with constant diagonals.
    pure function tridiag_scalars(n,sl,sm,su) result(T)

        ! PARAMETERS
        integer, intent(in) :: n    !! The size of the matrix.
        real(wp), intent(in) :: sl  !! Value of the lower diagonal.
        real(wp), intent(in) :: sm  !! Value of the main diagonal.
        real(wp), intent(in) :: su  !! Value of the upper diagonal

        ! RETURNS
        type(tridiag_matrix) :: T   !! A tridiagonal matrix with constant diagonals.

        T = tridiag_alloc(n)
        T%lo = sl
        T%ma = sm
        T%up = su
    end function


!>  Creates a tridiagonal matrix from the three diagonals.
    pure function tridiag_vectors(n,vl,vm,vu) result(T)
        
        ! PARAMETERS
        integer, intent(in) :: n        !! The size of the matrix.
        real(wp), intent(in) :: vl(n-1) !! A vector containing values of the lower diagonal.
        real(wp), intent(in) :: vm(n)   !! A vector containing values of the main diagonal.
        real(wp), intent(in) :: vu(n-1) !! A vector contianing values of the upper diagonal.

        ! RETURNS
        type(tridiag_matrix) :: T   !! A tridiagonal matrix.

        T = tridiag_alloc(n)
        T%lo = vl
        T%ma = vm
        T%up = vu
    end function


!>  Creates a tridiagonal matrix from a full matrix.
    pure function tridiag_array(A) result(T)

        ! PARAMETERS
        real(wp), intent(in) :: A(:,:)  !! A 2-dimensional array.

        ! RETURNS
        type(tridiag_matrix) :: T   !! A tridiagonal matrix.

        integer :: i, n

        n = min(size(A,dim=1),size(A,dim=2))

        T = tridiag_alloc(n)

        T%ma(1) = A(1,1)
        T%up(1) = A(1,2)
        do i = 2, n - 1
            T%lo(i-1) = A(i,i-1)
            T%ma(i) = A(i,i)
            T%up(i-1) = A(i,i+1)
        end do
        T%ma(n) = A(n,n)
        T%lo(n-1) = A(n,n-1)
    end function


!>  Returns the product of a tridiagonal matrix with a scalar.
    pure function tridiag_by_scalar(T,s) result(P)
        
        ! PARAMETERS
        class(tridiag_matrix), intent(in) :: T  !! A tridiagonal matrix.
        real(wp), intent(in) :: s   !! A scalar.
        
        ! RETURNS
        type(tridiag_matrix) :: P   !! The product of a tridagonal matrix with a scalar.

        P = tridiag_alloc(T%n)
        P%lo = s*T%lo
        P%ma = s*T%ma
        P%up = s*T%up
    end function


!>  Returns the product of a tridiagonal matrix with a vector.
    pure function tridiag_by_vector(T,v) result(P)
        
        ! PARAMETERS
        class(tridiag_matrix), intent(in) :: T  !! A tridiagonal matrix.
        real(wp), intent(in) :: v(T%n)       !! A vector.
        
        ! RETURNS
        real(wp) :: P(T%n)   !! The product vector.
        
        integer :: i

        associate(n => T%n, lo => T%lo, ma => T%ma, up => T%up)

            P(1) = ma(1)*v(1) + up(1)*v(2)
            do i = 2, n-1
                P(i) = lo(i-1)*v(i-1) + ma(i)*v(i) + up(i)*v(i+1)
            end do
            P(n) = lo(n-1)*v(n-1) + ma(n)*v(n)
        end associate
    end function

    !
    ! Solve a tridiagonal system (wrapper)
    !
    function tridiag_solve(T,rhs,iflag) result(x)
        
        ! PARAMETERS
        type(tridiag_matrix), intent(in) :: T
            !! A tridiagonal matrix
        real(wp), intent(in) :: rhs(T%n)
            !! The right-hand side of the system of equations.
        integer, intent(out), optional :: iflag 
            !! An optional error flag (0 indicates sucess, -1 indicates there was a zero pivot).
        
        ! RETURNS
        real(wp) :: x(T%n) !! Solution to the system of equations
        
        integer :: iflag_

        associate(n => T%n, lo => T%lo, ma => T%ma, up => T%up)

        ! Solve tridiagonal system of equations by calling the procedural style algorithm
        call thomas(n,[0._wp,lo],ma,[up,0._wp],rhs,x,iflag_)

        ! Return the error flag
        if (present(iflag)) iflag = iflag_

        end associate
    end function tridiag_solve

!>  Returns the lower diagonal of a tridiagonal matrix.
    pure function tridiag_lower(T) result(lo)
        class(tridiag_matrix), intent(in) :: T !! A tridiagonal matrix.
        real(wp) :: lo(T%n-1) !! The lower diagonal.

        lo = T%lo
    end function

!>  Returns the main diagonal of a tridiagonal matrix.
    pure function tridiag_main(T) result(ma)
        class(tridiag_matrix), intent(in) :: T !! A tridiagonal matrix.
        real(wp) :: ma(T%n) !! The main diagonal.
        ma = T%ma
    end function
    
!>  Returns the upper diagonal of a tridiagonal matrix.
    pure function tridiag_upper(T) result(up)
        class(tridiag_matrix), intent(in) :: T !! A tridagonal matrix.
        real(wp) :: up(T%n-1) !! The upper diagonal.
        up = T%up
    end function

!>  Returns the size of a tridiagonal matrix.
    pure function tridiag_size(T) result(n)
        class(tridiag_matrix), intent(in) :: T !! A tridagonal matrix.
        integer :: n !! The upper diagonal.
        n = T%n
    end function


!>  Set the lower diagonal of a tridiagonal matrix.
    pure subroutine tridiag_set_lower(T,lo)
        class(tridiag_matrix), intent(inout) :: T !! A tridiagonal matrix.
        real(wp), intent(in) :: lo(T%n-1) !! A vector containing the values of the lower diagonal.
        T%lo = lo
    end subroutine


!>  Set the main diagonal of a tridiagonal matrix.
    pure subroutine tridiag_set_main(T,ma)
        class(tridiag_matrix), intent(inout) :: T !! A tridiagonal matrix.
        real(wp), intent(in) :: ma(T%n) !! A vector containing the values of the main diagonal.
        T%ma = ma
    end subroutine
    
!>  Set the upper diagonal of a tridiagonal matrix.
    pure subroutine tridiag_set_upper(T,up)
        class(tridiag_matrix), intent(inout) :: T !! A tridagonal matrix.
        real(wp), intent(in) :: up(T%n-1) !! A vector containing the values of the upper diagonal.
        T%up = up
    end subroutine

    !
    ! Solves tridiagonal system by Thomas algorithm
    !
    subroutine thomas_with_side_effects(n,a,b,c,r,u)
            ! system size
        integer, intent(in) :: n
            ! note all vectors are equal length
            ! a -> lower diagonal
            ! b -> main diagonal
            ! c -> upper diagonal
            ! r -> right-hand side
        real(wp), intent(inout) :: a(n), b(n), c(n), r(n)
            ! u -> solution
        real(wp), intent(out) :: u(n)
        real(wp) :: denom   ! denominator
        real(wp) :: tol = epsilon(1.0_wp)    ! tolerance
        integer :: i

        ! WARNING: vectors c and r will be over-written

        ! The set of equations:
        !
        ! | b(1) c(1)  0    0      .      .     | |u(1)|   |r(1)|
        ! | a(2) b(2) c(2)  0      .      .     | |u(2)|   |r(2)|
        ! |  0   a(3) b(3) c(3)    .      .     | |u(3)|   |r(3)|
        ! |  .    .    .    .      .      .     | | .  | = | .  |
        ! |  .    .    .   a(n-1) b(n-1) c(n-1) | | .  |   | .  |
        ! |  .    .    .    .     a(n)   b(n)   | |u(n)|   |r(n)|

            ! check in case of zero pivot
        if (abs(b(1)) <= tol) stop "[tridiag_solve] zero in first row"

            !-------------------------------------------------
            ! forward substitution (decomposition)
        denom = b(1)
        c(1) = c(1)/denom;   r(1) = r(1)/denom;    ! first row
        do i = 2, n - 1
            denom = b(i) - a(i)*c(i-1)
            if (abs(denom) <= tol) stop "[tridiag_solve] zero pivot"
            c(i) = c(i)/denom
            r(i) = (r(i) - a(i)*r(i-1))/denom
        end do

            ! last row
        denom = b(n) - a(n)*c(n-1)
        if (abs(denom) <= tol) stop "[tridiag_solve] zero pivot"
        r(n) = (r(n) - a(n)*r(n-1))/denom

            !------------------------------------------------
            ! back substitution
        u(n) = r(n)
        do i = n - 1, 1, -1
            u(i) = r(i) - c(i)*u(i+1)
        end do

    end subroutine thomas_with_side_effects

!>  Solves a tridiagonal system of equations using the Thomas algorithm.
!
!   Solves the following set of equations:

    pure subroutine thomas(n,lo,ma,up,rhs,x,iflag)

        ! PARAMETERS
        integer, intent(in) :: n        !! Size of the system of equations.
        real(wp), intent(in) :: lo(n)   !! The lower diagonal (first element will be neglected).
        real(wp), intent(in) ::  ma(n)  !! The main diagonal.
        real(wp), intent(in) ::  up(n)  !! The upper diagonal (last element will be neglected).
        real(wp), intent(in) ::  rhs(n) !! The right-hand side of the system of equations.
        real(wp), intent(out) :: x(n)   !! The solution to the system of equations.
        integer, intent(out) :: iflag  !! An error flag (0 indicates sucess, -1 indicates there was a zero pivot).
        

        real(wp) :: temp(n)
        real(wp) :: denom, tol
        integer :: i

        iflag = 0
        tol = epsilon(1.0_wp)

        if (abs(ma(1)) <= tol) then
            iflag = -1  ! Zero in first row!
            return
        end if

        ! Forward substitution
        denom = ma(1)
        x(1) = rhs(1)/denom
        do i = 2, n
            temp(i) = up(i-1)/denom
            denom = ma(i) - lo(i)*temp(i)
            if (abs(denom) <= tol) then
                iflag = -1 ! Zero pivot!
                return
            end if
            x(i) = (rhs(i) - lo(i)*x(i-1))/denom
        end do

        ! Back substitution
        do i = n - 1, 1, -1
            x(i) = x(i) - temp(i+1)*x(i+1)
        end do


    end subroutine
end module
