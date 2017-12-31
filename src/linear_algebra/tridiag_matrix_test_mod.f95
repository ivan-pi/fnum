module tridiag_test_mod
    use precision, only: wp
    use tridiag_matrix_mod, only: tridiag_matrix
    implicit none

    private
    public test1, advection, reaction

contains

    subroutine test1()
        integer, parameter :: n = 3
        real(wp) :: rhs(n) = [3.0_wp, 8.0_wp, 16.0_wp]
        real(wp) :: low(n-1) = [1.0_wp, 3.0_wp]
        real(wp) :: high(n-1) = [2.0_wp, 4.0_wp]
        real(wp) :: mid(n) = [1.0_wp, 3.0_wp, 13.0_wp]
        real(wp) :: ans(n)

        type(tridiag_matrix) :: M

        M = tridiag_matrix(n,low,mid,high)
        print *, "Given tridiagonal matrix diagonals:"
        call M%print()
        print *, "Given right-hand side:"
        print *, rhs

        ans = M .solve. rhs
        print *, "Computed solution:"
        print *, ans
        print *, "Error = ", sum(abs(M*ans - rhs))

        call M%free()

    end subroutine test1

    subroutine advection()
        real(wp), parameter :: Peclet = 10.0_wp
        integer, parameter :: n = 101
        real(wp) :: dx, coef
        type(tridiag_matrix) :: M
        real(wp) :: rhs(n-2), ans(n)
        integer :: i

        dx = 1.0_wp/real(n-1,wp)
        coef = 0.5*Peclet*dx

        M = tridiag_matrix(n-2,1.0_wp+coef,-2.0_wp,1.0_wp-coef)

        call M%print()

        rhs = 0.0_wp
        rhs(n-2) = coef - 1

        print*, rhs
        ans(1) = 0.0_wp
        ans(n) = 1.0_wp
        ans(2:n-1) = M .solve. rhs

        do i = 1, n
            print *, ans(i), analytical(Peclet, real(i-1,wp)*dx)
        end do

        print *, "Error = ", sum(abs(M*ans(2:n-1) - rhs))


    contains

        elemental function analytical(Pe,x) result(u)
            real(wp), intent(in) :: Pe, x
            real(wp) :: u
            u = (exp(Pe*x) - 1.0_wp)/(exp(Pe) - 1.0_wp)
        end function

    end subroutine advection

    subroutine reaction()
        real(wp), parameter :: Thiele = 1.0
        integer, parameter :: n = 1001
        real(wp) :: dx, coef
        real(wp) :: lower(n-1), main(n), upper(n-1), rhs(n), ans(n)
        type(tridiag_matrix) :: M
        integer :: i

        dx = 1.0/real(n-1,wp)
        coef = 2 + Thiele**2*dx**2

        main(1) = 1
        main(2:n) = -coef

        lower(1:n-2) = 1
        lower(n-1) = 2

        upper(1) = 0
        upper(2:n-1) = 1

        M = tridiag_matrix(n,lower,main,upper)

        rhs = 0
        rhs(1) = 1

        ans = M .solve. rhs

        do i = 1, n
            print *, ans(i), analytical(Thiele,real(i-1,wp)*dx)
        end do

        print *, "Error = ", sum(abs(M*ans - rhs))

    contains

        real(wp) function analytical(Th,x)
            real(wp), intent(in) :: Th, x
            analytical = cosh(Th*(1-x))/cosh(Th)
        end function

    end subroutine reaction

end module