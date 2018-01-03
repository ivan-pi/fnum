module math_mod

    use precision, only: wp

    implicit none
    private

    public :: pi, pi_2, pi_4, pi_8
    public :: euler
    public :: golden, golden_ratio

    real(wp), parameter :: pi = 4._wp*atan(1.0_wp)
    real(wp), parameter :: pi_2 = 0.5_wp*pi
    real(wp), parameter :: pi_4 = 0.25_wp*pi
    real(wp), parameter :: pi_8 = 0.125_wp*pi

    real(wp), parameter :: euler = exp(1.0_wp)

    real(wp), parameter :: golden = 0.5_wp*(1._wp + sqrt(5._wp))
    real(wp), parameter :: golden_ratio = golden
    
end module