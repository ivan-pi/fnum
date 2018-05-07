module math_mod

    use precision, only: wp

    implicit none
    private

    public :: one, two
    public :: pi, pi_2, pi_4, pi_8, tau
    public :: euler
    public :: golden, golden_ratio

    real(wp), parameter :: one = 1.0_wp
    real(wp), parameter :: two = 2.0_wp

    real(wp), parameter :: pi = 4._wp*atan(1.0_wp)
    real(wp), parameter :: pi_2 = 0.5_wp*pi
    real(wp), parameter :: pi_4 = 0.25_wp*pi
    real(wp), parameter :: pi_8 = 0.125_wp*pi

    real(wp), parameter :: tau = 2*pi ! https://www.youtube.com/watch?v=WywQ6a3uQ5I&t=1430s

    real(wp), parameter :: euler = exp(1.0_wp)

    real(wp), parameter :: golden = 0.5_wp*(1._wp + sqrt(5._wp))
    real(wp), parameter :: golden_ratio = golden
    
end module