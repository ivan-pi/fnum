program test_small

    use iso_fortran_env, only: real32, real64, real128

    implicit none


    real(real32) :: r1
    real(real64) :: r2
    real(real128) :: r3


    print *, epsilon(r1), tiny(r1), huge(r1)
    print *, epsilon(r2), tiny(r2), huge(r2)
    print *, epsilon(r3), tiny(r3), huge(r3)

end program