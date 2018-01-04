module precision
    
    implicit none

    private

    integer, parameter :: sp = kind(1.0e0) !! Single precision.
    integer, parameter :: dp = kind(1.0d0) !! Double precision.

    integer, parameter, public :: wp = dp !! Working precision

end module