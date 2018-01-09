PROGRAM test_print
  REAL ::  x = 5
  NAMELIST /name/ x
  
  interface
    function func(x)
        real, intent(in) :: x
        real :: func
    end function
  end interface


contains

    function sine(x)
        real, intent(in) :: x
        real :: sine

        sine = sin(x)
    end function

END PROGRAM test_print