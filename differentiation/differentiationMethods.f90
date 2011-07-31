! Caleb Wherry
! Created: Nov. 4th, 2010
! Last Modified: Nov. 4th, 2010
! Module with Differentiation Methods
!
! A) Function to be derivated
!
! First Derivatives:
! ------------------
! 1) Two-Point Formula
! 2) Three-Point Endpoint Formula
! 3) Three-Point Midpoint Formula
! 4) Five-Point Endpoint Formula
! 5) Five-Point Midpoint Formula
!
! Second Derivatives:
! -------------------
! 6) Three-Point Midpoint Formula

module differentiationMethods
  implicit none

  contains

    ! A) Function to be derivated
    double precision function f(x)

      double precision, intent(in) :: x

      f = x**2 * exp(x)

      return 

    end function f

    ! 1) Two-Point Formula - 1st Deriv
    double precision function twoPoint1st(x,h)

      double precision, intent(in) :: x,h

      twoPoint1st = ( f(x+h) - f(x) ) / h

      return

    end function twoPoint1st

    ! 2) Three-Point Endpoint - 1st Deriv
    double precision function threePointEndpoint1st(x,h)

      double precision, intent(in) :: x,h

      threePointEndpoint1st = ( 1.0d0/(2.0d0*h) ) * &
                              ( -3.0d0*f(x) + 4.0d0*f(x+h) - f(x+2.0d0*h) )

      return

    end function threePointEndpoint1st

    ! 3) Three-Point Midpoint - 1st Deriv
    double precision function threePointMidpoint1st(x,h)

      double precision, intent(in) :: x,h

      threePointMidpoint1st = ( 1.0d0/(2.0d0*h) )*( f(x+h) - f(x-h) )

      return

    end function threePointMidpoint1st

    ! 4) Five-Point Endpoint - 1st Deriv
    double precision function fivePointEndpoint1st(x,h)

      double precision, intent(in) :: x,h

      fivePointEndpoint1st = ( 1.0d0/(12.0d0*h) ) * &
                             ( -25.0d0*f(x) + 48.0d0*f(x+h) - 36.0d0*f(x+2.0d0*h) + &
                               16.0d0*f(x+3.0d0*h) - 3.0d0*f(x+4.0d0*h) &
                             )

      return

    end function fivePointEndpoint1st

    ! 5) Five-Point Midpoint - 1st Deriv
    double precision function fivePointMidpoint1st(x,h)

      double precision, intent(in) :: x,h

      fivePointMidpoint1st = ( 1.0d0/(12.0d0*h) ) * &
                             ( f(x-2.0d0*h) - 8.0d0*f(x-h) + 8.0d0*f(x+h) - &
                               f(x+2.0d0*h) &
                             )

      return

    end function fivePointMidpoint1st

    ! 6) Three-Point Midpoint - 2nd Deriv
    double precision function threePointMidpoint2nd(x,h)

      double precision, intent(in) :: x,h

      threePointMidpoint2nd = ( 1.0d0/h**2  ) * ( f(x-h) - 2.0d0*f(x) + f(x+h) )

      return

    end function threePointMidpoint2nd

end module differentiationMethods
