! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Gaussian Quadrature
!   with n=5

program gaussianQuad1d5
  use integrationMethods
  implicit none

  double precision :: a, b

  ! Interval to integrate over:
  a = 10.0d0
  b = 5.0d0

  ! Gaussian Quadrature approximations:
  write(6,*)
  write(6,*) 'Gaussian Quadrature Approximation w/ n=5:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  write(6,*) 'Approx = ', gaussQuad1d5(a,b)
  write(6,*)

end program gaussianQuad1d5
