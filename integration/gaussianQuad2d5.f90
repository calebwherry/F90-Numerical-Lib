! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given 2D function using Gaussian Quadrature
!   with n=5

program gaussianQuad2d5
  use integrationMethods
  implicit none

  double precision :: a, b, c, d

  ! First interval to integrate over:
  a = 2.0d0
  b = 5.0d0

  ! Second interval to integrate over:
  c = PI/6.0d0
  d = PI/3.0d0

  ! Gaussian Quadrature approximations:
  write(6,*)
  write(6,*) 'Gaussian Quadrature 2D Approximation w/ n=5:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  write(6,*) 'c = ' ,c, 'd = ' ,d
  write(6,*) 'Approx = ', gaussQuad2d5(a,b,c,d)
  write(6,*)

end program gaussianQuad2d5
