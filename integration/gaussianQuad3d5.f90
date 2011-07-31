! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given 3D function using Gaussian Quadrature
!   with n=5

program gaussianQuad3d5
  use integrationMethods
  implicit none

  double precision :: a, b, c, d, e, f_var

  ! First interval to integrate over:
  a = -1.0d0
  b = 1.0d0

  ! Second interval to integrate over:
  c = -1.0d0
  d = 1.0d0

  ! Third intervail to integrate over:
  e = -1.0d0
  f_var = 1.0d0

  ! Gaussian Quadrature approximations:
  write(6,*)
  write(6,*) 'Gaussian Quadrature Approximation w/ n=5:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  write(6,*) 'c = ' ,c, 'd = ' ,d
  write(6,*) 'e = ' ,e, 'f = ', f_var
  write(6,*) 'Approx = ', gaussQuad3d5(a,b,c,d,e,f_var)
  write(6,*)

end program gaussianQuad3d5
