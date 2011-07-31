! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Romberg's Method

program rombergGaussianQuad1d5
  use integrationMethods
  implicit none

  double precision , allocatable, dimension(:,:) :: Rom
  double precision :: a, b
  integer :: i, n, m, p

  ! Variable for nice output of matrix:
  p = 0

  ! Number of initial subdivisions:
  m = 10

  ! Number of gaussian rule to calculate:
  n = 4

  ! Interval to integrate over:
  a = 10.0d0
  b = 5.0d0

  allocate( Rom(0:n-1,0:n-1) )

  Rom = rombergGaussQuad1d5(a,b,n-1,m)

  write(6,*)
  write(6,*) 'Romberg Approximations of Gaussian Quad 1D w/ n=5:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  do i=0,n-1
    write(6, "(1000(1x,f0.16))") Rom(i,0:p) 
    p = p + 1
  end do
  write(6,*)

  deallocate( Rom )

end program rombergGaussianQuad1d5
