! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Romberg's Method

program rombergCompTrapezoid2d
  use integrationMethods
  implicit none

  double precision , allocatable, dimension(:,:) :: Rom
  double precision :: a, b, c ,d
  integer :: i, k, m, n, p, q

  ! Variable for nice output of matrix:
  p = 0

  ! Initial subdivisions for trapezoid rule - xDir:
  n = 100

  ! Initial subdivisions for trapezoid rule - yDir
  m = 100

  ! Number of Romberg rows to calculate:
  q = 4

  ! First interval to integrate over:
  a = -2.0d0
  b = 2.0d0

  ! Second interval to integrate over:
  c = -2.0d0
  d = 2.0d0

  allocate( Rom(0:q-1,0:q-1) )

  Rom = rombergCompTrap2d(a,b,c,d,n,m,q)

  write(6,*)
  write(6,*) 'Romberg Approximations:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  write(6,*) 'c = ' ,c, 'd = ' ,d
  do i=0,q-1
    write(6, "(1000(1x,f0.16))") Rom(i,0:p) 
    p = p + 1
  end do
  write(6,*)

  deallocate( Rom )

end program rombergCompTrapezoid2d
