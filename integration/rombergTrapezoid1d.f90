! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Romberg's Method

program rombergCompTrapezoid1d
  use integrationMethods
  implicit none

  double precision , allocatable, dimension(:,:) :: Rom
  double precision :: a, b
  integer :: i, k, m, n, p

  ! Variable for nice output of matrix:
  p = 0

  ! Initial subdivisions for trapezoid rule:
  m = 20

  ! Number of trapezoid rules to calculate:
  n = 4

  ! Interval to integrate over:
  a = 10.0d0
  b = 5.0d0

  allocate( Rom(0:n,0:n) )

  Rom = rombergCompTrap1d(a,b,m,n)

  write(6,*)
  write(6,*) 'Romberg Approximations of Composite Trapezoid 1D:'
  do i=0,n
    write(6, "(1000(1x,f0.16))") Rom(i,0:p) 
    p = p + 1
  end do
  write(6,*)

  deallocate( Rom )

end program rombergCompTrapezoid1d
