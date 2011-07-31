! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Composite Wherry's Method

program compositeWherrys1d
  use integrationMethods
  implicit none

  double precision :: a, b
  integer :: i, m, n

  ! Subdivisions for Wherry's rule:
  m = 8

  ! Number of Wherry's methods to calculate:
  n = 8

  ! Interval to integrate over:
  a = 0.0d0
  b = 2.0d0

  ! Simpsons Method approximations:
  write(6,*)
  write(6,*) 'Composite Wherrys 1D Approximations:'
  write(6,*) 'a = ' ,a, 'b = ', b
  do i=0,n
    write(6,*) 'Step = ' ,i, 'Subdivisions = ' ,m, 'Approx = ', compWherrys1d(a,b,m)
    m = 2*m
  end do
  write(6,*)

end program compositeWherrys1d
