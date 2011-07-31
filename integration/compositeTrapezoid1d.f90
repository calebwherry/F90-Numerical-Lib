! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Composite Trapezoid Rule

program compositeTrapezoid1d
  use integrationMethods
  implicit none

  double precision :: a, b
  integer :: i, m, n

  ! Subdivisions for trapezoid rule:
  m = 8

  ! Number of trapezoid rules to calculate:
  n = 8

  ! Interval to integrate over:
  a = 10.0d0
  b = 5.0d0

  ! Loop to compute Trapezoid approximations:
  write(6,*)
  write(6,*) 'Composite Trapezoid 1D Approximations:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  do i=0, n
    write(6,*) 'Step = ' ,i, 'Subdivisons = ' ,m, 'Approx = ' , compTrapezoid1d(a,b,m)
    m = 2*m
  end do
  write(6,*)

end program compositeTrapezoid1d
