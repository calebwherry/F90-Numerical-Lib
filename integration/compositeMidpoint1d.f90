! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Composite Midpoint Method

program compositeMidpoint1d
  use integrationMethods
  implicit none

  double precision :: a, b, tol, approx, approxOld
  integer :: m, n, step

  ! Step 
  step = 1

  ! Tolerance threshhold
  tol = 1.0d-5

  ! Subdivisions for midpoint rule:
  m = 2

  ! Max number of midpoint methods to calculate:
  n = 40

  ! Interval to integrate over:
  a = -3.0d0
  b = 3.0d0

  ! Approximation intilizations:
  ! First approx is set to largest double value available:
  approx = huge(1.0d+0)
  approxOld = 0.0d0

  ! Midpoint Method approximations:
  write(6,*)
  write(6,*) 'Composite Midpoint 1D Approximations:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  do while ( (abs(approx-approxOld) > tol) .and. (step < n) )
    approxOld = approx
    approx = compMidpoint1d(a,b,m)
    write(6,*) 'Step = ' ,step, 'Subdivisions = ' ,m, 'Approx = ', approx
    m = 2*m
    step = step + 1
  end do
 
  write(6,*)
  write(6,*) 'Final data after loop termination:'
  write(6,*) 'Step = ' ,step, 'Subdivisions = ' ,m, 'Approx = ', approx

end program compositeMidpoint1d
