! Caleb Wherry (Alone)
! October 24th, 2010
! Program to integrate a given function using Composite Simpsons Method

program compositeSimpsons1d
  use integrationMethods
  implicit none

  double precision :: a, b, tol, approx, approxOld
  integer :: m, n, step

  ! Tolerance
  tol = 1.0d-14

  ! Subdivisions for simpsons rule:
  m = 2

  ! Max number of simpsons methods to calculate:
  n = 100

  ! Interval to integrate over:
  a = -3.0d0
  b = 3.0d0

  ! Step
  step = 1

  ! Initialize approximations:
  approx = huge(1.0d+0)
  approxOld = 0.0d0

  ! Simpsons Method approximations:
  write(6,*)
  write(6,*) 'Composite Simpsons 1D Approximations:'
  write(6,*) 'a = ' ,a, 'b = ' ,b
  do while ( (abs(approx-approxOld) > tol) .and. (step < n) )
    approxOld = approx
    approx = compSimpsons1d(a,b,m)
    write(6,*) 'Step = ' ,step, 'Subdivisions = ' ,m, 'Approx = ', approx
    m = 2*m
    step = step + 1
  end do

  write(6,*)
  write(6,*) 'Final data after loop termination:'
  write(6,*) 'Step = ' ,step, 'Subdivisions = ' ,m, 'Approx = ', approx
  write(6,*)  

end program compositeSimpsons1d
