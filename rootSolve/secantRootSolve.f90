! Caleb Wherry (Alone)
! September 9th, 2010
! Code to find approximate zeros by the secant method.
! Function to be evaluated is in function f(x).

program secant
  implicit none

  double precision :: a,b,f,tol,dx,x0,x1,x2
  integer :: step, max_iter

  ! Max interations in case doesn't converge
  max_iter = 100

  ! Starting interval:
  a = -1.0d0
  b = 1.0d0

  ! Tolerance level:
  tol = 1.0d-5

  ! We'll take x0 = a initially:
  x0 = a

  ! We'll take x1 = b initially:
  x1 = b

  ! Change in values of x0 and x1 to check against tol:
  dx = (x0 - x1)

  ! Step number:
  step = 1

  ! Do calculations until tol is reached or max_iter reached:
  do while ( (abs(dx) > tol) .and. (step < max_iter) )

    ! Formula for getting the secant line:
    x2 = x1 - ( f(x1)*(x1-x0) )/( f(x1) - f(x0) ) 

    ! Print out info:
    write(*,*) 'Step: ', step , ' x: ', x0 , ' dx: ', dx

    ! Reset x0, x1, and dx:
    x0 = x1
    x1 = x2
    dx = x1-x0

    ! Increase step number by 1:
    step = step + 1

  end do

  ! Print out final info that terminated the loop:
  write(*,*)
  write(*,*) 'Final data after termination of our loop:'
  write(*,*) 'Step: ', step , ' x: ', x0 , ' dx: ', dx


end program secant

! Function to calculate function value at x:
double precision function f(x) 
  implicit none

  double precision, intent(in) :: x

  f = 7.8d0*x**2 + 32.17d0*( exp(x) - exp(-x) - 2.0d0*sin(x))

  return

end function f
