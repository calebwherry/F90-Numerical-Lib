! Caleb Wherry (Alone)
! September 9th, 2010
! Code to find approximate zeros by Newton's method.
! Function to be evaluated is in function f(x).
! The functions derivative is in function df(x).

program newton
  implicit none

  double precision :: a,b,f,df,tol,dx,m0,m1
  integer :: step, max_iter

  ! Maximum iterations if case no convergence
  max_iter = 100

  ! Starting interval:
  a = 0.0d0
  b = 0.5d0

  ! Tolerance level:
  tol = 1.0d-5

  ! Change in values of b and a to check against tol:
  dx = b - a

  ! Slope of the line between the endpoints is a good
  ! starting point guess for newton's method:
  m0 = a + ( (b - a)/2.0d0 )
  ! We can also just make a guess and use that:
  ! m0 = -4.0d0

  ! Check to see if m0 = ( a OR b ), if so stop computing.
  if ( (m0 == a) .or. (m0 == b) ) then
    write(*,*) '*** Midpoint calculated as existing endpoint! Cannot converge, program terminating. ***'
    call abort
  endif

  ! Step number:
  step = 1

  ! Do calculations until TOL is reached or max_iter reached:
  do while ( (abs(dx) > tol) .and. (step < max_iter) )

    ! This is where the bulk of the calculations are done.
    ! The slope of the tangent line at the chosen guess (m0)
    !   is defined by the equation below:
    m1 = m0 - f(m0)/df(m0)

    ! Print out info:
    write(*,*) 'Step: ', step , ' x: ', m0 , ' dx: ', dx

    ! Recalculate the difference between the values to check
    !   it against the tol:
    dx = m1 - m0

    ! m0 is set to the calculated tangent line above.
    ! This is done until the tol is reached:
    m0 = m1

    ! Increase step number by 1:
    step = step + 1

  end do

  ! Print out final info after loop termination:
  write(*,*)
  write(*,*) 'Final data after loop termination:'
  write(*,*) 'Step: ', step , ' x: ', m0 , ' dx: ', dx


end program newton

! Function to calculate function value at x:
double precision function f(x) 
  implicit none

  double precision, intent(in) :: x

  ! f(x):
  f = x**3

  return

end function f

! Function to calculate the derivative of the function at x:
double precision function df(x)
  implicit none

  double precision, intent(in) :: x

  ! Derivative of f(x):
  df = x**2

end function df
