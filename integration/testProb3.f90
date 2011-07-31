program testProb3
  use integrationMethods
  implicit none

  double precision :: a,b,df,tol,dx,m0,m1
  integer :: step, max_iter, n

  ! Maximum iterations if case no convergence
  max_iter = 100

  ! Starting interval:
  a = 0.0d0
  b = 2.0d0
  n = 10

  ! Tolerance level:
  tol = 1.0d-5

  ! Change in values of b and a to check against tol:
  dx = b - a

  ! Slope of the line between the endpoints is a good
  ! starting point guess for newton's method:
  !m0 = a + ( (b - a)/2.0d0 )
  ! We can also just make a guess and use that:
   m0 = 0.5d0

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
    m1 = m0 - ( compTrapezoid1d(a,m0,n) - 0.45d0 )/df(m0)

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


end program testProb3


! Function to calculate the derivative of the function at x:
double precision function df(x)
  use integrationMethods
  implicit none

  double precision, intent(in) :: x

  ! Derivative of f(x):
  df = (1.0d0/sqrt(2.0d0*PI)) * exp(-x**2/2.0d0)

end function df
