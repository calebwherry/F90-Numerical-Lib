! Caleb Wherry (Alone)
! September 9th, 2010
! Code to find approximate zeros by the bisection method.
! Function to be evaluated is in function f(m).

program bisection
  implicit none

  double precision :: a,b,f,tol,dx,m
  integer :: step, max_iter

  ! Maximum interations in case no convergence
  max_iter = 100

  ! Endpoints:
  a = 0.0d0
  b = 1.0d0

  ! Tolerance level, continue calculations until this threshhold has been reached:
  tol = 1.0d-4

  ! Difference in the endpoints:
  dx = b - a

  ! Midpoint between the endpoints:
  m = 0.0d0

  ! Number of steps it takes to get to tolerance level:
  step = 1

  ! Do until our tolerance level has been obtained or max_iter reached:
  do while ( (abs(dx) > tol) .and. (step < max_iter) )

    ! Calculate midpoint between endpoints:
    m = a + ( (b - a)/2.0d0 )

    ! Check to see if f(m) = 0, if so we have the answer.
    if ( f(m) == 0.0d0 ) then
      write(*,*) 'Step: ', step , ' a: ', a , ' b: ', b , 'm: ', m , ' dx: ', dx
      stop
    endif

    ! Check to see if x = ( a OR b ), if so stop computing.
    if ( (m == a) .or. (m == b) ) then
      write(*,*) '*** Midpoint calculated as existing endpoint! Cannot converge, program terminating. ***'
      call abort
    endif

    ! Check to see if endpoints have opposite signs.
    if ( f(a)*f(b) >= 0.0d0 ) then
      write(*,*) '*** Endpoints have to have opposite function value signs. ***'
      call abort
    endif

    ! Print out info:
    write(*,*) 'Step: ', step , ' a: ', a , ' b: ', b , 'm: ', m , ' dx: ', dx

    ! If the function values at the endpoints have different signs, then the
    !    value we want is in the interval [a,x] (Thus we set b = x)
    ! On the other hand, if the endpoints have the same sign, then the
    !    value we are looking for is in the interval [x,b] (Thus we set a = x)
    if ( f(a)*f(m) < 0.0d0 ) then
      b = m
    else
      a = m
    end if

    ! Recalculate dx to see if our tolerance has been exceeded.
    dx = b - a

    ! Increase step size by 1:
    step = step + 1

  end do

  ! Print out final info that terminated the above loop.
  write(*,*)
  write(*,*) 'Final data after termination of our loop:'
  write(*,*) 'Step: ', step , ' a: ', a , ' b: ', b , 'm: ', m , ' dx: ', dx

end program bisection

! Function to calculate function value at x:
double precision function f(x) 
  implicit none

  double precision, intent(in) :: x

  ! Calculate function value at a given x:
  f = sqrt(x) - cos(x)

  return

end function f
