! Caleb Wherry (Alone)
! Created: Nov. 3, 2010
! Last Modified: Nov. 3, 2010
! Solve differential equation using euler's method

program euler
  implicit none

  double precision :: a,b,y_0,y_i,h,t,f
  integer :: n,i

  ! Interval
  a = 0.0d0
  b = 2.0d0

  ! Initial value
  y_0 = 0.5d0

  ! Number of steps
  n = 100

  ! Size of step
  h = (b-a) / dble(n)

  ! Initialize t to a
  t = a

  ! Initialize y_i to y_0
  y_i = y_0

  !write(6,*) 'i = 0   t = ' ,t, 'y = ' ,y_0
  write(6,*) t, y_0

  do i=1, n
    y_i = y_i + h*f(t,y_i)
    t = t + h
    !write(6,*) 'i = ' ,i, 't = ' ,t, 'y = ' ,y_i
    write(6,*) t, y_i
  end do

end program euler

double precision function f(t,y)
  implicit none

  double precision, intent(in) :: t,y

  f = 2.0d0 * t * y

end function f
