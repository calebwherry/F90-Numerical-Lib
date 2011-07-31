! Caleb Wherry (Alone)
! Created: Nov. 3, 2010
! Last Modified: Nov. 3, 2010
! Solve differential equation using Runge-Kutta 4 method

program rungeKutta4
  implicit none

  double precision :: y_0,y_i,t_0,t_i,t_f,h,f,k1,k2,k3,k4
  integer :: n,i

  ! Initial y_0 value
  y_0 = 0.5d0

  ! Initial t_0 value
  t_0 = 0.0d0

  ! T final
  t_f = 2.0d0

  ! Initialize t_i to t_0
  t_i = t_0 

  ! Initialize y_i to y_0
  y_i = y_0 

  ! Number of subdivisions
  n = 10

  ! Step size
  h = (t_f - t_0) / dble(n)

  ! File to write data to:
  open(9, file='points.dat', status='replace')

  write(6,*) 'h = ' ,h
  write(6,*) 'i = 0   t = ' ,t_0, 'y = ' ,y_0
  write(9,*) t_0, y_0

  do i=1, n

    k1 = h*f(t_i,y_i)
    k2 = h*f(t_i+h/2.0d0,y_i+k1/2.0d0)
    k3 = h*f(t_i+h/2.0d0,y_i+k2/2.0d0)
    k4 = h*f(t_i+h,y_i+k3)

    y_i = y_i + (k1 + 2.0d0*(k2+k3) + k4) / 6.0d0
    t_i = t_i + h

    write(6,*) 'i = ' ,i, 't = ' ,t_i, 'y = ' ,y_i
    write(9,*) t_i, y_i

  end do

  close(9)

end program rungeKutta4

double precision function f(t,y)
  implicit none

  double precision, intent(in) :: t,y

  !f = t*exp(3.0d0*t) - 2.0d0*y
  f = y - t**2 + 1

end function f
