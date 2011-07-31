program finalProject
  implicit none

  double precision :: PI,t_0,t_i,t_f,h,y(8),dy(8),k1(8),k2(8),k3(8),k4(8), theta
  integer :: n,i

  ! Constant Pi:
  PI = 3.14159265358979323846

  ! Initial y
  y = (/  1.0d0,    & ! Xe
          0.0d0,    & ! Ye
          0.0d0,    & ! Vex
          2.0d0*PI, & ! Vey
         -1.0d0,    & ! Xae
          0.0d0,    & ! Yae
          0.0d0,    & ! Vaex
          -2.0d0*PI /) ! Vaey

  ! Initial t_0 value
  t_0 = 0.0d0

  ! t final
  t_f = 10.0d0

  ! Initialize t_i to t_0
  t_i = t_0

  ! Number of subdivisions
  n = 100000


  ! Step size
  h = (t_f - t_0) / dble(n)
  write(6,*) h

  ! File to write data to:
  open(9, file='Earth_X_Orbit.dat', status='replace')
  open(10, file='Earth_Y_Orbit.dat', status='replace')
  open(11, file='AntiEarth_X_Orbit.dat', status='replace')
  open(12, file='AntiEarth_Y_Orbit.dat', status='replace')
  open(13, file='theta.dat', status='replace')

  write(9,*) t_0, y(1)
  write(10,*) t_0, y(2)
  write(11,*) t_0, y(5)
  write(12,*) t_0, y(6)

  do i=1, n

    theta = acos( (y(1)*y(5) + y(2)*y(6)) / (sqrt(y(1)**2+y(2)**2) * sqrt(y(5)**2 + y(6)**2)) )
    if (abs(theta-PI) > 0.3d0) then
      write(13,*) theta
    endif

    call f(t_i,y,dy)
    call f(t_i+h/2.0d0,y+h*dy/2.0d0,k1)
    call f(t_i+h/2.0d0,y+h*dy/2.0d0,k2)
    call f(t_i+h,y+h*dy,k3)

    y = y + h*(k1 + 2.0d0*(k2+k3) + k4) / 6.0d0
    t_i = t_i + h

    write(9,*) t_i, y(1)
    write(10,*) t_i, y(2)
    write(11,*) t_i, y(5)
    write(12,*) t_i, y(6)

  end do

  close(9)
  close(10)
  close(11)
  close(12)

end program finalProject

subroutine f(t,y,dy)
  implicit none

  double precision, intent(in)::t,y(8)
  double precision, intent(out)::dy(8)
  double precision :: axe, aye, axae, ayae, G, Me, Ms, Mj, Mae, PI, d_es, d_ej, d_eae, d_aes, d_aej, d_aee

  ! Defined all the constants here for simplicity, although speed might take a small hit b/c
  !   of overheard each time the subroutine is called. Oh well...

  PI = 3.14159265358979323846
  G = 4.0d0*PI**2
  Me = 1.0d0 / 300000.0d0
  Mae = Me
  Ms = 1.0d0
  Mj = 1.0d0 / 1000.0d0

  d_es  = sqrt( (y(1))**2 + (y(2))**2 )
  d_ej  = sqrt( (y(1) - 4.0d0*cos(t*PI/6.0d0))**2 + (y(2) - 4.0d0*sin(t*PI/6.0d0))**2 )
  d_eae = sqrt( (y(1) - y(5))**2 + (y(2) - y(6))**2 )

  d_aes  = sqrt( (y(5))**2 + (y(6))**2 )
  d_aej  = sqrt( (y(5) - 4.0d0*cos(t*PI/6.0d0))**2 + (y(6) - 4.0d0*sin(t*PI/6.0d0))**2 )
  d_aee  = sqrt( (y(5) - y(1))**2 + (y(6) - y(2))**2 )

  axe  = (G*(-1.0d0)*y(1))/d_es**3 + (G*Mj*(4.0d0*cos(t*PI/6.0d0)-y(1)))/d_ej**3 + (G*Mae*(y(5)-y(1)))/d_eae**3
  aye  = (G*(-1.0d0)*y(2))/d_es**3 + (G*Mj*(4.0d0*sin(t*PI/6.0d0)-y(2)))/d_ej**3 + (G*Mae*(y(6)-y(2)))/d_eae**3

  axae = (G*(-1.0d0)*y(5))/d_aes**3 + (G*Mj*(4.0d0*cos(t*PI/6.0d0)-y(5)))/d_aej**3 + (G*Me*(y(1)-y(5)))/d_aee**3
  ayae = (G*(-1.0d0)*y(6))/d_aes**3 + (G*Mj*(4.0d0*sin(t*PI/6.0d0)-y(6)))/d_aej**3 + (G*Me*(y(2)-y(6)))/d_aee**3

  dy(1)=y(3)
  dy(2)=y(4)
  dy(3)= axe
  dy(4)= aye
  dy(5)=y(7)
  dy(6)=y(8)
  dy(7)= axae
  dy(8)= ayae

  return

end
