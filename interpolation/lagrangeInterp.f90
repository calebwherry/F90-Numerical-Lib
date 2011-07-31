! Caleb Wherry (Alone)
! October 4th, 2010
! Interpolate a set of data points using lagrange polynomials.

program lagrange
  implicit none

  double precision , allocatable, dimension(:) :: xNodes, yNodes
  double precision :: x,p
  integer :: i,n,numDataPoints

  ! Read in first line of file which tells me how many files I have:
  open(unit=9, file='points.dat', status='old')
  read(9,*) numDataPoints

  ! Just to make sure I read in the right data:
  write(*,*) 'Number of Data Points: ', numDataPoints

  ! Degree of polynomial is actually numDataPoints - 1, set to n for brevity sake:
  n = numDataPoints - 1

  ! Allocate arrays needed for holding data points:
  allocate( xNodes(0:n),yNodes(0:n) )

  ! Read in data points:
  do i = 0, n
    read(9,*) xNodes(i),yNodes(i)
  end do

  ! We don't need the file anymore so we can close it:
  close(9)

  ! Display the data we read in to check if it is the correct data:
  write(*,*) 'Data:'

  do i = 0, n
    write(*,*) xNodes(i), yNodes(i)
  end do

  ! Check to see if our p function is working correctly:
  write(*,*) 'X, Y, P(x)'

  do i = 0, n
    x = xNodes(i)
    write(*,*) xNodes(i), yNodes(i), p( n,x,xNodes,yNodes )
  end do

  ! File to print to:
  open(8, file='interpolatedPoints.dat', status='replace')

  ! Select 100 evenly space x numbers between x0 and xn, compute p(x),
  !   print x,p(x) out to a file for plotting

  do i = 1, 100
    x = xNodes(0) + dble(i)*( xNodes(n)-xNodes(0) )/100.0d0
    write(8,*) x,p( n,x,xNodes,yNodes )
  end do  

  close(8)

  deallocate(xNodes,yNodes)

end program lagrange

! Function to to compute lagrange polynomial for given n and k
double precision function lnk( n,k,x,xNodes )
  implicit none

  integer, intent(in) :: n,k
  double precision, intent(in) :: x, xNodes(0:n)
  integer :: j

  lnk = 1.0d0

  do j = 0, n
    if (j /= k) then
      lnk = lnk * ( x-xNodes(j) )/( xNodes(k) - xNodes(j) )
    end if
  end do

  return

end function lnk

! Function to calculate each polynomial term of interpolating polynomial:
double precision function p( n,x,xNodes,yNodes )
  implicit none
  
  integer, intent(in) :: n
  integer :: j,k
  double precision, intent(in) :: x,xNodes(0:n),yNodes(0:n)
  double precision :: lnk

  p = 0.0d0

  do k = 0, n
    p = p + yNodes(k)*lnk(n,k,x,xNodes)
  end do

  return

end function p
