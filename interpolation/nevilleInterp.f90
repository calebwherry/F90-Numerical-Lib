! Caleb Wherry (Alone)
! October 4th, 2010
! Interpolate a set of data points using neville's method.

program neville
  implicit none

  double precision , allocatable, dimension(:) :: xNodes, yNodes
  double precision :: x,nev
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
  write(*,*) 'X, Y, Nev(x)'

  do i = 0, n
    x = xNodes(i)
    write(*,*) xNodes(i), yNodes(i), nev( n,x,xNodes,yNodes )
  end do

  ! File to print to:
  open(8, file='interpolatedPoints.dat', status='replace')

  ! Select 100 evenly space x numbers between x0 and xn, compute p(x),
  !   print x,p(x) out to a file for plotting

  do i = 1, 100
    x = xNodes(0) + dble(i)*( xNodes(n)-xNodes(0) )/100.0d0
    write(8,*) x,nev( n,x,xNodes,yNodes )
  end do  

  ! Close file to write to:
  close(8)

  deallocate(xNodes,yNodes)

end program neville

! Function to to compute polynomials using nevilles method:
double precision function nev( n,x,xNodes,yNodes )
  implicit none

  integer, intent(in) :: n
  double precision, intent(in) :: x, xNodes(0:n), yNodes(0:n)
  integer :: i,k
  double precision :: lnk
  double precision, allocatable, dimension(:,:) :: a

  allocate( a(0:n,0:n) )

  a = 1.0d0

  a(0:n,0) = yNodes

  do k = 1, n
    do i = k, n
      a(i,k) = ( (x-xNodes(i-k))*a(i,k-1) - (x-xNodes(i))*a(i-1,k-1) )/( xNodes(i)-xNodes(i-k) )
    end do
  end do

  nev = a(n,n)

  deallocate(a)

  return

end function nev
