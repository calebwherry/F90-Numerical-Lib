! Caleb Wherry (Alone)
! October 4th, 2010
! Interpolate a set of data points using newton's method.

program newton
  implicit none

  double precision , allocatable, dimension(:) :: xNodes, yNodes, coef
  double precision :: x,eval
  integer :: i,k,n,numDataPoints

  ! Read in first line of file which tells me how many files I have:
  open(unit=9, file='points.dat', status='old')
  read(9,*) numDataPoints

  ! Just to make sure I read in the right data:
  write(*,*) 'Number of Data Points: ', numDataPoints

  ! Degree of polynomial is actually numDataPoints - 1, set to n for brevity sake:
  n = numDataPoints - 1

  ! Allocate arrays needed for holding data points:
  allocate( xNodes(0:n),yNodes(0:n),coef(0:n) )

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

  ! Compute coef and display them:
  call computecoef( n,xNodes,yNodes,coef )

  write(*,*) 'Newton Coefficients:'
  do i = 0, n
    write(*,*) coef(i)
  end do

  ! Check to see if our p function is working correctly:
  write(*,*)
  write(*,*) 'X, Y, Eval(x)'

  do i = 0, n
    x = xNodes(i)
    write(*,*) xNodes(i), yNodes(i), eval( n,x,xNodes,coef )
  end do

  ! File to print to:
  open(8, file='interpolatedPoints.dat', status='replace')

  ! Select 100 evenly space x numbers between x0 and xn, compute p(x),
  !   print x,p(x) out to a file for plotting

  do i = 1, 100
    x = xNodes(0) + dble(i)*( xNodes(n)-xNodes(0) )/100.0d0
    write(8,*) x,eval( n,x,xNodes,coef )
  end do  

  ! Close file to write to:
  close(8)

  deallocate(xNodes,yNodes,coef)

end program newton

! Subroutine to compute newton's coefficients
subroutine computecoef( n,xNodes,yNodes,coef )
  implicit none

  integer, intent(in) :: n
  integer :: i,k
  double precision, intent(in) :: xNodes(0:n), yNodes(0:n)
  double precision, intent(out) :: coef(0:n)
  double precision, allocatable, dimension(:,:) :: a

  allocate( a(0:n,0:n) )
  a = 0.0d0

  do i = 0, n
    a(i,0) = yNodes(i)
  end do

  write(*,*) 'Initial a:'
  do i = 0, n
    write(*,*) ( a(i,k),k=0,n )
  end do

  write(*,*)

  do k = 1, n
    do i = k, n
      a(i,k) = ( a(i,k-1)-a(i-1,k-1) )/( xNodes(i)-xNodes(i-k) ) 
    end do
  end do

  do i = 0, n
    coef(i) = a(i,i)
  end do

  do i = 0, n
    write(*,*) ( a(i,k),k=0,n )
  end do

  write(*,*)

  deallocate(a)

  return

end subroutine computecoef

! Function to to evaluate values for polynomials using newtons coef
double precision function eval( n,x,xNodes,coef )
  implicit none

  integer, intent(in) :: n
  double precision, intent(in) :: x, xNodes(0:n), coef(0:n)
  integer :: i

  eval = coef(n)

  do i = n-1, 0, -1
    eval = eval * ( x-xNodes(i) ) + coef(i)
  end do

  return

end function eval
