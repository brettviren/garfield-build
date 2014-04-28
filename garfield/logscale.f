CDECK  ID>, LOGSCALE.
	subroutine logscale(q,xmin,xmax,x,xc)
c
c	Make a logariphmic mesh.
c
	implicit none
	integer q
	real xmin,xmax
	real x(*),xc(*)

	real rk,xr
	integer i
	rk=(xmax/xmin)**(1.0/q)
	xr=xmin
	x(1)=xr

	do i=2,q+1
		x(i)=xr*rk
		xc(i-1)=(x(i-1)+x(i))*0.5
		xr=x(i)
	enddo

	end

	subroutine logscale0(q,xmin,xmax,x,xc)
c
c	Make a logariphmic mesh with linear begin.
c	First, the logariohmic scale is calculated.
c	Second, the program tries to prolong it to zero
c	with the same number of points.
c	So several points of begin of logariphmic scale will be recalculeted.
c
	implicit none
	integer q
	real xmin,xmax
	real x(*),xc(*)
	integer i,j
	real r,h

	call logscale(q,xmin,xmax,x,xc)

	if(q.ge.2)then

	do i=2,q
	  r = x(i) / ( x(i+1) - x(i) )
	  if( r .le. i-1 )then
	    h = x(i) / ( i - 1 )
	    x(1) =  0.0
	    do j = 2,i
	      x(j) = h * ( j - 1 )
	      xc(j-1) = (x(j) + x(j-1))*0.5
	    enddo
	    go to 10
	  endif
	enddo
	write(6,*)' error in logscale0'
	stop

	else

	  write(6,*)' error in logscale0'
	  stop

	endif

10	end
