CDECK  ID>, LINE.
c	Package for integration and interpolation
c	of a function, defined by array.


	function glin_integ_ar(x,y,q,x1,x2,thresh)
c
c	It makes the same work as lin_integ_ar
c	but at some conditions it interpolates no the line
c	but power function.
c

	implicit none
	real glin_integ_ar
	real x(*),y(*),x1,x2,thresh
	integer q
	
	integer nr,nrr,n1,i
	real xt1,xt2
	real xr1,xr2
	real a,b
	real k,p
	real s
	s=0
	glin_integ_ar=0.0
	if(q.le.0)return
	if(x2.lt.x1 .or. x2.lt.x(1) .or. x1.gt.x(q))return

	if(x1.lt.x(1))then
		xt1=x(1)
	else
		xt1=x1
	endif
	do i=2,q
		if(x(i).gt.xt1)then
			n1=i
			goto 10
		endif
	enddo
10	continue
	nr=n1-1
	if(x2.gt.x(q))then	! it is not necessary
		xt2=x(q)
	else
		xt2=x2
	endif
	xr2=xt1
c	write(6,*)' x1=',x1,' x2=',x2,' xt1=',xt1,' xt2=',xt2
c	write(6,*)' n1=',n1,' n2=',n2,' n1=',n1,' nr=',nr
	do nrr=nr,q-1
		if(x(nrr).gt.x2)go to 20
		xr1=xr2
		if(xt2.lt.x(nrr+1))then
			xr2=xt2
		else
			xr2=x(nrr+1)
		endif
		if(x(nrr).gt.500.0e-6.and.x(nrr).gt.2*thresh.and.
     +		y(nrr+1).lt.y(nrr).and.y(nrr+1).gt.0.0)then
			p=dlog(dble(y(nrr))/y(nrr+1))/
     +			  dlog(dble(x(nrr+1))/x(nrr))
			k=y(nrr)*x(nrr)**p
			s=s+
     +			k/(1-p)*(1.0/xr2**(p-1)-1.0/xr1**(p-1))
c		write(6,*)' nrr=',nrr,' p=',p,' k=',k,' s=',s	
		else	
                a = (y(nrr+1) - y(nrr))/(x(nrr+1) - x(nrr))
                b = y(nrr)
                s = s+
     +		0.5*a*(xr2*xr2 - xr1*xr1) + (b - a*x(nrr))*(xr2 - xr1)
		endif
c		write(6,*)' nrr=',nrr
c		write(6,*)' y(nrr)=',y(nrr),' y(nrr+1)=',y(nrr+1)
c		write(6,*)' xr1=',xr1,' xr2=',xr2
c		write(6,*)' x(nrr)=',x(nrr),' x(nrr+1)=',x(nrr+1)
c		write(6,*)' s=',s
	enddo

20	glin_integ_ar=s

	end

	function lin_integ_ar(x,y,q,x1,x2)

	implicit none
	real lin_integ_ar
	real x(*),y(*),x1,x2
	integer q
	
	integer nr,nrr,n1,i
	real xt1,xt2
	real xr1,xr2
	real a,b
	real s
	s=0
	lin_integ_ar=0.0
	if(q.le.0)return
	if(x2.lt.x1 .or. x2.lt.x(1) .or. x1.gt.x(q))return

        if(x1.lt.x(1))then
                xt1=x(1)
        else
                xt1=x1
        endif
        do i=2,q
                if(x(i).gt.xt1)then
                        n1=i
                        goto 10
                endif
        enddo
10      continue
        nr=n1-1
        if(x2.gt.x(q))then      ! it is not necessary
                xt2=x(q)
        else
                xt2=x2
        endif
        xr2=xt1
c	write(6,*)' x1=',x1,' x2=',x2,' xt1=',xt1,' xt2=',xt2
c	write(6,*)' n1=',n1,' n2=',n2,' n1=',n1,' nr=',nr
	do nrr=nr,q-1
                if(x(nrr).gt.x2)go to 20
		xr1=xr2
                if(xt2.lt.x(nrr+1))then
                        xr2=xt2
                else
                        xr2=x(nrr+1)
                endif
                a = (y(nrr+1) - y(nrr))/(x(nrr+1) - x(nrr))
                b = y(nrr)
                s = s+
     +		0.5*a*(xr2*xr2 - xr1*xr1) + (b - a*x(nrr))*(xr2 - xr1)
c		write(6,*)' nrr=',nrr
c		write(6,*)' y(nrr)=',y(nrr),' y(nrr+1)=',y(nrr+1)
c		write(6,*)' xr1=',xr1,' xr2=',xr2
c		write(6,*)' x(nrr)=',x(nrr),' x(nrr+1)=',x(nrr+1)
c		write(6,*)' s=',s
	enddo

20	lin_integ_ar=s

	end


	function step_integ_ar(x,y,q,x1,x2)
c	
c	dimension of y must be q
c	dimension of x must be q+1
c	the last point means the end of last interval.
c
	implicit none
	real step_integ_ar
	real x(*),y(*),x1,x2
	integer q
	
	integer nr,nrr,n1,i
	real xt1,xt2
	real xr1,xr2
c	real a,b
	real s
	s=0
	step_integ_ar=0.0
c	write(6,*)' step:',q,x1,x2,x(1),x(q+1)
	if(q.le.0)return
	if(x2.lt.x1 .or. x2.lt.x(1) .or. x1.gt.x(q+1))return

        if(x1.lt.x(1))then
                xt1=x(1)
        else
                xt1=x1
        endif
        do i=2,q+1
                if(x(i).gt.xt1)then
                        n1=i
                        goto 10
                endif
        enddo
10      continue
        nr=n1-1
        if(x2.gt.x(q+1))then      ! it is not necessary
                xt2=x(q+1)
        else
                xt2=x2
        endif
        xr2=xt1


	do nrr=nr,q
                if(x(nrr).gt.x2)go to 20

		xr1=xr2
		if(xt2.lt.x(nrr+1))then
			xr2=xt2
		else
			xr2=x(nrr+1)
		endif
                s = s+ y(nrr)*(xr2-xr1)

c	write(6,*)' nrr=',nrr,' xr=',xr1,xr2
c	write(6,*)' y(nrr)=',y(nrr),' s=',s

	enddo

20	step_integ_ar=s


	end

	function interp_line_arr(x,y,q,tr,x0)
c	
c	special code
c	If x0<tr => 0
c	If tr<x0<x(1) linear interp.
c	If x0>x(q)  exponential interp., if it go down
c	

	implicit none

	real interp_line_arr
	integer q	! quantity of elements
	real x(*)	! abscissa
	real y(*)	! ordin.
	real tr 	! low treshold
	real x0		! point

	integer n,n1,n2
	real p,k
	real s

	if(x0.lt.tr)then
		interp_line_arr=0.0	
		return
	endif

	if(x0.gt.x(q))then
	    if(y(q-1).le.y(q))then
                interp_line_arr=0.0
                return
            endif
	    p = alog(y(q-1)/y(q)) / alog(x(q-1)/x(q))
	    k = y(q) / (x(q)**p)
	    s = k * (x0 ** p)
	    interp_line_arr = s
	    return
	endif			
	
	do n=2,q
		if(x0.le.x(n))then
			n1=n-1
			go to 10
		endif
	enddo
10	n2=n1+1

	k = (y(n2)-y(n1)) / (x(n2)-x(n1))
	s = y(n1) + k * ( x0-x(n1))
	interp_line_arr = s
c	write(6,*)' n1,n2=',n1,n2
c	write(6,*)' x=',x(n1),x(n2)
c	write(6,*)' y=',y(n1),y(n2)
c	write(6,*)' k,s=',k,s
c	stop
	return

	end

	function interp_linep_arr(x,y,q,tr,x0)
c	
c	special code
c	If x0<tr => 0
c	If tr<x0<x(1) linear interp.
c	If x0>x(q)  exponential interp., if it go down
c	If x(i+1).lt.x(i) then power line
c	

	implicit none

	real interp_linep_arr
	integer q	! quantity of elements
	real x(*)	! abscissa
	real y(*)	! ordin.
	real tr 	! low treshold
	real x0		! point

	integer n,n1,n2
	real p,k
	real s

	if(x0.lt.tr)then
		interp_linep_arr=0.0	
		return
	endif

	if(x0.gt.x(q))then
*	    if(y(q-1).le.y(q))then
*                interp_linep_arr=0.0
*                return
*            endif
*	    p = alog(y(q-1)/y(q)) / alog(x(q-1)/x(q))
	    p=-3.22
	    k = y(q) / (x(q)**p)
	    s = k * (x0 ** p)
	    interp_linep_arr = s
	    return
	endif			
	
	do n=2,q
		if(x0.le.x(n))then
			n1=n-1
			go to 10
		endif
	enddo
10	n2=n1+1

	if(y(n2).ge.y(n1))then
	    k = (y(n2)-y(n1)) / (x(n2)-x(n1))
	    s = y(n1) + k * ( x0-x(n1))
	else
	    p = alog(y(n1)/y(n2)) / alog(x(n1)/x(n2))
	    k = y(n1) / (x(n1)**p)
	    s = k * (x0 ** p)
	endif
	interp_linep_arr = s
c	write(6,*)' n1,n2=',n1,n2
c	write(6,*)' x=',x(n1),x(n2)
c	write(6,*)' y=',y(n1),y(n2)
c	write(6,*)' p,k,s=',p,k,s
c	stop
	return

	end
