CDECK  ID>, VECTORS.
c	several subroutines for vector algebra
c	single accuracy

	subroutine GoOldSys(e1,e2,e3,v,ov)
c
c	Go to old system
c
	implicit none

	real e1(3),e2(3),e3(3)	! coordinates of new orts in the old
	real v(3)		! vector in the new system
	real ov(3)		! vector in the old system
c	real s
c	integer i

	ov(1)=v(1)*e1(1) + v(2)*e2(1) + v(3)*e3(1)
	ov(2)=v(1)*e1(2) + v(2)*e2(2) + v(3)*e3(2)
	ov(3)=v(1)*e1(3) + v(2)*e2(3) + v(3)*e3(3)

c	write(6,*)' GoOldSys'
c	write(6,*)' v=',v
c	write(6,*)' ov=',ov
c	write(6,*)' e1=',e1
c	write(6,*)' e2=',e2
c	write(6,*)' e3=',e3
c	s=0.0
c	do i=1,3
c		s=s+e1(i)*e1(i)
c	enddo
c	write(6,*)' abs(e1)=',s
c	s=0.0
c	do i=1,3
c		s=s+e2(i)*e2(i)
c	enddo
c	write(6,*)' abs(e2)=',s
c	s=0.0
c	do i=1,3
c		s=s+e3(i)*e3(i)
c	enddo
c	write(6,*)' abs(e3)=',s
c	s=0.0
c	do i=1,3
c		s=s+ov(i)*ov(i)
c	enddo
c	write(6,*)' abs(ov)=',s
	

	end

	subroutine MakeNewSys(e1,e2,e3,v)
c
c	Make new system
c
	implicit none

	real e1(3),e2(3),e3(3)	! coordinates of new orts in the old

	real v(3)		! vector (equal)

	real s
	integer i

	do i=1,3
		e3(i)=v(i)
	enddo
	if(e3(2).eq.0.0.and.e3(3).eq.0.0)then
		e1(1)=0.0
		e1(2)=0.0
		e1(3)=-1.0
		e2(1)=0.0
		e2(2)=1.0
		e2(3)=0.0
c	write(6,*)' v=',v
c	write(6,*)' e1=',e1
c	write(6,*)' e2=',e2
c	write(6,*)' e3=',e3
		return
	endif
	e2(1)=0.0
	e2(2)=e3(3)
	e2(3)=-e3(2)
	s=0.0
	do i=1,3
		s=s+e2(i)*e2(i)
	enddo	
	s=sqrt(s)
	do i=1,3
		e2(i)=e2(i)/s
	enddo

	e1(1)=e2(2)*e3(3)-e3(2)*e2(3)
	e1(2)=e3(1)*e2(3)-e2(1)*e3(3)
	e1(3)=e2(1)*e3(2)-e3(1)*e2(2)

	s=0.0
	do i=1,3
		s=s+e1(i)*e1(i)
	enddo	
	s=sqrt(s)
	do i=1,3
		e1(i)=e1(i)/s
	enddo

c	write(6,*)' MakeNewSys'
c	write(6,*)' v=',v
c	write(6,*)' e1=',e1
c	write(6,*)' e2=',e2
c	write(6,*)' e3=',e3
c	s=0.0
c	do i=1,3
c		s=s+e1(i)*e1(i)
c	enddo
c	write(6,*)' abs(e1)=',s
c	s=0.0
c	do i=1,3
c		s=s+e2(i)*e2(i)
c	enddo
c	write(6,*)' abs(e2)=',s
c	s=0.0
c	do i=1,3
c		s=s+e3(i)*e3(i)
c	enddo
c	write(6,*)' abs(e3)=',s
c	s=0.0
c	do i=1,3
c		s=s+e1(i)*e2(i)
c	enddo
c	write(6,*)' e1*e2=',s
c	s=0.0
c	do i=1,3
c		s=s+e2(i)*e3(i)
c	enddo
c	write(6,*)' e2*e3=',s
c	s=0.0
c	do i=1,3
c		s=s+e3(i)*e1(i)
c	enddo
c	write(6,*)' e3*e1=',s
c	s=0.0
c	do i=1,3
c		s=s+v(i)*v(i)
c	enddo
c	write(6,*)' abs(v)=',s

	end	



	subroutine Ncirclesim(e1,e2,e3,v)
c
c	generate vector with circle simmetry in the system
c		around e3 axis
	implicit none

	real e1(3),e2(3),e3(3)	! coordinates of new orts in the old

	real v(3)		! vector (equal)

c	real ranfl

	real r(3)
c	real s
c	integer i

	call circlesim(r)
c	write(6,*)' Ncirclesim'
c	s=0.0
c	do i=1,3
c		s=s+r(i)*r(i)
c	enddo
c	write(6,*)' s=',s
	call GoOldSys(e1,e2,e3,r,v)
c	write(6,*)' Ncirclesim'
c	s=0.0
c	do i=1,3
c		s=s+e3(i)*v(i)
c	enddo
c	write(6,*)' s=',s
c	s=0.0
c	do i=1,3
c		s=s+v(i)*v(i)
c	enddo
c	write(6,*)' s=',s
c	write(6,*)' e3=',e3
c	write(6,*)' v=',v


	end



	subroutine circlesim(v)
c
c	generate vector with circle simmetry around e3
c		around z axis

	implicit none

	real v(3)		! vector (equal)

	real ranfl

	real F

        F=3.14159*2.0*ranfl()
        v(1)=cos(F)
        v(2)=sin(F)
        v(3)=0.0

	end


        subroutine sfersim(r)
c
c	generate vector with sferical simmetry
c
        implicit none
        real r(3)
        real costeta,sinteta,F
        real RANFL
c        real RANFL,COS,SIN,sqrt
C          SFERICAL SIMMETRY
                        costeta=1.0-2.0*RANFL()
                        sinteta=sqrt(1.0-costeta*costeta)
                        F=3.14159*2.0*RANFL()
                        r(1)=COS(F)*sinteta
                        r(2)=SIN(F)*sinteta
                        r(3)=costeta

        end



	subroutine turnvec(e1,e2,e3,teta, v)
c
c	turn the vector
c		assumed that old vector is along  e3 axis
c		the angle phi is rundom

	implicit none
c 	include 'cconst.inc'
	real*8 ELMAS		! Electron mass (MeV)
	parameter (ELMAS=0.51099906)
	real*8 FSCON		! Fine ctructure constant
	parameter (FSCON=137.0359895)
	real*8 ELRAD		! Electron radius (1/MeV)
	parameter (ELRAD=1.0/(FSCON*ELMAS))
	real*8 PI		
	parameter (PI=3.14159265358979323846)
	real*8 PI2
	parameter (PI2=PI*PI)
	real*8 AVOGADRO
	parameter (AVOGADRO=6.0221367e23)
	real*8 PLANK		! Plank constant (J*sec)
	parameter (PLANK=6.6260755e-34)
	real*8 ELCHARGE		! Electron charge (C)
	parameter (ELCHARGE=1.60217733e-19)
	real*8 CLIGHT		! Light vel.(sm/sec)
	parameter (CLIGHT=2.99792458e10)
c	real pionener
c	parameter (pionener=0.000026)


	real e1(3),e2(3),e3(3)	! coordinates of current orts in the old

	real v(3)		! vector (equal)
	real teta
	integer n,i
	real rad(3),rss
c	real sqrt

        if(Teta.lt.0.0)Teta=-Teta
        if(Teta.gt.4.0*PI)then
                n=Teta/(4.0*PI)
                Teta=Teta-n*4.0*PI
        endif
        if(Teta.gt.2.0*PI)then
                Teta=4.0*PI-Teta
        endif
        if(Teta.eq.PI)then
                do i=1,3
                        v(i)=-e3(i)
                enddo
        elseif(Teta.eq.0.0)then
                do i=1,3
                        v(i)=e3(i)
                enddo
	else
	        call Ncirclesim(e1,e2,e3,rad)
                rss=tan(Teta)
                if(rss.lt.0.0)then
                        rss=-rss
                        n=-1
                else
                        n=1
                endif
                do i=1,3
                        rad(i)=rad(i)*rss
                        v(i)=n*e3(i)+rad(i)
                enddo
		rss=0.0
		do i=1,3
			rss=rss+v(i)*v(i)
		enddo
		rss=sqrt(rss)
		do i=1,3
			v(i)=v(i)/rss
		enddo
        endif
c	write(6,*)' turnvec'
c	write(6,*)' teta=',teta
c	write(6,*)' e1=',e1
c	write(6,*)' e2=',e2
c	write(6,*)' e3=',e3
c	write(6,*)' v=',v
c	rss=0.0
c	do i=1,3
c		rss=rss+e3(i)*v(i)
c	enddo
c	rss=acos(rss)
c	write(6,*)' rss=',rss

	end
