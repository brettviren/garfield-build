CDECK  ID>, VOLNUMZC.
        subroutine VolNumZcoor(zcoor,veloc,num)

c       Find number of material for this coor.
c       zcoor - z coordinate
c       veloc - z velocity
c       num - number of volume
c               if(num.ne.0) particle go to next lay
c               correspodently with its velocity
c       if without of vol, returns 0
c       if num!=0 at call, go to next mat.

        implicit none

c         include 'volume.inc'
c	descriptions of the geometry of the setup
*** Decreased number of volumes (RV 22/11/06).
	integer pqvol		! Max. quantity of volumes
*	parameter (pqvol=150)	
	parameter (pqvol=3)	
	integer pQSVol		! Max. quantity of sensitive volumes
*	parameter (pQSVol=130)
	parameter (pQSVol=2)
	integer pQIVol		! Max. quantity of ionization volumes
*	parameter (pQIVol=130)
	parameter (pQIVol=2)
	integer QSVol
	integer QIVol
	integer qvol		! quantity of volumes
	integer upVol		! user's volume parameter
	integer nMatVol		! Material number for volume
	integer sSensit		! Sign of sensitivity
	integer sIonizat	! Sign of ionization
	real*8 wall1,wall2,wide	! Left, right side and wide of volume
	integer numSensVol,numVolSens	! pass from Volume number
					! to Sensitive volume number
	integer numIoniVol,numVolIoni	! The same for ionization
	real RLenRVol, RLenRAVol	! Radiation lengt for each volumes
					! and for whole detector.
	integer xxxVol		! dummy, for efficient alignment
	common / cvolum /
     +		qvol,
     +		QSVol,QIVol, xxxVol,
     +		upVol(pqvol), nMatVol(pqvol), sSensit(pqvol),
     +		sIonizat(pqvol),
     +		wall1(pqvol),wall2(pqvol),wide(pqvol),
     +		numSensVol(pqvol),numVolSens(pQSVol),
     +		numIoniVol(pqvol),numVolIoni(pQIVol),
     +		RLenRVol(pqvol),RLenRAVol
	save / cvolum /

        real veloc
        real*8 zcoor
        integer num
        integer i

        if(num.ne.0)then
                if(veloc.gt.0)then
                        if(num.lt.qvol)then
                                num=num+1
                                return
                        else
                                num=0
                                return
                        endif
                else
                        if(num.gt.1)then
                                num=num-1
                                return
                        else
                                num=0
                                return
                        endif
                endif
        endif

        num=0
        if(zcoor.lt.wall1(1))then
            return
	else
	    if(zcoor.eq.wall1(1))then
		if(veloc.gt.0)then
			num=1
		else
			num=0
		endif
		return
	    endif
        endif
        do i=1,qvol
                if(zcoor.lt.wall2(i))then
                        num=i
                        return
                elseif(zcoor.eq.wall2(i))then
                        if(veloc.gt.0)then
                                if(i.lt.qvol)then
                                        num=i
                                        return
                                else
                                        num=0
                                        return
                                endif
                        else
                                if(i.gt.1)then
                                        num=i-1
                                        return
                                else
                                        num=0
                                        return
                                endif
                        endif
                endif
        enddo
        return
        end
