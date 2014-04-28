CDECK  ID>, VOLPATHL.
        subroutine VolPathLeng(zcoor,veloc, num, mleng)

c       Find path leng in the current mat
c       zcoor - z coordinate
c       num - number of volume
c       veloc - velocity(cosine)

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

        real veloc(3)
        real*8 zcoor,mleng
	real*8 z
        integer num

c	write(oo,*)' zcoor=',zcoor
c	write(oo,*)' veloc=',veloc
c	write(oo,*)' num=',num
	z=zcoor
        if(veloc(3).eq.0.0)then
                mleng=1.e30
        else if(veloc(3).gt.0.0)then
                mleng=(wall2(num)-z)/veloc(3)
        else
                mleng=(wall1(num)-z)/veloc(3)
        endif

        end
