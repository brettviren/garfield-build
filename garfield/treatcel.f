CDECK  ID>, TREATCEL.
        subroutine treatcel
c
c       Calculate the total charge
c
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
c         include 'cel.inc'
c		Conductin electrons in sensitive volumes
c		Currently each the electron is considered as cluster

	integer pqcel		! Max. q of clusters
	parameter (pqcel=50000)
c	parameter (pqcel=1000000)	! If this, reduce numbers of volumes
c	parameter (pqcel=100000)	! If this, reduce numbers of volumes
	integer qcel 		! Q. of clusters
	real*8 pntcel	 	! point of cluster
	real zcel		! charge in unit of quantity of electron
				! in this cluster (now it is always 1)
	real szcel	! sum quantity of charge in the volume
	integer Ndelcel	! number of parent delta electron
	integer sOverflowCel	! sign of overflow in the current event
	integer qsOverflowCel	! quantity of the overflows in all events
	integer qOverflowCel	! quantity of the lossed electrons
				! in all events
	integer sactcel		! auxiliary sing.
			! It set to one if the delta-electron either
			! was born in an insensitive lawer or
			! after it had flied through an insensitive lawer.
	common / comcel /
     +	pntcel(3,pqcel,pQSVol),
     +	qcel(pQSVol),
     +	zcel(pqcel,pQSVol),
     +	szcel(pQSVol),
     +	Ndelcel(pqcel,pQSVol),
     +	sactcel(pqcel,pQSVol),
     +	sOverflowCel(pQSVol), qsOverflowCel(pQSVol),qOverflowCel(pQSVol)
	save / comcel /

        integer i,j
        real s
c	real r,cr

        do i=1,QSVol

                s=0
                do j=1,qcel(i)
                        s=s+zcel(j,i)
                enddo
                szcel(i)=s
        enddo

        end
