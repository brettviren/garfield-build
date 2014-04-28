CDECK  ID>, PRILSGVG.
	subroutine PriLsgvga

c	print the virt. ioniz.  photons

	implicit none

c         include 'GoEvent.inc'
c      Main control variables


	integer soo		   ! Flag, allowing to print
				   !  to stream 'oo'
				   ! If it is 0, no print will be at all,
				   ! except the case of serious problems.
	integer oo		   ! The output stream logical number.
	integer qevt		   ! Quantity of events to produce.
	integer nevt		   ! Current number of the event.
	integer ninfo		   ! Quantity of the first events
				   ! to print debug info.
	integer ssimioni	   ! Flag to simulate ionization loss,
				   ! 0 - no ionization,
				   ! 1 - to simulate ionization.
				   !
				   !
				   !
	integer srandoff	   ! Flag to swich off the randomization
				   ! in function treatdel.
				   ! It is for debug and without guarantee.
	parameter (srandoff=0)	   ! Normal regim with randommization.

	integer pqup		   ! dimensions of arrays of auxiliary
				   ! parameters in abs.inc, rga.inc,
				   ! del.inc
	parameter (pqup=1)


	integer sret_err	! Sign to return the control from current
		! subroutine to which is called it if error is occured.
		! 1 - to return, 0 - to stop.
		! It is intended for handling with subroutine SHEED.
		! In the case of error it can return the control instead of
		! stop. But not for every possible errors return is done.
		! Some of the most original errors could lead to stop.
		! When working with HEED program, sret_err must be zero.
	integer s_err	! Sign of error.
		! 1 - error, 0 - no error

	character*9  TaskName	   ! Name of task, using for generating
				   ! file names.
	character*40 OutputFile	   ! Name of file with output listing.
				   ! Using only in IniHeed.
*** Common split in character and non-character parts (RV 26/10/2007).
	common / cGoEve /
     +	soo, oo,
     +	qevt,nevt,ninfo,
     +	ssimioni,
     +	sret_err, s_err
        common / cGoEveCHR /
     +	TaskName,
     +	OutputFile

	save / cGoEve /
	save / cGoEveCHR /
*** End of change

c 	include 'volume.inc'
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
c 	include 'lsgvga.inc'
c		Results of ionization loss calculations
c		It is used only for hist filling

	integer pqgvga
*** Increased buffer (RV 22/11/06) - check INDPOS dimensions when changing !
	parameter (pqgvga=10000)
*	parameter (pqgvga=1000)
	integer qgvga,ganumat,ganumshl
*** Added overflow tracing (RV 22/11/06)
        integer sOverflowvga    ! sign of overflow in the current event
        integer qsOverflowvga   ! quantity of the overflows in all events
        integer qOverflowvga    ! quantity of the lossed photons
                                ! in all events
*** End of addition.
	real esgvga,egvga,velgvga
	real*8 pntgvga
	common / clsgva /
     +	qgvga(pQIVol),
     +	esgvga(pQIVol),
     +	egvga(pqgvga,pQIVol),
     +	pntgvga(3,pqgvga,pQIVol),
     +	velgvga(3,pqgvga,pQIVol),
     +	ganumat(pqgvga,pQIVol),
     +	ganumshl(pqgvga,pQIVol),
     +  sOverflowvga, qsOverflowvga,qOverflowvga
	save / clsgva /

	integer k,i,j

        if(soo.eq.0)return

	write(oo,*)
	write(oo,*)' PriLsgvga: virtual ionization  photons'
	do k=1,QSVol
	write(oo,*)' number of lay =',k
	write(oo,*)' qgvga()= ',qgvga(k),' esgvga()=',esgvga(k)
	if(qgvga(k).gt.0)then
	write(oo,*)'  egvga(i,k)  ganumat(i,k)  ganumshl(i.k)'
	write(oo,*)
     +	' pntgvga(1,i,k)  pntgvga(2,i,k)  pntgvga(3,i,k) ',
     +	' velgvga(1,i,k)  velgvga(2,i,k)  velgvga(3,i,k)  '
	do i=1,qgvga(k)
		write(oo,'(1X,e12.5,2(i12))')
     +		egvga(i,k),ganumat(i,k),ganumshl(i,k)
		write(oo,'(6(1X,e12.5))')(pntgvga(j,i,k),j=1,3),
     +		(velgvga(j,i,k),j=1,3)
	enddo
	endif
	enddo
*** Added overflow tracing (RV 22/11/06).
        if(nevt.eq.qevt)then

        if(qOverflowvga.gt.0)then
        write(oo,*)
        write(oo,*)' WorPriLsgvga: overflow of virtual photons  arrays'
        write(oo,*)' sOverflowvga   qsOverflowvga   qOverflowvga'
        write(oo,*)sOverflowvga,qsOverflowvga,qOverflowvga
        endif

        endif
*** End of addition.
	end
