CDECK  ID>, GOEVENT.


	subroutine GoEvent
c
c	Event processor. It is called from MainHEED.
c
	implicit none

c 	include 'GoEvent.inc'
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

c 	include 'abs.inc'

c		Gamma which is ready to absorb
c		There are two sorts of gamma
c		Real gamma after their absorbtion points are known and
c		virtual gamma from ionization loss
	integer pqtagam		 ! Max quantity of absorbtion gamma
	parameter (pqtagam=100000)
	integer qtagam, ctagam	 ! Full quantity and current number
				 ! of gamma which will be treat next.
				 ! If ctagam>qtagam then
				 ! there is no gamma to treat.
	real etagam,  vtagam ! Energy,  and velocity
				 ! direction of absorbtion gamma
	real*8 rtagam 		 ! position of absorbtion gamma
	integer nVolagam	 ! Volume number for this point
	integer nAtagam,nshlagam ! Number of atom and shell
				 ! which  absorbe this photon
	integer Stagam		 ! Generation number
	integer upagam		 ! additional parameters
        integer sOverflowagam    ! sign of overflow in the current event
        integer qsOverflowagam   ! quantity of the overflows in all events
        integer qOverflowagam    ! quantity of the lossed electrons
                                ! in all events

	common / comabs /
     +	qtagam, ctagam, etagam(pqtagam),
     +	rtagam(3,pqtagam), vtagam(3,pqtagam),
     +	nVolagam(pqtagam),nAtagam(pqtagam),nShlagam(pqtagam),
     +	Stagam(pqtagam), upagam(pqup,pqtagam),
     +  sOverflowagam, qsOverflowagam,qOverflowagam
	save / comabs /
c 	include 'rga.inc'
c		Real photons
*** Check INDPOS when changing pqrga.
	integer pqrga	
	parameter (pqrga=1000)
	integer qrga, crga
	real velrga, erga
*** Added origin of real gamma (RV 27/11/01)
	real*8 pntrga,  ! Current point of real gamma
     +         orirga   ! Location of gamma production
	integer Strga	! generation
	integer Ptrga 	! pointer to parent
	integer uprga	! number of trans vol
	integer SFrga 	! sign of fly out
	integer nVolrga
        integer sOverflowrga    ! sign of overflow in the current event
        integer qsOverflowrga   ! quantity of the overflows in all events
        integer qOverflowrga    ! quantity of the lossed photons
                                ! in all events

	common / comrga /
     +	qrga, crga,
     +	pntrga(3,pqrga), orirga(3,pqrga), velrga(3,pqrga), erga(pqrga),
     +	nVolrga(pqrga), Strga(pqrga), Ptrga(pqrga), uprga(pqup,pqrga),
     +	SFrga(pqrga),
     +  sOverflowrga, qsOverflowrga,qOverflowrga
	save / comrga /
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
c 	include 'hist.inc'


	integer sHist		   ! Sign to fill histograms
        character*100 HistFile     ! File name for file
                                   ! with histograms.
	integer HistLun		   ! Logical number of stream to write
				   ! this file.
	parameter (HistLun=34)

        real maxhisampl            ! maximum amplitude for histograms
        real maxhisample           ! maximum amplitude for histograms
				   ! in units of electrons
        real maxhisampl2           ! reduced maximum amplitude for histograms
        integer pqhisampl          ! quantity for histograms with amplitude.
	integer pqh
	parameter (pqh=100)	! usual number of divisions
        integer pqh2
        parameter (pqh2=200)     ! increased number of divisions

        integer shfillrang         ! sign to fill special histogram nh2_rd
                                   ! with practical range of delta electron
                                   ! It spends some computer time.
        integer MaxHistQSVol
        parameter (MaxHistQSVol=50) ! Maximum number of volumes,
				! used at initilisation of histograms.
				! If the number of the sensitive volumes
				! is more,
				! only MaxHistQSVol histograms will be created
				! and they will represent
				! the first MaxHistQSVol volumes
	integer hQSVol		! working number -- minimum of
				! MaxHistQSVol end QSVol
				! Defined in Inihist

c	Determination of histogram numbers:

c	Notation nh1 is number of 1-dimension histogram	
c	Notation nh2 is number of 2-dimension histogram	


	integer nh1_ampK
	parameter (nh1_ampK=100)	! amplitude (KeV)
				! Some fluctuations may be here if
				! each single  bin of this histogram corresponds
				! to differrent numbers of bins of
				! nh1_ampN histogram.
	integer nh1_ampKR	
	parameter (nh1_ampKR=150)	! amplitude (KeV)
				! Special treatment is applyed to smooth
				! the fluctuations mentioned above.
				! It increases the mean square dispersion
				! on a little value sqrt(1/12)* w .
	integer nh1_ampN	
	parameter (nh1_ampN=200)! amplitude in numbers of conduction electrons.

	integer nh1_cdx		! charge distribution along x
	parameter (nh1_cdx=300)
	integer nh1_cdy		! charge distribution along y
	parameter (nh1_cdy=500)
	integer nh1_cdz		! charge distribution along z
	parameter (nh1_cdz=700)

	integer nh2_ard		! Actual range of delta-electron(cm)
	parameter (nh2_ard=900)	! vs energy(MeV).
	integer nh2_rd		! Range along initial direction of
	parameter (nh2_rd=901)	! delta-electron vs energy.
	integer nh1_rd		! Range along initial direction of
	parameter (nh1_rd=902)	! delta-electron (cm).

	common / chist /
     +	sHist,
     +	maxhisampl,
     +	maxhisample,
     +	maxhisampl2,
     +	pqhisampl,
     +	shfillrang,
     +	hQSVol
	save / chist /
	common / chhist /
     +	HistFile
	save / chhist /

c 	include 'random.inc'
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	

	integer iempty


c	if(nevt.le.ninfo)then
	if(soo.eq.1)then
	write(oo,*)
	write(oo,*)' Event number ',nevt
	endif
	if(nevt.eq.1.and.sseed.eq.1)then
		call randset		! Set the start point of
	endif				! the random number generator.
	if(soo.eq.1)then
		call randget
		call randpri(oo)	! Print the current point of
	endif				! the random number generator.
c	endif

	call IniNTrack			! Generate the next track.
	if(nevt.le.ninfo)then
	        call PriMTrack(0)	! Print debug information
	        call PriMTrack(1)
	        call PriMTrack(2)
	        call PriMTrack(3)
	        call PriMTrack(4)
	endif		

	call IniLsgvga			! Initialize gvga.inc
	call Iniabs			! Initialize abs.inc
	call Inirga			! Initialize rga.inc
	call Inidel			! Initialize del.inc
	call Inicel			! Initialize cel.inc

	call UBegEvent			! User's subroutine

	if(ssimioni.eq.1)call rafflev	! Generate the primary energy transfers
					! from incoming particle

	if(soo.eq.1)then
	if(nevt.le.ninfo)then
		write(oo,*)
		call PriLsgvga		! Print debug information
	endif
	endif

	do iempty=1,10000

		if(soo.eq.1)then
		if(nevt.le.ninfo)then
			write(oo,*)
			write(oo,*)' before absorption of virtual photons:'
			call Priabs	! Print debug information

		endif
		endif

		call AbsGam		! Absorb the virtual photons

		if(soo.eq.1)then
		if(nevt.le.ninfo)then	! Print debug information
			write(oo,*)		
			write(oo,*)' after absorption of virtual photons:'
	
c			call Priabs
			call Prirga
			call Pridel

		endif
		endif
	
		call GoGam		! Absorb the photons

		if(soo.eq.1)then
		if(nevt.le.ninfo)then	! Print debug information
			write(oo,*)
			write(oo,*)' after absorption of photons:'

			call Priabs
c			call Prirga
			call PrirgaF

		endif
		endif

		if(ctagam.gt.qtagam.and.crga.gt.qrga)then
					! There are neither real no
					! virtual photons to trace.
			goto 50		! Exit the loop.
		endif
	
	enddo

50	continue


	call treatdel			! Trace the delta-electrons
					! and generate the conduction electrons.
	call treatcel			! Treat the cel.inc
	if(soo.eq.1)then
	if(nevt.le.ninfo)then	! since there are calculation of ranges
				! which in wroute to del inside treatdel
		write(oo,*)
		call Pridel
c		call Pricel
	endif
	endif

	if(sHist.eq.1)then
		call Fhist	! Fill predetermined histograms
	endif

	call UEndEvent		! User's routine

	if(soo.eq.1)then
	if(nevt.eq.qevt)then
		write(oo,*)
		write(oo,*)nevt,' events is done'
				! Printing the wornings about overful
		call WorPrirga
		call WorPriabs
		call WorPridel
		call WorPricel

	endif
	endif


	end
