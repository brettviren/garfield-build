CDECK  ID>, HIST.

	subroutine IniHist

c	initialize common histograms
c	

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


	integer nsv
	integer imaxhisample
	imaxhisample=maxhisample

	if(QSVol.le.MaxHistQSVol)then
		hQSVol=QSVol
	else
		hQSVol=MaxHistQSVol
	endif


	do nsv=1,hQSVol     ! circle over the sensitive volumes



	       CALL HBOOK1(
     +		nh1_ampK + nsv,
     +		'  amplitude (KeV)$',
     +		pqhisampl, 0.0, maxhisampl*1.0e3, 0.0)
				! it is defined in MeV
	       CALL HBOOK1(
     +		nh1_ampKR + nsv,
     +		'  amplitude (KeV)$',
     +		pqhisampl, 0.0, maxhisampl*1.0e3, 0.0)
				! it is defined in MeV

	       CALL HBOOK1(
     +		nh1_ampN+nsv,
     +	    	'  amplitude in numbers of conduction electrons$',
     +	    	imaxhisample, 0.0, maxhisample, 0.0)



	       CALL HBOOK1(
     +		nh1_cdx + nsv,
     +		' charge distribution along x$',
     +		pqh2,-0.02,0.02,0.0)

 	       CALL HBOOK1(
     +		nh1_cdy + nsv,
     +		' charge distribution along y$',
     +		pqh2,-0.02,0.02,0.0)

	       CALL HBOOK1(
     +		nh1_cdz + nsv,
     +		' charge distribution along z$',
     +		pqh2,
     +		real(wall1(numVolSens(nsv))),
     +		real(wall2(numVolSens(nsv))),0.0)




	enddo


	

       CALL HBOOK2(
     +	nh2_ard,
     +	' Actual range of delta-electron(cm) vs energy(MeV).$',
     +		pqh,0.0,1.0,
     +		pqh,0.0,0.002,0.0)
       CALL HBOOK2(
     +	nh2_rd,
     +	'Range along initial direction of delta-electron vs energy.$',
     +		pqh,0.0,0.01,
     +		pqh,0.0,0.002,0.0)
       CALL HBOOK1(
     +	nh1_rd,
     +	' Range along initial direction of delta-electron (cm). $',
     +		pqh,0.0,0.01,0.0)





	end


	subroutine FHist

c	fill histograms
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
c 	include 'cel.inc'
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
c 	include 'del.inc'
c		Delta electrons

	integer pqdel		! Max. q. of electrons
	parameter (pqdel=120000)	
	integer qdel 	! Q. of electrons
C	integer cdel	! Current electron (not used, RV 27/2/97)
			! number of el. which must be treated next
	real veldel	! direction of the velocity
	real*8 pntdel	! point
	
	real zdel, edel		! charge of current electrons
			! which must be produced and energy of Delta
	integer Stdel   ! Generation number
	integer Ptdel	! pointer to parent virtual photon
	integer updel	! additional parameters
	integer SOdel           ! 1 for ouger electrons 0 for other
	integer nVoldel		! Number of volume
	real*8 rangedel		! range
	real*8 rangepdel		! practical range
	integer qstepdel	! quantity of steps of simulation
				! of stopping
        integer sOverflowDel    ! sign of overflow in the current event
        integer qsOverflowDel   ! quantity of the overflows in all events
        integer qOverflowDel    ! quantity of the lossed electrons
                                ! in all events
	integer ii1del		! not used. only for alingment.
	common / comdel /
     +	qdel, ii1del,
     +	pntdel(3,pqdel), veldel(3,pqdel),
     +	rangedel(pqdel),rangepdel(pqdel), qstepdel(pqdel),
     +	zdel(pqdel), edel(pqdel), nVoldel(pqdel),
     +	Stdel(pqdel), Ptdel(pqdel), updel(pqup,pqdel), SOdel(pqdel),
     +  sOverflowDel, qsOverflowDel,qOverflowDel
        save / comdel /

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
c         include 'ener.inc'
c	Energy mesh

      integer pqener,qener	! Max. quantity and quantity of bins.
				! Quantity must not be more than pqener - 1.
      PARAMETER (pqener=501)
      real ener,enerc		! The left edges and the centers
				! of the energy intervals.
				! ener(qener+1) is the right edge
				! of the last interval.
C
      COMMON / coEner /
     +       qener, ener(pqener), enerc(pqener)
	save / coEner /
c         include 'atoms.inc'


	integer pQAt		! Max. quantity of atoms.
	parameter (pQAt=19)
	integer KeyTeor		! Key to use only theor. photo-absorbtion
				! cross section with thresholds and
				! weights from the subroutine shteor.
				! If 0 then they are used only for
				! the atoms which are absent
				! in the subroutine readPas and
				! in the subroutine shellfi.
	integer Zat		! Atomic number (charge of atomic nucleus).
	real Aat		! Atomic weight.
	integer pQShellAt	! Max. quantity of atomic shells.
	parameter (pQShellAt=17)
	integer QShellAt	! Quantity of atomic shells.
	real cphoAt		! Integral of photo-absorbtion
				! cross secton for one atom.
	real	ThresholdAt	! Threshold and
	real 	WeightShAt 	! Weight of atomic shells for the
				! photo-absorbtion cross secton
				! relatively cphoAt.
	real PWeightShAt	! Initial integral of
				! photo-absorbtion cross secton.
	real  PhotAt		! Photo-absorbtion cross secton.
	real  PhotIonAt		! Photo-ionization cross secton.
c	The physical definition of two previous arrays of values:
c	mean values of cross sections for each energy interval.
	real  RLenAt		! Radiation lengt*density for dens=1
	real  RuthAt		! Const for Rutherford cross cection
				! (dimensionless).
c	integer num_at_mol	! Number for atoms in several special
c				! molecules, now obsolete.
	real ISPhotBAt		! Shell integral of cs before normalization
	real IAPhotBAt 		! Atomic integral of cs before normalization
	real ISPhotAt		! Shell integral of cs
	real IAPhotAt		! Atomic integral of cs
	real ISPhotIonAt	! Shell integral of cs
	real IAPhotIonAt	! Atomic integral of cs
	real MinThresholdAt	! Minimal ionization potential of atom.
	integer NshMinThresholdAt ! Number of shell with minimal energy,
			! it must be the last shell ( see AbsGam.f)
	integer Min_ind_E_At, Max_ind_E_At ! Indexes of energy intervals
			! where program adds excitation to cs
			! They placed in common only to print and check.
	integer nseqAt	! Sequensed pointer in order of increasing Zat
			! atom number nsAt(1) is least charged.
	integer QseqAt	! Quantity of initialized atoms

	common / catoms /
     +		KeyTeor,
     +		Zat(pQAt), Aat(pQAt),	
     +		QShellAt(pQAt), cphoAt(pQAt),
     +		ThresholdAt(pQShellAt,pQAt), WeightShAt(pQShellAt,pQAt),
     +		PWeightShAt(pQShellAt,pQAt),
     +		PhotAt(pqener,pQShellAt,pQAt),
     +		PhotIonAt(pqener,pQShellAt,pQAt),
     +		ISPhotBAt(pQShellAt,pQAt),
     +		IAPhotBAt(pQAt),
     +		ISPhotAt(pQShellAt,pQAt),
     +		IAPhotAt(pQAt),
     +		ISPhotIonAt(pQShellAt,pQAt),
     +		IAPhotIonAt(pQAt),
     +		MinThresholdAt(pQAt),
     +		NshMinThresholdAt(pQAt),
     +		Min_ind_E_At(pQAt), Max_ind_E_At(pQAt),
     +		RLenAt(pQAt),
     +		RuthAt(pQAt),
     +		nseqAt(pQAt),
     +		QseqAt
	save / catoms /
c         include 'matters.inc'
	integer pQMat		! Max. quantity of matters.
	parameter (pQMat=10)
	integer QAtMat		! Quantity of atoms in matter.
	integer AtMAt		! Number of atom in matter
				! (the pointer to atoms.inc).
	real WeightAtMat	! Weight of atom in matter.
	real A_Mean		! Average A.
	real Z_Mean		! Average Z.
	real DensMat		! Density (g/cm3).
	real DensMatDL		! Density (g/cm3) for energy loss of deltaelect.
	real DensMatDS		! Density (g/cm3) for mult. scat. of deltaelect.
	real ElDensMat		! Electron density(MeV3).
	real XElDensMat		! Longitud. Electron Dens. for x=1cm(MeV2/cm)
	real wplaMat		! Plasm frequancy.
	real RLenMat		! Radiation Lengt.
	real RuthMat		! Const for Rutherford cross section (1/cm3).
	real PhotMat		! Photoabsirbtion cross section per one atom.
	real PhotIonMat		! Photoionization cross section per one atom.
	real epsip		! plasm dielectric constant.
	real epsi1		! real part of dielectric constant.
	real epsi2		! imaginary part of dielectric constant.
	real min_ioniz_pot	! Minimum ionization potential,
				! it is using only for switching off
				! the Cherenkov radiation below it.
	real Atm_Pressure	! Standart atmosferic pressure.
	parameter (Atm_Pressure=760.0)
	real Cur_Pressure	! Current pressure for initialized medium.
				! During gas initialization
				! the subroutine gasdens uses it for
				! calculating of density.
	real Pressure		! Pressure for given medium.	
	real Atm_Temper		! Standart atmosferic temperature.
	parameter (Atm_Temper=293.0)
	real Cur_Temper		! Current temperature for initialized medium.
				! During gas initialization
				! the subroutine gasdens uses it for
				! calculating of density.
	real Temper		! Temperature for given medium.	
	real WWW		! The mean work per pair production.
	real FFF		! Fano parameter.
	common / cmatte /
     +		QAtMat(pQMat),
     +		AtMat(pQAt,pQMat),
     +		WeightAtMat(pQAt,pQMat),
     +		A_Mean(pQMat),Z_Mean(pQMat),
     +		DensMat(pQMat),ElDensMat(pQMat),XElDensMat(pQMat),
     +		DensMatDL(pQMat),DensMatDS(pQMat),
     +		wplaMat(pQMat),
     +		RLenMat(pQMat),
     +		RuthMat(pQMat),
     +		PhotMat(pqener,pQMat),
     +		PhotIonMat(pqener,pQMat),
     +		epsip(pqener,pQMat),
     +		epsi1(pqener,pQMat),
     +		epsi2(pqener,pQMat),
     +		min_ioniz_pot(pQMat),
     +		Cur_Pressure,Pressure(pQMat),
     +		Cur_Temper,Temper(pQMat),
     +		WWW(pQMat),FFF(pQMat)
	save / cmatte /
c 	include 'track.inc'
c	The track information about the primary particle

	integer sign_ang	! sign to run the part. with effective angle
	real ang		! teta
	real phiang		! phi
	real ystart		! start Y coordinate
	integer srandtrack	! sign to randomize the Y coordinate
				! between ystart1 and  ystart2
				! It is done by call IniNTrack from GoEvent
				! if the track initialization was done by
				! call IniRTrack
	real ystart1
	real ystart2
	real sigmaang		! sigma of begin angle distribution
			!Currently, if sigmaang>0, the rundomization
			! is doing around the 0 angle.
			! So the values of pang and pphiang are ignored
			! It can be changed by modernization
			! of IniNTrack
	real e1ang,e2ang,e3ang	! coordinates of new orts in the old
	integer sigmtk		! sign of multiple scatering
	integer pQmtk		! max. quantity of the break point of the track
				! plus one
	parameter (pQmtk=10000)
	integer Qmtk		! actual quantity for current event
	real*8 pntmtk		! break point coordinates
	real velmtk		! directions of velocity
	real*8 lenmtk		! lengt of way for straight till next break
	real Tetamtk		! turn angle
	integer nVolmtk		! number of volume for given point,
				! the point on the frantier is correspond
				! to next volume of zero for end.
	real*8 vlenmtk		! lengt of way inside the volume
	integer nmtkvol1,nmtkvol2 ! numbers of first point in volume
			! and the previous for end point
	real*8 xdvmtk,ydvmtk	! deviations from strate line
			! using only for histograms

	! service data. They are using at initialization of the track.
	integer sruthmtk	! key to use Rutherford cross section
	integer nmtk		! current number of point.
			! After initialization it must be equal to Qmtk+1
	integer sgnmtk		! sign to go to next volume
	integer sturnmtk	! sign to turn
	real*8 lammtk		! mean free path
	real mlammtk		! minimum mean lengt of range
			! multiplied by density. sm*gr/sm**3 = gr/sm**2
	real mTetacmtk		! minimum threshold turn angle
	real Tetacmtk		! threshold turn angle
	real rTetacmtk		! restiction due to atomic shell
	real*8 CosTetac12mtk	! cos(tetac/2)
	real*8 SinTetac12mtk	! sin(tetac/2)
c	real CosTetac12mtk	! cos(tetac/2)
c	real SinTetac12mtk	! sin(tetac/2)
	real msigmtk		! msig without sqrt(x)
	real e1mtk,e2mtk,e3mtk
	common / ctrack /
     +	sign_ang, ang, phiang, ystart, srandtrack, ystart1, ystart2,
     +	e1ang(3),e2ang(3),e3ang(3),
     +	sigmtk,
     +	sruthmtk,
     +	Qmtk, nmtk,
     +	pntmtk(3,pQmtk), velmtk(3,pQmtk), lenmtk(pQmtk), Tetamtk(pQmtk),
     + 	nVolmtk(pQmtk), vlenmtk(pQVol),
     +	nmtkvol1(pQVol), nmtkvol2(pQVol),
     +	xdvmtk(pQSVol),ydvmtk(pQSVol),
     +	sgnmtk, sturnmtk,
     +	lammtk(pQMat), mlammtk, mTetacmtk,
     +	Tetacmtk(pQMat),
     +	rTetacmtk(pQMat),
     +	CosTetac12mtk(pQMat), SinTetac12mtk(pQMat), msigmtk,
     +	e1mtk(3,pQmtk),e2mtk(3,pQmtk),e3mtk(3,pQmtk),
     +	sigmaang
	save / ctrack /
			
	integer nsv,ncel,nv,nm

	real ranfl
	real r


	do nsv=1,hqSVol
	

                nv=numVolSens(nsv)
                nm=nMatVol(nv)

		call hf1(nh1_ampK + nsv,szcel(nsv)*WWW(nm)*1.0e3,1.0)

		r=ranfl()-0.5
		r=(szcel(nsv)+r)*WWW(nm)*1.0e3
		if(r.lt.0)r=0
		call hf1(nh1_ampKR + nsv, r, 1.0)

		call hf1(nh1_ampN + nsv,szcel(nsv),1.0)



	    do ncel=1,qcel(nsv)	! circle on conduction electrons

		call hf1(
     +		nh1_cdx + nsv,
     +		real(pntcel(1,ncel,nsv)), zcel(ncel,nsv))

		call hf1(
     +		nh1_cdy + nsv,
     +		real(pntcel(2,ncel,nsv)), zcel(ncel,nsv))

		call hf1(
     +		nh1_cdz + nsv,
     +		real(pntcel(3,ncel,nsv)), zcel(ncel,nsv))

	    enddo



	enddo



	end

        SUBROUTINE WHist
C
C-----------------------------------------------------------------
C|                                                               |
C|        TERMINATION ROUTINE TO PRINT HISTOGRAMS                |
C|                                                               |
C|                                                               |
C|                                                               |
C----------------------------------------------------------------|
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


c        Integer*4 i,j,k,l,m,n
        Integer*4 istat,icycle
C
          call hropen(HistLun,'mybook',HistFile,'nq',1024,istat) ! rz file
          if (istat.ne.0) go to 999        ! if error
          call hcdir('//PAWC',' ')         ! root directory in memory
          call hcdir('//mybook',' ')       ! root directory on disk
         CALL HROUT(0,icycle,' ')          ! write all on disk
C
         CALL HREND('mybook')
C
        goto 1000
 999    continue
        write (oo,100)istat
 100    format(1x,//,1x,'*** UGLAST: error of writing, ISTAT= ',i6)
 1000   continue
        CLOSE(HistLun)
        RETURN
        END
