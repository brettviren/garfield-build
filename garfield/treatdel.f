CDECK  ID>, TREATDEL.


	subroutine treatdel
c
c	make absorbtion af delta electrons
c	 write it to the cel.inc

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
c         include 'crosec.inc'
	integer pQShellC	! Max quantity of shells for all atoms
				! in one material
	parameter (pQShellC=20)
c	integer MatC		! Matter number
	integer sMatC		! Sign to calculate sross section
				! for this matter
	integer QShellC		! Quantity of shells for all atoms
				! in this matter
c	real ksi		! Help Landau constant
c				! (it seems it is't used)
	real log1C		! first log
	real log2C		! second log
	real chereC		
	real chereCangle
	real addaC		! energy tranfer cross section
	real quanC		! it's integral,
				! or quantity of energy transfers,
				! or primary cluster number.
	real meanC		! first moment,
				! or restricted mean energy loss, Mev.
	real meanC1		! first moment with whole additional tail
				! to emax - kinematically allowed transition.
				! Now it is calculated only for heavy particles
				! because the integral for electrons is not
				! trivial,
				! or mean energy loss, Mev.
	real meaneleC		! expected restricted quantity of
				! secondary ionization.
	real meaneleC1		! expected quantity of secondary ionization.
	integer NAtMC		! number of atom in the matter
				! for shell with corr. index
	integer NAtAC		! number of atom
	integer NSheC		! number of shell

	real flog1
	real flog2
	real cher
	real rezer
	real frezer
	real adda
	real fadda
	real quan
	real mean

        complex*16 pocaz	! it is help
				! coefficient at y
				! the value of imajinary part
				! corresponsd to with of wave front
	
	common / ccrosec /
     +	pocaz(pqener,pQMat),
     +	sMatC(pQMat),
     +	QShellC(pQMat),
c    +	ksi(pQMat),
     +	log1C(pqener,pQMat),
     +	log2C(pqener,pQMat),
     +	chereC(pqener,pQMat),
     +	chereCangle(pqener,pQMat),
     +	addaC(pqener,pQMat),
     +	quanC(pQMat),
     +	meanC(pQMat),
     +	meanC1(pQMat),
     +	meaneleC(pQMat),
     +	meaneleC1(pQMat),
c
     +	NAtMC(pQShellC,pQMat),
     +	NAtAC(pQShellC,pQMat),
     +	NSheC(pQShellC,pQMat),
c
     +	flog1(pqener,pQShellC,pQMat),
     +	flog2(pqener,pQShellC,pQMat),
     +	cher(pqener,pQShellC,pQMat),
     +	rezer(pqener,pQShellC,pQMat),
     +	frezer(pqener,pQShellC,pQMat),
     +	adda(pqener,pQShellC,pQMat),
     +	fadda(pqener,pQShellC,pQMat),
     +	quan(pQShellC,pQMat),
     +	mean(pQShellC,pQMat)
	save / ccrosec /
	
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
c 	include 'bdel.inc'
c		Information about tracing of current delta-electron
c

	real eMinBdel			! some condition step by energy
			! (the name is obsolete)
			! If step is larger than eMinBdel and 0.1*eBdel
			! the step is equate to 0.1*eBdel
			! In this case step can not be less than eMinBdel
			! and larger than eBdel
	integer iMinBdel		! not using now
	real eLossBdel			! array with energy loss for
					! all the matters
	real betaBdel
	real beta2Bdel
	real momentumBdel
	real momentum2Bdel
	real*8 lamaBdel
	real msigBdel
	integer nBdel			! number of the delta-electron
					! in the del.inc, which is
					! traced now
	real eBdel			! the current energy
	real*8 pntBdel,npntBdel		! current point and next point
					! Next is calc. in
					! subroutine SstepBdel
					! and moved to current in
					! subroutine treatdel
	real*8 stepBdel			! step - sm
	real estepBdel			!            and MeV
	real velBdel			! direction of the velocity
	real e1Bdel, e2Bdel, e3Bdel	! coordinate axises,
				! e3Bdel is along to velocity
				! e2Bdel is perpend. to e3Bdel and x
				! e1Bdel is perpend to e2Bdel and e3Bdel
	integer nVolBdel,sgonextBdel	! number of current volume
					! and sign to go to next volume
	integer sturnBdel		! sign of turn
	real TetacBdel,TetaBdel		! threshold turn angle and
					! actual angle
	real CosTetac12Bdel,SinTetac12Bdel
	real rTetacBdel			! restiction due to atomic shell
	real*8 lamBdel			! mean lengt of range
	real mlamBdel			! minimum mean lengt of range
			! multiplied by density. sm*gr/sm**3 = gr/sm**2
	real mTetacBdel			! minimum threshold turn angle
			! 	For Rutherford:
			! The interactions with less angle will not take
			! into account. The actual threshold angle can be
			! larger. The second restriction is going
			! from restriction of atomic shell.
			! The third one is from mlamBdel.
			! 	For usial multiple scatering:
			! Assuming that sigma = mTetacBdel
			! the paht lengt is calculating.
			! If mlamBdel/density is less then the last is using.
	integer iBdel			! index of current energy
					! in the enerc array
	integer StBdel		        ! Origin and generation sign
                        ! <10000 - origin is ionization loss
                        ! >=10000 - origin is transition radiation
                        ! 1 or 10000 first generation
                        ! 2 or 10001 second generation
                        ! 3 or 10002 third,  et al.
	integer NtvBdel		       ! Only for transition gammas:
                        ! number of transition volume, where it was born
	integer SOBdel	 	! 1 for ouger electrons 0 for other

	real*8 rangBdel		! whole delta-electron range
	real*8 rangpBdel		! mean projection of delta-electron range
				! The maximum projection lengt of
				! current electron point on the
				! primary velocity.
	integer sruthBdel	! sign of use
				! 1 - Rutherford cross-section
				! 0 - usial multiple scatering formula
	integer sisferBdel	! sign that the mean or the cut turn angle
			! is so big that there are no sense to turn
			! the particle. Insterd that the sferical simmetric
			! velocity is genegating. It is much more faster.
	integer sisferaBdel
	real cuteneBdel
	integer nstepBdel
	parameter (cuteneBdel=1.0e-3)
	common / cbdel /
     +		lamaBdel(pqener,pQMat),
     +		pntBdel(3),npntBdel(3),
     +		stepBdel, lamBdel,
     +		rangBdel,rangpBdel,
     +		eMinBdel, iMinBdel,
     +		eLossBdel(pqener,pQMat),
     +		betaBdel(pqener), beta2Bdel(pqener),
     +		momentumBdel(pqener), momentum2Bdel(pqener),
     +		 msigBdel(pqener),
     +		rTetacBdel(pqener,pQMat),
     +		nBdel,eBdel,
     +		estepBdel,
     +		velBdel(3),
     +		e1Bdel(3),e2Bdel(3),e3Bdel(3),
     +		nVolBdel,sgonextBdel,sturnBdel,
     +		TetacBdel(pqener,pQMat),
     +		CosTetac12Bdel(pqener,pQMat),
     +		SinTetac12Bdel(pqener,pQMat),
     +		TetaBdel,
     +		mlamBdel,mTetacBdel,
     +		iBdel,
     +		StBdel,NtvBdel,SOBdel,
     +		sruthBdel,
     +		sisferBdel,
     +		sisferaBdel(pqener,pQMat),
     +		nstepBdel
	save / cbdel /

c			below there are the values for exact elastic
c						scatering
	integer pqanCBdel
	parameter (pqanCBdel=31)
	integer qanCBdel
	parameter (qanCBdel=30)
	real anCBdel
	real ancCBdel
	
	integer pqeaCBdel
	parameter (pqeaCBdel=10)
	integer qeaCBdel
	parameter (qeaCBdel=9)
	real enerCBdel, enercCBdel
	real sign_ACBdel	! sign that the parameters are read
	real ACBdel		! parameters
	real CCBdel
	real BCBdel
	real sCBdel		! cross section, Angstrem**2 / strd
	real sRCBdel		! Rutherford cross section for comparison
	real sRmCBdel		! maximum of Rutherford die to cut
	real sRcmCBdel		! the cut angle again
	real smaCBdel		! cross section for material per one av. atom,
				! in MeV**-2/rad
	real smatCBdel		! cross section for material per one av. atom,
				! in MeV**-2/rad, for working energy mesh
	real ismatCBdel		! normalized integral
	real tsmatCBdel		! integral
	real gammaCBdel
	real beta2CBdel
	real momentum2CBdel
	real rrCBdel		! range by usual formula
	real koefredCBdel	! koef for derivation of step
				! from usual formula
	parameter (koefredCBdel=0.02)
	common / cbdel1 /
     +		anCBdel(pqanCBdel), ancCBdel(pqanCBdel),
     +		enerCBdel(pqeaCBdel), enercCBdel(pqeaCBdel),
     +		sign_ACBdel(pqAt),
     +		ACBdel(4,pqeaCBdel,pqAt), CCBdel(0:6,pqeaCBdel,pqAt),
     +		BCBdel(pqeaCBdel,pqAt),
     +		sCBdel(pqanCBdel,pqeaCBdel,pqAt),
     +		sRCBdel(pqanCBdel,pqeaCBdel,pqAt),
     +		sRmCBdel(pqeaCBdel,pqAt),
     +		sRcmCBdel(pqeaCBdel,pqAt),
     +		smaCBdel(pqanCBdel,pqeaCBdel,pQMat),
     +		smatCBdel(pqanCBdel,pqener,pQMat),
     +		ismatCBdel(pqanCBdel,pqener,pQMat),
     +		tsmatCBdel(pqener,pQMat),
     +		gammaCBdel(pqeaCBdel), beta2CBdel(pqeaCBdel),
     +		momentum2CBdel(pqeaCBdel),
     +		rrCBdel(pqener,pQMat)
	save / cbdel1 /

	real MagForFBdel
	real EleForFBdel
	real veloBdel
	common / cbdel2 /
     +	MagForFBdel(3), EleForFBdel(3),
     +	veloBdel(3)
	save / cbdel2 /




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

c	include 'hconst.inc'
*** Added TRACK common to select tracing of delta's (RV 21/2/97).
       INTEGER MXWIRE,MXSW,MXLIST,MXCHA,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR,
     -         MXPAIR,MXPART,MXFOUR,MXCLUS,
     -         MXLINE,MXEQUT,
     -         MXRECL,MXINCH,MXWORD,MXCHAR,MXNAME,MXLUN,
     -         MXINS,MXREG,MXARG,MXCONS,MXVAR,MXALGE,
     -         MXZERO,MXSTCK,MXFPNT,MXFPAR,MXWKLS,
     -         MXHLEV,MXHLRL,MXSUBT,
     -         MXDLVL,MXILVL,MXDLIN,
     -         MXHIST,MXFRAC,MXBANG,MXBTAB,
     -         MXEXG,MXIOG,MXCSG,
     -         MXORIA,
     -         MXMAT,MXEMAT,MXMDIM,
     -         MXSHOT,MXZPAR,
     -         MXMAP,MXEPS,MXWMAP,MXSOLI,MXSBUF,
     -         MXPLAN,MXPOIN,MXEDGE,
     -         MXMCA
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXHIST=   200, MXCHA = MXLIST/2)
       PARAMETER (MXGRID=    50)
       PARAMETER (MXNAME=   200, MXLUN =    30)
       PARAMETER (MXCLUS=   500, MXPAIR=  2000, MXPART= 10000)
       PARAMETER (MXLINE=   150, MXEQUT=    50)
       PARAMETER (MXFOUR=    16)
       PARAMETER (MXRECL= 10000)
       PARAMETER (MXINCH=  2000, MXWORD=   200, MXCHAR=MXINCH)
       PARAMETER (MXINS =  1000, MXREG =   500, MXCONS=  -500,
     -            MXVAR =   500, MXALGE=   500, MXARG =   100)
       PARAMETER (MXMAT =   500, MXEMAT=200000, MXMDIM=   10)
       PARAMETER (MXZERO=MXWIRE)
       PARAMETER (MXSTCK=     5)
       PARAMETER (MXFPNT= 20000, MXFPAR=    10)
       PARAMETER (MXWKLS=    10)
       PARAMETER (MXHLEV=     9, MXSUBT=   200, MXHLRL=  860)
       PARAMETER (MXDLVL=    10, MXILVL=    20, MXDLIN= 2500)
       PARAMETER (MXFRAC=    13)
       PARAMETER (MXBANG=    20, MXBTAB=    25)
       PARAMETER (MXEXG =    50, MXIOG =    10, MXCSG =  200)
       PARAMETER (MXORIA=  1000)
       PARAMETER (MXSHOT=    10, MXZPAR=4*MXSHOT+2)
       PARAMETER (MXMAP =350000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
*** End of modification.
	integer id
	integer k
	integer q
	integer j
	integer ti
*** Modification for tracking delta's (RV 10/2/97)
        INTEGER IFAIL
*** End of modification.
	real*8 h
	real rra,rrb
	
c	integer cV
        integer cSV
	integer qn
c	real e,rr(4)
	integer sact
	real v3
c	integer s_change_dir, n_change_dir
*
c	data n_change_dir/1/
	real*8 s
c	real mod_add
c	real add(3)
c	real ranfl
c	real bet,p,x
        real msig
c	real alog,sqrt

	real WW,FF

	ti=0

c	if(srandoff.eq.1)then
c		n_change_dir=10000
c	endif
c	s_change_dir=n_change_dir
c		next 3 lines must be done in Inicel called from GoEvent
c	do k=1,QSVol
c		qcel(k)=0
c	enddo

	do id=1,qdel		! main loop

c	call IniIonen
c	write(oo,*)' id=',id
c	write(oo,*)' rionener=', rionener

	nBdel=id
	ti=0


	rangBdel=0.0
	rangpBdel=0.0
	nstepBdel=0
	nVolBdel=nVoldel(id)
	if(sSensit(nVolBdel) .eq. 0)then
		sact=1
	else
		sact=0
	endif
c	if(srandoff.eq.1)then
c		nVolBdel=6
c		eBdel=esimtran
c		edel(id)=eBdel
c		pntBdel(1)=0.0
c		pntBdel(2)=0.0
cc		pntBdel(3)=wall1(nVolBdel)+
cc     +			(wall2(nVolBdel)-wall1(nVolBdel))*0.5
c		pntBdel(3)=29.0
c		velBdel(1)=0.0
c		velBdel(2)=0.0
c		velBdel(3)=1.0
c		do j=1,3
c	    	    pntdel(j,id)=pntBdel(j)
c	    	    veldel(j,id)=velBdel(j)
c	        enddo
c
c	else
		eBdel=edel(id)
		do j=1,3
	            pntBdel(j)=pntdel(j,id)
	            velBdel(j)=veldel(j,id)
		enddo
		if(eBdel.le.2.0*cuteneBdel)then

c		call PriBdel(1)

c		make the turn if the energy is too small
c		the electron must be traced by simple formula
c		for range without multiple scatering
c		so as it could be sensible
		if(eBdel.le.cuteneBdel)then
			msig=0.4
		else
		if(eBdel.le.2.0*cuteneBdel)then
			msig=0.2
		endif
		endif
                call lranor(rra,rrb)
                TetaBdel=rra*msig
		call MakeNewSys(e1Bdel,e2Bdel,e3Bdel,velBdel)
		call turnvec(e1Bdel,e2Bdel,e3Bdel,TetaBdel,velBdel)

c		call PriBdel(1)

		endif
c	endif
	sgonextBdel=0
	sturnBdel=0
	sisferBdel=0
	iBdel=0
	stepBdel=0.0

c	call MakeNewSys(e1Bdel,e2Bdel,e3Bdel,velBdel)

	if(nVolBdel.eq.0)then
c		call lstdelo
		go to 20
	endif
	if(eBdel.le.0.000001)then
c	if(eBdel.le.eMinBdel)then
c		call lstdelo
		go to 20
	endif
c	if(sMatC(nMatVol(nVolBdel)).eq.0)then	
c		call lstdelo
c		go to 20
c	endif

10	continue
*** Moved next statement after CALL SSTEPBDEL (RV 16/2/99)
C	nstepBdel=nstepBdel+1
*** End of modification.
c	call PriBdel(1)

*	if(s_change_dir.eq.1)then
*	if(sgonextBdel.eq.0.and.stepBdel.gt.0.0)then
**
c	e=eBdel
c	cV=nVolBdel
c        	rr(1)=(1.0E-5/DensMat(nMatVol(cV)))
c     +		*1.0E4*(e*1.0E3)**1.5
c		rr(1)=rr(1)/10000.0
c		rr(2)=0.71*(e**1.72)/DensMat(nMatVol(cV))
c		rr(3)=0.2115*(Z_Mean(nMatVol(cV))**0.26)*
c     +			e**(1.265-0.0954*alog(e))/DensMat(nMatVol(cV))
c		
c		e=e*1000
c		rr(4)=1.225e-3*e**1.912/DensMat(nMatVol(cV))
c		e=e/1000
c		write(oo,*)' rr=',rr
c		stop

c	    bet=1.0-ELMAS*ELMAS/((ELMAS+eBdel)*(ELMAS+eBdel))
c	    bet=sqrt(bet)
c	    p=eBdel*eBdel+2.0*ELMAS*eBdel
c	    p=sqrt(p)
c	    x=stepBdel/RLenMat(nMatVol(nVolBdel))
c	    msig=sqrt(2.0)*13.6/(bet*p)*
c    +		sqrt(x)
*cc	    msig=sqrt(2.0)*13.6/(bet*p)*
*cc     +		sqrt(x)*
*cc     +		(1.0 + 0.20*alog(x))
c	    write(oo,*)' eBdel,stepBdel=',eBdel,stepBdel
c	    write(oo,*)' msig=',msig
*
*c	    call PriBdel(1)
*c	    write(oo,*)' bet,p=',bet,p
*c	    write(oo,*)' x,msig=',x,msig
*	    mod_add=0.1*abs(ranfl())
*	    if(srandoff.eq.1)then
*		mod_add=mod_add*0.001
*	    endif
*	    if(mod_add.gt.0.9)mod_add=0.9
*	    call sfersim(add)
*	    s=0.0
*	    do j=1,3
*		velBdel(j)=velBdel(j)+mod_add*add(j)
*		s=s+velBdel(j)*velBdel(j)
*	    enddo
*	    s=sqrt(s)
*	    do j=1,3
*		velBdel(j)=velBdel(j)/s
*	    enddo
*cc	    write(oo,*)' next  velBdel=',velBdel
*	    s_change_dir=n_change_dir
*cc	    irnc=n_change_dir
*	endif
*	else
*	    s_change_dir=s_change_dir-1
*	endif
*** Modified the following line, original follows (RV 16/2/99).
C	call SstepBdel
C	if(nVolBdel.eq.0)then	! this is current numbers
C		go to 20
C	endif
*** New lines, forcing volume search when tracing deltas.
       IF(LTREXB)NVOLBDEL=0
       CALL SSTEPBDEL
       IF(NVOLBDEL.EQ.0)THEN
            PRINT *,' !!!!!! TREATD WARNING : Delta electron'//
     -           ' has left tracking area; delta incomplete.'
            GOTO 20
       ENDIF
       NSTEPBDEL=NSTEPBDEL+1
*** End of modification.

	if(sSensit(nVolBdel) .eq. 0)then
c	if(sgonextBdel.eq.1)then
		sact=1
	endif

	if(estepBdel.gt.0)then

c	if(sMatC(nMatVol(nVolBdel)).eq.0)then	
c		call lstdelo
c		go to 20
c	endif
	if(srandoff.ne.1)then
	    if(eBdel.gt.cuteneBdel)then
	    if(estepBdel.lt.eBdel)then
		call lranor(rra,rrb)
		if(rra.lt.-2.0)rra=-2.0
		if(rra.gt. 2.0)rra= 2.0
		estepBdel=estepBdel+0.33333*estepBdel*rra
		if(estepBdel.gt.eBdel)estepBdel=eBdel
	    endif
	    endif
	endif
	if(sSensit(nVolBdel).eq.1)then
	if(nMatVol(nVolBdel).gt.0)then     ! not a vacuum
	if(WWW(nMatVol(nVolBdel)).gt.0)then
	    WW=WWW(nMatVol(nVolBdel))
	    FF=FFF(nMatVol(nVolBdel))	
	    if(estepBdel.gt.0)then
		if(estepBdel.ne.eBdel)then
		    call lsgcele(estepBdel,WW,FF,q)
		else
		    call lsgcele1(estepBdel,WW,FF,q)
c		    call lsgcele(estepBdel,WW,FF,q)
		endif
		if(q.gt.0)then
		    h=stepBdel/q
		    cSV=numSensVol(nVolBdel)
c		    if(cSV.gt.0)then
		    if((qcel(cSV)+q) .gt. pqcel)then
			qOverflowCel(cSV)=qOverflowCel(cSV)+q
			if(sOverflowCel(cSV).eq.0)then
			    qsOverflowCel(cSV)=qsOverflowCel(cSV)+1
			    sOverflowCel(cSV)=1
			endif
		    else
	    	    do k=1,q
			qcel(cSV)=qcel(cSV)+1
*** Modification to trace delta's in E&B fields (RV 21/2/97, 19/1/09).
       IF(LTREXB.AND.LTRDEL)THEN
            IF(K.EQ.1)THEN
                 CALL TRAEXB(pntBdel,velBdel,            ! Start
     -                pntcel(1,qcel(csV),csV),velBdel,   ! End
     -                eBdel,h,IFAIL)                     ! Energy, step
                 IF(IFAIL.NE.0)THEN
                      do j=1,3
                      pntcel(j,qcel(cSV),cSV)=
     -                     pntBdel(j)+velBdel(j)*k*h
                      enddo
                 ENDIF
            ELSE
                 CALL TRAEXB(
     -                pntcel(1,qcel(csV)-1,csV),velBdel, ! Start
     -                pntcel(1,qcel(csV),csV),velBdel,   ! End
     -                eBdel-(k-1)*estepBdel/q,h,IFAIL)   ! Energy, step
                 IF(IFAIL.NE.0)THEN
                      do j=1,3
                      pntcel(j,qcel(cSV),cSV)=
     -                     pntcel(1,qcel(csV)-1,csV)+velBdel(j)*k*h
                      enddo
                 ENDIF
            ENDIF
       ELSE
	    do j=1,3
	        pntcel(j,qcel(cSV),cSV)=
     +		pntBdel(j)+velBdel(j)*k*h
	    enddo
       ENDIF
*** End of modification.
			zcel(qcel(cSV),cSV)=1
			Ndelcel(qcel(cSV),cSV)=id
			sactcel(qcel(cSV),cSV)=sact
	    	    enddo
*** Addition: update the location and reference frame (RV 11/2/97)
       IF(LTREXB)THEN
            DO J=1,3
            npntBdel(j)=pntcel(j,qcel(csV),csV)
            ENDDO
            call MakeNewSys(e1Bdel,e2Bdel,e3Bdel,velBdel)
       ENDIF
*** End of addition.
		    if(shfillrang.eq.1)then
c			make the change only for first and last electrons
			s=0.0
			qn=q-1
	    		do j=1,3
			    s = s +
     +			    (pntcel(j,(qcel(cSV)-qn),cSV)
     +			    - pntdel(j,nBdel)) * veldel(j,nBdel)
	    		enddo
			if(s.gt.rangpBdel)then
			    rangpBdel=s
			endif
			if(q.gt.1)then
			    s=0.0
	    		    do j=1,3
			        s = s +
     +			        (pntcel(j,qcel(cSV),cSV)
     +			        - pntdel(j,nBdel)) * veldel(j,nBdel)
	    		    enddo
			    if(s.gt.rangpBdel)then
			        rangpBdel=s
			    endif
			endif			
		    endif			
		    endif
c		    call Pricel
c		    if(nevt.eq.17.or.nevt.eq.18)then
c		    call PriBdel(1)
c		    write(oo,*)' q=',q,' rangpBdel=',rangpBdel
c		    endif
		endif
	    endif
	endif
	endif
	endif

	endif

	do j=1,3
		pntBdel(j)=npntBdel(j)
	enddo
	eBdel=eBdel-estepBdel
	rangBdel=rangBdel+stepBdel
*	if(shfillrang.eq.1)then
c		It is enouph to do at the end of each step!
c		It was wrong algorithm becouse
c		the electrons are created not on the each step
*	    s=0.0
*	    do j=1,3
*		s=s+(pntBdel(j)-pntdel(j,nBdel))*veldel(j,nBdel)
*	    enddo
*	    if(s.gt.rangpBdel)then
*		rangpBdel=s
*	    endif
*	endif
c	if(eBdel.le.eMinBdel)then
	if(eBdel.le.0.000001)then
c		call lstdelo
		go to 20
	endif
c		The treatment of the electric and magnetic field
c		Now it will be very preliminary.
c		Calculate the actual velocity
		
	
	if(sturnBdel.eq.1)then
c		if(ti.le.1)then
		ti=ti+1
		v3=velBdel(3)
		call TurnBdel
		if(sgonextBdel.eq.1)then
			if(v3.lt.0.and.velBdel(3).gt.0)then
				sgonextBdel=0
			else
			if(v3.gt.0.and.velBdel(3).lt.0)then
				sgonextBdel=0
			endif
			endif
		endif
c		endif
	endif

	go to 10

20	continue

*** Changed (RV 13/5/97).
C	call hfill(nh2_ard,rangBdel,edel(id),1.0)
	call hfill(nh2_ard,real(rangBdel),edel(id),1.0)
*** End of change.
	rangedel(nBdel)=rangBdel
	if(shfillrang.eq.1)then
		call hfill(nh2_rd,real(rangpBdel),edel(id),1.0)
		call hfill(nh1_rd,real(rangpBdel),0.0,1.0)
		rangepdel(nBdel)=rangpBdel
	endif

	qstepdel(nBdel)=nstepBdel

	enddo

	end



        subroutine lsgcele(e,WW,FF,irn)

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

c	include 'hconst.inc'
c       include 'lsmabs.inc'
	real wmabs,fmabs,e,RN,SIGMA,YY,DIMMY,w,wr
	real WW,FF	
	real r
	integer irn,i
	real ranfl
	wmabs=WW
c        wmabs=rionener
c       wmabs=0.000026
        fmabs=FF
c        fmabs=0.19
c	write(oo,*)' srandoff=',srandoff,' wmabs=',wmabs
	if(srandoff.eq.1)then
		fmabs=0.0
	endif
        if(e.gt.0.0)then
          RN=E/wmabs
          SIGMA=SQRT(fmabs*RN)
          CALL LRANOR(YY,DIMMY)
c          RN=RN+YY*SIGMA+0.4999
          r=YY*SIGMA		! so as to prevent shift
	  if(r.lt.-RN)then
		r=-RN
	  elseif(r.gt.RN)then
		r=RN
	  endif
c	  if(r.lt.-1.0)then
c		r=-1.0
c	  elseif(r.gt.1.0)then
c		r=1.0
c	  endif

          RN=RN+r
	  if(rn.le.0.0)then
	    irn=0
	    return
	  endif
	  i=rn
	  w=1.0-(rn-i)
	  wr=ranfl()		! this is very small random.
				! I don't want to swich it off
c	write(oo,*)' e,rn,i,w,wr='
c	write(oo,*)e,rn,i,w,wr
	  if(wr.lt.w)then
	    rn=i
	  else
	    rn=i+1
	  endif
          IF(RN.LT.0.0)RN=0.0
        else
                RN=0.0
        endif
        irn=rn
        end


        subroutine lsgcele1(e,WW,FF,irn)

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

c	include 'hconst.inc'
c       include 'lsmabs.inc'
	real wmabs,fmabs,e,RN,SIGMA,YY,DIMMY,w,wr
	real WW,FF
	real vmabs
	real r
	integer irn,i
	real ranfl
        wmabs=WW
c        wmabs=rionener
c	vmabs=0.000028
c	vmabs=wmabs*1.5
c	vmabs=wmabs
	vmabs=wmabs*0.5
c	vmabs=0.0000266
	if(e.le.vmabs)then
	    irn=1
	    return
	endif
c       wmabs=0.000026
        fmabs=FF
c        fmabs=0.19
c	write(oo,*)' srandoff=',srandoff
	if(srandoff.eq.1)then
		fmabs=0.0
	endif
        if(e.gt.0.0)then
          RN=(E-vmabs)/wmabs
          SIGMA=SQRT(fmabs*RN)
          CALL LRANOR(YY,DIMMY)
c          RN=RN+YY*SIGMA+0.4999
          r=YY*SIGMA		! so as to prevent shift
	  if(r.lt.-RN)then
		r=-RN
	  elseif(r.gt.RN)then
		r=RN
	  endif
c	  if(r.lt.-1.0)then
c		r=-1.0
c	  elseif(r.gt.1.0)then
c		r=1.0
c	  endif

          RN=RN+r
	  if(rn.le.0.0)then
	    irn=1
	    return
	  endif
	  i=rn
	  w=1.0-(rn-i)
	  wr=ranfl()		! this is very small random.
				! I don't want to swich it off
c	write(oo,*)' e,rn,i,w,wr='
c	write(oo,*)e,rn,i,w,wr
	  if(wr.lt.w)then
	    rn=i
	  else
	    rn=i+1
	  endif
          IF(RN.LT.0.0)RN=0.0
        else
                RN=0.0
        endif
c	IF(RN.LT.1.0)RN=1.0
	rn=rn+1
        irn=rn
        end
