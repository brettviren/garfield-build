CDECK  ID>, EHEED.
c	Initialization of HEED for simulation event by event
c	with calls of HEED from another program.
c	Volumes and tracks are to be initialized by usual HEED routines:
c	IniFVolume, IniNVolume, and IniRTrack


	subroutine IMHEED
     +	(qmol, nmol, pwmol, ppres, ptemp, psoo, poo, debug,
     +	density, ierror)
c
c	The subroutine for initialization of the medium.
c	Required are only information about matter.
c	Cross sections are to be initialized later, when the particle
c	velosity is fixed.
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

c         include 'molecules.inc'
        integer    pqMol           ! Quantity of sorts of molecules.
        parameter (pqMol=27)

	integer 	 numm_He
	parameter 	(numm_He= 1)	

	integer 	 numm_Ne
	parameter 	(numm_Ne= 2)	

	integer 	 numm_Ar
	parameter 	(numm_Ar= 3)	

	integer 	 numm_Kr
	parameter 	(numm_Kr= 4)	

	integer 	 numm_Xe
	parameter 	(numm_Xe= 5)	

	integer 	 numm_H2
	parameter 	(numm_H2= 6)	

	integer 	 numm_N2
	parameter 	(numm_N2= 7)	

	integer 	 numm_O2
	parameter 	(numm_O2= 8)	

	integer 	 numm_NH3
	parameter 	(numm_NH3= 9)

	integer 	 numm_N2O
	parameter 	(numm_N2O= 10)

	integer 	 numm_CO2
	parameter 	(numm_CO2= 11)	

	integer 	 numm_CF4
	parameter 	(numm_CF4= 12)	

	integer 	 numm_CH4
	parameter 	(numm_CH4= 13)

	integer 	 numm_C2H2
	parameter 	(numm_C2H2= 14)

	integer 	 numm_C2H4
	parameter 	(numm_C2H4= 15)

	integer 	 numm_C2H6
	parameter 	(numm_C2H6= 16)

	integer 	 numm_C3H8
	parameter 	(numm_C3H8= 17)

	integer 	 numm_iC4H10
	parameter 	(numm_iC4H10= 18)

	integer 	 numm_C			! for debug
	parameter 	(numm_C = 19)
*** Additions (RV 4/9/98).
        integer          numm_DME
        parameter       (numm_DME= 20)

        integer          numm_H2O
        parameter       (numm_H2O= 21)
*** Additions (RV 20/9/99).
        integer          numm_SF6
        parameter       (numm_SF6= 22)

        integer          numm_C2F4H2
        parameter       (numm_C2F4H2= 23)

*** Addition (RV 14/1/00).
        integer          numm_C5H12
        parameter       (numm_C5H12= 24)
*** Addition (RV 25/2/00).
        integer          numm_C2F5H
        parameter       (numm_C2F5H= 25)
*** Addition (RV 4/9/01).
        integer          numm_C3F8
        parameter       (numm_C3F8= 26)
*** Additions (RV 14/12/07).
        integer          numm_CS2
        parameter       (numm_CS2= 27)
*** End of additions.

c	integer 	 numm_CClF3
c	parameter 	(numm_CClF3= 19)

c	integer 	 numm_CClF2
c	parameter 	(numm_CClF2= 20)

c	integer 	 numm_CBrF3
c	parameter 	(numm_CBrF3= 21)

c	integer 	 numm_SF6
c	parameter 	(numm_SF6= 22)
c         include 'molecdef.inc'
	integer pqSAtMol	! Max. allowed quantity of sorts of atoms
				! in a molecule.
	parameter (pqSAtMol=3)
	integer qSAtMol		! Quantity of sorts of atoms in a molecules.
	integer nAtMol		! Number of atom in atoms.inc,
				! see LibAtMat.inc.
	integer qAtMol		! Quantity of atoms of given sort in molecule
	real weiMol		! Molecular weight
	real WWWMol		! Mean work for pair production
	real FFFMol		! Parammeter Fano
	common / cmodef /
     +	qSAtMol(pqMol),
     +	nAtMol(pqSAtMol,pqMol),
     +	qAtMol(pqSAtMol,pqMol),
     +	weiMol(pqMol),
     +	WWWMol(pqMol),
     +	FFFMol(pqMol)
	save / cmodef /


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
c 	include 'crosec.inc'
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
	

c         include 'cconst.inc'
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
c         include 'part.inc'
c		The incoming particle.
c		After changing the particle you have
c		to recalculate crossec
	real tkin,mass		! Kin.energy
	real*8 beta2,beta12	! Beta**2 and 1.0-Beta**2
	real emax		! Max. energy of delta electron
	real bem		! beta2/emax
	real coefPa		! help const
c		It is in energy transfer cross sections:
c			  Alpha
c			----------
c			beta2 * pi
	real partgamma		! gamma factor
	real partmom,partmom2	! momentum and momentum**2
	integer s_pri_elec	! Sign  that primary particle is electron.
				! It is recognized by mass near to 0.511
				! In some parts of program the direct condition
				! like mass < 0.512 is used.
	common / cpart /
     +	tkin,mass,
     +	beta2,beta12,
     +	partgamma,
     +	partmom,partmom2,
     +	emax,
c     +	ecut,
     +	bem ,
     +	coefPa,
     +	s_pri_elec
	save / cpart /
c         include 'hist.inc'


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

	

c         include 'random.inc'
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
	
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM



	integer qmol		! Quantity of different molecules
				! in the gas mixture.
        integer nmol(pqMol)     ! Their numbers from molecules.inc.
                                ! Use only the named constants
                                ! for compartibility with the future versions
	real pwmol(pqMol)	! Their weights
				! (relative quantities of molecules).
	real ppres		! Pressure in Torr.
	real ptemp		! Temperature in K.
	integer psoo		! Flag allowing to write.
	integer poo		! Output stream number.
	integer debug		! Flag allowing to write
				! more amount of information.

c	Output parameters:
	real density		! Density, calculated as for ideal gas, gr/cm3
	integer ierror		! Sign of error( 0 -- no error ).

	real wmol(pqMol)

C	integer nc
	integer n,i
	real s

	real pres		! Pressure in Torr.
	real temp		! Temperature in K.
	
c	real step_integ_ar
	integer tresh
	parameter (tresh=20)
c	real e1,e2

	integer nmat
	integer nat
*** Additional debug output (RV 13/8/98).
        IF(LDEBUG)THEN
             WRITE(LUNOUT,'(''  ++++++ IMHEED DEBUG   : '',
     -            ''Pressure:       '',F10.3,'' Torr''/26X,
     -            ''Temperature:    '',F10.3,'' K''/26X,
     -            ''Gas components: '',I5/26X,
     -            ''Identifier    Fraction'')') ppres,ptemp,qmol
             DO I=1,qmol
             WRITE(LUNOUT,'(26X,I10,F12.4)') nmol(i),pwmol(i)
             ENDDO
        ENDIF
*** End of modification.

c	restore after previous run

	do nat=1,pQAt
		Zat(nat)=0
	enddo

	nmat=1

	QAtMat(nmat)=0

c	go ahead

        s=0.0
        do n=1,qmol
          s=s+pwmol(n)
        enddo
        do n=1,qmol
          wmol(n)=pwmol(n)/s
        enddo


	call Iniranfl

	soo=psoo
	oo=poo
	sret_err=1

	sHist=0			! To ban operating with historgams
	HistFile='heed.hist'	! To make sure. Histograms must not be filled
				! and written here.
        maxhisampl=40.0e-3
        maxhisampl2=20.0e-3
	maxhisample=200
        pqhisampl=100
	shfillrang=0

c       Random number genarator
        sseed=0
        seed(1)=1121517854	! this is example
        seed(2)=612958528


        qevt=1       ! Quantity of events to generate

        ssimioni=1              ! Simulate ionization loss
        ninfo=0                 ! Number of first events with output listing

        call Inishl                     ! Cascade from excited atom

        call IniEner(150,3e-6,0.2)      ! Energy mesh
	if(debug.ge.2)call PriEner

        call AtomsByDefault             ! Library of atoms
*** Added argument to PriAtoms (RV 13/4/99)
	if(debug.ge.2)call PriAtoms(0)
*** End of modification.

	if(ppres.eq.0)then
		pres=Atm_Pressure
	else
		pres=ppres
	endif

	if(ptemp.eq.0)then
		temp=Atm_Temper
	else
		temp=ptemp
	endif

	call molecdef
	if(debug.ge.2)call Primolec

	call Inigas(nmat, qmol, nmol, wmol, pres, temp)
*** Added argument to PriMatter (RV 13/4/99).
	if(debug.ge.2)call PriMatter(0)
*** End of modification.
        if(s_err.eq.1)then
		ierror=1
		return
	endif
	density=DensMat(nmat)

	end


	subroutine IPHEED
     +	(ptkener, pmas, debug,
     +	ierror)

c	Initialization of particle, cross sections,
c	and tracing of delta-electrons.
c	The volume(s) have to be initialized before!

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


	real ptkener		! Kinetic energy of incident particle.
	real pmas		! Mass of incident particle.
		! In the case of zero in two above var. the following
		! two ones will be sensible (see text).
	real tkener		! Kinetic energy of incident particle.
	real mas		! Mass of incident particle.

	integer debug		! Flag allowing to write
				! more amount of information.

c	Output parameters:
	integer ierror		! Sign of error( 0 -- no error ).


	if(pmas.eq.0)then
		mas=938
	else
		mas=pmas
	endif

	if(ptkener.eq.0)then
		tkener=mas*(4-1)	! 'mip'
	else
		tkener=ptkener
	endif

        call IniPart(tkener,mas)        ! Particle
	if(debug.ge.2)call Pripart
        if(s_err.eq.1)then
		ierror=1
		return
	endif

        call IniCrosec                  ! Cross sections
	if(debug.ge.2)call PriCrosec(1,1)
		
        call InisBdel                   ! Data for tracing of delta-electrons

	end

c	After that the track must still be initialized by IniRTrack.

c	The UBegEvent end UEndEvent subroutine can be empty in this case.

        subroutine UBegEvent

	end

        subroutine UEndEvent

	end

c	The GoEvent must know the number of the current event
c	and the total ordered event number. If there was an overflow
c	of any controlled array - arrays with delta-electrons,
c	conduction electrons, real photons, virtual photons,
c	the GoEvent prints the wornings and auxiliary information
c	to the 'oo' after the last event generated.
c	So as avoid of including of GoEvent.inc , where the event number
c	nevt and quantity of events qevt are stored, user can call GoEventn ,
c	that takes nevt and qevt as arguments and simulates ONE event.

	subroutine GoEventn(pnevt, pqevt)

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

	integer pnevt, pqevt

	nevt = pnevt	
	qevt = pqevt

	call GoEvent

	end
