CDECK  ID>, INIBDEL5.


c
c       Package for tracing of delta-electrons.
c


        subroutine InisBdel

c
c       This is routine for standart initialization.
c       It is strictly recommended.
c
c        call IniBdel(1,0.0001, 0.00005*4.0e-3, 0.1)
        call IniBdel(2,0.0001, 0.001*4.0e-3, 0.1)

        end


	subroutine IniBdel(psruthBdel,peMinBdel,pmlamBdel,pmTetacBdel)
c
c	Initialization of the delta-eleectron tracing package
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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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


	integer psruthBdel
	real peMinBdel,pmlamBdel,pmTetacBdel
	integer n,nm,na,na1,nen
	real dedx1,sde,sde2
c       real dedx,dedx2
	real rms,rm(pQAt),adens
	real mT,A
	real*8 B,r
	real msig,x
	integer sienred
	real rr,ek,cor
	real fcalcsCBdel
c       real s
	integer nang
	integer nprev, nnext, qempt
	integer nempt(pqAt),nqe
	real*8 k,c
	real*8 f1,f2,z1,z2
	integer nam
	real*8 sd,st,st1
	integer n1,n2,nener


	sruthBdel=psruthBdel
	eMinBdel=peMinBdel
	mlamBdel=pmlamBdel
	mTetacBdel=pmTetacBdel
	if(eMinBdel.lt.ener(1))then
		write(oo,*)' eMinBdel is too small, eMinBdel=',eMinBdel
		stop
	endif
c	do n=2,qener
c		if(eMinBdel.lt.ener(n))then
c			iMinBdel=n-1
c			go to 10
c		endif
c	enddo
c	write(oo,*)' worning: eMinBdel is too hige, eMinBdel=',eMinBdel
c	iMinBdel=qener+1
c10	continue
        do n=1,3
                e1Bdel(n)=0.0
                e2Bdel(n)=0.0
                e3Bdel(n)=0.0
        enddo
	sturnBdel=0.0
	do nm=1,pQMat
		do nen=1,qener
			TetacBdel(nen,nm)=0.0
		enddo
	enddo
	TetaBdel=0.0
c	do n=iMinBdel,qener
c		call IniPart(enerc(n),0.511)
c		call IniCrosec
	do nm=1,pQMat
	  if(qAtMat(nm).gt.0)then
c	    if(sMatC(nm).gt.0)then
	      rms=0.0
	      do na=1,QAtMat(nm)
		rms=rms+Aat(AtMat(na,nm))*WeightAtMat(na,nm)
	      enddo
	      do na=1,QAtMat(nm)
		rm(na)=Aat(AtMat(na,nm))*WeightAtMat(na,nm)/rms
	      enddo
	      sienred=0
	      do n=qener+1,1,-1
		if(sienred.eq.0)then
		  sde=0.0
		  sde2=0.0
	          do na=1,QAtMat(nm)
		    adens=DensMat(nm)*rm(na)
c		    write(oo,*)' adens=',adens
*		    call lsrelp(
*     +		    Aat(AtMat(na,nm)),float(Zat(AtMat(na,nm))),adens,
*     +		    2000.0*ener(n)/1000.0,dedx)
*		    if(dedx.lt.0.0)dedx=0.0

*		    call lsrelm(
*     +		    Aat(AtMat(na,nm)),float(Zat(AtMat(na,nm))),adens,
*     +		    105.65/0.511*ener(n)/1000.0,dedx2)
*		    if(dedx2.lt.0.0)dedx2=0.0

*		    sde=sde+dedx*adens
*		    sde2=sde2+dedx2*adens

		  enddo
*		  sde=sde*1000.0
*		  sde2=sde2*1000.0
							

c			    call lsrelp(
c     +			    A_Mean(nm),Z_Mean(nm),DensMat(nm),
c     +			    2000.0*enerc(n)/1000.0,dedx)
c			    dedx=dedx*DensMat(nm)*1000.0
c		  eLossBdel(n,nm)=sde
		  call lstREL1(ener(n)/1000.0, -1.0, nm, dedx1)
		  dedx1=dedx1*1000.0
		  eLossBdel(n,nm)=dedx1
c		  write(oo,*)' n=',n,' nm=',nm,' ener(n)=',ener(n)
c		  write(oo,*)' sde=',sde,
c     +				' dedx1=',dedx1	,' sde2=',sde2	
		  if(n.lt.qener)then
		    if(eLossBdel(n,nm).lt.0.5*eLossBdel(n+1,nm))then
		      sienred=1
		      eLossBdel(n,nm)=0.5*eLossBdel(n+1,nm)
		    endif
		  endif
		else
		  eLossBdel(n,nm)=eLossBdel(n+1,nm)
		endif
	      enddo
c	    endif
	  endif
	enddo
c	stop	

	do nen=1,qener
	beta2Bdel(nen)=
     +	(2.0*ELMAS*enerc(nen) + enerc(nen)*enerc(nen)) /
     +	((ELMAS + enerc(nen)) * (ELMAS + enerc(nen)))
	betaBdel(nen) = sqrt(beta2Bdel(nen))
        momentum2Bdel(nen)= enerc(nen)*enerc(nen) + 2.0*ELMAS*enerc(nen)
	momentumBdel(nen) = sqrt(momentum2Bdel(nen))
	enddo
	
	if(sruthBdel.ne.2)then

	do nm=1,pQMat
        if(qAtMat(nm).gt.0)then
	do nen=1,qener

	ek=enerc(nen)*1000.0
	if(ek.le.10.0)then
        rr=1.0e-3 * A_Mean(nm)/Z_Mean(nm) * 3.872e-3 * ek ** 1.492
        rr=rr/DensMat(nm)
	else
        rr=1.0e-3 * 6.97e-3 * ek ** 1.6
        rr=rr/DensMat(nm)
	endif
	rr=rr*0.1
	call correctBdel(enerc(nen),cor)

        if(sruthBdel.eq.1)then
	
	lamBdel=mlamBdel/DensMatDS(nm)
	if(lamBdel.lt.rr) lamBdel=rr
	lamBdel=lamBdel*cor

c	if(sisferBdel.eq.1)then
c		go to 10
c	endif
c		Calculate the minimum angle for restriction of field by
c		atomic shell
	mT=2.0*asin(1.0/
     +		(2.0*momentumBdel(nen)*Z_Mean(nm)*5.07e2))
	rTetacBdel(nen,nm)=mT
c	write(oo,*)' mT=',mT
	if(mT.lt.mTetacBdel)then
		mT=mTetacBdel	! Throw out too slow interaction. They
				! do not influent to anything
	endif
c		Calculate the cut angle due to mean free part
	A = RuthMat(nm)/cor/
     +		(momentum2Bdel(nen)*beta2Bdel(nen))/(5.07e10)**2
	B = (lamBdel*A)
	B = sqrt( B / (B+1.0) )
	TetacBdel(nen,nm) = 2.0 * asin(B)
c	TetacBdel = acos( (B-1.0) / (B+1.0) )
c	TetacBdel=2.0*asin(sqrt(lamBdel*A))
c	if(TetacBdel.lt.0.2)then
c		TetacBdel=0.2

c		If it too little, reset it. It will lead to increasing
c		of lamBdel and decriasing of calculation time.
	if(TetacBdel(nen,nm) .lt. mT)then
		TetacBdel(nen,nm)=mT
		B=mT    ! B is double precision
		r=sin(B/2.0)
		lamBdel=1/A * 2.0 * r*r / ( 1 + cos(B) )
*		r=cos(TetacBdel(nen,nm))
*		lamBdel=A*(1.0+r)/(1.0-r)
*		lamBdel=1.0/lamBdel
c		lamBdel=(p2*bet2*sin(TetacBdel/2.0)**2) / A
	endif

	lamaBdel(nen,nm)=lamBdel
	B=TetacBdel(nen,nm)
        CosTetac12Bdel(nen,nm)=cos(B/2.0)
        SinTetac12Bdel(nen,nm)=sin(B/2.0)
	if(TetacBdel(nen,nm).gt.1.5)then
		sisferaBdel(nen,nm)=1
	else
		sisferaBdel(nen,nm)=0
	endif
		
c       debug mode:
c        lamaBdel(nen,nm)=2.0*lamaBdel(nen,nm)

	elseif( sruthBdel.eq.0)then 		! gaus formula

c		calculate paht lengt from mTetacBdel
	msig=mTetacBdel
	x=msig / ( sqrt(2.0) * 13.6/(betaBdel(nen)*momentumBdel(nen)))
	x=x*x

c	x=x/DensMatDS(nMatVol(nVolBdel))
	x=x*RLenMat(nm)*cor
	lamBdel = mlamBdel/DensMatDS(nm)
	if(lamBdel.lt.rr) lamBdel=rr
	lamBdel=lamBdel*cor
c	write(oo,*)' x=',x,' rleng=',rleng
c		reset if it is too large
	if(lamBdel.lt.x)lamBdel=x
	lamaBdel(nen,nm)=lamBdel
	msigBdel(nen)=sqrt(2.0)*13.6/
     +		(betaBdel(nen)*momentumBdel(nen))

c	debug mode:
c	lamaBdel(nen,nm)=2.0*lamaBdel(nen,nm)
c	msigBdel(nen)=0.5*msigBdel(nen)
	endif	

	enddo	! end of nen
	endif	! end of if(qAtMat(nm).gt.0)then
	enddo	! end of nm
	endif 	! if(sruthBdel.ne.2)

	if(sruthBdel.eq.2)then	


	call logscale0(qanCBdel,0.03,real(PI),anCBdel,ancCBdel)

c	call readCBdel
	call read1CBdel

	enerCBdel( 1) = 0.5E-3
	enerCBdel( 2) = 1.5E-3
	enerCBdel( 3) = 2.5E-3
	enerCBdel( 4) = 5.5E-3
	enerCBdel( 5) = 10.5E-3
	enerCBdel( 6) = 21.5E-3
	enerCBdel( 7) = 42.5E-3
	enerCBdel( 8) = 85.5E-3
	enerCBdel( 9) = 170.5E-3
	enerCBdel(10) = 341.1E-3
	enercCBdel( 1) = 1 E-3
	enercCBdel( 2) = 2E-3
	enercCBdel( 3) = 4E-3
	enercCBdel( 4) = 8E-3
	enercCBdel( 5) = 16E-3
	enercCBdel( 6) = 32E-3
	enercCBdel( 7) = 64E-3
	enercCBdel( 8) = 128E-3
	enercCBdel( 9) = 256E-3

	do nen=1,qeaCBdel
		gammaCBdel(nen) = 1.0 + enercCBdel(nen)/ELMAS
		beta2CBdel(nen) = ( 2.0 * enercCBdel(nen)/ELMAS
     +				+ (enercCBdel(nen)/ELMAS)**2 ) /
     +				gammaCBdel(nen)**2
	        momentum2CBdel(nen) =
     +			enercCBdel(nen)*enercCBdel(nen) +
     +			2.0*ELMAS*enercCBdel(nen)
	enddo


	do na=1,pqAt
	
	  if(Zat(na).gt.0)then	! atom is meant initialized

	    do nen=1,qeaCBdel
		mT=1.0/
     +		(2.0*sqrt(momentum2CBdel(nen))*Zat(na)*5.07e2)
		sRcmCBdel(nen,nm)=2.0*asin(mT)
		sRmCBdel(nen,na)= 1/4. *
     +		Zat(na)*Zat(na)*ELRAD*ELRAD*ELMAS*ELMAS/
     +		( momentum2CBdel(nen) * beta2CBdel(nen) * mT**4 ) /
     +		( 5.07E10 ** 2 ) * 1.E16
		
	      do nang=1,qanCBdel

		sRCBdel(nang,nen,na)= 1/4. *
     +		Zat(na)*Zat(na)*ELRAD*ELRAD*ELMAS*ELMAS/
     +		( momentum2CBdel(nen) * beta2CBdel(nen) *
     +		  sin(ancCBdel(nang)/2.0)**4 ) /
     +		( 5.07E10 ** 2 ) * 1.E16

	      enddo

	    enddo
			

	    if(sign_ACBdel(na).eq.1)then

	      do nen=1,qeaCBdel
	        do nang=1,qanCBdel
		  sCBdel(nang,nen,na)=fcalcsCBdel(nang,nen,na)
	        enddo
	      enddo

	    endif

	  endif

	enddo

			! Fill an empty places
	nnext = 0
	qempt = 0	! quantity of the empty places is zero

	

	do na1=1,QseqAt
	    na=nseqAt(na1)
	    if(Zat(na).eq.0)then	! atom is meant not initialized
	      write(oo,*)' error in IniBdel'
	      stop
	    endif

	    if(sign_ACBdel(na).eq.1)then
	      nprev=nnext
	      nnext=na
	    endif
	    if(sign_ACBdel(na).eq.0)then
	      qempt=qempt+1	! add pointer of empty place
	      nempt(qempt)=na
	    endif

	    if(sign_ACBdel(na).eq.1 .and. qempt.ne.0)then
	      if(nprev.eq.0)then	! first filled atom
					! fit by k*Z**2
	        do nen=1,qeaCBdel
	          do nang=1,qanCBdel
		    k=sCBdel(nang,nen,nnext) / Zat(nnext)**2
		    do nqe=1,qempt
		      sCBdel(nang,nen,nempt(nqe)) =
     +			k *Zat(nempt(nqe))**2
		    enddo	! nqe=1,qempt
		  enddo		! nang=1,qanCBdel	
		enddo		! nen=1,qeaCBdel
		qempt=0

	      else			! fit by previous and this filled atom
					! f = k*Z*(Z+c)
	        do nen=1,qeaCBdel
	          do nang=1,qanCBdel
		    f1=sCBdel(nang,nen,nprev)
		    f2=sCBdel(nang,nen,nnext)
		    z1=Zat(nprev)
		    z2=Zat(nnext)
		    c = (f1 * z2**2 - f2 * z1**2 ) /
     +		        (f2 * z1    - f1 * z2    )
		    k = f1 / (z1 * ( z1 + c ) )
		    do nqe=1,qempt
		      sCBdel(nang,nen,nempt(nqe)) =
     +			k*Zat(nempt(nqe))*(Zat(nempt(nqe)) + c)
		      if(sCBdel(nang,nen,nempt(nqe)).lt.0.)
     +			 sCBdel(nang,nen,nempt(nqe)) = 0.
		    enddo
		  enddo
		enddo
		qempt=0

	      endif
	    endif
	

	enddo

	if(qempt.ne.0)then
	  if(nprev.eq.0)then
	    write(oo,*)' error in IniBdel: wrong nprev'
	    stop
	  endif
	  	nnext=nprev		! so as to use the same lines as above	
	        do nen=1,qeaCBdel
	          do nang=1,qanCBdel
		    k=sCBdel(nang,nen,nnext) / Zat(nnext)**2
		    do nqe=1,qempt
		      sCBdel(nang,nen,nempt(nqe)) =
     +			k *Zat(nempt(nqe))**2
		    enddo	! nqe=1,qempt
		  enddo		! nang=1,qanCBdel	
		enddo		! nen=1,qeaCBdel
		qempt=0
	endif
	
c	On this point all the atomic cross sections are generated.
c	Now it is a high time to generate cross sections
c	for initialized materials.

	do nm=1,pQMat
          if(qAtMat(nm).gt.0)then

	    lamBdel=mlamBdel/DensMat(nm)

c	    write(oo,*)' lamBdel=',lamBdel,' mlamBdel=',mlamBdel

	    do nen=1,qeaCBdel
	      do nang=1,qanCBdel
	        sd=0.
		do nam=1,qAtMat(nm)
		  na=AtMAt(nam,nm)
		  sd = sd + sCBdel(nang,nen,na) * WeightAtMat(nam,nm)
		enddo
		sd = sd * 1.0E-16 * 5.07E10 * 5.07E10
c			  Angstrem**2 -> sm**2
c				    sm**2 -> MeV**-2
		sd=sd * 2.0 * PI * sin(ancCBdel(nang))
		smaCBdel(nang,nen,nm)=sd

	      enddo		! nang=1,qanCBdel
	    enddo		! nen=1,qeaCBdel
	
	    do nener=1,qener	! go to working mesh
				! ( The enercCBdel is to rare )
	      if(enerc(nener).lt.500.0e-6)then
		do nang=1,qanCBdel
		  smatCBdel(nang,nener,nm)=0.0
		enddo
	      	lamaBdel(nener,nm)=0.0
		tsmatCBdel(nener,nm)=0.0
	      else

		ek=enerc(nener)*1000.0	! Calculate step lenght by usual formula
		if(ek.le.10.0)then
        	  rr = 1.0e-3 * A_Mean(nm)/Z_Mean(nm) *
     +			3.872e-3 * ek ** 1.492
        	  rr=rr/DensMat(nm)
		else
        	  rr=1.0e-3 * 6.97e-3 * ek ** 1.6
        	  rr=rr/DensMat(nm)
		endif
		rrCBdel(nener,nm)=rr
		rr=rr*koefredCBdel
		if(rr.lt.lamBdel) rr=lamBdel
		do nen=2,qeaCBdel
		  if(enercCBdel(nen).gt.enerc(nener))then
		    n2=nen
		    goto 100
		  endif
		enddo
		n2=qeaCBdel
100		continue
		n1=n2-1
		do nang=1,qanCBdel
				! Linear interpolation
		  smatCBdel(nang,nener,nm)=smaCBdel(nang,n1,nm) +
     +		  (smaCBdel(nang,n2,nm) - smaCBdel(nang,n1,nm)) *
     +		  (enerc(nener) - enercCBdel(n1))	/
     +		  (enercCBdel(n2) - enercCBdel(n1))
		  ismatCBdel(nang,nener,nm)=
     +			smatCBdel(nang,nener,nm)
     +			* (anCBdel(nang+1) - anCBdel(nang))
		enddo		! nang=1,qanCBdel
		rr=1.0/
     +		(rr*(AVOGADRO/(5.07E10 * 5.07E10))
     +			*DensMat(nm)/A_mean(nm))
		st=0.0		! restrict low angles
		st1=0.0
		do nang=qanCBdel,1,-1
		  st = st + ismatCBdel(nang,nener,nm)
		  if(st.gt.rr)then
		    goto 110
		  else
		    st1=st
		  endif
		enddo 	! nang=qanCBdel,1,-1
		nang=0	
110		continue
		nang=nang+1
		TetacBdel(nener,nm)=anCBdel(nang)
		tsmatCBdel(nener,nm)=st1
	      	lamaBdel(nener,nm)=1.0/
     +		(tsmatCBdel(nener,nm)*(AVOGADRO/(5.07E10 * 5.07E10))
     +			*DensMat(nm)/A_mean(nm))
		do n=1,nang-1
		  ismatCBdel(n,nener,nm)=0.0
		enddo
	        call lhispre(ismatCBdel(1,nener,nm),qanCBdel)
		if(TetacBdel(nener,nm).gt.1.0)then
		  sisferaBdel(nener,nm)=1
		endif
	      endif
	    enddo	! nener=1,qener

	    			
	  endif
	enddo



c	All done !

	endif		! if(sruthBdel.eq.2)
		

	end	

	subroutine readCBdel

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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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





        character*1 a
        integer ios
	integer na,z,i,n,j

        open(1,FILE='cbdel.dat',IOSTAT=ios,STATUS='OLD')
        if(ios.ne.0)then
                write(oo,*)' readCBdel: can not open file readCBdel.dat'
                stop
        endif
	
	do na=1,pqAt
	
	    if(Zat(na).gt.0)then	! atom is meant initialized

		sign_ACBdel(na)=0	! cleaning

        do n=1,100000
            read(1,'(A1)',END=100)a
c           write (6,*)a
            if(a.eq.'$')then
                backspace (1)
                read(1,'(A1,I3)')a,z
                if(z.eq.Zat(na))then
                    write(oo,*)a,z
		    do i=1,4
		        read(1,*)(ACBdel(i,j,na),j=1,qeaCBdel)
		    enddo
		    do i=0,6
		        read(1,*)(CCBdel(i,j,na),j=1,qeaCBdel)
		    enddo
		        read(1,*)(BCBdel(j,na),j=1,qeaCBdel)
		    sign_ACBdel(na)=1	! sign of reading
		    go to 100
		endif	
	    endif
	enddo
100	rewind(1)

	    endif

	enddo

	close(1)

	end

	subroutine read1CBdel
c
c	This subroutine must copy data not from external file
c	but from internal data arrays (so as to avoid input which
c	is often machine-dependent)
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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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





c        character*1 a
c        integer ios
	integer na,i,n,j
c        integer z

        integer psqAt
        parameter (psqAt=11)    ! Now only 11 atoms included
	integer ZsCBdel(psqAt)  ! atomic charge
	real AsCBdel(4,pqeaCBdel,psqAt)
	real CsCBdel(0:6,pqeaCBdel,psqAt)
	real BsCBdel(pqeaCBdel,psqAt)
*** Modified on 6/2/97 RV
C        save /ZsCBdel/,/AsCBdel/,/CsCBdel/,/BsCBdel/
        save ZsCBdel,AsCBdel,CsCBdel,BsCBdel
*** End of modification.

c 	include 'cbdeldat.inc'
      data ZsCBdel(1)/ 1 /
      data (AsCBdel( 1 , i, 1 ),i=1,9)/
     + -0.9007, -0.6539, -0.3655, -0.5499, -0.0196,
     + 0.04526, -0.658, 0.008393, -0.3739   /
      data (AsCBdel( 2 , i, 1 ),i=1,9)/
     + 0.3975, 0.338, 0.2884, 0.3151, 0.2809,
     + 0.2774, 0.3126, 0.2787, 0.2928   /
      data (AsCBdel( 3 , i, 1 ),i=1,9)/
     + 0.002344, 0.003208, 0.00294, 0.001429, 0.0009329,
     + 0.00041, 3.017e-05, 0.0001038, 1.757e-05   /
      data (AsCBdel( 4 , i, 1 ),i=1,9)/
     + -3.534e-05, -1.59e-05, -5.392e-06, 9.522e-06, 8.538e-07,
     + -4.278e-08, 7.506e-07, 4.492e-09, 3.551e-08   /
      data (CsCBdel( 0 , i, 1 ),i=1,9)/
     + 1.105, 0.8986, 0.6487, 0.8062, 0.01901,
     + -0.09682, 0.9669, -0.1011, 0.4769   /
      data (CsCBdel( 1 , i, 1 ),i=1,9)/
     + 1.172, 1.05, 0.9256, 0.9955, 0.02643,
     + -0.1263, 1.229, -0.141, 0.6287   /
      data (CsCBdel( 2 , i, 1 ),i=1,9)/
     + 0.7611, 0.7519, 0.8045, 0.751, 0.02258,
     + -0.1017, 0.9513, -0.1224, 0.5042   /
      data (CsCBdel( 3 , i, 1 ),i=1,9)/
     + 0.4001, 0.4377, 0.5676, 0.4597, 0.01605,
     + -0.06736, 0.5969, -0.08834, 0.3282   /
      data (CsCBdel( 4 , i, 1 ),i=1,9)/
     + 0.1718, 0.2092, 0.3277, 0.2304, 0.009861,
     + -0.03748, 0.3072, -0.05421, 0.176   /
      data (CsCBdel( 5 , i, 1 ),i=1,9)/
     + 0.05558, 0.07568, 0.1426, 0.08723, 0.004891,
     + -0.0164, 0.1202, -0.02652, 0.07261   /
      data (CsCBdel( 6 , i, 1 ),i=1,9)/
     + 0.01031, 0.01571, 0.03491, 0.01878, 0.00171,
     + -0.004697, 0.02774, -0.008267, 0.0182   /
      data (BsCBdel( i, 1 ),i=1,9)/
     + 0.008057, 0.004506, 0.002592, 0.001872, 0.0008431,
     + 0.0003444, 0.0003049, 8.926e-05, 6.648e-05   /
      data ZsCBdel(2)/ 2 /
      data (AsCBdel( 1 , i, 2 ),i=1,9)/
     + 0.0327, -0.4242, -0.6746, -0.6343, -0.2289,
     + -0.3277, -0.2001, -1.227, -0.3022   /
      data (AsCBdel( 2 , i, 2 ),i=1,9)/
     + 0.3427, 0.3746, 0.363, 0.3388, 0.2998,
     + 0.298, 0.2891, 0.3407, 0.2914   /
      data (AsCBdel( 3 , i, 2 ),i=1,9)/
     + -0.00727, -0.002397, -0.001851, -0.0009558, 0.001271,
     + 0.0006719, 0.000343, -9.27e-05, 7.883e-05   /
      data (AsCBdel( 4 , i, 2 ),i=1,9)/
     + 5.556e-05, 2.941e-06, 3.477e-06, 9.459e-07, 1.384e-11,
     + 1.73e-07, -7.566e-14, 6.887e-07, 4.899e-08   /
      data (CsCBdel( 0 , i, 2 ),i=1,9)/
     + -0.09725, 0.4519, 0.8681, 0.8734, 0.3088,
     + 0.4817, 0.2759, 1.81, 0.3546   /
      data (CsCBdel( 1 , i, 2 ),i=1,9)/
     + -0.1434, 0.4205, 0.9635, 1.028, 0.3654,
     + 0.6172, 0.3678, 2.294, 0.4574   /
      data (CsCBdel( 2 , i, 2 ),i=1,9)/
     + -0.1141, 0.2335, 0.6551, 0.7411, 0.2638,
     + 0.4836, 0.3015, 1.763, 0.3535   /
      data (CsCBdel( 3 , i, 2 ),i=1,9)/
     + -0.06887, 0.1, 0.3606, 0.4342, 0.1544,
     + 0.3089, 0.2039, 1.09, 0.2158   /
      data (CsCBdel( 4 , i, 2 ),i=1,9)/
     + -0.03233, 0.03143, 0.1606, 0.2074, 0.07401,
     + 0.1633, 0.1164, 0.5456, 0.1024   /
      data (CsCBdel( 5 , i, 2 ),i=1,9)/
     + -0.01082, 0.00537, 0.05227, 0.0725, 0.0269,
     + 0.0664, 0.05306, 0.2027, 0.0328   /
      data (CsCBdel( 6 , i, 2 ),i=1,9)/
     + -0.00182, -0.000404, 0.008547, 0.01166, 0.005736,
     + 0.01634, 0.01557, 0.04167, 0.006162   /
      data (BsCBdel( i, 2 ),i=1,9)/
     + 0.01206, 0.007727, 0.00318, 0.001359, 0.001657,
     + 0.0008551, 0.0004051, 0.0003179, 0.0001234   /
      data ZsCBdel(3)/ 3 /
      data (AsCBdel( 1 , i, 3 ),i=1,9)/
     + 1.427, 1.875, 1.99, 1.699, 1.07,
     + 0.6406, -0.4004, -0.3638, -1.191   /
      data (AsCBdel( 2 , i, 3 ),i=1,9)/
     + 0.05527, 0.09522, 0.1452, 0.1939, 0.2375,
     + 0.2604, 0.3007, 0.2984, 0.3292   /
      data (AsCBdel( 3 , i, 3 ),i=1,9)/
     + -0.0002502, -0.0006965, -0.0008232, -0.000703, -0.0005227,
     + -0.0003072, -0.0002339, -0.0001217, -0.0001381   /
      data (AsCBdel( 4 , i, 3 ),i=1,9)/
     + 2.705e-05, 1.05e-05, 4.396e-06, 1.701e-06, 6.296e-07,
     + 1.826e-07, 7.576e-08, 2.354e-08, 3.617e-08   /
      data (CsCBdel( 0 , i, 3 ),i=1,9)/
     + -1.541, -2.386, -2.805, -2.555, -1.683,
     + -1.062, 0.5774, 0.4788, 1.77   /
      data (CsCBdel( 1 , i, 3 ),i=1,9)/
     + -1.472, -2.601, -3.317, -3.176, -2.153,
     + -1.397, 0.7406, 0.6022, 2.303   /
      data (CsCBdel( 2 , i, 3 ),i=1,9)/
     + -0.8666, -1.737, -2.391, -2.401, -1.672,
     + -1.115, 0.5758, 0.4548, 1.815   /
      data (CsCBdel( 3 , i, 3 ),i=1,9)/
     + -0.4155, -0.9407, -1.395, -1.469, -1.047,
     + -0.718, 0.3605, 0.2727, 1.152   /
      data (CsCBdel( 4 , i, 3 ),i=1,9)/
     + -0.1638, -0.4176, -0.6643, -0.7343, -0.5343,
     + -0.3768, 0.1825, 0.1288, 0.5931   /
      data (CsCBdel( 5 , i, 3 ),i=1,9)/
     + -0.04905, -0.1403, -0.2385, -0.2776, -0.2048,
     + -0.1487, 0.06829, 0.04247, 0.2284   /
      data (CsCBdel( 6 , i, 3 ),i=1,9)/
     + -0.00851, -0.02708, -0.04885, -0.06059, -0.04461,
     + -0.03362, 0.01358, 0.006216, 0.05031   /
      data (BsCBdel( i, 3 ),i=1,9)/
     + 0.004125, 0.002188, 0.001189, 0.0006433, 0.000348,
     + 0.0001781, 9.893e-05, 5.406e-05, 5.406e-05   /
      data ZsCBdel(4)/ 6 /
      data (AsCBdel( 1 , i, 4 ),i=1,9)/
     + -0.2288, -0.158, -0.002296, 0.1188, -0.113,
     + -0.1099, -0.2114, -0.321, -0.3712   /
      data (AsCBdel( 2 , i, 4 ),i=1,9)/
     + 0.1755, 0.1774, 0.1813, 0.1927, 0.2573,
     + 0.2617, 0.2751, 0.2829, 0.286   /
      data (AsCBdel( 3 , i, 4 ),i=1,9)/
     + -0.000567, 0.001007, 0.0005522, -0.0002222, -0.0006304,
     + -0.0003796, -0.0002618, -0.0001435, -7.271e-05   /
      data (AsCBdel( 4 , i, 4 ),i=1,9)/
     + -2.822e-06, -6.323e-06, -1.751e-06, 8.23e-08, 7.391e-06,
     + 2.077e-06, 6.244e-07, 1.488e-07, 3.304e-08   /
      data (CsCBdel( 0 , i, 4 ),i=1,9)/
     + 0.5481, 0.5514, 0.4277, 0.2874, 0.4173,
     + 0.4084, 0.4764, 0.5723, 0.5971   /
      data (CsCBdel( 1 , i, 4 ),i=1,9)/
     + 0.7001, 0.8468, 0.8727, 0.8116, 0.7996,
     + 0.8204, 0.8368, 0.9077, 0.9267   /
      data (CsCBdel( 2 , i, 4 ),i=1,9)/
     + 0.5164, 0.6987, 0.8691, 0.9514, 0.8364,
     + 0.9003, 0.8566, 0.8596, 0.8603   /
      data (CsCBdel( 3 , i, 4 ),i=1,9)/
     + 0.3055, 0.4423, 0.6429, 0.7965, 0.6723,
     + 0.7587, 0.695, 0.6525, 0.6395   /
      data (CsCBdel( 4 , i, 4 ),i=1,9)/
     + 0.1493, 0.2224, 0.3722, 0.5125, 0.4275,
     + 0.5034, 0.4532, 0.3989, 0.381   /
      data (CsCBdel( 5 , i, 4 ),i=1,9)/
     + 0.05661, 0.08288, 0.1587, 0.2398, 0.2002,
     + 0.2435, 0.2194, 0.1783, 0.1645   /
      data (CsCBdel( 6 , i, 4 ),i=1,9)/
     + 0.01273, 0.01736, 0.03764, 0.06171, 0.05196,
     + 0.06335, 0.05949, 0.04171, 0.0395   /
      data (BsCBdel( i, 4 ),i=1,9)/
     + 0.005592, 0.003821, 0.0019, 0.0004467, 0.00118,
     + 0.0005983, 0.0003049, 0.0001453, 6.647e-05   /
      data ZsCBdel(5)/ 7 /
      data (AsCBdel( 1 , i, 5 ),i=1,9)/
     + -0.2683, -0.1095, -0.2076, 1.155, 1.192,
     + 1.083, 0.6177, 0.6945, 0.1072   /
      data (AsCBdel( 2 , i, 5 ),i=1,9)/
     + 0.1794, 0.1917, 0.2207, 0.1476, 0.1849,
     + 0.2177, 0.2517, 0.2517, 0.2784   /
      data (AsCBdel( 3 , i, 5 ),i=1,9)/
     + -0.002106, -0.001189, 0.001094, 0.001768, 0.0006366,
     + 0.0001047, -0.0001064, -1.845e-05, -5.791e-05   /
      data (AsCBdel( 4 , i, 5 ),i=1,9)/
     + 8.363e-06, 2.424e-06, 6.217e-05, 4.937e-07, 3.26e-06,
     + 1.638e-06, 7.072e-07, 8.12e-08, 4.488e-08   /
      data (CsCBdel( 0 , i, 5 ),i=1,9)/
     + 0.587, 0.3883, 0.5649, -1.409, -1.614,
     + -1.596, -0.9572, -1.143, -0.2718   /
      data (CsCBdel( 1 , i, 5 ),i=1,9)/
     + 0.7239, 0.5554, 0.865, -1.48, -1.836,
     + -1.934, -1.17, -1.441, -0.327   /
      data (CsCBdel( 2 , i, 5 ),i=1,9)/
     + 0.5231, 0.4279, 0.73, -0.9541, -1.274,
     + -1.429, -0.8647, -1.105, -0.2408   /
      data (CsCBdel( 3 , i, 5 ),i=1,9)/
     + 0.2991, 0.2539, 0.4765, -0.4998, -0.7137,
     + -0.8552, -0.5104, -0.6825, -0.1421   /
      data (CsCBdel( 4 , i, 5 ),i=1,9)/
     + 0.1378, 0.1199, 0.2486, -0.2148, -0.3255,
     + -0.419, -0.2401, -0.3423, -0.06744   /
      data (CsCBdel( 5 , i, 5 ),i=1,9)/
     + 0.0478, 0.04201, 0.09691, -0.06986, -0.1112,
     + -0.1557, -0.08076, -0.1293, -0.02457   /
      data (CsCBdel( 6 , i, 5 ),i=1,9)/
     + 0.00979, 0.008339, 0.02151, -0.01307, -0.02128,
     + -0.03377, -0.01323, -0.02937, -0.006507   /
      data (BsCBdel( i, 5 ),i=1,9)/
     + 0.005535, 0.002575, 0.005228, 0.002104, 0.00129,
     + 0.0007012, 0.0003761, 0.0001529, 8.43e-05   /
      data ZsCBdel(6)/ 8 /
      data (AsCBdel( 1 , i, 6 ),i=1,9)/
     + -0.3151, -0.4143, -0.3378, 0.775, 1.151,
     + 1.043, 0.8495, 0.6484, 0.6268   /
      data (AsCBdel( 2 , i, 6 ),i=1,9)/
     + 0.1565, 0.2123, 0.228, 0.1668, 0.1769,
     + 0.2119, 0.2388, 0.2526, 0.2555   /
      data (AsCBdel( 3 , i, 6 ),i=1,9)/
     + 0.005179, 0.0008074, 0.002091, 0.00213, 0.001118,
     + 0.0003669, 5.394e-05, 5.051e-06, 1.052e-05   /
      data (AsCBdel( 4 , i, 6 ),i=1,9)/
     + -7.102e-05, -1.079e-05, 5.928e-05, 6.685e-12, 7.192e-07,
     + 1.642e-06, 7.253e-07, 1.528e-07, 1.002e-08   /
      data (CsCBdel( 0 , i, 6 ),i=1,9)/
     + 0.6907, 0.8183, 0.7333, -0.8508, -1.514,
     + -1.489, -1.311, -1.053, -1.081   /
      data (CsCBdel( 1 , i, 6 ),i=1,9)/
     + 0.8607, 1.068, 1.04, -0.8104, -1.685,
     + -1.755, -1.622, -1.305, -1.363   /
      data (CsCBdel( 2 , i, 6 ),i=1,9)/
     + 0.6281, 0.8144, 0.8428, -0.4708, -1.148,
     + -1.259, -1.224, -0.9807, -1.045   /
      data (CsCBdel( 3 , i, 6 ),i=1,9)/
     + 0.3597, 0.4966, 0.5392, -0.2198, -0.6336,
     + -0.728, -0.7484, -0.5893, -0.6437   /
      data (CsCBdel( 4 , i, 6 ),i=1,9)/
     + 0.1652, 0.2472, 0.28, -0.08269, -0.2864,
     + -0.3417, -0.3747, -0.2827, -0.3206   /
      data (CsCBdel( 5 , i, 6 ),i=1,9)/
     + 0.05686, 0.09356, 0.11, -0.02291, -0.09803,
     + -0.1195, -0.1422, -0.09731, -0.1192   /
      data (CsCBdel( 6 , i, 6 ),i=1,9)/
     + 0.01108, 0.02049, 0.02459, -0.003431, -0.01939,
     + -0.02313, -0.03158, -0.01668, -0.02626   /
      data (BsCBdel( i, 6 ),i=1,9)/
     + 0.01527, 0.006677, 0.006234, 0.002632, 0.001398,
     + 0.0008426, 0.0004476, 0.0002062, 7.411e-05   /
      data ZsCBdel(7)/ 9 /
      data (AsCBdel( 1 , i, 7 ),i=1,9)/
     + -0.271, -0.1705, -0.4203, -0.08103, 0.847,
     + 1.032, 0.9064, 0.737, 0.7296   /
      data (AsCBdel( 2 , i, 7 ),i=1,9)/
     + 0.06297, 0.1982, 0.2525, 0.2293, 0.1892,
     + 0.2059, 0.2323, 0.247, 0.251   /
      data (AsCBdel( 3 , i, 7 ),i=1,9)/
     + 0.0192, -0.001907, 0.001649, -0.0005853, 0.001314,
     + 0.0006477, 0.0002021, 6.899e-05, 2.812e-05   /
      data (AsCBdel( 4 , i, 7 ),i=1,9)/
     + -1.458e-05, 6.353e-06, 0.0001059, 4.938e-07, 1.198e-13,
     + 1e-06, 7.184e-07, 1.568e-07, 3.663e-09   /
      data (CsCBdel( 0 , i, 7 ),i=1,9)/
     + 0.8256, 0.4602, 0.7589, 0.3443, -1.043,
     + -1.44, -1.373, -1.174, -1.261   /
      data (CsCBdel( 1 , i, 7 ),i=1,9)/
     + 1.154, 0.6192, 0.9765, 0.5852, -1.093,
     + -1.665, -1.676, -1.445, -1.601   /
      data (CsCBdel( 2 , i, 7 ),i=1,9)/
     + 0.92, 0.4733, 0.7312, 0.5192, -0.6998,
     + -1.174, -1.249, -1.08, -1.243   /
      data (CsCBdel( 3 , i, 7 ),i=1,9)/
     + 0.5763, 0.2837, 0.4353, 0.3475, -0.3624,
     + -0.6677, -0.7544, -0.6459, -0.7811   /
      data (CsCBdel( 4 , i, 7 ),i=1,9)/
     + 0.2949, 0.1363, 0.2107, 0.1826, -0.1537,
     + -0.3085, -0.3728, -0.309, -0.3993   /
      data (CsCBdel( 5 , i, 7 ),i=1,9)/
     + 0.1166, 0.04879, 0.07714, 0.07063, -0.04901,
     + -0.1063, -0.1396, -0.1066, -0.1488   /
      data (CsCBdel( 6 , i, 7 ),i=1,9)/
     + 0.0272, 0.009832, 0.01628, 0.01543, -0.009001,
     + -0.02032, -0.0305, -0.01865, -0.03074   /
      data (BsCBdel( i, 7 ),i=1,9)/
     + 0.02583, 0.004772, 0.007849, 0.001104, 0.001634,
     + 0.0009459, 0.0005241, 0.0002429, 7.913e-05   /
      data ZsCBdel(8)/ 13 /
      data (AsCBdel( 1 , i, 8 ),i=1,9)/
     + -0.4378, -0.3167, -0.2708, -0.212, -0.2487,
     + -0.2509, -0.234, -0.265, -0.2887   /
      data (AsCBdel( 2 , i, 8 ),i=1,9)/
     + 0.0923, 0.1454, 0.1968, 0.2238, 0.244,
     + 0.2547, 0.2598, 0.2632, 0.2677   /
      data (AsCBdel( 3 , i, 8 ),i=1,9)/
     + -0.001988, -0.003033, -0.00252, -0.001545, -0.0008717,
     + -0.0004561, -0.0002297, -0.0001108, -5.184e-05   /
      data (AsCBdel( 4 , i, 8 ),i=1,9)/
     + 3.912e-05, 3.749e-05, 1.642e-05, 5.325e-06, 1.526e-06,
     + 3.975e-07, 9.745e-08, 2.235e-08, 4.724e-09   /
      data (CsCBdel( 0 , i, 8 ),i=1,9)/
     + 0.9154, 0.7984, 0.7195, 0.6202, 0.6319,
     + 0.6121, 0.571, 0.5794, 0.5696   /
      data (CsCBdel( 1 , i, 8 ),i=1,9)/
     + 1.089, 1.079, 1.064, 1.001, 1.008,
     + 0.9975, 0.9718, 0.9775, 0.9695   /
      data (CsCBdel( 2 , i, 8 ),i=1,9)/
     + 0.8455, 0.8439, 0.8883, 0.9071, 0.9025,
     + 0.9105, 0.9213, 0.9188, 0.9192   /
      data (CsCBdel( 3 , i, 8 ),i=1,9)/
     + 0.5493, 0.5267, 0.5759, 0.645, 0.6283,
     + 0.6424, 0.6721, 0.6653, 0.6698   /
      data (CsCBdel( 4 , i, 8 ),i=1,9)/
     + 0.3033, 0.2718, 0.2962, 0.3698, 0.3493,
     + 0.3588, 0.3856, 0.3802, 0.3813   /
      data (CsCBdel( 5 , i, 8 ),i=1,9)/
     + 0.1342, 0.1092, 0.1121, 0.1589, 0.1442,
     + 0.1474, 0.1612, 0.1593, 0.1552   /
      data (CsCBdel( 6 , i, 8 ),i=1,9)/
     + 0.03585, 0.02589, 0.02376, 0.03845, 0.03347,
     + 0.03359, 0.0368, 0.03715, 0.03315   /
      data (BsCBdel( i, 8 ),i=1,9)/
     + 0.006753, 0.004403, 0.002434, 0.001282, 0.0006546,
     + 0.0003271, 0.0001599, 7.58e-05, 3.417e-05   /
      data ZsCBdel(9)/ 14 /
      data (AsCBdel( 1 , i, 9 ),i=1,9)/
     + -0.482, -0.3436, 1.032, 1.099, -0.2834,
     + 0.7271, 0.4975, -0.3009, -0.3203   /
      data (AsCBdel( 2 , i, 9 ),i=1,9)/
     + 0.1315, 0.1377, 0.1022, 0.1591, 0.2496,
     + 0.2229, 0.2438, 0.2875, 0.2946   /
      data (AsCBdel( 3 , i, 9 ),i=1,9)/
     + -0.005324, -0.002923, -0.0008502, -0.000928, -0.001066,
     + -0.0003526, -0.000212, -0.0002344, -0.0001483   /
      data (AsCBdel( 4 , i, 9 ),i=1,9)/
     + 0.0001555, 4.879e-05, 9.499e-06, 4.498e-06, 2.597e-06,
     + 3.532e-07, 1.095e-07, 1.34e-07, 5.275e-08   /
      data (CsCBdel( 0 , i, 9 ),i=1,9)/
     + 0.7947, 0.8286, -1.163, -1.429, 0.6795,
     + -1.002, -0.6834, 0.5116, 0.4764   /
      data (CsCBdel( 1 , i, 9 ),i=1,9)/
     + 0.7724, 1.09, -1.231, -1.651, 1.068,
     + -1.165, -0.7525, 0.7734, 0.7112   /
      data (CsCBdel( 2 , i, 9 ),i=1,9)/
     + 0.5181, 0.8414, -0.8242, -1.192, 0.9573,
     + -0.8474, -0.5102, 0.6779, 0.6173   /
      data (CsCBdel( 3 , i, 9 ),i=1,9)/
     + 0.2907, 0.5252, -0.4605, -0.7067, 0.6767,
     + -0.5094, -0.2811, 0.4676, 0.4236   /
      data (CsCBdel( 4 , i, 9 ),i=1,9)/
     + 0.1401, 0.2746, -0.2163, -0.3463, 0.3866,
     + -0.2545, -0.1267, 0.257, 0.2332   /
      data (CsCBdel( 5 , i, 9 ),i=1,9)/
     + 0.05502, 0.1131, -0.0786, -0.1295, 0.1657,
     + -0.09712, -0.04289, 0.1034, 0.09503   /
      data (CsCBdel( 6 , i, 9 ),i=1,9)/
     + 0.01353, 0.02768, -0.01661, -0.02797, 0.03978,
     + -0.02127, -0.008133, 0.02257, 0.02181   /
      data (BsCBdel( i, 9 ),i=1,9)/
     + 0.009832, 0.005141, 0.002487, 0.001379, 0.0008077,
     + 0.0003422, 0.0001768, 0.0001453, 8.163e-05   /
      data ZsCBdel(10)/ 18 /
      data (AsCBdel( 1 , i, 10 ),i=1,9)/
     + 0.07435, -0.5446, -0.4682, 0.7745, 0.7001,
     + 0.3434, 0.5462, 0.5349, 0.7525   /
      data (AsCBdel( 2 , i, 10 ),i=1,9)/
     + 0.1468, 0.2051, 0.1962, 0.1519, 0.2065,
     + 0.2461, 0.244, 0.2528, 0.2519   /
      data (AsCBdel( 3 , i, 10 ),i=1,9)/
     + -0.0171, -0.009645, -0.004136, -0.001032, -0.001017,
     + -0.0007181, -0.0002647, -0.0001264, -4.787e-05   /
      data (AsCBdel( 4 , i, 10 ),i=1,9)/
     + 0.001165, 0.0003634, 9.998e-05, 2.092e-05, 8.324e-06,
     + 2.704e-06, 4.327e-07, 8.662e-08, 1.365e-08   /
      data (CsCBdel( 0 , i, 10 ),i=1,9)/
     + -0.1127, 0.7818, 0.9303, -0.8353, -0.8852,
     + -0.4207, -0.7605, -0.7908, -1.209   /
      data (CsCBdel( 1 , i, 10 ),i=1,9)/
     + -0.3553, 0.6983, 1.183, -0.8358, -0.9938,
     + -0.4465, -0.8634, -0.901, -1.464   /
      data (CsCBdel( 2 , i, 10 ),i=1,9)/
     + -0.2223, 0.3838, 0.8746, -0.525, -0.7013,
     + -0.3085, -0.6144, -0.6357, -1.093   /
      data (CsCBdel( 3 , i, 10 ),i=1,9)/
     + -0.1378, 0.1706, 0.515, -0.2731, -0.4069,
     + -0.1814, -0.3613, -0.365, -0.661   /
      data (CsCBdel( 4 , i, 10 ),i=1,9)/
     + -0.06122, 0.06301, 0.2496, -0.1187, -0.1946,
     + -0.0904, -0.1764, -0.171, -0.3252   /
      data (CsCBdel( 5 , i, 10 ),i=1,9)/
     + -0.02011, 0.01852, 0.09367, -0.03974, -0.07045,
     + -0.03483, -0.0657, -0.05986, -0.1192   /
      data (CsCBdel( 6 , i, 10 ),i=1,9)/
     + -0.003889, 0.003374, 0.02073, -0.00764, -0.0145,
     + -0.007855, -0.01405, -0.01164, -0.02465   /
      data (BsCBdel( i, 10 ),i=1,9)/
     + 0.02169, 0.01125, 0.005761, 0.002826, 0.001516,
     + 0.0007845, 0.0003452, 0.0001566, 6.648e-05   /
      data ZsCBdel(11)/ 54 /
      data (AsCBdel( 1 , i, 11 ),i=1,9)/
     + 0.2544, 0.004937, 0.4132, 0.6066, 1.275,
     + 1.901, 2.456, 2.576, 2.764   /
      data (AsCBdel( 2 , i, 11 ),i=1,9)/
     + -0.01013, 0.01016, 0.007881, 0.03123, 0.03961,
     + 0.06741, 0.1035, 0.1455, 0.1742   /
      data (AsCBdel( 3 , i, 11 ),i=1,9)/
     + 0.0004744, -3.434e-05, 0.0001231, -5.982e-05, -2.316e-05,
     + -3.843e-05, -4.707e-05, -4.937e-05, -2.956e-05   /
      data (AsCBdel( 4 , i, 11 ),i=1,9)/
     + 8.157e-07, 4.271e-08, 6.323e-08, 8.043e-07, 1.212e-08,
     + 1.6e-08, 1.522e-08, 1.106e-08, 2.676e-09   /
      data (CsCBdel( 0 , i, 11 ),i=1,9)/
     + -0.299, 0.1747, -0.3684, -0.5942, -1.543,
     + -2.5, -3.457, -3.721, -4.118   /
      data (CsCBdel( 1 , i, 11 ),i=1,9)/
     + -0.4626, 0.1589, -0.5238, -0.7772, -1.885,
     + -3.017, -4.248, -4.562, -5.088   /
      data (CsCBdel( 2 , i, 11 ),i=1,9)/
     + -0.2444, 0.3334, -0.2262, -0.5135, -1.412,
     + -2.28, -3.275, -3.508, -3.943   /
      data (CsCBdel( 3 , i, 11 ),i=1,9)/
     + -0.3055, 0.08116, -0.1946, -0.3306, -0.8995,
     + -1.426, -2.084, -2.212, -2.495   /
      data (CsCBdel( 4 , i, 11 ),i=1,9)/
     + -0.04217, 0.1795, -0.07936, -0.178, -0.4912,
     + -0.7426, -1.099, -1.146, -1.288   /
      data (CsCBdel( 5 , i, 11 ),i=1,9)/
     + -0.154, 0.05137, -0.02414, -0.07568, -0.2145,
     + -0.2989, -0.4425, -0.4457, -0.4933   /
      data (CsCBdel( 6 , i, 11 ),i=1,9)/
     + -0.01718, 0.02234, -0.004597, -0.01957, -0.05626,
     + -0.07006, -0.1017, -0.09934, -0.1057   /
      data (BsCBdel( i, 11 ),i=1,9)/
     + 0.009027, 0.001564, 0.002333, 0.001623, 0.0004254,
     + 0.0002607, 0.000166, 0.0001006, 4.482e-05   /

	
	do na=1,pqAt
	
	    if(Zat(na).gt.0)then	! atom is meant initialized

		sign_ACBdel(na)=0	! cleaning

        do n=1,psqAt
                if(ZsCBdel(n).eq.Zat(na))then
c                    write(oo,*)a,z
		    do i=1,4
		      do j=1,qeaCBdel
			ACBdel(i,j,na)=AsCBdel(i,j,n)
		      enddo
		    enddo
		    do i=0,6
		      do j=1,qeaCBdel
		        CCBdel(i,j,na)=CsCBdel(i,j,n)
		      enddo
		    enddo
		      do j=1,qeaCBdel
		        BCBdel(j,na)=BsCBdel(j,n)
		      enddo
		    sign_ACBdel(na)=1	! sign of reading
		    go to 100
		endif	
	enddo
100	continue

	    endif

	enddo

	end

	function fcalcsCBdel(nang,nen,na)
c
c	calculates elastic cross section per one atom by fit formula
c	in Angstrem**2/Srad. (10**-16 sm2 /Srad)
c

	implicit none

	real fcalcsCBdel
	integer nang,nen,na

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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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
c 	include 'part.inc'
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





	real*8 ang,cang,cang2,cang3,cang4,cang5,cang6,s,r
	real*8 coe
c	integer n
        integer i

	ang=ancCBdel(nang)
c	ang=0.0
	cang=cos(ang)
	cang2=cang *cang
	cang3=cang2*cang
	cang4=cang3*cang
	cang5=cang4*cang
	cang6=cang5*cang
	
c	write(oo,*)' A=',(ACBdel(i,nen,na),i=1,4)
c	write(oo,*)' C=',(CCBdel(i,nen,na),i=0,6)
c	write(oo,*)' B=',BCBdel(nen,na)

	    r=0.0
	    do i=1,4
		r=r+ACBdel(i,nen,na) /
     +		(1.0-cang+2.0*dble(BCBdel(nen,na)))**i
c	    write(oo,*)' r=',r
	    enddo
	
	    r=r+dble(CCBdel(0,nen,na))*
     +		1.0
c	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(1,nen,na))*
     +		cang
c	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(2,nen,na))*
     +		0.5*(3.0*cang2-1.0)
c	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(3,nen,na))*
     +		0.5*(5.0*cang3 - 3*cang)
c	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(4,nen,na))*
     +		1.0/8.0 * (35.0*cang4 - 30.0*cang2 + 3.0)
c	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(5,nen,na))*
     +		1.0/8.0 * (63.0*cang5 - 70.0*cang3 + 15.0*cang)
c	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(6,nen,na))*
     +		1.0/16.0 * (231.0*cang6 - 315.0*cang4 + 105.0*cang2 -5.0)
c	    write(oo,*)' r=',r

	    s=r

c	beneath is coefficient from erratum.	
	coe=Zat(na)/(FSCON*FSCON)/(gammaCBdel(nen)*beta2CBdel(nen))

	s=s*coe*coe

	fcalcsCBdel=s	
	
	end

	function fcalcsmCBdel(nang,nen,nm)

	implicit none

	real fcalcsmCBdel
	integer nang,nen,nm

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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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
c 	include 'part.inc'
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





	real*8 ang,cang,cang2,cang3,cang4,cang5,cang6,s,r
	real*8 coe
	integer n,na,i

	ang=ancCBdel(nang)
c	ang=0.0
	cang=cos(ang)
	cang2=cang *cang
	cang3=cang2*cang
	cang4=cang3*cang
	cang5=cang4*cang
	cang6=cang5*cang
	s=0.0
	do n=1,QAtMat(nm)
	    na=AtMat(n,nm)
c	write(oo,*)' A=',(ACBdel(i,nen,na),i=1,4)
c	write(oo,*)' C=',(CCBdel(i,nen,na),i=0,6)
c	write(oo,*)' B=',BCBdel(nen,na)

	    r=0.0
	    do i=1,4
		r=r+ACBdel(i,nen,na) /
     +		(1.0-cang+2.0*dble(BCBdel(nen,na)))**i
	    write(oo,*)' r=',r
	    enddo
	
	    r=r+dble(CCBdel(0,nen,na))*
     +		1.0
	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(1,nen,na))*
     +		cang
	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(2,nen,na))*
     +		0.5*(3.0*cang2-1.0)
	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(3,nen,na))*
     +		0.5*(5.0*cang3 - 3*cang)
	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(4,nen,na))*
     +		1.0/8.0 * (35.0*cang4 - 30.0*cang2 + 3.0)
	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(5,nen,na))*
     +		1.0/8.0 * (63.0*cang5 - 70.0*cang3 + 15.0*cang)
	    write(oo,*)' r=',r
	    r=r+dble(CCBdel(6,nen,na))*
     +		1.0/16.0 * (231.0*cang6 - 315.0*cang4 + 105.0*cang2 -5.0)
	    write(oo,*)' r=',r

	    r=r*WeightAtMat(n,nm)
	    write(oo,*)' r=',r
	    s=s+r

	enddo

	coe=Z_Mean(nm)/(FSCON*FSCON)/(gammaCBdel(nen)*beta2CBdel(nen))

	s=s*coe*coe

	fcalcsmCBdel=s	
	
	end

	
	subroutine SeLossBdel(nm,e,i,el)
c
c	Calculation of the energy loss in 1 sm
c
	implicit none

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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





	integer nm
	real e,el
	integer i,i1	! i is start index i1 is new
	integer n

c	if(e.lt.eMinBdel)then
c		el=0.0
c		i1=0
c		return
c	endif
	if(i.le.0.or.i.gt.qener)then
		i=qener
	endif
c	do n=i,iMinBdel,-1
	do n=i,1,-1
		if(e.ge.ener(n))then
			i1=n
			go to 10
		endif
	enddo
c	write(oo,*)' Error in FeLossBdel'
c	stop
	el=eLossBdel(1,nm)
	i=1
	return
10	continue
	i=i1
	el=eLossBdel(i,nm)+(e-ener(i))*
     +	(eLossBdel(i+1,nm)-eLossBdel(i,nm))/(ener(i+1)-ener(i))
c	write(oo,*)' nm,e,i,el=',nm,e,i,el

	end


	subroutine SstepBdel
c
c	Calc. of step lenght
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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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





c	real pntBdel(3),velBdel(3),step
c	integer nv,sgonext
	integer i
	real*8 mleng
	real lossmean
	real*8 rleng

	real rr,ek,r,ranfl
	integer nm

	if(nVolBdel.eq.0.or.sgonextBdel.eq.1)then	!first find the volume
c		sisferBdel=0	! obsolete
		call VolNumZcoor(pntBdel(3),velBdel(3),nVolBdel)
		if(nVolBdel.eq.0)return		!out of geometry
c		if(sMatC(nMatVol(nVolBdel)).eq.0)return
	endif
c	write(oo,*)' pntBdel(3)=',pntBdel(3)
c	write(oo,*)' velBdel=',velBdel
c	write(oo,*)' nVolBdel=',nVolBdel
c	write(oo,*)' mleng=',mleng
	call VolPathLeng(pntBdel(3),velBdel,nVolBdel,mleng)
	if(nMatVol(nVolBdel).eq.0)then	! empty volume: no interaction
		estepBdel=0.0
		stepBdel=mleng
		sgonextBdel=1
		sturnBdel=0
		go to 10
	endif

	if(eBdel.le.cuteneBdel)then	! the same number in treatdel.f

	nm=nMatVol(nVolBdel)
	ek=eBdel*1000.0
	if(ek.le.10.0)then
        rr=1.0e-3 * A_Mean(nm)/Z_Mean(nm) * 3.872e-3 * ek ** 1.492
        rr=rr/DensMat(nm)
	else
        rr=1.0e-3 * 6.97e-3 * ek ** 1.6
        rr=rr/DensMat(nm)
	endif
c	rr=rr*0.6
	r=ranfl()
c	rr = rr * (0.3 + 0.8*r)	
c	rr = rr * (0.4 + 1.0*r)	
	rr = rr * (0.3 + 0.8*r)	
		stepBdel=rr
		if(stepBdel.lt.mleng)then
			estepBdel=eBdel
			sgonextBdel=0
		else
			estepBdel=eBdel*mleng/stepBdel
			sgonextBdel=1
		endif
		sturnBdel=0
		go to 10
	endif

	call SeLossBdel(nMatVol(nVolBdel),eBdel,iBdel,lossmean)
c	if(nevt.eq.1.and.(nBdel.eq.8.or.nBdel.eq.9))then
c	write(oo,*)' mleng,lossmean=',mleng,lossmean
c	endif
	estepBdel=mleng*lossmean
	stepBdel=mleng
	sgonextBdel=1
	sturnBdel=0
c       if(srandoff.ne.1)then
	if(sruthBdel.eq.1.or.sruthBdel.eq.2)then	!lengt to coulomb interaction
		call SRLengBdel(rleng)
	else
		call SMLengBdel(rleng)
c		rleng=mlamBdel/DensMatDS(nMatVol(nVolBdel))
	endif
	if(stepBdel.gt.rleng)then	!reduce step to point of turn
		stepBdel=rleng
		estepBdel=rleng*lossmean
		sgonextBdel=0
		sturnBdel=1
	endif
c	endif
c	if(nevt.eq.1.and.(nBdel.eq.8.or.nBdel.eq.9))then
c	write(oo,*)' rleng,estepBdel=',rleng,estepBdel
c	endif
	if(estepBdel.gt.eMinBdel)then
	    if(estepBdel.gt.0.1*eBdel)then
			! reduce the step ...
		estepBdel=0.1*eBdel		! Maximum
			!                 but not too much:
		if(estepBdel.lt.eMinBdel)estepBdel=eMinBdel
			! For the case when eBdel<eMinBdel
		if(estepBdel.gt.eBdel)estepBdel=eBdel
		stepBdel=estepBdel/lossmean
		sgonextBdel=0
		if(sruthBdel.eq.1.or.sruthBdel.eq.2)then
				!since step must be reduced
			sturnBdel=0
		else
			sturnBdel=1
		endif
	    endif
	endif
c	if(nevt.eq.1.and.(nBdel.eq.8.or.nBdel.eq.9))then
c	write(oo,*)' estepBdel=',estepBdel
c	endif

c	if(estepBdel.gt.0.1*eBdel)then
c		estepBdel=0.1*eBdel
c		stepBdel=estepBdel/lossmean
c		sgonextBdel=0
c		if(sruthBdel.eq.1)then
c			sturnBdel=0
c		else
c			sturnBdel=1
c		endif
c	endif

10	do i=1,3
		npntBdel(i)=pntBdel(i)+stepBdel*velBdel(i)
	enddo

	if(estepBdel.lt.0.0)then
		write(oo,*)' error in SstepBdel: estepBdel is negative'
		call PriBdel(1)
		write(oo,*)' lossmean=',lossmean
		write(oo,*)' mleng=',mleng,' rleng=',rleng
		stop
	endif

	end
		
	

	subroutine SRLengBdel(rleng)
c
c	Step lenght limit due to multiple scatering
c	The method with Rutherford cross section
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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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


	real*8 rleng
c	real bet2,p2,A,B

c	real asin,acos,sqrt,alog,ranfl
	real ranfl
	real r
c       real mT

        r=ranfl()
        if(r.gt.0.99999)then
                rleng=1.0e30
                return
        endif
        rleng=-lamaBdel(iBdel,nMatVol(nVolBdel))*alog(1.0-r)
	lamBdel=lamaBdel(iBdel,nMatVol(nVolBdel))


	end
	
	
	subroutine SMLengBdel(rleng)
c
c	Step lenght limit due to multiple scatering
c	The method with mean multiple scatering angle form
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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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


	real*8 rleng
c	real bet,p,x
c	real sqrt
c	real msig
c
	rleng=lamaBdel(iBdel,nMatVol(nVolBdel))
*	go to 100
*c		calculate paht lengt from mTetacBdel
*        bet=1.0-ELMAS*ELMAS/((ELMAS+eBdel)*(ELMAS+eBdel))
*        bet=sqrt(bet)
*        p=eBdel*eBdel+2.0*ELMAS*eBdel
*        p=sqrt(p)
*	msig=mTetacBdel
*	x=msig/(sqrt(2.0)*13.6/(bet*p))
*	x=x*x
*
*c	x=x/DensMat(nMatVol(nVolBdel))
*	x=x*RLenMat(nMatVol(nVolBdel))
*	rleng=mlamBdel/DensMat(nMatVol(nVolBdel))
*c	write(oo,*)' x=',x,' rleng=',rleng
*c		reset if it is too large
*	if(rleng.lt.x)rleng=x
*
	end




	subroutine TurnBdel

c	Turn the vector of velocity of the delta electron

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


        real*8 r,rs,rsin12,rcos12
        real*8 x,msig

	real ranfl
c	real ranfl,sqrt,sin,cos,acos
c	real*8 dsqrt
c	real rs,rss
c	integer n,i
	real rra,rrb

	real xran,dran
	integer iran

c	if(sisferBdel.eq.0)then

	if(sruthBdel.eq.2)then

	  if(enerc(iBdel).lt.500.0e-6 .or.
     +		sisferaBdel(iBdel,nMatVol(nVolBdel)).eq.1)then
            sisferBdel=1
            TetaBdel=0.0
	  else
	    sisferBdel=0
	    call lhisran(ismatCBdel(1,iBdel,nMatVol(nVolBdel)),
     +		qanCBdel, 1.0, 1.0, xran)
	    iran=xran	
	    if(iran.lt.1.or.iran.gt.qanCBdel)then
	      write(oo,*)' Worning of TurnBdel: iran=',iran,
     +			' xran=',xran
	      if(iran.lt.1)then
		iran=1
	      else
		iran=qanCBdel
	      endif
	    endif
	    dran=xran-iran
	    TetaBdel=anCBdel(iran)+(anCBdel(iran+1)-anCBdel(iran))*dran
	  endif

	elseif(sruthBdel.eq.1)then

	if(sisferaBdel(iBdel,nMatVol(nVolBdel)).eq.1)then
                sisferBdel=1
                TetaBdel=0.0
	else
c	if(TetacBdel.ge.1.5)then
c		sisferBdel=1
c		TetaBdel=0.0
c	else

		r=ranfl()	
                rsin12=SinTetac12Bdel(iBdel,nMatVol(nVolBdel))
                rcos12=CosTetac12Bdel(iBdel,nMatVol(nVolBdel))
                rs = 1.0 - r * rcos12 * rcos12
                if(rs.eq.0.0)then
                        TetaBdel=PI
                else
                        rs=rsin12 / sqrt( rs )
                        rs=2.0 * asin(rs)
                        TetaBdel=rs
                endif

c	rs=sin(TetacBdel/2.0)/sqrt(1.0-r*cos(TetacBdel/2.0)**2)
c	TetaBdel=asin(rs)*2.0
*	rs=cos(TetacBdel)
*	rs=1.0-(1.0-rs)/(1.0-r*0.5*(1.0+rs))
*	TetaBdel=acos(rs)
c	write(oo,*)' TetacBdel,TetaBdel,r=',TetacBdel,TetaBdel,r

	endif

	else

        x=stepBdel/RLenMat(nMatVol(nVolBdel))
        msig=msigBdel(iBdel)*
     +                  sqrt(x)
        if(msig.ge.1.5)then
                sisferBdel=1
                TetaBdel=0.0
        else
                call lranor(rra,rrb)
                TetaBdel=rra*msig
	endif


	endif		! sruthBdel.eq. ...

	if(sisferBdel.eq.1)then
		call sfersim(velBdel)
	else
		call MakeNewSys(e1Bdel,e2Bdel,e3Bdel,velBdel)
		call turnvec(e1Bdel,e2Bdel,e3Bdel,TetaBdel,velBdel)
	endif

	end



        subroutine correctBdel(e,r)

        implicit none

	real e,r
	real a,b,k,x
c		b-k*(x-a)**2 = 0  =>  x= a +- sqrt(b/k)
c		k = b / (x - a)**2
	a=2.5
	b=4
c	k=1.0/4.0
	x=0.0
	k=b/((x-a)*(x-a))
	x=e*1000.0
	r=b-k*(x-a)*(x-a)
	if(r.lt.0.0)then
		r=1
	else
		r=r+1
	endif

	end	


	subroutine PriBdel(i)

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

c 	include 'ener.inc'
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
c 	include 'atoms.inc'


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
c 	include 'matters.inc'
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





	integer i

	integer ne,nm
	integer nang,nen,na

        if(soo.eq.0)return

	write(oo,*)
	write(oo,*)' PriBdel(',i,'):'
	if(i.eq.0)then
c	write(oo,*)' eMinBdel=',eMinBdel,' iMinBdel=',iMinBdel
	write(oo,*)' eMinBdel=',eMinBdel

	write(oo,*)'   ne,    enerc, betaBdel,',
     +	'  beta2Bdel,momentumBdel,momentum2Bdel,msigBdel'
        do ne=1,qener
        write(oo,'(1X,i5,6(1X,E10.5))')ne,enerc(ne),betaBdel(ne),
     +	beta2Bdel(ne),
     +	momentumBdel(ne),momentum2Bdel(ne),msigBdel(ne)
        enddo

	do nm=1,pQMat
	if(qAtMat(nm).gt.0)then
c	if(sMatC(nm).gt.0)then
	write(oo,*)' matter number ',nm
	write(oo,*)' enerc elossbdel',
     +	' lamaBdel rTetacBdel TetacBdel'
	write(oo,*)'                                   ',
     +	' Cos12TetacBdel Sin12TetacBdel',
     +	' sisferaBdel'

c	do ne=iMinBdel,qener
	do ne=1,qener
	write(oo,'(1X,7(1X,E9.4),1X,I3)')
     +	enerc(ne),eLossBdel(ne,nm),lamaBdel(ne,nm),
     +	rTetacBdel(ne,nm),TetacBdel(ne,nm),
     +	CosTetac12Bdel(ne,nm),SinTetac12Bdel(ne,nm),
     +	sisferaBdel(ne,nm)
	enddo

c	endif
	endif
	enddo

	elseif(i.eq.2)then

	write(oo,*)'        nang    anCBdel         ancCBdel'
	do nang=1,pqanCBdel
	write(oo,*)nang,anCBdel(nang),ancCBdel(nang)
	enddo
	write(oo,*)'         nen    enerCBdel        enercCBdel',
     +	'       gammaCBdel       beta2CBdel'
	do nen=1,pqeaCBdel
	write(oo,*)nen,enerCBdel(nen),enercCBdel(nen),
     +	gammaCBdel(nen), beta2CBdel(nen)
	enddo
	do na=1,pQAt

	if(Zat(na).gt.0)then

	write(oo,*)' atom number ',na
	if(sign_ACBdel(na).gt.0)then
	do i=1,4
	write(oo,'(1X,i1,1X,9E10.3)')i,(ACBdel(i,nen,na),nen=1,qeaCBdel)
	enddo
	do i=0,6
	write(oo,'(1X,i1,1X,9E10.3)')i,(CCBdel(i,nen,na),nen=1,qeaCBdel)
	enddo
	write(oo,'(1X,i1,1X,9E10.3)')i,(BCBdel(nen,na),nen=1,qeaCBdel)
	endif

	write(oo,*)' nang, ancCBdel, differentioal cross sections:'
	do nang=1,qanCBdel
	write(oo,'(1X,i3,1X,10E10.3)')
     +	    nang,ancCBdel(nang),(sCBdel(nang,nen,na),nen=1,qeaCBdel)
	enddo	
	write(oo,*)' nang, ancCBdel, Ruth. differentioal cross sections:'
	write(oo,'(1X,3X,1X,10X,9E10.3)')
     +		(sRcmCBdel(nen,na),nen=1,qeaCBdel)
	write(oo,'(1X,3X,1X,10X,9E10.3)')
     +		(sRmCBdel(nen,na),nen=1,qeaCBdel)
	do nang=1,qanCBdel
	write(oo,'(1X,i3,1X,10E10.3)')
     +	    nang,ancCBdel(nang),(sRCBdel(nang,nen,na),nen=1,qeaCBdel)
	enddo	

	endif	! Zat(na).gt.0
	enddo	! na=1,pQAt

	do nm=1,pQMat
	if(qAtMat(nm).gt.0)then
	write(oo,*)' matter number ',nm
	write(oo,*)' nang, ancCBdel, differentioal cross sections:'
	do nang=1,qanCBdel
	write(oo,'(1X,i3,1X,10E10.3)')
     +	    nang,ancCBdel(nang),(smaCBdel(nang,nen,nm),nen=1,qeaCBdel)
	enddo	
c		smatCBdel and ismatCBdel are not printed now, they is too big.

	write(oo,*)' nen,   enerc, tsmatCBdel,  lamaBdel, ',
     +		' usual range TetacBdel:'
	do nen=1,qener
	write(oo,'(1X,i3,1X,5E11.3)')
     +		nen,enerc(nen),tsmatCBdel(nen,nm),lamaBdel(nen,nm),
     +		rrCBdel(nen,nm),TetacBdel(nen,nm)
	enddo
	write(oo,*)' Beneth is invers order, energy along vertical'
	write(oo,*)' Angles are horizontally:'
	write(oo,'(1X,3X,1X,11X,30E11.3)')(ancCBdel(nang),
     +		nang=1,qanCBdel)
	write(oo,*)' nener,    ener, smatCBdel(nang,nen,nm)'
	do nen=1,qener			! next line fixed to 30 angles
	write(oo,'(1X,i3,1X,31E11.3)')
     +	    nen,enerc(nen),(smatCBdel(nang,nen,nm),nang=1,qanCBdel)
	enddo	
	write(oo,*)' nener,    ener, ismatCBdel(nang,nen,nm)'
	do nen=1,qener			! next line fixed to 30 angles
	write(oo,'(1X,i3,1X,31E11.3)')
     +	    nen,enerc(nen),(ismatCBdel(nang,nen,nm),nang=1,qanCBdel)
	enddo	
c	write(oo,'(5X,9E11.3)')
c     +		(tsmatCBdel(nen,nm),nen=1,qeaCBdel)
c	write(oo,*)' nang, ancCBdel, integrated cross sections:'
c	do nang=1,qanCBdel
c	write(oo,'(1X,i3,1X,10E11.3)')
c     +	    nang,ancCBdel(nang),(ismatCBdel(nang,nen,nm),nen=1,qeaCBdel)
c	enddo	

	endif	! qAtMat(nm).gt.0
	enddo	! nm=1,pQMat

	else	! i=1

	

	write(oo,*)' nBdel=',nBdel,' nstepBdel=',nstepBdel,
     +		' eBdel=',eBdel
	write(oo,*)' pntBdel=',pntBdel
	write(oo,*)' npntBdel=',npntBdel
	write(oo,*)' velBdel=',velBdel
	write(oo,*)' stepBdel=',stepBdel,' estepBdel=',estepBdel
	write(oo,*)' e1Bdel=',e1Bdel
	write(oo,*)' e2Bdel=',e2Bdel
	write(oo,*)' e3Bdel=',e3Bdel
	if(iBdel.ge.1 .and. iBdel.le.qener .and.
     +		nVolBdel.ge.1 .and. nVolBdel.le.qVol)then
	if(nMatVol(nVolBdel).ge.1 .and. nMatVol(nVolBdel).le.pqMat)then
	write(oo,*)' TetacBdel(iBdel,.)=',
     +		    TetacBdel(iBdel,nMatVol(nVolBdel)),
     +		   ' TetaBdel=',TetaBdel,' -usually prev.'
	else
		write(oo,*)' cannot print TetacBdel'
		write(oo,*)' nMatVol(nVolBdel)=',nMatVol(nVolBdel)
	endif
	else
		write(oo,*)' cannot print TetacBdel'
		write(oo,*)' iBdel=',iBdel,' nVolBdel=',nVolBdel
	endif
	write(oo,*)' lamBdel=',lamBdel
	write(oo,*)' sturnBdel=',sturnBdel
	write(oo,*)' sruthBdel=',sruthBdel,' sisferBdel=',sisferBdel
	write(oo,*)' nVolBdel=',nVolBdel,' sgonextBdel=',sgonextBdel,
     +		' iBdel=',iBdel
	endif



	end

	
