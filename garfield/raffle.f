CDECK  ID>, RAFFLE.
	subroutine raffle(nm,x,e)
	
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
	
c 	include 'raffle.inc'
	integer pQGRaf		! Max. quantity of energy transfer
	parameter (pQGRaf=10000)
	integer QGRaf		! Quantity of energy transfers
	integer NAtGRaf,NShAtGRaf ! Numbers of atom and shell
	real ESGRaf,EGRaf	! Cumulative energy and just energy
	real pntraf,velraf
	
	common / craffle /
     +	QGRaf,
     +	ESGRaf,
     +	EGRaf(pQGRaf),
     +	NAtGRaf(pQGRaf),
     +	NShAtGRaf(pQGRaf) ,
     +	pntraf(3,pQGRaf), velraf(3,pQGRaf)


	save / craffle /

	integer nm
	real x
	real e


	integer nshc,n,ierror
	real eran
	real xran,dran
	integer iran
	integer rquan

	QGRaf=0
	e=0.0

	do nshc=1,QShellC(nm)

	    call lspois(quan(nshc,nm)*x,rquan,ierror)
	    if(ierror.ne.0)then
		write(oo,*)' error in raffle: lspois returned ',
     +		'sign of error,'
		write(oo,*)' quan(nshc,nm)*x=',quan(nshc,nm)*x
		write(oo,*)' quan(nshc,nm)=',quan(nshc,nm)
		write(oo,*)' x=',x
		write(oo,*)' nshc=',nshc,' nm=',nm
		stop 'error in poisson'
	    endif

	    do n=1,rquan

		if(QGRaf.eq.pQGRaf)then
		    write(oo,*)' Worning og raffle: too much ',
     +		    ' photons: QGRaf=',QGRaf
		    write(oo,*)' other wiil be ignored'
		    go to 10
		endif

		QGRaf=QGRaf+1

	
		call lhisran(fadda(1,nshc,nm),qener,1.0,1.0,xran)	
	
		iran=xran
		if(iran.lt.1.or.iran.gt.qener)then
		    write(oo,*)' Worning of raffle: iran=',iran,
     +			' xran=',xran
		    if(iran.lt.1)then
		        iran=1
		    else
		        iran=qener
		    endif
		endif
		dran=xran-iran
		eran=ener(iran)+(ener(iran+1)-ener(iran))*dran
c		if(nshc.eq.1)then
c		write(oo,*)' xran,iran,dran=',xran,iran,dran
c		write(oo,*)' ener(iran),ener(iran+1),eran=',
c    +			ener(iran),ener(iran+1),eran
c		endif
		e=e+eran
		EGRaf(QGRaf)=eran
		NAtGRaf(QGRaf)=NAtAC(nshc,nm)
		NShAtGRaf(QGRaf)=NSheC(nshc,nm)

	    enddo

	enddo

10	continue

	ESGraf=e

	end
