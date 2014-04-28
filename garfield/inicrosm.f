CDECK  ID>, INICROSM.
	Subroutine IniCrosecm(nmat)
c
c	Initialization of ionization cross section for given matter
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


	integer nmat

c	real spa,sio

	integer i
	real*8 r,R0,R1,R2,R3,RR12,RR22
	real*8 s,sa
	integer k

c	real ALOG,SQRT,ATAN
	real fquan,fmean,fmean1
	integer nen,nat,nsh,nshc,ne
c	integer nat0,nat1,iat

c	real spa(pqener)		! sum of photoabsorption
c			! It is luzy to put it to matter.

	real*8 delta,pg,pg2

        complex*16 eeee
        real*8 eee(2)
        equivalence (eeee,eee(1))

c	MatC=nmat

c	ksi=0.1534*DensMat(nmat)*Z_Mean(nmat)/(beta2*A_Mean(nmat))

      DO 100 I=1,qener
        R=-EPSI1(I,nmat)+(1.0+EPSI1(I,nmat))*BETA12
        R=R*R+beta2*beta2*EPSI2(I,nmat)*EPSI2(I,nmat)
        R=1.0/SQRT(R)
        R=DLOG(R)
        LOG1C(I,nmat)=R
100   CONTINUE
C
      DO 200 I=1,qener
        R=2.0*0.511*beta2/ENERC(I)
	if(R.gt.1.0)then
          R=DLOG(R)
	else
	  R=0.0
	endif
        LOG2C(I,nmat)=R
200   continue
c

	
      DO 300 I=1,qener
        R0=1.0+EPSI1(I,nmat)
        R=-EPSI1(I,nmat)+R0*BETA12
        RR12=R0*R0
        RR22=EPSI2(I,nmat)*EPSI2(I,nmat)
        R1=(-R0*R+beta2*RR22)/(RR12+RR22)
        R2=EPSI2(I,nmat)*Beta2/R
        R3=ATAN(R2)
        IF(R.LT.0.0) R3=3.14159+R3

c	R2=R/(EPSI2(I,nmat)*Beta2)	! it is the same as
c				previous three lines but less exactly
c				if EPSI2 --> 0
c	R3=PI/2.0 - ATAN(R2)
	
	chereCangle(I,nmat)=R3
        CHEREC(I,nmat)=(COEFPa/ElDENSMat(nmat))*R1*R3

c	spa=0.0
c	sio=0.0
c
c	do nat=1,QAtMat(nmat)
c	    do nsh=1,QShellAt(AtMat(nat,nmat))
c
c		spa=spa+PhotAt(I,nsh,nat)
c		sio=sio+PhotIonAt(I,nsh,nat)
c
c	    enddo
c	enddo
c	if(spa.gt.0.0)then
c	    CHEREC(I,nmat)=CHEREC(I,nmat)*sio/spa
c	endif
300   continue

c		debug:
c	write(oo,*)' probb'
c	do nen=1,qener
c
c	R=log1C(nen,nmat)*coefPa*PhotIonMat(nen,nmat)
c     +		/(enerc(nen)*Z_Mean(nmat))
c	if(PhotMat(nen,nmat).gt.0.0)then
c	R1= R + PhotIonMat(nen,nmat)/PhotMat(nen,nmat)*CHEREC(nen,nmat)
c	endif
c	r2=r1+log2C(nen,nmat)*coefPa*PhotIonMat(nen,nmat)
c     +          /(enerc(nen)*Z_Mean(nmat))
c	write(oo,'(5E10.3)')enerc(nen),R,CHEREC(nen,nmat),R1,r2
c
c	enddo
c		end debug


	nshc=0
	do 800 nat=1,QAtMat(nmat)
	
	    do 700 nsh=1,QShellAt(AtMat(nat,nmat))

		nshc=nshc+1

		NAtMC(nshc,nmat)=nat
		NAtAC(nshc,nmat)=AtMat(nat,nmat)
		NSheC(nshc,nmat)=nsh	

		do 400 nen=1,qener

		    flog1(nen,nshc,nmat)=
     +		    WeightAtMat(nat,nmat)*log1C(nen,nmat)*coefPa*
     +		    PhotIonAt(nen,nsh,AtMat(nat,nmat))/
     +		    (enerc(nen)*Z_Mean(nmat))

		    flog2(nen,nshc,nmat)=
     +		    WeightAtMat(nat,nmat)*log2C(nen,nmat)*coefPa*
     +		    PhotIonAt(nen,nsh,AtMat(nat,nmat))/
     +		    (enerc(nen)*Z_Mean(nmat))

		    if(PhotMat(nen,nmat).gt.0.0)then

		    cher(nen,nshc,nmat)= chereC(nen,nmat)*
     +			WeightAtMat(nat,nmat)*
     +			PhotIonAt(nen,nsh,AtMat(nat,nmat))/
     +			PhotMat(nen,nmat)
c     +		    WeightAtMat(nat,nmat)*chereC(nen,nmat)*
c     +		    WeightShAt(nsh,AtMat(nat,nmat))

		    else

		    cher(nen,nshc,nmat)=0.0

		    endif

400		continue

		s=0

		do 500 nen=1,qener

		    r=PhotAt(nen,nsh,AtMat(nat,nmat))*WeightAtMat(nat,nmat)*
     +		    (ener(nen+1)-ener(nen))
		    rezer(nen,nshc,nmat)=s+0.5*r
		    if(enerc(nen).gt.MinThresholdAt(AtMat(nat,nmat))
     +			.and.
     +			enerc(nen).lt.emax)then		! kinematical limit
		      if(s_pri_elec.eq.0)then
		    	frezer(nen,nshc,nmat)=(s+0.5*r)*coefPa/
     +			(enerc(nen)*enerc(nen)*Z_Mean(nmat))*
     +			(1.0-beta2*enerc(nen)/emax +
     +			enerc(nen)*enerc(nen)/
     +			(2.0*(tkin+mass)*(tkin+mass)))
		      else
			delta=enerc(nen)/mass
			pg=partgamma
			pg2=pg*pg
		    	frezer(nen,nshc,nmat)=(s+0.5*r)*coefPa/
     +			Z_Mean(nmat) * beta2/mass *
     +			1.0/(pg2-1) *
     +			((pg-1)**2 * pg2 / ((delta*(pg-1-delta))**2)
     +			-
     +			(2*pg2 + 2*pg - 1)/
     +			(delta*(pg-1-delta))
     +			+ 1 )
			
		      endif
		    else
			frezer(nen,nshc,nmat)=0.0
		    endif
		    s=s+r

500		continue

700	    continue

800	continue

	QShellC(nmat)=nshc
	r=0.0

c	add cherenkov radiation to lowest energy level shell
c	nat0=NAtAC(1)
c	iat=1
c	nat1=nat0
c	nsh=NSheC(1)
c	i=1
c850	do nshc=1,QShellC
c	  if(NAtAC(nshc).eq.nat0)then
c	    if(NSheC(nshc).gt.nsh)then
c	      nsh=NSheC(nshc)
c	      i=nshc
c	    endif
c	  else
c	    if(nshc.gt.iat.and.nat1.eq.nat0)then
c	      iat=nshc
c	      nat1=NAtAC(nshc)
c	    endif
c	  endif
c	enddo
c	write(oo,*)' crosec: i,nat0,nat1,nmat,iat='
c	write(oo,*)i,nat0,nat1,nmat,iat
c	if(nat1.gt.nat0)then
c		nat=nat1
c		go to 850
c	endif

c	The cherenkov is added to last shell
c	i=0
c	do nat=1,QAtMat(nmat)
c	i=i+QShellAt(AtMat(nat,nmat))
c	do nen=1,qener
c		    cher(nen,i,nmat)=
c     +		    WeightAtMat(nat,nmat)*chereC(nen,nmat)
cc		    write(oo,*)cher(nen,i),WeightAtMat(nat,nmat),
cc     +			chereC(nen)
c	enddo
c	enddo

	  	
	do 1000 nen=1,qener

	    s=0.0
	    sa=0.0
	    k=0.0

	DO  nshc=1,QShellC(nmat)	
	    ADDA(nen,nshc,nmat)=FLOG1(nen,nshc,nmat)+
     +	    FLOG2(nen,nshc,nmat)+FREZER(nen,nshc,nmat)
c	    ADDA(nen,nshc,nmat)=FLOG1(nen,nshc,nmat)+
c     +	    FLOG2(nen,nshc,nmat)
	    s=s+ADDA(nen,nshc,nmat)
	
	if(enerc(nen).gt.min_ioniz_pot(nmat))then
		ADDA(nen,nshc,nmat)=ADDA(nen,nshc,nmat)+
     +		cher(nen,nshc,nmat)
		if(ADDA(nen,nshc,nmat).lt.0.0)then
		    write(oo,*)' worning of IniCrosec: negative ADDA'
		    write(oo,*)' nmat=',nmat,' nshc=',nshc,' nen=',nen
		    ADDA(nen,nshc,nmat)=0.0
		endif
	endif
		
	enddo

c	if(enerc(nen).gt.min_ioniz_pot(nmat))then
c	    if(s.lt.-chereC(nen,nmat))then
c		DO  nshc=1,QShellC(nmat)
c		    ADDA(nen,nshc,nmat)=0.0
c		enddo
c	    else
c	    	s=1.0+chereC(nen,nmat)/s
c		DO  nshc=1,QShellC(nmat)
c		    ADDA(nen,nshc,nmat)=ADDA(nen,nshc,nmat)*s
c		enddo
c	    endif
c	endif
	    	
	s=0.0
	DO  nshc=1,QShellC(nmat)
	    s=s+ADDA(nen,nshc,nmat)
	enddo
	ADDAC(nen,nmat)=s

	
c       DO 900 nshc=1,QShellC(nmat)
c          R=FLOG1(nen,nshc,nmat)+FLOG2(nen,nshc,nmat)+
c     +		FREZER(nen,nshc,nmat)
cc         IF(CHER(nen,nshc).LT.0.0)THEN
c            R=R+CHER(nen,nshc,nmat)
cc         END IF
c          IF(R.LT.0.0)THEN
c            K=1
c            SA=SA+R
c          ELSE
c            S=S+R
c	  ENDIF
c          ADDA(nen,nshc,nmat)=R
c900    ADDAC(nen,nmat)=ADDAC(nen,nmat)+ADDA(nen,nshc,nmat)
c
c        IF(K.EQ.1)THEN
c          IF(ABS(SA).LT.S)THEN
c            DO 906 nshc=1,QShellC(nmat)
c              IF(ADDA(nen,nshc,nmat).GT.0.0)THEN
c                ADDA(nen,nshc,nmat)=ADDA(nen,nshc,nmat)*(1.0+SA/S)
c              ELSE
c                ADDA(nen,nshc,nmat)=0.0
c              END IF
c906         CONTINUE
c          ELSE
c            DO 907 nshc=1,QShellC(nmat)
c                ADDA(nen,nshc,nmat)=0.0
c907         CONTINUE
c            ADDAC(nen,nmat)=0.0
c          END IF
c        END IF

1000	continue

       DO  nshc=1,QShellC(nmat)

	    do nen=1,qener
		fadda(nen,nshc,nmat)=adda(nen,nshc,nmat)*
     +			(ener(nen+1)-ener(nen))
	    enddo

	    call lhispre(fadda(1,nshc,nmat),qener)

	enddo

	quanC(nmat)=fquan(addaC(1,nmat),1.0,nmat)
	meanC(nmat)=fmean(addaC(1,nmat),1.0,nmat)

	if(s_pri_elec.eq.0)then
	meanC1(nmat)=fmean1(addaC(1,nmat),1.0,nmat)
	else
	meanC1(nmat)=0.0	! for electrons it is not calculated
	endif

	meaneleC(nmat)=meanC(nmat)/WWW(nmat)
	meaneleC1(nmat)=meanC1(nmat)/WWW(nmat)

	do nshc=1,QShellC(nmat)
c	    quan(nshc)=fquan(adda(1,nshc,nmat),
c     +		WeightAtMat(NAtMC(nshc),nmat),nmat)
c	    mean(nshc)=fmean(adda(1,nshc,nmat),
c     +WeightAtMat(NAtMC(nshc),nmat),nmat)
	    quan(nshc,nmat)=fquan(adda(1,nshc,nmat),1.0,nmat)
	    mean(nshc,nmat)=fmean(adda(1,nshc,nmat),1.0,nmat)
	enddo

        do ne=1,qener
        eee(1)=dble(1.)+dble(epsi1(ne,nmat))
        eee(2)=dble(epsi2(ne,nmat))
c	write(oo,*)enerc(ne),eeee
        eeee=beta2*eeee - 1.0
c	write(oo,*)enerc(ne),eeee
        eeee=sqrt(eeee)
c	write(oo,*)enerc(ne),eeee
        eeee=enerc(ne)/sqrt(beta2) * eeee
c	write(oo,*)enerc(ne),eeee
        pocaz(ne,nmat)=eeee * 5.07e10
c	write(oo,*)enerc(ne),pocaz(ne,nmat)
        enddo


	end
