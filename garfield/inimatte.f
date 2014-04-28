CDECK  ID>, INIMATTE.

	subroutine IniMatter(num,Atom,Weight,q,dens,pw,pf)
c
c	Initialization of the Matter
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

	integer num,Atom(*),q
	real Weight(*),dens,pw,pf

	integer nat,nsh,nen,i,j
	real rms,rm(pQAt)
	real sw,ph,ph1
	real E,E2,S,EE1,EE2,EP1,EP2


	if(num.le.0.or.num.gt.pQMat)then
		write(oo,*)' Error in IniMatter: Wrong matter number',num
        	if(sret_err.eq.0) stop
        	s_err=1
        	return
	endif
	if(QAtMat(num).gt.0)then
		write(oo,*)' Error in IniMatter: matter number',num,
     +		' is initialized already'
        	if(sret_err.eq.0) stop
        	s_err=1
        	return
	endif
	if(q.le.0)then
		write(oo,*)' Error in IniMatter: empty list of atoms',
     +		' for matter number ',num
        	if(sret_err.eq.0) stop
        	s_err=1
        	return
	endif
	QAtMat(num)=q
 	sw=0.0
	if(q.eq.1)then
		Weight(1)=1.0
	endif
 	do nat=1,q

	    if(Zat(Atom(nat)).le.0)then
		write(oo,*)' Error in IniMatter: Atom number',
     +		nat,' is not initialized'
        	if(sret_err.eq.0) stop
        	s_err=1
        	return
	    endif
	    if(Weight(nat).lt.0.0)then
		write(oo,*)' Error in IniMatter: Weight is negative'
        	if(sret_err.eq.0) stop
        	s_err=1
        	return
	    endif

	    AtMat(nat,num)=Atom(nat)
	    WeightAtMat(nat,num)=Weight(nat)
	    sw=sw+Weight(nat)
	enddo
	
	
	do nat=1,q
	    WeightAtMat(nat,num)=WeightAtMat(nat,num)/sw
	enddo
	A_Mean(num)=0.0
	Z_Mean(num)=0.0
	do nat=1,q
	    A_Mean(num)=A_Mean(num)+Aat(Atom(nat))*WeightAtMat(nat,num)
	    Z_Mean(num)=Z_Mean(num)+Zat(Atom(nat))*WeightAtMat(nat,num)
	enddo

	DensMat(num)=dens

	DensMatDL(num)=DensMat(num)
	DensMatDS(num)=DensMat(num)	! if it is not equal
				! than the multiple scatering of the
				! insident particle will be calculated wrongly
c	DensMatDS(num)=0.2*DensMat(num)

	Pressure(num)=Cur_Pressure	! It is never used, only for printing

	WWW(num)=pw
	FFF(num)=pf


	do nen=1,qener
	    ph=0.0
	    do nat=1,q
		ph1=0.0
		do nsh=1,QShellAt(Atom(nat))
		    ph1=ph1+PhotAt(nen,nsh,Atom(nat))
		enddo
		ph=ph+ph1*WeightAtMat(nat,num)
	    enddo
	    PhotMat(nen,num)=ph	
	enddo

	do nen=1,qener		! the same but with ionization potential
	    ph=0.0
	    do nat=1,q
		ph1=0.0
		do nsh=1,QShellAt(Atom(nat))
		    ph1=ph1+PhotIonAt(nen,nsh,Atom(nat))
		enddo
		ph=ph+ph1*WeightAtMat(nat,num)
	    enddo
	    PhotIonMat(nen,num)=ph	
	enddo

	ElDensMat(num)=Z_Mean(num)/A_Mean(num)*AVOGADRO*DensMat(num)/
     +		((5.07**3)*1.0e30)
	XElDensMat(num)=ElDensMat(num)*5.07e10
	wplaMat(num)=ElDensMat(num)*4.0*PI/(ELMAS*FSCON)				
	
	RLenMat(num)=0.0
        rms=0.0
        do nat=1,QAtMat(num)
        	rms=rms+Aat(AtMat(nat,num))*WeightAtMat(nat,num)
        enddo
        do nat=1,QAtMat(num)
        	rm(nat)=Aat(AtMat(nat,num))*WeightAtMat(nat,num)/rms
        enddo
c	write(oo,*)' rm(1)=',rm(1)
        do nat=1,QAtMat(num)
		RLenMat(num)=RLenMat(num)+rm(nat)/RLenAt(AtMat(nat,num))
	enddo
	RLenMat(num)=1.0/(DensMatDS(num)*RLenMat(num))
c	RLenMat(num)=1.0/RLenMat(num)

	RuthMat(num)=0.0
        do nat=1,QAtMat(num)
		RuthMat(num)=RuthMat(num)+
     +		WeightAtMat(nat,num)*RuthAt(AtMat(nat,num))
	enddo
	RuthMat(num)=RuthMat(num)*DensMatDS(num)*AVOGADRO/A_Mean(num)


	DO nen=1,qener
         epsi2(nen,num)=
     +	(PhotMat(nen,num)/enerc(nen))*ElDensMat(num)/Z_Mean(num)
	enddo

	min_ioniz_pot(num)=1.0e30
        do nat=1,QAtMat(num)
	    do nsh=1,QShellAt(Atom(nat))
		if(min_ioniz_pot(num).gt.ThresholdAt(nsh,Atom(nat)))then
		    min_ioniz_pot(num)=ThresholdAt(nsh,Atom(nat))
		endif
	     enddo
	enddo

	do i=1,qener
            E=ENERC(I)
            E2=E*E
            EPSIP(I,num)=-WPLAMat(num)/E2
	    S=0.0
	    do j=1,qener

          IF(J.NE.I)THEN
            S=S+EPSI2(J,num)*ENERC(J)*(ENER(J+1)-ENER(J))/
     +          (ENERC(J)*ENERC(J)-E2)
          ELSE
            EE1=(ENER(J)+ENERC(J))/2.0
            EE2=(ENER(J+1)+ENERC(J))/2.0
            IF(J.GT.1)THEN
            EP1=EPSI2(J-1,num)+(EE1-ENERC(J-1))*
     +          (EPSI2(J,num)-EPSI2(J-1,num))/
     +		(ENERC(J)-ENERC(J-1))
            ELSE
            EP1=EPSI2(J,num)+(EE1-ENERC(J))*
     +          (EPSI2(J+1,num)-EPSI2(J,num))/
     +		(ENERC(J+1)-ENERC(J))
            END IF
            IF(J.LT.qener)THEN
            EP2=EPSI2(J,num)+(EE2-ENERC(J))*
     +          (EPSI2(J+1,num)-EPSI2(J,num))/
     +		(ENERC(J+1)-ENERC(J))
            ELSE
            EP2=EPSI2(J,num)+(EE2-ENERC(J))*
     +          (EPSI2(J,num)-EPSI2(J-1,num))/
     +		(ENERC(J)-ENERC(J-1))
            END IF
            S=S+EP1*EE1*(ENERC(J)-ENER(J))/
     +          (EE1*EE1-E2)
            S=S+EP2*EE2*(ENER(J+1)-ENERC(J))/
     +          (EE2*EE2-E2)
          END IF                                                  		
	    epsi1(i,num)=(2.0/PI)*S
	enddo
       enddo

	end
