CDECK  ID>, INIGAS.
	subroutine Inigas( nmat, pqmole, pnmole, pwmole, pres, temp)

c
c	initialization of the gas
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


	integer nmat		! Number of material	
        integer pqmole            ! Quantity of different molecules
                                ! in the gas mixture.
        integer pnmole(pqMol)     ! Their numbers in molecdef.inc
                                ! accordingly with molecules.inc
        real pwmole(pqMol)       ! Their weights
                                ! (relative quantities of molecules).
        real pres               ! Pressure in Torr.
        real temp               ! Temperature in K.

        integer qmol, qold
        integer nmol(pqMol)
        real wmol(pqMol)

        integer n
        real s
	integer na,nm,i


        integer A(pqAt)
        real AW(pqAt)
        integer q
        real Ad(pqMol)
        real dens
        real gasdens
	real w
	real f

c	write(oo,*)' nmat=',nmat
c	write(oo,*)' qmol=',qmol
c	do n=1,qmol
c	write(oo,*)nmol(n),pwmol(n)
c	enddo
c	write(oo,*)' temp=',temp
c	write(oo,*)' pres=',pres


c		Copy everything 	
	qmol=pqmole
        do n=1,qmol
	  nmol(n)=pnmole(n)
	  wmol(n)=pwmole(n)
	enddo
        do n=1,qmol	! Check for negative weights
	  if(wmol(n).lt.0)then
	    write(oo,*)' error in Inigas: negative weight: wmol=',
     -           wmol(n)
	    if(sret_err.eq.0) stop
	    s_err=1
	    return
	  endif
	enddo
	s=0.0		! Compute the sun of weights
	do n=1,qmol
	  s=s+wmol(n)
	enddo
	if(s.eq.0)then	! Check zero sum
	  write(oo,*)' error in Inigas: all weights are zero'
	  if(sret_err.eq.0) stop
	  s_err=1
	  return
	endif
	do n=1,qmol	! Normalize the weights
	  wmol(n)=wmol(n)/s
	enddo
*** Remove components with zero weight, rewritten (RV 9/6/99).
       qold=qmol
       qmol=0
       do n=1,qold
       if(wmol(n).gt.0)then
            qmol=qmol+1
            nmol(qmol)=nmol(n)
            wmol(qmol)=wmol(n)
       endif
       enddo
       if(qmol.le.0)then
            print *,' !!!!!! INIGAS WARNING : No non-zero weight'//
     -           ' gas components found; mixture rejected.'
            if(sret_err.eq.0) stop
            s_err=1
            return
       endif
*** End of modification.

	
c	fill material
	q=0
	do n=1,qmol	! Take the next molecule
	  nm=nmol(n)	! Its number in molecdef.inc
c	  write(oo,*)' nm=',nm,' qSAtMol(nm)=',qSAtMol(nm)
c		Check that this molecule exists in list.
	  if(nm.le.0.or.nm.gt.pqMol)then
	    write(oo,*)' error in Inigas: the wrong molecule number'
	    if(sret_err.eq.0) stop
	    s_err=1
	    return
	  endif
	  do na=1,qSAtMol(nm)	! Loop over atoms of current molecule
	    do i=1,q	! Loop over enrolled atoms
			! Check if the atom is already enrolled
	      if(A(i).eq.nAtMol(na,nm))then
		goto 10
	      endif
	    enddo
	    q=q+1	! To enroll it
	    A(q)=nAtMol(na,nm)
	    AW(q)=qAtMol(na,nm) * wmol(n)	! The weight of the atom
c	    write(oo,*)' q=',q,' A(q)=',A(q),' AW(q)=',AW(q)
	    goto 20
10	    continue
	    AW(i)=AW(i) + qAtMol(na,nm) * wmol(n)
c	    write(oo,*)' q=',q,' A(q)=',A(q),' AW(q)=',AW(q)
20	    continue
	  enddo
	enddo	

	do n=1,qmol
	  nm=nmol(n)
	  Ad(n)=weiMol(nm)
	enddo

c	pressure, temperature

	Cur_Pressure=pres
	Cur_Temper=temp

c	density of the ideal gas	
	dens = gasdens(Ad,  wmol, qmol)
	if(s_err.eq.1) return

	w=0.0
	f=0.0
	do n=1,qmol
	  nm=nmol(n)
	  w = w + WWWMol(nm) * wmol(n)
	  f = f + FFFMol(nm) * wmol(n)
	enddo

        call IniMatter(nmat,A,AW,q,dens,w,f)
	if(s_err.eq.1) return

*** Added argument to PriMatter (RV 13/4/99).
c	call PriMatter(0)

	
	
	end
	
		
