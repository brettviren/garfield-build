CDECK  ID>, MOLECDEF.
	subroutine molecdef

	implicit none

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

c 	include 'molecules.inc'
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
c 	include 'molecdef.inc'
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
c 	include 'LibAtMat.inc'
c	Numbers(pointers) of atoms in atom.inc.

c	Since for some of them a special treatment is provided
c	in subroutine Iniatom and this subroutine recognize them by number,
c	the user must not initialize another atoms on these places,
c	even if subroutine AtomsByDefault is not called.
c	Another atoms can be initialized on free places.

	integer num_H
	integer num_H3
	integer num_H4
	integer num_He
	integer num_Li
	integer num_C
	integer num_C1
	integer num_C2
	integer num_C3
c	integer num_C4
	integer num_N
	integer num_O
	integer num_F
	integer num_Ne
	integer num_Al
	integer num_Si
	integer num_Ar
	integer num_Kr
	integer num_Xe
	parameter (num_H  = 1  )
	parameter (num_H3 = 2  )
	parameter (num_H4 = 3  )
	parameter (num_He = 4  )
	parameter (num_Li = 5  )
	parameter (num_C  = 6  )
	parameter (num_N  = 7  )
	parameter (num_O  = 8  )
	parameter (num_F  = 9  )
	parameter (num_Ne  =10 )
	parameter (num_Al = 11 )
	parameter (num_Si = 12 )
	parameter (num_Ar = 13 )
	parameter (num_Kr = 14 )
	parameter (num_Xe = 15 )
	parameter (num_C1  = 16 )	! C in CO2
	parameter (num_C2  = 17 )	! C in CF4
	parameter (num_C3  = 18 )	! C in CH4
*** Additions (RV 20/9/99).
        integer num_S
        parameter (num_S = 19)
*** End of additions.
	
	integer n,na
	real s

c	Mean work per pair production is accordingly with
c	ICRU REPORT 31, Average Energy Required To Produce An Ion Pair, 1979.
	

	qSAtMol( numm_He)=1
	nAtMol(1,numm_He)=num_He
	qAtMol(1,numm_He)=1
	WWWMol(  numm_He)=41.0e-6
	FFFMol(  numm_He)=0.19

	qSAtMol( numm_Ne)=1
	nAtMol(1,numm_Ne)=num_Ne
	qAtMol(1,numm_Ne)=1
	WWWMol(  numm_Ne)=35.4e-6
	FFFMol(  numm_Ne)=0.19

	qSAtMol( numm_Ar)=1
	nAtMol(1,numm_Ar)=num_Ar
	qAtMol(1,numm_Ar)=1
	WWWMol(  numm_Ar)=26.0e-6
	FFFMol(  numm_Ar)=0.19

	qSAtMol( numm_Kr)=1
	nAtMol(1,numm_Kr)=num_Kr
	qAtMol(1,numm_Kr)=1
	WWWMol(  numm_Kr)=24.0e-6
	FFFMol(  numm_Kr)=0.19

	qSAtMol( numm_Xe)=1
	nAtMol(1,numm_Xe)=num_Xe
	qAtMol(1,numm_Xe)=1
	WWWMol(  numm_Xe)=22.0e-6
	FFFMol(  numm_Xe)=0.19

	qSAtMol( numm_H2)=1
	nAtMol(1,numm_H2)=num_H
	qAtMol(1,numm_H2)=2
	WWWMol(  numm_H2)=37.0e-6
	FFFMol(  numm_H2)=0.19

	qSAtMol( numm_N2)=1
	nAtMol(1,numm_N2)=num_N
	qAtMol(1,numm_N2)=2
	WWWMol(  numm_N2)=35.0e-6
	FFFMol(  numm_N2)=0.19

	qSAtMol( numm_O2)=1
	nAtMol(1,numm_O2)=num_O
	qAtMol(1,numm_O2)=2
	WWWMol(  numm_O2)=31.0e-6
	FFFMol(  numm_O2)=0.19

	qSAtMol( numm_NH3)=2
	nAtMol(1,numm_NH3)=num_N
	qAtMol(1,numm_NH3)=1
	nAtMol(2,numm_NH3)=num_H4
	qAtMol(2,numm_NH3)=3
	WWWMol(  numm_NH3)=26.6e-6
	FFFMol(  numm_NH3)=0.19

	qSAtMol( numm_N2O)=2
	nAtMol(1,numm_N2O)=num_N
	qAtMol(1,numm_N2O)=2
	nAtMol(2,numm_N2O)=num_O
	qAtMol(2,numm_N2O)=1
	WWWMol(  numm_N2O)=32.6e-6
	FFFMol(  numm_N2O)=0.19

	qSAtMol( numm_CO2)=2
	nAtMol(1,numm_CO2)=num_C1
	qAtMol(1,numm_CO2)=1
	nAtMol(2,numm_CO2)=num_O
	qAtMol(2,numm_CO2)=2
	WWWMol(  numm_CO2)=33.0e-6
	FFFMol(  numm_CO2)=0.19

	qSAtMol( numm_CF4)=2
	nAtMol(1,numm_CF4)=num_C2
	qAtMol(1,numm_CF4)=1
	nAtMol(2,numm_CF4)=num_F
	qAtMol(2,numm_CF4)=4
	WWWMol(  numm_CF4)=34.3e-6
	FFFMol(  numm_CF4)=0.19

	qSAtMol( numm_CH4)=2
	nAtMol(1,numm_CH4)=num_C3
	qAtMol(1,numm_CH4)=1
	nAtMol(2,numm_CH4)=num_H3
	qAtMol(2,numm_CH4)=4
	WWWMol(  numm_CH4)=27.3e-6
	FFFMol(  numm_CH4)=0.19

	qSAtMol( numm_C2H2)=2
	nAtMol(1,numm_C2H2)=num_C3
	qAtMol(1,numm_C2H2)=2
	nAtMol(2,numm_C2H2)=num_H3
	qAtMol(2,numm_C2H2)=2
	WWWMol(  numm_C2H2)=25.8e-6
	FFFMol(  numm_C2H2)=0.19

	qSAtMol( numm_C2H4)=2
	nAtMol(1,numm_C2H4)=num_C3
	qAtMol(1,numm_C2H4)=2
	nAtMol(2,numm_C2H4)=num_H3
	qAtMol(2,numm_C2H4)=4
	WWWMol(  numm_C2H4)=25.8e-6
	FFFMol(  numm_C2H4)=0.19

	qSAtMol( numm_C2H6)=2
	nAtMol(1,numm_C2H6)=num_C3
	qAtMol(1,numm_C2H6)=2
	nAtMol(2,numm_C2H6)=num_H3
	qAtMol(2,numm_C2H6)=6
	WWWMol(  numm_C2H6)=25.0e-6
	FFFMol(  numm_C2H6)=0.19

	qSAtMol( numm_C3H8)=2
	nAtMol(1,numm_C3H8)=num_C3
	qAtMol(1,numm_C3H8)=3
	nAtMol(2,numm_C3H8)=num_H3
	qAtMol(2,numm_C3H8)=8
	WWWMol(  numm_C3H8)=24.0e-6
	FFFMol(  numm_C3H8)=0.19

	qSAtMol( numm_iC4H10)=2
	nAtMol(1,numm_iC4H10)=num_C3
	qAtMol(1,numm_iC4H10)=4
	nAtMol(2,numm_iC4H10)=num_H3
	qAtMol(2,numm_iC4H10)=10
	WWWMol(  numm_iC4H10)=23.4e-6
	FFFMol(  numm_iC4H10)=0.19

*** Addition (RV 14/1/00).
	qSAtMol( numm_C5H12)=2
	nAtMol(1,numm_C5H12)=num_C3
	qAtMol(1,numm_C5H12)=5
	nAtMol(2,numm_C5H12)=num_H3
	qAtMol(2,numm_C5H12)=12
	WWWMol(  numm_C5H12)=23.2e-6    ! ICRU report 31
	FFFMol(  numm_C5H12)=0.19
*** End of addition.

	qSAtMol( numm_C)=1		! for debug
	nAtMol(1,numm_C)=num_C
	qAtMol(1,numm_C)=1
	WWWMol(  numm_C)=31.0e-6
	FFFMol(  numm_C)=0.19

*** Additions (RV 4/9/98).
	qSAtMol( numm_DME)=3
	nAtMol(1,numm_DME)=num_C3
	qAtMol(1,numm_DME)=2
	nAtMol(2,numm_DME)=num_H
	qAtMol(2,numm_DME)=6
	nAtMol(3,numm_DME)=num_O
	qAtMol(3,numm_DME)=1
	WWWMol(  numm_DME)=45.4e-6
	FFFMol(  numm_DME)=0.19

	qSAtMol( numm_H2O)=2
	nAtMol(1,numm_H2O)=num_H
	qAtMol(1,numm_H2O)=2
	nAtMol(2,numm_H2O)=num_O
	qAtMol(2,numm_H2O)=1
	WWWMol(  numm_H2O)=29.6e-6    !  ICRU 31 (1/5/79)
	FFFMol(  numm_H2O)=0.19

*** Additions (RV 20/9/99).
	qSAtMol( numm_SF6)=2
	nAtMol(1,numm_SF6)=num_S
	qAtMol(1,numm_SF6)=1
	nAtMol(2,numm_SF6)=num_F
	qAtMol(2,numm_SF6)=6
	WWWMol(  numm_SF6)=35.75e-6   ! ICRU 31 (1/5/79)
	FFFMol(  numm_SF6)=0.19

	qSAtMol( numm_C2F4H2)=3
	nAtMol(1,numm_C2F4H2)=num_C3
	qAtMol(1,numm_C2F4H2)=2
	nAtMol(2,numm_C2F4H2)=num_F
	qAtMol(2,numm_C2F4H2)=4
	nAtMol(3,numm_C2F4H2)=num_H
	qAtMol(3,numm_C2F4H2)=2
	WWWMol(  numm_C2F4H2)=24.0e-6 ! Guess
	FFFMol(  numm_C2F4H2)=0.19

	qSAtMol( numm_C2F5H)=3
	nAtMol(1,numm_C2F5H)=num_C3
	qAtMol(1,numm_C2F5H)=2
	nAtMol(2,numm_C2F5H)=num_F
	qAtMol(2,numm_C2F5H)=5
	nAtMol(3,numm_C2F5H)=num_H
	qAtMol(3,numm_C2F5H)=1
	WWWMol(  numm_C2F5H)=24.0e-6 ! Guess
	FFFMol(  numm_C2F5H)=0.19

*** Additions (RV 4/9/01).
        qSAtMol( numm_C3F8)=2
        nAtMol(1,numm_C3F8)=num_C2
        qAtMol(1,numm_C3F8)=3
        nAtMol(2,numm_C3F8)=num_F
        qAtMol(2,numm_C3F8)=8
        WWWMol(  numm_C3F8)=34.3e-6
        FFFMol(  numm_C3F8)=0.19

*** Additions (RV 14/12/07).
        qSAtMol( numm_CS2)=2
        nAtMol(1,numm_CS2)=num_C
        qAtMol(1,numm_CS2)=1
        nAtMol(2,numm_CS2)=num_S
        qAtMol(2,numm_CS2)=2
        WWWMol(  numm_CS2)=19.0e-6 ! From Pawel Majewski
        FFFMol(  numm_CS2)=0.19    ! Idem

*** End of additions.

c	qSAtMol( numm_CClF3)=2
c	nAtMol(1,numm_CClF3)=num_C3
c	qAtMol(1,numm_CClF3)=1
c	nAtMol(1,numm_CClF3)=num_Cl
c	qAtMol(1,numm_CClF3)=1
c	nAtMol(2,numm_CClF3)=num_F
c	qAtMol(2,numm_CClF3)=3
c	WWWMol(  numm_CClF3)=24.0e-6
c	FFFMol(  numm_CClF3)=0.19



	do n=1,pqMol
	  s=0.0
	  do na=1,qSAtMol(n)
	    s=s+Aat(nAtMol(na,n))*qAtMol(na,n)
	  enddo
	  weiMol(n)=s
	enddo

	
	end




	subroutine Primolec

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

c 	include 'molecules.inc'
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
c 	include 'molecdef.inc'
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
c 	include 'LibAtMat.inc'
c	Numbers(pointers) of atoms in atom.inc.

c	Since for some of them a special treatment is provided
c	in subroutine Iniatom and this subroutine recognize them by number,
c	the user must not initialize another atoms on these places,
c	even if subroutine AtomsByDefault is not called.
c	Another atoms can be initialized on free places.

	integer num_H
	integer num_H3
	integer num_H4
	integer num_He
	integer num_Li
	integer num_C
	integer num_C1
	integer num_C2
	integer num_C3
c	integer num_C4
	integer num_N
	integer num_O
	integer num_F
	integer num_Ne
	integer num_Al
	integer num_Si
	integer num_Ar
	integer num_Kr
	integer num_Xe
	parameter (num_H  = 1  )
	parameter (num_H3 = 2  )
	parameter (num_H4 = 3  )
	parameter (num_He = 4  )
	parameter (num_Li = 5  )
	parameter (num_C  = 6  )
	parameter (num_N  = 7  )
	parameter (num_O  = 8  )
	parameter (num_F  = 9  )
	parameter (num_Ne  =10 )
	parameter (num_Al = 11 )
	parameter (num_Si = 12 )
	parameter (num_Ar = 13 )
	parameter (num_Kr = 14 )
	parameter (num_Xe = 15 )
	parameter (num_C1  = 16 )	! C in CO2
	parameter (num_C2  = 17 )	! C in CF4
	parameter (num_C3  = 18 )	! C in CH4
*** Additions (RV 20/9/99).
        integer num_S
        parameter (num_S = 19)
*** End of additions.
	
	integer n,na

	if(soo.eq.0)return

	write(oo,*)
	write(oo,*)' Primolec'
	write(oo,*)' pqMol=',pqMol
	do n=1,pqMol
	write(oo,*)' n=',n,' qSAtMol(n)=',qSAtMol(n)
	write(oo,*)' weiMol=',weiMol(n)
	write(oo,*)' WWWMol=',WWWMol(n)
	write(oo,*)' FFFMol=',FFFMol(n)
	do na=1,qSAtMol(n)
	write(oo,*)' nAtMol=',nAtMol(na,n),' qAtMol=',qAtMol(na,n)
	enddo
	enddo

	end	
	
