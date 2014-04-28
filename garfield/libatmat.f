CDECK  ID>, LIBATMAT.
	subroutine AtomsByDefault
c
c	Initializations of several atoms
c
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
c         include 'LibAtMat.inc'
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

c	integer na

        KeyTeor=0
	QseqAt=0		! It is necessary before run IniAtom
				! ( if memory is not cleaned automatically).
c	do na=1,pQAt
c		num_at_mol(na)=0
c	enddo


        call IniAtom(num_H  ,  1,   1.0	)	! H
        call IniAtom(num_H3 ,  1,   1.0	)	! H in CH4
        call IniAtom(num_H4 ,  1,   1.0	)	! H in NH3
        call IniAtom(num_He ,  2,   4.0	)	! He
        call IniAtom(num_Li ,  3,   6.94)	! Li
        call IniAtom(num_C  ,  6,  12.01)	! C
c	num_at_mol(num_C1)=1	
        call IniAtom(num_C1  ,  6,  12.01)	! C in CO2
c	num_at_mol(num_C2)=2	
        call IniAtom(num_C2  ,  6,  12.01)	! C in CF4
        call IniAtom(num_C3  ,  6,  12.01)	! C in CH4
        call IniAtom(num_N  ,  7,  14.01)	! N
        call IniAtom(num_O  ,  8,  16.0	)	! O
        call IniAtom(num_F  ,  9,  19.0	)	! F
        call IniAtom(num_Ne , 10,  20.2	)	! Ne
        call IniAtom(num_Al , 13,  26.98)	! Al
        call IniAtom(num_Si , 14,  28.09)	! Si
        call IniAtom(num_Ar , 18,  40.0	)	! Ar
        call IniAtom(num_Kr , 36,  84.0	)	! Kr
        call IniAtom(num_Xe , 54, 131.3	)	! Xe	
*** Additions (RV, 20/9/99).
        call IniAtom(num_S  , 16,  32.066)      ! S

	end
