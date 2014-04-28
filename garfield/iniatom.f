CDECK  ID>, INIATOM.


	subroutine IniAtom(num,z,a)
c
c	The special cases incorporated by fortran code:
c	Ar and O : with including exp. data
c	and change of part of 3p and 2p shell corespondently.
c	C for CO2 (C1) : 2p sift from 8.9 to 13.79
c	C for CF4 (C2) : 2p sift from 8.9 to 16.23
c	C for CH4 : 2p sift
c	c for C2H10 : 2p sift
c
	implicit none

	save

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
c 	include 'shellfi.inc'
	integer pqash		! Max. q. of shells
	parameter (pqash=7)
	integer zato		! Z of atom
	integer qash		! quantity of shells
	real athreshold,aweight	! threshold and weight of shells
	integer pqaener,qaener	! Max. and just q. of shell energy
	parameter (pqaener=500)
	real aener		! Energy
	real aphot		! Photoabsorbtion crossection
			! for this point of energy
	common / cshellfi /
     +		zato,
     +		qash,
     +		athreshold(pqash),aweight(pqash),
     +		qaener(pqash),
     +		aener(pqaener,pqash),aphot(pqaener,pqash)
	save / cshellfi /
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

c 	include 'shl.inc'
	integer pqschl,pqshl,pqatm,pqsel,pqsga
	parameter (pqschl=3)	! Max. q. of channels
	parameter (pqshl=7)	! Max. q. of shells
	parameter (pqatm=20)	! Max. q. of atoms
	parameter (pqsel=3)	! Max. q. of secondary electrons in
				! one channel
	parameter (pqsga=3)	! Max. q. of secondary photons in
				! one channel
	integer qschl,qshl,qatm,qsel,qsga
	real charge	! charge of atom
	real eshell	! energy of shells
			! The distanse must be bigger the
			! threshold in the atom.inc
			! if secondary photons is generated
	real secprobch	! Probubility function for channels
			! Attention!!! - Probubility function
			! i.e. last channel prob must be 1
	real secenel	! Energies of secondary electrons
	real secenga	! Energies of secondary photons
	common / comshl /
     +	charge(pqatm),
     +	qschl(pqshl,pqatm),qshl(pqatm),qatm,
     +	qsel(pqschl,pqshl,pqatm),qsga(pqschl,pqshl,pqatm),
     +	eshell(pqshl,pqatm),secprobch(pqschl,pqshl,pqatm),
     +	secenel(pqsel,pqschl,pqshl,pqatm),
     +	secenga(pqsga,pqschl,pqshl,pqatm)
	save / comshl /
c 	include 'tpasc.inc'
	integer pqshPas
	parameter (pqshPas=5)
	integer qshPas
	integer lPas
	real E0Pas,EthPas,ywPas,yaPas,PPas,sigma0Pas
	common / Pascom /
     +	qshPas(pQAt),
     +	lPas(pqshPas,pQAt),
     +	E0Pas(pqshPas,pQAt),EthPas(pqshPas,pQAt),ywPas(pqshPas,pQAt),
     +	yaPas(pqshPas,pQAt),PPas(pqshPas,pQAt),sigma0Pas(pqshPas,pQAt)
	save / Pascom /


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

	integer num	!number of atom in the bank
	integer z	!charge
	real a		!atomic weight

	real w,sw,s

	integer qbener
        parameter (qbener=138)
        real aenerc(qbener),epa(qbener)
	integer qbener1
        parameter (qbener1=5)
        real aenerc1(qbener1),epa1(qbener1)
	real e
c	integer num_at_mol
c	parameter (num_at_mol=2)
	real interp_linep_arr

*** Declarations moved ahead of data statements (RV 26/10/2007).
        integer pqnpasc
        parameter(pqnpasc=20)
        integer nnpasc
        integer pqnene
        parameter(pqnene=100)
        integer nnene
        real Tresh_npasc
        real nene,npasc

        common / comasc /
     +          nnpasc,Tresh_npasc(pqnpasc),nnene(pqnpasc),
     +          nene(pqnene,pqnpasc),npasc(pqnene,pqnpasc)
        save / comasc /
	

	integer i,iener,n,ne,j,ns,k,nn
c       integer ios
	real glin_integ_ar, step_integ_ar, sigma_nl
c       real lin_integ_ar
c	real interp_line_arr
c	real alog,sqrt
*** End of modification.

c	include 'shellescar.inc'
	data aenerc(1) / 15.83 /
	data epa(1) / 29.2 /
	data aenerc(2) / 15.89 /
	data epa(2) / 29.5 /
	data aenerc(3) / 16.1 /
	data epa(3) / 30.3 /
	data aenerc(4) / 16.31 /
	data epa(4) / 31.1 /
	data aenerc(5) / 16.53 /
	data epa(5) / 31.8 /
	data aenerc(6) / 16.75 /
	data epa(6) / 32.5 /
	data aenerc(7) / 16.98 /
	data epa(7) / 33.1 /
	data aenerc(8) / 17.22 /
	data epa(8) / 33.7 /
	data aenerc(9) / 17.46 /
	data epa(9) / 34.2 /
	data aenerc(10) / 17.71 /
	data epa(10) / 34.7 /
	data aenerc(11) / 17.97 /
	data epa(11) / 35.1 /
	data aenerc(12) / 18.23 /
	data epa(12) / 35.5 /
	data aenerc(13) / 18.5 /
	data epa(13) / 35.8 /
	data aenerc(14) / 18.78 /
	data epa(14) / 36.1 /
	data aenerc(15) / 19.07 /
	data epa(15) / 36.3 /
	data aenerc(16) / 19.37 /
	data epa(16) / 36.5 /
	data aenerc(17) / 19.68 /
	data epa(17) / 36.3 /
	data aenerc(18) / 20 /
	data epa(18) / 36.7 /
	data aenerc(19) / 20.32 /
	data epa(19) / 36.8 /
	data aenerc(20) / 20.66 /
	data epa(20) / 36.7 /
	data aenerc(21) / 21.01 /
	data epa(21) / 36.7 /
	data aenerc(22) / 21.38 /
	data epa(22) / 36.5 /
	data aenerc(23) / 21.75 /
	data epa(23) / 36.3 /
	data aenerc(24) / 22.14 /
	data epa(24) / 36.1 /
	data aenerc(25) / 22.54 /
	data epa(25) / 35.7 /
	data aenerc(26) / 22.96 /
	data epa(26) / 35.4 /
	data aenerc(27) / 23.39 /
	data epa(27) / 34.9 /
	data aenerc(28) / 23.84 /
	data epa(28) / 34.4 /
	data aenerc(29) / 24.31 /
	data epa(29) / 33.8 /
	data aenerc(30) / 24.8 /
	data epa(30) / 33.1 /
	data aenerc(31) / 25.3 /
	data epa(31) / 32.3 /
	data aenerc(32) / 25.83 /
	data epa(32) / 31.4 /
	data aenerc(33) / 26.38 /
	data epa(33) / 30.5 /
	data aenerc(34) / 26.95 /
	data epa(34) / 29.5 /
	data aenerc(35) / 27.55 /
	data epa(35) / 28.3 /
	data aenerc(36) / 28.18 /
	data epa(36) / 27.1 /
	data aenerc(37) / 28.83 /
	data epa(37) / 25.7 /
	data aenerc(38) / 29.52 /
	data epa(38) / 24.3 /
	data aenerc(39) / 30.24 /
	data epa(39) / 22.7 /
	data aenerc(40) / 30.99 /
	data epa(40) / 21 /
	data aenerc(41) / 31.79 /
	data epa(41) / 19.1 /
	data aenerc(42) / 32.63 /
	data epa(42) / 17.1 /
	data aenerc(43) / 33.51 /
	data epa(43) / 15 /
	data aenerc(44) / 34.44 /
	data epa(44) / 12.8 /
	data aenerc(45) / 35.42 /
	data epa(45) / 10.3 /
	data aenerc(46) / 36.46 /
	data epa(46) / 7.77 /
	data aenerc(47) / 37.57 /
	data epa(47) / 6.1 /
	data aenerc(48) / 38.74 /
	data epa(48) / 4.62 /
	data aenerc(49) / 39.99 /
	data epa(49) / 3.41 /
	data aenerc(50) / 41.33 /
	data epa(50) / 2.47 /
	data aenerc(51) / 42.75 /
	data epa(51) / 1.77 /
	data aenerc(52) / 44.28 /
	data epa(52) / 1.3 /
	data aenerc(53) / 45.92 /
	data epa(53) / 1.03 /
	data aenerc(54) / 47.68 /
	data epa(54) / 0.914 /
	data aenerc(55) / 49.59 /
	data epa(55) / 0.916 /
	data aenerc(56) / 51.66 /
	data epa(56) / 1 /
	data aenerc(57) / 53.9 /
	data epa(57) / 1.13 /
	data aenerc(58) / 56.35 /
	data epa(58) / 1.28 /
	data aenerc(59) / 59.04 /
	data epa(59) / 1.36 /
	data aenerc(60) / 61.99 /
	data epa(60) / 1.42 /
	data aenerc(61) / 65.25 /
	data epa(61) / 1.45 /
	data aenerc(62) / 68.88 /
	data epa(62) / 1.48 /
	data aenerc(63) / 72.93 /
	data epa(63) / 1.48 /
	data aenerc(64) / 77.49 /
	data epa(64) / 1.47 /
	data aenerc(65) / 82.65 /
	data epa(65) / 1.45 /
	data aenerc(66) / 88.56 /
	data epa(66) / 1.41 /
	data aenerc(67) / 95.37 /
	data epa(67) / 1.36 /
	data aenerc(68) / 103.3 /
	data epa(68) / 1.29 /
	data aenerc(69) / 112.7 /
	data epa(69) / 1.2 /
	data aenerc(70) / 124 /
	data epa(70) / 1.1 /
	data aenerc(71) / 130.5 /
	data epa(71) / 1.05 /
	data aenerc(72) / 137.8 /
	data epa(72) / 0.987 /
	data aenerc(73) / 145.9 /
	data epa(73) / 0.923 /
	data aenerc(74) / 155 /
	data epa(74) / 0.856 /
	data aenerc(75) / 165.3 /
	data epa(75) / 0.785 /
	data aenerc(76) / 177.1 /
	data epa(76) / 0.709 /
	data aenerc(77) / 190.7 /
	data epa(77) / 0.63 /
	data aenerc(78) / 206.6 /
	data epa(78) / 0.547 /
	data aenerc(79) / 225.4 /
	data epa(79) / 0.461 /
	data aenerc(80) / 245 /
	data epa(80) / 0.381 /
	data aenerc(81) / 248 /
	data epa(81) / 4.66 /
	data aenerc(82) / 258.3 /
	data epa(82) / 4.23 /
	data aenerc(83) / 269.5 /
	data epa(83) / 3.83 /
	data aenerc(84) / 281.8 /
	data epa(84) / 3.45 /
	data aenerc(85) / 295.2 /
	data epa(85) / 3.1 /
	data aenerc(86) / 310 /
	data epa(86) / 2.76 /
	data aenerc(87) / 326.3 /
	data epa(87) / 2.45 /
	data aenerc(88) / 344.4 /
	data epa(88) / 2.16 /
	data aenerc(89) / 364.7 /
	data epa(89) / 1.89 /
	data aenerc(90) / 387.4 /
	data epa(90) / 1.64 /
	data aenerc(91) / 413.3 /
	data epa(91) / 1.41 /
	data aenerc(92) / 442.8 /
	data epa(92) / 1.2 /
	data aenerc(93) / 476.9 /
	data epa(93) / 1.01 /
	data aenerc(94) / 516.6 /
	data epa(94) / 0.836 /
	data aenerc(95) / 563.6 /
	data epa(95) / 0.682 /
	data aenerc(96) / 619.9 /
	data epa(96) / 0.546 /
	data aenerc(97) / 652.5 /
	data epa(97) / 0.484 /
	data aenerc(98) / 688.8 /
	data epa(98) / 0.426 /
	data aenerc(99) / 729.3 /
	data epa(99) / 0.373 /
	data aenerc(100) / 774.9 /
	data epa(100) / 0.324 /
	data aenerc(101) / 826.5 /
	data epa(101) / 0.278 /
	data aenerc(102) / 885.6 /
	data epa(102) / 0.237 /
	data aenerc(103) / 953.7 /
	data epa(103) / 0.199 /
	data aenerc(104) / 1044 /
	data epa(104) / 0.165 /
	data aenerc(105) / 1127 /
	data epa(105) / 0.135 /
	data aenerc(106) / 1240 /
	data epa(106) / 0.108 /
	data aenerc(107) / 1305 /
	data epa(107) / 0.0955 /
	data aenerc(108) / 1378 /
	data epa(108) / 0.0842 /
	data aenerc(109) / 1459 /
	data epa(109) / 0.0736 /
	data aenerc(110) / 1550 /
	data epa(110) / 0.0639 /
	data aenerc(111) / 1653 /
	data epa(111) / 0.0549 /
	data aenerc(112) / 1771 /
	data epa(112) / 0.0467 /
	data aenerc(113) / 1907 /
	data epa(113) / 0.0393 /
	data aenerc(114) / 2066 /
	data epa(114) / 0.0326 /
	data aenerc(115) / 2254 /
	data epa(115) / 0.0266 /
	data aenerc(116) / 2480 /
	data epa(116) / 0.0213 /
	data aenerc(117) / 2755 /
	data epa(117) / 0.0166 /
	data aenerc(118) / 3100 /
	data epa(118) / 0.0126 /
	data aenerc(119) / 3204 /
	data epa(119) / 0.0117 /
	data aenerc(120) / 3263 /
	data epa(120) / 0.0959 /
	data aenerc(121) / 3444 /
	data epa(121) / 0.0827 /
	data aenerc(122) / 3646 /
	data epa(122) / 0.0706 /
	data aenerc(123) / 3874 /
	data epa(123) / 0.0598 /
	data aenerc(124) / 4133 /
	data epa(124) / 0.0501 /
	data aenerc(125) / 4428 /
	data epa(125) / 0.0414 /
	data aenerc(126) / 4768 /
	data epa(126) / 0.0338 /
	data aenerc(127) / 5166 /
	data epa(127) / 0.0271 /
	data aenerc(128) / 5635 /
	data epa(128) / 0.0213 /
	data aenerc(129) / 6199 /
	data epa(129) / 0.0164 /
	data aenerc(130) / 6888 /
	data epa(130) / 0.0123 /
	data aenerc(131) / 7749 /
	data epa(131) / 0.00889 /
	data aenerc(132) / 8856 /
	data epa(132) / 0.00616 /
	data aenerc(133) / 10330 /
	data epa(133) / 0.00403 /
	data aenerc(134) / 12400 /
	data epa(134) / 0.00244 /
	data aenerc(135) / 15500 /
	data epa(135) / 0.00132 /
	data aenerc(136) / 20660 /
	data epa(136) / 0.000599 /
	data aenerc(137) / 31000 /
	data epa(137) / 0.000196 /
	data aenerc(138) / 61990 /
	data epa(138) / 2.9e-05 /



c	include 'shellesco.inc'

	data aenerc1(1) / 14.2 /
	data epa1(1) / 2.51 /
	data aenerc1(2) / 16.2 /
	data epa1(2) / 3.98 /
	data aenerc1(3) / 17.4 /
	data epa1(3) / 12.59 /
	data aenerc1(4) / 25.1 /
	data epa1(4) / 10.72 /
	data aenerc1(5) / 31.6 /
	data epa1(5) / 10 /

	if(num.le.0.or.num.gt.pQAt)then
		write(oo,*)' Error in IniAtom: Wrong Atom number ',num
		stop
	endif
	if(Zat(num).ne.0)then
		write(oo,*)' Error in IniAtom: Atom number ',num,
     +		'is initialized already'
		stop
	endif
	do n=1,QseqAt			! fill sequensed number
	  if(Zat(n).gt.z)then
	    do nn=QseqAt,n,-1
	      nseqAt(nn+1)=nseqAt(nn)
	    enddo
	    nseqAt(n)=num
	    QseqAt=QseqAt+1
	    go to 4
	  endif
	enddo
	QseqAt=QseqAt+1
	nseqAt(QseqAt)=num
4	continue
	Zat(num)=z
	Aat(num)=a
	cphoAt(num)=2.0*PI2*Zat(num)/(FSCON*ELMAS)		
	RLenAt(num)=716.4*Aat(num)/
     +		(Zat(num)*(Zat(num)+1)*alog(287/sqrt(float(Zat(num)))))
	RuthAt(num)=4.0*PI*Zat(num)*Zat(num)*ELRAD*ELRAD*ELMAS*ELMAS
	zato=zat(num)
	if(KeyTeor.eq.0)then

	    if(Zat(num).eq.1)then	! H

	QShellAt(num)=1
	ThresholdAt(1,num)=16.4e-6	! ionization potential of H2
c		accordingly with At.Data.Nucl.Data.Tables 24,323-371(1979)
	if(num.eq.num_H3)then   ! for CH4
c		ThresholdAt(1,num)=15.2e-06
		ThresholdAt(1,num)=12.0e-06
	endif
	if(num.eq.num_H4)then   ! for NH4
		ThresholdAt(1,num)=10.0e-06
	endif
	do ne=1,qener
	    if(ener(ne+1).gt.ThresholdAt(1,num))then
c		PhotAt(ne,1,num)=1.51*0.0535*
		PhotAt(ne,1,num)=0.0535*
     +		((100.0e-6/
     +		(enerc(ne) + 16.4e-6 - ThresholdAt(1,num)))**3.228)
	        if(ener(ne).lt.ThresholdAt(1,num))then
		    PhotAt(ne,1,num)=PhotAt(ne,1,num)*
     +			(ThresholdAt(1,num)-ener(ne))/
     +			(ener(ne+1)-ener(ne))
		endif	
	    endif
	enddo

c       Now the cross section is generated in Mega-barns.
c       Calc. coef for going from 10**-18 sm**2 to Mev-2
        s=1.e-18 * 5.07e10 * 5.07e10

        do ne=1,qener
        do ns=1,QShellAt(num)
                PhotAt(ne,ns,num)=PhotAt(ne,ns,num)*s
        enddo
        enddo

        do ns=1,QShellAt(num)
        WeightShAt(ns,num)=step_integ_ar(ener,PhotAt(1,ns,num),qener,
     +          ener(1),ener(qener+1))/cphoAt(num)
        enddo


        go to 100

	    endif

	    if(Zat(num).eq.6)then

	call henke

	QShellAt(num)=qash
	do ns=1,QShellAt(num)
	    ThresholdAt(ns,num)=athreshold(ns)
	    if(ns.eq.QShellAt(num))then
		if(num.eq.num_C1)then
		    ThresholdAt(ns,num)=13.79e-6	! CO2
		endif
		if(num.eq.num_C2)then
		    ThresholdAt(ns,num)=16.23e-6	! CF4
		endif
		if(num.eq.num_C3)then
c		    ThresholdAt(ns,num)=15.2e-6 	! CH4
		    ThresholdAt(ns,num)=12.0e-6 	! CH4 and so on
		endif
	    endif	
	    do ne=1,qener
		PhotAt(ne,ns,num)=
     +		interp_linep_arr(aener(1,ns),aphot(1,ns),qaener(ns),
     +		athreshold(ns),
     +		(enerc(ne) - (ThresholdAt(ns,num) - athreshold(ns))) )
	    enddo
	enddo	

c       Now the cross section is generated in Mega-barns.
c       Calc. coef for going from 10**-18 sm**2 to Mev-2
        s=1.e-18 * 5.07e10 * 5.07e10

        do ne=1,qener
        do ns=1,QShellAt(num)
                PhotAt(ne,ns,num)=PhotAt(ne,ns,num)*s
        enddo
        enddo

        do ns=1,QShellAt(num)
        WeightShAt(ns,num)=step_integ_ar(ener,PhotAt(1,ns,num),qener,
     +          ener(1),ener(qener+1))/cphoAt(num)
        enddo


        go to 100

	    endif

		
	    qshPas(num)=0
	    call readPas(num)
	    if(qshPas(num).gt.0)then


	
	QShellAt(num)=qshPas(num)
	do ns=1,qshPas(num)
		ThresholdAt(ns,num)=EthPas(ns,num)*1.e-6
		if(Zat(num).eq.6.and.ns.eq.3.and.
     +		num.eq.num_C1)then
c     +		num_at_mol(num).eq.1)then
		ThresholdAt(ns,num)=13.79*1.e-6		! for CO2
		endif		
		if(Zat(num).eq.6.and.ns.eq.3.and.
     +		num.eq.num_C2)then
c     +		num_at_mol(num).eq.2)then
		ThresholdAt(ns,num)=16.23*1.e-6		! for CF4
		endif
		if(Zat(num).eq.6.and.ns.eq.3.and.
     +		num.eq.num_C3)then
		ThresholdAt(ns,num)=15.2*1.e-6		! for CH4
		endif
		if(ThresholdAt(ns,num).lt.ener(1))then
			write(oo,*)' error in IniAtom:'
			write(oo,*)' too high ener(1)=',ener(1)
			write(oo,*)' ThresholdAt(ns,num)=',
     +				ThresholdAt(ns,num)
			stop
		endif
			
	enddo



        do ne=1,qener
        do i=1,qshPas(num)
        s=0.0
c       do i=5,5
        if(Zat(num).eq.18.and.
     +	i.eq.5.and.
     +	enerc(ne)*1.e6.gt.EthPas(i,num).and.enerc(ne)*1.e6.le.40)then
        j=qbener
        do k=2,qbener
                if(aenerc(k).ge.enerc(ne)*1.e6)then
                        j=k-1
                        go to 5
                endif
        enddo
5       s=s+ epa(j)+(enerc(ne)*1.e6-aenerc(j))*
     + (epa(j+1)-epa(j))/(aenerc(j+1)-aenerc(j))

        elseif(Zat(num).eq.8.and.
     +	i.eq.3.and.
     +	enerc(ne)*1.e6.gt.EthPas(i,num).and.enerc(ne)*1.e6.le.25.1)then
        j=qbener1
        do k=2,qbener1
                if(aenerc1(k).ge.enerc(ne)*1.e6)then
                        j=k-1
                        go to 6
                endif
        enddo
6       s=s+ epa1(j)+(enerc(ne)*1.e6-aenerc1(j))*
     + (epa1(j+1)-epa1(j))/(aenerc1(j+1)-aenerc1(j))

        else
	if(Zat(num).eq.6.and.i.eq.3)then
c	if(num.eq.num_C1)then
cc	if(num_at_mol(num).eq.1)then
c		e=enerc(ne)*1.e6-(13.79-.8987E+01)
c	elseif(num.eq.num_C2)then
cc	elseif(num_at_mol(num).eq.2)then
c		e=enerc(ne)*1.e6-(16.23-.8987E+01)
c	else
c		e=enerc(ne)*1.e6
c	endif
	e=enerc(ne) - ThresholdAt(i,num) + .8987E+01*1.0e-6
	e=e*1.e6
	else
		e=enerc(ne)*1.e6
	endif

        s=s + sigma_nl
     +  (e , E0Pas(i,num),EthPas(i,num),
     +		ywPas(i,num),lPas(i,num),
     +  	yaPas(i,num),PPas(i,num),sigma0Pas(i,num))


        endif

	PhotAt(ne,i,num)=s

	enddo	
	enddo	

c	Now the cross section is generated in Mega-barns.
c	Calc. coef for going from 10**-18 sm**2 to Mev-2
	s=1.e-18 * 5.07e10 * 5.07e10

        do ne=1,qener
        do i=1,qshPas(num)
		PhotAt(ne,i,num)=PhotAt(ne,i,num)*s
	enddo
	enddo

        do ns=1,qshPas(num)
	WeightShAt(ns,num)=step_integ_ar(ener,PhotAt(1,ns,num),qener,
     +		ener(1),ener(qener+1))/cphoAt(num)
	enddo

	
	go to 100

		endif     ! continuing of old algorithm
		
	
		call shellfi
c	        call prishellfi
	endif
	if(qash.eq.0.or.KeyTeor.ne.0)then
		call shteor(num)
		if(qash.eq.0)then
			write(oo,*)' Error in IniAtom:',
     +		'can not find atom with z=',z
			stop
		endif
		call GenTheorPhot
c	        call prishellfi
	endif

	call shellfico
c	        call prishellfi
	
	QShellAt(num)=qash
	do i=1,qatm
		if(ZAt(num).eq.charge(i))then
			if(QShellAt(num).ne.qshl(i))then
	write(oo,*)' Worning of IniAtom:'
	write(oo,*)' Quantity of shell is different for shl'
	write(oo,*)' In may lead to error'
			endif
			goto 10
		endif
	enddo
10	continue
	do i=1,QShellAt(num)
	    ThresholdAt(i,num)=athreshold(i)
		if(ThresholdAt(i,num).lt.ener(1))then
			write(oo,*)' error in IniAtom:'
			write(oo,*)' too high ener(1)=',ener(1)
			write(oo,*)' ThresholdAt(ns,num)=',
     +				ThresholdAt(i,num)
			stop
		endif
	    WeightShAt(i,num)=aweight(i)

	    do iener=1,qener

		PhotAt(iener,i,num)=
     +		glin_integ_ar(aener(1,i),aphot(1,i),qaener(i),
     +		ener(iener),ener(iener+1),ThresholdAt(i,num))/
     +				(ener(iener+1)-ener(iener))

	    enddo

	enddo

*** Added argument to PriAtoms (RV 13/4/99)
c	call PriAtoms(0)
*** End of modification.
	
	w=0.0
	do i=1,QShellAt(num)
		w=w+WeightShAt(i,num)			
	enddo
	do i=1,QShellAt(num)
		WeightShAt(i,num)=WeightShAt(i,num)/w			
	enddo
	sw=0.0
	do i=1,QShellAt(num)
	    w=step_integ_ar(ener,PhotAt(1,i,num),qener,
     +		ener(1),ener(qener+1))
	    PWeightShAt(i,num)=w
	    sw=sw+w
	    if(w.lt.0.0)then
		do n=1,qener
		    PhotAt(n,i,num)=0.0
		enddo
	    else
		do n=1,qener
		    PhotAt(n,i,num)=PhotAt(n,i,num)*cphoAt(num)*
     +			WeightShAt(i,num)/w
		enddo
*******		write(oo,*)' koef=',cphoAt(num)*WeightShAt(i,num)/w
	    endif
	enddo
	do i=1,QShellAt(num)
		PWeightShAt(i,num)=PWeightShAt(i,num)/sw
	enddo
			
100	continue

	do i=1,qatm
		if(ZAt(num).eq.charge(i))then
			if(QShellAt(num).ne.qshl(i))then
	write(oo,*)' Worning of IniAtom:'
	write(oo,*)' Quantity of shell is different for shl'
	write(oo,*)' In may lead to error'
			endif
			goto 20
		endif
	enddo
20	continue

	s=0.0
	do ns=1,QShellAt(num)
c	write(oo,*)' start integration'
		ISPhotBAt(ns,num)=step_integ_ar
     +		(ener,PhotAt(1,ns,num),qener,ener(1),ener(qener+1))
		s=s+ISPhotBAt(ns,num)
	enddo
	IAPhotBAt(num)=s
	MinThresholdAt(num)=ThresholdAt(QShellAt(num),num)
	NshMinThresholdAt(num)=QShellAt(num)
	Min_ind_E_At(num)=0
	Max_ind_E_At(num)=0

	if(IAPhotBAt(num).gt.cphoAt(num))then
c		reduce all shells
		s=cphoAt(num)/IAPhotBAt(num)
	        do ne=1,qener
	        do ns=1,QShellAt(num)
			PhotAt(ne,ns,num)=PhotAt(ne,ns,num)*s
		enddo
		enddo
c		copy absorbtion to ionization
	        do ne=1,qener
	        do ns=1,QShellAt(num)
			PhotIonAt(ne,ns,num)=PhotAt(ne,ns,num)
		enddo
		enddo
c		reduce weights
		do ns=1,QShellAt(num)
			WeightShAt(ns,num)=WeightShAt(ns,num)*s	
		enddo		

	elseif(IAPhotBAt(num).lt.cphoAt(num))then
c		copy absorbtion to ionzation	
	        do ne=1,qener
	        do ns=1,QShellAt(num)
			PhotIonAt(ne,ns,num)=PhotAt(ne,ns,num)
		enddo
		enddo
c		add excitation part to absorption
		

		j=qener
		do ne=3,qener
		    if(ener(ne).gt.MinThresholdAt(num))then
			j=ne-1		! ener(j) in the last point
				! So the last interval has number j-1
			go to 25
		    endif
		enddo
25		continue
		if(j.le.2)then
			write(oo,*)' Error in IniAtom:'
			write(oo,*)' cannot insert excitation'
			write(oo,*)' too large ener(1)=',ener(1)
			write(oo,*)' MinThresholdAt(num)=',
     +					MinThresholdAt(num)
			stop
		endif
		nn=1
		do ne=j-1,1,-1
		    if(enerc(ne).lt. 0.7*MinThresholdAt(num))then
			nn=ne
			go to 30
		    endif
		enddo
30		continue
		s=(-IAPhotBAt(num)+cphoAt(num))/
     +			(ener(j) - ener(nn))

		do ne=nn,j-1
			PhotAt(ne,NshMinThresholdAt(num),num)=
     +			PhotAt(ne,NshMinThresholdAt(num),num)+s
		enddo
		Min_ind_E_At(num)=nn
		Max_ind_E_At(num)=j-1

		

	else
c		copy absorbtion to ionzation	
	        do ne=1,qener
	        do ns=1,QShellAt(num)
			PhotIonAt(ne,ns,num)=PhotAt(ne,ns,num)
		enddo
		enddo
c		add excitation part to absorption
	
	endif

	s=0.0
	do ns=1,QShellAt(num)
		ISPhotAt(ns,num)=step_integ_ar
     +		(ener,PhotAt(1,ns,num),qener,ener(1),ener(qener+1))
		s=s+ISPhotAt(ns,num)
	enddo
	IAPhotAt(num)=s

	s=0.0
	do ns=1,QShellAt(num)
		ISPhotIonAt(ns,num)=step_integ_ar
     +		(ener,PhotIonAt(1,ns,num),qener,
     +          ener(1),ener(qener+1))
		s=s+ISPhotIonAt(ns,num)
	enddo
	IAPhotIonAt(num)=s


	
	end
	

	subroutine GenTheorPhot

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
c 	include 'shellfi.inc'
	integer pqash		! Max. q. of shells
	parameter (pqash=7)
	integer zato		! Z of atom
	integer qash		! quantity of shells
	real athreshold,aweight	! threshold and weight of shells
	integer pqaener,qaener	! Max. and just q. of shell energy
	parameter (pqaener=500)
	real aener		! Energy
	real aphot		! Photoabsorbtion crossection
			! for this point of energy
	common / cshellfi /
     +		zato,
     +		qash,
     +		athreshold(pqash),aweight(pqash),
     +		qaener(pqash),
     +		aener(pqaener,pqash),aphot(pqaener,pqash)
	save / cshellfi /
	
	integer nsh,nen

	do nsh=1,qash

	    qaener(nsh)=qener
	    do nen=1,qener
		aener(nen,nsh)=enerc(nen)
		if(athreshold(nsh).lt.ener(nen+1))then
		    aphot(nen,nsh)=1.0/(enerc(nen)**2.5)
		    if(athreshold(nsh).gt.ener(nen))then
			aphot(nen,nsh)=aphot(nen,nsh)*
     +			(ener(nen+1)-athreshold(nsh))/
     +			(ener(nen+1)-ener(nen))
		    endif
		else
		    aphot(nen,nsh)=0.0
		endif
	    enddo
	enddo

	end


	subroutine shellfico

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
c 	include 'shellfi.inc'
	integer pqash		! Max. q. of shells
	parameter (pqash=7)
	integer zato		! Z of atom
	integer qash		! quantity of shells
	real athreshold,aweight	! threshold and weight of shells
	integer pqaener,qaener	! Max. and just q. of shell energy
	parameter (pqaener=500)
	real aener		! Energy
	real aphot		! Photoabsorbtion crossection
			! for this point of energy
	common / cshellfi /
     +		zato,
     +		qash,
     +		athreshold(pqash),aweight(pqash),
     +		qaener(pqash),
     +		aener(pqaener,pqash),aphot(pqaener,pqash)
	save / cshellfi /

	integer is,iaen,iaens,ien,iens
	real np
	np=2.5
c	the prolongation is needed only for first shell

	do is=1,qash

	
c	is=1

		do iaen=qaener(is),1,-1
			if(aphot(iaen,is).gt.0)then
				iaens=iaen
				go to 10
			endif
		enddo
10		continue

		if(is.ne.1)then
			if(aener(iaens,is).eq.aener(1,is-1))then
				go to 30
			endif
		endif

c		same strange empty place in file in some atoms
			
		if(aener(iaens,is).lt.enerc(qener))then
			do ien=1,qener
				if(enerc(ien).gt.aener(iaens,is))then
					iens=ien
					goto 20
				endif
			enddo
20			continue
			iaen=iaens
			do ien=iens,qener
				iaen=iaen+1
				aener(iaen,is)=enerc(ien)
				aphot(iaen,is)=aphot(iaens,is)*
     +				(aener(iaens,is)/enerc(ien))**np
			enddo
			qaener(is)=iaen
		endif

30	continue	

	enddo

c	if(zato.eq.18)then
c	call prishellfi
c	endif	
	
	end


	subroutine priatoms(n)

	implicit none

	integer n		! n = 0,1 short output
				! n >= 2 long output
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

	integer nat, nsh, nen, nat1

	if(soo.eq.0)return

	write(oo,*)
	write(oo,*)' priatoms: Atomic data'
	write(oo,*)' KeyTeor=',KeyTeor
	do nat=1,pQAt
	    if(Zat(nat).gt.0)then
	write(oo,*)
	write(oo,*)' nat=',nat,' Zat=',Zat(nat),' Aat=',Aat(nat),
     +	' QShellAt=',QShellAt(nat)
c	write(oo,*)' num_at_mol=',num_at_mol(nat)
	write(oo,*)' cphoAt=',cphoAt(nat)
	write(oo,*)' RLenAt=',RLenAt(nat)
	write(oo,*)' RuthAt=',RuthAt(nat)
		do nsh=1,QShellAt(nat)
	write(oo,*)' ThresholdAt=',ThresholdAt(nsh,nat),
     +	' WeightShAt=',WeightShAt(nsh,nat)
	write(oo,*)' PWeightShAt=',PWeightShAt(nsh,nat)
		enddo
	write(oo,*)' IAPhotBAt     IAPhotAt   IAPhotIonAt '
	write(oo,*)IAPhotBAt(nat), IAPhotAt(nat), IAPhotIonAt(nat)
		do nsh=1,QShellAt(nat)
	write(oo,*)nsh,
     +	ISPhotBAt(nsh,nat), ISPhotAt(nsh,nat), ISPhotIonAt(nsh,nat)
		enddo
	write(oo,*)' MinThresholdAt=',MinThresholdAt(nat)
	write(oo,*)' NshMinThresholdAt=',NshMinThresholdAt(nat)
	write(oo,*)' Min_ind_E_At=',Min_ind_E_At(nat),
     +		' Max_ind_E_At=',Max_ind_E_At(nat)
		if(n.ge.2)then
		write(oo,*)' energy and photoabs cross sections'
c		do nen=1,qener
c	write(oo,'(10e12.3)')
c     +	enerc(nen),(PhotAt(nen,nsh,nat),nsh=1,QShellAt(nat))
c		enddo
		do nsh=1,QShellAt(nat)
	write(oo,*)' shell number=',nsh
	write(oo,*)' enerc,   PhotAt,   PhotIonAt'
		do nen=1,qener
	write(oo,'(3e10.3)')
     +	enerc(nen),PhotAt(nen,nsh,nat),PhotIonAt(nen,nsh,nat)
		enddo	! nen=1,qener
		enddo	! nsh=1,QShellAt(nat)
		endif	! if(n.ge.2)
	    endif	! if(Zat(nat).gt.0)
	enddo		! nat=1,pQAt

	write(oo,*)' Sequenced numbers:'
	write(oo,*)'          nat    Zat(nat)   nseqAt(nat)'
	do nat=1,QseqAt
	write(oo,*) nat, Zat(nat), nseqAt(nat)
	enddo
	write(oo,*)
     +		'         nat1        nat    Zat(nat)'
	do nat1=1,QseqAt
	nat=nseqAt(nat1)
	write(oo,*) nat1, nat, Zat(nat)
	enddo

	end


	
