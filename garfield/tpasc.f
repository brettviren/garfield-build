CDECK  ID>, TPASC.
	
	subroutine readPas(na)

	implicit none

	integer na

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



	integer Za,npas

	integer i

c	include 'shelltsc.inc'
	integer pq
	parameter (pq=10)
	integer z(pq)
	integer n(pq)
	integer pmaxn
	parameter (pmaxn=5)
	integer l(pq,pmaxn)
	real p(pq,pmaxn,6)
	data z(1) / 2 /
	data n(1) / 1 /
	data l(1,1) / 0 /
	data p(1,1,1) / 23.42 /
	data p(1,1,2) / 2.024 /
	data p(1,1,3) / 2578 /
	data p(1,1,4) / 9.648 /
	data p(1,1,5) / 6.218 /
	data p(1,1,6) / 0 /
	data z(2) / 3 /
	data n(2) / 2 /
	data l(2,1) / 0 /
	data p(2,1,1) / 59.85 /
	data p(2,1,2) / 29.51 /
	data p(2,1,3) / 125.2 /
	data p(2,1,4) / 73020 /
	data p(2,1,5) / 0.9438 /
	data p(2,1,6) / 0 /
	data l(2,2) / 0 /
	data p(2,2,1) / 5.495 /
	data p(2,2,2) / 3.466 /
	data p(2,2,3) / 47.74 /
	data p(2,2,4) / 20.35 /
	data p(2,2,5) / 4.423 /
	data p(2,2,6) / 0 /
	data z(3) / 6 /
	data n(3) / 3 /
	data l(3,1) / 0 /
	data p(3,1,1) / 291 /
	data p(3,1,2) / 86.55 /
	data p(3,1,3) / 74.21 /
	data p(3,1,4) / 54.98 /
	data p(3,1,5) / 1.503 /
	data p(3,1,6) / 0 /
	data l(3,2) / 0 /
	data p(3,2,1) / 17.55 /
	data p(3,2,2) / 10.26 /
	data p(3,2,3) / 4564 /
	data p(3,2,4) / 1.568 /
	data p(3,2,5) / 10.85 /
	data p(3,2,6) / 0 /
	data l(3,3) / 1 /
	data p(3,3,1) / 8.987 /
	data p(3,3,2) / 9.435 /
	data p(3,3,3) / 1152 /
	data p(3,3,4) / 5.687 /
	data p(3,3,5) / 6.336 /
	data p(3,3,6) / 0.4474 /
	data z(4) / 7 /
	data n(4) / 3 /
	data l(4,1) / 0 /
	data p(4,1,1) / 404.8 /
	data p(4,1,2) / 127 /
	data p(4,1,3) / 47.48 /
	data p(4,1,4) / 138 /
	data p(4,1,5) / 1.252 /
	data p(4,1,6) / 0 /
	data l(4,2) / 0 /
	data p(4,2,1) / 23.1 /
	data p(4,2,2) / 14.82 /
	data p(4,2,3) / 772.2 /
	data p(4,2,4) / 2.306 /
	data p(4,2,5) / 9.139 /
	data p(4,2,6) / 0 /
	data l(4,3) / 1 /
	data p(4,3,1) / 11.49 /
	data p(4,3,2) / 11.64 /
	data p(4,3,3) / 10290 /
	data p(4,3,4) / 2.361 /
	data p(4,3,5) / 8.821 /
	data p(4,3,6) / 0.4239 /
	data z(5) / 8 /
	data n(5) / 3 /
	data l(5,1) / 0 /
	data p(5,1,1) / 537.3 /
	data p(5,1,2) / 177.4 /
	data p(5,1,3) / 32.37 /
	data p(5,1,4) / 381.2 /
	data p(5,1,5) / 1.083 /
	data p(5,1,6) / 0 /
	data l(5,2) / 0 /
	data p(5,2,1) / 29.22 /
	data p(5,2,2) / 19.94 /
	data p(5,2,3) / 241.5 /
	data p(5,2,4) / 3.241 /
	data p(5,2,5) / 8.037 /
	data p(5,2,6) / 0 /
	data l(5,3) / 1 /
	data p(5,3,1) / 14.16 /
	data p(5,3,2) / 13.91 /
	data p(5,3,3) / 122000 /
	data p(5,3,4) / 1.364 /
	data p(5,3,5) / 11.4 /
	data p(5,3,6) / 0.4103 /
	data z(6) / 9 /
	data n(6) / 3 /
	data l(6,1) / 0 /
	data p(6,1,1) / 688.3 /
	data p(6,1,2) / 239 /
	data p(6,1,3) / 22.95 /
	data p(6,1,4) / 1257 /
	data p(6,1,5) / 0.9638 /
	data p(6,1,6) / 0 /
	data l(6,2) / 0 /
	data p(6,2,1) / 35.93 /
	data p(6,2,2) / 25.68 /
	data p(6,2,3) / 109.7 /
	data p(6,2,4) / 4.297 /
	data p(6,2,5) / 7.303 /
	data p(6,2,6) / 0 /
	data l(6,3) / 1 /
	data p(6,3,1) / 17 /
	data p(6,3,2) / 16.58 /
	data p(6,3,3) / 277500 /
	data p(6,3,4) / 1.242 /
	data p(6,3,5) / 12.49 /
	data p(6,3,6) / 0.3857 /
	data z(7) / 10 /
	data n(7) / 3 /
	data l(7,1) / 0 /
	data p(7,1,1) / 858.2 /
	data p(7,1,2) / 314.4 /
	data p(7,1,3) / 16.64 /
	data p(7,1,4) / 204200 /
	data p(7,1,5) / 0.845 /
	data p(7,1,6) / 0 /
	data l(7,2) / 0 /
	data p(7,2,1) / 43.24 /
	data p(7,2,2) / 32.04 /
	data p(7,2,3) / 56.15 /
	data p(7,2,4) / 5.808 /
	data p(7,2,5) / 6.678 /
	data p(7,2,6) / 0 /
	data l(7,3) / 1 /
	data p(7,3,1) / 20 /
	data p(7,3,2) / 20 /
	data p(7,3,3) / 16910 /
	data p(7,3,4) / 2.442 /
	data p(7,3,5) / 10.43 /
	data p(7,3,6) / 0.3345 /
	data z(8) / 13 /
	data n(8) / 5 /
	data l(8,1) / 0 /
	data p(8,1,1) / 1550 /
	data p(8,1,2) / 367 /
	data p(8,1,3) / 22.06 /
	data p(8,1,4) / 44.05 /
	data p(8,1,5) / 1.588 /
	data p(8,1,6) / 0 /
	data l(8,2) / 0 /
	data p(8,2,1) / 119 /
	data p(8,2,2) / 55.94 /
	data p(8,2,3) / 14.25 /
	data p(8,2,4) / 30.94 /
	data p(8,2,5) / 4.399 /
	data p(8,2,6) / 0 /
	data l(8,3) / 1 /
	data p(8,3,1) / 80.87 /
	data p(8,3,2) / 64.45 /
	data p(8,3,3) / 173.5 /
	data p(8,3,4) / 11310 /
	data p(8,3,5) / 2.762 /
	data p(8,3,6) / 0.02337 /
	data l(8,4) / 0 /
	data p(8,4,1) / 10.16 /
	data p(8,4,2) / 12.04 /
	data p(8,4,3) / 5.384 /
	data p(8,4,4) / 434.1 /
	data p(8,4,5) / 4.088 /
	data p(8,4,6) / 0 /
	data l(8,5) / 1 /
	data p(8,5,1) / 4.878 /
	data p(8,5,2) / 18.6 /
	data p(8,5,3) / 182.8 /
	data p(8,5,4) / 2.797 /
	data p(8,5,5) / 10.84 /
	data p(8,5,6) / 0.3076 /
	data z(9) / 14 /
	data n(9) / 5 /
	data l(9,1) / 0 /
	data p(9,1,1) / 1828 /
	data p(9,1,2) / 532.2 /
	data p(9,1,3) / 11.84 /
	data p(9,1,4) / 258 /
	data p(9,1,5) / 1.102 /
	data p(9,1,6) / 0 /
	data l(9,2) / 0 /
	data p(9,2,1) / 151.5 /
	data p(9,2,2) / 70.17 /
	data p(9,2,3) / 11.66 /
	data p(9,2,4) / 47.42 /
	data p(9,2,5) / 3.933 /
	data p(9,2,6) / 0 /
	data l(9,3) / 1 /
	data p(9,3,1) / 108.2 /
	data p(9,3,2) / 78.08 /
	data p(9,3,3) / 153.2 /
	data p(9,3,4) / 5.765e+06 /
	data p(9,3,5) / 2.639 /
	data p(9,3,6) / 0.0002774 /
	data l(9,4) / 0 /
	data p(9,4,1) / 13.61 /
	data p(9,4,2) / 14.13 /
	data p(9,4,3) / 11.66 /
	data p(9,4,4) / 22.88 /
	data p(9,4,5) / 5.334 /
	data p(9,4,6) / 0 /
	data l(9,5) / 1 /
	data p(9,5,1) / 6.542 /
	data p(9,5,2) / 22.12 /
	data p(9,5,3) / 184.5 /
	data p(9,5,4) / 3.849 /
	data p(9,5,5) / 9.721 /
	data p(9,5,6) / 0.2921 /
	data z(10) / 18 /
	data n(10) / 5 /
	data l(10,1) / 0 /
	data p(10,1,1) / 3178 /
	data p(10,1,2) / 1135 /
	data p(10,1,3) / 4.28 /
	data p(10,1,4) / 3.285e+07 /
	data p(10,1,5) / 0.7631 /
	data p(10,1,6) / 0 /
	data l(10,2) / 0 /
	data p(10,2,1) / 313.5 /
	data p(10,2,2) / 130.2 /
	data p(10,2,3) / 9.185 /
	data p(10,2,4) / 26.93 /
	data p(10,2,5) / 4.021 /
	data p(10,2,6) / 0 /
	data l(10,3) / 1 /
	data p(10,3,1) / 247.9 /
	data p(10,3,2) / 164.7 /
	data p(10,3,3) / 83.72 /
	data p(10,3,4) / 54.52 /
	data p(10,3,5) / 3.328 /
	data p(10,3,6) / 0.627 /
	data l(10,4) / 0 /
	data p(10,4,1) / 28.92 /
	data p(10,4,2) / 25.25 /
	data p(10,4,3) / 6.394 /
	data p(10,4,4) / 170 /
	data p(10,4,5) / 4.223 /
	data p(10,4,6) / 0 /
	data l(10,5) / 1 /
	data p(10,5,1) / 14.49 /
	data p(10,5,2) / 38.54 /
	data p(10,5,3) / 48.72 /
	data p(10,5,4) / 26.4 /
	data p(10,5,5) / 6.662 /
	data p(10,5,6) / 0.2355 /



	Za=Zat(na)


	do i=1,pq

	if(z(i).eq.Za)then

		qshPas(na)=n(i)
		do npas=1,qshPas(na)
	lPas(npas,na)=l(i,npas)
	EthPas(npas,na)=p(i,npas,1)
	E0Pas(npas,na)=p(i,npas,2)
     	sigma0Pas(npas,na)=p(i,npas,3)
     	yaPas(npas,na)=p(i,npas,4)
	PPas(npas,na)=p(i,npas,5)
	ywPas(npas,na)=p(i,npas,6)
		enddo
		go to 110

	endif
	enddo
*** Warning message commented out (RV 29/6/98).
C        if(soo.eq.1)then
C        write(oo,*)
C     +	' Worning of readPas: atom z=',Za,' is not found.'
C     	write(oo,*)
C     +	'  The data will be seached by shellfi, accuracy will be lower.'
C	endif
*** End of modification.
110     continue

		
	end
	


	function sigma_nl(E,E0,Eth,yw,l,ya,P,sigma0)

	implicit none

	real sigma_nl,Fpasc
	real E,E0,Eth,yw,ya,P,sigma0
	integer l

	real Q,y

	if(E.ge.Eth)then
	
	Q=5.5+l-0.5*P
	y=E/E0
	Fpasc=((y-1)*(y-1) + yw*yw) * y**(-Q) * (1.0 + sqrt(y/ya))**(-P)
	Fpasc=Fpasc*sigma0

	else

	Fpasc=0.0

	endif

	sigma_nl=Fpasc

	end

	subroutine Pripasc

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



	integer na,ns

        if(soo.eq.0)return
	write(oo,*)
	write(oo,*)' Pripasc:'
	do na=1,PQat
	if(Zat(na).gt.0)then
	write(oo,*)' qshPas(na)=',qshPas(na)
	write(oo,*)' l,E0,Eth,yw, ya,P,sigma0:'
	do ns=1,qshPas(na)
	write(oo,'(1X,i3,6e10.3)')lPas(ns,na),E0Pas(ns,na),
     +	EthPas(ns,na),ywPas(ns,na),yaPas(ns,na),PPas(ns,na),
     +	sigma0Pas(ns,na)
	enddo
	endif

	enddo

	end
