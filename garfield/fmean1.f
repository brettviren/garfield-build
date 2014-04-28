CDECK  ID>, FMEAN1.
 	function fmean1(ad,weig,nmat)
c
c	Calc. mean energy loss for 1 sm
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


	real fmean1,ad(*),weig
	integer nmat
	real step_integ_ar
	real addd(pqener)
	real e1,e2

	integer nen

	do nen=1,qener
		addd(nen)=ad(nen)*enerc(nen)
	enddo
	fmean1=step_integ_ar(ener,addd,qener,ener(1),ener(qener+1))
	fmean1=fmean1*weig*XElDensMat(nmat)
	if(emax.gt.ener(qener+1))then
		e1=ener(qener+1)
		e2=emax
		
	fmean1 = fmean1 +
     +	2.0 * PI / (FSCON**2 * ELMAS * beta2)
     +	* weig * XElDensMat(nmat) *
     +	( log(e2/e1) - bem * (e2-e1) +
     +	(e2*e2-e1*e1)/(4.0 * (tkin+mass) * (tkin+mass) ) )

	endif

	end
