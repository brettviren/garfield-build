CDECK  ID>, ININTRAC.
	subroutine IniNTrack

c
c	Generate the next track
c	It calls from GoEvent
c	If there are no randomization of the track requried
c	and the are no multiple scattering, it does nothing
c	except filling of some data structure.

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
c 	include 'track.inc'
c	The track information about the primary particle

	integer sign_ang	! sign to run the part. with effective angle
	real ang		! teta
	real phiang		! phi
	real ystart		! start Y coordinate
	integer srandtrack	! sign to randomize the Y coordinate
				! between ystart1 and  ystart2
				! It is done by call IniNTrack from GoEvent
				! if the track initialization was done by
				! call IniRTrack
	real ystart1
	real ystart2
	real sigmaang		! sigma of begin angle distribution
			!Currently, if sigmaang>0, the rundomization
			! is doing around the 0 angle.
			! So the values of pang and pphiang are ignored
			! It can be changed by modernization
			! of IniNTrack
	real e1ang,e2ang,e3ang	! coordinates of new orts in the old
	integer sigmtk		! sign of multiple scatering
	integer pQmtk		! max. quantity of the break point of the track
				! plus one
	parameter (pQmtk=10000)
	integer Qmtk		! actual quantity for current event
	real*8 pntmtk		! break point coordinates
	real velmtk		! directions of velocity
	real*8 lenmtk		! lengt of way for straight till next break
	real Tetamtk		! turn angle
	integer nVolmtk		! number of volume for given point,
				! the point on the frantier is correspond
				! to next volume of zero for end.
	real*8 vlenmtk		! lengt of way inside the volume
	integer nmtkvol1,nmtkvol2 ! numbers of first point in volume
			! and the previous for end point
	real*8 xdvmtk,ydvmtk	! deviations from strate line
			! using only for histograms

	! service data. They are using at initialization of the track.
	integer sruthmtk	! key to use Rutherford cross section
	integer nmtk		! current number of point.
			! After initialization it must be equal to Qmtk+1
	integer sgnmtk		! sign to go to next volume
	integer sturnmtk	! sign to turn
	real*8 lammtk		! mean free path
	real mlammtk		! minimum mean lengt of range
			! multiplied by density. sm*gr/sm**3 = gr/sm**2
	real mTetacmtk		! minimum threshold turn angle
	real Tetacmtk		! threshold turn angle
	real rTetacmtk		! restiction due to atomic shell
	real*8 CosTetac12mtk	! cos(tetac/2)
	real*8 SinTetac12mtk	! sin(tetac/2)
c	real CosTetac12mtk	! cos(tetac/2)
c	real SinTetac12mtk	! sin(tetac/2)
	real msigmtk		! msig without sqrt(x)
	real e1mtk,e2mtk,e3mtk
	common / ctrack /
     +	sign_ang, ang, phiang, ystart, srandtrack, ystart1, ystart2,
     +	e1ang(3),e2ang(3),e3ang(3),
     +	sigmtk,
     +	sruthmtk,
     +	Qmtk, nmtk,
     +	pntmtk(3,pQmtk), velmtk(3,pQmtk), lenmtk(pQmtk), Tetamtk(pQmtk),
     + 	nVolmtk(pQmtk), vlenmtk(pQVol),
     +	nmtkvol1(pQVol), nmtkvol2(pQVol),
     +	xdvmtk(pQSVol),ydvmtk(pQSVol),
     +	sgnmtk, sturnmtk,
     +	lammtk(pQMat), mlammtk, mTetacmtk,
     +	Tetacmtk(pQMat),
     +	rTetacmtk(pQMat),
     +	CosTetac12mtk(pQMat), SinTetac12mtk(pQMat), msigmtk,
     +	e1mtk(3,pQmtk),e2mtk(3,pQmtk),e3mtk(3,pQmtk),
     +	sigmaang
	save / ctrack /
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

	real r
	real ranfl
	real pang,pphiang,pystart
	real yy,dimmy

        integer n,nv,i

         if(srandtrack.eq.1)then
		r=ranfl()
		ystart=ystart1+(ystart2-ystart1)*r
		if(sigmaang.gt.0.0)then
10			call lranor(yy,dimmy)
			if(yy.lt.0.0) yy=-yy
			yy=yy*sigmaang
			if(yy.gt.1.0)goto 10
			ang=yy
			yy=ranfl()
			phiang=yy*2.0*PI
			pang=ang
			pphiang=phiang
			pystart=ystart
			call IniTrack(pystart, pang, pphiang)
			srandtrack=1 ! it falled in IniTrack
		endif
	endif

	if(sigmtk.eq.1)then
		call TTrack
	else
	  do nv=1,QVol
	    pntmtk(3,nv)=wall1(nv)
	    pntmtk(1,nv)=(wall1(nv)-wall1(1))*e3ang(1)/e3ang(3)
	    pntmtk(2,nv)=(wall1(nv)-wall1(1))*e3ang(2)/e3ang(3)+ystart
	    velmtk(1,nv)=e3ang(1)
	    velmtk(2,nv)=e3ang(2)
	    velmtk(3,nv)=e3ang(3)
	    do i=1,3
	      e1mtk(i,nv)=e1ang(i)
	      e2mtk(i,nv)=e2ang(i)
	      e3mtk(i,nv)=e3ang(i)
	    enddo
	  enddo
	  pntmtk(3,qVol+1)=wall2(qVol)
	  pntmtk(1,qVol+1)=(wall2(qVol)-wall1(1))*e3ang(1)/e3ang(3)
	  pntmtk(2,qVol+1)=(wall2(qVol)-wall1(1))
     +          	   *e3ang(2)/e3ang(3)+ystart
	  velmtk(1,qVol+1)=e3ang(1)
	  velmtk(2,qVol+1)=e3ang(2)
	  velmtk(3,qVol+1)=e3ang(3)
	  do i=1,3
	    e1mtk(i,qVol+1)=e1ang(i)
	    e2mtk(i,qVol+1)=e2ang(i)
	    e3mtk(i,qVol+1)=e3ang(i)
	  enddo
	
	  Qmtk=qVol
	  nmtk=Qmtk+1
	  do n=1,Qmtk
	    lenmtk(n)=sqrt((pntmtk(1,n+1)-pntmtk(1,n))**2+
     +		(pntmtk(2,n+1)-pntmtk(2,n))**2+
     +		(pntmtk(3,n+1)-pntmtk(3,n))**2 )
	  enddo
	  do n=1,Qmtk
	    Tetamtk(n)=0.0
	  enddo
	  do n=1,Qmtk
	    nVolmtk(n)=n
	  enddo
	  nVolmtk(Qmtk+1)=qVol
	  do n=1,Qmtk
	    vlenmtk(n)=lenmtk(n)
	    nmtkvol1(n)=n
	    nmtkvol2(n)=n
	    xdvmtk(n)=0.0
	    ydvmtk(n)=0.0
      	  enddo
				
	endif

	end
