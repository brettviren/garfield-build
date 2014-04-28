CDECK  ID>, TTRACK.
	subroutine TTrack

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

	real*8 mleng,rleng
	integer nsv
	real*8 rst(3),rl
	integer j

	if(qVol.le.0)then
		write(oo,*)' error in TTrack: there are not volumes'
		stop
	endif	

1	nmtk=1
	pntmtk(1,1)=0.0
	pntmtk(2,1)=ystart
	pntmtk(3,1)=wall1(1)
	velmtk(1,1)=e3ang(1)
	velmtk(2,1)=e3ang(2)
	velmtk(3,1)=e3ang(3)
	sgnmtk=1
	sturnmtk=0
	nmtkvol1(1)=1
	vlenmtk(1)=0.0
	nVolmtk(nmtk)=0

10	if(sgnmtk.eq.1)then
	    call VolNumZcoor(pntmtk(3,nmtk),velmtk(3,nmtk),nVolmtk(nmtk))
	    sgnmtk=0
	    if(nVolmtk(nmtk).ne.0)then
	    	vlenmtk(nVolmtk(nmtk))=0.0
	    endif
	endif
	if(nVolmtk(nmtk).eq.0)then
		go to 100
	endif

        call MakeNewSys
     +	(e1mtk(1,nmtk),e2mtk(1,nmtk),e3mtk(1,nmtk),velmtk(1,nmtk))

	if(sturnmtk.eq.1)then
		call TurnTrack
		sturnmtk=0
		if(velmtk(3,nmtk).le.0.0)then
		    write(oo,*)' worning in TTrack: particle goes back'
		    go to 1
		endif
	endif
	call VolPathLeng
     +		(pntmtk(3,nmtk),velmtk(1,nmtk),nVolmtk(nmtk),mleng)
	if(nMatVol(nVolmtk(nmtk)).eq.0)then  ! empty volume: no interaction
		lenmtk(nmtk)=mleng
		sgnmtk=1
		sturnmtk=0
	else
            if(sruthmtk.eq.1)then  !lengt to coulomb interaction
                call SRLengmtk(rleng)
            else
                call SMLengmtk(rleng)
            endif
	    if(rleng.le.mleng)then
		lenmtk(nmtk)=rleng
		sturnmtk=1
		sgnmtk=0
	    else
		lenmtk(nmtk)=mleng
		sgnmtk=1
		if(sruthmtk.eq.1)then
		    sturnmtk=0
		else
		    sturnmtk=1
		endif
	    endif
	endif
	do j=1,3
	    pntmtk(j,nmtk+1)=
     +		pntmtk(j,nmtk)+lenmtk(nmtk)*velmtk(j,nmtk)
	    velmtk(j,nmtk+1)=velmtk(j,nmtk)
	enddo
	vlenmtk(nVolmtk(nmtk))=vlenmtk(nVolmtk(nmtk))+lenmtk(nmtk)
	nVolmtk(nmtk+1)=nVolmtk(nmtk)
	if(sgnmtk.eq.1)then
	    nmtkvol2(nVolmtk(nmtk))=nmtk
	    nmtkvol1(nVolmtk(nmtk)+1)=nmtk+1
	    if(sSensit(nVolmtk(nmtk)).eq.1)then
		nsv=numSensVol(nVolmtk(nmtk))
		rst(3)=(wall2(nVolmtk(nmtk))-wall1(1))     ! it was error here
        	rl=rst(3)/e3ang(3)
        	rst(1)=e3ang(1)*rl
        	rst(2)=e3ang(2)*rl
		xdvmtk(nsv)=pntmtk(1,nmtk+1)-rst(1)
		ydvmtk(nsv)=pntmtk(2,nmtk+1)-rst(2)
	    endif
	endif	
	if(nmtk.ge.pQmtk-2)then
		write(oo,*)' worning of TTrack: '
   		write(oo,*)
     +		' Overflow of mtk. You have increase the common blok'
		go to 1
	endif
	nmtk=nmtk+1
	go to 10


	
100	Qmtk=nmtk-1

	end	
