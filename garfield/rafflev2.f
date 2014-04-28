CDECK  ID>, RAFFLEV2.
	subroutine rafflevirt2(nv,niv)
	
	implicit none

	integer nv,niv

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
	
c 	include 'raffle.inc'
	integer pQGRaf		! Max. quantity of energy transfer
	parameter (pQGRaf=10000)
	integer QGRaf		! Quantity of energy transfers
	integer NAtGRaf,NShAtGRaf ! Numbers of atom and shell
	real ESGRaf,EGRaf	! Cumulative energy and just energy
	real pntraf,velraf
	
	common / craffle /
     +	QGRaf,
     +	ESGRaf,
     +	EGRaf(pQGRaf),
     +	NAtGRaf(pQGRaf),
     +	NShAtGRaf(pQGRaf) ,
     +	pntraf(3,pQGRaf), velraf(3,pQGRaf)


	save / craffle /
c 	include 'volume.inc'
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
c 	include 'lsgvga.inc'
c		Results of ionization loss calculations
c		It is used only for hist filling

	integer pqgvga
*** Increased buffer (RV 22/11/06) - check INDPOS dimensions when changing !
	parameter (pqgvga=10000)
*	parameter (pqgvga=1000)
	integer qgvga,ganumat,ganumshl
*** Added overflow tracing (RV 22/11/06)
        integer sOverflowvga    ! sign of overflow in the current event
        integer qsOverflowvga   ! quantity of the overflows in all events
        integer qOverflowvga    ! quantity of the lossed photons
                                ! in all events
*** End of addition.
	real esgvga,egvga,velgvga
	real*8 pntgvga
	common / clsgva /
     +	qgvga(pQIVol),
     +	esgvga(pQIVol),
     +	egvga(pqgvga,pQIVol),
     +	pntgvga(3,pqgvga,pQIVol),
     +	velgvga(3,pqgvga,pQIVol),
     +	ganumat(pqgvga,pQIVol),
     +	ganumshl(pqgvga,pQIVol),
     +  sOverflowvga, qsOverflowvga,qOverflowvga
	save / clsgva /
c 	include 'abs.inc'

c		Gamma which is ready to absorb
c		There are two sorts of gamma
c		Real gamma after their absorbtion points are known and
c		virtual gamma from ionization loss
	integer pqtagam		 ! Max quantity of absorbtion gamma
	parameter (pqtagam=100000)
	integer qtagam, ctagam	 ! Full quantity and current number
				 ! of gamma which will be treat next.
				 ! If ctagam>qtagam then
				 ! there is no gamma to treat.
	real etagam,  vtagam ! Energy,  and velocity
				 ! direction of absorbtion gamma
	real*8 rtagam 		 ! position of absorbtion gamma
	integer nVolagam	 ! Volume number for this point
	integer nAtagam,nshlagam ! Number of atom and shell
				 ! which  absorbe this photon
	integer Stagam		 ! Generation number
	integer upagam		 ! additional parameters
        integer sOverflowagam    ! sign of overflow in the current event
        integer qsOverflowagam   ! quantity of the overflows in all events
        integer qOverflowagam    ! quantity of the lossed electrons
                                ! in all events

	common / comabs /
     +	qtagam, ctagam, etagam(pqtagam),
     +	rtagam(3,pqtagam), vtagam(3,pqtagam),
     +	nVolagam(pqtagam),nAtagam(pqtagam),nShlagam(pqtagam),
     +	Stagam(pqtagam), upagam(pqup,pqtagam),
     +  sOverflowagam, qsOverflowagam,qOverflowagam
	save / comabs /

	integer i,j,nmt,nmta
	real ranfl
	real*8 rr
	real*8 rrr

       esgvga(niv)=ESGRaf
	if(QGRaf.le.pqgvga)then
		qgvga(niv)=QGRaf
	else
		qgvga(niv)=pqgvga
*** Added overflow tracing (RV 22/11/06).
                qOverflowvga=qOverflowvga+1
                if(sOverflowvga.eq.0)then
                    qsOverflowvga=qsOverflowvga+1
                    sOverflowvga=1
                endif
*** End of addition.
	endif
       do i=1,QGRaf
                rr=ranfl()
                rr=rr*vlenmtk(nv)
                rrr=rr
                do nmt=nmtkvol1(nv),nmtkvol2(nv)
                    if(rrr.le.lenmtk(nmt))then
                        do j=1,3
                        pntraf(j,i)=pntmtk(j,nmt)+rrr*velmtk(j,nmt)
                        enddo
                        nmta=nmt
                        go to 10
                    else
                        rrr=rrr-lenmtk(nmt)
                    endif
                enddo
                write(oo,*)' worning in rafflevirt2: strange step'
                        nmta=nmtkvol2(nv)
                        do j=1,3
                        pntraf(j,i)=pntmtk(j,nmta)+
     +                          vlenmtk(nv)*velmtk(j,nmta)
                        enddo

10              continue

                call Ncirclesim(
     +          e1mtk(1,nmta),e2mtk(1,nmta),e3mtk(1,nmta),
     +          velraf(1,i))

            if(i.le.pqgvga)then
                egvga(i,niv)=EGRaf(i)
                do j=1,3
                        pntgvga(j,i,niv)=pntraf(j,i)
                        velgvga(j,i,niv)=velraf(j,i)
                enddo
                ganumat(i,niv)=NAtGRaf(i)
                ganumshl(i,niv)=NShAtGRaf(i)
*** Added overflow tracing (RV 22/11/06).
            else
                qOverflowvga=qOverflowvga+1
                if(sOverflowvga.eq.0)then
                    qsOverflowvga=qsOverflowvga+1
                    sOverflowvga=1
                endif
*** End of addition.
            endif

c               write(oo,*)' rafflevirt1:'
c               write(oo,*)(rst(j),j=1,3)
c               write(oo,*)(wid(j),j=1,3)
c               write(oo,*)(pntgvga(j,i,nsv),j=1,3)
c               write(oo,*)(vel(j),j=1,3)
c               write(oo,*)(velgvga(j,i,nsv),j=1,3)
c                ganumat(i,niv)=NAtGRaf(i)
c                ganumshl(i,niv)=NShAtGRaf(i)

            if(qtagam .eq. pqtagam)then
                qOverflowagam=qOverflowagam+1
                if(sOverflowagam.eq.0)then
                   qsOverflowagam=qsOverflowagam+1
                   sOverflowagam=1
                endif
            else
                qtagam=qtagam+1
                etagam(qtagam)=EGRaf(i)
                do j=1,3
                        rtagam(j,qtagam)=pntraf(j,i)
                        vtagam(j,qtagam)=velraf(j,i)
                enddo
                nVolagam(qtagam)=nv
                nAtagam(qtagam)=NAtGRaf(i)
                nShlagam(qtagam)=NShAtGRaf(i)
                Stagam(qtagam)=1
            endif
        enddo   !i=1,QGRaf

        end
