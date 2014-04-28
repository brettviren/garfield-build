CDECK  ID>, LSTAABS1.
	subroutine lsta_abs1(eg,nrga,curpnt,veloc,num,isabs,nmat,nmshl)

c	make step to end of matter or to absorption point
c	curpnt - current point of photon
c	veloc - cosine
c	num - number of volume
c	isabs - sign of absorbtion

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

	real eg,veloc(3)
	real*8 curpnt(3)
	integer num
	integer nrga,isabs,nmat,nmshl
c	real dnst
	integer i
	real*8 mleng,xleng


	do i=1,1000	! number of mat is about 10
	
	isabs=0
	if(i.eq.1.and.num.ne.0)goto 10
	call VolNumZcoor(curpnt(3),veloc(3),num)
10	if(num.eq.0)return
	call VolPathLeng(curpnt(3),veloc,num,mleng)
c	write(oo,*)' num=',num,' mleng=',mleng
	call lsta_abs(eg,nrga,num,mleng,isabs,xleng,nmat,nmshl)
	curpnt(1)=curpnt(1)+xleng*veloc(1)
	curpnt(2)=curpnt(2)+xleng*veloc(2)
	curpnt(3)=curpnt(3)+xleng*veloc(3)

	if(isabs.eq.1)return

	enddo

	end



	subroutine lsta_abs(eg,nrga,nvol,mleng,
     +		isabs,xleng,nm_at,nmshl)

c	Raffle the absorbtion in volume number nvol
c	eg - energy of the photon
c	isabs - sign of absorbtion
c	xleng - coord of point of absorbtion
c	nm_at and nmshl - numbes of the atom and the shell
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
c 	include 'rga.inc'
c		Real photons
*** Check INDPOS when changing pqrga.
	integer pqrga	
	parameter (pqrga=1000)
	integer qrga, crga
	real velrga, erga
*** Added origin of real gamma (RV 27/11/01)
	real*8 pntrga,  ! Current point of real gamma
     +         orirga   ! Location of gamma production
	integer Strga	! generation
	integer Ptrga 	! pointer to parent
	integer uprga	! number of trans vol
	integer SFrga 	! sign of fly out
	integer nVolrga
        integer sOverflowrga    ! sign of overflow in the current event
        integer qsOverflowrga   ! quantity of the overflows in all events
        integer qOverflowrga    ! quantity of the lossed photons
                                ! in all events

	common / comrga /
     +	qrga, crga,
     +	pntrga(3,pqrga), orirga(3,pqrga), velrga(3,pqrga), erga(pqrga),
     +	nVolrga(pqrga), Strga(pqrga), Ptrga(pqrga), uprga(pqup,pqrga),
     +	SFrga(pqrga),
     +  sOverflowrga, qsOverflowrga,qOverflowrga
	save / comrga /
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

	real eg
	real*8 xleng,mleng
	integer nrga,nvol,isabs,nm_at,nmshl
	integer nmat
c	real dnst
	real rrr(100)
	integer iarrr(100),isrrr(100)
	integer ia,is
	real r,s
	real ranfl
	integer i,j,k
c       integer n
	real thr
	integer iatm,natm

		nmat=nMatVol(nvol)	

	if(nmat.eq.0)then
		isabs=0
		xleng=mleng
		return
	endif

	r=ranfl()
	if(r.gt.0.99999)then
		isabs=0
		xleng=mleng
		return
	endif
	j=qener+1
	do i=2,qener+1
		if(eg.lt.ener(i))then
			j=i-1
			go to 10
		endif
	enddo
	if(j.eq.qener+1)then
		isabs=0
		xleng=mleng
		return
	endif
10	k=0
	s=0
	do ia=1,QAtMat(nmat)
	do iatm=1,qatm
		if(Zat(AtMat(ia,nmat)).eq.charge(iatm))then
			natm=iatm
			go to 15
		endif
	enddo
	natm=0
15	do is=1,QShellAt(AtMat(ia,nmat))
c		write(oo,*)
c     +		' ia,AtMat(ia,nmat),is,ThresholdAt(is,AtMat(ia,nmat))='
c		write(oo,*)
c     +		ia,AtMat(ia,nmat),is,ThresholdAt(is,AtMat(ia,nmat))
		if(natm.eq.0)then
			thr=ThresholdAt(is,AtMat(ia,nmat))
		else
			thr=eshell(natm,is)
		endif
		if(eg.gt.thr)then
			k=k+1
			rrr(k)=PhotAt(j,is,AtMat(ia,nmat))
     +			*WeightAtMat(ia,nmat)

			iarrr(k)=ia
			isrrr(k)=is
			s=s+rrr(k)
c			write(oo,*)' PhotAt(j,is,AtMat(ia,nmat))=',
c     +				    PhotAt(j,is,AtMat(ia,nmat))
c			write(oo,*)' WeightAtMat(ia,nmat)=',
c     +				    WeightAtMat(ia,nmat)
c			write(oo,*)' s=',s
		endif
	enddo
	enddo
c	write(oo,*)(rrr(i),i=1,3)
	if(k.eq.0)then
		isabs=0
		xleng=mleng
		return
	endif	
	s=s* ElDensMat(nmat)/Z_Mean(nmat) *5.07E10
	xleng=-alog(1.0-r)/s
		
c	write(oo,*)' xleng=',xleng,' r=',r,' j=',j,' nmat=',nmat
c	write(oo,*)' k=',k,' eg=',eg,' s=',s
	if(xleng.gt.mleng)then
		isabs=0
		xleng=mleng
	else
		isabs=1
c		r=ranfl()
		call lhispre(rrr,k)
c		write(oo,*)(rrr(i),i=1,3)
		call lhisran(rrr,k,1.0,1.0,r)
c		write(oo,*)'  r=',r
		i=r
		if(i.lt.1) i=1
		if(i.gt.k)i=k
		nm_at=AtMat(iarrr(i),nmat)
		nmshl=isrrr(i)
c		write(oo,*)'  i=',i

c		write(oo,*)' nm_at=',nm_at,' nmshl=',nmshl
c		dnst=densit(nmat)

	endif

	end
