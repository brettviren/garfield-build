CDECK  ID>, ABSGAM.



	subroutine AbsGam

c	make absorption in the knowing point
c		of the all photons in the abs.inc
c	All of them are transferred to the real photons rga.inc
c		and to the delta electrons del.inc
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

c	real eg,veloc(3),abspnt(3)
c	integer numat,numshl
	integer i
	do i=ctagam,100000
		if(i.gt.qtagam)go to 10
		call lsta_abs3
     +		(i,etagam(i),rtagam(1,i),vtagam(1,i),
     +		nVolagam(i),nAtagam(i),nShlagam(i),Stagam(i),upagam(1,i))

	enddo
10	ctagam=qtagam+1
	end



	subroutine lsta_abs3(iagam,eg,abspnt,veloc,
     +		nVolagam,nAtagam,nShlagam,Stagam,upagam)

c	make absorption in the knowing point
c	and generate secondaries photons and delta electrons
c	eg - enegy of photon
c	abspnt - point of absorbtion
c	nVolagam - number of  matter
c	nAtagam - number of  atom
c	nShlagam - number of shell
c	Stagam - sign of source of this photon
c	veloc - direction of veloc.

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
c 	include 'del.inc'
c		Delta electrons

	integer pqdel		! Max. q. of electrons
	parameter (pqdel=120000)	
	integer qdel 	! Q. of electrons
C	integer cdel	! Current electron (not used, RV 27/2/97)
			! number of el. which must be treated next
	real veldel	! direction of the velocity
	real*8 pntdel	! point
	
	real zdel, edel		! charge of current electrons
			! which must be produced and energy of Delta
	integer Stdel   ! Generation number
	integer Ptdel	! pointer to parent virtual photon
	integer updel	! additional parameters
	integer SOdel           ! 1 for ouger electrons 0 for other
	integer nVoldel		! Number of volume
	real*8 rangedel		! range
	real*8 rangepdel		! practical range
	integer qstepdel	! quantity of steps of simulation
				! of stopping
        integer sOverflowDel    ! sign of overflow in the current event
        integer qsOverflowDel   ! quantity of the overflows in all events
        integer qOverflowDel    ! quantity of the lossed electrons
                                ! in all events
	integer ii1del		! not used. only for alingment.
	common / comdel /
     +	qdel, ii1del,
     +	pntdel(3,pqdel), veldel(3,pqdel),
     +	rangedel(pqdel),rangepdel(pqdel), qstepdel(pqdel),
     +	zdel(pqdel), edel(pqdel), nVoldel(pqdel),
     +	Stdel(pqdel), Ptdel(pqdel), updel(pqup,pqdel), SOdel(pqdel),
     +  sOverflowDel, qsOverflowDel,qOverflowDel
        save / comdel /

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

	integer iagam
	real eg,veloc(3)
	real*8 abspnt(3)
	integer nVolagam,nAtagam,nShlagam,Stagam,upagam(pqup)
	real eedel(pqsel),velocdel(3,pqsel)
	real eedga(pqsga),velocdga(3,pqsga)
	integer nndel,nndga
	
	integer i,j
	real s

	call lsta_abs2(eg,abspnt,veloc,nVolagam,nAtagam,nShlagam,
     +		nndel,eedel,velocdel,nndga,eedga,velocdga)

	if(nndga.gt.0.and.Stagam.eq.9999)then
		write(oo,*)' Worning of lsta_abs3:'
		write(oo,*)' too many generetion of secondary ',
     +		' photons, Stagam=',Stagam,' nndga=',nndga
		write(oo,*)' Others will be ignored'
		go to 10
	endif

	s=0.0
	do i=1,nndel
		s=s+eedel(i)
	enddo
	do i=1,nndga
		s=s+eedga(i)
	enddo
c	if(s.gt.eg)then
	if( (s-eg) .gt. 1.0e-6 * (s+eg) )then
		write(oo,*)'worning of lsta_abs3:',
     +		' break of energy preservation'
		write(oo,*)' eg=',eg,' s=',s
		write(oo,*)' nAtagam=',nAtagam,' nShlagam',nShlagam
		write(oo,*)' nndel=',nndel
		do i=1,nndel
			write(oo,*)' eedel(i)=',eedel(i)
		enddo
		do i=1,nndga
			write(oo,*)' eedga(i)=',eedga(i)
		enddo
	endif
	

	do i=1,nndga

            if(qrga .eq. pqrga)then
                qOverflowrga=qOverflowrga+1
                if(sOverflowrga.eq.0)then
                    qsOverflowrga=qsOverflowrga+1
                    sOverflowrga=1
                endif
            else

		qrga=qrga+1

c		if(qrga.eq.pqrga)then
c		    write(oo,*)' wroning lsta_abs3:',
c     +			' too much of real photons'
c		    write(oo,*)' other will  be ignored'			
c		    go to 10
c		endif
		
		Strga(qrga)=Stagam+1
		Ptrga(qrga)=iagam
		do j=1,pqup
			uprga(j,qrga)=upagam(j)
		enddo
		SFrga(qrga)=0
		do j=1,3
			pntrga(j,qrga)=abspnt(j)
*** Added origin of real gamma's (RV 27/11/01)
			orirga(j,qrga)=abspnt(j)
		enddo
		do j=1,3
			velrga(j,qrga)=velocdga(j,i)
		enddo
		erga(qrga)=eedga(i)
		nVolrga(qrga)=nVolagam
	    endif
	enddo
10	continue
c	write(oo,*)' nndel=',nndel
	do i=1,nndel
            if(qdel .eq. pqdel)then
                qOverflowDel=qOverflowDel+1
                if(sOverflowDel.eq.0)then
                    qsOverflowDel=qsOverflowDel+1
                    sOverflowDel=1
                endif
	    else

c		if(qdel.eq.pqdel)then
c		    write(oo,*)' wroning lsta_abs3:',
c     +			' too much of delta electr.'
c		    write(oo,*)' other will not be taken into account'
c		    go to 20
c		endif
		qdel=qdel+1
		Stdel(qdel)=Stagam
		Ptdel(qdel)=iagam
		do j=1,pqup
			updel(j,qdel)=upagam(j)
		enddo
		if(i.eq.1)then
			SOdel(qdel)=0
		else
			SOdel(qdel)=1
		endif
		do j=1,3
			pntdel(j,qdel)=abspnt(j)
		enddo
		do j=1,3
			veldel(j,qdel)=velocdel(j,i)
		enddo
		edel(qdel)=eedel(i)
		nVoldel(qdel)=nVolagam
		rangepdel(qdel)=0.0
		rangedel(qdel)=0.0
	    endif
	enddo



20	end

	
		


	subroutine lsta_abs2(eg,abspnt,veloc,nVolagam,nAtagam,nShlagam,
     +		nndel,eedel,velocdel,nndga,eedga,velocdga)


c	make absorption in the knowing point
c	and generate secondaries photons and delta electrons
c	eg - enegy of photon
c	abspnt - point of absorbtion
c	veloc - direction of veloc.
c	nVolagam - number of  matter
c	nAtagam - number of  atom
c	nShlagam - number of shell
c	output:
c	nndel - quantity of delta-electrons
c	eedel - enegies of the delta-electrons
c	velocdel - enegies of the delta-electrons
c	nndga,eedga,velocdga - the same for secondary photons

	implicit none

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

	real eg,veloc(3)
	real*8 abspnt(3)
	integer nVolagam,nAtagam,nShlagam
	real eedel(pqsel),velocdel(3,pqsel)
	real eedga(pqsga),velocdga(3,pqsga)
	integer nndel,nndga

	integer num
	integer numat,numshl
	integer i,j
	real r
	real hdist
	
	real ranfl

	hdist=0.0
c	if(numat.lt.0.or.numat.gt.qatm)then
c		stop 'wrong numat'
c	endif
c	if(numat.gt.0)then
c		if(numshl.lt.1.or.numshl.gt.qshl(numat))then
c			stop 'wrong numshl'
c		endif
c	endif


	num=0
c	call lsta_fmat(abspnt(3),veloc(3),num)
	nndel=0
	nndga=0
c	write(oo,*)' num=',num
	if(nVolagam.eq.0)then
		return
	endif
	
	nndel=1
	do i=1,3
		velocdel(i,nndel)=veloc(i)
	enddo
	do i=1,qatm
c		write(oo,*)' Zat(nAtagam)',Zat(nAtagam)
c		write(oo,*)' charge(i)',charge(i)
		if(Zat(nAtagam).eq.charge(i))then
			numat=i
			go to 5
		endif
	enddo
c		The place of question
c		Several lines was commented
		eedel(nndel)=eg-ThresholdAt(nShlagam,nAtagam)
		if(eedel(nndel).le.0.0)then
			hdist=-eedel(nndel)
			eedel(nndel)=0.0
		endif
c
c	write(oo,*)' nShlagam=',nShlagam,
c     +	' QShellAt(nAtagam)=',QShellAt(nAtagam)
	if(nShlagam.lt.QShellAt(nAtagam))then
		nndel=nndel+1
		eedel(nndel)=ThresholdAt(nShlagam,nAtagam)-hdist-
     +		2.0*ThresholdAt(QShellAt(nAtagam),nAtagam)
c		eedel(nndel)=ThresholdAt(nShlagam,nAtagam)-hdist
		if(eedel(nndel).le.0.0)then
				nndel=nndel-1
				goto 2
		endif
		call sfersim(velocdel(1,nndel))
	endif
2	continue
	return

5	continue

c		asumed that the last shell is zero energy or 1 eV
c	if(nAtagam.ne.0)then
	 	eedel(nndel)=eg-eshell(nShlagam,numat)
c	write(oo,*)' eg=',eg,' nShlagam=',nShlagam,' numat=',numat
c	write(oo,*)' eedel(nndel)=',eedel(nndel)
c	else
c		eedel(nndel)=eg-20.0e-6	!avarege energy of last shell
c	endif

	if(eedel(nndel).le.0.0)then
		hdist=-eedel(nndel)
		eedel(nndel)=0.0
	endif
	
c	if(numat.gt.0)then
	numshl=nShlagam
	if(qschl(numshl,numat).gt.0)then


		r=ranfl()
		j=qschl(numshl,numat)

		if(j.gt.0)then
			j=qschl(numshl,numat)
			do i=1,	qschl(numshl,numat)
				if(r.lt.secprobch(i,numshl,numat))then
					j=i
					go to 10
				endif
			enddo
10			continue
c			write(oo,*)' prob: r=',r,' j=',j

			do i=1,qsel(j,numshl,numat)
				nndel=nndel+1
				eedel(nndel)=secenel(i,j,numshl,numat)
     +				-hdist
				if(eedel(nndel).lt.0)then
					hdist=-eedel(nndel)
					eedel(nndel)=0.0
				else
					hdist=0.0
				endif
				call sfersim(velocdel(1,nndel))
			enddo
			do i=1,qsga(j,numshl,numat)
				nndga=nndga+1
				eedga(nndga)=secenga(i,j,numshl,numat)
     +				-hdist
				if(eedga(nndga).lt.0)then
					hdist=-eedga(nndga)
					eedga(nndga)=0.0
				else
					hdist=0.0
				endif
				call sfersim(velocdga(1,nndga))
			enddo

		endif
	else
		if(nShlagam.lt.QShellAt(nAtagam))then
			nndel=nndel+1
			eedel(nndel)=eshell(nShlagam,numat)-hdist-
     +			2.0*eshell(qshl(numat),numat)
			if(eedel(nndel).le.0.0)then
				nndel=nndel-1
				goto 20
			endif
			call sfersim(velocdel(1,nndel))
		endif
20		continue

	endif		
	
c	endif
	
	end
