CDECK  ID>, INIDEL.
	subroutine Inidel
c
c	Initialize the delta eleectrons
c
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


	qdel=0
	sOverflowDel=0
	if(nevt.eq.1)then
		qOverflowDel=0
		qsOverflowDel=0
	endif

	end


	subroutine WorPridel

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


c	integer i,j

        if(nevt.eq.qevt)then

	if(qOverflowDel.gt.0)then
	write(oo,*)
        write(oo,*)' WorPridel: overflow of delta electrons arrays '
        write(oo,*)' sOverflowDel   qsOverflowDel   qOverflowDel'
        write(oo,*)sOverflowDel,qsOverflowDel,qOverflowDel
	endif

	endif

	end

	subroutine Pridel

c	print the delta electrons

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


	integer i,j

        if(soo.eq.0)return
	write(oo,*)
	write(oo,*)' Pridel: delta electron'
        write(oo,*)' sOverflowDel   qsOverflowDel   qOverflowDel'
        write(oo,*)sOverflowDel,qsOverflowDel,qOverflowDel

	write(oo,*)' qdel= ',qdel
	if(qdel.gt.0)then
	write(oo,*)
     +	' ndel zdel   edel nVoldel Stdel ',
     +	'Ptdel updel(1) SOdel',
     +	' rangepdel rangedel qstep'
	write(oo,*)
     +	' pntdel(1,i)  pntdel(2,i)  pntdel(3,i) ',
     +	' veldel(1,i)  veldel(2,i)  veldel(3,i)  '
	do i=1,qdel
	write(oo,
     + '(1X,I5,2(1X,e10.5),1(1X,I3),1(1X,I5),3(1X,I3),2(1X,E9.4),I6)')
     +		i,zdel(i),edel(i),nVoldel(i),Stdel(i),Ptdel(i),
     +		updel(1,i),
     +		SOdel(i),rangepdel(i),rangedel(i),qstepdel(i)
		write(oo,'(6(1X,e12.5))')(pntdel(j,i),j=1,3),
     +		(veldel(j,i),j=1,3)
	enddo
	endif

	end
