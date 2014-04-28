CDECK  ID>, PRIRAFFL.
        subroutine PriRaffle

c       print the virt. ioniz.  photons

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

c         include 'raffle.inc'
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

        integer i

        if(soo.eq.0)return
        write(oo,*)
        write(oo,*)' PriRaffle: virt. ioniz.  photons'
        write(oo,*)' QGRaf= ',QGRaf,' ESGRaf=',ESGRaf
        if(QGRaf.gt.0)then
        write(oo,*)'  EGRaf(i)  NAtGRaf(i)  NShAtGRaf(i)'
        do i=1,QGRaf
                write(oo,'(1X,e12.5,2(i12))')
     +          EGRaf(i),  NAtGRaf(i),  NShAtGRaf(i)
        enddo
        endif

        end
