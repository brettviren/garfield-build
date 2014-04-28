CDECK  ID>, INISHL.



	subroutine Inishl

c	Initialize common comshl
c	It will be very difficult
c	Modifying is the best way to loss your temper
c	Description of channels of getting exiting from atom
c	 after photoabsorbtion and electron emission

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

	integer n

c	qatm=0		!nahui!
	qatm=2

c	Argon
	charge(1)=18
	qshl(1)=5
	eshell(1,1)=.3178E-2
	eshell(2,1)=.3135E-3
	eshell(3,1)=.2479E-3
	eshell(4,1)=.2892E-4
	eshell(5,1)=.1449E-4
	qschl(1,1)=2
	qschl(2,1)=2
	qschl(3,1)=2
	qschl(4,1)=0
	qschl(5,1)=0
	secprobch(1,1,1)=0.878
	secprobch(2,1,1)=1.0
	secprobch(1,2,1)=0.999
	secprobch(2,2,1)=1.0
	secprobch(1,3,1)=0.999
	secprobch(2,3,1)=1.0
	qsel(1,1,1)=1
	qsga(1,1,1)=0
	qsel(2,1,1)=0
	qsga(2,1,1)=1
	qsel(1,2,1)=1
	qsga(1,2,1)=0
	qsel(2,2,1)=0
	qsga(2,2,1)=1
	qsel(1,3,1)=1
	qsga(1,3,1)=0
	qsel(2,3,1)=0
	qsga(2,3,1)=1
	secenel(1,1,1,1)=eshell(1,1)-2.0*eshell(5,1)
	secenga(1,2,1,1)=eshell(1,1)-eshell(5,1)
	secenel(1,1,2,1)=eshell(2,1)-2.0*eshell(5,1)
	secenga(1,2,2,1)=eshell(2,1)-eshell(5,1)
	secenel(1,1,3,1)=eshell(3,1)-2.0*eshell(5,1)
	secenga(1,2,3,1)=eshell(3,1)-eshell(5,1)

c	Xenon
	n=2
	charge(n)=54
	qshl(n)=6
	eshell(1,n)=0.041328
c	eshell(2,n)=0.006199
	eshell(2,n)=0.0041
	eshell(3,n)=0.000827
	eshell(4,n)=0.00031
	eshell(5,n)=8.265694e-05
	eshell(6,n)=1.239854e-05
	qschl(1,n)=2
	qschl(2,n)=2
	qschl(3,n)=0
	qschl(4,n)=0
	qschl(5,n)=0
	qschl(6,n)=0
	secprobch(1,1,n)=0.106
	secprobch(2,1,n)=1.0
	secprobch(1,2,n)=0.897
	secprobch(2,2,n)=1.0
	qsel(1,1,n)=1
	qsga(1,1,n)=0
	qsel(2,1,n)=0
	qsga(2,1,n)=1
	qsel(1,2,n)=1
	qsga(1,2,n)=0
	qsel(2,2,n)=0
	qsga(2,2,n)=1
	secenel(1,1,1,n)=eshell(1,n)-2.0*eshell(6,n)
	secenga(1,2,1,n)=eshell(1,n)-eshell(6,n)
	secenel(1,1,2,n)=eshell(2,n)-2.0*eshell(6,n)
	secenga(1,2,2,n)=eshell(2,n)-eshell(6,n)


	end




	
	subroutine Prishl

c	print the featcher of the mater

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

	integer iatm, ishl, ischl, isel, isga

        if(soo.eq.0)return
	write(oo,*)
	write(oo,*)' Prishl: print materials '
	write(oo,*)' qatm=',qatm
	do iatm=1,qatm
	write(oo,*)' ****atom=',iatm
	write(oo,*)' charge()=',charge(iatm),
     +		' qshl(iatm)= ',qshl(iatm)
	do ishl=1,qshl(iatm)
	write(oo,*)' ----number of shell=',ishl
	write(oo,*)' eshell(ishl,iatm)=',eshell(ishl,iatm),
     +	' qschl(ishl,iatm)=',qschl(ishl,iatm)
	do ischl=1,qschl(ishl,iatm)
	write(oo,*)' ------number of channel=',ischl
	write(oo,*)' qsel(ischl,ishl,iatm)=',qsel(ischl,ishl,iatm),
     +	' qsga(ischl,ishl,iatm)=',qsga(ischl,ishl,iatm)
	do isel=1,qsel(ischl,ishl,iatm)
	write(oo,*)' -------- electron number ',isel
	write(oo,*)' secenel(isel,ischl,ishl,iatm)=',
     +		secenel(isel,ischl,ishl,iatm)
	enddo
	do isga=1,qsga(ischl,ishl,iatm)
	write(oo,*)' -------- photon   number ',isga
	write(oo,*)' secenga(isga,ischl,ishl,iatm)=',
     +		secenga(isga,ischl,ishl,iatm)
	enddo
	enddo
	enddo
	enddo
	

	end
