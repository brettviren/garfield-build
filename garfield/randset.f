CDECK  ID>, RANDSET.
	subroutine randset
c
c	set the start point
c
        implicit none

c 	include  'random.inc'
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	

	call ranset(rseed)

	end

	subroutine randget
c
c	get the current point
c
        implicit none

c 	include  'random.inc'
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	

	call ranget(rseed)

	end

	subroutine randpri(oo)
c
c	print the current point
c
        implicit none

	integer oo

c 	include  'random.inc'
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	

        write(oo,*)'seed=',seed
	
	end
	

        subroutine Priranfl

c       It is called at the end of program

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

c 	include  'random.inc'
        real*8 iranfl

        integer sseed              ! Flag to start first event
                                   ! from seed point of random number generator.
        real*8 rseed               ! Place for seed.
        integer seed(2)            ! Form for writting and inputting
                                   ! without modification during
                                   ! binary to demical transformation.
        equivalence (rseed,seed(1))

        common / comran /
     +  iranfl,
     +  rseed, sseed

        save / comran /
	

c	real*8 iranfl
c	common / comran / iranfl
c	save / comran /

	if(soo.eq.0)return
	write(oo,*)
	write(oo,*)'  Priranfl:     iranfl=',iranfl

	end



      SUBROUTINE LRANOR(A,B)
C.
c	Copy of the geant321 routine GRANOR for ranfl generator
C.    ******************************************************************
C.    *                                                                *
C.    *       To generate 2 numbers A and B following a NORMAL         *
C.    *       distribution (mean=0 sigma=1.)                           *
C.    *         Copy of the CERN Library routine RANNOR                *
C.    *                                                                *
C.    *    ==>Called by : <USER>, many GEANT routines                  *
C.    *       Author    F.Carminati *********                          *
C.    *                                                                *
C.    ******************************************************************
C.
*      DIMENSION RNDM(2)
*
*      CALL GRNDM(RNDM,2)
      Y=ranfl()
      Z=ranfl()
      X=6.283185*Z
      A1=SQRT (-2.0*LOG(Y))
      A=A1*SIN (X)
      B=A1*COS (X)
      RETURN
      END

      SUBROUTINE LSPOIS (AMU,N,IERROR)
C
c       This is modified library routine poissn.
c       One or two errors was corrected here.
c
C    POISSON GENERATOR
C    CODED FROM LOS ALAMOS REPORT      LA-5061-MS
C    PROB(N)=EXP(-AMU)*AMU**N/FACT(N)
C        WHERE FACT(N) STANDS FOR FACTORIAL OF N
C    ON RETURN IERROR.EQ.0 NORMALLY
C              IERROR.EQ.1 IF AMU.LE.0.
C
	SAVE	!my correction
      DATA AMUOL/-1./
      DATA AMAX/100./
c	write(6,*)' amu=',amu
      IERROR=0	!my correction
      IF(AMU.GT.AMAX) GO TO 500
      IF(AMU.EQ.AMUOL) GO TO 200
      IF(AMU.GT.0.) GO TO 100
C    MEAN SHOULD BE POSITIVE
      IERROR=1
      N = 0
      RETURN
C    SAVE EXPONENTIAL FOR FURTHER IDENTICAL REQUESTS
  100 IERROR=0
      AMUOL=AMU
      EXPMA=EXP(-AMU)
  200 PIR=1.
c	write(6,*)' ierror=',ierror
      N=-1
  300 N=N+1
c      PIR=PIR*RNDM(N)
      PIR=PIR*ranfl()
      IF(PIR.GT.EXPMA) GO TO 300
      RETURN
C   NORMAL APPROXIMATION FOR AMU.GT.AMAX
  500 CALL LRANOR(RAN,DUMMY)
      N=RAN*SQRT(AMU)+AMU+.5
      RETURN
C   ENTRY FOR USER TO SET AMAX, SWITCHOVER POINT TO NORMAL APPROXIMATION
      ENTRY lPOISET(AMU)
      PRINT 1001,AMU
 1001 FORMAT(77H POISSON RANDOM NUMBER GENERATOR TO SWITCH TO NORMAL APP
     CROXIMATION ABOVE AMU= ,F12.2)
      AMAX=AMU
      RETURN
      END


      SUBROUTINE lHISRAN(Y,N,XLO,XWID,XRAN)

c	corrected for working with  program HEED

C         SUBROUTINE TO GENERATE RANDOM NUMBERS
C         ACCORDING TO AN EMPIRICAL DISTRIBUTION
C         SUPPLIED BY THE USER IN THE FORM OF A HISTOGRAM
C         F. JAMES,    MAY, 1976
      DIMENSION Y(*)
      DATA IERR,NTRY,NXHRAN,NXHPRE/0,3HRAN,3HRAN,3HPRE/
      IF(Y(N).EQ.1.0) GOTO 200
      WRITE(6,1001) Y(N)
 1001 FORMAT('0SUBROUTINE HISRAN FINDS Y(N) NOT EQUAL TO 1.0     Y(N)='
     +,E15.6/' ASSUMES USER HAS SUPPLIED HISTOGRAM RATHER THAN CUMUL',
     +'ATIVE DISTRIBUTION AND HAS FORGOTTEN TO CALL lHISPRE'/)
      NTRY=NXHRAN
      GOTO 50
C         INITIALIZE HISTOGRAM TO FORM CUMULATIVE DISTRIBUTION
C+SELF,IF=CDC,IF=F4.
C      ENTRY lHISPRE
C+SELF,IF=-CDC,-F4.
      ENTRY lHISPRE(Y,N)
C+SELF.
      NTRY=NXHPRE
   50 CONTINUE
      YTOT = 0.
      DO 100 I= 1, N
      IF(Y(I).LT.0.) GOTO 900
      YTOT = YTOT + Y(I)
  100 Y(I) = YTOT
      IF(YTOT.LE.0.) GOTO 900
      YINV = 1.0/YTOT
      DO 110 I= 1, N
  110 Y(I) = Y(I) * YINV
      Y(N) = 1.0
      IF(NTRY.EQ.NXHPRE) RETURN
C         NOW GENERATE RANDOM NUMBER BETWEEN 0 AND ONE
  200 CONTINUE
c      YR = RNDM(-1)
	YR=ranfl()
C         AND TRANSFORM IT INTO THE CORRESPONDING X-VALUE
      L = LOCATF(Y,N,YR)
      IF(L.EQ.0) GOTO 240
      IF(L.GT.0) GOTO 250
C         USUALLY COME HERE.
      L = ABS(L)
      XRAN = XLO + XWID * (L +((YR-Y(L))/(Y(L+1)-Y(L))))
      RETURN
C         POINT FALLS IN FIRST BIN.  SPECIAL CASE
  240 XRAN = XLO + XWID * (YR/Y(1))
      RETURN
C         GUARD AGAINST SPECIAL CASE OF FALLING ON EMPTY BIN
  250 XRAN = XLO + L * XWID
      RETURN
  900 CONTINUE
      IERR = IERR + 1
      IF(IERR.LT.6) WRITE(6,1000)NTRY
      IF(L.GT.0) GOTO 250
      IF(NTRY.EQ.NXHPRE) RETURN
 1000 FORMAT('0ERROR IN INPUT DATA FOR HIS',A3,' VALUES NOT ALL >=0'/)
      WRITE(6,1002) (Y(K),K=1,N)
 1002 FORMAT(1X,10F13.7)
      XRAN = 0.
      RETURN
      END
