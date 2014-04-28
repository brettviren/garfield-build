CDECK  ID>, RNDINI.
       SUBROUTINE RNDINI(NRNDM)
*-----------------------------------------------------------------------
*   RNDINI - Calls all random number generators a number of times.
*   VARIABLES : NRNDM      : Number of calls.
*   (Last changed on  7/ 4/06.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER NRNDM,I
       REAL RVEC(1),RNDM,RANFL,DUMMY
       DOUBLE PRECISION DVEC(1)
*** Loop.
       DUMMY=0
       DO 10 I=1,NRNDM
*   RANLUX: Underlying generator for most generators
       CALL RANLUX(RVEC,1)
*   RANMAR: Used by RNORML
       CALL RANMAR(RVEC,1)
*   RANFL: Used by Heed
       DUMMY=DUMMY+RANFL()
*   RM48: Used by Magboltz
       CALL RM48(DVEC,1)
*   RNDM: Used for non-critical purposes
       DUMMY=DUMMY+RNDM(I)+RVEC(1)
10     CONTINUE
*** Print.
       PRINT *,' ------ RNDINI MESSAGE : Random number generators'//
     -      ' have been called ',NRNDM,' times.'
       END
