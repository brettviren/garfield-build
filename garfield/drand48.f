CDECK  ID>, DRAND48.
       DOUBLE PRECISION FUNCTION drand48(DUMMY)
*-----------------------------------------------------------------------
*   RNDM2  - Returns double precision random numbers by calling RM48.
*   (Last changed on 26/10/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER NVEC
       PARAMETER(NVEC=1000)
       DOUBLE PRECISION RVEC(NVEC),DUMMY
       INTEGER IVEC
       SAVE RVEC,IVEC
       DATA IVEC/0/
*** Now generate random number between 0 and one.
       IF(IVEC.EQ.0.OR.IVEC.GE.NVEC)THEN
            CALL RM48(RVEC,NVEC)
            IVEC=1
       ELSE
            IVEC=IVEC+1
       ENDIF
*** Assign result.
       drand48=RVEC(IVEC)
       END
