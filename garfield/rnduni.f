CDECK  ID>, RNDUNI.
       REAL FUNCTION RNDUNI(SCALE)
*-----------------------------------------------------------------------
*   RNDUNI - Function generating random numbers according to a uniform
*            distribution over the range <0,SCALE>, end-points are
*            excluded.
*   VARIABLES : SCALE      : upper limit of range of the distribution.
*   (Last changed on  6/10/00.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IVEC,MXVEC
       PARAMETER(MXVEC=1000)
       REAL SCALE,RVEC(MXVEC)
       SAVE RVEC,IVEC
       DATA IVEC/0/
       IF(IVEC.EQ.0.OR.IVEC+1.GT.MXVEC)THEN
            CALL RANLUX(RVEC,MXVEC)
            IVEC=1
       ENDIF
       RNDUNI=SCALE*RVEC(IVEC)
       IVEC=IVEC+1
       END
