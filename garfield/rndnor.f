CDECK  ID>, RNDNOR.
       REAL FUNCTION RNDNOR(AVER,SIGMA)
*-----------------------------------------------------------------------
*   RNDNOR - Function generating random numbers according to a normal
*            distribution with expected value MU and standard deviation
*            SIGMA.
*   VARIABLES : MU         : average of the random numbers.
*               SIGMA      : standard deviation of the random numbers.
*   (Last changed on  15/ 9/99.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IVEC,MXVEC
       PARAMETER(MXVEC=1000)
       REAL AVER,SIGMA,RVEC(MXVEC)
       SAVE RVEC,IVEC
       DATA IVEC/0/
       IF(IVEC.EQ.0.OR.IVEC+1.GT.MXVEC)THEN
            CALL RNORML(RVEC,MXVEC)
            IVEC=1
       ENDIF
       RNDNOR=AVER+SIGMA*RVEC(IVEC)
       IVEC=IVEC+1
       END
