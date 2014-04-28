CDECK  ID>, RNDLAP.
       REAL FUNCTION RNDLAP(SIGMA)
*-----------------------------------------------------------------------
*   RNDLAP - Function generating random numbers according to a Laplace
*            distribution with width SIGMA.
*   VARIABLES : MU         : average of the random numbers.
*               SIGMA      : standard deviation of the random numbers.
*   (Last changed on  9/11/07.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IVEC,NVEC
       PARAMETER(NVEC=1000)
       REAL SIGMA,RVEC(NVEC)
       SAVE RVEC,IVEC
*** Generate new random numbers as needed.
       DATA IVEC/0/
       IF(IVEC.EQ.0.OR.IVEC+1.GT.NVEC)THEN
            CALL RANLUX(RVEC,NVEC)
            IVEC=1
       ENDIF
*** Generate a number.
       IF(1-2*ABS(RVEC(IVEC)-0.5).GT.0)THEN
            RNDLAP=SIGMA*LOG(1-2*ABS(RVEC(IVEC)-0.5))
       ELSE
            RNDLAP=SIGMA*LOG(1.0E-20)
            print *,' rvec(',ivec,') = ',rvec(ivec),' rnd = ',rndlap
       ENDIF
       IF(RVEC(IVEC).LT.0.5)RNDLAP=-RNDLAP
*** Next random number.
       IVEC=IVEC+1
       END
