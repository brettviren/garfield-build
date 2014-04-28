CDECK  ID>, RNDEXP.
       REAL FUNCTION RNDEXP(A)
*-----------------------------------------------------------------------
*   RNDEXP - Function returning a randomly distributed number from an
*            exponential distribution with parameter A.
*   VARIABLES : X          : A homogeneously distributed number.
*               A          : Expectation value of the distribution.
*   (Last changed on 17/10/95.)
*-----------------------------------------------------------------------
       PARAMETER(NVEC=100)
       REAL RVEC(NVEC)
       INTEGER IVEC
       SAVE RVEC,IVEC
       DATA IVEC/0/
*** Return here if we got by accident an end-point (should not happen).
10     CONTINUE
*   Get a random number.
       IF(IVEC.EQ.0.OR.IVEC.GE.NVEC)THEN
            CALL RANLUX(RVEC,NVEC)
            IVEC=1
       ELSE
            IVEC=IVEC+1
       ENDIF
       X=RVEC(IVEC)
*   Check the value we got.
       IF(X.LE.0.0.OR.X.GT.1.0)GOTO 10
*   And assign.
       RNDEXP=-A*LOG(X)
       END
