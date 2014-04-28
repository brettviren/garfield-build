CDECK  ID>, RNDPOL.
       REAL FUNCTION RNDPOL(THETA)
*-----------------------------------------------------------------------
*   RNDPOL - Generates random numbers according to a Polya distribution
*            with parameter THETA. Since this is simply a scaled Gamma
*            distribution with parameter 1+THETA, RNGAMA (V135) is used.
*   (Last changed on  6/ 7/95.)
*-----------------------------------------------------------------------
       implicit none
       REAL RNGAMA,THETA
       EXTERNAL RNGAMA
*** Verify the parameter.
       IF(THETA.GT.-1)THEN
            RNDPOL=RNGAMA(1+THETA)/(1+THETA)
       ELSE
            RNDPOL=0
       ENDIF
       END
