CDECK  ID>, GQOPS.
       SUBROUTINE GQOPS(IOPS)
*-----------------------------------------------------------------------
*   GQOPS  - Returns the GKS operating state.
*   (Last changed on 17/ 6/95.)
*-----------------------------------------------------------------------
       INTEGER IOPS,IERR1,IERR2,NACT,NOP,IWK
*** Count number of open and active workstations.
       CALL GQACWK(0,IERR1,NACT,IWK)
       CALL GQOPWK(0,IERR2,NOP,IWK)
*** Depending on the result, return the state.
       IF(NACT.GE.1)THEN
            IOPS=3
       ELSEIF(NOP.GE.1)THEN
            IOPS=2
       ELSE
            IOPS=1
       ENDIF
       END
