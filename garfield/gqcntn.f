CDECK  ID>, GQCNTN.
       SUBROUTINE GQCNTN(IERR,NT)
*-----------------------------------------------------------------------
*   GQCNTN - Returns the current normalisation transformation.
*   (Last changed on 19/ 6/95.)
*-----------------------------------------------------------------------
       implicit none
       REAL AUX
       INTEGER IERR,NT
*** Set the error flag.
       IERR=0
*** Find out what the current normalisation transformation is.
       CALL IGQWK(0,'NTNB',AUX)
       NT=NINT(AUX)
       END
