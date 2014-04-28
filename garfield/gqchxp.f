CDECK  ID>, GQCHXP.
       SUBROUTINE GQCHXP(IERR,CHEXP)
*-----------------------------------------------------------------------
*   GQCHXP - Returns the current character expansion factor.
*   (Last changed on  2/ 4/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR
       REAL CHEXP
*** Return by default an expansion factor of 1.
       IERR=0
       CHEXP=1.0
       END
