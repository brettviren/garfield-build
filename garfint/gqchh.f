CDECK  ID>, GQCHH.
       SUBROUTINE GQCHH(IERR,CHH)
*-----------------------------------------------------------------------
*   GQCHH  - Returns the current character height.
*   (Last changed on 19/ 6/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR
       REAL CHH
*** Set the error flag.
       IERR=0
*** Call IGQ to determine the character size.
       CALL IGQ('CHHE',CHH)
       END
