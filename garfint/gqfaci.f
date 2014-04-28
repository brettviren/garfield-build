CDECK  ID>, GQFACI.
       SUBROUTINE GQFACI(IERR,ICOL)
*-----------------------------------------------------------------------
*   GQFACI - Inquiry of current fill area colour.
*   (Last changed on 29/11/97.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR,ICOL
       REAL RCOL
*** Call the HIGZ function.
       CALL IGQ('FACI',RCOL)
*** Convert to integer.
       ICOL=NINT(RCOL)
*** Set the error flag.
       IERR=0
       END
