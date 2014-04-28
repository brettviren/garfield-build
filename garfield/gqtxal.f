CDECK  ID>, GQTXAL.
       SUBROUTINE GQTXAL(IERR,ITXALH,ITXALV)
*-----------------------------------------------------------------------
*   GQTXAL - Returns the current text alignment.
*   (Last changed on 19/ 6/95.)
*-----------------------------------------------------------------------
       REAL RVAL(2)
       INTEGER IERR,ITXALH,ITXALV
*** Set the error flag.
       IERR=0
*** Inquire.
       CALL IGQ('TXAL',RVAL)
*** Set the alignments.
       ITXALH=RVAL(1)
       ITXALV=RVAL(2)
       END
