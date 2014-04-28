CDECK  ID>, GQCHW.
       SUBROUTINE GQCHW(IERR,CHW)
*-----------------------------------------------------------------------
*   GQCHW  - Returns the current width.
*   (Last changed on  2/ 4/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR
       REAL CHW
*** We don't know the width.
       IERR=1
       CHW=0.01
       END
