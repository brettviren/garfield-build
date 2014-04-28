CDECK  ID>, GQDSP.
       SUBROUTINE GQDSP(IWKTYP,IERR,IUNIT,RX,RY,LX,LY)
*-----------------------------------------------------------------------
*   GQDSP  - Returns the screen size.
*   (Last changed on  6/ 4/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IWKTYP,IERR,IUNIT,LX,LY
       REAL RX,RY
*** We don't know this.
       IERR=1
*** Return some parameters nevertheless.
       IUNIT=1
       RX=1.0
       RY=1.0
       LX=1
       LY=1
       END
