CDECK  ID>, GRSCRH.
       SUBROUTINE GRSCR(IWKID,ICOL,RED,GREEN,BLUE)
*-----------------------------------------------------------------------
*   GRSCR  - Sets a colour representation.
*   GRQCR  - Query of a colour representation.
*   (Last changed on 18/ 5/96.)
*-----------------------------------------------------------------------
       INTEGER IWKID,ICOL,IERR,IFLAG,MXCOL
       PARAMETER(MXCOL=100)
       REAL RED,GREEN,BLUE,RGB(MXCOL,3)
       LOGICAL COLSET(MXCOL)
       SAVE RGB,COLSET
       DATA RGB /MXCOL*0,MXCOL*0,MXCOL*0/,
     -      COLSET /MXCOL*.FALSE./
*** Setting colours: if index makes sense, store it.
       IF(ICOL.GE.1.AND.ICOL.LE.MXCOL)THEN
            RGB(ICOL,1)=RED
            RGB(ICOL,2)=GREEN
            RGB(ICOL,3)=BLUE
            COLSET(ICOL)=.TRUE.
       ENDIF
*   At any rate pass on to HIGZ.
       CALL ISCR(IWKID,ICOL,RED,GREEN,BLUE)
       RETURN
*** Queries on colour.
       ENTRY GRQCR(IWKID,ICOL,IFLAG,IERR,RED,GREEN,BLUE)
*   If within range, return colour setting.
       IF(ICOL.EQ.0)THEN
            RED=1
            GREEN=1
            BLUE=1
            IERR=0
       ELSEIF(ICOL.EQ.1)THEN
            RED=0
            GREEN=0
            BLUE=0
            IERR=0
       ELSEIF(ICOL.GE.1.AND.ICOL.LE.MXCOL)THEN
            RED=RGB(ICOL,1)
            GREEN=RGB(ICOL,2)
            BLUE=RGB(ICOL,3)
            IF(COLSET(ICOL))THEN
                 IERR=0
            ELSE
                 IERR=1
            ENDIF
*   Otherwise don't.
       ELSE
            RED=0
            GREEN=0
            BLUE=0
            IERR=1
       ENDIF
       END
