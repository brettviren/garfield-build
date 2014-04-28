CDECK  ID>, GQCF.
       SUBROUTINE GQCF(IWKTYP,IERR,NCOLS,ICOLS,NPRE)
*-----------------------------------------------------------------------
*   GQCF   - Returns information on colour facilities.
*   (Last changed on  2/ 4/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR,NCOLS,ICOLS,NPRE,IWKTYP
*** No idea, so return generous values.
       IERR=0
       NCOLS=10
       ICOLS=1
       NPRE=2
       END
