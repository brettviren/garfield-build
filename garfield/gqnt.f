CDECK  ID>, GQNT.
       SUBROUTINE GQNT(NT,IERR,WINDOW,VIEWPT)
*-----------------------------------------------------------------------
*   GQNT   - Returns information about normalisation transformations.
*   (Last changed on 19/ 6/95.)
*-----------------------------------------------------------------------
       implicit none
       REAL WINDOW(4),VIEWPT(4)
       INTEGER IERR,NT
*** Call IGQWK to find out.
       CALL IGQWK(0,'NTWN',WINDOW)
       CALL IGQWK(0,'NTVP',VIEWPT)
*** Set the error indicator.
       IERR=0
       END
