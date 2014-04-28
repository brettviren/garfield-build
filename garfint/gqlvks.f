CDECK  ID>, GQLVKS.
       SUBROUTINE GQLVKS(IERR,LEVEL)
*-----------------------------------------------------------------------
*   GQLVKS - Returns the GKS level.
*   (Last changed on 17/ 6/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IERR,LEVEL
*** HIGZ is not reall a GKS, so return a non-existing value.
       IERR=0
       LEVEL=8
       END
