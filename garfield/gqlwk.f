CDECK  ID>, GQLWK.
       SUBROUTINE GQLWK(IWKTYP,IERR,MPL,MPM,MTX,MFA,MPA,MXCOLI)
*-----------------------------------------------------------------------
*   GQLWK  - Returns properties of the workstation.
*   (Last changed on  2/ 4/95.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER IWKTYP,IERR,MPL,MPM,MTX,MFA,MPA,MXCOLI
*** Not known, but we don't really need accurate information either.
       IERR=0
*** Return generous settings.
       MPL=100
       MPM=100
       MTX=100
       MFA=100
       MPA=100
       MXCOLI=100
       END
