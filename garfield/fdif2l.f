CDECK  ID>, FDIF2L.
       SUBROUTINE FDIF2L(M,U2,F2,X)
*-----------------------------------------------------------------------
*   FDIF2L - One of 2 auxiliary routines for integrating t_mean
*   (Last changed on 25/ 2/95.)
*-----------------------------------------------------------------------
C       implicit none
       IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       DOUBLE PRECISION U2(*),F2(*),X(2),MAT(2,2)
       COMMON /DF2DAT/ MAT,SIG1,SIG2,DET,XW,YW,DW,XC,YC,ZC,C,S,FCENT
       EXTERNAL FDIF1L,DGMLT1
*** Loop over the positions.
       DO 10 L=1,M
*   Copy the y component of the position.
       X(2)=U2(L)
*   Evaluate the integral over x.
       F2(L)=DGMLT1(FDIF1L,-5*SIG1,5*SIG1,5,6,X)
10     CONTINUE
       END
