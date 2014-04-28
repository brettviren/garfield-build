CDECK  ID>, FDIF2N.
       SUBROUTINE FDIF2N(M,U2,F2,X)
*-----------------------------------------------------------------------
*   FDIF2N - One of 2 auxiliary routines for integrating W
*   (Last changed on 26/ 2/95.)
*-----------------------------------------------------------------------
C       implicit none
       IMPLICIT DOUBLE PRECISION(A-H,O-Z)
       DOUBLE PRECISION U2(*),F2(*),X(2),MAT(2,2)
       COMMON /DF2DAT/ MAT,SIG1,SIG2,DET,XW,YW,DW,XC,YC,ZC,C,S,FCENT
       EXTERNAL FDIF1N,DGMLT1
*** Loop over the positions.
       DO 10 L=1,M
       X(2)=U2(L)
       F2(L)=DGMLT1(FDIF1N,-5*SIG1,5*SIG1,5,6,X)
10     CONTINUE
       END
