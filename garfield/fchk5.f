CDECK  ID>, FCHK5.
       SUBROUTINE FCHK5(M,U1,F1,X)
*-----------------------------------------------------------------------
*   FCHK5  - One of 2 auxiliary routines for calculating a flux.
*   (Last changed on 28/ 5/98.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION U1(*),F1(*),X(2),
     -      X0,Y0,Z0,DX1,DY1,DZ1,DX2,DY2,DZ2,XN,YN,ZN
       REAL XF,YF,ZF,EX,EY,EZ,ETOT,VOLT
       INTEGER ILOC,L,M,NU,NV
       COMMON /FCHDA4/ X0,Y0,Z0,DX1,DY1,DZ1,DX2,DY2,DZ2,XN,YN,ZN,NU,NV
*** Loop over the positions.
       DO 10 L=1,M
       X(1)=U1(L)
       XF=X0+X(1)*DX1+X(2)*DX2
       YF=Y0+X(1)*DY1+X(2)*DY2
       ZF=Z0+X(1)*DZ1+X(2)*DZ2
       CALL EFIELD(XF,YF,ZF,EX,EY,EZ,ETOT,VOLT,0,ILOC)
       F1(L)=EX*XN+EY*YN+EZ*ZN
10     CONTINUE
       END
