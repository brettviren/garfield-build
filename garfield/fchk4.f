CDECK  ID>, FCHK4.
       SUBROUTINE FCHK4(M,U2,F2,X)
*-----------------------------------------------------------------------
*   FCHK4  - One of 2 auxiliary routines for calculating a flux.
*   (Last changed on 28/ 5/98.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION U2(*),F2(*),X(2),DGMLT1,
     -      X0,Y0,Z0,DX1,DY1,DZ1,DX2,DY2,DZ2,XN,YN,ZN
       INTEGER L,M,NU,NV
       EXTERNAL FCHK5,DGMLT1
       COMMON /FCHDA4/ X0,Y0,Z0,DX1,DY1,DZ1,DX2,DY2,DZ2,XN,YN,ZN,NU,NV
*** Loop over the positions.
       DO 10 L=1,M
       X(2)=U2(L)
       F2(L)=DGMLT1(FCHK5,0.0D0,1.0D0,NU,6,X)
10     CONTINUE
       END
