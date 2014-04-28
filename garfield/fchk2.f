CDECK  ID>, FCHK2.
       SUBROUTINE FCHK2(M,U2,F2,X)
*-----------------------------------------------------------------------
*   FCHK2  - One of 2 auxiliary routines for verifying that space
*            charges indeed have the proper charge.
*   (Last changed on  8/ 4/98.)
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
       DOUBLE PRECISION U2(*),F2(*),X(2),XC,YC,ZC,RC,DGMLT1
       INTEGER L,M
       EXTERNAL FCHK1,DGMLT1
       COMMON /FCHDAT/ XC,YC,ZC,RC
*** Loop over the positions.
       DO 10 L=1,M
       X(2)=U2(L)
       F2(L)=RC**2*COS(X(2))*DGMLT1(FCHK1,0.0D0,DBLE(2*PI),20,6,X)
10     CONTINUE
       END
