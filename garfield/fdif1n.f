CDECK  ID>, FDIF1N.
       SUBROUTINE FDIF1N(M,U1,F1,X)
*-----------------------------------------------------------------------
*   FDIF1N - One of 2 auxiliary routines for integrating t_mean
*   (Last changed on 26/ 2/95.)
*-----------------------------------------------------------------------
C       implicit none
       IMPLICIT DOUBLE PRECISION(A-H,O-Z)
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
       DOUBLE PRECISION U1(*),F1(*),X(2),MAT(2,2)
       COMMON /DF2DAT/ MAT,SIG1,SIG2,DET,XW,YW,DW,XC,YC,ZC,C,S,FCENT
*** Loop over the positions.
       DO 10 L=1,M
       X(1)=U1(L)
       ARG=-0.5*(X(1)**2*MAT(2,2)+X(2)**2*MAT(1,1)-
     -      2*X(1)*X(2)*MAT(1,2))/DET
       IF(ARG.LT.-50)THEN
            W=0.0
       ELSE
            W=EXP(ARG)/(2*PI*SQRT(DET))
       ENDIF
       F1(L)=W
10     CONTINUE
       END
