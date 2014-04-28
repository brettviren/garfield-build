CDECK  ID>, NORFUN.
       SUBROUTINE NORFUN(X,A,F)
*-----------------------------------------------------------------------
*   NORFUN - Auxiliary function for fitting a Gaussian.
*   (Last changed on 22/ 5/95.)
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
       DOUBLE PRECISION A(*),X,F
*** Avoid floating over and underflow.
       IF(ABS(X-A(2)).GT.5*ABS(A(3)).OR.A(3).EQ.0)THEN
            F=0.0
*** Otherwise evaluate the exponential.
       ELSE
            F=A(1)*EXP(-0.5*((X-A(2))/A(3))**2)/(SQRT(2*PI)*A(3))
       ENDIF
       END
