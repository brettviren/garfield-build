CDECK  ID>, MSNFUN.
       SUBROUTINE MSNFUN(X,A,F)
*-----------------------------------------------------------------------
*   MSNFUN - Auxiliary function for fitting a Mathieson distribution.
*   (Last changed on 17/ 4/97.)
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
       DOUBLE PRECISION A(6),X,F,K1,K2,K3,K4,D,XC,L1,L2,FACTOR,XMIN,
     -      STRIP,S
*** Check for illegal values of K3.
       IF(A(3).LT.0)THEN
            F=0
            RETURN
       ENDIF
*** Compute the various K's.
       XC=A(1)
       FACTOR=A(2)
       K3=A(3)
       K2=PI*(1-SQRT(K3)/2)/2
       K1=K2*SQRT(K3)/(4*ATAN(SQRT(K3)))
       K4=K1/(K2*SQRT(K3))
       D=A(4)
       XMIN=A(5)
       S=A(6)
*** Determine integration range.
       STRIP=DINT((X-XMIN)/D)
       IF(STRIP.LT.0.5)STRIP=STRIP-1
       L1=((XMIN-XC)+STRIP*D)/S
       L2=((XMIN-XC)+(STRIP+1)*D)/S
*** Compute function.
       F=2*FACTOR*K4*(ATAN(SQRT(K3)*TANH(K2*L2))-
     -      ATAN(SQRT(K3)*TANH(K2*L1)))
       END
