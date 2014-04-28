CDECK  ID>, CFMRTP.
       SUBROUTINE CFMRTP(RHO,PHI,R,THETA,N)
*-----------------------------------------------------------------------
*   CFMRTP - Routine transforming (r,theta) to (rho,phi) via the map
*            (r,theta)=(exp(rho),180*phi/pi).
*   (Last changed on 14/ 2/97.)
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
       REAL R(*),THETA(*),RHO(*),PHI(*),RI,THETAI
       INTEGER N,I
*** Loop over the points.
       DO 10 I=1,N
       RI=EXP(RHO(I))
       THETAI=180.0*PHI(I)/PI
       R(I)=RI
       THETA(I)=THETAI
10     CONTINUE
       END
