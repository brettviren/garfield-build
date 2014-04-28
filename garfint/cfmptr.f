CDECK  ID>, CFMPTR.
       SUBROUTINE CFMPTR(R,THETA,RHO,PHI,N,IFAIL)
*-----------------------------------------------------------------------
*   CFMPTR - Routine transforming (r,theta) to (rho,phi) via the map
*            (r,theta)=(exp(rho),180*phi/pi). It makes entering cells
*            in polar coordinates somewhat easier.
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
       REAL R(*),THETA(*),RHO(*),PHI(*),RHOI,PHII
       INTEGER N,IFAIL,I
*** Preset error flag.
       IFAIL=0
*** Loop over the points.
       DO 10 I=1,N
       IF(R(I).EQ.0)THEN
            RHOI=-25.0
       ELSEIF(R(I).GT.0.0)THEN
            RHOI=LOG(R(I))
       ELSE
            IFAIL=1
            RHO(I)=1
            RETURN
       ENDIF
       PHII=PI*THETA(I)/180.0
       RHO(I)=RHOI
       PHI(I)=PHII
10     CONTINUE
       END
