CDECK  ID>, FLDIN3.
       SUBROUTINE FLDIN3(XXC,YYC,ZZC,RRC,QINT)
*-----------------------------------------------------------------------
*   FLDIN3 - Integrates the charge in a sphere with radius RC around
*            (XC,YC,ZC).
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
       REAL XXC,YYC,ZZC,RRC,QINT
       DOUBLE PRECISION XAUX(6),XC,YC,ZC,RC,DGMLT2
       EXTERNAL FCHK2,DGMLT2
       COMMON /FCHDAT/ XC,YC,ZC,RC
*** Generate double precision copies for the common block.
       XC=DBLE(XXC)
       YC=DBLE(YYC)
       ZC=DBLE(ZZC)
       RC=DBLE(RRC)
*** Perform the integration.
       QINT=REAL(DGMLT2(FCHK2,DBLE(-PI/2),DBLE(PI/2),20,6,XAUX))/(4*PI)
       END
