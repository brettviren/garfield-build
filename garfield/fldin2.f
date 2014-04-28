CDECK  ID>, FLDIN2.
       SUBROUTINE FLDIN2(XXC,YYC,RRC,QINT)
*-----------------------------------------------------------------------
*   FLDIN2 - Integrates the charge in a circle with radius RC around
*            (XC,YC).
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
       REAL XXC,YYC,RRC,QINT
       DOUBLE PRECISION XAUX(6),XC,YC,ZC,RC,DGMLT1
       EXTERNAL FCHK3,DGMLT1
       COMMON /FCHDAT/ XC,YC,ZC,RC
*** Generate double precision copies for the common block.
       XC=DBLE(XXC)
       YC=DBLE(YYC)
       ZC=0.0D0
       RC=DBLE(RRC)
*** Perform the integration.
       QINT=REAL(DGMLT1(FCHK3,0.0D0,DBLE(2*PI),50,6,XAUX))/(2*PI)
       END
