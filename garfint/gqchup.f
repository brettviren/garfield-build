CDECK  ID>, GQCHUP.
       SUBROUTINE GQCHUP(IERR,XUP,YUP)
*-----------------------------------------------------------------------
*   GQCHUP - Returns the current character up vector.
*   (Last changed on 16/ 5/97.)
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
       INTEGER IERR
       REAL XUP,YUP,RANGLE
*** Set the error flag.
       IERR=0
*** Call IGQ to obtain the text orientation.
       CALL IGQ('TANG',RANGLE)
*** And compute up vector.
       XUP=COS(PI*(RANGLE+90)/180)
       YUP=SIN(PI*(RANGLE+90)/180)
       END
