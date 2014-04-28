CDECK  ID>, FLDIN5.
       SUBROUTINE FLDIN5(XX0,YY0,ZZ0,XX1,YY1,ZZ1,XXP,YYP,ZZP,Q,NNU,
     -      IISIGN)
*-----------------------------------------------------------------------
*   FLDIN5 - Integrates the electric field flux through a line from
*            (X0,Y0,Z0) to (X1,Y1,Z1) along a direction (XP,YP,ZP).
*   (Last changed on 14/ 5/99.)
*-----------------------------------------------------------------------
       implicit none
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       REAL XX0,YY0,ZZ0,XX1,YY1,ZZ1,XXP,YYP,ZZP,Q
       DOUBLE PRECISION X0,Y0,Z0,X1,Y1,Z1,XP,YP,ZP,XAUX(6),DGMLT1
       INTEGER NNU,NU,IISIGN,ISIGN
       EXTERNAL FCHK6,DGMLT1
       COMMON /FCHDA6/ X0,Y0,Z0,X1,Y1,Z1,XP,YP,ZP,NU,ISIGN
*** Create double precision copies of the coordinates.
       X0=DBLE(XX0)
       Y0=DBLE(YY0)
       Z0=DBLE(ZZ0)
       X1=DBLE(XX1)
       Y1=DBLE(YY1)
       Z1=DBLE(ZZ1)
*   Normalise the norm vector.
       IF(XXP**2+YYP**2+ZZP**2.GT.0)THEN
            XP=DBLE(XXP/SQRT(XXP**2+YYP**2+ZZP**2))
            YP=DBLE(YYP/SQRT(XXP**2+YYP**2+ZZP**2))
            ZP=DBLE(ZZP/SQRT(XXP**2+YYP**2+ZZP**2))
       ELSE
            PRINT *,' !!!!!! FLDIN5 WARNING : Normal vector has zero'//
     -           ' length; flux set to 0.'
            Q=0
            RETURN
       ENDIF
*   Copy number of integration intervals.
       NU=NNU
*   Copy the integration sign.
       ISIGN=IISIGN
*** Check integration points.
       IF(NU.LE.1)THEN
            PRINT *,' !!!!!! FLDIN5 WARNING : Number of points to'//
     -           ' integrate over is not > 1 ; flux set to 0.'
            Q=0
            RETURN
       ENDIF
*** Ensure the segment has non-zero length.
       IF((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2.LE.0)THEN
            PRINT *,' !!!!!! FLDIN5 WARNING : Segment has zero'//
     -           ' length; flux set to 0.'
            Q=0
            RETURN
*   Segment should be perpendicular to the norm vector.
       ELSEIF(ABS((X1-X0)*XP+(Y1-Y0)*YP+(Z1-Z0)*ZP).GT.
     -      1D-4*SQRT(((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2)*
     -      (XP**2+YP**2+ZP**2)))THEN
C      print *,' product: ',ABS((X1-X0)*XP+(Y1-Y0)*YP+(Z1-Z0)*ZP)
C      print *,' length:  ',SQRT((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2)
C      print *,' norm:    ',sqrt(XP**2+YP**2+ZP**2)
            PRINT *,' !!!!!! FLDIN5 WARNING : Segment is not'//
     -           ' perpendicular to norm vector; flux set to 0.'
            Q=0
            RETURN
       ENDIF
*** Perform the integration.
       Q=REAL(DGMLT1(FCHK6,0.0D0,1.0D0,NU,6,XAUX))*
     -      SQRT((X0-X1)**2+(Y0-Y1)**2+(Z0-Z1)**2)
       END
