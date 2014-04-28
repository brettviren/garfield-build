CDECK  ID>, FLDIN4.
       SUBROUTINE FLDIN4(XX0,YY0,ZZ0,DDX1,DDY1,DDZ1,DDX2,DDY2,DDZ2,Q,
     -      NNU,NNV)
*-----------------------------------------------------------------------
*   FLDIN4 - Integrates the electric field flux through a parallelogram
*            with corners (X0,Y0,Z0), (X0+DX1,Y0+DY1,Z0+DZ1),
*            (X0+DX2,Y0+DY2,Z0+DZ2).
*   (Last changed on 28/ 5/98.)
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
       REAL XX0,YY0,ZZ0,DDX1,DDY1,DDZ1,DDX2,DDY2,DDZ2,Q
       DOUBLE PRECISION X0,Y0,Z0,DX1,DY1,DZ1,DX2,DY2,DZ2,XN,YN,ZN,
     -      XAUX(6),DGMLT2
       INTEGER NNU,NNV,NU,NV
       EXTERNAL FCHK4,DGMLT2
       COMMON /FCHDA4/ X0,Y0,Z0,DX1,DY1,DZ1,DX2,DY2,DZ2,XN,YN,ZN,NU,NV
*** Create double precision copies of the coordinates.
       X0=DBLE(XX0)
       Y0=DBLE(YY0)
       Z0=DBLE(ZZ0)
       DX1=DBLE(DDX1)
       DY1=DBLE(DDY1)
       DZ1=DBLE(DDZ1)
       DX2=DBLE(DDX2)
       DY2=DBLE(DDY2)
       DZ2=DBLE(DDZ2)
       NU=NNU
       NV=NNV
*** Check integration points.
       IF(NU.LE.1.OR.NV.LE.1)THEN
            PRINT *,' !!!!!! FLDIN4 WARNING : Number of points to'//
     -           ' integrate over is not > 1 ; flux set to 0.'
            Q=0
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ FLDIN4 DEBUG   : Number'',
     -      '' of integration points: '',2I5)') NU,NV
*** Compute the normal vector.
       XN=DY1*DZ2-DZ1*DY2
       YN=DZ1*DX2-DX1*DZ2
       ZN=DX1*DY2-DY1*DX2
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ FLDIN4 DEBUG   : Norm'',
     -      '' vector = '',3E12.5)') XN,YN,ZN
*** If this vector has zero norm, return 0 flux.
       IF(XN**2+YN**2+ZN**2.LT.1D-10*
     -      SQRT((DX1**2+DY1**2+DZ1**2)*(DX2**2+DY2**2+DZ2**2)).OR.
     -      DX1**2+DY1**2+DZ1**2.LT.1D-10*(DX2**2+DY2**2+DZ2**2).OR.
     -      DX2**2+DY2**2+DZ2**2.LT.1D-10*(DX1**2+DY1**2+DZ1**2))THEN
            PRINT *,' !!!!!! FLDIN4 WARNING : Area is not a'//
     -           ' parallelogram with non-zero area; flux set to 0.'
            Q=0
            RETURN
       ENDIF
*** Perform the integration.
       Q=REAL(DGMLT2(FCHK4,0.0D0,1.0D0,NV,6,XAUX))
       END
