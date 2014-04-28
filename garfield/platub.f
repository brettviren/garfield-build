CDECK  ID>, PLATUB.
       SUBROUTINE PLATUB(R,NTUBE,ZMIN,ZMAX)
*-----------------------------------------------------------------------
*   PLATUB - Cross section between a plane and a tube.
*   (Last changed on 19/11/97.)
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
       INTEGER NMAX
       PARAMETER(NMAX=200)
       DOUBLE PRECISION XPL(NMAX),YPL(NMAX),R,ZMIN,ZMAX,
     -      X1,Y1,X2,Y2,XCUT,YCUT
       INTEGER NTUBE,NPL,I,N
       LOGICAL CUT
*** Ensure the radius is not zero, and the number of corners reasonable.
       IF(R.LE.0.OR.NTUBE.LT.0)THEN
            PRINT *,' !!!!!! PLATUB WARNING : Receiving invalid'//
     -           ' tube parameters; tube not plotted.'
            RETURN
       ENDIF
*** Check that the receiving array is large enough.
       IF(NMAX.LT.NTUBE+1.AND.NTUBE.GT.0)THEN
            PRINT *,' !!!!!! PLATUB WARNING : Plot vector'//
     -           ' is too small; tube not plotted.'
            RETURN
       ENDIF
*** Number of corners.
       IF(NTUBE.EQ.0)THEN
            N=NMAX-1
       ELSE
            N=NTUBE
       ENDIF
*** Go around the polygon or circle, initialise on first edge.
       X1=R
       Y1=0
       CALL PLACUT(X1,Y1,ZMIN,X1,Y1,ZMAX,XCUT,YCUT,CUT)
*   Loop over the edges.
       DO 10 I=1,N
       IF(CUT)THEN
            NPL=1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
       ELSE
            NPL=0
       ENDIF
*   New edge.
       X2=R*COS(2.0D0*PI*DBLE(I)/DBLE(N))
       Y2=R*SIN(2.0D0*PI*DBLE(I)/DBLE(N))
*   Cut along the bottom lid.
       CALL PLACUT(X1,Y1,ZMIN,X2,Y2,ZMIN,XCUT,YCUT,CUT)
       IF(CUT)THEN
            NPL=NPL+1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
       ENDIF
*   Cut along the top lid.
       CALL PLACUT(X1,Y1,ZMAX,X2,Y2,ZMAX,XCUT,YCUT,CUT)
       IF(CUT)THEN
            NPL=NPL+1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
       ENDIF
*   Cut along the edge.
       CALL PLACUT(X2,Y2,ZMIN,X2,Y2,ZMAX,XCUT,YCUT,CUT)
       IF(CUT)THEN
            NPL=NPL+1
            XPL(NPL)=XCUT
            YPL(NPL)=YCUT
       ENDIF
*   Plot in case a one of the lids has been crossed.
       IF(NPL.GE.2)CALL GRLIN2(NPL,XPL,YPL)
*   Shift the point.
       X1=X2
       Y1=Y2
10     CONTINUE
       END
