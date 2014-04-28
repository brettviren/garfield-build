CDECK  ID>, DLCMIN.
       SUBROUTINE DLCMIN(XW,YW,X0,Y0,X1,Y1,DIST2,IFLAG)
*-----------------------------------------------------------------------
*   DLCMIN - Minimizes the distance between a line segment and a point.
*   VARIABLES: (XW,YW)     : Coordinates of the 'point'
*              (X0,Y0)-(X1,Y1): The track.
*              IFLAG       : -1 minimum is located before (X0,Y0),
*                             0    "     "    "    at an interior point,
*                            +1    "     "    "    behind (X1,Y1).
*              XINP0,XINP1 : Inner products.
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X0,Y0,X1,Y1,DIST2,STEP2,XINP0,XINP1,XW,YW
       INTEGER IFLAG
*** Compute the step length and check it is non-zero.
       STEP2=(X1-X0)**2+(Y1-Y0)**2
*** Check these two are non-zero.
       IF(STEP2.LE.0.0)THEN
            IFLAG=0
            DIST2=MAX(0.0D0,(X1-XW)**2+(Y1-YW)**2)
            RETURN
       ENDIF
*** Find the precise location of the smallest distance.
       XINP0=((X1-X0)*(XW-X0)+(Y1-Y0)*(YW-Y0))
       XINP1=((X0-X1)*(XW-X1)+(Y0-Y1)*(YW-Y1))
       IF(XINP0.LT.0.0D0)THEN
            IFLAG=-1
            DIST2=(XW-X0)**2+(YW-Y0)**2
       ELSEIF(XINP1.LT.0.0D0)THEN
            IFLAG=+1
            DIST2=(XW-X1)**2+(YW-Y1)**2
       ELSEIF(XINP1**2*((XW-X0)**2+(YW-Y0)**2).GT.
     -      XINP0**2*((XW-X1)**2+(YW-Y1)**2))THEN
            IFLAG=0
            DIST2=(XW-X0)**2+(YW-Y0)**2-XINP0**2/STEP2
       ELSE
            IFLAG=0
            DIST2=(XW-X1)**2+(YW-Y1)**2-XINP1**2/STEP2
       ENDIF
       END
