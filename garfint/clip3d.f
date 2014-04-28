CDECK  ID>, CLIP3D.
       SUBROUTINE CLIP3D(X0,Y0,Z0,X1,Y1,Z1,
     -      XLL,YLL,ZLL,XUR,YUR,ZUR,IFAIL)
*-----------------------------------------------------------------------
*   CLIP3D - Routine clipping the line (X0,Y0,Z0) to (X1,Y1,Z1) to the
*            size of the box formed by (XLL,YLL,ZLL) (XUR,YUR,ZUR).
*   VARIABLES : (X0,Y0,Z0) : Begin point of line.
*               (X1,Y1,Z1) : End point of line.
*               (X/Y/ZLL)  : Lower left hand corner of the box.
*               (X/Y/ZUR)  : Upper right hand corner of the box.
*   (Last changed on  6/12/97.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X0,Y0,Z0,X1,Y1,Z1,XLL,YLL,ZLL,XUR,YUR,ZUR
       INTEGER IFAIL
*** Return on IFAIL=0 if no changes have to be made.
       IFAIL=0
       IF(XLL.LE.X0.AND.X0.LE.XUR.AND.XLL.LE.X1.AND.X1.LE.XUR.AND.
     -      YLL.LE.Y0.AND.Y0.LE.YUR.AND.YLL.LE.Y1.AND.Y1.LE.YUR.AND.
     -      ZLL.LE.Z0.AND.Z0.LE.ZUR.AND.ZLL.LE.Z1.AND.Z1.LE.ZUR)RETURN
*** The next few returns are on IFAIL=1.
       IFAIL=1
*** Return with IFAIL=1 if X0 and X1 are out of range.
       IF((X0.LT.XLL.AND.X1.LT.XLL).OR.(X0.GT.XUR.AND.X1.GT.XUR))RETURN
       IF(X0.NE.X1)THEN
*   Adjust X0.
            IF(X0.LT.XLL)THEN
                 Y0=Y0+((Y1-Y0)/(X1-X0))*(XLL-X0)
                 Z0=Z0+((Z1-Z0)/(X1-X0))*(XLL-X0)
                 X0=XLL
            ENDIF
            IF(X0.GT.XUR)THEN
                 Y0=Y0+((Y1-Y0)/(X1-X0))*(XUR-X0)
                 Z0=Z0+((Z1-Z0)/(X1-X0))*(XUR-X0)
                 X0=XUR
            ENDIF
*   Adjust X1.
            IF(X1.LT.XLL)THEN
                 Y1=Y1+((Y1-Y0)/(X1-X0))*(XLL-X1)
                 Z1=Z1+((Z1-Z0)/(X1-X0))*(XLL-X1)
                 X1=XLL
            ENDIF
            IF(X1.GT.XUR)THEN
                 Y1=Y1+((Y1-Y0)/(X1-X0))*(XUR-X1)
                 Z1=Z1+((Z1-Z0)/(X1-X0))*(XUR-X1)
                 X1=XUR
            ENDIF
       ENDIF
*** Return with an IFAIL=1 if Y0 and Y1 are out of range.
       IF((Y0.LT.YLL.AND.Y1.LT.YLL).OR.(Y0.GT.YUR.AND.Y1.GT.YUR))RETURN
       IF(Y0.NE.Y1)THEN
*   Adjust Y0.
            IF(Y0.LT.YLL)THEN
                 X0=X0+((X1-X0)/(Y1-Y0))*(YLL-Y0)
                 Z0=Z0+((Z1-Z0)/(Y1-Y0))*(YLL-Y0)
                 Y0=YLL
            ENDIF
            IF(Y0.GT.YUR)THEN
                 X0=X0+((X1-X0)/(Y1-Y0))*(YUR-Y0)
                 Z0=Z0+((Z1-Z0)/(Y1-Y0))*(YUR-Y0)
                 Y0=YUR
            ENDIF
*   Adjust Y1.
            IF(Y1.LT.YLL)THEN
                 X1=X1+((X1-X0)/(Y1-Y0))*(YLL-Y1)
                 Z1=Z1+((Z1-Z0)/(Y1-Y0))*(YLL-Y1)
                 Y1=YLL
            ENDIF
            IF(Y1.GT.YUR)THEN
                 X1=X1+((X1-X0)/(Y1-Y0))*(YUR-Y1)
                 Z1=Z1+((Z1-Z0)/(Y1-Y0))*(YUR-Y1)
                 Y1=YUR
            ENDIF
       ENDIF
*** Return with an IFAIL=1 if Z0 and Z1 are out of range.
       IF((Z0.LT.ZLL.AND.Z1.LT.ZLL).OR.(Z0.GT.ZUR.AND.Z1.GT.ZUR))RETURN
       IF(Z0.NE.Z1)THEN
*   Adjust Z0.
            IF(Z0.LT.ZLL)THEN
                 X0=X0+((X1-X0)/(Z1-Z0))*(ZLL-Z0)
                 Y0=Y0+((Y1-Y0)/(Z1-Z0))*(ZLL-Z0)
                 Z0=ZLL
            ENDIF
            IF(Z0.GT.ZUR)THEN
                 X0=X0+((X1-X0)/(Z1-Z0))*(ZUR-Z0)
                 Y0=Y0+((Y1-Y0)/(Z1-Z0))*(ZUR-Z0)
                 Z0=ZUR
            ENDIF
*   Adjust Z1.
            IF(Z1.LT.ZLL)THEN
                 X1=X1+((X1-X0)/(Z1-Z0))*(ZLL-Z1)
                 Y1=Y1+((Y1-Y0)/(Z1-Z0))*(ZLL-Z1)
                 Z1=ZLL
            ENDIF
            IF(Z1.GT.ZUR)THEN
                 X1=X1+((X1-X0)/(Z1-Z0))*(ZUR-Z1)
                 Y1=Y1+((Y1-Y0)/(Z1-Z0))*(ZUR-Z1)
                 Z1=ZUR
            ENDIF
       ENDIF
*** If begin and end point coincide, return with IFAIL=1.
       IF(X0.EQ.X1.AND.Y0.EQ.Y1.AND.Z0.EQ.Z1)RETURN
*** All is OK, therefore IFAIL=0.
       IFAIL=0
       END
