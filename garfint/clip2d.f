CDECK  ID>, CLIP2D.
       SUBROUTINE CLIP2D(X0,Y0,X1,Y1,XLL,YLL,XUR,YUR,IFAIL)
*-----------------------------------------------------------------------
*   CLIP2D - Routine clipping the line (X0,Y0) (X1,Y1) to the size of
*            the box formed by (XLL,YLL) (XUR,YUR).
*   VARIABLES : (X0,Y0)    : Begin point of line.
*               (X1,Y1)    : End point of line.
*               (XLL,ULL)  : Lower left hand corner of the box.
*               (XUR,YUR)  : Upper right hand corner of the box.
*   (Last changed on  5/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X0,Y0,X1,Y1,XLL,YLL,XUR,YUR
       INTEGER IFAIL
*** Return on IFAIL=0 if no changes have to be made.
       IFAIL=0
       IF(XLL.LE.X0.AND.X0.LE.XUR.AND.XLL.LE.X1.AND.X1.LE.XUR.AND.
     -    YLL.LE.Y0.AND.Y0.LE.YUR.AND.YLL.LE.Y1.AND.Y1.LE.YUR)RETURN
*** The next few returns are on IFAIL=1.
       IFAIL=1
*** Return with IFAIL=1 if X0 and X1 are out of range.
       IF((X0.LT.XLL.AND.X1.LT.XLL).OR.(X0.GT.XUR.AND.X1.GT.XUR))RETURN
       IF(X0.NE.X1)THEN
*   Adjust X0.
            IF(X0.LT.XLL)THEN
                 Y0=Y0+((Y1-Y0)/(X1-X0))*(XLL-X0)
                 X0=XLL
            ENDIF
            IF(X0.GT.XUR)THEN
                 Y0=Y0+((Y1-Y0)/(X1-X0))*(XUR-X0)
                 X0=XUR
            ENDIF
*   Adjust X1.
            IF(X1.LT.XLL)THEN
                 Y1=Y1+((Y1-Y0)/(X1-X0))*(XLL-X1)
                 X1=XLL
            ENDIF
            IF(X1.GT.XUR)THEN
                 Y1=Y1+((Y1-Y0)/(X1-X0))*(XUR-X1)
                 X1=XUR
            ENDIF
       ENDIF
*** Return with an IFAIL=1 if Y0 and Y1 are out of range.
       IF((Y0.LT.YLL.AND.Y1.LT.YLL).OR.(Y0.GT.YUR.AND.Y1.GT.YUR))RETURN
       IF(Y0.NE.Y1)THEN
*   Adjust y0.
            IF(Y0.LT.YLL)THEN
                 X0=X0+((X1-X0)/(Y1-Y0))*(YLL-Y0)
                 Y0=YLL
            ENDIF
            IF(Y0.GT.YUR)THEN
                 X0=X0+((X1-X0)/(Y1-Y0))*(YUR-Y0)
                 Y0=YUR
            ENDIF
*   Adjust y1.
            IF(Y1.LT.YLL)THEN
                 X1=X1+((X1-X0)/(Y1-Y0))*(YLL-Y1)
                 Y1=YLL
            ENDIF
            IF(Y1.GT.YUR)THEN
                 X1=X1+((X1-X0)/(Y1-Y0))*(YUR-Y1)
                 Y1=YUR
            ENDIF
       ENDIF
*** If begin and end point coincide, return with IFAIL=1.
       IF(X0.EQ.X1.AND.Y0.EQ.Y1)RETURN
*** All is OK, therefore IFAIL=0.
       IFAIL=0
       END
