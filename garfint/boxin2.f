CDECK  ID>, BOXIN2.
       SUBROUTINE BOXIN2(VALUE,XAXIS,YAXIS,MAXX,MAXY,NX,NY,X,Y,F,IORDER,
     -      IFAIL)
*-----------------------------------------------------------------------
*   BOXIN2 - Interpolation of order 1 and 2 in an irregular rectangular
*            2-dimensional grid.
*   (Last changed on 24/ 1/00.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MAXX,MAXY,NX,NY,IORDER,IFAIL,I,INODE,IGRID,IX,IX0,IX1,
     -      IY,IY0,IY1
       REAL VALUE(MAXX,MAXY),XAXIS(MAXX),YAXIS(MAXY),X,Y,F,DIST,
     -      XLOCAL,YLOCAL,XALPHA,YALPHA,FX(3),FY(3)
*** Ensure we are in the grid.
       IF((XAXIS(NX)-X)*(X-XAXIS(1)).LT.0.OR.
     -      (YAXIS(NY)-Y)*(Y-YAXIS(1)).LT.0)THEN
C           PRINT *,' !!!!!! BOXIN2 WARNING : Point not in the grid;'//
C                ' no interpolation.'
            F=0
            IFAIL=1
            RETURN
*   Make sure we have enough points.
       ELSEIF(IORDER.LT.0.OR.IORDER.GT.2.OR.
     -      NX.LT.1.OR.NX.GT.MAXX.OR.NY.LT.1.OR.NY.GT.MAXY)THEN
            PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect order or'//
     -           ' number of points; no interpolation.'
            F=0
            IFAIL=1
            RETURN
       ENDIF
*** Zeroth order interpolation in x.
       IF(IORDER.EQ.0.OR.NX.LE.1)THEN
*   Find the nearest node.
            DIST=ABS(X-XAXIS(1))
            INODE=1
            DO 10 I=2,NX
            IF(ABS(X-XAXIS(I)).LT.DIST)THEN
                 DIST=ABS(X-XAXIS(I))
                 INODE=I
            ENDIF
10          CONTINUE
*   Set the summing range.
            IX0=INODE
            IX1=INODE
*   Establish the shape functions.
            FX(1)=1
            FX(2)=0
            FX(3)=0
*** First order interpolation in x.
       ELSEIF(IORDER.EQ.1.OR.NX.LE.2)THEN
*   Find the grid segment containing this point.
            IGRID=0
            DO 20 I=2,NX
            IF((XAXIS(I-1)-X)*(X-XAXIS(I)).GE.0)IGRID=I
20          CONTINUE
*   Ensure there won't be divisions by zero.
            IF(XAXIS(IGRID).EQ.XAXIS(IGRID-1))THEN
                 PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Compute local coordinates.
            XLOCAL=(X-XAXIS(IGRID-1))/(XAXIS(IGRID)-XAXIS(IGRID-1))
*   Set the summing range.
            IX0=IGRID-1
            IX1=IGRID
*   Set the shape functions.
            FX(1)=1-XLOCAL
            FX(2)=XLOCAL
            FX(3)=0
*** Second order interpolation in x.
       ELSEIF(IORDER.EQ.2)THEN
*   Find the nearest node and the grid segment.
            DIST=ABS(X-XAXIS(1))
            INODE=1
            DO 30 I=2,NX
            IF(ABS(X-XAXIS(I)).LT.DIST)THEN
                 DIST=ABS(X-XAXIS(I))
                 INODE=I
            ENDIF
30          CONTINUE
*   Find the nearest fitting 2x2 matrix.
            IGRID=MAX(2,MIN(NX-1,INODE))
*   Ensure there won't be divisions by zero.
            IF(XAXIS(IGRID+1).EQ.XAXIS(IGRID-1))THEN
                 PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Compute the alpha and local coordinate for this grid segment.
            XALPHA=(XAXIS(IGRID)-XAXIS(IGRID-1))/
     -           (XAXIS(IGRID+1)-XAXIS(IGRID-1))
            XLOCAL=(X-XAXIS(IGRID-1))/(XAXIS(IGRID+1)-XAXIS(IGRID-1))
*   Ensure there won't be divisions by zero.
            IF(XALPHA.LE.0.OR.XALPHA.GE.1)THEN
                 PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Set the summing range.
            IX0=IGRID-1
            IX1=IGRID+1
*   Set the shape functions.
            FX(1)=XLOCAL**2/XALPHA-XLOCAL*(1+XALPHA)/XALPHA+1
            FX(2)=(XLOCAL**2-XLOCAL)/(XALPHA**2-XALPHA)
            FX(3)=(XLOCAL**2-XLOCAL*XALPHA)/(1-XALPHA)
       ENDIF
*** Zeroth order interpolation in y.
       IF(IORDER.EQ.0.OR.NY.LE.1)THEN
*   Find the nearest node.
            DIST=ABS(Y-YAXIS(1))
            INODE=1
            DO 40 I=2,NY
            IF(ABS(Y-YAXIS(I)).LT.DIST)THEN
                 DIST=ABS(Y-YAXIS(I))
                 INODE=I
            ENDIF
40          CONTINUE
*   Set the summing range.
            IY0=INODE
            IY1=INODE
*   Establish the shape functions.
            FY(1)=1
            FY(2)=0
            FY(3)=0
*** First order interpolation in y.
       ELSEIF(IORDER.EQ.1.OR.NY.LE.2)THEN
*   Find the grid segment containing this point.
            IGRID=0
            DO 50 I=2,NY
            IF((YAXIS(I-1)-Y)*(Y-YAXIS(I)).GE.0)IGRID=I
50          CONTINUE
*   Ensure there won't be divisions by zero.
            IF(YAXIS(IGRID).EQ.YAXIS(IGRID-1))THEN
                 PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Compute local coordinates.
            YLOCAL=(Y-YAXIS(IGRID-1))/(YAXIS(IGRID)-YAXIS(IGRID-1))
*   Set the summing range.
            IY0=IGRID-1
            IY1=IGRID
*   Set the shape functions.
            FY(1)=1-YLOCAL
            FY(2)=YLOCAL
            FY(3)=0
*** Second order interpolation in y.
       ELSEIF(IORDER.EQ.2)THEN
*   Find the nearest node and the grid segment.
            DIST=ABS(Y-YAXIS(1))
            INODE=1
            DO 60 I=2,NY
            IF(ABS(Y-YAXIS(I)).LT.DIST)THEN
                 DIST=ABS(Y-YAXIS(I))
                 INODE=I
            ENDIF
60          CONTINUE
*   Find the nearest fitting 2x2 matrix.
            IGRID=MAX(2,MIN(NY-1,INODE))
*   Ensure there won't be divisions by zero.
            IF(YAXIS(IGRID+1).EQ.YAXIS(IGRID-1))THEN
                 PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Compute the alpha and local coordinate for this grid segment.
            YALPHA=(YAXIS(IGRID)-YAXIS(IGRID-1))/
     -           (YAXIS(IGRID+1)-YAXIS(IGRID-1))
            YLOCAL=(Y-YAXIS(IGRID-1))/(YAXIS(IGRID+1)-YAXIS(IGRID-1))
*   Ensure there won't be divisions by zero.
            IF(YALPHA.LE.0.OR.YALPHA.GE.1)THEN
                 PRINT *,' !!!!!! BOXIN2 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Set the summing range.
            IY0=IGRID-1
            IY1=IGRID+1
*   Set the shape functions.
            FY(1)=YLOCAL**2/YALPHA-YLOCAL*(1+YALPHA)/YALPHA+1
            FY(2)=(YLOCAL**2-YLOCAL)/(YALPHA**2-YALPHA)
            FY(3)=(YLOCAL**2-YLOCAL*YALPHA)/(1-YALPHA)
       ENDIF
*** Sum the shape functions.
       F=0
       DO 100 IX=IX0,IX1
       DO 110 IY=IY0,IY1
       F=F+VALUE(IX,IY)*FX(IX-IX0+1)*FY(IY-IY0+1)
110    CONTINUE
100    CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
