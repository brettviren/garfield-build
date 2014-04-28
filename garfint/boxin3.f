CDECK  ID>, BOXIN3.
       SUBROUTINE BOXIN3(VALUE,XAXIS,YAXIS,ZAXIS,MAXX,MAXY,MAXZ,
     -      NX,NY,NZ,XX,YY,ZZ,F,IORDER,IFAIL)
*-----------------------------------------------------------------------
*   BOXIN3 - Interpolation of order 1 and 2 in an irregular rectangular
*            3-dimensional grid.
*   (Last changed on 13/ 2/00.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MAXX,MAXY,MAXZ,NX,NY,NZ,IORDER,IFAIL,I,INODE,IGRID,
     -      IX,IX0,IX1,IY,IY0,IY1,IZ,IZ0,IZ1
       REAL VALUE(MAXX,MAXY,MAXZ),XAXIS(MAXX),YAXIS(MAXY),ZAXIS(MAXZ),
     -      X,Y,Z,F,DIST,XLOCAL,YLOCAL,ZLOCAL,
     -      FX(4),FY(4),FZ(4),XX,YY,ZZ
*** Ensure we are in the grid.
       X=MIN(MAX(XX,MIN(XAXIS(1),XAXIS(NX))),MAX(XAXIS(1),XAXIS(NX)))
       Y=MIN(MAX(YY,MIN(YAXIS(1),YAXIS(NY))),MAX(YAXIS(1),YAXIS(NY)))
       Z=MIN(MAX(ZZ,MIN(ZAXIS(1),ZAXIS(NZ))),MAX(ZAXIS(1),ZAXIS(NZ)))
*   Make sure we have enough points.
       IF(IORDER.LT.0.OR.IORDER.GT.2.OR.
     -      NX.LT.1.OR.NX.GT.MAXX.OR.
     -      NY.LT.1.OR.NY.GT.MAXY.OR.
     -      NZ.LT.1.OR.NZ.GT.MAXZ)THEN
            PRINT *,' !!!!!! BOXIN3 WARNING : Incorrect order or'//
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
                 PRINT *,' !!!!!! BOXIN3 WARNING : Incorrect grid;'//
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
*   Find the grid segment containing this point.
            IGRID=0
            DO 30 I=2,NX
            IF((XAXIS(I-1)-X)*(X-XAXIS(I)).GE.0)IGRID=I
30          CONTINUE
*   Compute the local coordinate for this grid segment.
            XLOCAL=(X-XAXIS(IGRID-1))/(XAXIS(IGRID)-XAXIS(IGRID-1))
*   Set the summing range and shape functions.
            IF(IGRID.EQ.2)THEN
                 IX0=IGRID-1
                 IX1=IGRID+1
                 IF(  XAXIS(IX0  ).EQ.XAXIS(IX0+1).OR.
     -                XAXIS(IX0  ).EQ.XAXIS(IX0+2).OR.
     -                XAXIS(IX0+1).EQ.XAXIS(IX0+2))GOTO 3010
                 FX(1)=(X           -XAXIS(IX0+1))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0  )-XAXIS(IX0+1))*
     -                 (XAXIS(IX0  )-XAXIS(IX0+2)))
                 FX(2)=(X           -XAXIS(IX0  ))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0+1)-XAXIS(IX0  ))*
     -                 (XAXIS(IX0+1)-XAXIS(IX0+2)))
                 FX(3)=(X           -XAXIS(IX0  ))*
     -                 (X           -XAXIS(IX0+1))/
     -                ((XAXIS(IX0+2)-XAXIS(IX0  ))*
     -                 (XAXIS(IX0+2)-XAXIS(IX0+1)))
            ELSEIF(IGRID.EQ.NX)THEN
                 IX0=IGRID-2
                 IX1=IGRID
                 IF(  XAXIS(IX0  ).EQ.XAXIS(IX0+1).OR.
     -                XAXIS(IX0  ).EQ.XAXIS(IX0+2).OR.
     -                XAXIS(IX0+1).EQ.XAXIS(IX0+2))GOTO 3010
                 FX(1)=(X           -XAXIS(IX0+1))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0  )-XAXIS(IX0+1))*
     -                 (XAXIS(IX0  )-XAXIS(IX0+2)))
                 FX(2)=(X           -XAXIS(IX0  ))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0+1)-XAXIS(IX0  ))*
     -                 (XAXIS(IX0+1)-XAXIS(IX0+2)))
                 FX(3)=(X           -XAXIS(IX0  ))*
     -                 (X           -XAXIS(IX0+1))/
     -                ((XAXIS(IX0+2)-XAXIS(IX0  ))*
     -                 (XAXIS(IX0+2)-XAXIS(IX0+1)))
            ELSE
                 IX0=IGRID-2
                 IX1=IGRID+1
                 IF(  XAXIS(IX0  ).EQ.XAXIS(IX0+1).OR.
     -                XAXIS(IX0  ).EQ.XAXIS(IX0+2).OR.
     -                XAXIS(IX0  ).EQ.XAXIS(IX0+3).OR.
     -                XAXIS(IX0+1).EQ.XAXIS(IX0+2).OR.
     -                XAXIS(IX0+1).EQ.XAXIS(IX0+3).OR.
     -                XAXIS(IX0+2).EQ.XAXIS(IX0+3))GOTO 3010
                 FX(1)=(1-XLOCAL)*
     -                 (X           -XAXIS(IX0+1))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0  )-XAXIS(IX0+1))*
     -                 (XAXIS(IX0  )-XAXIS(IX0+2)))
                 FX(2)=(1-XLOCAL)*
     -                 (X           -XAXIS(IX0  ))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0+1)-XAXIS(IX0  ))*
     -                 (XAXIS(IX0+1)-XAXIS(IX0+2)))+
     -                 XLOCAL*
     -                 (X           -XAXIS(IX0+2))*
     -                 (X           -XAXIS(IX0+3))/
     -                ((XAXIS(IX0+1)-XAXIS(IX0+2))*
     -                 (XAXIS(IX0+1)-XAXIS(IX0+3)))
                 FX(3)=(1-XLOCAL)*
     -                 (X           -XAXIS(IX0  ))*
     -                 (X           -XAXIS(IX0+1))/
     -                ((XAXIS(IX0+2)-XAXIS(IX0  ))*
     -                 (XAXIS(IX0+2)-XAXIS(IX0+1)))+
     -                 XLOCAL*
     -                 (X           -XAXIS(IX0+1))*
     -                 (X           -XAXIS(IX0+3))/
     -                ((XAXIS(IX0+2)-XAXIS(IX0+1))*
     -                 (XAXIS(IX0+2)-XAXIS(IX0+3)))
                 FX(4)=XLOCAL*
     -                 (X           -XAXIS(IX0+1))*
     -                 (X           -XAXIS(IX0+2))/
     -                ((XAXIS(IX0+3)-XAXIS(IX0+1))*
     -                 (XAXIS(IX0+3)-XAXIS(IX0+2)))
            ENDIF
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
                 PRINT *,' !!!!!! BOXIN3 WARNING : Incorrect grid;'//
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
*   Find the grid segment containing this point.
            IGRID=0
            DO 60 I=2,NY
            IF((YAXIS(I-1)-Y)*(Y-YAXIS(I)).GE.0)IGRID=I
60          CONTINUE
*   Compute the local coordinate for this grid segment.
            YLOCAL=(Y-YAXIS(IGRID-1))/(YAXIS(IGRID)-YAXIS(IGRID-1))
*   Set the summing range and shape functions.
            IF(IGRID.EQ.2)THEN
                 IY0=IGRID-1
                 IY1=IGRID+1
                 IF(  YAXIS(IY0  ).EQ.YAXIS(IY0+1).OR.
     -                YAXIS(IY0  ).EQ.YAXIS(IY0+2).OR.
     -                YAXIS(IY0+1).EQ.YAXIS(IY0+2))GOTO 3010
                 FY(1)=(Y           -YAXIS(IY0+1))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0  )-YAXIS(IY0+1))*
     -                 (YAXIS(IY0  )-YAXIS(IY0+2)))
                 FY(2)=(Y           -YAXIS(IY0  ))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0+1)-YAXIS(IY0  ))*
     -                 (YAXIS(IY0+1)-YAXIS(IY0+2)))
                 FY(3)=(Y           -YAXIS(IY0  ))*
     -                 (Y           -YAXIS(IY0+1))/
     -                ((YAXIS(IY0+2)-YAXIS(IY0  ))*
     -                 (YAXIS(IY0+2)-YAXIS(IY0+1)))
            ELSEIF(IGRID.EQ.NY)THEN
                 IY0=IGRID-2
                 IY1=IGRID
                 IF(  YAXIS(IY0  ).EQ.YAXIS(IY0+1).OR.
     -                YAXIS(IY0  ).EQ.YAXIS(IY0+2).OR.
     -                YAXIS(IY0+1).EQ.YAXIS(IY0+2))GOTO 3010
                 FY(1)=(Y           -YAXIS(IY0+1))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0  )-YAXIS(IY0+1))*
     -                 (YAXIS(IY0  )-YAXIS(IY0+2)))
                 FY(2)=(Y           -YAXIS(IY0  ))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0+1)-YAXIS(IY0  ))*
     -                 (YAXIS(IY0+1)-YAXIS(IY0+2)))
                 FY(3)=(Y           -YAXIS(IY0  ))*
     -                 (Y           -YAXIS(IY0+1))/
     -                ((YAXIS(IY0+2)-YAXIS(IY0  ))*
     -                 (YAXIS(IY0+2)-YAXIS(IY0+1)))
            ELSE
                 IY0=IGRID-2
                 IY1=IGRID+1
                 IF(  YAXIS(IY0  ).EQ.YAXIS(IY0+1).OR.
     -                YAXIS(IY0  ).EQ.YAXIS(IY0+2).OR.
     -                YAXIS(IY0  ).EQ.YAXIS(IY0+3).OR.
     -                YAXIS(IY0+1).EQ.YAXIS(IY0+2).OR.
     -                YAXIS(IY0+1).EQ.YAXIS(IY0+3).OR.
     -                YAXIS(IY0+2).EQ.YAXIS(IY0+3))GOTO 3010
                 FY(1)=(1-YLOCAL)*
     -                 (Y           -YAXIS(IY0+1))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0  )-YAXIS(IY0+1))*
     -                 (YAXIS(IY0  )-YAXIS(IY0+2)))
                 FY(2)=(1-YLOCAL)*
     -                 (Y           -YAXIS(IY0  ))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0+1)-YAXIS(IY0  ))*
     -                 (YAXIS(IY0+1)-YAXIS(IY0+2)))+
     -                 YLOCAL*
     -                 (Y           -YAXIS(IY0+2))*
     -                 (Y           -YAXIS(IY0+3))/
     -                ((YAXIS(IY0+1)-YAXIS(IY0+2))*
     -                 (YAXIS(IY0+1)-YAXIS(IY0+3)))
                 FY(3)=(1-YLOCAL)*
     -                 (Y           -YAXIS(IY0  ))*
     -                 (Y           -YAXIS(IY0+1))/
     -                ((YAXIS(IY0+2)-YAXIS(IY0  ))*
     -                 (YAXIS(IY0+2)-YAXIS(IY0+1)))+
     -                 YLOCAL*
     -                 (Y           -YAXIS(IY0+1))*
     -                 (Y           -YAXIS(IY0+3))/
     -                ((YAXIS(IY0+2)-YAXIS(IY0+1))*
     -                 (YAXIS(IY0+2)-YAXIS(IY0+3)))
                 FY(4)=YLOCAL*
     -                 (Y           -YAXIS(IY0+1))*
     -                 (Y           -YAXIS(IY0+2))/
     -                ((YAXIS(IY0+3)-YAXIS(IY0+1))*
     -                 (YAXIS(IY0+3)-YAXIS(IY0+2)))
            ENDIF
       ENDIF
*** Zeroth order interpolation in z.
       IF(IORDER.EQ.0.OR.NZ.LE.1)THEN
*   Find the nearest node.
            DIST=ABS(Z-ZAXIS(1))
            INODE=1
            DO 70 I=2,NZ
            IF(ABS(Z-ZAXIS(I)).LT.DIST)THEN
                 DIST=ABS(Z-ZAXIS(I))
                 INODE=I
            ENDIF
70          CONTINUE
*   Set the summing range.
            IZ0=INODE
            IZ1=INODE
*   Establish the shape functions.
            FZ(1)=1
            FZ(2)=0
            FZ(3)=0
*** First order interpolation in z.
       ELSEIF(IORDER.EQ.1.OR.NZ.LE.2)THEN
*   Find the grid segment containing this point.
            IGRID=0
            DO 80 I=2,NZ
            IF((ZAXIS(I-1)-Z)*(Z-ZAXIS(I)).GE.0)IGRID=I
80          CONTINUE
*   Ensure there won't be divisions by zero.
            IF(ZAXIS(IGRID).EQ.ZAXIS(IGRID-1))THEN
                 PRINT *,' !!!!!! BOXIN3 WARNING : Incorrect grid;'//
     -                ' no interpolation.'
                 F=0
                 IFAIL=1
                 RETURN
            ENDIF
*   Compute local coordinates.
            ZLOCAL=(Z-ZAXIS(IGRID-1))/(ZAXIS(IGRID)-ZAXIS(IGRID-1))
*   Set the summing range.
            IZ0=IGRID-1
            IZ1=IGRID
*   Set the shape functions.
            FZ(1)=1-ZLOCAL
            FZ(2)=ZLOCAL
            FZ(3)=0
*** Second order interpolation in z.
       ELSEIF(IORDER.EQ.2)THEN
*   Find the grid segment containing this point.
            IGRID=0
            DO 90 I=2,NZ
            IF((ZAXIS(I-1)-Z)*(Z-ZAXIS(I)).GE.0)IGRID=I
90          CONTINUE
*   Compute the local coordinate for this grid segment.
            ZLOCAL=(Z-ZAXIS(IGRID-1))/(ZAXIS(IGRID)-ZAXIS(IGRID-1))
*   Set the summing range and shape functions.
            IF(IGRID.EQ.2)THEN
                 IZ0=IGRID-1
                 IZ1=IGRID+1
                 IF(  ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+1).OR.
     -                ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+2).OR.
     -                ZAXIS(IZ0+1).EQ.ZAXIS(IZ0+2))GOTO 3010
                 FZ(1)=(Z           -ZAXIS(IZ0+1))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0  )-ZAXIS(IZ0+1))*
     -                 (ZAXIS(IZ0  )-ZAXIS(IZ0+2)))
                 FZ(2)=(Z           -ZAXIS(IZ0  ))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0+1)-ZAXIS(IZ0  ))*
     -                 (ZAXIS(IZ0+1)-ZAXIS(IZ0+2)))
                 FZ(3)=(Z           -ZAXIS(IZ0  ))*
     -                 (Z           -ZAXIS(IZ0+1))/
     -                ((ZAXIS(IZ0+2)-ZAXIS(IZ0  ))*
     -                 (ZAXIS(IZ0+2)-ZAXIS(IZ0+1)))
            ELSEIF(IGRID.EQ.NZ)THEN
                 IZ0=IGRID-2
                 IZ1=IGRID
                 IF(  ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+1).OR.
     -                ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+2).OR.
     -                ZAXIS(IZ0+1).EQ.ZAXIS(IZ0+2))GOTO 3010
                 FZ(1)=(Z           -ZAXIS(IZ0+1))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0  )-ZAXIS(IZ0+1))*
     -                 (ZAXIS(IZ0  )-ZAXIS(IZ0+2)))
                 FZ(2)=(Z           -ZAXIS(IZ0  ))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0+1)-ZAXIS(IZ0  ))*
     -                 (ZAXIS(IZ0+1)-ZAXIS(IZ0+2)))
                 FZ(3)=(Z           -ZAXIS(IZ0  ))*
     -                 (Z           -ZAXIS(IZ0+1))/
     -                ((ZAXIS(IZ0+2)-ZAXIS(IZ0  ))*
     -                 (ZAXIS(IZ0+2)-ZAXIS(IZ0+1)))
            ELSE
                 IZ0=IGRID-2
                 IZ1=IGRID+1
                 IF(  ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+1).OR.
     -                ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+2).OR.
     -                ZAXIS(IZ0  ).EQ.ZAXIS(IZ0+3).OR.
     -                ZAXIS(IZ0+1).EQ.ZAXIS(IZ0+2).OR.
     -                ZAXIS(IZ0+1).EQ.ZAXIS(IZ0+3).OR.
     -                ZAXIS(IZ0+2).EQ.ZAXIS(IZ0+3))GOTO 3010
                 FZ(1)=(1-ZLOCAL)*
     -                 (Z           -ZAXIS(IZ0+1))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0  )-ZAXIS(IZ0+1))*
     -                 (ZAXIS(IZ0  )-ZAXIS(IZ0+2)))
                 FZ(2)=(1-ZLOCAL)*
     -                 (Z           -ZAXIS(IZ0  ))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0+1)-ZAXIS(IZ0  ))*
     -                 (ZAXIS(IZ0+1)-ZAXIS(IZ0+2)))+
     -                 ZLOCAL*
     -                 (Z           -ZAXIS(IZ0+2))*
     -                 (Z           -ZAXIS(IZ0+3))/
     -                ((ZAXIS(IZ0+1)-ZAXIS(IZ0+2))*
     -                 (ZAXIS(IZ0+1)-ZAXIS(IZ0+3)))
                 FZ(3)=(1-ZLOCAL)*
     -                 (Z           -ZAXIS(IZ0  ))*
     -                 (Z           -ZAXIS(IZ0+1))/
     -                ((ZAXIS(IZ0+2)-ZAXIS(IZ0  ))*
     -                 (ZAXIS(IZ0+2)-ZAXIS(IZ0+1)))+
     -                 ZLOCAL*
     -                 (Z           -ZAXIS(IZ0+1))*
     -                 (Z           -ZAXIS(IZ0+3))/
     -                ((ZAXIS(IZ0+2)-ZAXIS(IZ0+1))*
     -                 (ZAXIS(IZ0+2)-ZAXIS(IZ0+3)))
                 FZ(4)=ZLOCAL*
     -                 (Z           -ZAXIS(IZ0+1))*
     -                 (Z           -ZAXIS(IZ0+2))/
     -                ((ZAXIS(IZ0+3)-ZAXIS(IZ0+1))*
     -                 (ZAXIS(IZ0+3)-ZAXIS(IZ0+2)))
            ENDIF
       ENDIF
*** Sum the shape functions.
       F=0
       DO 100 IX=IX0,IX1
       DO 110 IY=IY0,IY1
       DO 120 IZ=IZ0,IZ1
       F=F+VALUE(IX,IY,IZ)*FX(IX-IX0+1)*FY(IY-IY0+1)*FZ(IZ-IZ0+1)
120    CONTINUE
110    CONTINUE
100    CONTINUE
*** Seems to have worked.
       IFAIL=0
       RETURN
*** Error handling.
3010   CONTINUE
       PRINT *,' !!!!!! BOXIN3 WARNING : One or more grid points in'//
     -      ' x coincide; no interpolation.'
       F=0
       IFAIL=1
       END
