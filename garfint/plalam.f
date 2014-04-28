CDECK  ID>, PLALAM.
       SUBROUTINE PLALAM(X1,X0,X2,Y1,Y0,Y2,XLAM)
*-----------------------------------------------------------------------
*   PLALAM - Computes lambda for a point on a line (0 = start, 1 = end).
*   (Last changed on 20/ 1/98.)
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
       DOUBLE PRECISION X1,X0,X2,Y0,Y1,Y2,XLAM
*** Segment of zero length.
       IF((X1-X2).EQ.0.AND.(Y1-Y2).EQ.0)THEN
            PRINT *,' !!!!!! PLALAM WARNING : Zero length segment.'
            if(lgstop)then
                 print *,' x1-y2=',x1,x2
                 print *,' y1-y2=',y1,y2
            endif
            XLAM=2
*** Point nearer to (X1,Y1).
       ELSEIF((X0-X1)**2+(Y0-Y1)**2.LT.(X0-X2)**2+(Y0-Y2)**2)THEN
            IF(ABS(Y1-Y2).GT.ABS(X1-X2))THEN
                 XLAM=(Y0-Y1)/(Y2-Y1)
            ELSE
                 XLAM=(X0-X1)/(X2-X1)
            ENDIF
*** Point nearer to (X2,Y2).
       ELSE
            IF(ABS(Y1-Y2).GT.ABS(X1-X2))THEN
                 XLAM=1-(Y0-Y2)/(Y1-Y2)
            ELSE
                 XLAM=1-(X0-X2)/(X1-X2)
            ENDIF
       ENDIF
       END
