CDECK  ID>, GRCBIS.
       SUBROUTINE GRCBIS(F,FC,X0,Y0,XL,YL,FL,IL,XR,YR,FR,IR,IFAIL)
*-----------------------------------------------------------------------
*   GRCBIS - Computes a starting point (X0,Y0) for a contour at function
*            value FC using bisection between (XL,YL) and (XR,YR).
*   (Last changed on 18/ 6/98.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXCHA,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR,
     -         MXPAIR,MXPART,MXFOUR,MXCLUS,
     -         MXLINE,MXEQUT,
     -         MXRECL,MXINCH,MXWORD,MXCHAR,MXNAME,MXLUN,
     -         MXINS,MXREG,MXARG,MXCONS,MXVAR,MXALGE,
     -         MXZERO,MXSTCK,MXFPNT,MXFPAR,MXWKLS,
     -         MXHLEV,MXHLRL,MXSUBT,
     -         MXDLVL,MXILVL,MXDLIN,
     -         MXHIST,MXFRAC,MXBANG,MXBTAB,
     -         MXEXG,MXIOG,MXCSG,
     -         MXORIA,
     -         MXMAT,MXEMAT,MXMDIM,
     -         MXSHOT,MXZPAR,
     -         MXMAP,MXEPS,MXWMAP,MXSOLI,MXSBUF,
     -         MXPLAN,MXPOIN,MXEDGE,
     -         MXMCA
       PARAMETER (MXWIRE=   300,MXSW  =   50)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXHIST=   200, MXCHA = MXLIST/2)
       PARAMETER (MXGRID=    50)
       PARAMETER (MXNAME=   200, MXLUN =    30)
       PARAMETER (MXCLUS=   500, MXPAIR=  2000, MXPART= 10000)
       PARAMETER (MXLINE=   150, MXEQUT=    50)
       PARAMETER (MXFOUR=    16)
       PARAMETER (MXRECL= 10000)
       PARAMETER (MXINCH=  2000, MXWORD=   200, MXCHAR=MXINCH)
       PARAMETER (MXINS =  1000, MXREG =   500, MXCONS=  -500,
     -            MXVAR =   500, MXALGE=   500, MXARG =   100)
       PARAMETER (MXMAT =   500, MXEMAT=200000, MXMDIM=   10)
       PARAMETER (MXZERO=MXWIRE)
       PARAMETER (MXSTCK=     5)
       PARAMETER (MXFPNT= 20000, MXFPAR=    10)
       PARAMETER (MXWKLS=    10)
       PARAMETER (MXHLEV=     9, MXSUBT=   200, MXHLRL=  860)
       PARAMETER (MXDLVL=    10, MXILVL=    20, MXDLIN= 2500)
       PARAMETER (MXFRAC=    13)
       PARAMETER (MXBANG=    20, MXBTAB=    25)
       PARAMETER (MXEXG =    50, MXIOG =    10, MXCSG =  200)
       PARAMETER (MXORIA=  1000)
       PARAMETER (MXSHOT=    10, MXZPAR=4*MXSHOT+2)
       PARAMETER (MXMAP =  5000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       LOGICAL XDONE(0:MXGRID,0:MXGRID),YDONE(0:MXGRID,0:MXGRID),
     -      TRANS,CLAB
       REAL GRID(0:MXGRID,0:MXGRID),EPSTRA,EPSGRA,CXMIN,CXMAX,CYMIN,
     -      CYMAX,STINIT,DNTHR,DXGRA,DYGRA
       INTEGER ILOCGR(0:MXGRID,0:MXGRID),NBITER,NNITER,NFC,NGCMAX
       COMMON /CONDAT/ GRID,XDONE,YDONE,ILOCGR,
     -      NBITER,NNITER,EPSTRA,EPSGRA,DXGRA,DYGRA,
     -      STINIT,DNTHR,CXMIN,CXMAX,CYMIN,CYMAX,NFC,NGCMAX,TRANS,CLAB
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL FC,X0,Y0,F0,X1,Y1,F1,X2,Y2,F2,X3,Y3,F3,XL,YL,FL,XR,YR,FR,
     -      SCALE,SCALE1,SCALE2,DISC,FTEST1,FTEST2,P1,P2,P3
       INTEGER IFAIL,IL,IR,I1,I2,I3,ILOC0,ILOCT1,ILOCT2,IBITER
       EXTERNAL F
*** Assume the procedure converges.
       IFAIL=0
       IF(LDEBUG)WRITE(10,'(1X,A,3E15.8,I3/25X,A,3E15.8,I3,A,E15.8)')
     -      ' ++++++ GRCBIS DEBUG   : Bisection between ',
     -      XL,YL,FL,IL,' and ',XR,YR,FR,IR,' for F=',FC
*** Make sure that not both points have special ILOCs.
       IF(IL.NE.0.AND.IR.NE.0)THEN
            WRITE(10,'(''  !!!!!! GRCBIS WARNING : Bisection called'',
     -           '' between 2 ILOC#0 points, ILOC='',2I5)') IL,IR
            IFAIL=1
            RETURN
       ENDIF
*** Set up the bisection and search cycles.
       X1=XL
       Y1=YL
       F1=FL
       I1=IL
       X3=XR
       Y3=YR
       F3=FR
       I3=IR
*** In case either of the end points has ILOC/=0, fix range.
       IF(I1.NE.0.AND.I3.EQ.0)THEN
            DO 20 IBITER=1,NBITER
            X2=(X1+X3)/2
            Y2=(Y1+Y3)/2
            CALL F(X2,Y2,F2,I2)
            NFC=NFC+1
            IF(I2.EQ.0)THEN
                 X3=X2
                 Y3=Y2
                 F3=F2
                 I3=I2
            ELSE
                 X1=X2
                 Y1=Y2
                 F1=F2
                 I1=I2
            ENDIF
            IF((ABS(X3-X1)+ABS(Y3-Y1)).LT.
     -           1E-5*(ABS(X1+X3)+ABS(Y1+Y3)))GOTO 30
20          CONTINUE
30          CONTINUE
            X1=X3
            Y1=Y3
            F1=F3
            I1=I3
            X3=XR
            Y3=YR
            F3=FR
            I3=IR
       ELSEIF(I1.EQ.0.AND.I3.NE.0)THEN
            DO 40 IBITER=1,NBITER
            X2=(X1+X3)/2
            Y2=(Y1+Y3)/2
            CALL F(X2,Y2,F2,I2)
            NFC=NFC+1
            IF(I2.EQ.0)THEN
                 X1=X2
                 Y1=Y2
                 F1=F2
                 I1=I2
            ELSE
                 X3=X2
                 Y3=Y2
                 F3=F2
                 I3=I2
            ENDIF
            IF((ABS(X3-X1)+ABS(Y3-Y1)).LT.
     -           1E-5*(ABS(X1+X3)+ABS(Y1+Y3)))GOTO 50
40          CONTINUE
50          CONTINUE
            X1=XL
            Y1=YL
            F1=FL
            I1=IL
            X3=X1
            Y3=Y1
            F3=F1
            I3=I1
       ENDIF
*** Iterate the bisection steps.
       DO 10 IBITER=1,NBITER
       IF(LDEBUG)WRITE(10,'(1X,A,I2)') ' ++++++ GRCBIS DEBUG   :'//
     -      ' Bisection cycle ',IBITER
**  Add one point in the middle, to be used for a parabolic fit.
       X2=(X1+X3)/2
       Y2=(Y1+Y3)/2
       CALL F(X2,Y2,F2,I2)
       NFC=NFC+1
       IF(LDEBUG)WRITE(10,'(26X,''Middle point: '',2E15.8,
     -      '', F='',E15.8,'', ILOC='',I5)') X2,Y2,F2,I2
       SCALE=-1
**  First attempt to find the parabolic crossing point ...
       P1=2*(F1-2*F2+F3)
       P2=-3*F1+4*F2-F3
       P3=F1-FC
       DISC=P2**2-4*P1*P3
*   Immediate failure for zero discriminant and degenerate parabola's.
       IF(DISC.GE.0.AND.P1.NE.0)THEN
            SCALE1=(-P2+SQRT(DISC))/(2*P1)
            SCALE2=(-P2-SQRT(DISC))/(2*P1)
            IF(LDEBUG)WRITE(10,'(1X,A,2E15.8)') ' ++++++ GRCBIS'//
     -           ' DEBUG   : Parabolic scales: ',SCALE1,SCALE2
*   Only the first point is within range.
            IF(SCALE1.GE.0.AND.SCALE1.LE.1.AND.
     -           (SCALE2.LT.0.OR.SCALE2.GT.1))THEN
                 SCALE=SCALE1
                 CALL F(X1+SCALE*(X3-X1),Y1+SCALE*(Y3-Y1),F0,ILOC0)
                 NFC=NFC+1
                 IF(ILOC0.NE.0)THEN
                      IFAIL=1
                      RETURN
                 ENDIF
                 IF(LDEBUG)WRITE(10,'(26X,A)') 'Only first satisfies.'
*   Only the second point is within range.
            ELSEIF(SCALE2.GE.0.0.AND.SCALE2.LE.1.0.AND.
     -           (SCALE1.LT.0.0.OR.SCALE1.GT.1.0))THEN
                 SCALE=SCALE2
                 CALL F(X1+SCALE*(X3-X1),Y1+SCALE*(Y3-Y1),F0,ILOC0)
                 NFC=NFC+1
                 IF(ILOC0.NE.0)THEN
                      IFAIL=1
                      RETURN
                 ENDIF
                 IF(LDEBUG)WRITE(10,'(26X,A)') 'Only second satisfies.'
*   Both are in range, select the one with the best function value.
            ELSEIF(SCALE1.GE.0.0.AND.SCALE1.LE.1.0.AND.
     -           SCALE2.GE.0.0.AND.SCALE2.LE.1.0)THEN
                 CALL F(X1+SCALE1*(X3-X1),Y1+SCALE1*(Y3-Y1),
     -                FTEST1,ILOCT1)
                 CALL F(X1+SCALE2*(X3-X1),Y1+SCALE2*(Y3-Y1),
     -                FTEST2,ILOCT2)
                 NFC=NFC+2
                 IF(ILOCT1.NE.0.OR.ILOCT2.NE.0)THEN
                      IFAIL=1
                      RETURN
                 ENDIF
                 IF(ABS(FTEST1-FC).LT.ABS(FTEST2-FC))THEN
                      SCALE=SCALE1
                      F0=FTEST1
                      IF(LDEBUG)WRITE(10,'(26X,A,E15.8)') 'First'//
     -                     ' scale gives closest function value: ',F0
                 ELSE
                      SCALE=SCALE2
                      F0=FTEST2
                      IF(LDEBUG)WRITE(10,'(26X,A,E15.8)') 'Second'//
     -                     ' scale gives closest function value: ',F0
                 ENDIF
            ELSE
                 SCALE=-1.0
                 IF(LDEBUG)WRITE(10,'(26X,A)') 'Neither satisfies.'
            ENDIF
       ENDIF
**  Attempt a linear procedure if the parabolic method failed.
       IF((F1.NE.F3).AND.(SCALE.LT.0.0.OR.SCALE.GT.1.0))THEN
            SCALE=(FC-F1)/(F3-F1)
            CALL F(X1+(X3-X1)*SCALE,Y1+(Y3-Y1)*SCALE,F0,ILOC0)
            NFC=NFC+1
            IF(ILOC0.NE.0)THEN
                 IFAIL=1
                 RETURN
            ENDIF
            IF(LDEBUG)WRITE(10,'(1X,2(A,E15.8))') ' +++++++ GRCBIS'//
     -           ' DEBUG   : Linear scale = ',SCALE,' F=',F0
       ENDIF
**  Now try to insert the new point if it's there at the good place.
       IF(SCALE.GE.0.0.AND.SCALE.LE.1.0)THEN
            X0=X1+SCALE*(X3-X1)
            Y0=Y1+SCALE*(Y3-Y1)
*   Presumed crossing between point 1 and the 'optimum'.
            IF((F1-FC)*(FC-F0).GE.0.AND.SCALE.LE.0.5)THEN
                 X3=X0
                 Y3=Y0
                 F3=F0
C                 IF(LDEBUG)WRITE(10,'(26X,A)') 'New edges: 1, opt.'
*   Presumed crossing between point 'optimum' and point 2.
            ELSEIF((F0-FC)*(FC-F2).GE.0.AND.SCALE.LE.0.5)THEN
                 X1=X0
                 Y1=Y0
                 F1=F0
                 X3=X2
                 Y3=Y2
                 F3=F2
                 IF(LDEBUG)WRITE(10,'(26X,A)') 'New edges: opt, 2.'
*   Presumed crossing between point 2 and the 'optimum'.
            ELSEIF((F2-FC)*(FC-F0).GE.0.AND.SCALE.GT.0.5)THEN
                 X1=X2
                 Y1=Y2
                 F1=F2
                 X3=X0
                 Y3=Y0
                 F3=F0
*   Presumed crossing between point 'optimum' and point 3.
            ELSEIF((F0-FC)*(FC-F3).GE.0.AND.SCALE.GT.0.5)THEN
                 X1=X0
                 Y1=Y0
                 F1=F0
*   Elsewhere: failure, fall back on pure bisection.
            ELSE
                 IF(LDEBUG)THEN
                      WRITE(10,'(1X,A)') ' ++++++ GRCBIS DEBUG   :'//
     -                     ' Pure bisection fallback forced'//
     -                     ' because of an unexpected case:'
                      WRITE(10,'(25X,A,3E15.8)') ' point 1:  ',X1,Y1,F1
                      WRITE(10,'(25X,A,3E15.8)') ' point 2:  ',X2,Y2,F2
                      WRITE(10,'(25X,A,3E15.8)') ' point 3:  ',X3,Y3,F3
                      WRITE(10,'(25X,A,E15.8,A,E15.8)') ' parabola:'//
     -                     ' SCALE=',SCALE,' F=',F0
                 ENDIF
                 SCALE=-1.0
            ENDIF
       ENDIF
**  Pure bisection.
       IF(SCALE.LT.0.0.OR.SCALE.GT.1.0)THEN
*   Set the new edges.
            IF((F1-FC)*(FC-F2).GT.0)THEN
                 X3=X2
                 Y3=Y2
                 F3=F2
            ELSE
                 X1=X2
                 Y1=Y2
                 F1=F2
            ENDIF
*   Compute F0 as the value halfway the interval.
            X0=0.5*(X1+X3)
            Y0=0.5*(Y1+Y3)
            CALL F(X0,Y0,F0,ILOC0)
            NFC=NFC+1
            IF(ILOC0.NE.0)THEN
                 IFAIL=1
                 RETURN
            ENDIF
       ENDIF
**  Check for convergence.
       IF(ABS(F0-FC).LT.EPSTRA*(1+ABS(FC)))THEN
            IF(LDEBUG)WRITE(10,'(1X,A)') ' ++++++ GRCBIS DEBUG   :'//
     -           ' Convergence achieved between F0 and FC at:'
            IF(LDEBUG)WRITE(10,'(26X,A,3E15.8)') '(x,y,f) = ',X0,Y0,F0
            RETURN
       ENDIF
10     CONTINUE
*** This point is only reached if no convergence ia achieved.
       WRITE(10,'(1X,A)') ' !!!!!! GRCBIS WARNING : Bisection'//
     -      ' didn''t converge.'
       IFAIL=1
       END
