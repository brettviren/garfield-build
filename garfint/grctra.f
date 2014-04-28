CDECK  ID>, GRCTRA.
       SUBROUTINE GRCTRA(F,FC,XST,YST,DIR,LOOP)
*-----------------------------------------------------------------------
*   GRCTRA - Traces a contour of F at function value FC starting from
*            (XST,YST). The tracing method iterates in two stages (1) a
*            side step orthogonal to the gradient (2) a Newton-Raphson
*            stepping back to the contour. Conditions that can cause
*            termination include (1) leaving the plotting area (2) the
*            contour is back at its origin ...
*   VARIABLES : LOOP        : Is set to .TRUE. if a full loop is found.
*   (Last changed on 14/ 3/08.)
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
       REAL FC,XST,YST,DIR,X0,Y0,X1,Y1,X2,Y2,F2,DFDX,DFDY,XSEG1,YSEG1,
     -      XSEG2,YSEG2,XL,YL,H,DFNORM
       INTEGER IFLAG,IFLGST,ISTEP,IFLG0,IFLG2,INITER,ILOC2,IFAIL
       LOGICAL CROSS,LOOP
       EXTERNAL F,CROSS
*** Initialise plotting of this contour fragment.
       CALL GRCPLT(XST,YST,FC,'INIT')
       CALL GRCUPD(F,XST,YST,FC,'START',IFLAG)
       LOOP=.FALSE.
*** Store a small segment that will be used to catch circular contours.
       CALL GRCGRA(F,XST,YST,DFDX,DFDY,0,1,IFLGST)
*   Check initial position.
       IF(IFLGST.NE.0)THEN
           IF(LDEBUG)WRITE(10,'(''  ++++++ GRCTRA DEBUG   : Initial'',
     -          '' point has non-zero gradient flag: '',I3)') IFLGST
           RETURN
       ENDIF
*   Gradient calculated successfully, store the segment.
       XSEG1=XST-DFDX*STINIT
       YSEG1=YST-DFDY*STINIT
       XSEG2=XST+DFDX*STINIT
       YSEG2=YST+DFDY*STINIT
*** Initialise the previous step, used from step 2 onwards.
       XL=XST
       YL=YST
*** Initialise stepping.
       H=STINIT
       X0=XST
       Y0=YST
*** Start of the stepping procedure.
       ISTEP=0
100    CONTINUE
       ISTEP=ISTEP+1
*** Step to the side orthogonal to the gradient.
       CALL GRCGRA(F,X0,Y0,DFDX,DFDY,1,1,IFLG0)
       IF(IFLG0.NE.0)GOTO 3010
       IF(DFDX**2+DFDY**2.LE.0)GOTO 3000
       X1=X0+DIR*DFDX*H
       Y1=Y0+DIR*DFDY*H
*** Newton-Raphson step back to the contour following the gradient.
       X2=X1
       Y2=Y1
       CALL F(X2,Y2,F2,ILOC2)
       NFC=NFC+1
       DO 10 INITER=1,NNITER
       CALL GRCGRA(F,X2,Y2,DFDX,DFDY,0,0,IFLG2)
       DFNORM=DFDX**2+DFDY**2
       IF(IFLG2.NE.0)GOTO 3010
       IF(DFNORM.LE.0.0)GOTO 3000
       X2=X2+DFDX*(FC-F2)/DFNORM
       Y2=Y2+DFDY*(FC-F2)/DFNORM
       CALL F(X2,Y2,F2,ILOC2)
       NFC=NFC+1
       IF(LDEBUG)WRITE(10,'(1X,A,I3,A,I2,A,3E15.8)')
     -      ' ++++++ GRCTRA DEBUG   : Step ',ISTEP,' Newton iteration ',
     -      INITER,' leads to (x,y,f) = ',X2,Y2,F2
       IF(ABS(F2-FC).LE.EPSTRA*(1.0+ABS(FC)))THEN
            IF(LDEBUG)WRITE(10,'(1X,A,I2,A)')
     -           ' ++++++ GRCTRA DEBUG   : Newton search converged'//
     -           ' at step ',INITER,'.'
            GOTO 20
       ENDIF
10     CONTINUE
       WRITE(10,'(1X,A)') ' !!!!!! GRCTRA WARNING : Newton search'//
     -      ' didn''t converge ; tracing terminated.'
       CALL GRCPLT(X2,Y2,FC,'PLOT')
       RETURN
20     CONTINUE
*** Update the stepsize.
*** Check whether we are leaving the box.
       IF(X2.LE.CXMIN.OR.X2.GE.CXMAX.OR.Y2.LE.CYMIN.OR.Y2.GE.CYMAX)THEN
            CALL CLIP(X0,Y0,X2,Y2,CXMIN,CYMIN,CXMAX,CYMAX,IFAIL)
            CALL GRCPLT(X2,Y2,FC,'ADD')
            CALL GRCPLT(X2,Y2,FC,'PLOT')
            IFLAG=0
            IF(X2.LE.CXMIN)IFLAG=IFLAG+1
            IF(X2.GE.CXMAX)IFLAG=IFLAG+2
            IF(Y2.LE.CYMIN)IFLAG=IFLAG+4
            IF(Y2.GE.CYMAX)IFLAG=IFLAG+8
            CALL GRCUPD(F,X2,Y2,FC,'EDGE,END',IFLAG)
            IF(LDEBUG)WRITE(10,'(1X,A,I3,A,2E15.8)')
     -           ' ++++++ GRCTRA DEBUG   : Contour leaves area, step ',
     -           ISTEP,' tracing ended at ',X2,Y2
            RETURN
       ENDIF
*** Check whether we have a full circle.
       IF(ISTEP.GT.1.AND.CROSS(X0,Y0,X2,Y2,XSEG1,YSEG1,XSEG2,YSEG2))THEN
            CALL GRCPLT(X2,Y2,FC,'ADD')
            CALL GRCPLT(X2,Y2,FC,'PLOT')
            CALL GRCUPD(F,X2,Y2,FC,'LOOP,END',IFLAG)
            IF(LDEBUG)WRITE(10,'(1X,A,I3,A,2E15.8)')
     -           ' ++++++ GRCTRA DEBUG   : Full loop detected at step ',
     -           ISTEP,' tracing ended at ',X2,Y2
            LOOP=.TRUE.
            RETURN
       ENDIF
*** Make sure to avoid going back and forth, e.g. on a saddle point.
       IF(ISTEP.GT.1.AND.(X2-X0)*(X0-XL)+(Y2-Y0)*(Y0-YL).LT.0)THEN
            CALL GRCPLT(X2,Y2,FC,'DUMP')
            CALL GRCUPD(F,X2,Y2,FC,'TURN,END',IFLAG)
            IF(LDEBUG)WRITE(10,'(1X,A,I3,A,2E15.8)')
     -           ' ++++++ GRCTRA DEBUG   : Attempt to turn at step ',
     -           ISTEP,' tracing ended at ',X2,Y2
            RETURN
       ENDIF
*** Check the number of steps.
       IF(ISTEP.GT.NGCMAX)THEN
            WRITE(10,'(1X,A)') ' !!!!!! GRCTRA WARNING : Maximum'//
     -           ' number of steps reached, contour abandoned.'
            CALL GRCPLT(X2,Y2,FC,'ADD')
            CALL GRCPLT(X2,Y2,FC,'PLOT')
            CALL GRCUPD(F,X2,Y2,FC,'MAX,END',IFLAG)
            RETURN
       ENDIF
*** Check we didn't miss a grid point.
*** Add the point to the plotting buffer.
       XL=X0
       YL=Y0
       X0=X2
       Y0=Y2
       CALL GRCPLT(X0,Y0,FC,'ADD')
       CALL GRCUPD(F,X0,Y0,FC,'AREA',IFLAG)
       IF(IFLAG.NE.0)THEN
            IF(LDEBUG)WRITE(10,'(1X,A)') ' ++++++ GRCTRA DEBUG   :'//
     -           ' GRCUPD has raised IFLAG ; tracing abandoned.'
            RETURN
       ENDIF
*** New step.
       GOTO 100
*** Errors.
3000   CONTINUE
       CALL GRCPLT(X2,Y2,FC,'DUMP')
       WRITE(10,'(1X,A,I3,A)') ' !!!!!! GRCTRA WARNING : Zero'//
     -      ' gradient at step ',ISTEP,'; tracing terminated.'
       RETURN
3010   CONTINUE
       CALL GRCPLT(X2,Y2,FC,'DUMP')
       WRITE(10,'(1X,A,2E12.5,A,I3,A)') ' !!!!!! GRCTRA WARNING :'//
     -      ' Stepped into forbidden zone, at ',X2,Y2,' (step ',ISTEP,
     -      '); tracing terminated.'
       END
