CDECK  ID>, HISFPR.
       SUBROUTINE HISFPR(IREF,OPTION,FACT,OFF,SLOPE,THETA,
     -      EFACT,EOFF,ESLOPE,ETHETA,IFAIL)
*-----------------------------------------------------------------------
*   HISFPR - Fits a Polya distribution to a histogram.
*   (Last changed on 19/ 8/96.)
*-----------------------------------------------------------------------
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
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
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
       PARAMETER (MXMAP =350000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL LSQRT,LAUTO,LSCALE,LPRINT,LPLOT
       CHARACTER*(*) OPTION
       REAL FACT,OFF,SLOPE,THETA,EFACT,EOFF,ESLOPE,ETHETA,
     -      X(MXCHA),Y(MXCHA),EY(MXCHA),XPL(MXLIST),YPL(MXLIST)
       DOUBLE PRECISION AA(4),EA(4),XX,YY
       INTEGER IFAIL,IFAIL1
*** Assume the fit will fail.
       IFAIL=1
*** Check reference number.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISFPR WARNING : Histogram reference'//
     -           ' not valid; histogram not fitted.'
            RETURN
*** No entries yet.
       ELSEIF(NENTRY(IREF).EQ.0.OR.SX0(IREF).EQ.0)THEN
            PRINT *,' !!!!!! HISFPR WARNING : Histogram has no'//
     -           ' entries yet; histogram not fitted.'
            RETURN
*** Range not yet set.
       ELSEIF(.NOT.SET(IREF))THEN
            PRINT *,' !!!!!! HISFPR WARNING : Range of this auto'//
     -           'range histogram not yet set; histogram not fitted.'
            RETURN
       ENDIF
*** Decode the option string.
       LSQRT=.TRUE.
       LPRINT=.FALSE.
       LPLOT=.FALSE.
       LAUTO=.TRUE.
       LSCALE=.TRUE.
       IF(INDEX(OPTION,'NOPLOT').NE.0)THEN
            LPLOT=.FALSE.
       ELSEIF(INDEX(OPTION,'PLOT').NE.0)THEN
            LPLOT=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'NOPRINT').NE.0)THEN
            LPRINT=.FALSE.
       ELSEIF(INDEX(OPTION,'PRINT').NE.0)THEN
            LPRINT=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'EQUAL').NE.0)THEN
            LSQRT=.FALSE.
       ELSEIF(INDEX(OPTION,'POISSON').NE.0)THEN
            LSQRT=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'FIT').NE.0)THEN
            LSCALE=.TRUE.
       ELSEIF(INDEX(OPTION,'FIX').NE.0)THEN
            LSCALE=.FALSE.
       ENDIF
       IF(INDEX(OPTION,'AUTO').NE.0)THEN
            LAUTO=.TRUE.
       ELSEIF(INDEX(OPTION,'MANUAL').NE.0)THEN
            LAUTO=.FALSE.
       ENDIF
*** Prepare the arrays.
       DO 10 I=1,NCHA(IREF)
       X(I)=XMIN(IREF)+(I-0.5)*(XMAX(IREF)-XMIN(IREF))/REAL(NCHA(IREF))
       Y(I)=CONTEN(IREF,I)
       IF(LSQRT)THEN
            EY(I)=SQRT(Y(I)+1)
       ELSE
            EY(I)=1
       ENDIF
10     CONTINUE
*** Call the fitting routine.
       AA(1)=FACT
       AA(2)=THETA
       AA(3)=OFF
       AA(4)=SLOPE
       CALL PYAFIT(X,Y,EY,NCHA(IREF),
     -      LPRINT,LSQRT,LSCALE,LAUTO,AA,EA,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISFPL WARNING : The Polya fit'//
     -           ' failed.'
            RETURN
       ENDIF
       FACT=AA(1)
       THETA=AA(2)
       OFF=AA(3)
       SLOPE=AA(4)
       EFACT=EA(1)
       ETHETA=EA(2)
       EOFF=EA(3)
       ESLOPE=EA(4)
*** Make a plot of the fit, start plotting the frame.
       IF(LPLOT)THEN
*   Switch to logarithmic scale.
            CALL GRAOPT('LIN-X, LOG-Y')
*   Make the plot.
            CALL HISPLT(IREF,'Coordinate','Histogram',.TRUE.)
*   Plot the error bars.
            CALL GRATTS('FUNCTION-1','POLYLINE')
            IF(LSQRT)THEN
                 DO 20 I=1,NCHA(IREF)
                 XPL(1)=XMIN(IREF)+(I-0.5)*(XMAX(IREF)-XMIN(IREF))/
     -                REAL(NCHA(IREF))
                 YPL(1)=Y(I)+EY(I)
                 XPL(2)=XPL(1)
                 YPL(2)=Y(I)-EY(I)
                 CALL GRLINE(2,XPL,YPL)
20               CONTINUE
            ENDIF
*   Prepare the plot vector.
            DO 30 I=1,MXLIST
            XPL(I)=XMIN(IREF)+REAL(I-1)*(XMAX(IREF)-XMIN(IREF))/
     -           REAL(MXLIST-1)
            XX=XPL(I)
            CALL PYAFUN(XX,AA,YY)
            YPL(I)=YY
30          CONTINUE
*   Set the attributes.
            CALL GRATTS('FUNCTION-2','POLYLINE')
*   Plot the line itself.
            CALL GRLINE(MXLIST,XPL,YPL)
*   Close the plot.
            CALL GRNEXT
*   Switch to normal mode.
            CALL GRAOPT('LIN-X, LIN-Y')
*   Register the plot.
            CALL GRALOG('Polya fit to a histogram')
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
