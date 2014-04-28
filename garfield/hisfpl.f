CDECK  ID>, HISFPL.
       SUBROUTINE HISFPL(IREF,OPTION,PAR,ERR,NPAR,IFAIL)
*-----------------------------------------------------------------------
*   HISFPL - Fits a polynomial to a histogram.
*   (Last changed on 12/ 6/96.)
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
       LOGICAL LSQRT,LPRINT,LPLOT
       CHARACTER*(*) OPTION
       REAL PAR(*),ERR(*),
     -      X(MXCHA),Y(MXCHA),EY(MXCHA),XPL(MXLIST),YPL(MXLIST)
       DOUBLE PRECISION AA(MXFPAR),EA(MXFPAR),XX,YY
       INTEGER IFAIL,IFAIL1,NPAR
*** Assume the fit will fail.
       IFAIL=1
*** Check reference number.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISFPL WARNING : Histogram reference'//
     -           ' not valid; histogram not fitted.'
            RETURN
*** No entries yet.
       ELSEIF(NENTRY(IREF).EQ.0.OR.SX0(IREF).EQ.0)THEN
            PRINT *,' !!!!!! HISFPL WARNING : Histogram has no'//
     -           ' entries yet; histogram not fitted.'
            RETURN
*** Range not yet set.
       ELSEIF(.NOT.SET(IREF))THEN
            PRINT *,' !!!!!! HISFPL WARNING : Range of this auto'//
     -           'range histogram not yet set; histogram not fitted.'
            RETURN
       ENDIF
*** Decode the option string.
       LSQRT=.TRUE.
       LPRINT=.FALSE.
       LPLOT=.FALSE.
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
       CALL POLFIT(X,Y,EY,NCHA(IREF),LPRINT,AA,EA,NPAR,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISFPL WARNING : The polynomial fit'//
     -           ' failed.'
            RETURN
       ENDIF
       DO 15 I=1,NPAR
       PAR(I)=REAL(AA(I))
       ERR(I)=REAL(EA(I))
15     CONTINUE
*** Make a plot of the fit, start plotting the frame.
       IF(LPLOT)THEN
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
            CALL POLFUN(XX,AA,YY)
            YPL(I)=YY
30          CONTINUE
*   Set the attributes.
            CALL GRATTS('FUNCTION-2','POLYLINE')
*   Plot the line itself.
            CALL GRLINE(MXLIST,XPL,YPL)
*   Close the plot.
            CALL GRNEXT
*   Register the plot.
            CALL GRALOG('Polynomial fit to a histogram.')
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
