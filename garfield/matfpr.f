CDECK  ID>, MATFPR.
       SUBROUTINE MATFPR(IREFX,IREFY,IREFEY,OPTION,
     -      FACT,OFF,SLOPE,THETA,EFACT,EOFF,ESLOPE,ETHETA,IFAIL)
*-----------------------------------------------------------------------
*   MATFPR - Fits a Polya distribution to a matrix.
*   (Last changed on 21/ 8/96.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL LPRINT,LPLOT,LSCALE,LAUTO
       CHARACTER*(*) OPTION
       REAL FACT,OFF,SLOPE,THETA,EFACT,EOFF,ESLOPE,ETHETA,
     -      XPL(MXLIST),YPL(MXLIST),XMIN,XMAX,YMIN,YMAX
       DOUBLE PRECISION AA(4),EA(4),XX,YY
       INTEGER IFAIL,MATSLT,ISX,ISY,ISEY,IREFX,IREFY,IREFEY
       EXTERNAL MATSLT
*** Assume the fit will fail.
       IFAIL=1
*** Locate the matrices.
       ISX=MATSLT(IREFX)
       ISY=MATSLT(IREFY)
       ISEY=MATSLT(IREFEY)
*   Make sure that they exist.
       IF(ISX.LE.0.OR.ISY.LE.0.OR.ISEY.LE.0)THEN
            PRINT *,' !!!!!! MATFPR WARNING : One or more matrix'//
     -           ' references not valid; no fit.'
            RETURN
*   Make sure they are 1-dimensional.
       ELSEIF(MDIM(ISX).NE.1.OR.MDIM(ISY).NE.1.OR.MDIM(ISEY).NE.1)THEN
            PRINT *,' !!!!!! MATFPR WARNING : One or more matrices'//
     -           ' is not 1-dimensional; no fit.'
            RETURN
*   Make sure there are the same length and sufficiently long.
       ELSEIF(MLEN(ISX).NE.MLEN(ISY).OR.MLEN(ISY).NE.MLEN(ISEY).OR.
     -      MLEN(ISX).LT.4)THEN
            PRINT *,' !!!!!! MATFPR WARNING : Matrix dimensions not'//
     -           ' compatible or too small; no fit.'
            RETURN
       ENDIF
*** Decode the option string.
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
*** Call the fitting routine.
       AA(1)=FACT
       AA(2)=THETA
       AA(3)=OFF
       AA(4)=SLOPE
       CALL PYAFIT(MVEC(MORG(ISX)+1),MVEC(MORG(ISY)+1),
     -      MVEC(MORG(ISEY)+1),MLEN(ISX),LPRINT,.TRUE.,LSCALE,LAUTO,
     -      AA,EA,IFAIL)
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
*   Determine scale.
            DO 20 I=1,MLEN(ISX)
            IF(I.EQ.1)THEN
                 XMIN=MVEC(MORG(ISX)+I)
                 XMAX=MVEC(MORG(ISX)+I)
                 YMIN=MVEC(MORG(ISY)+I)-ABS(MVEC(MORG(ISEY)+I))
                 YMAX=MVEC(MORG(ISY)+I)+ABS(MVEC(MORG(ISEY)+I))
            ELSE
                 XMIN=MIN(XMIN,MVEC(MORG(ISX)+I))
                 XMAX=MAX(XMAX,MVEC(MORG(ISX)+I))
                 YMIN=MIN(YMIN,MVEC(MORG(ISY)+I)-
     -                ABS(MVEC(MORG(ISEY)+I)))
                 YMAX=MAX(YMAX,MVEC(MORG(ISY)+I)+
     -                ABS(MVEC(MORG(ISEY)+I)))
            ENDIF
20          CONTINUE
*   Plot frame.
            CALL GRCART(XMIN-0.1*(XMAX-XMIN),0.9*YMIN,
     -           XMAX+0.1*(XMAX-XMIN),1.1*YMAX,
     -           'Multiplication','Frequency','Polya fit')
*   Plot the error bars.
            CALL MATERR(IREFX,IREFY,0,IREFEY,0,IREFEY,'CIRCLE',0.01)
*   Prepare the plot vector.
            DO 30 I=1,MXLIST
            XPL(I)=XMIN+REAL(I-1)*(XMAX-XMIN)/REAL(MXLIST-1)
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
            CALL GRALOG('Polya fit to a matrix.')
       ENDIF
       END
