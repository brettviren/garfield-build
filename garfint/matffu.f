CDECK  ID>, MATFFU.
       SUBROUTINE MATFFU(IREFX,IREFY,IREFEY,FUN,OPTION,IA,IE,NPAR,IFAIL)
*-----------------------------------------------------------------------
*   MATFFU - Fits a function to a matrix.
*   (Last changed on 20/ 2/97.)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL LPRINT,LPLOT
       CHARACTER*(*) OPTION,FUN
       CHARACTER*40 TITLE
       REAL XPL(MXLIST),YPL(MXLIST),XMIN,XMAX,YMIN,YMAX
       DOUBLE PRECISION AA(MXFPAR),XX,YY
       INTEGER IFAIL,IFAIL1,NPAR,NNA,IIA,IA(*),IE(*),I,IENTRY,MATSLT,
     -      ISX,ISY,ISEY,IREFX,IREFY,IREFEY
       COMMON /FFUDAT/ NNA,IENTRY,IIA(MXVAR)
       EXTERNAL MATSLT
*** Assume the fit will fail.
       IFAIL=1
*** Locate the matrices.
       ISX=MATSLT(IREFX)
       ISY=MATSLT(IREFY)
       ISEY=MATSLT(IREFEY)
*   Make sure that they exist.
       IF(ISX.LE.0.OR.ISY.LE.0.OR.ISEY.LE.0)THEN
            PRINT *,' !!!!!! MATFFU WARNING : One or more matrix'//
     -           ' references not valid; no fit.'
            RETURN
*   Make sure they are 1-dimensional.
       ELSEIF(MDIM(ISX).NE.1.OR.MDIM(ISY).NE.1.OR.MDIM(ISEY).NE.1)THEN
            PRINT *,' !!!!!! MATFFU WARNING : One or more matrices'//
     -           ' is not 1-dimensional; no fit.'
            RETURN
*   Make sure there are the same length and sufficiently long.
       ELSEIF(MLEN(ISX).NE.MLEN(ISY).OR.MLEN(ISY).NE.MLEN(ISEY).OR.
     -      MLEN(ISX).LT.NPAR.OR.NPAR.LT.1)THEN
            PRINT *,' !!!!!! MATFFU WARNING : Matrix dimensions not'//
     -           ' compatible or too small; no fit.'
            RETURN
       ENDIF
*** Decode the option string.
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
*** Call the fitting routine.
       CALL FUNFIT(FUN,MVEC(MORG(ISX)+1),MVEC(MORG(ISY)+1),
     -      MVEC(MORG(ISEY)+1),MLEN(ISX),LPRINT,IA,IE,NPAR,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MATFFU WARNING : The fit to ',FUN,
     -           ' failed.'
            CALL ALGCLR(IENTRY)
            RETURN
       ENDIF
*** Make a plot of the fit, start plotting the frame.
       IF(LPLOT)THEN
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
            WRITE(TITLE,'(''Fit to '',A)') FUN(1:MIN(LEN(FUN),33))
            CALL GRCART(XMIN-0.1*(XMAX-XMIN),YMIN-0.1*(YMAX-YMIN),
     -           XMAX+0.1*(XMAX-XMIN),YMAX+0.1*(YMAX-YMIN),
     -           'x','y',TITLE)
*   Plot the error bars.
            CALL MATERR(IREFX,IREFY,0,IREFEY,0,IREFEY,'CIRCLE',0.01)
*   Prepare the parameter list.
            DO 40 I=1,NPAR
            AA(I)=GLBVAL(IIA(I))
40          CONTINUE
*   Prepare the plot vector.
            DO 30 I=1,MXLIST
            XPL(I)=XMIN+REAL(I-1)*(XMAX-XMIN)/REAL(MXLIST-1)
            XX=XPL(I)
            CALL FUNFUN(XX,AA,YY)
            YPL(I)=YY
30          CONTINUE
*   Set the attributes.
            CALL GRATTS('FUNCTION-2','POLYLINE')
*   Plot the line itself.
            CALL GRLINE(MXLIST,XPL,YPL)
*   Close the plot.
            CALL GRNEXT
*   Register the plot.
            CALL GRALOG('Function fit to a matrix.')
       ENDIF
*** We're now done with the function, so can delete the entry point.
       CALL ALGCLR(IENTRY)
*** Seems to have worked.
       IFAIL=0
       END
