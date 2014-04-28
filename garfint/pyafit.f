CDECK  ID>, PYAFIT.
       SUBROUTINE PYAFIT(X,Y,EY,N,LPRINT,LSQRT,LSCALE,LAUTO,AA,EA,IFAIL)
*-----------------------------------------------------------------------
*   PYAFIT - Fits a Polya distribution to a polynomial or histogram.
*   (Last changed on  3/ 6/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL X(*),Y(*),EY(*),XPL(200),YPL(200)
       DOUBLE PRECISION XXX,YYY,XX(MXLIST),YY(MXLIST),EEY(MXLIST),
     -      SFACT,SSIG,AA(*),EA(*),CHI2,D(2,4),YTOT,YSUM,YINT,
     -      XFIRST,XLAST,TOL
       INTEGER N,IFAIL,IFAIL1,NDATA,IWORK(2),I,NFIRST,NLAST,NSTART
       LOGICAL LPRINT,LSCALE,LAUTO,XSET,LSQRT
       EXTERNAL PYAFUN
*** Preset the error flag.
       IFAIL=1
*** Debugging and identification output.
       IF(LIDENT)PRINT *,' /// ROUTINE PYAFIT ///'
*** Check dimensions.
       IF(N.GT.MXLIST)THEN
            PRINT *,' !!!!!! PYAFIT WARNING : Dimensions of the'//
     -           ' problem exceed compilation parameters; no fit.'
            RETURN
       ENDIF
*** Copy the vectors, prepare matrix etc - first initialise.
       XSET=.FALSE.
       NFIRST=0
       NLAST=0
       DO 20 I=1,N
*   Vector copy.
       XX(I)=DBLE(X(I))
       YY(I)=DBLE(Y(I))
       EEY(I)=DBLE(EY(I))
*   Find smallest and largest x.
       IF(Y(I).GT.0)THEN
            IF(XSET)THEN
                 IF(XX(I).LT.XFIRST)XFIRST=XX(I)
                 IF(XX(I).GT.XLAST)XLAST=XX(I)
            ELSE
                 XFIRST=XX(I)
                 XLAST=XX(I)
                 XSET=.TRUE.
            ENDIF
*   Keep track of starting point
            IF(NFIRST.EQ.0)NFIRST=I
*   Keep track of end point
            NLAST=I
       ENDIF
20     CONTINUE
*** Exponential fit matrix: initialise
       D(1,1)=0
       D(1,2)=0
       D(1,3)=0
       D(2,1)=0
       D(2,2)=0
       D(2,3)=0
       YSUM=0
       YINT=0
       YTOT=0
       SFACT=0
       SSIG=0
       NDATA=0
       NSTART=NFIRST+(NLAST-NFIRST)/3
*   Sum terms.
       DO 30 I=1,N
       IF(EY(I).GT.0.AND.Y(I).GT.0.AND.I.GE.NSTART)THEN
            NDATA=NDATA+1
            D(1,1)=D(1,1)+               (Y(I)/EY(I))**2
            D(1,2)=D(1,2)+X(I)          *(Y(I)/EY(I))**2
            D(2,1)=D(2,1)+X(I)          *(Y(I)/EY(I))**2
            D(2,2)=D(2,2)+X(I)**2       *(Y(I)/EY(I))**2
            D(1,3)=D(1,3)+LOG(Y(I))     *(Y(I)/EY(I))**2
            D(2,3)=D(2,3)+LOG(Y(I))*X(I)*(Y(I)/EY(I))**2
       ENDIF
*   Normalisation for fixed scale fits.
       IF(I.GE.NSTART.AND..NOT.LSCALE)THEN
            SFACT=SFACT+EY(I)*Y(I)/EXP(-AA(3)-AA(4)*X(I))
            SSIG=SSIG+EY(I)
       ENDIF
*   Integral.
       YTOT=YTOT+Y(I)
       IF(I.EQ.1)THEN
            YINT=0
       ELSE
            YSUM=YSUM+0.5*(Y(I)+Y(I-1))*ABS(X(I)-X(I-1))
            IF(I.GT.NSTART)YINT=YINT+0.5*(Y(I)+Y(I-1))*
     -           ABS(X(I)-X(I-1))
       ENDIF
30     CONTINUE
*** See whether there are enough valid points.
       IF(NDATA.LT.4.OR.
     -      (.NOT.LSCALE.AND.SSIG.LE.0).OR.
     -      YSUM.LE.0.OR.YINT.LE.0.OR.
     -      XLAST.LE.XFIRST)THEN
            PRINT *,' !!!!!! PYAFIT WARNING : The problem is under-'//
     -           'determined (after eliminating y<=0 points); no fit.'
            RETURN
       ENDIF
*   Now solve the equation.
       CALL DEQN(2,D,2,IWORK,IFAIL1,1,D(1,3))
*   Check error condition.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! PYAFIT WARNING : Failure to obtain'//
     -           ' a first estimate of the solution; not solved.'
            RETURN
       ENDIF
*   Copy the solution.
       IF(LAUTO)THEN
            IF(D(2,3).EQ.0)THEN
                 PRINT *,' !!!!!! PYAFIT WARNING : Estimated scale'//
     -                ' is zero; no fit.'
                 RETURN
            ENDIF
            AA(2)=0.5
            IF(LSCALE)THEN
                 AA(1)=EXP(D(1,3)+D(2,3)*XFIRST+
     -                     0.01*ABS(D(2,3)*(XLAST-XFIRST)))/
     -                     ABS(D(2,3))
                 AA(3)=D(2,3)*XFIRST+
     -                0.01*ABS(D(2,3)*(XLAST-XFIRST))
                 AA(4)=-D(2,3)
            ELSE
                 AA(1)=SFACT/SSIG
            ENDIF
       ENDIF
*   Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ PYAFIT DEBUG   : Guess'',
     -           '' before fit: a_i=''/26X,4E15.8)') (AA(I),I=1,4)
*   Switch to logarithmic scale.
            CALL GRAOPT('LIN-X, LOG-Y')
*   Make the plot.
            CALL GRGRPH(X,Y,N,'x','y','Pre-fit situation')
*   Prepare the plot vector.
            DO 10 I=1,200
            XPL(I)=X(1)+REAL(I-1)*(X(N)-X(1))/199.0
            XXX=XPL(I)
            CALL PYAFUN(XXX,AA,YYY)
            YPL(I)=YYY
10          CONTINUE
*   Set the attributes.
            CALL GRATTS('FUNCTION-2','POLYLINE')
*   Slot the line itself.
            CALL GRLINE(200,XPL,YPL)
*   Close the plot.
            CALL GRNEXT
*   Switch to normal mode.
            CALL GRAOPT('LIN-X, LIN-Y')
       ENDIF
*** Now carry out the fit.
       IF(LSQRT)THEN
            TOL=3
       ELSE
            TOL=0.01*YTOT/N
       ENDIF
       IF(LSCALE)THEN
            CALL LSQFIT(PYAFUN,AA,EA,4,XX,YY,EEY,N,200,TOL,
     -           CHI2,1.0D-3,LPRINT,IFAIL)
       ELSE
            CALL LSQFIT(PYAFUN,AA,EA,2,XX,YY,EEY,N,200,TOL,
     -           CHI2,1.0D-3,LPRINT,IFAIL)
            EA(3)=0
            EA(4)=0
       ENDIF
       END
