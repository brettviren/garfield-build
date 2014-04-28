CDECK  ID>, POLFIT.
       SUBROUTINE POLFIT(X,Y,EY,N,LPRINT,AA,EA,NA,IFAIL)
*-----------------------------------------------------------------------
*   POLFIT - Fits a Polynomial
*   (Last changed on  9/ 6/96.)
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
       INTEGER NNA,IWORK(MXFPAR)
       COMMON /PFDAT/ NNA
       REAL X(*),Y(*),EY(*)
       DOUBLE PRECISION XX(MXLIST),YY(MXLIST),EEY(MXLIST),
     -      AA(*),EA(*),CHI2,D(MXFPAR,MXFPAR+2),AUX,YSUM
       INTEGER N,NA,IFAIL
       LOGICAL LPRINT
       EXTERNAL POLFUN
*** Preset the error flag.
       IFAIL=1
*** Debugging and identification output.
       IF(LIDENT)PRINT *,' /// ROUTINE POLFIT ///'
*** Check dimensions.
       IF(NA.GT.MXFPAR.OR.N.GT.MXLIST)THEN
            PRINT *,' !!!!!! POLFIT WARNING : Dimensions of the'//
     -           ' problem exceed compilation parameters; no fit.'
            RETURN
       ENDIF
*** Copy the vectors.
       YSUM=0
       DO 100 I=1,N
       XX(I)=DBLE(X(I))
       YY(I)=DBLE(Y(I))
       YSUM=YSUM+ABS(YY(I))
       EEY(I)=DBLE(EY(I))
100    CONTINUE
*** Estimate fitting results, first fill matrix.
       DO 10 I=0,2*(NA-1)
       IF(I.EQ.0)THEN
            AUX=N
       ELSE
            AUX=0
            DO 20 J=1,N
            AUX=AUX+XX(J)**I
20          CONTINUE
       ENDIF
       DO 30 J=1,NA
       K=I+2-J
       IF(K.LT.1.OR.K.GT.NA)GOTO 30
       D(J,K)=AUX
30     CONTINUE
10     CONTINUE
*   Left hand side.
       DO 40 I=0,NA-1
       AUX=0
       DO 50 J=1,N
       IF(I.EQ.0)THEN
            AUX=AUX+YY(J)
       ELSE
            AUX=AUX+YY(J)*XX(J)**I
       ENDIF
50     CONTINUE
       D(I+1,MXFPAR+1)=AUX
40     CONTINUE
*   Now solve the equation.
       CALL DEQN(NA,D,MXFPAR,IWORK,IFAIL1,1,D(1,MXFPAR+1))
*   Check error condition.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! POLFIT WARNING : Failure to obtain'//
     -           ' a first estimate of the solution; not solved.'
            RETURN
       ENDIF
*   Copy the solution.
       DO 60 I=1,NA
       AA(I)=D(I,MXFPAR+1)
60     CONTINUE
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ POLFIT DEBUG   : Guess'',
     -      '' before fit: a_i='',3E15.8,(/26X,5E15.8:))')
     -      (AA(I),I=1,NA)
*** Now carry out the fit.
       NNA=NA
       CALL LSQFIT(POLFUN,AA,EA,NA,XX,YY,EEY,N,200,0.01*YSUM/N,
     -      CHI2,1.0D-3,LPRINT,IFAIL)
       END
