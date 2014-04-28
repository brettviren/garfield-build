CDECK  ID>, MATDER.
       SUBROUTINE MATDER(IRX,IRY,XINT,DERIV,OPTION,IFAIL)
*-----------------------------------------------------------------------
*   MATDER - Computes a numerical derivative of one vector interpolated
*            vs another vector.
*   (Last changed on  8/ 5/96.)
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
       REAL F1,F2,FM,EPS,EPSMAX,DELTA,XINT,DERIV,DIVDIF
       INTEGER I,N,ITER,NITMAX,INIT,IORD,IRX,IRY,MATSLT,MATADR,IFAIL
       CHARACTER*(*) OPTION
       EXTERNAL DIVDIF,MATSLT,MATADR
       SAVE INIT,NITMAX,DELTA
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATDER ///'
*** Preset the IFAIL flag.
       IFAIL=1
*** Decode the option string.
       IORD=2
       IF(INDEX(OPTION,'LINEAR').NE.0)THEN
            IORD=1
       ELSEIF(INDEX(OPTION,'PARABOLIC')+
     -      INDEX(OPTION,'QUADRATIC').NE.0)THEN
            IORD=2
       ELSEIF(INDEX(OPTION,'CUBIC').NE.0)THEN
            IORD=3
       ENDIF
*** Locate the matrices.
       ISX=MATSLT(IRX)
       ISY=MATSLT(IRY)
       IF(ISX.LE.0.OR.ISY.LE.0)THEN
            PRINT *,' !!!!!! MATDER WARNING : Unable to find an'//
     -           ' input vector; no derivative.'
            RETURN
       ENDIF
       IF(MDIM(ISX).NE.1.OR.MDIM(ISY).NE.1.OR.
     -      MLEN(ISX).NE.MLEN(ISY).OR.
     -      MLEN(ISX).LT.IORD+1.OR.
     -      MLEN(ISY).LT.IORD+1)THEN
            PRINT *,' !!!!!! MATDER WARNING : Input matrices not'//
     -           ' 1D, not same length or too short; no derivative.'
            RETURN
       ENDIF
       N=MIN(MLEN(ISX),MLEN(ISY))
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATDER DEBUG   : x='',I5,
     -      '', y='',I5,'' length='',I5,'' order='',I5)') IRX,IRY,N,IORD
*** Check proper sequence.
       DO 100 I=1,N-1
       IF(MVEC(MORG(ISX)+I).GE.MVEC(MORG(ISX)+I+1))THEN
            PRINT *,' !!!!!! MATDER WARNING : Ordinates not ordered'//
     -           ' ; no derivative calculated.'
            RETURN
       ENDIF
100    CONTINUE
*** Initialise delta.
       DATA INIT/0/
       IF(INIT.EQ.0)THEN
*   Set number of iterations.
            NITMAX=50
*   Compute DELTA.
            DELTA=1
            ITER=0
10          CONTINUE
            ITER=ITER+1
            IF(1+DELTA.GT.1)THEN
                 DELTA=DELTA/2
                 IF(ITER.LE.NITMAX)GOTO 10
                 DELTA=1E-8
            ENDIF
            DELTA=SQRT(DELTA)
*   Initialisation done.
            INIT=1
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATDER DEBUG   :'',
     -           '' Delta='',E12.5,'', Iter max='',I5)') DELTA,NITMAX
       ENDIF
*** Find minimum and maximum value for EPS.
       DO 20 I=1,N-1
*   Intermediate points.
       IF((MVEC(MORG(ISX)+I)-XINT)*(XINT-MVEC(MORG(ISX)+I+1)).GE.0)THEN
            EPSMAX=MAX(DELTA,ABS(XINT-MVEC(MORG(ISX)+I)),
     -           ABS(XINT-MVEC(MORG(ISX)+I+1)))
            GOTO 30
       ENDIF
20     CONTINUE
*   External points.
       IF(XINT.LT.MVEC(MORG(ISX)+1))THEN
            EPSMAX=MAX(DELTA,2*ABS(XINT-MVEC(MORG(ISX)+1)))
       ELSE
            EPSMAX=MAX(DELTA,2*ABS(XINT-MVEC(MORG(ISX)+N)))
       ENDIF
30     CONTINUE
*** Iterate to find the proper value for EPS, starting values.
       FM=DIVDIF(MVEC(MORG(ISY)+1),MVEC(MORG(ISX)+1),N,XINT,IORD)
       EPS=DELTA*(1+ABS(XINT))
       ITER=0
*   Loop.
40     CONTINUE
*   Increment iteration counter to avoid endless loops.
       ITER=ITER+1
*   Compute function values at x +/- eps.
       F1=DIVDIF(MVEC(MORG(ISY)+1),MVEC(MORG(ISX)+1),N,XINT+EPS,IORD)
       F2=DIVDIF(MVEC(MORG(ISY)+1),MVEC(MORG(ISX)+1),N,XINT-EPS,IORD)
*   Update EPS accordingly.
       IF(ITER.GT.NITMAX)THEN
            GOTO 50
       ELSEIF(ABS(F1-F2).GT.5*DELTA*MAX(ABS(F1),ABS(FM),ABS(F2)))THEN
            EPS=EPS/2
            IF(EPS.GT.EPSMAX)GOTO 50
       ELSEIF(ABS(F1-F2).LT.DELTA*MAX(ABS(F1),ABS(FM),ABS(F2))/5)THEN
            EPS=2*EPS
       ELSE
            GOTO 50
       ENDIF
       GOTO 40
50     CONTINUE
*** Set the derivative.
       DERIV=(F1-F2)/(2*EPS)
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATDER DEBUG   : For x='',
     -      E12.5,'' found dy='',E12.5)') XINT,DERIV
*** Seems to have worked.
       IFAIL=0
       END
