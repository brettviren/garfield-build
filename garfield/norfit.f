CDECK  ID>, NORFIT.
       SUBROUTINE NORFIT(X,Y,EY,N,LPRINT,AA,EA,IFAIL)
*-----------------------------------------------------------------------
*   NORFIT - Fits a Gaussian.
*   (Last changed on 25/ 2/97.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL X(*),Y(*),EY(*),FACT,AVER,SIGMA
       DOUBLE PRECISION XX(MXLIST),YY(MXLIST),EEY(MXLIST),
     -      AA(3),EA(3),S0,S1,S2,CHI2
       INTEGER N,IFAIL,I,NUSE
       LOGICAL LPRINT
       EXTERNAL NORFUN
*** Estimate fitting results.
       S0=0
       S1=0
       S2=0
       NUSE=0
       DO 10 I=1,N
       IF(Y(I).GT.0)NUSE=NUSE+1
       S0=S0+Y(I)
       S1=S1+Y(I)*X(I)
       S2=S2+Y(I)*X(I)**2
       XX(I)=X(I)
       YY(I)=Y(I)
       EEY(I)=EY(I)
10     CONTINUE
*** Avoid divide by zero.
       IF(S0.LE.0)THEN
            FACT=0
            AVER=0
            SIGMA=0
            IFAIL=1
            PRINT *,' !!!!!! NORFIT WARNING : Integrated contents'//
     -           ' too small for fit; no fit.'
            RETURN
       ELSEIF(NUSE.LE.3)THEN
            FACT=0
            AVER=0
            SIGMA=0
            IFAIL=1
            PRINT *,' !!!!!! NORFIT WARNING : Too few non-zero data'//
     -           ' points; no fit.'
            RETURN
       ENDIF
*** Make a reasonable initial guess.
       AA(1)=(X(N)-X(1))*S0/REAL(N)
       AA(2)=S1/S0
       AA(3)=SQRT(MAX(0.0D0,(S2-S1**2/S0)/S0))
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ NORFIT DEBUG   : Guess'',
     -      '' before fit: f/m/s='',3E15.8)') AA
*** Call LSQFIT to do the real fit.
       CALL LSQFIT(NORFUN,AA,EA,3,XX,YY,EEY,N,200,0.01*AA(1)/N,
     -      CHI2,1.0D-3,LPRINT,IFAIL)
       END
