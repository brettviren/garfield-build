CDECK  ID>, MSNFIT.
       SUBROUTINE MSNFIT(X,Y,EY,N,LPRINT,LFITK3,AA,EA,IFAIL)
*-----------------------------------------------------------------------
*   MSNFIT - Fits a Mathieson distribution.
*   (Last changed on 14/10/06.)
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
       REAL X(*),Y(*),EY(*)
       DOUBLE PRECISION XX(MXLIST),YY(MXLIST),EEY(MXLIST),
     -      AA(6),EA(6),S0,S1,S2,CHI2
       INTEGER N,IFAIL,I,NUSE
       LOGICAL LPRINT,LFITK3
       EXTERNAL MSNFUN
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
            PRINT *,' !!!!!! MSNFIT WARNING : Integrated contents'//
     -           ' is not larger than zero; no fit.'
            IFAIL=1
            RETURN
       ELSEIF(NUSE.LE.3)THEN
            PRINT *,' !!!!!! MSNFIT WARNING : Fewer than 4 non-zero'//
     -           ' data points; no fit.'
            IFAIL=1
            RETURN
       ELSEIF(AA(3).LE.0.AND..NOT.LFITK3)THEN
            PRINT *,' !!!!!! MSNFIT WARNING : K3 is to be fixed, but'//
     -           ' its value is not > 0; no fit.'
            IFAIL=1
            RETURN
       ENDIF
*** Make a reasonable initial guess.
       AA(1)=S1/S0
       AA(2)=S0
       AA(4)=X(2)-X(1)
       AA(5)=X(1)-AA(4)/2
***  from Sigma = SQRT(MAX(0.0D0,(S2-S1**2/S0)/S0))
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MSNFIT DEBUG   : Guess'',
     -      '' before fit: ''/
     -      26X,''Centre:        '',E15.8,'' [cm]''/
     -      26X,''Normalisation: '',E15.8/
     -      26X,''K3:            '',E15.8/
     -      26X,''Strip width:   '',E15.8,'' [cm]''/
     -      26X,''x Offset:      '',E15.8,'' [cm]''/
     -      26X,''Anode-cathode: '',E15.8,'' [cm]'')') AA
*** Call LSQFIT to do the real fit.
       IF(LFITK3)THEN
            CALL LSQFIT(MSNFUN,AA,EA,3,XX,YY,EEY,N,200,0.01*AA(2)/N,
     -           CHI2,1.0D-3,LPRINT,IFAIL)
       ELSE
            CALL LSQFIT(MSNFUN,AA,EA,2,XX,YY,EEY,N,200,0.01*AA(2)/N,
     -           CHI2,1.0D-3,LPRINT,IFAIL)
            EA(3)=0
       ENDIF
       END
