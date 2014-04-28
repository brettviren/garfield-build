CDECK  ID>, HISREB.
       SUBROUTINE HISREB(IREF1,NGROUP,IREF2,IFAIL)
*-----------------------------------------------------------------------
*   HISREB - Rebins a histogram.
*   (Last changed on 12/ 9/99.)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER IREF1,IREF2,IFAIL,IFAIL1,NGROUP,I,II,NBIN
       REAL X0,X1
*** Assume this will fail.
       IFAIL=1
*** Check reference number.
       IF(IREF1.LE.0.OR.IREF1.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISREB WARNING : Invalid histogram'//
     -           ' reference; no rebinning.'
            RETURN
*   See whether the histogram is in use.
       ELSEIF(.NOT.HISUSE(IREF1))THEN
            PRINT *,' !!!!!! HISREB WARNING : Histogram is not'//
     -           ' currently in use; no rebinning.'
            RETURN
*   See whether the range is set.
       ELSEIF(.NOT.SET(IREF1))THEN
            PRINT *,' !!!!!! HISREB WARNING : Range not yet set;'//
     -           ' no rebinning.'
            RETURN
*   Make sure that the grouping makes sense.
       ELSEIF(NGROUP.LE.1.OR.NGROUP.GT.NCHA(IREF1))THEN
            PRINT *,' !!!!!! HISREB WARNING : Number of bins to'//
     -           ' be grouped out of range; no rebinning.'
            RETURN
       ELSEIF(NCHA(IREF1).NE.NGROUP*(NCHA(IREF1)/NGROUP))THEN
            PRINT *,' ------ HISREB MESSAGE : Grouping does not'//
     -           ' divide number of bins; binned data will be lost.'
       ENDIF
*** Compute the parameters of the new histogram.
       NBIN=NCHA(IREF1)/NGROUP
       X0=XMIN(IREF1)
       X1=X0+NBIN*NGROUP*(XMAX(IREF1)-XMIN(IREF1))/REAL(NCHA(IREF1))
*** Allocate a new histogram.
       CALL HISADM('ALLOCATE',IREF2,NBIN,X0,X1,.FALSE.,IFAIL1)
*   Ensure that this has worked.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISREB WARNING : Unable to allocate'//
     -           ' space for the rebinned histogram.'
            RETURN
       ENDIF
*** Fill the new histogram.
       CONTEN(IREF2,0)=CONTEN(IREF1,0)
       CONTEN(IREF2,NCHA(IREF2)+1)=CONTEN(IREF1,NCHA(IREF1)+1)
       DO 10 I=1,NCHA(IREF1)
       II=1+(I-1)/NGROUP
       IF(II.LE.NBIN)THEN
            CONTEN(IREF2,II)=CONTEN(IREF2,II)+CONTEN(IREF1,I)
       ELSE
            CONTEN(IREF2,NCHA(IREF2)+1)=CONTEN(IREF1,NCHA(IREF1)+1)+
     -           CONTEN(IREF1,I)
       ENDIF
10     CONTINUE
*** Copy entries and summing information.
       SX0(IREF2)=SX0(IREF1)
       SX1(IREF2)=SX1(IREF1)
       SX2(IREF2)=SX2(IREF1)
       NENTRY(IREF2)=NENTRY(IREF1)
*** Seems to have worked.
       IFAIL=0
       END
