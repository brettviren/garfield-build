CDECK  ID>, HISCUT.
       SUBROUTINE HISCUT(IREF1,X0,X1,IREF2,IFAIL)
*-----------------------------------------------------------------------
*   HISCUT - Cuts a piece from a histogram.
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
       INTEGER IREF1,IREF2,IFAIL,IFAIL1,I0,I1,I,IAUX
       REAL X0,X1,XX0,XX1
*** Assume this will fail.
       IFAIL=1
*** Check reference number.
       IF(IREF1.LE.0.OR.IREF1.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISCUT WARNING : Invalid histogram'//
     -           ' reference; no sub-range.'
            RETURN
*   See whether the histogram is in use.
       ELSEIF(.NOT.HISUSE(IREF1))THEN
            PRINT *,' !!!!!! HISCUT WARNING : Histogram is not'//
     -           ' currently in use; no sub-range.'
            RETURN
*   See whether the range is set.
       ELSEIF(.NOT.SET(IREF1))THEN
            PRINT *,' !!!!!! HISCUT WARNING : Range not yet set;'//
     -           ' no sub-range.'
            RETURN
*   Ensure that the range at least partially overlaps.
       ELSEIF(MAX(X0,X1).LT.XMIN(IREF1).OR.
     -      MIN(X0,X1).GT.XMAX(IREF1))THEN
            PRINT *,' !!!!!! HISCUT WARNING : Sub-range does not'//
     -           ' overlap with histogram range ; no sub-range.'
            RETURN
*   Warn if there is only a partial overlap.
       ELSEIF((XMIN(IREF1)-X0)*(X0-XMAX(IREF1)).LT.0.OR.
     -      (XMIN(IREF1)-X1)*(X1-XMAX(IREF1)).LT.0)THEN
            PRINT *,' ------ HISCUT MESSAGE : Sub-range overlaps'//
     -           ' only partially with histogram range.'
       ENDIF
*** Compute the parameters of the new histogram.
       I0=1+INT(REAL(NCHA(IREF1))*(X0-XMIN(IREF1))/
     -      (XMAX(IREF1)-XMIN(IREF1)))
       I1=1+INT(REAL(NCHA(IREF1))*(X1-XMIN(IREF1))/
     -      (XMAX(IREF1)-XMIN(IREF1)))
*   Reorder if needed.
       IF(I1.LT.I0)THEN
            IAUX=I1
            I1=I0
            I0=I1
       ENDIF
*   Verify boundaries.
       IF(I0.LT.1)I0=1
       IF(I1.GT.NCHA(IREF1))I1=NCHA(IREF1)
       IF(I0.GT.NCHA(IREF1).OR.I1.LT.1)THEN
            PRINT *,' !!!!!! HISCUT WARNING : Sub-range does not'//
     -           ' overlap with histogram range ; no sub-range.'
            RETURN
       ENDIF
*   Ensure that there is at least 1 bin left.
       IF(I0.GE.I1)THEN
            PRINT *,' !!!!!! HISCUT WARNING : Sub-range overlaps'//
     -           ' with less than 1 bin with histogram ; no sub-range.'
            RETURN
       ENDIF
*   Range.
       XX0=XMIN(IREF1)+(I0-1)*(XMAX(IREF1)-XMIN(IREF1))/
     -      REAL(NCHA(IREF1))
       XX1=XMIN(IREF1)+I1*(XMAX(IREF1)-XMIN(IREF1))/REAL(NCHA(IREF1))
*** Allocate a new histogram.
       CALL HISADM('ALLOCATE',IREF2,I1-I0+1,XX0,XX1,.FALSE.,IFAIL1)
*   Ensure that this has worked.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISCUT WARNING : Unable to allocate'//
     -           ' space for the sub-range histogram.'
            RETURN
       ENDIF
*** Fill the new histogram.
       CONTEN(IREF2,0)=CONTEN(IREF1,0)
       CONTEN(IREF2,NCHA(IREF2)+1)=CONTEN(IREF1,NCHA(IREF1)+1)
       DO 10 I=1,NCHA(IREF1)
       IF(I.LT.I0)THEN
            CONTEN(IREF2,0)=CONTEN(IREF2,0)+CONTEN(IREF1,I)
       ELSEIF(I.GE.I0.AND.I.LE.I1)THEN
            CONTEN(IREF2,I-I0+1)=CONTEN(IREF1,I)
       ELSE
            CONTEN(IREF2,NCHA(IREF2)+1)=CONTEN(IREF2,NCHA(IREF2)+1)+
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
