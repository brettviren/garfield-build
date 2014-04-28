CDECK  ID>, HISPLT.
       SUBROUTINE HISPLT(IREF,XTXT,TITLE,FRAME)
*-----------------------------------------------------------------------
*   HISPLT - Plots a histogram via GRHIST.
*   (Last changed on 17/ 5/99.)
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
       CHARACTER*(*) XTXT,TITLE
       CHARACTER*20 AUX1,AUX2,AUX3
       REAL AUX(0:MXCHA+1)
       INTEGER IREF,I,NC1,NC2,NC3
       LOGICAL FRAME
*** Check reference number and scale setting.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISPLT WARNING : Histogram reference'//
     -           ' not valid; plotting empty box.'
            IF(FRAME)CALL GRCART(-1.0,-1.0,1.0,1.0,' ',' ',
     -           'Invalid histogram reference')
            RETURN
       ENDIF
       IF(.NOT.SET(IREF))THEN
            PRINT *,' !!!!!! HISPLT WARNING : The scale of this'//
     -           ' auto-range histogram is not yet set; no plot.'
            IF(FRAME)CALL GRCART(-1.0,-1.0,1.0,1.0,' ',' ',
     -           'Range not yet set')
            RETURN
       ENDIF
*** Call GRHIST.
       DO 10 I=0,MXCHA+1
       AUX(I)=CONTEN(IREF,I)
10     CONTINUE
       CALL GRHIST(AUX,NCHA(IREF),XMIN(IREF),XMAX(IREF),XTXT,TITLE,
     -      FRAME)
*** Show contents, mean and RMS.
       IF(FRAME)THEN
            IF(SX0(IREF).EQ.0)THEN
                 CALL GRCOMM(4,'Sum: 0, Mean and RMS undefined.')
            ELSE
                 CALL OUTFMT(REAL(SX0(IREF)),2,AUX1,NC1,'LEFT')
                 CALL OUTFMT(REAL(SX1(IREF)/SX0(IREF)),
     -                2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(REAL(SQRT(MAX(0.0D0,
     -                (SX2(IREF)-SX1(IREF)**2/SX0(IREF))/SX0(IREF)))),2,
     -                AUX3,NC3,'LEFT')
                 CALL GRCOMM(4,'Sum: '//AUX1(1:NC1)//', Mean: '//
     -                AUX2(1:NC2)//', RMS: '//AUX3(1:NC3))
            ENDIF
       ENDIF
       END
