CDECK  ID>, HISENT.
       SUBROUTINE HISENT(IREF,X,W)
*-----------------------------------------------------------------------
*   HISENT - Routine storing entries in a histogram, taking care of the
*            range setting if requested.
*   (Last changed on 18/ 1/12.)
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
       REAL AUX(MXCHA),X,AVER,SIGMA,W,STEP
       INTEGER I,IREF,IND,NBIN,NADD1,NADD2
*** Check reference number.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            IF(LDEBUG)PRINT *,' ++++++ HISENT DEBUG   : Entry ignored'//
     -           ' because IREF=',IREF,' is not valid.'
            RETURN
       ENDIF
*** Keep track of sum of entries and their squares.
       SX0(IREF)=SX0(IREF)+W
       SX1(IREF)=SX1(IREF)+W*X
       SX2(IREF)=SX2(IREF)+W*X**2
       NENTRY(IREF)=NENTRY(IREF)+1
*** Histogram range has been set.
       IF(SET(IREF))THEN
            IND=1+INT(REAL(NCHA(IREF))*(X-XMIN(IREF))/
     -           (XMAX(IREF)-XMIN(IREF)))
            IF(X.LT.XMIN(IREF))THEN
                 IND=0
            ELSEIF(X.GE.XMAX(IREF))THEN
                 IND=NCHA(IREF)+1
            ELSEIF(IND.LT.0)THEN
                 IND=0
            ELSEIF(IND.GT.NCHA(IREF))THEN
                 IND=NCHA(IREF)+1
            ENDIF
            CONTEN(IREF,IND)=CONTEN(IREF,IND)+W
*** Histogram range has not yet been set.
       ELSE
**  Not yet enough entries to normalise.
            IF(2*NENTRY(IREF).LE.NCHA(IREF))THEN
                 CONTEN(IREF,2*NENTRY(IREF)-1)=X
                 CONTEN(IREF,2*NENTRY(IREF))=W
**  There are enough entries, but the total weight is near zero.
            ELSEIF(SX0(IREF).EQ.0)THEN
                 PRINT *,' !!!!!! HISENT WARNING : Not yet able to'//
     -                ' autoscale since the integrated weight is 0.'
                 NENTRY(IREF)=0
                 SX0(IREF)=0
                 SX1(IREF)=0
                 SX2(IREF)=0
**  Normalise.
            ELSE
*   Compute average and width.
                 AVER=REAL(SX1(IREF)/SX0(IREF))
                 SIGMA=REAL(SQRT(MAX(0.0D0,(SX2(IREF)-SX1(IREF)**2/
     -                SX0(IREF))/SX0(IREF))))
*   If width is zero, then take either mean or arbitrarily 1.
                 IF(SIGMA.LE.0)SIGMA=ABS(AVER)
                 IF(SIGMA.LE.0)SIGMA=1
*   Determine a reasonable range for the histogram.
                 XMIN(IREF)=AVER-3*SIGMA
                 XMAX(IREF)=AVER+3*SIGMA
                 IF(HISLIN(IREF))THEN
                      CALL ROUND(XMIN(IREF),XMAX(IREF),NCHA(IREF),
     -                     'LARGER,COARSER,INTEGER',STEP)
                      XMIN(IREF)=XMIN(IREF)-0.5
                      XMAX(IREF)=XMAX(IREF)-0.5
                 ELSE
                      CALL ROUND(XMIN(IREF),XMAX(IREF),NCHA(IREF),
     -                     'LARGER,COARSER',STEP)
                 ENDIF
                 IF(STEP.LE.0)STEP=1
                 NBIN=0.1+(XMAX(IREF)-XMIN(IREF))/STEP
                 NADD1=(NBIN-NCHA(IREF))/2
                 NADD2=NBIN-NCHA(IREF)-NADD1
                 XMIN(IREF)=XMIN(IREF)+NADD1*STEP
                 XMAX(IREF)=XMAX(IREF)-NADD2*STEP
*   Debugging output.
                 IF(LDEBUG)PRINT *,' ++++++ HISENT DEBUG   :'//
     -                ' Range of histogram ',IREF,' has been set.'
*   Remember the range has been set.
                 SET(IREF)=.TRUE.
*   Save the entries collected so far and reset the histogram.
                 DO 10 I=1,NCHA(IREF)
                 AUX(I)=CONTEN(IREF,I)
                 CONTEN(IREF,I)=0.0
10               CONTINUE
                 CONTEN(IREF,0)=0
                 CONTEN(IREF,NCHA(IREF)+1)=0
*   Fill the histogram.
                 DO 20 I=1,NCHA(IREF)-1,2
                 IND=1+INT(REAL(NCHA(IREF))*(AUX(I)-XMIN(IREF))/
     -                (XMAX(IREF)-XMIN(IREF)))
                 IF(IND.LT.0.OR.AUX(I).LT.XMIN(IREF))THEN
                      IND=0
                 ELSEIF(IND.GT.NCHA(IREF))THEN
                      IND=NCHA(IREF)+1
                 ENDIF
                 CONTEN(IREF,IND)=CONTEN(IREF,IND)+AUX(I+1)
20               CONTINUE
*   Add this entry.
                 IND=1+INT(REAL(NCHA(IREF))*(X-XMIN(IREF))/
     -                (XMAX(IREF)-XMIN(IREF)))
                 IF(IND.LT.0.OR.X.LT.XMIN(IREF))THEN
                      IND=0
                 ELSEIF(IND.GT.NCHA(IREF))THEN
                      IND=NCHA(IREF)+1
                 ENDIF
                 CONTEN(IREF,IND)=CONTEN(IREF,IND)+W
            ENDIF
       ENDIF
       END
