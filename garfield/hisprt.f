CDECK  ID>, HISPRT.
       SUBROUTINE HISPRT(IREF,XTXT,TITLE)
*-----------------------------------------------------------------------
*   HISPRT - Prints a histogram.
*   (Last changed on 19/11/10.)
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
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       CHARACTER*80 LINE
       CHARACTER*15 AUX1,AUX2
       CHARACTER*(*) XTXT,TITLE
       INTEGER IREF,I,IND,NCAUX1,NCAUX2
       REAL DIV,HISMIN,HISMAX,SUM0,SUM1,SUM2
*** Check reference number.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISPRT WARNING : Histogram reference'//
     -           ' not valid; no histogram printed.'
            RETURN
       ENDIF
*** No entries yet.
       IF(NENTRY(IREF).EQ.0.OR.SX0(IREF).EQ.0)THEN
            WRITE(LUNOUT,'(''1''/''  Title: '',A/''  Axis:  '',A//,
     -           ''  No entries sofar (with weight > 0).'')')
            RETURN
*** Straight dump for auto range histogram without set range.
       ELSEIF(.NOT.SET(IREF))THEN
            WRITE(LUNOUT,'(''1''/''  Title: '',A/''  Axis:  '',A//,
     -           ''  This is an auto-range histogram for which the'',
     -           '' range has not yet been set.''//
     -           ''  Entry            Value'')')
     -           TITLE,XTXT
            SUM0=0.0
            SUM1=0.0
            SUM2=0.0
            DO 10 I=1,NENTRY(IREF)
            CALL OUTFMT(REAL(CONTEN(IREF,I)),2,AUX1,NCAUX1,'RIGHT')
            WRITE(LUNOUT,'(2X,I5,2X,A15)') I,AUX1
            SUM1=SUM1+CONTEN(IREF,I)
10          CONTINUE
       ELSE
*** Determine maximum and minimum.
            HISMIN=REAL(CONTEN(IREF,1))
            HISMAX=REAL(CONTEN(IREF,1))
            DO 20 I=2,NCHA(IREF)
            HISMIN=MIN(HISMIN,REAL(CONTEN(IREF,I)))
            HISMAX=MAX(HISMAX,REAL(CONTEN(IREF,I)))
20          CONTINUE
*** Set the scale of the printing axes.
            IF(HISMAX.LE.HISMIN)THEN
                 DIV=0.0
            ELSE
                 DIV=LEN(LINE)/(HISMAX-HISMIN)
            ENDIF
*** Print the header for the histogram.
            WRITE(LUNOUT,'(''1''/''  Title:     '',A/
     -           ''  Axis:      '',A/''  Reference: '',I4//
     -           ''  Bin       Bin centre         Contents'',
     -           ''  Histogram''/)') TITLE,XTXT,IREF
*** Print the histogram.
            SUM0=CONTEN(IREF,0)
            SUM1=0.0
            SUM2=CONTEN(IREF,NCHA(IREF)+1)
            DO 30 I=1,NCHA(IREF)
            LINE='*****************************************'//
     -           '*****************************************'
            IND=NINT(DIV*(CONTEN(IREF,I)-HISMIN))
            IND=MIN(LEN(LINE),MAX(1,IND))
            LINE(IND:)=' '
            CALL OUTFMT(XMIN(IREF)+(I-0.5)*(XMAX(IREF)-XMIN(IREF))/
     -           REAL(NCHA(IREF)),2,AUX1,NCAUX1,'RIGHT')
            CALL OUTFMT(REAL(CONTEN(IREF,I)),2,AUX2,NCAUX2,'RIGHT')
            WRITE(LUNOUT,'(2X,I3,2X,A15,2X,A15,2X,A)')
     -           I,AUX1,AUX2,LINE(1:MAX(1,IND-1))
            SUM1=SUM1+CONTEN(IREF,I)
30          CONTINUE
            SUM0=SUM0*(XMAX(IREF)-XMIN(IREF))/REAL(NCHA(IREF))
            SUM1=SUM1*(XMAX(IREF)-XMIN(IREF))/REAL(NCHA(IREF))
            SUM2=SUM2*(XMAX(IREF)-XMIN(IREF))/REAL(NCHA(IREF))
       ENDIF
*** Print histogram statistics.
       WRITE(LUNOUT,'(/''  STATISTICS:''//
     -      ''  Entries   : '',I8,'' (including under and overflow),''//
     -      ''  Underflow : '',E15.8,
     -      '' (coordinates below '',E15.8,''),''/
     -      ''  Contents  : '',E15.8/
     -      ''  Overflow  : '',E15.8,
     -      '' (coordinates above '',E15.8,''),''//
     -      ''  Average   : '',E15.8/
     -      ''  RMS       : '',E15.8/)')
     -      NENTRY(IREF),SUM0,XMIN(IREF),SUM1,SUM2,XMAX(IREF),
     -      SX1(IREF)/SX0(IREF),
     -      SQRT((SX2(IREF)-SX1(IREF)**2/SX0(IREF))/SX0(IREF))
       END
