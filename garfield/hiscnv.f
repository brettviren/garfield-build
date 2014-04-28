CDECK  ID>, HISCNV.
       SUBROUTINE HISCNV(IREF1,IREF2,IREF3,IFAIL)
*-----------------------------------------------------------------------
*   HISCNV - Convolutes histograms IREF1 and IREF2 to yield IREF3.
*   (Last changed on  4/ 2/96.)
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
       DOUBLE PRECISION CONTEN(MXHIST,0:MXCHA+1)
       REAL XMIN(MXHIST),XMAX(MXHIST)
       DOUBLE PRECISION SX0(MXHIST),SX1(MXHIST),SX2(MXHIST)
       INTEGER NCHA(MXHIST),NENTRY(MXHIST)
       LOGICAL SET(MXHIST),HISUSE(MXHIST),HISLIN(MXHIST)
       COMMON /HISDAT/ SX0,SX1,SX2,CONTEN,XMIN,XMAX,HISUSE,HISLIN,NCHA,
     -      NENTRY,SET
       INTEGER IREF1,IREF2,IREF3,IFAIL
       REAL BIN1,BIN2
*** Preset IREF3 to 0, i.e. non-existing.
       IREF3=0
       IFAIL=1
*** Ensure that both IREF1 and IREF2 exist and have a range.
       IF(IREF1.LE.0.OR.IREF1.GT.MXHIST.OR.
     -      IREF2.LE.0.OR.IREF2.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISCNV WARNING : Histogram reference'//
     -           ' not valid; no convolution.'
            RETURN
       ELSEIF((.NOT.SET(IREF1)).OR.(.NOT.SET(IREF2)))THEN
            PRINT *,' !!!!!! HISCNV WARNING : The scale of an'//
     -           ' input histogram is not yet set; no convolution.'
            RETURN
       ELSEIF(NCHA(IREF1).LE.0.OR.NCHA(IREF2).LE.0)THEN
            PRINT *,' !!!!!! HISCNV WARNING : An input histogram'//
     -           ' has no bins; no convolution.'
            RETURN
       ENDIF
*** Check the compatibility between the histograms.
       BIN1=(XMAX(IREF1)-XMIN(IREF1))/NCHA(IREF1)
       BIN2=(XMAX(IREF2)-XMIN(IREF2))/NCHA(IREF2)
       IF(ABS(BIN1-BIN2).GT.1E-4*(ABS(BIN1)+ABS(BIN2)))THEN
            PRINT *,' !!!!!! HISCNV WARNING : Bin size of the'//
     -           ' histograms differs, no convolution.'
            RETURN
       ENDIF
*** Obtain a new histogram.
       CALL HISADM('ALLOCATE',IREF3,NCHA(IREF1)+NCHA(IREF2)-1,
     -      XMIN(IREF1)+XMIN(IREF2)+(BIN1+BIN2)/4,
     -      XMAX(IREF1)+XMAX(IREF2)-(BIN1+BIN2)/4,
     -      .FALSE.,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISCNV WARNING : Unable to obtain an'//
     -           ' output histogram; no convolution.'
            RETURN
       ENDIF
*** Now perform the convolution.
       DO 10 I=1,NCHA(IREF3)
       CONTEN(IREF3,I)=0
       DO 20 J=1,NCHA(IREF1)
       IF(I-J+1.LT.1.OR.I-J+1.GT.NCHA(IREF2))GOTO 20
       CONTEN(IREF3,I)=CONTEN(IREF3,I)+
     -      CONTEN(IREF1,J)*CONTEN(IREF2,I-J+1)
20     CONTINUE
10     CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
