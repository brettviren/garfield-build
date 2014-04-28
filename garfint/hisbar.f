CDECK  ID>, HISBAR.
       SUBROUTINE HISBAR(IREF,NBAR,XBAR,IFAIL)
*-----------------------------------------------------------------------
*   HISBAR - Returns the barycentre.
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
       INTEGER IREF,NBAR,IFAIL
       REAL XBAR,XSUM,SUM,SUMMAX,WSUM
*** Preset output for the event of failure.
       XBAR=0.0
       IFAIL=1
*** Ensure that IREF exists and has a range.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISBAR WARNING : Histogram reference'//
     -           ' not valid; no barycentre.'
            RETURN
       ELSEIF(.NOT.SET(IREF))THEN
            PRINT *,' !!!!!! HISBAR WARNING : The scale of the'//
     -           ' input histogram is not yet set; no barycentre.'
            RETURN
       ELSEIF(NCHA(IREF).LE.0)THEN
            PRINT *,' !!!!!! HISBAR WARNING : Input histogram'//
     -           ' has no bins; no barycentre.'
            RETURN
       ENDIF
*** Also make sure the the number of bins to average over is OK.
       IF(NBAR.LE.0)THEN
            PRINT *,' !!!!!! HISBAR WARNING : Number of bins to'//
     -           ' average over < 1; no barycentre.'
            RETURN
       ENDIF
*** Locate the maximum.
       SUMMAX=-1
       DO 10 I=1,MAX(1,NCHA(IREF)-NBAR+1)
       SUM=0
       XSUM=0
       WSUM=0
       DO 20 J=I,MIN(I+NBAR-1,NCHA(IREF))
       SUM=SUM+ABS(CONTEN(IREF,J))
       XSUM=XSUM+CONTEN(IREF,J)*
     -      (XMIN(IREF)+(J-0.5)*(XMAX(IREF)-XMIN(IREF))/
     -      REAL(NCHA(IREF)))
       WSUM=WSUM+CONTEN(IREF,J)
20     CONTINUE
       IF(SUM.GT.SUMMAX.AND.WSUM.NE.0)THEN
            SUMMAX=SUM
            XBAR=XSUM/WSUM
       ENDIF
10     CONTINUE
*** Check that a maximum has been found.
       IF(SUMMAX.LE.0)THEN
            PRINT *,' !!!!!! HISBAR WARNING : No maximum has been'//
     -           ' found; no barycentre.'
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
