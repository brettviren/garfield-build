CDECK  ID>, HISRZO.
       SUBROUTINE HISRZO(IREF,FILE,TITLE,IFAIL)
*-----------------------------------------------------------------------
*   HISRZO - Writes an histogram to an RZ file.
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
       LOGICAL EXIST
       INTEGER LREC,ISTAT,IFAIL,IREF,ICYCLE,I
       REAL AUX(MXCHA)
       CHARACTER*(*) FILE,TITLE
       CHARACTER*10 CHOPT
*** Assume the call will work.
       IFAIL=0
*** Check reference number and scale setting.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISRZO WARNING : Histogram reference'//
     -           ' not valid; histogram not written.'
            IFAIL=1
            RETURN
       ENDIF
       IF(.NOT.SET(IREF))THEN
            PRINT *,' !!!!!! HISRZO WARNING : The scale of this'//
     -           ' auto-range histogram is not yet set; not written.'
            IFAIL=1
            RETURN
       ENDIF
*** Book the histogram.
       CALL HBOOK1(IREF,TITLE,NCHA(IREF),XMIN(IREF),XMAX(IREF),0.0)
*** Copy the histogram to HBOOK.
       DO 10 I=1,NCHA(IREF)
       AUX(I)=CONTEN(IREF,I)
10     CONTINUE
       CALL HPAK(IREF,AUX)
*** Open the RZ file.
       INQUIRE(FILE=FILE,EXIST=EXIST)
       IF(EXIST)THEN
            CHOPT='U'
       ELSE
            CHOPT='N'
       ENDIF
       LREC=1024
       CALL HROPEN(12,'Garfield',FILE,CHOPT,LREC,ISTAT)
       IF(ISTAT.NE.0)THEN
            PRINT *,' !!!!!! HISRZO WARNING : Error while opening'//
     -           ' the RZ file.'
            IFAIL=1
            RETURN
       ENDIF
*** Write the histogram.
       ICYCLE=0
       CALL HROUT(IREF,ICYCLE,' ')
       PRINT *,' ------ HISRZO MESSAGE : Histogram written to ',FILE,
     -      ' with identifier ',IREF,', cycle ',ICYCLE,'.'
*** Close the file.
       CALL HREND('Garfield')
       CLOSE(UNIT=12,STATUS='KEEP')
*** Delete the histogram from memory.
       CALL HDELET(IREF)
       END
