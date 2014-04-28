CDECK  ID>, HISWRT.
       SUBROUTINE HISWRT(IREF,FILE,MEMB,REM,IFAIL)
*-----------------------------------------------------------------------
*   HISWRT - This routine writes a histogram to a dataset.
*   VARIABLES :
*   (Last changed on 30/ 8/97.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXINCH) STRING
       CHARACTER*(*) FILE,MEMB,REM
       CHARACTER*29 REMARK
       CHARACTER*8 TIME,DATE,MEMBER
       LOGICAL EXMEMB
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE HISWRT ///'
*** Preset IFAIL to 1: failure.
       IFAIL=1
*** Transfer variables.
       REMARK=REM
       MEMBER=MEMB
*** Print some debugging output if requested.
       IF(LDEBUG)PRINT *,' ++++++ HISWRT DEBUG   : Ref=',IREF,
     -      ', File=',FILE,', member=',MEMBER,', Remark=',REMARK,'.'
*** Check whether the member already exists.
       CALL DSNREM(FILE,MEMB,'HIST',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ HISWRT MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! HISWRT WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*** Verify the histogram reference.
       IF(IREF.LE.0.OR.IREF.GT.MXHIST)THEN
            PRINT *,' !!!!!! HISWRT WARNING : Invalid histogram'//
     -           ' reference received; histogram not written.'
            IFAIL=1
            RETURN
       ENDIF
       IF(.NOT.HISUSE(IREF))THEN
            PRINT *,' !!!!!! HISWRT WARNING : Histogram to be'//
     -           ' written does not exist; histogram not written.'
            IFAIL=1
            RETURN
       ENDIF
*** Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,LEN(FILE),12,'WRITE-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISWRT WARNING : Opening ',FILE,
     -           ' failed ; histogram will not be written.'
            IFAIL=1
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Histogram ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ HISWRT DEBUG   : Dataset ',
     -      FILE,' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,'' HIST    '',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) STRING
*   Write the histogram.
       WRITE(12,'('' HISTOGRAM INFORMATION:'')',IOSTAT=IOS,ERR=2010)
       WRITE(12,'('' Minimum:   '',E15.8/'' Maximum:   '',E15.8/
     -      '' Bins:      '',I10/'' Range set: '',L1/
     -      '' Integer:   '',L1)',IOSTAT=IOS,ERR=2010) XMIN(IREF),
     -      XMAX(IREF),NCHA(IREF),SET(IREF),HISLIN(IREF)
       WRITE(12,'('' Sums:      '',3E15.8/'' Entries:   '',I10)',
     -      IOSTAT=IOS,ERR=2010) SX0(IREF),SX1(IREF),SX2(IREF),
     -      NENTRY(IREF)
       WRITE(12,'('' CONTENTS'')',IOSTAT=IOS,ERR=2010)
       DO 210 I=0,NCHA(IREF)+1
       WRITE(12,'(I10,E15.8)',IOSTAT=IOS,ERR=2010) I,CONTEN(IREF,I)
210    CONTINUE
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Writing an histogram to a dataset:      ')
*** Things worked, reset error flag.
       IFAIL=0
       RETURN
*** Handle the error conditions.
2010   CONTINUE
       PRINT *,' ###### HISWRT ERROR   : Error while writing'//
     -      ' to ',FILE,' via unit 12 ; histogram not written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### HISWRT ERROR   : Dataset ',FILE,
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
