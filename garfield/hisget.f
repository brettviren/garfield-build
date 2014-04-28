CDECK  ID>, HISGET.
       SUBROUTINE HISGET(IREF,FILE,MEMB,IFAIL)
*-----------------------------------------------------------------------
*   HISGET - This routine reads an histogram from a file.
*   VARIABLES : STRING      : Character string that should contain a
*                             description of the dataset being read.
*   (Last changed on 20/ 3/97.)
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
       CHARACTER*(*) FILE,MEMB
       CHARACTER*8 MEMBER
       CHARACTER*1 DUMMY
       LOGICAL DSNCMP,EXIS
       EXTERNAL DSNCMP
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE HISGET ///'
*** Initialise IFAIL on 1 (i.e. fail).
       IFAIL=1
*** Transfer variables.
       MEMBER=MEMB
*** Initialise IREF so that HISADM always gets a valid argument.
       IREF=-1
*** Open the dataset and inform DSNLOG.
       CALL DSNOPN(FILE,LEN(FILE),12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISGET WARNING : Opening ',FILE,
     -           ' failed ; histogram not read.'
            IFAIL=1
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Histogram ','Sequential','Read only ')
       IF(LDEBUG)PRINT *,' ++++++ HISGET DEBUG   : Dataset ',
     -      FILE,' opened on unit 12 for seq read.'
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,LEN(MEMBER),'HIST    ',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,LEN(MEMBER),'HIST    ',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### HISGET ERROR   : Histogram ',MEMBER,
     -                ' has been deleted from ',FILE,'; not read.'
            ELSE
                 PRINT *,' ###### HISGET ERROR   : Histogram ',MEMBER,
     -                ' not found on ',FILE,'.'
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            IFAIL=1
            RETURN
       ENDIF
*** Check that the member is acceptable.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(DSNCMP('20- 3-97',STRING(11:18)))THEN
            PRINT *,' !!!!!! HISGET WARNING : Member ',STRING(32:39),
     -           ' can not be read because of a change in format.'
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            IFAIL=1
            RETURN
       ENDIF
       WRITE(LUNOUT,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      STRING(32:39),STRING(11:18),STRING(23:30),STRING(51:79)
*** Find a free histogram.
       CALL HISADM('ALLOCATE',IREF,1,0.0,0.0,.TRUE.,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! HISGET WARNING : Unable to obtain space'//
     -           ' to store the histogram to be read; not read.'
            IFAIL=1
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*** Execute read operations if a valid name is available.
       READ(12,'(/12X,E15.8/12X,E15.8/12X,I10/12X,L1/12X,L1/
     -      12X,3E15.8/12X,I10)',IOSTAT=IOS,ERR=2010,END=2000)
     -      XMIN(IREF),XMAX(IREF),NCHA(IREF),SET(IREF),HISLIN(IREF),
     -      SX0(IREF),SX1(IREF),SX2(IREF),NENTRY(IREF)
       READ(12,'(A1)',IOSTAT=IOS,ERR=2010,END=2000) DUMMY
       DO 210 I=0,NCHA(IREF)+1
       READ(12,'(10X,E15.8)',IOSTAT=IOS,ERR=2010,END=2000)
     -      CONTEN(IREF,I)
210    CONTINUE
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
*** Register the amount of CPU time used for reading.
       CALL TIMLOG('Reading an histogram from a dataset:    ')
*** Things worked, reset the error flag.
       IFAIL=0
       RETURN
*** Handle the I/O error conditions.
2000   CONTINUE
       PRINT *,' ###### HISGET ERROR   : EOF encountered while',
     -      ' reading ',FILE,' from unit 12 ; no histogram read.'
       CALL INPIOS(IOS)
       IF(IREF.NE.-1)CALL HISADM('DELETE',IREF,1,0.0,0.0,.TRUE.,IFAIL1)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### HISGET ERROR   : Error while reading ',
     -      FILE,' from unit 12 ; no histogram read.'
       CALL INPIOS(IOS)
       IF(IREF.NE.-1)CALL HISADM('DELETE',IREF,1,0.0,0.0,.TRUE.,IFAIL1)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### HISGET ERROR   : Dataset ',FILE,
     -      ' on unit 12 cannot be closed ; results not predictable.'
       CALL INPIOS(IOS)
       END
