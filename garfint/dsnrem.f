CDECK  ID>, DSNREM.
       SUBROUTINE DSNREM(FILE,MEMBER,TYPE,EXMEMB)
*-----------------------------------------------------------------------
*   DSNREM - Checks whether a member already exists when writing a new
*            one and marks the old member for deletion if required.
*   VARIABLES : FILE        : File name
*               MEMBER      : Member name
*               TYPE        : Member type
*   (Last changed on 30/ 8/97.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) FILE,MEMBER,TYPE
       CHARACTER*133 LINE
       INTEGER NCFILE,NCMEMB,NCTYPE,IFAIL,IOS
       LOGICAL EXIST,MATMEM,MATTYP,EXMEMB
*** Assume that the member does not exist.
       EXMEMB=.FALSE.
*** Establish the lengths of the various strings.
       NCFILE=LEN(FILE)
       NCMEMB=LEN(MEMBER)
       NCTYPE=LEN(TYPE)
*** See whether the file exists.
       CALL DSNINQ(FILE,NCFILE,EXIST)
*   If the file doesn't exist, don't do anything else.
       IF(.NOT.EXIST)RETURN
*** Open the file.
       CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DSNREM WARNING : Unable to open ',
     -           FILE(1:NCFILE),'; not checked for existing members.'
            RETURN
       ENDIF
*** Open a temporary file if "delete old copy" has been selected.
       IF(JEXMEM.EQ.1)THEN
            OPEN(UNIT=9,STATUS='SCRATCH',IOSTAT=IOS,ERR=2020)
       ENDIF
*** Read through the dataset and mark, then copy to scratch.
       READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=110) LINE
100    CONTINUE
       READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=110) LINE
       IF(LINE(1:1).EQ.'%')THEN
            CALL WLDCRD(LINE(32:39),MEMBER(1:NCMEMB),.FALSE.,
     -           MATMEM)
            CALL WLDCRD(LINE(41:48),TYPE(1:NCTYPE),.FALSE.,
     -           MATTYP)
       ELSE
            MATMEM=.FALSE.
            MATTYP=.FALSE.
       ENDIF
       IF(LINE(1:1).EQ.'%'.AND.LINE(2:2).NE.'X'.AND.
     -      MATMEM.AND.MATTYP.AND.JEXMEM.EQ.1)THEN
            LINE(2:2)='X'
            EXMEMB=.TRUE.
            PRINT *,' Member ',MEMBER(1:NCMEMB),' written on '//
     -           LINE(11:18)//' at '//LINE(23:30)//' has been'//
     -           ' marked for deletion.'
       ELSEIF(LINE(1:1).EQ.'%'.AND.LINE(2:2).NE.'X'.AND.
     -      MATMEM.AND.MATTYP.AND.(JEXMEM.EQ.2.OR.JEXMEM.EQ.3))THEN
            EXMEMB=.TRUE.
            PRINT *,' !!!!!! DSNREM WARNING : A member called ',
     -           MEMBER(1:NCMEMB),' was already written on '//
     -           LINE(11:18)//' at '//LINE(23:30)//'.'
       ENDIF
       IF(JEXMEM.EQ.1)WRITE(9,'(A133)',IOSTAT=IOS,ERR=2015) LINE
       GOTO 100
110    CONTINUE
*** Copy the file from unit 9 to unit 12, after deleting old copy.
       IF(JEXMEM.EQ.1)THEN
            CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
*   Create a new file with the same name.
            CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' ###### DSNREM ERROR   : Unable to'//
     -                ' create the file again ; dataset lost.'
                 CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
                 CALL DSNLOG(FILE,'Cleanup   ','Sequential',
     -                'File lost')
                 CALL DSNLOG('< intermediate file for copying >',
     -                'Cleanup   ','Sequential','Read/Write')
                 RETURN
            ENDIF
*   And copy the whole file back to the original file.
            REWIND(UNIT=9,IOSTAT=IOS,ERR=2055)
120         CONTINUE
            READ(9,'(A133)',IOSTAT=IOS,ERR=2015,END=130) LINE
            WRITE(12,'(A133)',IOSTAT=IOS,ERR=2010) LINE
            GOTO 120
130         CONTINUE
*   Close the main file.
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            CALL DSNLOG(FILE,'Cleanup   ','Sequential',
     -           'Read/Write')
*   Close the scratch file and log its use.
            CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
            CALL DSNLOG('< intermediate file for copying >',
     -           'Cleanup   ','Sequential','Read/Write')
*** Or simply close the file.
       ELSE
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            CALL DSNLOG(FILE,'Check     ','Sequential',
     -           'Read/Write')
       ENDIF
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' !!!!!! DSNREM WARNING : Read/write error on ',
     -      FILE(1:NCFILE),'; no check for existing members.'
       CALL INPIOS(IOS)
       CLOSE(12,IOSTAT=IOS,ERR=2030)
       RETURN
2015   CONTINUE
       PRINT *,' !!!!!! DSNREM WARNING : Read/write error on a'//
     -      ' temporary file ; no check for existing members.'
       CALL INPIOS(IOS)
       CLOSE(9,IOSTAT=IOS,ERR=2035)
       RETURN
2020   CONTINUE
       PRINT *,' !!!!!! DSNREM WARNING : Error opening a temporary'//
     -      ' file for copying; no check for existing members.'
       CALL INPIOS(IOS)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! DSNREM WARNING : File closing error on ',
     -      FILE(1:NCFILE),'; no check for existing members.'
       CALL INPIOS(IOS)
       RETURN
2035   CONTINUE
       PRINT *,' !!!!!! DSNREM WARNING : File closing error on a',
     -      ' temporary file; no check for existing members.'
       CALL INPIOS(IOS)
       RETURN
2055   CONTINUE
       PRINT *,' !!!!!! DSNREM WARNING : Rewind error on a',
     -      ' temporary file; no check for existing members.'
       CLOSE(9,IOSTAT=IOS,ERR=2035)
       CALL INPIOS(IOS)
       RETURN
       END
