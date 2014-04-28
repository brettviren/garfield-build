CDECK  ID>, DSNINP.
       SUBROUTINE DSNINP
*-----------------------------------------------------------------------
*   DSNINP - Handles dataset information requests like INDEX, LIST,
*            DELETE etc.
*   VARIABLES : STRING      : Used for various character manipulations.
*               FILE, MEMBER: Obvious.
*               EXFILE, EXMEMB: Indicate whether file resp memb exist.
*               LOOP        : .TRUE. if one should remain in here.
*   (Last changed on  5/ 6/08.)
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
       CHARACTER*133 LINE
       CHARACTER*20 AUX1,AUX2
       CHARACTER*(MXCHAR) STRING,FILE
       CHARACTER*8 MEMBER,DELETE,TYPE
       CHARACTER CHAR
       LOGICAL EXMEMB,LOOP,LIST,MATMEM,MATTYP
       INTEGER NWORD,NC,IFAIL,IKEY,NCFILE,NCMEMB,NCTYPE,NMEMB,NMALL,
     -      NPURGE,I,IOS,NC1,NC2,INPCMP,STRLEN
       EXTERNAL INPCMP,STRLEN
*** Identify the subroutine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DSNINP ///'
*** First pick up the number of words and the first word.
       CALL INPNUM(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Check it is a dataset command.
       IF(STRING(1:1).NE.'%')RETURN
*** Determine whether it is a single command or not.
       IF(NWORD.EQ.1.AND.NC.EQ.1)THEN
            LOOP=.TRUE.
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------     Dataset subsection     ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
            CALL INPPRM('Dataset','ADD-PRINT')
       ELSE
            LOOP=.FALSE.
       ENDIF
*** Return here if LOOP is .TRUE.
1000   CONTINUE
       IF(LOOP)THEN
            CALL INPGET
            CALL INPNUM(NWORD)
       ENDIF
       CALL INPSTR(1,1,STRING,NC)
*** Skip blank lines and warn for section headers.
       IF(STRING(1:1).EQ.'&')THEN
            PRINT *,' !!!!!! DSNINP WARNING : The section cannot be'//
     -           ' left at this point; first type EXIT.'
            GOTO 1000
       ELSEIF(INDEX('$!?><@',STRING(1:1)).NE.0)THEN
            PRINT *,' !!!!!! DSNINP WARNING : This command cannot be'//
     -           ' executed at the present level; first type EXIT.'
            GOTO 1000
       ELSEIF(STRING(1:1).EQ.'*')THEN
            GOTO 1000
       ENDIF
       IF(LOOP.AND.(NWORD.EQ.0.OR.(NWORD.EQ.1.AND.NC.EQ.1.AND.
     -      STRING(1:1).EQ.'%')))GOTO 1000
       IF(.NOT.LOOP.AND.NC.EQ.1.AND.NWORD.EQ.1)RETURN
*** Prepare a help file if the command is PACK-HELP-FILE.
       IF(INPCMP(1,'%PAC#K-H#ELP-#FILE')+INPCMP(2,'PAC#K-H#ELP-#FILE')
     -      .NE.0)THEN
            PRINT *,' !!!!!! DSNINP WARNING : The help section has'//
     -           ' not been compiled; command ignored.'
            GOTO 1020
       ENDIF
*** Dump the help file if the command is DUMP-HELP-FILE.
       IF(INPCMP(1,'%DUMP-H#ELP-#FILE')+INPCMP(2,'DUMP-H#ELP-#FILE')
     -      .NE.0)THEN
            PRINT *,' !!!!!! DSNINP WARNING : The help section has'//
     -           ' not been compiled; command ignored.'
            GOTO 1020
       ENDIF
*** Set the position of the command.
       IF(NC.EQ.1.AND.STRING(1:1).EQ.'%')THEN
            IKEY=2
       ELSE
            IKEY=1
       ENDIF
*** Find the dataset and the member name.
       FILE=' '
       MEMBER=' '
*   Start with the dataset name, check it has been specified.
       IF(INPCMP(IKEY,'EX#IT')+INPCMP(IKEY,'%EX#IT').NE.0)THEN
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------   Dataset subsection end   ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
            CALL INPPRM(' ','BACK-PRINT')
            RETURN
       ELSEIF(IKEY+1.LE.NWORD)THEN
            CALL INPSTR(IKEY+1,IKEY+1,STRING,NC)
            FILE=STRING
            NCFILE=NC
       ELSE
            PRINT *,' !!!!!! DSNINP WARNING : All dataset commands'//
     -           ' have a dataset name as first argument; ignored.'
            GOTO 1020
       ENDIF
*   Return immediately if the file does not exist or is corrupt.
       CALL DSNOPN(FILE,NCFILE,12,'RW-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DSNINP WARNING : '//FILE(1:NCFILE)//
     -           ' could not be opened; no action.'
            GOTO 1020
       ENDIF
*   Next the member name, no checks except for length.
       IF(NWORD.GE.IKEY+2)THEN
            CALL INPSTR(IKEY+2,IKEY+2,STRING,NCMEMB)
            MEMBER=STRING
            IF(NCMEMB.GT.LEN(MEMBER))THEN
                 PRINT *,' !!!!!! DSNINP WARNING : The member name '//
     -                STRING(1:NCMEMB)//' is too long; truncated.'
                 NCMEMB=LEN(MEMBER)
            ENDIF
       ELSE
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Finally the TYPE argument.
       IF(NWORD.GE.IKEY+3)THEN
            CALL INPSTR(IKEY+3,IKEY+3,STRING,NCTYPE)
            TYPE=STRING(1:NCTYPE)
       ELSE
            TYPE='*'
            NCTYPE=1
       ENDIF
*** Identify the instruction, start with DELETE.
       IF(INPCMP(IKEY,'%DEL#ETE')+INPCMP(IKEY,'DEL#ETE')+
     -      INPCMP(IKEY,'%SCR#ATCH')+INPCMP(IKEY,'SCR#ATCH').NE.0)THEN
            IF(IKEY+2.GT.NWORD)THEN
                  PRINT *,' !!!!!! DSNINP WARNING : A member must be'//
     -                 ' specified on a DELETE command.'
                  GOTO 1010
            ENDIF
*   Read through the dataset and mark, then copy to scratch.
            EXMEMB=.FALSE.
            OPEN(UNIT=9,STATUS='SCRATCH',IOSTAT=IOS,ERR=2025)
            READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=110) LINE
100         CONTINUE
            READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=110) LINE
            IF(LINE(1:1).EQ.'%')THEN
                 CALL WLDCRD(LINE(32:39),MEMBER(1:NCMEMB),.FALSE.,
     -                MATMEM)
                 CALL WLDCRD(LINE(41:48),TYPE(1:NCTYPE),.FALSE.,
     -                MATTYP)
            ELSE
                 MATMEM=.FALSE.
                 MATTYP=.FALSE.
            ENDIF
            IF(LINE(1:1).EQ.'%'.AND.LINE(2:2).NE.'X'.AND.
     -           MATMEM.AND.MATTYP)THEN
                 EXMEMB=.TRUE.
                 LINE(2:2)='X'
                 PRINT *,' Member '//MEMBER(1:NCMEMB)//' of type '//
     -                LINE(41:48)//' marked for deletion.'
            ENDIF
            WRITE(9,'(A133)',IOSTAT=IOS,ERR=2015) LINE
            GOTO 100
110         CONTINUE
*   Print an error message if the member has not been found.
            IF(.NOT.EXMEMB)THEN
                 PRINT *,' !!!!!! DSNINP WARNING : '//MEMBER(1:NCMEMB)//
     -                ' does not exist or has already been deleted.'
                 CALL DSNLOG(FILE,'% Search  ','Sequential',
     -                'Read only ')
            ELSE
*   Close the file on unit 12, deleting it at the same time.
                 CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
*   Create a new file with the same name.
                 CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
                 IF(IFAIL.EQ.1)THEN
                      PRINT *,' ###### DSNINP ERROR   : Unable to'//
     -                     ' create the file again ; dataset lost.'
                      CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
                      CALL DSNLOG(FILE,'% Delete  ','Sequential',
     -                     'Deleted !!')
                      CALL DSNLOG('< intermediate file for copying >',
     -                     'Dataset   ','Sequential','Read/Write')
                      GOTO 1020
                 ENDIF
*   And copy the whole file back to the original file.
                 REWIND(UNIT=9,IOSTAT=IOS,ERR=2055)
120              CONTINUE
                 READ(9,'(A133)',IOSTAT=IOS,ERR=2015,END=130) LINE
                 WRITE(12,'(A)',IOSTAT=IOS,ERR=2010)
     -                LINE(1:STRLEN(LINE))
                 GOTO 120
130              CONTINUE
                 CALL DSNLOG(FILE,'% Delete  ','Sequential',
     -                'Read/Write')
            ENDIF
*   Close the scratch file and log its use.
            CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
            CALL DSNLOG('< intermediate file for copying >',
     -           'Dataset   ','Sequential','Read/Write')
**  Look for the keyword DIRECTORY.
       ELSEIF(INPCMP(IKEY,'%DIR#ECTORY')+INPCMP(IKEY,'DIR#ECTORY')+
     -      INPCMP(IKEY,'%IND#EX')+INPCMP(IKEY,'IND#EX').NE.0)THEN
*   Print a heading for the table.
            WRITE(LUNOUT,'(/''  Index for '',A,//,''  Member   '',
     -           ''Type     Date     Time     Deleted  Remarks''/)')
     -           FILE(1:NCFILE)
*   Read it record by record, printing if it's a header.
            NMEMB=0
            NMALL=0
10          CONTINUE
            READ(12,'(A1)',END=20,IOSTAT=IOS,ERR=2010) CHAR
            IF(CHAR.EQ.'%')THEN
                 NMALL=NMALL+1
                 BACKSPACE(UNIT=12,IOSTAT=IOS,ERR=2040)
                 READ(12,'(A80)',END=20,IOSTAT=IOS,ERR=2010) STRING
                 CALL WLDCRD(STRING(32:39),MEMBER(1:NCMEMB),.FALSE.,
     -                MATMEM)
                 CALL WLDCRD(STRING(41:48),TYPE(1:NCTYPE),.FALSE.,
     -                MATTYP)
                 IF(.NOT.(MATMEM.AND.MATTYP))GOTO 10
                 NMEMB=NMEMB+1
                 IF(STRING(2:2).EQ.'X')THEN
                      DELETE='Yes     '
                 ELSE
                      DELETE='No      '
                 ENDIF
                 WRITE(LUNOUT,'(1X,5(1X,A8),1X,A29)') STRING(32:39),
     -                STRING(41:48),STRING(11:18),STRING(23:30),DELETE,
     -                STRING(51:79)
            ENDIF
            GOTO 10
*   Finished, close the unit, log access and print number of members.
20          CONTINUE
            CALL OUTFMT(REAL(NMALL),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(NMEMB),2,AUX2,NC2,'LEFT')
            WRITE(LUNOUT,'(/''  Out of the '',A,'' members in the'',
     -           '' file, '',A,'' match.'')') AUX1(1:NC1),AUX2(1:NC2)
            CALL DSNLOG(FILE,'% Index   ','Sequential','Read only ')
**  Look for the keyword LIST.
       ELSEIF(INPCMP(IKEY,'%L#IST')+INPCMP(IKEY,'L#IST')+
     -      INPCMP(IKEY,'%T#YPE')+INPCMP(IKEY,'T#YPE').NE.0)THEN
*   Read through the dataset, listing if LIST is on.
            EXMEMB=.FALSE.
            LIST=.FALSE.
200         CONTINUE
            READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=210) LINE
            IF(LIST)THEN
                 IF(LINE(1:1).EQ.'%')GOTO 230
                 DO 220 I=133,1,-1
                 IF(LINE(I:I).NE.' ')THEN
                      WRITE(LUNOUT,'(1X,A)') LINE(1:I)
                      GOTO 230
                 ENDIF
220              CONTINUE
                 WRITE(LUNOUT,'('' '')')
230              CONTINUE
            ENDIF
*   Switch LIST on and off depending on the header records.
            IF(LINE(1:1).EQ.'%')THEN
                 CALL WLDCRD(LINE(32:39),MEMBER(1:NCMEMB),.FALSE.,
     -                MATMEM)
                 CALL WLDCRD(LINE(41:48),TYPE(1:NCTYPE),.FALSE.,
     -                MATTYP)
            ELSE
                 MATMEM=.FALSE.
                 MATTYP=.FALSE.
            ENDIF
            IF(LINE(1:2).EQ.'% '.AND.MATMEM.AND.MATTYP)THEN
                 EXMEMB=.TRUE.
                 LIST=.TRUE.
                 WRITE(LUNOUT,'(''  Listing of member '',A8,
     -                '' of type '',A8,'', created on '',A8,
     -                '' at '',A8)') LINE(32:39),LINE(41:48),
     -                LINE(11:18),LINE(23:30)
                 IF(LINE(51:79).NE.' ')WRITE(LUNOUT,'(''  Remarks: '',
     -                A29)') LINE(51:79)
                 WRITE(LUNOUT,'('' '')')
            ELSEIF(LINE(1:1).EQ.'%'.AND..NOT.MATMEM)THEN
                 LIST=.FALSE.
            ENDIF
            GOTO 200
210         CONTINUE
*   Print an error message if the member has not been found.
            IF(.NOT.EXMEMB)THEN
                 PRINT *,' !!!!!! DSNINP WARNING : '//MEMBER(1:NCMEMB)//
     -                ' either does not exist or has been deleted.'
                 CALL DSNLOG(FILE,'% Search  ','Sequential',
     -                'Read only ')
            ELSE
                 CALL DSNLOG(FILE,'% List    ','Sequential',
     -                'Read only ')
            ENDIF
**  Look for the keyword PURGE.
       ELSEIF(INPCMP(IKEY,'%PUR#GE')+INPCMP(IKEY,'PUR#GE')+INPCMP
     -      (IKEY,'%COND#ENSE')+INPCMP(IKEY,'COND#ENSE').NE.0)THEN
            IF(NWORD.GT.IKEY+1)THEN
                  PRINT *,' !!!!!! DSNINP WARNING : No member must be'//
     -                 ' specified on a PURGE command; do not mix up'
                  PRINT *,'                         with DELETE, this'//
     -                 ' statement hurts ! (not executed).'
                  GOTO 1010
            ENDIF
*   Read through the dataset copying the non-marked members.
            OPEN(UNIT=9,STATUS='SCRATCH',IOSTAT=IOS,ERR=2025)
            LIST=.TRUE.
            NPURGE=0
            READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=410) LINE
400         CONTINUE
            READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=410) LINE
            IF(LINE(1:1).EQ.'%'.AND.LINE(2:2).EQ.'X')THEN
                 LIST=.FALSE.
                 NPURGE=NPURGE+1
                 PRINT *,' Removing member '//LINE(32:39)//' (type '//
     -                LINE(41:48)//'),'
                 PRINT *,' created on '//LINE(11:18)//' at '//
     -                LINE(23:30)//', remarks: '//LINE(51:79)
                 PRINT *,' '
            ELSEIF(LINE(1:1).EQ.'%')THEN
                 LIST=.TRUE.
            ENDIF
            IF(LIST)WRITE(9,'(A133)',IOSTAT=IOS,ERR=2015) LINE
            GOTO 400
410         CONTINUE
            IF(NPURGE.GT.0)THEN
                 PRINT *,' A total of ',NPURGE,' members were removed.'
            ELSE
                 PRINT *,' No members were marked for deletion.'
            ENDIF
*   Close the file on unit 12, deleting it at the same time.
            CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
*   Create a new file with the same name.
            CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' ###### DSNINP ERROR   : Unable to'//
     -                ' create the file again ; dataset lost.'
                 CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
                 CALL DSNLOG(FILE,'% Purge   ','Sequential',
     -                'Deleted !!')
                 CALL DSNLOG('< intermediate file for copying >',
     -                'Dataset   ','Sequential','Read/Write')
                 GOTO 1020
            ENDIF
*   And copy the whole file back to the original file.
            REWIND(UNIT=9,IOSTAT=IOS,ERR=2055)
420         CONTINUE
            READ(9,'(A133)',IOSTAT=IOS,ERR=2015,END=430) LINE
            WRITE(12,'(A)',IOSTAT=IOS,ERR=2010) LINE(1:STRLEN(LINE))
            GOTO 420
430         CONTINUE
*   Close the scratch file and log its use.
            CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
            CALL DSNLOG('< intermediate file for copying >',
     -           'Dataset   ','Sequential','Read/Write')
            CALL DSNLOG(FILE,'% Purge   ','Sequential',
     -           'Read/Write')
**  Look for the keyword RECOVER.
       ELSEIF(INPCMP(IKEY,'%REC#OVER')+INPCMP(IKEY,'REC#OVER')+
     -      INPCMP(IKEY,'%RES#CUE')+INPCMP(IKEY,'RES#CUE').NE.0)THEN
            IF(IKEY+2.GT.NWORD)THEN
                  PRINT *,' !!!!!! DSNINP WARNING : A member must be'//
     -                 ' specified on a RECOVER command.'
                  GOTO 1010
            ENDIF
*   Read through the dataset and mark, then copy to scratch.
            EXMEMB=.FALSE.
            OPEN(UNIT=9,STATUS='SCRATCH',IOSTAT=IOS,ERR=2025)
300         CONTINUE
            READ(12,'(A133)',IOSTAT=IOS,ERR=2010,END=310) LINE
            IF(LINE(1:2).EQ.'%X')THEN
                 CALL WLDCRD(LINE(32:39),MEMBER(1:NCMEMB),.FALSE.,
     -                MATMEM)
                 CALL WLDCRD(LINE(41:48),TYPE(1:NCTYPE),.FALSE.,
     -                MATTYP)
            ELSE
                 MATMEM=.FALSE.
                 MATTYP=.FALSE.
            ENDIF
            IF(LINE(1:2).EQ.'%X'.AND.MATMEM.AND.MATTYP)THEN
                 EXMEMB=.TRUE.
                 LINE(2:2)=' '
                 PRINT *,' Member '//MEMBER(1:NCMEMB)//' of type '//
     -                LINE(41:48)//' recovered.'
            ENDIF
            WRITE(9,'(A133)',IOSTAT=IOS,ERR=2015) LINE
            GOTO 300
310         CONTINUE
*   Print an error message if the member has not been found.
            IF(.NOT.EXMEMB)THEN
                 PRINT *,' !!!!!! DSNINP WARNING : '//MEMBER(1:NCMEMB)//
     -                ' does not exist or has already been recovered.'
                 CALL DSNLOG(FILE,'% Search  ','Sequential',
     -                'Read only ')
            ELSE
*   Close the file on unit 12, deleting it at the same time.
                 CLOSE(UNIT=12,STATUS='DELETE',IOSTAT=IOS,ERR=2030)
*   Create a new file with the same name.
                 CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
                 IF(IFAIL.EQ.1)THEN
                      PRINT *,' ###### DSNINP ERROR   : Unable to'//
     -                     ' create the file again ; dataset lost.'
                      CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
                      CALL DSNLOG(FILE,'% Recover ','Sequential',
     -                     'Delete !!!')
                      CALL DSNLOG('< intermediate file for copying >',
     -                     'Dataset   ','Sequential','Read/Write')
                      GOTO 1020
                 ENDIF
*   And copy the whole file back to the original file.
                 REWIND(UNIT=9,IOSTAT=IOS,ERR=2055)
320              CONTINUE
                 READ(9,'(A133)',IOSTAT=IOS,ERR=2015,END=330) LINE
                 WRITE(12,'(A)',IOSTAT=IOS,ERR=2010)
     -                LINE(1:STRLEN(LINE))
                 GOTO 320
330              CONTINUE
                 CALL DSNLOG(FILE,'% Recover ','Sequential',
     -                'Read/Write')
            ENDIF
*   Close the scratch file and log its use.
            CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
            CALL DSNLOG('< intermediate file for copying >',
     -           'Dataset   ','Sequential','Read/Write')
**  Keyword not known.
       ELSE
            CALL INPSTR(IKEY,IKEY,STRING,NC)
            PRINT *,' !!!!!! DSNINP WARNING : The instruction '//
     -           STRING(1:NC)//' is not valid; ignored.'
            CALL DSNLOG(FILE,'% Illegal ','Open/Close','None      ')
       ENDIF
*** Close the I/O unit.
1010   CONTINUE
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
1020   CONTINUE
       IF(LOOP)GOTO 1000
       RETURN
*** Handle error conditions.
2010   CONTINUE
       PRINT *,' ###### DSNINP ERROR   : I/O error reading dataset'//
     -      ' "'//FILE(1:NCFILE)//'" via LUN 12 ; attempt to close.'
       CALL INPIOS(IOS)
       GOTO 1010
2015   CONTINUE
       PRINT *,' !!!!!! DSNINP WARNING : I/O error to a temporary'//
     -      ' file on LUN 9; operation not completed, attempt to close.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
       GOTO 1010
2025   CONTINUE
       PRINT *,' !!!!!! DSNINP WARNING : Error opening a temporary'//
     -      ' file on LUN 12 ; operation not started.'
       CALL INPIOS(IOS)
       GOTO 1020
2030   CONTINUE
       PRINT *,' ###### DSNINP ERROR   : Error closing '//
     -      FILE(1:NCFILE)//' on LUN 12 ; results unpredictable.'
       CALL INPIOS(IOS)
       GOTO 1020
2035   CONTINUE
       PRINT *,' !!!!!! DSNINP WARNING : Error closing a temporary'//
     -      ' file on LUN 12 ; results unpredictable.'
       CALL INPIOS(IOS)
       GOTO 1020
2040   CONTINUE
       PRINT *,' ###### DSNINP ERROR   : Error during backspace on '//
     -      FILE(1:NCFILE)//', via LUN 12 ; attempt to close.'
       CALL INPIOS(IOS)
       GOTO 1010
2055   CONTINUE
       PRINT *,' !!!!!! DSNINP WARNING : Error during a rewind of a'//
     -      ' temporary file on LUN 12 ; attempt to close.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=9,IOSTAT=IOS,ERR=2035)
       GOTO 1010
       END
