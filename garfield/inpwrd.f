CDECK  ID>, INPWRD.
       SUBROUTINE INPWRD(NNWORD)
*-----------------------------------------------------------------------
*   INPWRD - Asks INPGET to read a record, checks whether it contains
*            any special characters, takes appropriate action if
*            required and returns otherwise.
*   VARIABLES : NNWORD      : =NWORD
*   (Last changed on  1/11/12.)
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
       CHARACTER*(MXINCH+1) STRING
       CHARACTER*(MXINCH)   ARGSTR
       CHARACTER*30         ERRCDE(MXWORD)
       CHARACTER*(MXCHAR)   WORD(MXWORD)
       CHARACTER*80         PROMPT,EOFSTR,SHELL
       CHARACTER            ESCAPE
       CHARACTER*(MXNAME)   FNINP,FNOUT
       INTEGER NCHAR(MXWORD),INDWRD(MXWORD),ICHSET,LUNSTR(5:MXLUN,3),
     -      NWORD,LUN,NCPROM,NCEOF,NCSH,NCARG,NCFNI,NCFNO
       LOGICAL ERRPRT(MXWORD),LPROM,DOEXEC,DOREAD,LINREC
       COMMON /INPCOM/ NCHAR,INDWRD,LUNSTR,NWORD,LUN,ICHSET,NCPROM,
     -      ERRPRT,LPROM,DOEXEC,DOREAD,NCEOF,LINREC,NCSH,NCARG,
     -      NCFNI,NCFNO
       COMMON /INPCHR/ ERRCDE,STRING,WORD,PROMPT,EOFSTR,ESCAPE,SHELL,
     -      ARGSTR,FNINP,FNOUT
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       CHARACTER*(MXINCH) FILE,LINE
       CHARACTER*(MXNAME) AUX
       CHARACTER*29 REMARK
       CHARACTER*8 DATE,TIME,MEMBER
       CHARACTER ESCAUX
C       LOGICAL EXMEMB,USE(MXVAR)
       INTEGER NCMEMB,NCREM,NCFILE,NC,IFILE,LUNTRY,IEOF,NCAUX,IFAIL,
     -      IKEY,I,IOS,IDOLLR,NNWORD,INPCMP,IDUMMY,NCESC,NREXP
C       INTEGER IENTRY
       EXTERNAL INPCMP
       integer systemf,ierr
       external systemf
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE INPWRD ///'
*** Return here if the command has been recognised as global.
1000   CONTINUE
*** Next read a line from the input.
       CALL INPGET
*** Pick up the first word to see whether there is an escape character.
       CALL INPSTR(1,1,LINE,NC)
*** Open a unit if input is to continue from an external file.
       IF(NWORD.GE.1.AND.LINE(1:1).EQ.'<'.AND.NC.GE.1)THEN
            IF(LDEBUG)PRINT *,' ++++++ INPWRD DEBUG   : Statement is'//
     -           ' an alternate input request.'
*   Decode the file name.
            IF(NC.EQ.1.AND.NWORD.EQ.1)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : A file name must'//
     -                ' be specified on a "<" line; no file opened.'
                 GOTO 1000
            ELSEIF(NC.EQ.1)THEN
                 IFILE=2
                 CALL INPSTR(2,2,FILE,NCFILE)
            ELSE
                 IFILE=1
                 FILE=LINE(2:)
                 NCFILE=NC-1
            ENDIF
*   Check whether there is perhaps also an EOF string.
            IF(IFILE.LT.NWORD)CALL INPSTR(IFILE+1,IFILE+1,LINE,NC)
            IF(LINE(1:2).EQ.'<<'.AND.NWORD.GT.IFILE.AND.NC.GE.2)THEN
                 IF(NC.GT.2)THEN
                      EOFSTR=LINE(3:)
                      NCEOF=NC-2
                      IEOF=IFILE+1
                 ELSEIF(NWORD.GE.IFILE+2)THEN
                      CALL INPSTR(IFILE+2,IFILE+2,EOFSTR,NCEOF)
                      IEOF=IFILE+2
                 ELSE
                      PRINT *,' INPWRD WARNING : The "<<" sign must'//
     -                     ' be followed by a label; no file opened.'
                      GOTO 1000
                 ENDIF
            ELSE
                 EOFSTR='EOF'
                 NCEOF=3
                 IEOF=IFILE
            ENDIF
*   All remaining arguments should go to the arguments string.
            IF(NWORD.GT.IEOF)THEN
                 CALL INPSTR(IEOF+1,NWORD,ARGSTR,NCARG)
            ELSE
                 ARGSTR=' '
                 NCARG=1
            ENDIF
*   Fetch old file name for printing error messages,
            CALL STRBUF('READ',LUNSTR(LUN,1),AUX,NCAUX,IFAIL)
*   Increment the LUN by one.
            IF(LUN.GE.20)LUNTRY=LUN+1
            IF(LUN.EQ.5 )LUNTRY=20
            IF(LUNTRY.GT.MXLUN)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : Maximum number of'//
     -                ' open I/O units reached ; input resumed from'//
     -                AUX(1:NCAUX)//'.'
                 GOTO 1000
            ENDIF
*   Open the file and register the opening with DSNLOG.
            CALL DSNOPN(FILE,NCFILE,LUNTRY,'READ-FILE',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : Opening '//
     -                FILE(1:NCFILE)//' failed; input resumed'//
     -                ' from '//AUX(1:NCAUX)//'.'
                 GOTO 1000
            ENDIF
            CALL DSNLOG(FILE,'Input     ','Sequential','Read only ')
            IF(LDEBUG)PRINT *,' ++++++ INPWRD DEBUG   : '//
     -           FILE(1:NCFILE)//' opened on unit ',LUNTRY
*   Store the logical unit.
            LUN=LUNTRY
*   Store file name, EOF label and arguments for reference purposes.
            CALL STRBUF('STORE',LUNSTR(LUN,1),FILE(1:NCFILE),
     -           NCFILE,IFAIL)
            CALL STRBUF('STORE',LUNSTR(LUN,2),EOFSTR(1:NCEOF),
     -           NCEOF,IFAIL)
            GLBVAL(6)=LUNSTR(LUN,1)
            CALL STRBUF('STORE',LUNSTR(LUN,3),ARGSTR(1:NCARG),
     -           NCARG,IFAIL)
*** Recording requests.
       ELSEIF(NWORD.GE.1.AND.LINE(1:2).EQ.'>>')THEN
            IF(LDEBUG)PRINT *,' ++++++ INPWRD DEBUG   : Statement is'//
     -           ' a recording request.'
*   First of all close the present recording file.
            IF(LINREC)CLOSE(UNIT=18,STATUS='KEEP',
     -           IOSTAT=IOS,ERR=2030)
*   Next find the new file name.
            CALL INPSTR(1,1,LINE,NC)
            IF(NWORD.EQ.1.AND.NC.GT.2)THEN
                 FILE=LINE(3:)//'  '
                 IKEY=1
                 NCFILE=NC-2
            ELSEIF(NWORD.EQ.1.AND.NC.EQ.2)THEN
                 IF(.NOT.LINREC)PRINT *,' !!!!!! INPWRD WARNING :'//
     -                ' Input recording was not active.'
                 LINREC=.FALSE.
                 GOTO 1000
            ELSEIF(NWORD.GT.1.AND.NC.EQ.2)THEN
                 CALL INPSTR(2,2,FILE,NCFILE)
                 IKEY=2
            ENDIF
*   Open a file on unit 18 for recording.
            CALL DSNOPN(FILE,NCFILE,18,'WRITE-FILE',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : Recording on '//
     -                FILE(1:NCFILE)//' cancelled because of an'//
     -                ' error while opening the file.'
                 LINREC=.FALSE.
                 GOTO 1000
            ENDIF
            CALL DSNLOG(FILE,'Recording ','Sequential','Write     ')
*   And set the recording flag to active.
            LINREC=.TRUE.
*** Redirect output if requested.
       ELSEIF(NWORD.GE.1.AND.LINE(1:1).EQ.'>')THEN
            IF(LDEBUG)PRINT *,' ++++++ INPWRD DEBUG   : Statement is'//
     -           ' an alternate output request.'
*   First of all close the present output file, if connected to unit 8.
            IF(LUNOUT.EQ.8)CLOSE(UNIT=8,STATUS='KEEP',
     -           IOSTAT=IOS,ERR=2030)
*   Next find the new file name.
            CALL INPSTR(1,1,LINE,NC)
            IF(NWORD.EQ.1.AND.NC.GT.1)THEN
                 FILE=LINE(2:)//' '
                 IKEY=1
                 NCFILE=NC-1
            ELSEIF(NWORD.EQ.1.AND.NC.EQ.1)THEN
                 IF(LUNOUT.EQ.6)PRINT *,' !!!!!! INPWRD WARNING : No'//
     -                ' output rerouting was in effect.'
                 CALL STRSAV('Standard output','OUTPUT',IFAIL)
                 LUNOUT=6
                 GOTO 1000
            ELSEIF(NWORD.GT.1.AND.NC.EQ.1)THEN
                 CALL INPSTR(2,2,FILE,NCFILE)
                 IKEY=2
            ENDIF
*   And find the member name, if present.
            IF(NWORD.GE.IKEY+1)THEN
                 CALL INPSTR(IKEY+1,IKEY+1,LINE,NCMEMB)
                 MEMBER=LINE(1:8)
            ELSE
                 MEMBER='< none >'
                 NCMEMB=8
            ENDIF
*   All that remains, is taken to be the remark.
            IF(NWORD.GE.IKEY+2)THEN
                 CALL INPSTR(IKEY+2,NWORD,LINE,NCREM)
                 REMARK=LINE(1:29)
            ELSE
                 REMARK='Printed output'
                 NCREM=14
            ENDIF
*   Print warnings for too long member names and remarks.
            IF(NCMEMB.GT.8)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : The member name is'//
     -                ' truncated to '//MEMBER//', first 8 characters.'
                 NCMEMB=8
            ENDIF
            IF(NCREM.GT.29)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : The remark is'//
     -                ' truncated to "'//REMARK//'" (29 characters).'
                 NCREM=29
            ENDIF
*   Check whether the member already exists.
C            CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'OUTPUT',EXMEMB)
C            IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
C                 PRINT *,' ------ INPWRD MESSAGE : A copy of the'//
C     -                ' member exists; output will be appended.'
C            ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
C                 PRINT *,' !!!!!! INPWRD WARNING : A copy of the'//
C     -                ' member exists already; output not redirected.'
C                 GOTO 1000
C            ENDIF
*   Open a file on unit 8 for the output.
            CALL DSNOPN(FILE,NCFILE,8,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPWRD WARNING : The output can not'//
     -                ' be rerouted to '//FILE(1:NCFILE)//' due to an'//
     -                ' error while opening the file.'
                 GOTO 1000
            ENDIF
            CALL DSNLOG(FILE,'Output    ','Sequential','Write     ')
*   Now write a heading record to the file ...
            CALL DATTIM(DATE,TIME)
            WRITE(LINE,'(''% Created '',A8,'' At '',A8,1X,A8,1X,
     -           ''OUTPUT  '',1X,''"'',A29,''"'')') DATE,TIME,MEMBER,
     -           REMARK
            WRITE(8,'(A80)',IOSTAT=IOS,ERR=2010) LINE
*   and set the new output logical file number.
            LUNOUT=8
*   Set the name of the output stream.
            CALL STRSAV(FILE(1:NCFILE),'OUTPUT',IFAIL)
*** Algebra debugging.
       ELSEIF(LINE(1:1).EQ.'@')THEN
            NREXP=0
            CALL ALGINP
C            CALL ALGEDT(GLBVAR,NGLB,IENTRY,USE,NREXP)
C            CALL ALGCLR(IENTRY)
*** String buffer dump.
       ELSEIF(INPCMP(1,'DUMP-ST#RING-#BUFFER').NE.0)THEN
            CALL STRBUF('DUMP',0,' ',1,IFAIL)
*** Pass command to the environment if the line starts with a $.
       ELSEIF(NWORD.GE.1.AND.LINE(1:1).EQ.'$')THEN
            CALL INPSTR(1,NWORD,LINE,NC)
            IDOLLR=INDEX(LINE,'$')
            IF(IDOLLR.NE.0)LINE(IDOLLR:IDOLLR)=' '
*** Unix version courtesy Francois Marabelle.
            IF(NC.EQ.1.AND.NWORD.EQ.1)THEN
                 PRINT *,' ------ INPWRD MESSAGE : You enter a'//
     -                ' subprocess, type exit to get back.'
                 IERR=SYSTEMF(SHELL(1:NCSH))
                 IF(IERR.NE.0)PRINT *,' !!!!!! INPWRD'//
     -                ' WARNING : The subprocess did not complete'//
     -                ' successfully.'
                 PRINT *,' ------ INPWRD MESSAGE : You are back'//
     -                ' inside Garfield.'
            ELSE
                 IF(LDEBUG)PRINT *,' ++++++ INPWRD DEBUG   : Spawn "'//
     -                LINE(1:NC)//'".'
                 IERR=SYSTEMF(SHELL(1:NCSH)//' -c "'//LINE(1:NC)//'"')
                 IF(IERR.NE.0)PRINT *,' !!!!!! INPWRD'//
     -                ' WARNING : The '//SHELL(1:NCSH)//' command did'//
     -                ' not complete successfully.'
            ENDIF
*** Skip comment lines, starting with a '*'.
       ELSEIF(NWORD.GE.1.AND.LINE(1:1).EQ.'*')THEN
            GOTO 1000
*** Check for help lines, starting with ?.
       ELSEIF(NWORD.GE.1.AND.(LINE(1:1).EQ.'?'.OR.INPCMP(1,'HELP')+
     -      INPCMP(1,'INFO#RMATION').NE.0))THEN
            CALL HLPINP
*** Graphics options are lines starting with a !.
       ELSEIF(NWORD.GE.1.AND.LINE(1:1).EQ.'!')THEN
            CALL GRAINP
*** Dataset commands are lines starting with a %.
       ELSEIF(NWORD.GE.1.AND.LINE(1:1).EQ.'%')THEN
            CALL DSNINP
*** List current options.
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0.AND.NWORD.EQ.1)THEN
            WRITE(LUNOUT,'(
     -           ''  GLOBAL OPTIONS CURRENTLY IN EFFECT:''//
     -           ''  Routine identifiers printed (IDENTIFICATION): '',
     -           L1/
     -           ''  Debugging output is generated (DEBUG):        '',
     -           L1/
     -           ''  Echoing of the input lines (INPUT-LISTING):   '',
     -           L1/
     -           ''  Record input from terminal (RECORDING):       '',
     -           L1/
     -           ''  Inform about progress (PROGRESS-PRINT):       '',
     -           L1)') LIDENT,LDEBUG,LINPUT,LINREC,LPROPR
            IF(JFAIL.EQ.1)WRITE(LUNOUT,'(
     -           ''  Action to be taken in case of input errors:   '',
     -           ''carry on with defaults.'')')
            IF(JFAIL.EQ.2)WRITE(LUNOUT,'(
     -           ''  Action to be taken in case of input errors:   '',
     -           ''skip the instruction.'')')
            IF(JFAIL.EQ.3)WRITE(LUNOUT,'(
     -           ''  Action to be taken in case of input errors:   '',
     -           ''terminate execution.'')')
            IF(JEXMEM.EQ.1)WRITE(LUNOUT,'(
     -           ''  If a member to be written exists already:     '',
     -           ''mark existing member for deletion.'')')
            IF(JEXMEM.EQ.2)WRITE(LUNOUT,'(
     -           ''  If a member to be written exists already:     '',
     -           ''issue a warning, and append new member.'')')
            IF(JEXMEM.EQ.3)WRITE(LUNOUT,'(
     -           ''  If a member to be written exists already:     '',
     -           ''issue a warning, do not write new member.'')')
            IF(LGSTOP)THEN
                 WRITE(LUNOUT,'(
     -                ''  In case of a graphics error:             '',
     -                ''     dump data and quit.'')')
            ELSE
                 WRITE(LUNOUT,'(
     -                ''  In case of a graphics error:             '',
     -                ''     print a warning.'')')
            ENDIF
            NNWORD=1
            RETURN
*   Update options.
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            I=2
            NNWORD=NWORD
10          CONTINUE
*   Trace routine calls or not.
            IF(INPCMP(I,'ID#ENTIFICATION').NE.0)THEN
                 LIDENT=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NOID#ENTIFICATION').NE.0)THEN
                 LIDENT=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Debug output.
            ELSEIF(INPCMP(I,'DEB#UGGING').NE.0)THEN
                 LDEBUG=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NODEB#UGGING').NE.0)THEN
                 LDEBUG=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Input echoing.
            ELSEIF(INPCMP(I,'IN#PUT-#LISTING').NE.0)THEN
                 LINPUT=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NOIN#PUT-#LISTING').NE.0)THEN
                 LINPUT=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Synchronisation output.
            ELSEIF(INPCMP(I,'SYN#CHRONISE').NE.0)THEN
                 LSYNCH=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NOSYN#CHRONISE').NE.0)THEN
                 LSYNCH=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Permit input redirection in loops
            ELSEIF(INPCMP(I,'REDIR#ECT-LOOP-#INPUT').NE.0)THEN
                 LINPRD=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NOREDIR#ECT-LOOP-#INPUT').NE.0)THEN
                 LINPRD=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Record terminal input.
            ELSEIF(INPCMP(I,'REC#ORDING').NE.0)THEN
*   First of all close the present recording file.
                 IF(LINREC)CLOSE(UNIT=18,STATUS='KEEP',
     -                IOSTAT=IOS,ERR=2030)
*   Next set the new file name.
                 FILE='garflast.dat'
                 NCFILE=12
*   Open a file on unit 18 for recording.
                 CALL DSNOPN(FILE,NCFILE,18,'WRITE-FILE',IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! INPWRD WARNING : Recording on '//
     -                     FILE(1:NCFILE)//' cancelled because of an'//
     -                     ' error while opening the file.'
                      LINREC=.FALSE.
                      GOTO 10
                 ENDIF
                 CALL DSNLOG(FILE,'Recording ','Sequential',
     -                'Write     ')
*   And set the recording flag to active.
                 LINREC=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NOREC#ORDING').NE.0)THEN
                 LINREC=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Keep informed about progress.
            ELSEIF(INPCMP(I,'PRO#GRESS-#PRINT').NE.0)THEN
                 LPROPR=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NOPRO#GRESS-#PRINT').NE.0)THEN
                 LPROPR=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Handling of errors.
            ELSEIF(INPCMP(I,'ON-E#RROR-C#ONTINUE').NE.0)THEN
                 JFAIL=1
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'ON-E#RROR-S#KIP').NE.0)THEN
                 JFAIL=2
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'ON-E#RROR-T#ERMINATE').NE.0)THEN
                 JFAIL=3
                 CALL INPDEL(I)
                 GOTO 10
*   Graphics error handling.
            ELSEIF(INPCMP(I,'DUMP-ON-GR#APHICS-#ERROR').NE.0)THEN
                 LGSTOP=.TRUE.
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'NODUMP-ON-GR#APHICS-#ERROR').NE.0)THEN
                 LGSTOP=.FALSE.
                 CALL INPDEL(I)
                 GOTO 10
*   Handling of existing members.
            ELSEIF(INPCMP(I,'DEL#ETE-OLD-MEM#BER').NE.0)THEN
                 JEXMEM=1
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'WARN-BUT-WR#ITE')+
     -           INPCMP(I,'WR#ITE-BUT-WARN').NE.0)THEN
                 JEXMEM=2
                 CALL INPDEL(I)
                 GOTO 10
            ELSEIF(INPCMP(I,'WARN-AND-NOWR#ITE')+
     -           INPCMP(I,'NOWR#ITE-AND-WARN').NE.0)THEN
                 JEXMEM=3
                 CALL INPDEL(I)
                 GOTO 10
            ENDIF
            I=I+1
            IF(I.LE.NWORD)GOTO 10
            IF(NNWORD.GT.1.AND.NWORD.EQ.1)GOTO 1000
            NNWORD=NWORD
            RETURN
*** Escape character handling.
       ELSEIF(INPCMP(1,'ESC#APE').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/''  Current escape character is '',
     -                A1,'' ('',I3,'').''/)') ESCAPE,ICHAR(ESCAPE)
            ELSE
                 CALL INPSTR(2,2,ESCAUX,NCESC)
                 IF(INDEX('''"` ,=',ESCAUX).NE.0)THEN
                      PRINT *,' !!!!!! INPWRD WARNING : The escape'//
     -                     ' character can not be an accent or a'//
     -                     ' word separator ; not redefined.'
                 ELSEIF(INDEX('!%&#<>$*?@',ESCAUX).NE.0)THEN
                      PRINT *,' !!!!!! INPWRD WARNING : The escape'//
     -                     ' character can not be a (sub-)section'//
     -                     ' header ; not redefined.'
                 ELSEIF(INDEX('{}[]()',ESCAUX).NE.0)THEN
                      PRINT *,' !!!!!! INPWRD WARNING : The escape'//
     -                     ' character can not be a parenthesis ;'//
     -                     ' not redefined.'
                 ELSEIF(NCESC.LE.0)THEN
                      PRINT *,' !!!!!! INPWRD WARNING : The escape'//
     -                     ' character can not be a null string ;'//
     -                     ' not redefined.'
                 ELSE
                      IF(NCESC.GT.1)PRINT *,' ------ INPWRD MESSAGE :'//
     -                     ' Only first character of escape used.'
                      ESCAPE=ESCAUX
                 ENDIF
            ENDIF
*** Shell.
       ELSEIF(INPCMP(1,'SH#ELL').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/''  Current shell is '',
     -                A,''.''/)') SHELL(1:NCSH)
            ELSE
                 CALL INPSTR(2,2,SHELL,NCSH)
            ENDIF
*** Input translation commands.
       ELSEIF(INPCMP(1,'TRAN#SLATE').NE.0)THEN
            CALL INPTRR
       ELSEIF(INPCMP(1,'GET-TRAN#SLATION-#TABLE').NE.0)THEN
            CALL INPTRG(IFAIL)
       ELSEIF(INPCMP(1,'WR#ITE-TRAN#SLATION-#TABLE').NE.0)THEN
            CALL INPTRW(IFAIL)
*** CERN library error messages.
       ELSEIF(INPCMP(1,'ERR#OR-#HANDLING').NE.0)THEN
            CALL CRNERR
*** Read some vectors.
       ELSEIF(INPCMP(1,'R#EAD-VEC#TORS')+
     -      INPCMP(1,'VEC#TORS-#READ').NE.0)THEN
            CALL MATVCR(IFAIL)
*** Start of a DO loop.
       ELSEIF(INPCMP(1,'FOR')+INPCMP(1,'WHILE')+INPCMP(1,'UNTIL')+
     -      INPCMP(1,'DO')+INPCMP(1,'IF')+INPCMP(1,'STEP').NE.0.AND.
     -      INPCMP(NWORD,'DO').NE.0)THEN
            DOREAD=.TRUE.
            CALL INPRDO(IFAIL)
            DOREAD=.FALSE.
            IF(IFAIL.EQ.0)THEN
                 DOEXEC=.TRUE.
            ELSE
                 PRINT *,' !!!!!! INPWRD WARNING : Reading the DO'//
     -                ' loop failed; normal input resumed.'
            ENDIF
*** Global variables.
       ELSEIF(INPCMP(1,'GL#OBALS').NE.0)THEN
            CALL INPGLB
*** Read a line.
       ELSEIF(INPCMP(1,'PARSE').NE.0)THEN
            CALL INPPAR(IFAIL)
*** Echo a line.
       ELSEIF(INPCMP(1,'SAY').NE.0)THEN
            CALL INPSTR(2,NWORD,LINE,NC)
            WRITE(LUNOUT,'(2X,A)') LINE(1:NC)
*** Procedure calls.
       ELSEIF(INPCMP(1,'CALL').NE.0)THEN
            CALL INPCAL('EXECUTE',IDUMMY,IFAIL)
*** Return because it's apparently not a special command.
       ELSE
            NNWORD=NWORD
            RETURN
       ENDIF
       GOTO 1000
*** Handle I/O problems.
2010   CONTINUE
       PRINT *,' !!!!!! INPWRD WARNING : Error writing the'//
     -      ' heading record ; output not rerouted.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=8,IOSTAT=IOS,ERR=2030)
       GOTO 1000
2030   CONTINUE
       PRINT *,' !!!!!! INPWRD WARNING : Closing the unit failed,'//
     -      ' rerouting the output will no longer be possible.'
       CALL INPIOS(IOS)
       GOTO 1000
       END
