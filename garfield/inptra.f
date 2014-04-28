CDECK  ID>, INPTRA.
       SUBROUTINE INPTRA(STR,NC)
*-----------------------------------------------------------------------
*   INPTRA - Translation of an input string.
*   INPTRG - Reads a translation table from a dataset.
*   INPTRR - Reads new translation entries.
*   INPTRW - Writes a table to a dataset.
*   (Last changed on 15/ 2/11.)
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
       INTEGER TABLE(0:255),CHRIN,CHROUT,INPCMP,NCYCLE,ICYC,NCYCR,
     -      NCFILE,NCMEMB,NCREM,IFAIL,IFAIL1,IKEY,IOS,INEXT,INIT,NMOD,
     -      NCAUX,NC,I,J
       CHARACTER*(*) STR
       CHARACTER*(MXNAME) FILE
       CHARACTER*80 HEADER,AUX
       CHARACTER*29 REMARK
       CHARACTER*8 DATE,TIME,MEMBER
       CHARACTER*3 IN,OUT
       LOGICAL DSNCMP,EXIS,EXMEMB
       EXTERNAL INPCMP,DSNCMP
       SAVE INIT,TABLE,NCYCLE
       DATA NCYCLE /1/
*** Carry out a translation.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ INPTRA DEBUG   : In ="'',A,
     -      ''"'')') STR(1:MAX(1,MIN(NC,LEN(STR))))
       DO 80 ICYC=1,NCYCLE
       DO 50 I=1,NC
       IF(I.GT.1.AND.STR(MAX(1,I-1):MAX(1,I-1)).EQ.ESCAPE)GOTO 50
       STR(I:I)=CHAR(TABLE(ICHAR(STR(I:I))))
50     CONTINUE
80     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Out="'',A,''"'')')
     -      STR(1:MIN(NC,100))
       RETURN
*** Read the translation table to a file.
       ENTRY INPTRG(IFAIL)
*   Initial values.
       FILE=' '
       NCFILE=8
       MEMBER='*'
       NCMEMB=1
       IFAIL=1
       IKEY=1
**  First decode the argument string: only one argument: file name.
       IF(NWORD.GE.IKEY+1)
     -      CALL INPSTR(IKEY+1,IKEY+1,FILE,NCFILE)
*   If there's a second argument, it is the member name.
       IF(NWORD.GE.IKEY+2)
     -      CALL INPSTR(IKEY+2,IKEY+2,MEMBER,NCMEMB)
*   Check the various lengths.
       IF(NCFILE.GT.MXNAME)THEN
            PRINT *,' !!!!!! INPTRG WARNING : The file name is'//
     -           ' truncated to MXNAME (=',MXNAME,') characters.'
            NCFILE=MIN(NCFILE,MXNAME)
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! INPTRG WARNING : The member name is'//
     -           ' shortened to '//MEMBER//', first 8 characters.'
            NCMEMB=MIN(NCMEMB,8)
       ELSEIF(NCMEMB.LE.0)THEN
            PRINT *,' !!!!!! INPTRG WARNING : The member'//
     -           ' name has zero length, replaced by "*".'
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Reject the empty file name case.
       IF(FILE.EQ.' '.OR.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! INPTRG WARNING : GET must be at least'//
     -           ' followed by a dataset name ; no table is read.'
            RETURN
       ENDIF
*   If there are even more args, warn they are ignored.
       IF(NWORD.GT.IKEY+2)PRINT *,' !!!!!! INPTRG WARNING : GET takes'//
     -     ' at most two arguments (dataset and member); rest ignored.'
**  Open the dataset and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! INPTRG WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; translation table not read.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Translate ','Sequential','Read only ')
       IF(LDEBUG)PRINT *,' ++++++ INPTRG DEBUG   : Dataset',
     -      FILE(1:NCFILE),' opened on unit 12 for seq read.'
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,NCMEMB,'TRANSLAT',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,NCMEMB,'TRANSLAT',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### INPTRG ERROR   : The translation'//
     -                ' table '//MEMBER(1:NCMEMB)//' has been deleted'//
     -                ' from '//FILE(1:NCFILE),'; not read.'
            ELSE
                 PRINT *,' ###### INPTRG ERROR   : Translation table'//
     -                MEMBER(1:NCMEMB)//' not found on '//
     -                FILE(1:NCFILE)//'.'
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
**  Check that the member is acceptable date wise.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) HEADER
       IF(LDEBUG)THEN
            PRINT *,' ++++++ INPTRG DEBUG   : Dataset header'//
     -              ' record follows:'
            PRINT *,HEADER
       ENDIF
       IF(DSNCMP('06-06-90',HEADER(11:18)))THEN
            PRINT *,' !!!!!! INPTRG WARNING : Member '//HEADER(32:39)//
     -           ' can not be read because of a change in format.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
       WRITE(LUNOUT,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      HEADER(32:39),HEADER(11:18),HEADER(23:30),HEADER(51:79)
*   Read the actual data.
       READ(12,'(8X,BN,I3)',END=2000,ERR=2010,IOSTAT=IOS) NCYCLE
       DO 60 I=1,8
       READ(12,'(1X,32I4)',END=2000,ERR=2010,IOSTAT=IOS)
     -      (TABLE(32*I+J-32),J=0,31)
60     CONTINUE
**  Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Reading an input translation table:     ')
       IFAIL=0
       RETURN
*** Initial table.
       ENTRY INPTRI
       DATA INIT/0/
       IF(INIT.EQ.0)THEN
*   Original table is 1 to 1 on most machines.
            DO 10 I=0,255
            TABLE(I)=I
10          CONTINUE
*   On Vax, tabs should become blanks.
            TABLE(9)=32
            TABLE(13)=32
*   Number of cycles.
            NCYCLE=1
*   Remember we set the table.
            INIT=1
       ENDIF
       RETURN
*** Change table entries.
       ENTRY INPTRR
       CALL INPNUM(NWORD)
*   Display current settings if arguments are absent.
       IF(NWORD.EQ.1)THEN
            WRITE(LUNOUT,'(''  INPUT TRANSLATION TABLE:''/)')
            NMOD=0
            DO 40 I=0,255
            IF(TABLE(I).NE.I)THEN
                 IN=' '//CHAR(I)//' '
                 OUT=' '//CHAR(TABLE(I))//' '
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     -                'abcdefghijklmnopqrstuvwxyz'//
     -                '0123456789~!@#$%^&*()_-+={[}]:;"''|\\,.?/><',
     -                CHAR(I)).EQ.0)IN='---'
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ'//
     -                'abcdefghijklmnopqrstuvwxyz'//
     -                '0123456789~!@#$%^&*()_-+={[}]:;"''|\\,.?/><',
     -                CHAR(TABLE(I))).EQ.0)OUT='---'
                 WRITE(LUNOUT,'(2X,I3,'' ('',A3,'') --> '',I3,'' ('',A3,
     -                '')'')') I,IN,TABLE(I),OUT
                 NMOD=NMOD+1
            ENDIF
40          CONTINUE
            IF(NMOD.EQ.0)
     -           WRITE(LUNOUT,'(''  All characters unchanged.'')')
            WRITE(LUNOUT,'(/''  Number of cycles: '',I3,''.''/)') NCYCLE
            RETURN
       ENDIF
*   Loop over the input words.
       INEXT=1
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*   Number of cycles.
       IF(INPCMP(I,'CYC#LES').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Number of cycles is missing.  ')
                 GOTO 30
            ENDIF
            CALL INPCHK(I+1,1,IFAIL1)
            CALL INPRDI(I+1,NCYCR,NCYCLE)
            IF(IFAIL1.EQ.0.AND.(NCYCR.LT.0.OR.NCYCR.GT.256))THEN
                 CALL INPMSG(I+1,'Invalid number of cycles.     ')
            ELSE
                 NCYCLE=NCYCR
            ENDIF
            INEXT=I+2
            GOTO 20
       ENDIF
*   Pick up the character to be translated.
       CHRIN=-1
       IF(INPCMP(I,'INT#EGER').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Character code is missing.    ')
                 GOTO 30
            ENDIF
            CALL INPCHK(I+1,1,IFAIL1)
            CALL INPRDI(I+1,CHRIN,-1)
            IF(IFAIL1.EQ.0.AND.(CHRIN.LT.0.OR.CHRIN.GT.255))THEN
                 CALL INPMSG(I+1,'Character not within range.   ')
                 CHRIN=-1
            ENDIF
            INEXT=I+2
       ELSEIF(INPCMP(I,'HEX#ADECIMAL').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Character code is missing.    ')
                 GOTO 30
            ENDIF
            CALL INPCHK(I+1,3,IFAIL1)
            CALL INPRDH(I+1,CHRIN,-1)
            IF(IFAIL1.EQ.0.AND.(CHRIN.LT.0.OR.CHRIN.GT.255))THEN
                 CALL INPMSG(I+1,'Character not within range.   ')
                 CHRIN=-1
            ENDIF
            INEXT=I+2
       ELSE
            CALL INPSTR(I,I,AUX,NCAUX)
            IF(NCAUX.GT.1)THEN
                 CALL INPMSG(I,'Specify only one character.   ')
                 CHRIN=-1
            ELSE
                 CHRIN=ICHAR(AUX(1:1))
            ENDIF
            INEXT=I+1
       ENDIF
*   Ensure there is an output specification.
       IF(INEXT.GT.NWORD)THEN
            CALL INPMSG(I,'Output character is missing.  ')
            GOTO 30
       ENDIF
*   Pick up the output character.
       CHROUT=-1
       IF(INPCMP(INEXT,'INT#EGER').NE.0)THEN
            IF(INEXT+1.GT.NWORD)THEN
                 CALL INPMSG(INEXT,'Character code is missing.    ')
                 GOTO 30
            ENDIF
            CALL INPCHK(INEXT+1,1,IFAIL1)
            CALL INPRDI(INEXT+1,CHROUT,-1)
            IF(IFAIL1.EQ.0.AND.(CHROUT.LT.0.OR.CHROUT.GT.255))THEN
                 CALL INPMSG(INEXT+1,'Character not within range.   ')
                 CHROUT=-1
            ENDIF
            INEXT=INEXT+2
       ELSEIF(INPCMP(INEXT,'HEX#ADECIMAL').NE.0)THEN
            IF(INEXT+1.GT.NWORD)THEN
                 CALL INPMSG(INEXT,'Character code is missing.    ')
                 GOTO 30
            ENDIF
            CALL INPCHK(INEXT+1,3,IFAIL1)
            CALL INPRDH(INEXT+1,CHROUT,-1)
            IF(IFAIL1.EQ.0.AND.(CHROUT.LT.0.OR.CHROUT.GT.255))THEN
                 CALL INPMSG(INEXT+1,'Character not within range.   ')
                 CHROUT=-1
            ENDIF
            INEXT=INEXT+2
       ELSE
            CALL INPSTR(INEXT,INEXT,AUX,NCAUX)
            IF(NCAUX.GT.1)THEN
                 CALL INPMSG(INEXT,'Specify only one character.   ')
                 CHROUT=-1
            ELSE
                 CHROUT=ICHAR(AUX(1:1))
            ENDIF
            INEXT=INEXT+1
       ENDIF
*   Update the translation table.
       IF(CHRIN.GE.0.AND.CHROUT.GE.0.AND.
     -      CHRIN.LE.255.AND.CHROUT.LE.255)TABLE(CHRIN)=CHROUT
20     CONTINUE
30     CONTINUE
*   Dump error messages.
       CALL INPERR
       RETURN
*** Write the translation table to a file.
       ENTRY INPTRW(IFAIL)
*   Initial settings.
       FILE=' '
       NCFILE=1
       MEMBER='< none >'
       NCMEMB=8
       REMARK='none'
       NCREM=4
       IFAIL=1
       IKEY=1
*   First decode the argument string.
       CALL INPNUM(NWORD)
*   Make sure there is at least one argument.
       IF(NWORD.EQ.IKEY)THEN
            PRINT *,' !!!!!! INPTRW WARNING : WRITE takes at least one',
     -           ' argument (a dataset name); data will not be written.'
            RETURN
*   Check whether keywords have been used.
       ELSEIF(INPCMP(IKEY+1,'D#ATASET')+
     -      INPCMP(IKEY+1,'R#EMARK').NE.0)THEN
            INEXT=IKEY+1
            DO 410 I=IKEY+1,NWORD
            IF(I.LT.INEXT)GOTO 410
            IF(INPCMP(I,'DATA#SET').NE.0)THEN
                 IF(INPCMP(I+1,'REM#ARK').NE.0.OR.I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'The dataset name is missing.  ')
                      INEXT=I+1
                 ELSE
                      CALL INPSTR(I+1,I+1,FILE,NCFILE)
                      INEXT=I+2
                      IF(INPCMP(I+2,'R#EMARK').EQ.0.AND.
     -                     I+2.LE.NWORD)THEN
                           CALL INPSTR(I+2,I+2,MEMBER,NCMEMB)
                           INEXT=I+3
                      ENDIF
                 ENDIF
            ELSEIF(INPCMP(I,'REM#ARK').NE.0)THEN
                 IF(INPCMP(I+1,'D#ATASET').NE.0.OR.I+1.GT.NWORD)THEN
                      CALL INPMSG(I,'The remark is missing.        ')
                      INEXT=I+1
                 ELSE
                      CALL INPSTR(I+1,I+1,REMARK,NCREM)
                      INEXT=I+2
                 ENDIF
            ELSE
                 CALL INPMSG(I,'The parameter is not known.    ')
            ENDIF
410         CONTINUE
*   Otherwise the string is interpreted as a file name (+ member name).
       ELSE
            CALL INPSTR(IKEY+1,IKEY+1,FILE,NCFILE)
            IF(NWORD.GE.IKEY+2)
     -           CALL INPSTR(IKEY+2,IKEY+2,MEMBER,NCMEMB)
            IF(NWORD.GE.IKEY+3)
     -           CALL INPSTR(IKEY+3,NWORD,REMARK,NCREM)
       ENDIF
*   Print error messages.
       CALL INPERR
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! INPTRW WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! INPTRW WARNING : The member',
     -      ' name is shortened to ',MEMBER,', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! INPTRW WARNING : The remark',
     -      ' shortened to ',REMARK,', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'TRANSLAT',EXMEMB)
       IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
            PRINT *,' ------ INPTRW MESSAGE : A copy of the member'//
     -           ' exists; new member will be appended.'
       ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
            PRINT *,' !!!!!! INPTRW WARNING : A copy of the member'//
     -           ' exists already; member will not be written.'
            RETURN
       ENDIF
*   Print some debugging output if requested.
       IF(LDEBUG)THEN
            PRINT *,' ++++++ INPTRW DEBUG   : File= '//FILE(1:NCFILE)//
     -           ', member= '//MEMBER(1:NCMEMB)
            PRINT *,'                         Remark= '//REMARK(1:NCREM)
       ENDIF
**  Open the dataset for sequential write and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! INPTRW WARNING : Opening '//FILE(1:NCFILE),
     -           ' failed ; the translation table is not written.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Translate ','Sequential','Write     ')
       IF(LDEBUG)PRINT *,' ++++++ INPTRW DEBUG   : Dataset ',
     -      FILE(1:NCFILE),' opened on unit 12 for seq write.'
*   Now write a heading record to the file.
       CALL DATTIM(DATE,TIME)
       WRITE(HEADER,'(''% Created '',A8,'' At '',A8,1X,A8,'' TRANSLAT'',
     -      1X,''"'',A29,''"'')') DATE,TIME,MEMBER,REMARK
       WRITE(12,'(A80)',IOSTAT=IOS,ERR=2010) HEADER
       IF(LDEBUG)THEN
            PRINT *,' ++++++ INPTRW DEBUG   : Dataset heading record:'
            PRINT *,HEADER
       ENDIF
*   Write the translation table.
       WRITE(12,'(''Cycles: '',I3)',ERR=2010,IOSTAT=IOS) NCYCLE
       DO 70 I=1,8
       WRITE(12,'(1X,32I4)',ERR=2010,IOSTAT=IOS)
     -      (TABLE(32*I+J-32),J=0,31)
70     CONTINUE
*   Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       CALL TIMLOG('Writing out a translation table:        ')
       IFAIL=0
       RETURN
*** I/O error handling.
2000   CONTINUE
       PRINT *,' ###### INPTRG ERROR   : Premature EOF ecountered on '//
     -      FILE(1:NCFILE)//' read via unit 12 ; no valid data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### INPTRA ERROR   : I/O error accessing '//
     -      FILE(1:NCFILE)//' via unit 12 ; no data read or written.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### INPTRA ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
