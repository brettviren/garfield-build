CDECK  ID>, HLPINPOT.
       SUBROUTINE HLPINP
*-----------------------------------------------------------------------
*   HLPINP - Reads the help commands and fetches the information.
*   (Last changed on 10/11/09.)
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
       CHARACTER*100 FILE
       CHARACTER*80 HOME
       INTEGER NCHOME
       CHARACTER*80 STRING,BLANK
       CHARACTER*20 TOPIC,SEARCH(MXHLEV),TOPL(MXHLEV),TOPTL(MXHLEV),AUX
       INTEGER PATH(MXHLEV),IRECL(MXHLEV),IRECTL(MXHLEV),NOCCUR,
     -      INPCMP,NCFILE,IOS,NWORD,I,ISMIN,ISMAX,NC,IOLD,NPATH,NSUB,
     -      NSUBN,IREC,IFAIL,ISTR,NCAUX
       LOGICAL EXIST,MATCH,DSNCMP
       EXTERNAL INPCMP,DSNCMP
*** Set the blank string which is used for indenting.
       BLANK=' '
*** Open the help file.
*   Determine home directory.
       CALL GETENV('HOME',HOME)
       DO 50 I=LEN(HOME),1,-1
       IF(HOME(I:I).NE.' ')THEN
            NCHOME=I
            GOTO 60
       ENDIF
50     CONTINUE
       NCHOME=1
       HOME=' '
60     CONTINUE
*   Try a file or link in the current directory.
       FILE='garfield.packhelp'
       NCFILE=17
       INQUIRE(FILE=FILE(1:NCFILE),EXIST=EXIST)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPINP DEBUG   :'',
     -      '' Checking for '',A/26X,''Existence flag: '',L1)')
     -      FILE(1:NCFILE),EXIST
*   If not found, look in the home directory.
       IF(.NOT.EXIST)THEN
            FILE=HOME(1:NCHOME)//'/garfield.packhelp'
            NCFILE=MIN(NCHOME+18,LEN(FILE))
            INQUIRE(FILE=FILE(1:NCFILE),EXIST=EXIST)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPINP DEBUG   :'',
     -           '' Checking for '',A/26X,''Existence flag: '',L1)')
     -      FILE(1:NCFILE),EXIST
       ENDIF
       IF(.NOT.EXIST)THEN
            FILE=HOME(1:NCHOME)//'/.garfield.packhelp'
            NCFILE=MIN(NCHOME+19,LEN(FILE))
            INQUIRE(FILE=FILE(1:NCFILE),EXIST=EXIST)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPINP DEBUG   :'',
     -           '' Checking for '',A/26X,''Existence flag: '',L1)')
     -      FILE(1:NCFILE),EXIST
       ENDIF
*   If still not found, try the AFS file name.
       IF(.NOT.EXIST)THEN
            FILE='/afs/cern.ch/user/r/rjd/Garfield/Files/'//
     -           'garfield.packhelp'
            NCFILE=MIN(56,LEN(FILE))
            INQUIRE(FILE=FILE(1:NCFILE),EXIST=EXIST)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPINP DEBUG   :'',
     -           '' Checking for '',A/26X,''Existence flag: '',L1)')
     -      FILE(1:NCFILE),EXIST
       ENDIF
*   If found, open the file.
       IF(EXIST)THEN
            OPEN(UNIT=17,FILE=FILE(1:NCFILE),ACCESS='DIRECT',
     -           STATUS='OLD',RECL=MXHLRL,IOSTAT=IOS,ERR=2020)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPINP DEBUG   :'',
     -           '' Opened '',A)') FILE(1:NCFILE)
       ELSE
            PRINT *,' !!!!!! HLPINP WARNING : No help library'//
     -           ' found; try the URL'
            PRINT *,'                         http://consult'//
     -           '.cern.ch/writeup/garfield/help'
            CALL INPPRM(' ','BACK')
            RETURN
       ENDIF
*** Read the root record to check the date on which the file was packed.
       READ(UNIT=17,REC=1,ERR=2010,IOSTAT=IOS) TOPIC
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ HLPINP DEBUG   :'',
     -      '' Creation date of help library: '',A)') TOPIC(5:12)
       IF(DSNCMP('01-01-09',TOPIC(5:12)).OR.
     -      DSNCMP(TOPIC(5:12),'01-01-11').OR.
     -      TOPIC(5:12).EQ.'        ')THEN
            PRINT *,' !!!!!! HLPINP WARNING : Mismatch between the'//
     -           ' help file and program versions;'
            PRINT *,'                         contact the program'//
     -           ' library office or the author.'
C            CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
C            RETURN
       ENDIF
*** Set the prompt.
       CALL INPPRM('Help','ADD-PRINT')
*** Pick up the initial list.
       CALL INPNUM(NWORD)
       IF(NWORD.EQ.1.AND.INPCMP(1,'?')+INPCMP(1,'HELP')+
     -      INPCMP(1,'INFO#RMATION').NE.0)THEN
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------      Help subsection       ----------'
            PRINT *,' ------------------------------------------------'
40          CONTINUE
            CALL HLPSUB(1,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! HLPINP WARNING : Unable to list'//
     -                ' the subtopics; help ended.'
                 CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
                 RETURN
            ENDIF
            CALL INPPRM('Topic','ADD-PRINT')
            WRITE(LUNOUT,'('' '')')
            CALL INPGET
            CALL INPNUM(NWORD)
            CALL INPPRM(' ','BACK-PRINT')
            IF(INPCMP(1,'?').NE.0)GOTO 40
       ENDIF
*** Return if all parameters are absent, shouldn't be the case.
       IF(NWORD.EQ.0)THEN
            CALL INPPRM(' ','BACK')
            CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   Store the parameters in the search stack.
       ISMIN=1
       ISMAX=0
       DO 10 I=1,NWORD
*   Get the string, skip if blank.
       CALL INPSTR(I,I,STRING,NC)
       IF(NC.EQ.0.OR.STRING.EQ.' '.OR.(INPCMP(I,'?')+INPCMP(I,'HELP')+
     -      INPCMP(I,'INFO#RMATION').NE.0.AND.I.EQ.1))GOTO 10
*   Add to the stack.
       IF(ISMAX+1.GT.MXHLEV)THEN
            PRINT *,' !!!!!! HLPINP WARNING : Too many keywords'//
     -           ' provided, list truncated.'
            GOTO 30
       ENDIF
       ISMAX=ISMAX+1
       IF(I.EQ.1.AND.STRING(1:1).EQ.'?')THEN
            SEARCH(ISMAX)=STRING(2:NC)
       ELSE
            SEARCH(ISMAX)=STRING(1:NC)
       ENDIF
10     CONTINUE
30     CONTINUE
*** Loop over the input.
       IOLD=1
20     CONTINUE
*   Search for the topic, starting from the root.
       NPATH=1
       PATH(1)=1
       NOCCUR=0
**  Return at this point for a next item to be examined.
100    CONTINUE
**  Determine whether the item exists at all.
       CALL HLPINQ(PATH,NPATH,EXIST,NSUB,TOPIC,IREC,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! HLPINP WARNING : Inquiry for the'//
     -           ' existence of a topic failed; help ended.'
            CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   If it exists, check whether the strings match.
       IF(EXIST)THEN
            CALL WLDCRD(TOPIC,SEARCH(NPATH),.TRUE.,MATCH)
       ELSE
            MATCH=.FALSE.
       ENDIF
**  Assume the strings match ...
       IF(EXIST.AND.MATCH)THEN
*   Remember the full reference string and record reference.
            TOPTL(NPATH)=TOPIC
            IRECTL(NPATH)=IREC
*   print if we are at the end of the tree and keep track,
            IF(NPATH.EQ.ISMAX)THEN
                 NOCCUR=NOCCUR+1
                 NSUBN=NSUB
                 DO 120 I=1,ISMAX
                 TOPL(I)=TOPTL(I)
                 IRECL(I)=IRECTL(I)
120              CONTINUE
                 DO 130 I=1,ISMAX-1
                 WRITE(LUNOUT,'(1X,A)') BLANK(1:1+3*(I-1))//TOPL(I)
130              CONTINUE
                 CALL HLPPRT(IREC,1+3*(NPATH-1),IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! HLPINP WARNING : Unable to'//
     -                     ' print the subtopics; help ended.'
                      CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
                      RETURN
                 ENDIF
                 PATH(NPATH)=PATH(NPATH)+1
*   if there are subtopics, go deeper,
            ELSEIF(NSUB.GT.0)THEN
                 NPATH=NPATH+1
                 PATH(NPATH)=1
*   otherwise go further on the same level.
            ELSE
                 PATH(NPATH)=PATH(NPATH)+1
            ENDIF
**  In case the item exists but doesn't match.
       ELSEIF(EXIST)THEN
            PATH(NPATH)=PATH(NPATH)+1
**  If there is no match, return one level.
       ELSE
            NPATH=NPATH-1
            IF(NPATH.LT.ISMIN)GOTO 200
            PATH(NPATH)=PATH(NPATH)+1
       ENDIF
*   And go for the next item.
       GOTO 100
*** Take care of the subtopics.
200    CONTINUE
*   Information not found, revert to old record.
       IF(NOCCUR.EQ.0)THEN
            PRINT *,' '
            PRINT *,' The information you requested is not available.'
            PRINT *,' '
            IREC=IOLD
            ISTR=1
*   Only one occurence and subtopics for that one.
       ELSEIF(NOCCUR.EQ.1.AND.NSUBN.GT.0)THEN
            IREC=IRECL(ISMAX)
            IOLD=IREC
            ISMIN=ISMAX+1
            ISTR=2
*   Anything else: go back to the previous choice.
       ELSE
            IREC=IOLD
            ISTR=1
       ENDIF
*   Display the subtopics.
220    CONTINUE
       IF(ISTR.EQ.1)THEN
            WRITE(LUNOUT,'('' '')')
            DO 230 I=1,ISMIN-1
            WRITE(LUNOUT,'(1X,A)') BLANK(1:1+3*(I-1))//TOPL(I)
230         CONTINUE
            CALL INPPRM('Topic','ADD-PRINT')
       ELSE
            WRITE(LUNOUT,'('' '')')
            CALL OUTFMT(REAL(ISMIN),2,AUX,NCAUX,'LEFT')
            CALL INPPRM('Subtopic_'//AUX(1:NCAUX),'ADD-PRINT')
       ENDIF
       CALL HLPSUB(IREC,MAX(1,1+3*(ISMIN-2)),IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! HLPINP WARNING : Unable to list'//
     -           ' the subtopics; help ended.'
            CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   And ask which of the subtopics the user likes most.
       WRITE(LUNOUT,'('' '')')
       CALL INPGET
       CALL INPNUM(NWORD)
       CALL INPSTR(1,1,STRING,NC)
       CALL INPPRM(' ','BACK-PRINT')
       IF(NWORD.EQ.1.AND.STRING.EQ.'?'.AND.NC.EQ.1)GOTO 220
*   Put the new words on the stack.
       IF(NWORD.GE.1)THEN
            DO 240 I=1,NWORD
            IF(ISMIN+I-1.GT.MXHLEV)THEN
                 PRINT *,' !!!!!! HLPINP WARNING : Too many keywords'//
     -                ' provided, list truncated.'
                 ISMAX=MXHLEV
                 GOTO 250
            ENDIF
            CALL INPSTR(I,I,STRING,NC)
            SEARCH(ISMIN+I-1)=STRING(1:NC)
240         CONTINUE
            ISMAX=ISMIN+NWORD-1
250         CONTINUE
*   Return one level if the return is blank.
       ELSE
            ISMIN=ISMIN-1
            IF(ISMIN.LE.0)THEN
                 CALL INPPRM(' ','BACK')
                 CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
                 RETURN
            ENDIF
            IF(ISMIN.GT.1)THEN
                 IREC=IRECL(ISMIN-1)
            ELSE
                 IREC=1
            ENDIF
            IOLD=IREC
            ISTR=1
            GOTO 220
       ENDIF
*** Go back for a new input line.
       GOTO 20
*** Handle I/O problems.
2010   CONTINUE
       PRINT *,' !!!!!! HLPINP WARNING : I/O error reading the root'//
     -      ' record of the help file ; no help can ne provided.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=17,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2020   CONTINUE
       PRINT *,' !!!!!! HLPINP WARNING : Unable to open the help'//
     -      ' file ; no help can be provided.'
       CALL INPIOS(IOS)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! HLPINP WARNING : Unable to close the help'//
     -      ' file after use ; future use might be troublesome.'
       CALL INPIOS(IOS)
       END
