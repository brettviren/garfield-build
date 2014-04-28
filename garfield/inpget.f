CDECK  ID>, INPGET.
       SUBROUTINE INPGET
*-----------------------------------------------------------------------
*   INPGET - This routine reads a line from unit LUN (without checking
*            that it is opened). It isolates the words.
*   VARIABLES : SQUOTE      : Becomes TRUE when a single quote has been
*                             met (separators are ignored inside quotes)
*               DQUOTE      : Similar to SQUOTE, but for double quotes
*               BQUOTE      : Similar to SQUOTE, but for reverse quotes
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       CHARACTER*(MXNAME) FILE
       INTEGER I,I0,I1,I0STR,NCSTR,IFLAG,IFIRST,IOS,IFAIL,NC,IC
       LOGICAL SQUOTE,DQUOTE,BQUOTE,BRACK,DQINBR,BQINBR,KPCASE,
     -      STDSTR,REREAD,ACT1,ACT2
       EXTERNAL STDSTR
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE INPGET ///'
*** Initialise the number of words, the quote logicals, the error codes
30     CONTINUE
       NWORD=0
       DO 50 I=1,MXWORD
       ERRPRT(I)=.FALSE.
       ERRCDE(I)=' '
       WORD(I)=' '
       NCHAR(I)=1
50     CONTINUE
*** Read a line from the DO buffer, if available.
       IF(DOEXEC)THEN
*   Fetch the line.
            CALL INPXDO(STRING,NCSTR,IFLAG)
*   Error in the DO loop execution routine.
            IF(IFLAG.LT.0)THEN
                 PRINT *,' ------ INPGET MESSAGE : Resuming input'//
     -                ' from normal stream after DO execution error.'
                 DOEXEC=.FALSE.
*   End of loop reached without error.
            ELSEIF(IFLAG.EQ.+2)THEN
                 DOEXEC=.FALSE.
            ENDIF
*   Line didn't come from the buffer.
       ELSE
            IFLAG=0
       ENDIF
*** Read a line from normal input, disable condition handling.
       IF(.NOT.DOEXEC)THEN
*   Initial settings.
            STRING=' '
            IFIRST=1
*   Return here for more string portions.
110         CONTINUE
*   Adjust prompt for multiple sections.
            IF(IFIRST.NE.1)CALL INPPRM('More ...','ADD')
*   Synchronisation prompt.
            IF(LSYNCH.AND.LUN.EQ.5.AND.STDSTR('INPUT').AND.
     -           NCPROM.GE.1)THEN
                 WRITE(6,'(''  >>>>>> input '',A)') PROMPT(1:NCPROM)
*   Display the prompt in underlined, fat mode (VT100 escape sequence).
            ELSEIF(LUN.EQ.5.AND.STDSTR('INPUT').AND.NCPROM.GE.1)THEN
                 WRITE(6,'(''  '',A,'': '',$)')
     -           CHAR(27)//CHAR(91)//CHAR(49)//CHAR(109)//
     -           CHAR(27)//CHAR(91)//CHAR(52)//CHAR(109)//
     -           PROMPT(1:NCPROM)//CHAR(27)//CHAR(91)//CHAR(109)
*   Display the prompt by appending it to the READY string.
            ELSEIF((LUN.EQ.5).AND.STDSTR('INPUT').AND.
     -           (NCPROM.GE.1).AND.LPROM)THEN
                 WRITE(6,'(''  Ready ('',A,'')'')') PROMPT(1:NCPROM)
            ENDIF
*   Restablish the prompt.
            IF(IFIRST.NE.1)CALL INPPRM(' ','BACK')
*   Read a portion of the line.
            IF(IFIRST.GE.MXINCH)THEN
                 PRINT *,' !!!!!! INPGET WARNING : No room for more'//
     -                ' input characters.'
                 GOTO 130
            ELSE
                 READ(LUN,'(A)',END=2000,IOSTAT=IOS,ERR=2010)
     -                STRING(IFIRST:MXINCH)
*   Input translation.
                 CALL INPTRA(STRING(IFIRST:MXINCH),MXINCH-IFIRST+1)
*   Write out to the recording file if requested and appropriate.
                 IF(LUN.EQ.5.AND.LINREC)THEN
                      DO 150 I=MXINCH,IFIRST,-1
                      IF(STRING(I:I).NE.' ')THEN
                           WRITE(18,'(A)',IOSTAT=IOS,ERR=2020)
     -                          STRING(IFIRST:MIN(132+IFIRST,I))
                           GOTO 160
                      ENDIF
150                   CONTINUE
                      WRITE(18,'('' '')',IOSTAT=IOS,ERR=2020)
160                   CONTINUE
                 ENDIF
            ENDIF
*   Remove any part of the string beyond //
            IF(INDEX(STRING,'//').NE.0)STRING(INDEX(STRING,'//'):)=' '
*   Print the string if requested and determine whether to continue.
            DO 120 I=MXINCH-2,IFIRST,-1
            IF(STRING(I:I+2).EQ.'...')THEN
                 IF(LINPUT)PRINT *,' ====== INPGET INPUT   : '//
     -                STRING(IFIRST:I+2)
                 IFIRST=I
                 GOTO 110
            ELSEIF(STRING(I:I+2).NE.'   '.AND.STRING(I:I+2).NE.'.  '
     -           .AND.STRING(I:I+2).NE.'.. ')THEN
                 IF(LINPUT)PRINT *,' ====== INPGET INPUT   : '//
     -                STRING(IFIRST:I+2)
                 GOTO 130
            ENDIF
120         CONTINUE
130         CONTINUE
*   Check the EOF label.
            IF(STRING.EQ.EOFSTR.AND.EOFSTR.NE.'EOF')GOTO 2000
       ENDIF
*   Determine the length of the string.
       NCSTR=1
       I0STR=1
       DO 140 I=MXINCH,1,-1
       IF(STRING(I:I).NE.' ')THEN
            IF(NCSTR.EQ.1)NCSTR=I
            I0STR=I
       ENDIF
140    CONTINUE
*** Change lower case characters to upper case, except for $ lines.
       IF(INDEX('$><',STRING(I0STR:I0STR)).NE.0)THEN
            KPCASE=.TRUE.
       ELSE
            KPCASE=.FALSE.
       ENDIF
       DQUOTE=.FALSE.
       BQUOTE=.FALSE.
       BRACK=.FALSE.
       DQINBR=.FALSE.
       BQINBR=.FALSE.
       DO 40 I=1,NCSTR
*   Keep track of double quotes and curly brackets.
       IF(I.EQ.1.OR.STRING(MAX(1,I-1):MAX(1,I-1)).NE.ESCAPE)THEN
            IF(STRING(I:I).EQ.'"')DQUOTE=.NOT.DQUOTE
            IF(STRING(I:I).EQ.'`')BQUOTE=.NOT.BQUOTE
            IF(BRACK.AND.STRING(I:I).EQ.'"')DQINBR=.NOT.DQINBR
            IF(BRACK.AND.STRING(I:I).EQ.'`')BQINBR=.NOT.BQINBR
            IF(STRING(I:I).EQ.'{')BRACK=.TRUE.
            IF(STRING(I:I).EQ.'{')DQINBR=.FALSE.
            IF(STRING(I:I).EQ.'{')BQINBR=.FALSE.
            IF(STRING(I:I).EQ.'}')BRACK=.FALSE.
            IF(STRING(I:I).EQ.'}')DQINBR=.FALSE.
            IF(STRING(I:I).EQ.'}')BQINBR=.FALSE.
       ENDIF
*   Do not change case inside quotes but change inside brackets but ...
       IF(DQUOTE.AND.(((.NOT.BRACK).AND.(.NOT.DQINBR)).OR.
     -      (BRACK.AND.DQINBR)))GOTO 40
       IF(BQUOTE.AND.(((.NOT.BRACK).AND.(.NOT.BQINBR)).OR.
     -      (BRACK.AND.BQINBR)))GOTO 40
*   Do not change special commands, except in brackets and quotes.
       IF(KPCASE.AND..NOT.(BRACK.OR.BQINBR.OR.DQINBR))GOTO 40
*   Loop up character sequence number.
       IC=ICHAR(STRING(I:I))
*   ASCII: all letters are contiguous and located between 97 and 122.
       IF(ICHSET.EQ.1.AND.IC.LE.122.AND.IC.GE.97)THEN
            STRING(I:I)=CHAR(IC-32)
*   EBCDIC: there are 2 gaps in the set (idea from IBM of course).
       ELSEIF(ICHSET.EQ.2.AND.((IC.GE.129.AND.IC.LE.137).OR.
     -      (IC.GE.145.AND.IC.LE.153).OR.(IC.GE.162.AND.IC.LE.169)))THEN
            STRING(I:I)=CHAR(IC+64)
       ENDIF
40     CONTINUE
*** Perform substitutions.
       IF((.NOT.DOREAD).AND.(STRING(I0STR:I0STR).NE.'*'))THEN
            CALL INPIFQ(ACT1,ACT2)
            IF(ACT2.OR.(ACT1.AND.
     -           STRING(I0STR:MIN(I0STR+6,NCSTR)).EQ.'ELSEIF '))
     -           CALL INPSUB(STRING,NCSTR,IFAIL)
       ENDIF
*** Get rid of escape characters.
       CALL INPESC(STRING,NCSTR,IFAIL)
*** Split the string in pieces.
       SQUOTE=.FALSE.
       DQUOTE=.FALSE.
       BQUOTE=.FALSE.
*   Locate start of next word.
       I0=0
10     CONTINUE
       I0=I0+1
       IF(I0.GT.NCSTR)GOTO 100
*   If first character is a quote, set flags accordingly.
       IF(STRING(I0:I0).EQ.'''')THEN
            SQUOTE=.TRUE.
       ELSE
            SQUOTE=.FALSE.
       ENDIF
       IF(STRING(I0:I0).EQ.'"')THEN
            DQUOTE=.TRUE.
       ELSE
            DQUOTE=.FALSE.
       ENDIF
       IF(STRING(I0:I0).EQ.'`')THEN
            BQUOTE=.TRUE.
       ELSE
            BQUOTE=.FALSE.
       ENDIF
*   Proceed with next character if STRING(I0:I0) is a separator.
       IF(INDEX(' ,=',STRING(I0:I0)).NE.0)GOTO 10
*   Scan for the end of the word
       DO 20 I1=I0+1,NCSTR+1
       IF(I1.NE.NCSTR+1.AND.
     -      (INDEX('''"` ,=:',STRING(I1:I1)).EQ.0.OR.
     -      ((DQUOTE.OR.SQUOTE.OR.BQUOTE).AND.
     -           INDEX(' ,=:',STRING(I1:I1)).NE.0).OR.
     -      (STRING(I1:I1).EQ.''''.AND.(DQUOTE.OR.BQUOTE)).OR.
     -      (STRING(I1:I1).EQ.'`'.AND.(DQUOTE.OR.SQUOTE)).OR.
     -      (STRING(I1:I1).EQ.'"'.AND.(SQUOTE.OR.BQUOTE))))GOTO 20
*   Check that the string ends on a quote
       IF((SQUOTE.AND.STRING(I1:I1).NE.'''').OR.
     -      (DQUOTE.AND.STRING(I1:I1).NE.'"').OR.
     -      (BQUOTE.AND.STRING(I1:I1).NE.'`'))
     -      PRINT *,' !!!!!! INPGET WARNING : A quote is missing in'//
     -      ' the line ; assuming one at the end.'
*   Make sure that the maximum number of words is not exceeded
       IF(NWORD+1.GT.MXWORD)THEN
            PRINT *,' !!!!!! INPGET WARNING : The number of keywords'//
     -              ' exceeds MXWORD (=',MXWORD,') ; rest is ignored.'
            GOTO 100
       ENDIF
       NWORD=NWORD+1
*   Store word together with its length and the index of first character
       IF(INDEX('''"',STRING(I0:I0)).NE.0)THEN
            IF(I0.EQ.I1-1)THEN
                 WORD(NWORD)=' '
                 NCHAR(NWORD)=0
            ELSE
                 WORD(NWORD)=STRING(I0+1:I1-1)
                 NCHAR(NWORD)=MIN(MXCHAR,I1-I0-1)
            ENDIF
            INDWRD(NWORD)=I0+1
            IF(I1-I0-1.GT.MXCHAR)PRINT *,' !!!!!! INPGET WARNING : "'//
     -           STRING(I0+1:I1-1)//'" is truncated to "'//
     -           WORD(NWORD)//'" (MXCHAR characters).'
       ELSEIF(STRING(I0:I0).EQ.'`')THEN
            WORD(NWORD)=STRING(I0:I1)
            NCHAR(NWORD)=MIN(MXCHAR,I1-I0+1)
            IF(I1-I0+1.GT.MXCHAR)PRINT *,' !!!!!! INPGET WARNING : "'//
     -           STRING(I0:I1)//'" is truncated to "'//
     -           WORD(NWORD)//'" (MXCHAR characters).'
            INDWRD(NWORD)=I0
       ELSE
            WORD(NWORD)=STRING(I0:I1-1)
            NCHAR(NWORD)=MIN(MXCHAR,I1-I0)
            IF(I1-I0.GT.MXCHAR)PRINT *,' !!!!!! INPGET WARNING : "'//
     -           STRING(I0:I1-1)//'" is truncated to "'//
     -           WORD(NWORD)//'" (MXCHAR characters).'
            INDWRD(NWORD)=I0
       ENDIF
*   Continue with the next word.
       IF((STRING(I1:I1).EQ.''''.AND..NOT.SQUOTE).OR.
     -      (STRING(I1:I1).EQ.'"'.AND..NOT.DQUOTE).OR.
     -      (STRING(I1:I1).EQ.'`'.AND..NOT.BQUOTE))THEN
            I0=I1-1
       ELSE
            I0=I1
       ENDIF
       GOTO 10
20     CONTINUE
100    CONTINUE
*   Care for the empty string case.
       IF(NWORD.EQ.0)THEN
            WORD(1)=' '
            NCHAR(1)=1
       ENDIF
*** Print the list of words if the debug option is on.
       IF(LDEBUG)THEN
            IF(NWORD.EQ.0)THEN
                 WRITE(LUNOUT,'(1X,A)')
     -                ' ++++++ INPGET DEBUG   : Empty input string.'
            ELSE
                 WRITE(LUNOUT,'(1X,A)') ' ++++++ INPGET DEBUG   :'//
     -                '    Word Length  Start  Text'
                 DO 200 I=1,NWORD
                 WRITE(LUNOUT,'(26X,3I7,2X,A)')
     -                I,NCHAR(I),INDWRD(I),
     -                WORD(I)(1:MAX(1,MIN(MXCHAR,NCHAR(I))))
200              CONTINUE
                 WRITE(LUNOUT,'('' '')')
            ENDIF
       ENDIF
*** Input line started with an IF clause.
       IF(IFLAG.EQ.+1)THEN
            DO 210 I=1,NWORD
            IF(WORD(1).EQ.'THEN')THEN
                 CALL INPDEL(1)
                 GOTO 220
            ELSE
                 CALL INPDEL(1)
            ENDIF
210         CONTINUE
220         CONTINUE
       ENDIF
*** Check the IF condition outside the DO loops.
       IF((.NOT.DOREAD).AND.(.NOT.DOEXEC))THEN
            CALL INPIFT(REREAD,IFAIL)
            IF(REREAD)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(1X,A)')
     -                ' ++++++ INPGET DEBUG   : Line is skipped.'
                 GOTO 30
            ENDIF
       ENDIF
*** Normal end of this routine.
       RETURN
*** Handle I/O problems, first EOF on standard input.
2000   CONTINUE
       IF(LUN.EQ.5)THEN
            PRINT *,' ------ INPGET MESSAGE : EOF on standard'//
     -          ' input ; end of program execution.'
            CALL QUIT
*   Next, EOF on switched input.
       ELSEIF(LUN.EQ.12)THEN
            NWORD=0
            RETURN
*   And finally EOF on alternate input.
       ELSE
            CALL STRBUF('READ',LUNSTR(LUN,1),FILE,NC,IFAIL)
            PRINT *,' ------ INPGET MESSAGE : End of file reached on '//
     -           FILE(1:NC)//','
            CLOSE(UNIT=LUN,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            CALL STRBUF('DELETE',LUNSTR(LUN,1),FILE,NC,IFAIL)
            CALL STRBUF('DELETE',LUNSTR(LUN,2),EOFSTR,NCEOF,IFAIL)
            CALL STRBUF('DELETE',LUNSTR(LUN,3),ARGSTR,NCARG,IFAIL)
            IF(LUN.EQ.20)LUN=5
            IF(LUN.GT.20)LUN=LUN-1
            CALL STRBUF('READ',LUNSTR(LUN,1),FILE,NC,IFAIL)
            CALL STRBUF('READ',LUNSTR(LUN,2),EOFSTR,NCEOF,IFAIL)
            CALL STRBUF('READ',LUNSTR(LUN,3),ARGSTR,NCARG,IFAIL)
            PRINT *,'                         input will continue'//
     -           ' from '//FILE(1:NC)//' until '//EOFSTR(1:NCEOF)//'.'
            GLBVAL(6)=LUNSTR(LUN,1)
            GOTO 30
       ENDIF
*** I/O error reading the input, stop if on unit 5, else close.
2010   CONTINUE
       CALL STRBUF('READ',LUNSTR(LUN,1),FILE,NC,IFAIL)
       PRINT *,' ###### INPGET ERROR   : I/O error detected on '//
     -      FILE(1:NC)//','
       CALL INPIOS(IOS)
       IF(LUN.NE.5)THEN
            CALL STRBUF('DELETE',LUNSTR(LUN,1),FILE,NC,IFAIL)
            CALL STRBUF('DELETE',LUNSTR(LUN,2),EOFSTR,NCEOF,IFAIL)
            CALL STRBUF('DELETE',LUNSTR(LUN,3),ARGSTR,NCARG,IFAIL)
            CLOSE(UNIT=LUN,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            IF(LUN.EQ.20)LUN=5
            IF(LUN.GT.20)LUN=LUN-1
            CALL STRBUF('READ',LUNSTR(LUN,1),FILE,NC,IFAIL)
            CALL STRBUF('READ',LUNSTR(LUN,2),EOFSTR,NCEOF,IFAIL)
            CALL STRBUF('READ',LUNSTR(LUN,3),ARGSTR,NCARG,IFAIL)
            PRINT *,'                         file closed, reading'//
     -           ' from '//FILE(1:NC)//' until '//EOFSTR(1:NCEOF)//'.'
            GLBVAL(6)=LUNSTR(LUN,1)
            GOTO 30
       ELSE
            PRINT *,'                         end of program execution.'
            CALL QUIT
       ENDIF
*** Recording errors.
2020   CONTINUE
       PRINT *,' ###### INPGET ERROR   : Error while recording input'//
     -      ' statements; recording stopped.'
       LINREC=.FALSE.
       CALL INPIOS(IOS)
       GOTO 30
*** Error closing an alternate input file.
2030   CONTINUE
       CALL STRBUF('READ',LUNSTR(LUN,1),FILE,NC,IFAIL)
       PRINT *,' ###### INPGET ERROR   : Unable to close '//FILE(1:NC)//
     -      ' ; further alternative input may cause problems.'
       CALL STRBUF('DELETE',LUNSTR(LUN,1),FILE,NC,IFAIL)
       CALL STRBUF('DELETE',LUNSTR(LUN,2),EOFSTR,NCEOF,IFAIL)
       CALL STRBUF('DELETE',LUNSTR(LUN,3),ARGSTR,NCARG,IFAIL)
       CALL INPIOS(IOS)
       IF(LUN.EQ.20)LUN=5
       IF(LUN.GT.20)LUN=LUN-1
       GLBVAL(6)=LUNSTR(LUN,1)
       GOTO 30
       END
