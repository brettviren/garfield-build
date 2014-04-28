CDECK  ID>, INPCHK.
       SUBROUTINE INPCHK(IWRD,IFMT,IFAIL)
*-----------------------------------------------------------------------
*   INPCHK - Routine checking the validity of numeric input and applying
*            corrections if necessary, before the Fortran input routines
*            are called.
*   VARIABLES : IFMT        : Expected type 0=char,1=int,2=real,3=hex
*               IEXP        : 0 If no exponent ('E') notation has been
*                             come across yet, 1 if this is the case.
*               IDOT,ISIGN  : Similar to IEXP.
*               INUM        : 0 And 1 see IEXP, 2 a blank has been seen
*                             after a number.
*   (Last changed on 27/11/10.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       CHARACTER*(MXCHAR) AUX
       CHARACTER CHAR
       LOGICAL NUMBER,HEX
       INTEGER IWRD,IFMT,IFAIL,INUM,IDOT,IEXP,ISIGN,IDELET,ICONV,I,
     -      ILAST,NUMEXP
*** Define 2 statement functions to be used to identify symbols.
       NUMBER(CHAR)=INDEX('0123456789',CHAR).NE.0
       HEX(CHAR)=INDEX('0123456789ABCDEF',CHAR).NE.0
*** Identify the subroutine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE INPCHK ///'
*** Preset IFAIL to 0, ie OK.
       IFAIL=0
*** Return without checking if IWRD is out of range.
       IF(IWRD.LE.0.OR.IWRD.GT.NWORD)THEN
            IFAIL=1
            RETURN
       ENDIF
*** Initialise ERRCDE(IWRD) and ERRPRT(IWRD).
       ERRCDE(IWRD)=' '
       ERRPRT(IWRD)=.FALSE.
*** Handle format 0 and word='*': no checks.
       IF(IFMT.EQ.0.OR.WORD(IWRD).EQ.'*'.OR.WORD(IWRD).EQ.' ')RETURN
*** Initialise the counting variables (0=not yet seen, 1=seen, 2=end).
       INUM=0
       IDOT=0
       IEXP=0
       ISIGN=0
       IDELET=0
       ICONV=0
*** Return immediately if the field is too long.
       IF(NCHAR(IWRD).GT.25)THEN
            ERRCDE(IWRD)='Word is longer than 25 chars. '
            GOTO 100
       ENDIF
*** Hexadecimal numbers.
       IF(IFMT.EQ.3)THEN
            IF(NCHAR(IWRD).GT.6)THEN
                 ERRCDE(IWRD)='Hex number longer than 6 byte.'
                 GOTO 100
            ELSE
                 DO 30 I=1,NCHAR(IWRD)
                 IF(.NOT.HEX(WORD(IWRD)(I:I)))THEN
                      ERRCDE(IWRD)='Illegal characters seen.      '
                      GOTO 100
                 ENDIF
30               CONTINUE
            ENDIF
            RETURN
       ENDIF
*** Handle the normal formats: integer(=1) and real (=2).
       I=0
20     CONTINUE
       I=I+1
       CHAR=WORD(IWRD)(I:I)
*   Remove character if IDELET is 1.
       IF(CHAR.EQ.'E'.AND.IDELET.EQ.1.AND.ICONV.EQ.1)IDELET=0
       IF(IDELET.EQ.1)THEN
            IF(CHAR.NE.' '.AND.ERRCDE(IWRD).EQ.' ')THEN
                 ERRPRT(IWRD)=.TRUE.
                 ERRCDE(IWRD)='The second number is removed. '
            ENDIF
            WORD(IWRD)(I:I)=' '
*   Set INUM to 1 if at least one number is seen, delete after a blank.
       ELSEIF(NUMBER(CHAR))THEN
            INUM=1
*   Delete from the first blank onwards.
       ELSEIF(CHAR.EQ.' ')THEN
            IF(WORD(IWRD)(:I).NE.' ')IDELET=1
*   Only one '.' is allowed, only for reals and only before the E.
       ELSEIF(CHAR.EQ.'.')THEN
            IF(IDOT.EQ.1.OR.IEXP.EQ.1)THEN
                 ERRCDE(IWRD)='Illegal use of a decimal dot. '
                 GOTO 100
            ELSEIF(IFMT.EQ.1)THEN
                 WORD(IWRD)(I:I)=' '
                 ERRCDE(IWRD)='Decimal not allowed in integer'
                 IDELET=1
                 ICONV=1
                 ERRPRT(IWRD)=.TRUE.
            ENDIF
            IDOT=1
*   Only one E is allowed (after a number), no '.' allowed anymore.
       ELSEIF(CHAR.EQ.'E')THEN
            IF(IEXP.EQ.1)THEN
                 ERRCDE(IWRD)='E has been used at least twice'
                 GOTO 100
            ELSEIF(INUM.EQ.0)THEN
                 IF(IFMT.EQ.1.AND.WORD(IWRD)(MXCHAR:MXCHAR).EQ.' ')THEN
                      IF(I.GT.1)THEN
                           AUX=WORD(IWRD)(1:I-1)//'0'//
     -                          WORD(IWRD)(I:MXCHAR-1)
                      ELSE
                           AUX='0'//WORD(IWRD)(I:MXCHAR-1)
                      ENDIF
                      WORD(IWRD)=AUX
                      I=I+1
                      ERRCDE(IWRD)='0 is required before the E.   '
                 ELSEIF(IFMT.EQ.2.AND.
     -                WORD(IWRD)(MXCHAR-1:MXCHAR).EQ.'  ')THEN
                      IF(I.GT.1)THEN
                           AUX=WORD(IWRD)(1:I-1)//'0.'//
     -                          WORD(IWRD)(I:MXCHAR-2)
                      ELSE
                           AUX='0.'//WORD(IWRD)(I:MXCHAR-2)
                      ENDIF
                      WORD(IWRD)=AUX
                      I=I+2
                      ERRCDE(IWRD)='0. is required before the E.  '
                 ELSE
                      ERRCDE(IWRD)='E is not preceded by a number.'
                      GOTO 100
                 ENDIF
            ELSEIF(IFMT.EQ.2.AND.IDOT.EQ.0)THEN
                 IF(WORD(IWRD)(MXCHAR:MXCHAR).EQ.' '
     -                .AND.I.GE.2.AND.I.LT.MXCHAR)THEN
                      IF(I.GT.1)THEN
                           AUX=WORD(IWRD)(1:I-1)//'.'//
     -                          WORD(IWRD)(I:MXCHAR-1)
                      ELSE
                           AUX='.'//WORD(IWRD)(I:MXCHAR-1)
                      ENDIF
                      WORD(IWRD)=AUX
                      ERRCDE(IWRD)='Decimal dot required for reals'
                      I=I+1
                 ELSE
                      ERRCDE(IWRD)='Unable to insert a dot.       '
                      GOTO 100
                 ENDIF
            ENDIF
            IEXP=1
            IDOT=1
            ISIGN=0
            INUM=0
*   Accept only one sign before and one after E and before numbers.
       ELSEIF(CHAR.EQ.'+'.OR.CHAR.EQ.'-')THEN
            IF(INUM.EQ.1.OR.ISIGN.EQ.1.OR.(IDOT.EQ.1.AND.IEXP.EQ.0))THEN
                 ERRCDE(IWRD)='Illegal use of a + or - sign. '
                 GOTO 100
            ENDIF
            ISIGN=1
*   Check that character is legal, remove if not.
       ELSE
            IF(IEXP.EQ.0.AND.INUM.EQ.0.AND.IDOT.EQ.0.AND.ISIGN.EQ.0)THEN
                 WORD(IWRD)(I:I)=' '
                 ERRCDE(IWRD)='Illegal character(s) removed. '
                 ERRPRT(IWRD)=.TRUE.
            ELSE
                 ERRCDE(IWRD)='Illegal character "'//CHAR//'" found.  '
                 GOTO 100
            ENDIF
       ENDIF
       IF(I.LT.MXCHAR)GOTO 20
*** Stop if line is blank after correction.
       IF(WORD(IWRD).EQ.' ')GOTO 100
*** Make some additional checks on numbers with an E.
       IF(IEXP.EQ.1.AND.INUM.EQ.0)THEN
            WORD(IWRD)(INDEX(WORD(IWRD),'E'):)=' '
            ERRCDE(IWRD)='No exponential sign is needed.'
            IEXP=0
            ISIGN=0
*   In case there is an E, make sure the exponent is not too large.
       ELSEIF(IEXP.EQ.1)THEN
            AUX=WORD(IWRD)(INDEX(WORD(IWRD),'E'):)
            AUX(1:1)=' '
            READ(AUX,'(BN,I10)') NUMEXP
            IF(ABS(NUMEXP).GT.30)THEN
                 ERRCDE(IWRD)='Exponent is out of range.     '
                 IF(LINUND)THEN
                      WORD(IWRD)='0.0'
                      IEXP=0
                      ISIGN=0
                 ELSE
                      GOTO 100
                 ENDIF
            ENDIF
       ENDIF
*** Add zeros in numbers with a sign without number.
       IF(IEXP.EQ.0.AND.ISIGN.EQ.1.AND.INUM.EQ.0)THEN
            IF(IFMT.EQ.1)WORD(IWRD)='0'
            IF(IFMT.EQ.2)WORD(IWRD)='0.0'
            ERRCDE(IWRD)='Only a + or a - sign was found.    '
*** Supplement a dot (if not yet present) to a real without an E.
       ELSEIF(IFMT.EQ.2.AND.IEXP.EQ.0.AND.IDOT.EQ.0)THEN
            ILAST=0
            INUM=0
            DO 40 I=1,MXCHAR
            IF(NUMBER(WORD(IWRD)(I:I)))THEN
                 IF(INUM.EQ.0)INUM=1
            ELSE
                 IF(INUM.EQ.1)THEN
                      INUM=2
                      ILAST=I
                 ENDIF
            ENDIF
40          CONTINUE
            IF(INUM.NE.2)THEN
                 ERRCDE(IWRD)='Unable to insert a dot (no E).'
                 GOTO 100
            ELSE
                 WORD(IWRD)(ILAST:ILAST)='.'
                 ERRCDE(IWRD)='Decimal dot required for reals'
            ENDIF
       ENDIF
       GOTO 110
*** Case of irrepairable syntax errors.
100    CONTINUE
       ERRPRT(IWRD)=.TRUE.
       WORD(IWRD)='*'
       NCHAR(IWRD)=1
       IFAIL=1
*** Remove blanks and count the number of characters again.
110    CONTINUE
       NCHAR(IWRD)=0
       DO 120 I=1,MXCHAR
       IF(WORD(IWRD)(I:I).NE.' ')THEN
            NCHAR(IWRD)=NCHAR(IWRD)+1
            WORD(IWRD)(NCHAR(IWRD):NCHAR(IWRD))=WORD(IWRD)(I:I)
       ENDIF
120    CONTINUE
       IF(NCHAR(IWRD).LT.MXCHAR)WORD(IWRD)(NCHAR(IWRD)+1:)=' '
       END
