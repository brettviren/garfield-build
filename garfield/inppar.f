CDECK  ID>, INPPAR.
       SUBROUTINE INPPAR(IFAIL)
*-----------------------------------------------------------------------
*   INPPAR - Imitates the Parse instruction from REXX by assigning bits
*            of a string to global variables.
*   (Last changed on  9/11/00.)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       EXTERNAL INPCMP
       INTEGER INPCMP,IFAIL,NCFMT,NCLINE,NCVAR,MODRES(MXVAR),IGLB,
     -      IFAIL1,NRES,IENTRY,I,ITYPE
       CHARACTER*10 VARNAM
       CHARACTER*(MXINCH) FORMAT,LINE
       LOGICAL USE(MXVAR),EXEC
       REAL RES(1)
*** Identify the routine for tracing purposes.
       IF(LIDENT)PRINT *,' /// ROUTINE INPPAR ///'
*** Assume that things will work out correctly.
       IFAIL=0
*** Assume we are in non-execution mode.
       EXEC=.FALSE.
*** Check for the EVALUATE and LITERAL options.
       IF(INPCMP(2,'EVAL#UATE')+INPCMP(2,'EXEC#UTE').NE.0)THEN
            EXEC=.TRUE.
            ITYPE=3
       ELSEIF(INPCMP(2,'LIT#ERALLY')+INPCMP(2,'NOEVAL#UATE')+
     -      INPCMP(2,'NOEXEC#UTE').NE.0)THEN
            EXEC=.FALSE.
            ITYPE=3
       ELSE
            ITYPE=2
       ENDIF
*** Get the number of words.
       IF(NWORD.LT.ITYPE)RETURN
*** Input is a global variable.
       IF(INPCMP(ITYPE,'GL#OBAL').NE.0)THEN
*   Check that there are enough arguments.
            IF(NWORD.LT.ITYPE+2)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Parse Global needs'//
     -                ' at least a global name and a template; ignored.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Locate the global variable.
            CALL INPSTR(ITYPE+1,ITYPE+1,VARNAM,NCVAR)
            IGLB=0
            DO 10 I=1,NGLB
            IF(VARNAM(1:NCVAR).EQ.GLBVAR(I))IGLB=I
10          CONTINUE
            IF(IGLB.EQ.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : The global'//
     -                ' variable '//VARNAM(1:NCVAR)//' is not'//
     -                ' known; Parse Global ignored.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Get the global variable.
            CALL OUTFMT(GLBVAL(IGLB),GLBMOD(IGLB),LINE,NCLINE,'LEFT')
*   And get the template.
            CALL INPSTR(ITYPE+2,NWORD,FORMAT,NCFMT)
*   Assign the globals.
            CALL INPTMP(LINE,NCLINE,FORMAT,NCFMT,EXEC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Error detected'//
     -                ' in Parse Global for '//VARNAM(1:NCVAR)//'.'
                 IFAIL=1
                 RETURN
            ENDIF
*** Input is from regular input.
       ELSEIF(INPCMP(ITYPE,'IN#PUT').NE.0)THEN
*   Check that there are enough arguments.
            IF(NWORD.LT.ITYPE+1)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Parse Input needs'//
     -                ' at least a template; ignored.'
                 IFAIL=1
                 RETURN
            ENDIF
*   And get the template.
            CALL INPSTR(ITYPE+1,NWORD,FORMAT,NCFMT)
*   Set a prompt.
            CALL INPPRM('Input','ADD-PRINT')
*   Get an input line.
            CALL INPGET
            CALL INPSTR(1,NWORD,LINE,NCLINE)
*   Remove prompt.
            CALL INPPRM(' ','BACK-PRINT')
*   Assign the globals.
            CALL INPTMP(LINE,NCLINE,FORMAT,NCFMT,EXEC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Error detected'//
     -                ' in Parse Input.'
                 IFAIL=1
                 RETURN
            ENDIF
*** Input file argument.
       ELSEIF(INPCMP(ITYPE,'ARG#UMENT').NE.0)THEN
*   Check that there are enough arguments.
            IF(NWORD.LT.ITYPE+1)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Parse Argument'//
     -                ' needs at least a template; ignored.'
                 IFAIL=1
                 RETURN
            ENDIF
*   And get the template.
            CALL INPSTR(ITYPE+1,NWORD,FORMAT,NCFMT)
*   Assign the globals.
            CALL INPTMP(ARGSTR,NCARG,FORMAT,NCFMT,EXEC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Error detected'//
     -                ' in Parse Argument.'
                 IFAIL=1
                 RETURN
            ENDIF
*** Input is from terminal input.
       ELSEIF(INPCMP(ITYPE,'TERM#INAL').NE.0)THEN
*   Check that there are enough arguments.
            IF(NWORD.LT.ITYPE+1)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Parse Terminal'//
     -                ' needs at least a template; ignored.'
                 IFAIL=1
                 RETURN
            ENDIF
*   And get the template.
            CALL INPSTR(ITYPE+1,NWORD,FORMAT,NCFMT)
*   Switch to terminal input.
            CALL INPSWI('TERMINAL')
*   Set a prompt.
            CALL INPPRM('Input','ADD-PRINT')
*   Get an input line.
            CALL INPGET
            CALL INPSTR(1,NWORD,LINE,NCLINE)
*   Remove prompt.
            CALL INPPRM(' ','BACK-PRINT')
*   Return to regular input.
            CALL INPSWI('RESTORE')
*   Assign the globals.
            CALL INPTMP(LINE,NCLINE,FORMAT,NCFMT,EXEC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Error detected'//
     -                ' in Parse Terminal.'
                 IFAIL=1
                 RETURN
            ENDIF
*** Input from the result of some calculation.
       ELSEIF(INPCMP(ITYPE,'VAL#UE').NE.0)THEN
*   Check that there are enough arguments.
            IF(NWORD.LT.ITYPE+2)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Parse Value needs'//
     -                ' at least a global name and a template; ignored.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Get the expression.
            CALL INPSTR(ITYPE+1,ITYPE+1,LINE,NCLINE)
*   Translate the expression.
            CALL ALGPRE(LINE(1:NCLINE),NCLINE,
     -           GLBVAR,NGLB,NRES,USE,IENTRY,IFAIL1)
*   Make sure that the formula was OK.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Translation'//
     -                ' of expression '//LINE(1:NCLINE)//
     -                ' failed; Parse Value ignored.'
                 IFAIL=1
                 CALL ALGCLR(IENTRY)
                 RETURN
*   Verify that we get indeed only one result.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Translation'//
     -                ' of expression '//LINE(1:NCLINE)//
     -                ' does not yield 1 result; Parse Value ignored.'
                 CALL ALGCLR(IENTRY)
                 IFAIL=1
                 RETURN
            ENDIF
*   Set the execution time.
            CALL TIMEL(GLBVAL(1))
*   Evaluate the formula.
            CALL ALGEXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,1,IFAIL1)
*   Check the return code of the evaluation.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPTMP WARNING : Evaluation of'//
     -                ' expression '//LINE(1:NCLINE)//
     -                ' failed; Parse Value ignored.'
                 CALL ALGCLR(IENTRY)
                 IFAIL=1
                 RETURN
            ENDIF
*   Print any evaluation errors.
            CALL ALGERR
*   Remove the entry point of the formula.
            CALL ALGCLR(IENTRY)
*   Assign the result to the string.
            CALL OUTFMT(RES(1),MODRES(1),LINE,NCLINE,'LEFT')
*   And get the template.
            CALL INPSTR(ITYPE+2,NWORD,FORMAT,NCFMT)
*   Assign the globals.
            CALL INPTMP(LINE,NCLINE,FORMAT,NCFMT,EXEC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPPAR WARNING : Error detected'//
     -                ' in Parse Value.'
                 IFAIL=1
                 RETURN
            ENDIF
*** Other sources.
       ELSE
            CALL INPSTR(ITYPE,ITYPE,LINE,NCLINE)
            PRINT *,' !!!!!! INPPAR WARNING : '//LINE(1:NCLINE)//
     -           ' is not a known source for Parse; ignored.'
            IFAIL=1
            RETURN
       ENDIF
       END
