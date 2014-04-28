CDECK  ID>, INPERR.
       SUBROUTINE INPERR
*-----------------------------------------------------------------------
*   INPERR - Prints the errors detected by INPCHK in a compact manner.
*   (Last changed on 20/ 5/99.)
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
       LOGICAL OK
       CHARACTER*(MXINCH+1) MARK
       INTEGER LASTCH(MXWORD),I,J,IORIG,IPART,JSTART
       IF(LIDENT)PRINT *,' /// ROUTINE INPERR ///'
*** Find out whether something is wrong or not and preset the mark line.
       MARK=' '
       OK=.TRUE.
       DO 10 I=1,NWORD
       IF(ERRPRT(I))OK=.FALSE.
       IF(ERRCDE(I).NE.' ')THEN
            IF(ERRPRT(I))MARK(INDWRD(I):INDWRD(I))='#'
            IF(.NOT.ERRPRT(I))MARK(INDWRD(I):INDWRD(I))='!'
       ENDIF
10     CONTINUE
*** Return at this point if there are no error messages.
       IF(OK)RETURN
*   Otherwise print a heading for the messages.
       PRINT *,' !!!!!! INPERR WARNING : The words marked # and !'//
     -      ' have been changed:'
*** Find out where each string ends.
       DO 20 I=1,NWORD
*   Starting point of the search.
       IF(I.EQ.NWORD)THEN
            JSTART=MXCHAR
       ELSE
            JSTART=INDWRD(I+1)-1
       ENDIF
*   Search for last non-blank character of the string.
       DO 30 J=JSTART,INDWRD(I),-1
       IF(STRING(J:J).NE.' ')THEN
            LASTCH(I)=J
            GOTO 40
       ENDIF
30     CONTINUE
       LASTCH(I)=INDWRD(I)
40     CONTINUE
20     CONTINUE
*   Add as many words as will fit without spilling to next line.
       IORIG=1
       IPART=0
       DO 50 I=1,NWORD
       IF(I.NE.NWORD)THEN
            IF(LASTCH(I+1)-INDWRD(IORIG)+25.LE.75)GOTO 50
       ENDIF
       IF(IORIG.EQ.1.AND.I.EQ.NWORD)THEN
            PRINT *,'        Original input : '//
     -           STRING(INDWRD(IORIG):LASTCH(I))
       ELSE
            IPART=IPART+1
            WRITE(*,'(/''         Input part '',I3,'' : '',A)') IPART,
     -           STRING(INDWRD(IORIG):LASTCH(I))
       ENDIF
       PRINT *,'        Modified words : '//
     -      MARK(INDWRD(IORIG):LASTCH(I))
       DO 60 J=IORIG,I
       IF(ERRCDE(J).NE.' '.AND.WORD(J)(1:NCHAR(J)).EQ.'*DELETED*')THEN
            PRINT *,'        Deleted, reason: '//ERRCDE(J)
       ELSEIF(ERRCDE(J).NE.' ')THEN
            PRINT *,'        Changed into "'//WORD(J)(1:NCHAR(J))//
     -           '", reason: '//ERRCDE(J)
       ENDIF
60     CONTINUE
       IORIG=I+1
50     CONTINUE
*** End of the printout.
       PRINT *,' '
       END
