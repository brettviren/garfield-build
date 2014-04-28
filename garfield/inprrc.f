CDECK  ID>, INPRRC.
       SUBROUTINE INPRRC(INSTR,VAL,DEF,IFAIL)
*-----------------------------------------------------------------------
*   INPRRC - Checks that INSTR contains one real, reads it into VAL
*            taking DEF instead if necessary and returns IFAIL=1
*            if serious errors were detected by INPCHK.
*   (Last changed on 23/ 8/95.)
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
       CHARACTER*(*)      INSTR
       CHARACTER*(MXWORD) WRDRES
       CHARACTER*30       ECDRES
       LOGICAL            ERRRES
       REAL               VAL,DEF
       INTEGER            IFAIL,NCHRES,NWRRES
*** First store all data on word 1 and remember the number of words.
       WRDRES=WORD(1)
       ECDRES=ERRCDE(1)
       ERRRES=ERRPRT(1)
       NCHRES=NCHAR(1)
       NWRRES=NWORD
*** Store the word to be checked in word 1.
       NWORD=1
       WORD(1)=INSTR
       ERRPRT(1)=.FALSE.
       ERRCDE(1)=' '
       NCHAR(1)=LEN(INSTR)
*** Check the word and read it.
       CALL INPCHK(1,2,IFAIL)
       CALL INPRDR(1,VAL,DEF)
*** Print the error message, if any.
       IF(ERRPRT(1))THEN
            PRINT *,' !!!!!! INPRRC WARNING : ',INSTR,
     -           ' was changed into '//WORD(1)(1:NCHAR(1))
            PRINT *,'                         Reason: '//ERRCDE(1)
            PRINT *,'                         Value assigned : ',VAL
       ENDIF
*** Restore the old word 1 in its place.
       WORD(1)  =WRDRES
       ERRCDE(1)=ECDRES
       ERRPRT(1)=ERRRES
       NCHAR(1) =NCHRES
       NWORD    =NWRRES
       END
