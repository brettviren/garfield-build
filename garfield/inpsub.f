CDECK  ID>, INPSUB.
       SUBROUTINE INPSUB(STR,NC,IFAIL)
*-----------------------------------------------------------------------
*   INPSUB - Evaluates global variables and substitutes them.
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
       CHARACTER*(*)      STR
       CHARACTER*(MXINCH) OUT,AUX
       LOGICAL USE(MXVAR)
       REAL RES(100)
       INTEGER MODRES(100),I,J,K,NCOUT,NC,IFAIL,INEXT,IENTRY,NRES,NCRES
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE INPSUB ///'
*** Initial values.
       INEXT=1
       NCOUT=0
       OUT=' '
       IFAIL=0
*** Scan the string.
       DO 10 I=1,NC
       IF(I.LT.INEXT.OR.STR(I:I).NE.'{'.OR.
     -      (I.GT.1.AND.STR(MAX(1,I-1):I).EQ.ESCAPE//'{'))GOTO 10
*** Copy the string up to the bracket.
       IF(I-1.GE.INEXT)THEN
            IF(NCOUT+I-INEXT.GT.LEN(STR).OR.
     -           NCOUT+I-INEXT.GT.LEN(OUT))GOTO 3000
            OUT(NCOUT+1:NCOUT+I-INEXT)=STR(INEXT:I-1)
            NCOUT=NCOUT+I-INEXT
       ENDIF
*** Scan for the closing bracket.
       DO 20 J=I+1,NC
*   Make sure we don't see a new open before this one is closed.
       IF(STR(J:J).EQ.'{'.AND.
     -      (J.GT.1.AND.STR(MAX(1,J-1):J).NE.ESCAPE//'{'))THEN
            PRINT *,' !!!!!! INPSUB WARNING : No nesting of'//
     -           ' substitution brackets allowed; no substitution.'
            IFAIL=1
            RETURN
       ENDIF
*   Skip until the closing bracket is seen.
       IF(STR(J:J).NE.'}'.OR.
     -      (J.GT.1.AND.STR(MAX(1,J-1):J).EQ.ESCAPE//'}'))GOTO 20
       INEXT=J+1
*   String is empty.
       IF(J.LE.I+1)GOTO 10
*   String is not empty, translate.
       CALL ALGPRE(STR(I+1:J-1),J-I-1,GLBVAR,NGLB,NRES,USE,IENTRY,
     -      IFAIL)
       IF(IFAIL.NE.0.OR.NRES.GT.100)THEN
            PRINT *,' !!!!!! INPSUB WARNING : The string "',
     -           STR(I+1:J-1),'" can not be translated'//
     -           ' or produces too many results.'
            IF(NCOUT+1.GT.LEN(STR).OR.NCOUT+1.GT.LEN(OUT))GOTO 3000
            OUT(NCOUT+1:NCOUT+1)='?'
            NCOUT=NCOUT+1
            CALL ALGCLR(IENTRY)
            GOTO 10
       ENDIF
*   Execute.
       CALL TIMEL(GLBVAL(1))
       CALL AL2EXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,NRES,IFAIL)
       CALL ALGERR
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! INPSUB WARNING : The expression "',
     -           STR(I+1:J-1),'" is syntax-wise correct'//
     -           ' but can not be evaluated.'
            CALL ALGCLR(IENTRY)
            DO 40 K=1,NRES
            CALL ALGREU(NINT(RES(K)),MODRES(K),1)
40          CONTINUE
            IF(NCOUT+1.GT.LEN(STR).OR.NCOUT+1.GT.LEN(OUT))GOTO 3000
            OUT(NCOUT+1:NCOUT+1)='?'
            NCOUT=NCOUT+1
            GOTO 10
       ENDIF
*   Remove the entry point.
       CALL ALGCLR(IENTRY)
*   Format each of the resulting numbers.
       DO 30 K=1,NRES
       CALL OUTFMT(RES(K),MODRES(K),AUX,NCRES,'LEFT')
       CALL ALGREU(NINT(RES(K)),MODRES(K),1)
       IF(NCOUT+NCRES.GT.LEN(STR).OR.
     -      NCOUT+NCRES.GT.LEN(OUT))GOTO 3000
       OUT(NCOUT+1:NCOUT+NCRES)=AUX(1:NCRES)
       NCOUT=NCOUT+NCRES
       IF(K.NE.NRES.AND.NRES.GT.1)THEN
            IF(NCOUT+2.GT.LEN(STR).OR.NCOUT+2.GT.LEN(OUT))GOTO 3000
            OUT(NCOUT+1:NCOUT+2)=', '
            NCOUT=NCOUT+2
       ENDIF
30     CONTINUE
*** Next component.
       GOTO 10
20     CONTINUE
*** Arrive here if the bracket is not closed.
       PRINT *,' !!!!!! INPSUB WARNING : Substitution bracket is not'//
     -      ' closed ; no substitution.'
       IFAIL=1
       RETURN
10     CONTINUE
*** Copy the remainder.
       IF(NC.GE.INEXT)THEN
            IF(NCOUT+NC-INEXT+1.GT.LEN(STR).OR.
     -           NCOUT+NC-INEXT+1.GT.LEN(OUT))GOTO 3000
            OUT(NCOUT+1:NCOUT+NC-INEXT+1)=STR(INEXT:NC)
            NCOUT=NCOUT+NC-INEXT+1
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ INPSUB DEBUG   : In ="'',A,
     -      ''"''/26X,''Out="'',A,''"'')')
     -      STR(1:MIN(LEN(STR),MAX(1,NC))),
     -      OUT(1:MIN(LEN(OUT),MAX(1,NCOUT)))
*** Send the string back.
       NC=NCOUT
       STR=OUT(1:MAX(1,MIN(MXINCH,LEN(STR),LEN(OUT),NCOUT)))
       IFAIL=0
       RETURN
*** Error because the resulting string is too long.
3000   CONTINUE
       PRINT *,' !!!!!! INPSUB WARNING : Substitution results in a'//
     -      ' string that is too long; no substitution.'
       IFAIL=1
       END
