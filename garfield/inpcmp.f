CDECK  ID>, INPCMP.
       INTEGER FUNCTION INPCMP(IWRD,REF)
*-----------------------------------------------------------------------
*   INPCMP - Integer function returning 1 if word IWRD matches with
*            REF in all segments (delimited by - signs).
*   VARIABLES : REF         : Reference string, the hash (#) signs
*                             indicate the abbreviation points.
*               IWRD        : The word to be matched with REF.
*               NMIN        : Minimum of characters required to match.
*   (Last changed on 26/10/07.)
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
       CHARACTER*(*) REF
       CHARACTER*80  REFSTR
       INTEGER IWRD,IFREF,IFCMP,ILREF,ILCMP,NMIN,NCREF,IHASH,NCOMP
*** Initialise some parameters.
       INPCMP=0
       IFREF=1
       IFCMP=1
*** Return right away if the string to be compared with does not exist.
       IF(IWRD.LE.0.OR.IWRD.GT.NWORD)RETURN
       IF(NCHAR(IWRD).EQ.0)RETURN
*** Return to this point if further segments are to be searched for.
10     CONTINUE
*** Find the next part of the reference string.
       ILREF=INDEX(REF(IFREF:LEN(REF)),'-')
       IF(ILREF.EQ.0)THEN
            ILREF=LEN(REF)
       ELSE
            ILREF=IFREF+ILREF-2
       ENDIF
*   Remove the # sign from the string and store NMIN.
       REFSTR=' '
       IF(ILREF.LT.IFREF)THEN
            REFSTR=' '
            NMIN=0
            NCREF=0
       ELSE
            IHASH=INDEX(REF(IFREF:ILREF),'#')
            IF(IHASH.EQ.0)THEN
                 REFSTR(1:ILREF-IFREF+1)=REF(IFREF:ILREF)
                 NMIN=ILREF-IFREF+1
                 NCREF=ILREF-IFREF+1
            ELSE
                 IF(IHASH.GE.2)
     -                REFSTR(1:IHASH-1)=REF(IFREF:IFREF+IHASH-2)
                 IF(IHASH.LT.ILREF-IFREF+1)REFSTR(IHASH:ILREF-IFREF)=
     -                REF(IFREF+IHASH:ILREF)
                 NMIN=IHASH-1
                 NCREF=ILREF-IFREF
            ENDIF
       ENDIF
**  Do similar things with the string to be compared.
       ILCMP=INDEX(WORD(IWRD)(IFCMP:NCHAR(IWRD)),'-')
       IF(ILCMP.EQ.0)THEN
            ILCMP=NCHAR(IWRD)
       ELSE
            ILCMP=IFCMP+ILCMP-2
       ENDIF
**  And compare the two strings.
       IF(NCREF.LT.ILCMP-IFCMP+1)RETURN
       NCOMP=MIN(NCREF,MAX(NMIN,ILCMP-IFCMP+1))
       IF(NCOMP.GT.0)THEN
            IF(REFSTR(1:NCOMP).NE.WORD(IWRD)(IFCMP:IFCMP+NCOMP-1))RETURN
       ENDIF
*** Return for a further cycle if there is more to compare.
       IFREF=ILREF+2
       IFCMP=ILCMP+2
       IF(IFREF.GT.LEN(REF))THEN
            IF(IFCMP.GT.NCHAR(IWRD))INPCMP=1
            RETURN
       ELSEIF(IFCMP.GT.NCHAR(IWRD))THEN
            IF(REF(IFREF:IFREF).EQ.'#')INPCMP=1
            RETURN
       ENDIF
       GOTO 10
       END
