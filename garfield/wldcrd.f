CDECK  ID>, WLDCRD.
       SUBROUTINE WLDCRD(REFIN,WILDIN,FREEND,MATCH)
*-----------------------------------------------------------------------
*   WLDCRD - Compares a string with a wildcard (the asterix may stand
*            for any number of arbitrary characters).
*   VARIABLES : REF         : The reference string, without asterix
*               WILD        : The wildcard
*               FREEND      : Equivalent to a final asterix in WILD.
*               MATCH       : Set to .TRUE. only if the strings match.
*               IW0, IW1    : Begin and end of a segment in the wildcard
*               IR0, IR1    : Begin of the part of the reference string
*                             to be searched and the start of the match.
*-----------------------------------------------------------------------
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
       CHARACTER*(*) REFIN,WILDIN
       CHARACTER*80 REF,WILD
       LOGICAL FREEND,MATCH,ASTER
*** Check for empty strings.
       IF(REFIN.EQ.' '.OR.LEN(REFIN).EQ.0.OR.
     -      WILDIN.EQ.' '.OR.LEN(WILDIN).EQ.0)THEN
            MATCH=.FALSE.
            RETURN
       ENDIF
*** Avoid out of bounds array references.
       IF(LEN(REFIN).GT.80.OR.LEN(WILDIN).GT.80)THEN
            PRINT *,' ###### WLDCRD ERROR   : Input strings too long:'//
     -           ' REF: ',LEN(REF),' WILD: ',LEN(WILD)
            PRINT *,'                         (program bug - please'//
     -           ' report); strings declared non-matching.'
            MATCH=.FALSE.
            RETURN
       ENDIF
*** Copy the strings translating to upper case.
       NREF=0
       DO 10 I=1,LEN(REFIN)
       IC=ICHAR(REFIN(I:I))
*   ASCII: all letters are contiguous and located between 97 and 122.
       IF(ICHSET.EQ.1.AND.IC.LE.122.AND.IC.GE.97)THEN
            REF(I:I)=CHAR(IC-32)
*   EBCDIC: there are 2 gaps in the set (idea from IBM of course).
       ELSEIF(ICHSET.EQ.2.AND.((IC.GE.129.AND.IC.LE.137).OR.
     -      (IC.GE.145.AND.IC.LE.153).OR.(IC.GE.162.AND.IC.LE.169)))THEN
            REF(I:I)=CHAR(IC+64)
*   Anything else: don't do anything.
       ELSE
            REF(I:I)=CHAR(IC)
       ENDIF
*   Keep track of the lenghts.
       IF(REF(I:I).NE.' ')NREF=I
10     CONTINUE
**  Do the same for the wildcard.
       NWILD=0
       DO 20 I=1,LEN(WILDIN)
       IC=ICHAR(WILDIN(I:I))
*   ASCII: all letters are contiguous and located between 97 and 122.
       IF(ICHSET.EQ.1.AND.IC.LE.122.AND.IC.GE.97)THEN
            WILD(I:I)=CHAR(IC-32)
*   EBCDIC: there are 2 gaps in the set (idea from IBM of course).
       ELSEIF(ICHSET.EQ.2.AND.((IC.GE.129.AND.IC.LE.137).OR.
     -      (IC.GE.145.AND.IC.LE.153).OR.(IC.GE.162.AND.IC.LE.169)))THEN
            WILD(I:I)=CHAR(IC+64)
*   Anything else: don't do anything.
       ELSE
            WILD(I:I)=CHAR(IC)
       ENDIF
*   Keep track of the lenghts.
       IF(WILD(I:I).NE.' ')NWILD=I
20     CONTINUE
*** Compare segment by segment.
       IW0=1
       IW1=1
       IR0=1
       IR1=1
       INIT=1
*   Pick up the next segment of the wildcard.
100    CONTINUE
       IW1=IW0+INDEX(WILD(IW0:NWILD),'*')-2
       IF(IW1.EQ.IW0-2)THEN
            IW1=NWILD
       ELSEIF(IW1.LT.IW0)THEN
            IW0=IW1+2
            IF(IW0.GT.NWILD)GOTO 500
            GOTO 100
       ENDIF
*   Attempt to match with the reference string.
       IR1=IR0+INDEX(REF(IR0:NREF),WILD(IW0:IW1))-1
       IF(IR1.EQ.IR0-1)THEN
            MATCH=.FALSE.
            RETURN
       ENDIF
*   Check the asterix at the beginning of the wildcard.
       IF(IR1.NE.1.AND.INIT.EQ.1.AND.WILD(1:1).NE.'*')THEN
            MATCH=.FALSE.
            RETURN
       ENDIF
*   Update the start of string pointers.
       IR0=IR1+(IW1-IW0+1)
       IW0=IW1+2
*   Check whether the end has been reached.
       IF(IW0.GT.NWILD.OR.IR0.GT.NREF)GOTO 500
*   Look for the next segment.
       INIT=0
       GOTO 100
*** End of the line is reached.
500    CONTINUE
*   Figure out whether the end of the wildcard is pure asterix.
       ASTER=.TRUE.
       DO 510 I=MAX(1,IW0-1),NWILD
       IF(WILD(I:I).NE.'*')THEN
            ASTER=.FALSE.
            GOTO 520
       ENDIF
510    CONTINUE
520    CONTINUE
*   Match if both strings have been used up entirely.
       IF(IR0.GT.NREF.AND.IW0.GT.NWILD)THEN
            MATCH=.TRUE.
*   Free end of reference string matching.
       ELSEIF(IR0.LE.NREF)THEN
            IF((IW0.GT.NWILD.AND.FREEND).OR.
     -           (IW0.LE.NWILD+1.AND.ASTER))THEN
                 MATCH=.TRUE.
            ELSE
                 MATCH=.FALSE.
            ENDIF
*   Excess of non-asterix characters at the end of the wildcard.
       ELSEIF(IW0.LE.NWILD)THEN
            IF(IR0.GT.NREF.AND..NOT.ASTER)THEN
                 MATCH=.FALSE.
            ELSE
                 MATCH=.TRUE.
            ENDIF
*   Strange case.
       ELSE
            PRINT *,' ###### WLDCRD ERROR   : No handling available,'//
     -           ' program bug ; declared not to match.'
            PRINT *,'                         IW0=',IW0,', IW1=',IW1,
     -           ', NWILD=',NWILD,', WILD="'//WILD(1:NWILD)//'"'
            PRINT *,'                         IR0=',IR0,', IR1=',IR1,
     -           ', NREF =',NREF,', REF ="'//REF(1:NREF)//'"'
            PRINT *,'                         FREEND=',FREEND,
     -           ', ASTER=',ASTER
            MATCH=.FALSE.
       ENDIF
       END
