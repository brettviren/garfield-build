CDECK  ID>, OUTFM5.
       SUBROUTINE OUTFM5(VAL,STRING)
*-----------------------------------------------------------------------
*   OUTFM5 - Takes care of formatting a matrix.
*   VARIABLES : VAL         : Reference to the matrix to be formatted.
*               STRING      : Output string.
*   (Last changed on  9/ 4/96.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       CHARACTER*(*) STRING
       CHARACTER*20 AUX
       REAL VAL
*** Initialise the string.
       STRING=' '
*** Matrix reference.
       IREF=NINT(VAL)
*** Locate the matrix.
       DO 10 I=1,MXMAT
       IF(MREF(I).EQ.IREF)THEN
            ISLOT=I
            GOTO 20
       ENDIF
10     CONTINUE
*   Issue warning if this doesn't exist.
       IF(LEN(STRING).GE.18)THEN
            STRING='<%unknown%matrix%>'
       ELSE
            STRING='?'
       ENDIF
       RETURN
20     CONTINUE
*** If the matrix is not suitable for formatting, show dimensions.
       IF(MDIM(ISLOT).GT.1.AND.MLEN(ISLOT).GT.1)THEN
*   Format the number of dimensions.
            CALL OUTFM2(REAL(MDIM(ISLOT)),AUX)
*   Get the length.
            DO 30 I=LEN(AUX),1,-1
            IF(AUX(I:I).NE.' ')THEN
                 NC=I
                 GOTO 70
            ENDIF
30          CONTINUE
            NC=1
70          CONTINUE
*   Format the description.
            IF(LEN(STRING).GE.NC+7)THEN
                 STRING=AUX(1:NC)//'-Matrix'
            ELSE
                 STRING='***'
            ENDIF
            RETURN
       ENDIF
*** If the string is too short, no way to format.
       IF(LEN(STRING).LT.5)THEN
            STRING='***'
            RETURN
       ENDIF
*** Format the first bit of the matrix.
       STRING(1:1)='('
       NCSTR=1
       DO 40 I=1,MLEN(ISLOT)
*   Format an element.
       CALL OUTFM2(MVEC(MORG(ISLOT)+I),AUX)
*   Get the length.
       DO 50 J=LEN(AUX),1,-1
       IF(AUX(J:J).NE.' ')THEN
            NC=J
            GOTO 60
       ENDIF
50     CONTINUE
       NC=1
60     CONTINUE
*   Add it to the string.
       IF(LEN(STRING).GE.NCSTR+NC+2)THEN
            STRING(NCSTR+1:NCSTR+NC+2)=AUX(1:NC)//',%'
            NCSTR=NCSTR+NC+2
       ELSEIF(LEN(STRING).GE.NCSTR+4)THEN
            STRING(NCSTR+1:NCSTR+4)='...)'
            NCSTR=NCSTR+4
            RETURN
       ELSE
            STRING(LEN(STRING)-3:)='***)'
            NCSTR=LEN(STRING)
            RETURN
       ENDIF
40     CONTINUE
       IF(NCSTR.GE.2)STRING(NCSTR-1:NCSTR)=') '
       END
