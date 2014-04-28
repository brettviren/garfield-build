CDECK  ID>, ALGTYP.
       SUBROUTINE ALGTYP(VARINP,IMODE)
*-----------------------------------------------------------------------
*   ALGTYP - Determines the type of the argument string. Return one of
*            the following: 0 - Undefined, 1 - String, 2 - Number,
*            3 - Logical, 4 - Histogram or 5 - Matrix.
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
       CHARACTER*(*) VARINP
       CHARACTER*(MXINCH) VAR
       INTEGER IMODE,I,NC
       LOGICAL MANT,POWER,DOT,NUMBER,PASS,END,SIGN
*** Store the length.
       NC=LEN(VARINP)
*** Ensure the length is not nill or too large.
       IF(NC.LT.1.OR.NC.GT.MXINCH)THEN
            PRINT *,' !!!!!! ALGTYP WARNING : Argument string is too'//
     -           ' long or too short; returning Undefined as type.'
            IMODE=0
            RETURN
       ENDIF
*** Convert to upper case.
       VAR=VARINP
       CALL CLTOU(VAR)
*** Check for Undefined.
       IF(VAR(1:NC).EQ.'NILL')THEN
            IMODE=0
*** Check for Logical.
       ELSEIF(VAR(1:NC).EQ.'TRUE'.OR.VAR(1:NC).EQ.'FALSE')THEN
            IMODE=3
*** Separate numbers and strings.
       ELSE
*   Preset the state flags.
            MANT=.FALSE.
            POWER=.FALSE.
            DOT=.FALSE.
            END=.FALSE.
            NUMBER=.FALSE.
            SIGN=.FALSE.
            PASS=.TRUE.
            END=.FALSE.
*   Loop over the string.
            DO 10 I=1,NC
*   Only leading and trailing blanks.
            IF(VAR(I:I).EQ.' ')THEN
                 IF(MANT.OR.POWER.OR.DOT)END=.TRUE.
*   Only only dot and only in the mantissa.
            ELSEIF(VAR(I:I).EQ.'.')THEN
                 IF(END.OR.DOT.OR.POWER)PASS=.FALSE.
                 DOT=.TRUE.
                 IF(.NOT.POWER)MANT=.TRUE.
*   Only one exponent; switch from mantissa to exponent.
            ELSEIF(VAR(I:I).EQ.'E')THEN
                 IF(END.OR.POWER)PASS=.FALSE.
                 MANT=.FALSE.
                 POWER=.TRUE.
                 NUMBER=.FALSE.
                 DOT=.FALSE.
                 SIGN=.FALSE.
*   Only one leading sign per mantissa and per exponent.
            ELSEIF(INDEX('+-',VAR(I:I)).NE.0)THEN
                 IF(END.OR.SIGN.OR.NUMBER)PASS=.FALSE.
                 SIGN=.TRUE.
                 IF(.NOT.POWER)MANT=.TRUE.
*   Numbers anywhere, except after blanks.
            ELSEIF(INDEX('0123456789',VAR(I:I)).NE.0)THEN
                 IF(END)PASS=.FALSE.
                 NUMBER=.TRUE.
                 IF(.NOT.POWER)MANT=.TRUE.
*   Unknown characters are rejected.
            ELSE
                 PASS=.FALSE.
            ENDIF
10          CONTINUE
*   If there is an exponent part, there must be a number.
            IF(POWER.AND..NOT.NUMBER)PASS=.FALSE.
*   If all tests passed, assign Number, otherwise String.
            IF(PASS)THEN
                 IMODE=2
            ELSE
                 IMODE=1
            ENDIF
       ENDIF
       END
