CDECK  ID>, ALGEX3.
       SUBROUTINE ALGEX3(I,IFAIL)
*-----------------------------------------------------------------------
*   ALGEX3 - Routine executing instruction I (produced by ALGPRE).
*            This routine takes care of operations on logicals.
*   (Last changed on  4/ 3/94.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       INTEGER I,IFAIL,IREF,NCAUX
       REAL EPS
       CHARACTER*20 AUXSTR
*** Set IFAIL to 1 and EPS to 1.0E-5.
       IFAIL=1
       EPS=1.0E-5
*** Logical function call.
       IF(INS(I,2).EQ.6)THEN
            IF(INS(I,1).EQ.10)THEN
                 IF(ABS(REG(INS(I,3))).GT.EPS.AND.
     -                ABS(REG(INS(I,3))-1.0).GT.EPS)RETURN
                 REG(INS(I,4))=1.0-REG(INS(I,3))
                 MODREG(INS(I,4))=3
*   Make a string from a logical.
            ELSEIF(INS(I,1).EQ.12)THEN
                 CALL OUTFMT(REG(INS(I,3)),MODREG(INS(I,3)),
     -                AUXSTR,NCAUX,'LEFT')
                 CALL STRBUF('STORE',IREF,AUXSTR(1:NCAUX),NCAUX,IFAIL)
                 IF(IFAIL.NE.0)RETURN
                 MODREG(INS(I,4))=1
                 REG(INS(I,4))=IREF
*   Return the type of the argument.
            ELSEIF(INS(I,1).EQ.17)THEN
                 CALL STRBUF('STORE',IREF,'Logical',7,IFAIL)
                 IF(IFAIL.NE.0)RETURN
                 REG(INS(I,4))=IREF
                 MODREG(INS(I,4))=1
*   No other functions are known.
            ELSE
                 RETURN
            ENDIF
*** Binary logical operators between logical type arguments.
       ELSEIF((INS(I,2).GE.1.AND.INS(I,2).LE.3).OR.
     -      (INS(I,2).GE.10.AND.INS(I,2).LE.11).OR.
     -      (INS(I,2).GE.16.AND.INS(I,2).LE.17))THEN
*   Check that the numbers are really logicals.
            IF((ABS(REG(INS(I,1))-1.0).GT.EPS.AND.
     -           ABS(REG(INS(I,1))).GT.EPS).OR.
     -           (ABS(REG(INS(I,3))-1.0).GT.EPS.AND.
     -           ABS(REG(INS(I,3))).GT.EPS))RETURN
*   Or.
            IF(INS(I,2).EQ.17.OR.INS(I,2).EQ.1)
     -           REG(INS(I,4))=MIN(1.0,REG(INS(I,1))+REG(INS(I,3)))
*   Exclusive or.
            IF(INS(I,2).EQ.2)
     -           REG(INS(I,4))=MOD(REG(INS(I,1))+REG(INS(I,3)),2.0)
*   And.
            IF(INS(I,2).EQ.16.OR.INS(I,2).EQ.3)
     -           REG(INS(I,4))=REG(INS(I,1))*REG(INS(I,3))
*   Equivalence.
            IF(INS(I,2).EQ.10)REG(INS(I,4))=
     -           REG(INS(I,1))*REG(INS(I,3))+
     -           (1-REG(INS(I,1)))*(1-REG(INS(I,3)))
*   Non-equivalence.
            IF(INS(I,2).EQ.11)REG(INS(I,4))=
     -           (1-REG(INS(I,1)))*REG(INS(I,3))+
     -           REG(INS(I,1))*(1-REG(INS(I,3)))
*   Round the result to the nearest whole number.
            REG(INS(I,4))=ANINT(REG(INS(I,4)))
*   Propagate mode.
            MODREG(INS(I,4))=3
*** Unidentified operation code.
       ELSE
            RETURN
       ENDIF
*** Reset IFAIL to 0 because the calculations were probably successful.
       IFAIL=0
       END
