CDECK  ID>, ALGEX4.
       SUBROUTINE ALGEX4(I,IFAIL)
*-----------------------------------------------------------------------
*   ALGEX4 - Routine executing instruction I (produced by ALGPRE).
*            This routine takes care of operations on characters.
*   (Last changed on 10/ 1/02.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER I,J,IFAIL,IFAIL1,IFAIL2,IFAIL3,NC1,NC2,IREF
       CHARACTER*(MXINCH) STR1,STR2
       LOGICAL EXIST
*** Set IFAIL to 1.
       IFAIL=1
*** Binary operations, concatenation.
       IF(INS(I,2).EQ.1.OR.INS(I,2).EQ.4.OR.INS(I,2).EQ.16)THEN
*   Fetch the strings.
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
*   Depending on whether one or both have 0 length, concatenate.
            IF(NC1.GT.0.AND.NC2.GT.0)THEN
                 CALL STRBUF('STORE',IREF,STR1(1:NC1)//STR2(1:NC2),
     -                NC1+NC2,IFAIL3)
            ELSEIF(NC1.GT.0)THEN
                 CALL STRBUF('STORE',IREF,STR1(1:NC1),NC1,IFAIL3)
            ELSEIF(NC2.GT.0)THEN
                 CALL STRBUF('STORE',IREF,STR2(1:NC2),NC2,IFAIL3)
            ELSE
                 CALL STRBUF('STORE',IREF,' ',0,IFAIL3)
            ENDIF
*   Store the result.
            REG(INS(I,4))=IREF
            MODREG(INS(I,4))=1
*   Check error flag.
            IF(IFAIL1+IFAIL2+IFAIL3.NE.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEX4 DEBUG   :'',
     -                '' String buffer operation error:'',26X,
     -                '' Fetch: '',2I2,'' Store: '',I2)')
     -                IFAIL1,IFAIL2,IFAIL3
                 RETURN
            ENDIF
*   Minus
*      ELSEIF(INS(I,2).EQ.2)THEN
*   Product
*      ELSEIF(INS(I,2).EQ.3)THEN
*   Exponentiation
*      ELSEIF(INS(I,2).EQ.5)THEN
*** Function calls.
       ELSEIF(INS(I,2).EQ.6)THEN
*   Make a string from a string.
            IF(INS(I,1).EQ.12)THEN
                 REG(INS(I,4))=REG(INS(I,3))
*   Make a number from a string.
            ELSEIF(INS(I,1).EQ.-12)THEN
                 CALL STRBUF('READ',NINT(REG(INS(I,3))),STR1,NC1,IFAIL1)
                 MODREG(INS(I,4))=2
                 CALL INPRRC(STR1(1:NC1),REG(INS(I,4)),0.0,IFAIL2)
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
*   Locate a global variable from its name.
            ELSEIF(INS(I,1).EQ.16)THEN
                 CALL STRBUF('READ',NINT(REG(INS(I,3))),STR1,NC1,IFAIL1)
                 IF(NC1.GT.0)THEN
                      CALL CLTOU(STR1(1:NC1))
                      DO 10 J=1,NGLB
                      IF(STR1(1:NC1).EQ.GLBVAR(J))THEN
                           MODREG(INS(I,4))=GLBMOD(J)
                           REG(INS(I,4))=GLBVAL(J)
                           GOTO 20
                      ENDIF
10                    CONTINUE
                 ENDIF
                 MODREG(INS(I,4))=0
                 REG(INS(I,4))=0
20               CONTINUE
*   Return the type of the argument.
            ELSEIF(INS(I,1).EQ.17)THEN
                 CALL STRBUF('STORE',IREF,'String',6,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 REG(INS(I,4))=IREF
                 MODREG(INS(I,4))=1
*   Determine whether a file exists.
            ELSEIF(INS(I,1).EQ.46)THEN
                 CALL STRBUF('READ',NINT(REG(INS(I,3))),STR1,NC1,IFAIL1)
                 IF(IFAIL1.NE.0)RETURN
                 CALL DSNINQ(STR1,NC1,EXIST)
                 IF(EXIST)THEN
                      REG(INS(I,4))=1
                 ELSE
                      REG(INS(I,4))=0
                 ENDIF
                 MODREG(INS(I,4))=3
*   Other functions are not known.
            ELSE
                 RETURN
            ENDIF
*** Binary logical operators between character strings. First =
       ELSEIF(INS(I,2).EQ.10)THEN
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
            IF(NC1.LE.0.AND.NC2.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(NC1.LE.0.OR.NC2.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(STR1(1:NC1).EQ.STR2(1:NC2))THEN
                 REG(INS(I,4))=1
            ELSE
                 REG(INS(I,4))=0
            ENDIF
            MODREG(INS(I,4))=3
*   Not equal:
       ELSEIF(INS(I,2).EQ.11)THEN
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
            IF(NC1.LE.0.AND.NC2.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(NC1.LE.0.OR.NC2.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(STR1(1:NC1).NE.STR2(1:NC2))THEN
                 REG(INS(I,4))=1
            ELSE
                 REG(INS(I,4))=0
            ENDIF
            MODREG(INS(I,4))=3
*   Less:
       ELSEIF(INS(I,2).EQ.12)THEN
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
            IF(NC1.LE.0.AND.NC2.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(NC1.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(NC2.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(STR1(1:NC1).LT.STR2(1:NC2))THEN
                 REG(INS(I,4))=1
            ELSE
                 REG(INS(I,4))=0
            ENDIF
            MODREG(INS(I,4))=3
*   Less or equal:
       ELSEIF(INS(I,2).EQ.13)THEN
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
            IF(NC1.LE.0.AND.NC2.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(NC1.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(NC2.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(STR1(1:NC1).LE.STR2(1:NC2))THEN
                 REG(INS(I,4))=1
            ELSE
                 REG(INS(I,4))=0
            ENDIF
            MODREG(INS(I,4))=3
*   Greater:
       ELSEIF(INS(I,2).EQ.14)THEN
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
            IF(NC1.LE.0.AND.NC2.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(NC1.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(NC2.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(STR1(1:NC1).GT.STR2(1:NC2))THEN
                 REG(INS(I,4))=1
            ELSE
                 REG(INS(I,4))=0
            ENDIF
            MODREG(INS(I,4))=3
*   Greater or equal:
       ELSEIF(INS(I,2).EQ.15)THEN
            CALL STRBUF('READ',NINT(REG(INS(I,1))),STR1,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(REG(INS(I,3))),STR2,NC2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)RETURN
            IF(NC1.LE.0.AND.NC2.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(NC1.LE.0)THEN
                 REG(INS(I,4))=0
            ELSEIF(NC2.LE.0)THEN
                 REG(INS(I,4))=1
            ELSEIF(STR1(1:NC1).GE.STR2(1:NC2))THEN
                 REG(INS(I,4))=1
            ELSE
                 REG(INS(I,4))=0
            ENDIF
            MODREG(INS(I,4))=3
*** Unrecognised code.
       ELSE
            RETURN
       ENDIF
*** Reset IFAIL to 0 because the calculations were probably successful.
       IFAIL=0
       END
