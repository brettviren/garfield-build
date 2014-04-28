CDECK  ID>, STRCAL.
       SUBROUTINE STRCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   STRCAL - Handles string procedure calls.
*   (Last changed on 21/ 1/00.)
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
       CHARACTER*(MXINCH) STRING,AUX1,AUX2,AUX3
       INTEGER INSTR,IFAIL,NARG,IPROC,NC,NC1,NC2,NC3,IFAIL1,IFAIL2,
     -      IFAIL3,IFAIL4,IAUX,IF,IL,I,NOUT,ISEP,ISQ,IDQ,NWORD,I0,I1,
     -      IMATCH,INEXT,INPCMX,IREF
       EXTERNAL INPCMX
*** Assume that this will fail.
       IFAIL=1
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Locate one string inside another.
       IF(IPROC.EQ.-901)THEN
*   Check arguments.
            IF(NARG.NE.3.OR.ARGREF(3,1).GE.2.OR.
     -           MODARG(1).NE.1.OR.MODARG(2).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_INDEX.'
                 RETURN
            ENDIF
*   Get strings from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(ARG(2)),AUX2,NC2,IFAIL2)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   And store result of operation.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                 ARG(3)=INDEX(STRING(1:NC1),AUX2(1:NC2))
                 MODARG(3)=2
            ELSE
                 ARG(3)=-1
                 MODARG(3)=2
                 PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch a'//
     -                ' string for STRING_INDEX.'
            ENDIF
*** Return a substring.
       ELSEIF(IPROC.EQ.-902)THEN
*   Check arguments.
            IF(NARG.NE.4.OR.ARGREF(4,1).GE.2.OR.MODARG(1).NE.1.OR.
     -           MODARG(2).NE.2.OR.MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_PORTION.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   And store result of operation.
            IF(IFAIL1.NE.0)THEN
                 IFAIL2=1
            ELSEIF((ARG(2).GT.NC.AND.ARG(3).GT.NC).OR.
     -           (ARG(2).LT.1.AND.ARG(3).LT.1))THEN
                 CALL STRBUF('STORE',IAUX,' ',0,IFAIL2)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
            ELSE
                 IF=MAX(1,MIN(NC,NINT(ARG(2))))
                 IL=MAX(1,MIN(NC,NINT(ARG(3))))
                 IF(IL.GE.IF)THEN
                      DO 100 I=IF,IL
                      AUX1(I-IF+1:I-IF+1)=STRING(I:I)
100                   CONTINUE
                 ELSE
                      DO 110 I=IF,IL,-1
                      AUX1(IF-I+1:IF-I+1)=STRING(I:I)
110                   CONTINUE
                 ENDIF
                 NC=ABS(IL-IF)+1
                 CALL STRBUF('STORE',IAUX,AUX1(1:NC),NC,IFAIL2)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
            ENDIF
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)
     -           PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch or'//
     -           ' store a string for STRING_PORTION.'
*** Delete part of a string.
       ELSEIF(IPROC.EQ.-903)THEN
*   Check arguments.
            IF(NARG.NE.4.OR.ARGREF(4,1).GE.2.OR.MODARG(1).NE.1.OR.
     -           MODARG(2).NE.2.OR.MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_DELETE.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   And store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 NOUT=0
                 AUX1=' '
                 DO 120 I=1,NC
                 IF(I.GE.MIN(NINT(ARG(2)),NINT(ARG(3))).AND.
     -                I.LE.MAX(NINT(ARG(2)),NINT(ARG(3))))GOTO 120
                 NOUT=NOUT+1
                 AUX1(NOUT:NOUT)=STRING(I:I)
120              CONTINUE
                 CALL STRBUF('STORE',IAUX,AUX1(1:(MAX(1,NC))),NC,
     -                IFAIL2)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
            ELSE
                 IFAIL2=1
            ENDIF
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)
     -           PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch or'//
     -           ' store a string for STRING_DELETE.'
*** Convert a string to lower case.
       ELSEIF(IPROC.EQ.-904)THEN
*   Check arguments.
            IF(NARG.NE.1.OR.ARGREF(1,1).GE.2.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_LOWER.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 CALL CUTOL(STRING(1:NC))
                 CALL STRBUF('STORE',IAUX,STRING(1:NC),NC,IFAIL2)
                 ARG(1)=REAL(IAUX)
                 MODARG(1)=1
            ELSE
                 IFAIL2=0
            ENDIF
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)
     -           PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch or'//
     -           ' store a string for STRING_LOWER.'
*** Convert a string to upper case.
       ELSEIF(IPROC.EQ.-905)THEN
*   Check arguments.
            IF(NARG.NE.1.OR.ARGREF(1,1).GE.2.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_UPPER.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 CALL CLTOU(STRING(1:NC))
                 CALL STRBUF('STORE',IAUX,STRING(1:NC),NC,IFAIL2)
                 ARG(1)=REAL(IAUX)
                 MODARG(1)=1
            ELSE
                 IFAIL2=0
            ENDIF
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)
     -           PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch or'//
     -           ' store a string for STRING_UPPER.'
*** Number of words in a string.
       ELSEIF(IPROC.EQ.-906)THEN
*   Check arguments.
            IF(NARG.NE.2.OR.ARGREF(2,1).GE.2.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_WORDS.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 ARG(2)=0
                 MODARG(2)=2
                 ISEP=1
                 ISQ=0
                 IDQ=0
                 DO 130 I=1,NC
                 IF(STRING(I:I).EQ.''''.AND.IDQ.EQ.0)ISQ=1-ISQ
                 IF(STRING(I:I).EQ.'"'.AND.ISQ.EQ.0)IDQ=1-IDQ
                 IF(ISQ.EQ.0.AND.IDQ.EQ.0.AND.
     -                INDEX(' :,=',STRING(I:I)).NE.0)THEN
                      IF(ISEP.EQ.0)ARG(2)=ARG(2)+1
                      ISEP=1
                 ELSE
                      ISEP=0
                 ENDIF
130              CONTINUE
                 IF(ISEP.EQ.0)ARG(2)=ARG(2)+1
                 IF(ISQ.NE.0)PRINT *,' !!!!!! STRCAL WARNING: Odd'//
     -                ' number of single quotes; one added at end.'
                 IF(IDQ.NE.0)PRINT *,' !!!!!! STRCAL WARNING: Odd'//
     -                ' number of double quotes; one added at end.'
            ELSE
                 ARG(2)=-1
                 MODARG(2)=2
                 PRINT *,' !!!!!! STRCAL WARNING :'//
     -                ' Unable to fetch a string for STRING_WORDS.'
            ENDIF
*** Return a word from a string.
       ELSEIF(IPROC.EQ.-907)THEN
*   Check arguments.
            IF(NARG.NE.3.OR.ARGREF(3,1).GE.2.OR.
     -           MODARG(1).NE.1.OR.MODARG(2).NE.2)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_WORD.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 ISEP=1
                 ISQ=0
                 IDQ=0
                 I0=1
                 AUX1=' '
                 NC1=0
                 NWORD=0
                 DO 140 I=1,NC
                 IF(STRING(I:I).EQ.''''.AND.IDQ.EQ.0)ISQ=1-ISQ
                 IF(STRING(I:I).EQ.'"'.AND.ISQ.EQ.0)IDQ=1-IDQ
                 IF(ISQ.EQ.0.AND.IDQ.EQ.0.AND.
     -                INDEX(' :,=',STRING(I:I)).NE.0)THEN
                      IF(ISEP.EQ.0)NWORD=NWORD+1
                      IF(NWORD.EQ.NINT(ARG(2)).AND.ISEP.EQ.0)THEN
                           IF(INDEX('''"',STRING(I0:I0)).NE.0)I0=I0+1
                           I1=I-1
                           IF(INDEX('''"',STRING(I1:I1)).NE.0)I1=I1-1
                           IF(I1.GE.I0.AND.I0.GE.1.AND.I1.GE.1.AND.
     -                          I0.LE.NC.AND.I1.LE.NC)THEN
                                AUX1=STRING(I0:I1)
                                NC1=I1-I0+1
                           ELSE
                                AUX1=' '
                                NC1=1
                           ENDIF
                      ENDIF
                      ISEP=1
                 ELSE
                      IF(ISEP.EQ.1)I0=I
                      ISEP=0
                 ENDIF
140              CONTINUE
                 IF(ISEP.EQ.0)NWORD=NWORD+1
                 IF(NWORD.EQ.NINT(ARG(2)).AND.ISEP.EQ.0)THEN
                      IF(INDEX('''"',STRING(I0:I0)).NE.0)I0=I0+1
                      I1=NC
                      IF(INDEX('''"',STRING(I1:I1)).NE.0)I1=I1-1
                      IF(I1.GE.I0.AND.I0.GE.1.AND.I1.GE.1.AND.
     -                     I0.LE.NC.AND.I1.LE.NC)THEN
                           AUX1=STRING(I0:I1)
                           NC1=I1-I0+1
                      ELSE
                           AUX1=' '
                           NC1=1
                      ENDIF
                 ENDIF
                 CALL STRBUF('STORE',IAUX,AUX1(1:NC1),NC1,IFAIL2)
                 ARG(3)=REAL(IAUX)
                 MODARG(3)=1
            ELSE
                 IFAIL2=0
            ENDIF
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)
     -           PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch or'//
     -           ' store a string for STRING_WORD.'
*** See whether two strings match.
       ELSEIF(IPROC.EQ.-908)THEN
*   Check arguments.
            IF(NARG.NE.3.OR.ARGREF(3,1).GE.2.OR.
     -           MODARG(1).NE.1.OR.MODARG(2).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_MATCH.'
                 RETURN
            ENDIF
*   Get strings from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(ARG(2)),AUX2,NC2,IFAIL2)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Result of opetration.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                 IMATCH=INPCMX(STRING(1:NC1),AUX2(1:NC2))
                 IF(IMATCH.NE.0)THEN
                      ARG(3)=1
                 ELSE
                      ARG(3)=0
                 ENDIF
                 MODARG(3)=3
            ELSE
                 PRINT *,' !!!!!! STRCAL WARNING : Unable to fetch'//
     -                ' a string for STRING_MATCH.'
            ENDIF
*** Replace parts of a string.
       ELSEIF(IPROC.EQ.-909)THEN
*   Check arguments.
            IF(NARG.NE.3.OR.ARGREF(4,1).GE.2.OR.MODARG(1).NE.1.OR.
     -           MODARG(2).NE.1.OR.MODARG(3).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_REPLACE.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC1,IFAIL1)
            CALL STRBUF('READ',NINT(ARG(2)),AUX2,  NC2,IFAIL2)
            CALL STRBUF('READ',NINT(ARG(3)),AUX3,  NC3,IFAIL3)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Unable to retrieve'//
     -                ' an argument of STRING_REPLACE; not executed.'
                 RETURN
            ENDIF
*   Start.
            INEXT=1
            NOUT=0
*   Replace.
            DO 150 I=1,NC1
            IF(I.LT.INEXT)GOTO 150
            IF(STRING(I:MIN(NC1,I+NC2-1)).EQ.AUX2(1:NC2).AND.
     -           I+NC2-1.LE.NC1)THEN
                 IF(NOUT+NC3.GT.LEN(AUX1))THEN
                      PRINT *,' !!!!!! STRCAL WARNING : String grows'//
     -                     ' too much while replacing characters;'//
     -                     ' string not changed.'
                      RETURN
                 ENDIF
                 AUX1(NOUT+1:NOUT+NC3)=AUX3(1:NC3)
                 NOUT=NOUT+NC3
                 INEXT=I+NC2
            ELSE
                 IF(NOUT+1.GT.LEN(AUX1))THEN
                      PRINT *,' !!!!!! STRCAL WARNING : String grows'//
     -                     ' too much while replacing characters;'//
     -                     ' string not changed.'
                      RETURN
                 ENDIF
                 AUX1(NOUT+1:NOUT+1)=STRING(I:I)
                 NOUT=NOUT+1
                 INEXT=I+1
            ENDIF
150         CONTINUE
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   And store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 CALL STRBUF('STORE',IAUX,AUX1(1:NOUT),NOUT,IFAIL4)
                 ARG(1)=REAL(IAUX)
                 MODARG(1)=1
                 IF(IFAIL4.NE.0)THEN
                      PRINT *,' !!!!!! STRCAL WARNING : Unable to'//
     -                     ' store the processed string; original'//
     -                     ' string lost.'
                      RETURN
                 ENDIF
            ENDIF
*** List the string buffer.
       ELSEIF(IPROC.EQ.-910)THEN
            IF(NARG.NE.0)PRINT *,' !!!!!! STRCAL WARNING : The'//
     -           ' LIST_STRINGS procedure has no arguments; ignored.'
            CALL STRBUF('DUMP',IREF,' ',1,IFAIL)
*** Length of a string.
       ELSEIF(IPROC.EQ.-911)THEN
*   Check arguments.
            IF(NARG.NE.2.OR.ARGREF(2,1).GE.2.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! STRCAL WARNING : Incorrect set'//
     -                ' of arguments for STRING_LENGTH.'
                 RETURN
            ENDIF
*   Get string from store.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL1)
*   Clear previous use of result.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Store result of operation.
            IF(IFAIL1.EQ.0)THEN
                 ARG(2)=REAL(NC)
                 MODARG(2)=2
            ELSE
                 ARG(2)=-1
                 MODARG(2)=2
                 PRINT *,' !!!!!! STRCAL WARNING :'//
     -                ' Unable to fetch a string for STRING_LENGTH.'
            ENDIF
*** Delete strings.
       ELSEIF(IPROC.EQ.-912)THEN
*   Without arguments, delete all strings.
            IF(NARG.LT.1)THEN
                 DO 10 I=1,NGLB
                 IF(GLBMOD(I).EQ.1)THEN
                      CALL STRBUF('DELETE',NINT(GLBVAL(I)),' ',1,IFAIL1)
                      GLBVAL(I)=0
                      GLBMOD(I)=0
                 ENDIF
10               CONTINUE
*   Delete all the matrices in the arguments.
            ELSE
                 DO 20 I=1,NARG
                 IF(MODARG(I).NE.1)THEN
                      PRINT *,' !!!!!! STRCAL WARNING : Argument ',I,
     -                     ' is not a string; not deleted.'
                      GOTO 20
                 ENDIF
                 CALL STRBUF('DELETE',NINT(ARG(I)),' ',1,IFAIL1)
                 ARG(I)=0
                 MODARG(I)=0
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! STRCAL WARNING :'//
     -                ' Deleting a string failed.'
20               CONTINUE
            ENDIF
*** Other procedures are not known.
       ELSE
            PRINT *,' !!!!!! STRCAL WARNING : Unknown procedure code'//
     -           ' received.'
            RETURN
       ENDIF
*** Things worked fine.
       IFAIL=0
       END
