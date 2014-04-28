CDECK  ID>, ALGEXE.
       SUBROUTINE ALGEXE(IENTRY,VAR,MODVAR,NVAR,RES,MODRES,NNRES,IFAIL)
*-----------------------------------------------------------------------
*   ALGEXE - Routine executing the instructions produced by ALGPRE.
*   (Last changed on  1/ 9/09.)
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
       REAL VAR(*),RES(*),EPS
       INTEGER MODVAR(*),MODRES(*),IENTRY,NVAR,NNRES,IFAIL,I,J,IENTNO,
     -      INEXT,IDUM,NCDUM,IFAIL1
       CHARACTER*1 DUMSTR
       PARAMETER(EPS=1.0E-5)
*** Early returns mean evalution failed.
       IFAIL=1
*** Assign zero to all expected results.
       DO 40 I=1,NNRES
       RES(I)=0.0
       MODRES(I)=0
40     CONTINUE
*** Zero argument buffer.
       DO 160 I=1,MXARG
       ARG(I)=0.0
       MODARG(I)=0
       ARGREF(I,1)=0
       ARGREF(I,2)=0
160    CONTINUE
*** Locate the entry point.
       IENTNO=0
       DO 30 I=1,NALGE
       IF(ALGENT(I,1).EQ.IENTRY)IENTNO=I
30     CONTINUE
       IF(IENTNO.EQ.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,*) ' ++++++ ALGEXE DEBUG   :'//
     -           ' Requested entry point does not exist.'
            RETURN
       ENDIF
       IF(ALGENT(IENTNO,2).EQ.0.OR.ALGENT(IENTNO,3).EQ.0.OR.
     -      ALGENT(IENTNO,7).GT.NVAR.OR.
     -      (ALGENT(IENTNO,10).NE.0.AND.ALGENT(IENTNO,10).GT.NNRES))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEXE DEBUG   :'',
     -           '' List for entry point is not executable.''/
     -           26X,''Serial number='',I4,'', Reference number='',I4/
     -           26X,''In use='',I1,'', List correct='',I1,
     -               '', Sequential='',I1/
     -           26X,''First instruction='',I4,'', # instructions='',I4/
     -           26X,''# variables expected='',I4,'' (given='',I4,'')''/
     -           26X,''First constant='',I4,'', # constants='',I4/
     -           26X,''# results from list='',I4,'' (expected='',I4,
     -               '').'')')
     -           IENTNO,(ALGENT(IENTNO,I),I=1,7),NVAR,
     -           (ALGENT(IENTNO,I),I=8,10),NNRES
            RETURN
       ENDIF
*** First assign the values of the variables to REG.
       DO 10 I=1,MXREG
       IF(I.LE.NVAR.AND.I.LE.ALGENT(IENTNO,7))THEN
            REG(I)=VAR(I)
            MODREG(I)=MODVAR(I)
       ELSE
            REG(I)=0
            MODREG(I)=0
       ENDIF
10     CONTINUE
       IFAIL=0
*** Execute all the instructions.
       INEXT=ALGENT(IENTNO,5)-1
20     CONTINUE
       INEXT=INEXT+1
*** Return at the end of the list and if INEXT has been set to 0.
       IF(INEXT.GT.ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1.OR.
     -      INEXT.EQ.0)GOTO 3000
*** Do nothing statement
       IF(INS(INEXT,2).EQ.-1)THEN
            GOTO 20
*** Lines of the result-assignment type.
       ELSEIF(INS(INEXT,2).EQ.0)THEN
            IF(INS(INEXT,4).LT.1.OR.INS(INEXT,4).GT.NNRES)THEN
                 IFAIL=1
                 IF(LDEBUG)WRITE(LUNOUT,'(1X,A,I3,A)')
     -                ' ++++++ ALGEXE DEBUG   : No room for result'//
     -                ' produced at line ',INEXT,' in receiving array.'
                 GOTO 3000
            ELSE
                 RES(INS(INEXT,4))=REG(INS(INEXT,3))
                 MODRES(INS(INEXT,4))=MODREG(INS(INEXT,3))
            ENDIF
*** GOTO statement.
       ELSEIF(INS(INEXT,2).EQ.7)THEN
            IF(ABS(REG(INS(INEXT,1))-1).LT.EPS)THEN
                 INEXT=NINT(REG(INS(INEXT,3)))-1
            ELSEIF(ABS(REG(INS(INEXT,1))).GT.EPS)THEN
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(2X,A)') '++++++ ALGEXE DEBUG   :'//
     -                     ' Logical value error at the line:'
                      CALL ALGPRT(INEXT,INEXT)
                      WRITE(LUNOUT,'(26X,''Reg('',I3,'')='',E15.7,
     -                     '';  Reg('',I3,'')='',E15.7,/)')
     -                     INS(INEXT,1),REG(INS(INEXT,1)),
     -                     INS(INEXT,3),REG(INS(INEXT,3))
                 ENDIF
                 IFAIL=1
                 GOTO 3000
            ENDIF
*** Arguments.
       ELSEIF(INS(INEXT,2).EQ.8)THEN
            IF(INS(INEXT,4).LE.0.OR.INS(INEXT,4).GT.MXARG)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGEXE DEBUG   :'',
     -                '' Invalid argument # '',I3,'' found in line '',
     -                I3,'':'')') INS(INEXT,4),INEXT
                 IF(LDEBUG)CALL ALGPRT(INEXT,INEXT)
                 IFAIL=1
                 GOTO 3000
            ENDIF
            ARG(INS(INEXT,4))=REG(INS(INEXT,3))
            MODARG(INS(INEXT,4))=MODREG(INS(INEXT,3))
            ARGREF(INS(INEXT,4),1)=INS(INEXT,1)
            ARGREF(INS(INEXT,4),2)=INS(INEXT,3)
*** Procedure calls.
       ELSEIF(INS(INEXT,2).EQ.9)THEN
*   Execute the procedure.
            CALL ALGCAL(INEXT,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 NERR=NERR+1
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(1X,A)') ' ++++++ ALGEXE'//
     -                     ' DEBUG   : Procedure call error in:'
                      CALL ALGPRT(INEXT,INEXT)
                      WRITE(LUNOUT,'(26X,''Arguments:''/26X,
     -                     ''   No         Value Mode Ref1 Ref2'')')
                      DO 170 I=1,INS(INEXT,3)
                      WRITE(LUNOUT,'(26X,I5,2X,E12.5,3I5)')
     -                     I,ARG(I),MODARG(I),ARGREF(I,1),ARGREF(I,2)
170                   CONTINUE
                 ENDIF
                 IFAIL=1
                 GOTO 3000
            ENDIF
*   Back transfer of arguments to origin registers and variables.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Arguments:''/26X,
     -           ''   No         Value Mode Ref1 Ref2'')')
            DO 100 I=1,INS(INEXT,3)
            IF(LDEBUG)WRITE(LUNOUT,'(26X,I5,2X,E12.5,3I5)')
     -           I,ARG(I),MODARG(I),ARGREF(I,1),ARGREF(I,2)
            IF(ARGREF(I,1).GE.2)GOTO 100
            REG(ARGREF(I,2))=ARG(I)
            MODREG(ARGREF(I,2))=MODARG(I)
            IF(ARGREF(I,2).GE.1.AND.
     -           ARGREF(I,2).LE.NVAR.AND.
     -           ARGREF(I,2).LE.ALGENT(IENTNO,7))THEN
C                 CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
                 VAR(ARGREF(I,2))=ARG(I)
                 MODVAR(ARGREF(I,2))=MODARG(I)
            ENDIF
100         CONTINUE
*** RETURN, EXIT and QUIT instruction codes.
       ELSEIF(INS(INEXT,2).EQ.-9)THEN
*   Condition satisfied.
            IF(ABS(REG(INS(INEXT,1))-1).LT.EPS)THEN
                 IF(INS(INEXT,3).EQ.0.OR.INS(INEXT,3).EQ.1)THEN
                      INEXT=-1
                 ELSEIF(INS(INEXT,3).EQ.2)THEN
                      CALL QUIT
                 ELSE
                      IF(LDEBUG)THEN
                           WRITE(LUNOUT,'(2X,A)')
     -                          '++++++ ALGEXE DEBUG   : Unrecognised'//
     -                          ' RETURN option seen in the line:'
                           CALL ALGPRT(INEXT,INEXT)
                           WRITE(LUNOUT,'(26X,''Reg('',I3,'')='',E15.7,
     -                          '';  Reg('',I3,'')='',E15.7,/)')
     -                          INS(INEXT,1),REG(INS(INEXT,1)),
     -                          INS(INEXT,3),REG(INS(INEXT,3))
                      ENDIF
                      IFAIL=1
                      GOTO 3000
                 ENDIF
*   Invalid logical.
            ELSEIF(ABS(REG(INS(INEXT,1))).GT.EPS)THEN
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(2X,A)') '++++++ ALGEXE DEBUG   :'//
     -                     ' Logical value error detected in the line:'
                      CALL ALGPRT(INEXT,INEXT)
                      WRITE(LUNOUT,'(26X,''Reg('',I3,'')='',E15.7,
     -                     '';  Reg('',I3,'')='',E15.7,/)')
     -                     INS(INEXT,1),REG(INS(INEXT,1)),
     -                     INS(INEXT,3),REG(INS(INEXT,3))
                 ENDIF
                 IFAIL=1
                 GOTO 3000
            ENDIF
*** Algebraic instruction.
       ELSE
            IF((INS(INEXT,2).EQ.6.AND.MODREG(INS(INEXT,3)).EQ.0).OR.
     -           (INS(INEXT,2).NE.6.AND.(MODREG(INS(INEXT,1)).EQ.0.OR.
     -           MODREG(INS(INEXT,3)).EQ.0)))THEN
                 CALL ALGEX0(INEXT,IFAIL)
            ELSEIF((INS(INEXT,2).EQ.6.AND.MODREG(INS(INEXT,3)).EQ.2).OR.
     -           (INS(INEXT,2).NE.6.AND.MODREG(INS(INEXT,1)).EQ.2.AND.
     -           MODREG(INS(INEXT,3)).EQ.2))THEN
                 CALL ALGEX2(INEXT,IFAIL)
            ELSEIF((INS(INEXT,2).EQ.6.AND.MODREG(INS(INEXT,3)).EQ.3).OR.
     -           (INS(INEXT,2).NE.6.AND.MODREG(INS(INEXT,1)).EQ.3.AND.
     -           MODREG(INS(INEXT,3)).EQ.3))THEN
                 CALL ALGEX3(INEXT,IFAIL)
            ELSEIF((INS(INEXT,2).EQ.6.AND.MODREG(INS(INEXT,3)).EQ.1).OR.
     -           (INS(INEXT,2).NE.6.AND.MODREG(INS(INEXT,1)).EQ.1.AND.
     -           MODREG(INS(INEXT,3)).EQ.1))THEN
                 CALL ALGEX4(INEXT,IFAIL)
            ELSEIF((INS(INEXT,2).EQ.6.AND.MODREG(INS(INEXT,3)).EQ.4).OR.
     -           (INS(INEXT,2).NE.6.AND.MODREG(INS(INEXT,1)).EQ.4.OR.
     -           MODREG(INS(INEXT,3)).EQ.4))THEN
                 CALL ALGEX5(INEXT,IFAIL)
            ELSEIF((INS(INEXT,2).EQ.6.AND.MODREG(INS(INEXT,3)).EQ.5).OR.
     -           (INS(INEXT,2).NE.6.AND.MODREG(INS(INEXT,1)).EQ.5.OR.
     -           MODREG(INS(INEXT,3)).EQ.5))THEN
                 CALL ALGEX6(INEXT,IFAIL)
            ELSE
                 PRINT *,' !!!!!! ALGEXE WARNING : Unable to evaluate'//
     -                ' a variable because of mode incompatibility.'
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(26X,''Error occured in:'')')
                      CALL ALGPRT(INEXT,INEXT)
                      IF(INS(INEXT,2).NE.6)WRITE(LUNOUT,'(26X,''Reg '',
     -                     I3,'' = '',E15.7,'', Mode = '',I2,''.'')')
     -                     INS(INEXT,1),REG(INS(INEXT,1)),
     -                     MODREG(INS(INEXT,1))
                      WRITE(LUNOUT,'(26X,''Reg '',
     -                     I3,'' = '',E15.7,'', Mode = '',I2,''.'')')
     -                     INS(INEXT,3),REG(INS(INEXT,3)),
     -                     MODREG(INS(INEXT,3))
                 ENDIF
                 GOTO 3000
            ENDIF
            IF(IFAIL.NE.0)NERR=NERR+1
            IF(IFAIL.NE.0.AND.LDEBUG)THEN
                 WRITE(LUNOUT,'(1X,A)') ' ++++++ ALGEXE DEBUG   :'//
     -                ' Arithmetic error while evaluating:'
                 CALL ALGPRT(INEXT,INEXT)
                 IF(INS(INEXT,2).EQ.6)WRITE(LUNOUT,'(26X,''Reg('',I3,
     -                '')='',E15.7,/)') INS(INEXT,3),REG(INS(INEXT,3))
                 IF(INS(INEXT,2).NE.6)WRITE(LUNOUT,'(26X,''Reg('',I3,
     -                '')='',E15.7,'';  Reg('',I3,'')='',E15.7,/)')
     -                INS(INEXT,1),REG(INS(INEXT,1)),
     -                INS(INEXT,3),REG(INS(INEXT,3))
                 IFAIL=1
                 GOTO 3000
            ENDIF
       ENDIF
*** Next instruction.
       GOTO 20
*** Clean up temporary strings.
3000   CONTINUE
*   Loop over the instructions.
       DO 50 I=ALGENT(IENTNO,5),ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1
*   Skip results and control statements.
       IF(INS(I,2).EQ.0.OR.INS(I,2).EQ.8)GOTO 50
*   Select lines that result in string type variables.
       IF(MODREG(INS(I,4)).NE.1)GOTO 70
       DO 60 J=I+1,ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1
       IF(INS(J,2).EQ.0.AND.INS(J,3).EQ.INS(I,4))GOTO 70
60     CONTINUE
       DO 150 J=1,NGLB
       IF(GLBMOD(J).NE.1)GOTO 150
       IF(NINT(GLBVAL(J)).EQ.NINT(REG(INS(I,4))))GOTO 70
150    CONTINUE
       CALL STRBUF('DELETE',NINT(REG(INS(I,4))),DUMSTR,NCDUM,IFAIL1)
70     CONTINUE
*   Select lines that result in histogram type variables.
       IF(MODREG(INS(I,4)).NE.4)GOTO 80
       DO 90 J=I+1,ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1
       IF(INS(J,2).EQ.0.AND.INS(J,3).EQ.INS(I,4))GOTO 80
90     CONTINUE
       DO 110 J=1,NGLB
       IF(GLBMOD(J).NE.4)GOTO 110
       IF(NINT(GLBVAL(J)).EQ.NINT(REG(INS(I,4))))GOTO 80
110    CONTINUE
       CALL HISADM('DELETE',NINT(REG(INS(I,4))),0,0.0,0.0,.FALSE.,IDUM)
80     CONTINUE
*   Select lines that result in matrix type variables.
       IF(MODREG(INS(I,4)).NE.5)GOTO 120
       DO 130 J=I+1,ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1
       IF(INS(J,2).EQ.0.AND.INS(J,3).EQ.INS(I,4))GOTO 120
130    CONTINUE
       DO 140 J=1,NGLB
       IF(GLBMOD(J).NE.5)GOTO 140
       IF(NINT(GLBVAL(J)).EQ.NINT(REG(INS(I,4))))GOTO 120
140    CONTINUE
       CALL MATADM('DELETE',NINT(REG(INS(I,4))),0,IDUM,IDUM,IFAIL1)
120    CONTINUE
*   Next instruction.
50     CONTINUE
       END
