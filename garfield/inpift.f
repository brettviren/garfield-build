CDECK  ID>, INPIFT.
       SUBROUTINE INPIFT(REREAD,IFAIL)
*-----------------------------------------------------------------------
*   INPIFT - Checks IF structures outside a DO loop.
*   INPIFQ - Tells the status of the current level and one level below.
*   (Last changed on 14/ 4/00.)
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
       INTEGER TRACIF(0:MXILVL,2),INPCMP,CIFLVL,I,IFAIL,NWORD,IENTRY,NC,
     -      MODRES(1),NRES,NCPRM,ITHEN
       LOGICAL USE(MXVAR),ACTIVE(0:MXILVL),REREAD,IFCOND,ACT1,ACT2
       CHARACTER*(MXINCH) STRING
       CHARACTER*13 PROMPT
       REAL RES(1)
       EXTERNAL INPCMP
       SAVE TRACIF,CIFLVL,ACTIVE
*** Initial state.
       DATA CIFLVL /0/, ACTIVE(0) /.TRUE./
       DATA (TRACIF(0,I),I=1,2) /0,0/
*** Number of words is needed frequently.
       CALL INPNUM(NWORD)
*   Locate the THEN, if there is one.
       DO 10 I=1,NWORD
       IF(INPCMP(I,'THEN').NE.0)THEN
            ITHEN=I
            GOTO 20
       ENDIF
10     CONTINUE
       ITHEN=0
20     CONTINUE
*   Would usually work.
       IFAIL=0
       REREAD=.FALSE.
*** This routine should not touch a start of DO loop.
       IF(INPCMP(1,'IF').NE.0.AND.ITHEN.NE.0.AND.
     -      INPCMP(NWORD,'DO').NE.0.AND.NWORD.GT.ITHEN)THEN
            RETURN
*** Ensure that THEN does not follow IF immediately.
       ELSEIF((INPCMP(1,'IF').NE.0.OR.INPCMP(1,'ELSEIF').NE.0).AND.
     -      ITHEN.LE.2)THEN
            PRINT *,' !!!!!! INPIFT WARNING : Empty clause in an IF'//
     -           ' or ELSEIF line; line ignored.'
            IFAIL=1
*** Check whether this is an IF-line.
       ELSEIF(NWORD.GT.ITHEN.AND.INPCMP(1,'IF').NE.0.AND.ITHEN.NE.0)THEN
*   Active area, see what the condition looks like.
            IF(ACTIVE(CIFLVL))THEN
                 CALL INPSTR(2,ITHEN-1,STRING,NC)
                 CALL ALGPRE(STRING(1:NC),NC,GLBVAR,NGLB,NRES,
     -                USE,IENTRY,IFAIL)
                 IFCOND=.FALSE.
                 IF(IFAIL.NE.0.OR.NRES.NE.1)THEN
                      PRINT *,' !!!!!! INPIFT WARNING : Failed to'//
     -                     ' translate condition of IF-line;'//
     -                     ' assumed not to hold.'
                      IFCOND=.FALSE.
                 ELSE
                      CALL TIMEL(GLBVAL(1))
                      CALL ALGEXE(IENTRY,GLBVAL,GLBMOD,NGLB,
     -                     RES,MODRES,NRES,IFAIL)
                      IF(IFAIL.EQ.0.AND.MODRES(1).NE.3)THEN
                           PRINT *,' !!!!!! INPIFT WARNING : '//
     -                          STRING(1:NC)//' does not evaluate'//
     -                          ' to a logical; assumed not to hold.'
                      ELSEIF(IFAIL.EQ.0.AND.ABS(RES(1)).LT.1.0E-5)THEN
                           IFCOND=.FALSE.
                      ELSEIF(IFAIL.EQ.0.AND.ABS(RES(1)-1).LT.1.0E-5)THEN
                           IFCOND=.TRUE.
                      ELSE
                           PRINT *,' !!!!!! INPIFT WARNING : Failed'//
     -                          ' to evaluate '//STRING(1:NC)//
     -                          ' ; assumed not to hold.'
                           IFCOND=.FALSE.
                      ENDIF
                 ENDIF
                 CALL ALGCLR(IENTRY)
*   If the condition holds, delete the first words and have executed.
                 IF(IFCOND)THEN
                      DO 30 I=ITHEN,1,-1
                      CALL INPDEL(I)
30                    CONTINUE
                      NWORD=NWORD-ITHEN
                      REREAD=.FALSE.
*   If not, just read the new line.
                 ELSE
                      REREAD=.TRUE.
                 ENDIF
*   Inactive area, also read a new line no matter the condition.
            ELSE
                 REREAD=.TRUE.
            ENDIF
*** Check whether this is an IF block piece.
       ELSEIF(INPCMP(1,'IF').NE.0.AND.ITHEN.NE.0)THEN
*   Check whether we may still increase the IF level.
            IF(CIFLVL.GE.MXILVL)THEN
                 PRINT *,' !!!!!! INPIFT WARNING : The IF blocks'//
     -                ' are nested too deep; IF ignored.'
                 IFAIL=1
            ELSE
*   Check whether this is the first IF, if so add prompt.
                 IF(CIFLVL.EQ.0)CALL INPPRM('If','ADD')
*   Increment level counter.
                 CIFLVL=CIFLVL+1
*   Add the new block to the trace.
                 TRACIF(CIFLVL,1)=1
                 TRACIF(CIFLVL,2)=0
*   The activity starts out the same as at the previous level.
                 ACTIVE(CIFLVL)=ACTIVE(CIFLVL-1)
*   If we are inside an accepted region, evaluate and execute.
                 IF(ACTIVE(CIFLVL))THEN
                      CALL INPSTR(2,ITHEN-1,STRING,NC)
                      CALL ALGPRE(STRING(1:NC),NC,GLBVAR,NGLB,NRES,
     -                     USE,IENTRY,IFAIL)
                      IFCOND=.FALSE.
                      IF(IFAIL.NE.0.OR.NRES.NE.1)THEN
                           PRINT *,' !!!!!! INPIFT WARNING : Failed'//
     -                          ' to translate condition of IF-block;'//
     -                          ' assumed not to hold.'
                           IFCOND=.FALSE.
                      ELSE
                           CALL TIMEL(GLBVAL(1))
                           CALL ALGEXE(IENTRY,GLBVAL,GLBMOD,NGLB,
     -                          RES,MODRES,NRES,IFAIL)
                           IF(IFAIL.EQ.0.AND.MODRES(1).NE.3)THEN
                                PRINT *,' !!!!!! INPIFT WARNING : '//
     -                               STRING(1:NC)//' does not'//
     -                               ' evaluate to a logical;'//
     -                               ' assumed not to hold.'
                           ELSEIF(IFAIL.EQ.0.AND.
     -                          ABS(RES(1)).LT.1.0E-5)THEN
                                IFCOND=.FALSE.
                           ELSEIF(IFAIL.EQ.0.AND.
     -                          ABS(RES(1)-1).LT.1.0E-5)THEN
                                IFCOND=.TRUE.
                           ELSE
                                PRINT *,' !!!!!! INPIFT WARNING :'//
     -                               ' Failed to evaluate '//
     -                               STRING(1:NC)//'; assumed not'//
     -                               ' to hold.'
                                IFCOND=.FALSE.
                           ENDIF
                      ENDIF
                      CALL ALGCLR(IENTRY)
*   If the condition holds, mark block as executed.
                      IF(IFCOND)THEN
                           TRACIF(CIFLVL,2)=1
*   Otherwise mark this area is inactive.
                      ELSE
                           ACTIVE(CIFLVL)=.FALSE.
                      ENDIF
                 ENDIF
            ENDIF
*   Whatever happened, read a new line.
            REREAD=.TRUE.
*** Ensure this is not an attempt at an ELSEIF ... THEN command.
       ELSEIF(NWORD.GT.ITHEN.AND.INPCMP(1,'ELSEIF').NE.0.AND.
     -      ITHEN.NE.0)THEN
            PRINT *,' !!!!!! INPIFT WARNING : An ELSEIF line can not'//
     -           ' have a command on it; line ignored.'
            IFAIL=1
*** Check whether this is an ELSEIF branch.
       ELSEIF(INPCMP(1,'ELSEIF').NE.0.AND.ITHEN.NE.0)THEN
*   Check whether we are really inside an IF block.
            IF(CIFLVL.LE.0)THEN
                 PRINT *,' !!!!!! INPIFT WARNING : An ELSEIF may only'//
     -                ' occur inside an IF-block; ignored.'
                 IFAIL=1
*   Check this ELSEIF was not preceded by an ELSE.
            ELSEIF(TRACIF(CIFLVL,1).GE.3)THEN
                 PRINT *,' !!!!!! INPIFT WARNING : An ELSEIF may not'//
     -                ' follow an ELSE in the same IF-block; ignored.'
                 IFAIL=1
*   Already executed IF block.
            ELSEIF(TRACIF(CIFLVL,2).EQ.1)THEN
                 ACTIVE(CIFLVL)=.FALSE.
*   Check condition if embedding block is active and block not yet ex.
            ELSEIF(ACTIVE(CIFLVL-1).AND.TRACIF(CIFLVL,2).EQ.0)THEN
                 CALL INPSTR(2,ITHEN-1,STRING,NC)
                 CALL ALGPRE(STRING(1:NC),NC,GLBVAR,NGLB,NRES,
     -                USE,IENTRY,IFAIL)
                 IFCOND=.FALSE.
                 IF(IFAIL.NE.0.OR.NRES.NE.1)THEN
                      PRINT *,' !!!!!! INPIFT WARNING : Failed to'//
     -                     ' translate condition of an ELSEIF'//
     -                     ' line; assumed not to hold.'
                      IFCOND=.FALSE.
                 ELSE
                      CALL TIMEL(GLBVAL(1))
                      CALL ALGEXE(IENTRY,GLBVAL,GLBMOD,NGLB,
     -                     RES,MODRES,NRES,IFAIL)
                      IF(IFAIL.EQ.0.AND.MODRES(1).NE.3)THEN
                           PRINT *,' !!!!!! INPIFT WARNING : '//
     -                          STRING(1:NC)//' does not evaluate'//
     -                          ' to a logical; assumed not to hold.'
                      ELSEIF(IFAIL.EQ.0.AND.ABS(RES(1)).LT.1.0E-5)THEN
                           IFCOND=.FALSE.
                      ELSEIF(IFAIL.EQ.0.AND.ABS(RES(1)-1).LT.1.0E-5)THEN
                           IFCOND=.TRUE.
                      ELSE
                           PRINT *,' !!!!!! INPIFT WARNING : Failed'//
     -                          ' to evaluate '//STRING(1:NC)//
     -                          ' ; assumed not to hold.'
                           IFCOND=.FALSE.
                      ENDIF
                 ENDIF
                 CALL ALGCLR(IENTRY)
*   If the condition holds, make active and mark block as executed.
                 IF(IFCOND)THEN
                      TRACIF(CIFLVL,2)=1
                      ACTIVE(CIFLVL)=.TRUE.
*   Otherwise mark area as inactive.
                 ELSE
                      ACTIVE(CIFLVL)=.FALSE.
                 ENDIF
            ENDIF
*   Remember we saw an ELSEIF line but don't overrule an ELSE.
            TRACIF(CIFLVL,1)=MAX(2,TRACIF(CIFLVL,1))
*   Always read a new line.
            REREAD=.TRUE.
*** Warn for an ELSE outside an IF block.
       ELSEIF(INPCMP(1,'ELSE').NE.0.AND.CIFLVL.LE.0)THEN
            PRINT *,' !!!!!! INPIFT WARNING : An ELSE may only occur'//
     -           ' inside an IF-block; line ignored.'
            IFAIL=1
*** Warn for an ELSE with additional words.
       ELSEIF(INPCMP(1,'ELSE').NE.0.AND.NWORD.GT.1)THEN
            PRINT *,' !!!!!! INPIFT WARNING : An ELSE line may not'//
     -           ' have a command on it; line ignored.'
            IFAIL=1
*** An ELSE part of an IF block.
       ELSEIF(INPCMP(1,'ELSE').NE.0)THEN
*   Check this ELSEIF was not preceded by an ELSE.
            IF(TRACIF(CIFLVL,1).GE.3)THEN
                 PRINT *,' !!!!!! INPIFT WARNING : There may not be'//
     -                ' two ELSE parts in the same IF-block; ignored.'
                 IFAIL=1
*   Already executed IF block.
            ELSEIF(TRACIF(CIFLVL,2).EQ.1)THEN
                 ACTIVE(CIFLVL)=.FALSE.
*   Execute active area of not yet executed IF block.
            ELSEIF(ACTIVE(CIFLVL-1).AND.TRACIF(CIFLVL,2).EQ.0)THEN
                 TRACIF(CIFLVL,2)=1
                 ACTIVE(CIFLVL)=.TRUE.
            ENDIF
*   Remember we saw an ELSE line but don't overrule an ENDIF.
            TRACIF(CIFLVL,1)=MAX(3,TRACIF(CIFLVL,1))
*   Always read a new line.
            REREAD=.TRUE.
*** Warn for an ENDIF line outside an IF block.
       ELSEIF(INPCMP(1,'ENDIF').NE.0.AND.CIFLVL.LE.0)THEN
            PRINT *,' !!!!!! INPIFT WARNING : An ENDIF may only occur'//
     -           ' inside an IF-block; line ignored.'
            IFAIL=1
*** Warn for an attempt of an ENDIF with additional words.
       ELSEIF(INPCMP(1,'ENDIF').NE.0.AND.NWORD.GT.1)THEN
            PRINT *,' !!!!!! INPIFT WARNING : An ENDIF line may not'//
     -           ' have a command on it; line ignored.'
            IFAIL=1
*** The ENDIF part of a block.
       ELSEIF(INPCMP(1,'ENDIF').NE.0)THEN
*   Check whether we are really inside an IF block.
            IF(CIFLVL.LE.0)THEN
                 PRINT *,' !!!!!! INPIFT WARNING : An ENDIF may only'//
     -                ' occur at the end of an IF-block; ignored.'
                 IFAIL=1
*   In other cases, just go back by one level.
            ELSE
                 TRACIF(CIFLVL,1)=4
                 CIFLVL=CIFLVL-1
                 CALL INPPRM(' ','BACK')
            ENDIF
*   Reread always.
            REREAD=.TRUE.
*** Any other line.
       ELSE
            REREAD=.NOT.ACTIVE(CIFLVL)
       ENDIF
*** Update the prompt.
       PROMPT=' '
       IF(CIFLVL.GT.0)THEN
            WRITE(PROMPT,'(''If_'',I10)') CIFLVL
            NCPRM=0
            DO 400 I=1,13
            IF(PROMPT(I:I).NE.' ')THEN
                 NCPRM=NCPRM+1
                 PROMPT(NCPRM:NCPRM)=PROMPT(I:I)
            ENDIF
400         CONTINUE
            CALL INPPRM(' ','BACK')
            CALL INPPRM(PROMPT(1:MAX(1,NCPRM)),'ADD')
       ENDIF
*** Normal end of this routine.
       RETURN
*** Entry for quick check whether substitution must be carried out.
       ENTRY INPIFQ(ACT1,ACT2)
       ACT1=ACTIVE(MAX(0,CIFLVL-1))
       ACT2=ACTIVE(CIFLVL)
       END
