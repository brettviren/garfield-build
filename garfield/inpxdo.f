CDECK  ID>, INPXDO.
       SUBROUTINE INPXDO(STRING,NC,IFLAG)
*-----------------------------------------------------------------------
*   INPXDO - Executes a DO loop and returns commands.
*   (Last changed on  2/11/01.)
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
       INTEGER DOREF,IFREF,LINREF,CURLIN,CDOLVL,CIFLVL,TRACDO,TRACIF,
     -      ISTATE,NDOLIN,NLOOP,NIF
       COMMON /DODAT/ LINREF(MXDLIN,8),DOREF(MXDLVL,12),IFREF(MXILVL,5),
     -      TRACDO(0:MXDLVL),TRACIF(0:MXILVL),CURLIN,CDOLVL,CIFLVL,
     -      NDOLIN,NLOOP,NIF,ISTATE
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(*) STRING
       REAL RES(11)
       INTEGER NC,IFLAG,OLDLVL,MODRES(11),ILOOP,IFAIL1,IFAIL2,IFAIL3,
     -      IFAIL4,IFAIL5,IFAIL11,IFAIL,IBLOCK,I,MATSLT,IREF
       LOGICAL IFCOND
       EXTERNAL MATSLT
*** Be sure this routine is entered legally.
       IF(NDOLIN.LE.0.OR.NLOOP.LE.0.OR.ISTATE.LT.0)THEN
            PRINT *,' ###### INPXDO ERROR   : No valid DO loop'//
     -           ' stored; routine should not have been called.'
            IFLAG=-1
            RETURN
       ENDIF
*** Initial settings.
       IF(ISTATE.EQ.0)THEN
            CURLIN=0
            CDOLVL=0
            ISTATE=1
       ENDIF
*** Return at this point if a new line has to be read.
10     CONTINUE
*** Increment line counter.
       CURLIN=CURLIN+1
*   Check we're still in the loop.
       IF(CURLIN.GT.NDOLIN)THEN
            PRINT *,' ------ INPXDO MESSAGE : End of loop reached.'
            CALL ALGERR
            IFLAG=+2
            IF(CDOLVL.NE.0)THEN
                 PRINT *,' ###### INPXDO ERROR   : The loop is left'//
     -                ' at a non-zero level: ',CDOLVL,'.'
                 PRINT *,'                         Program bug -'//
     -                ' please report; all loops ended.'
                 IFLAG=-1
            ENDIF
            GOTO 3000
       ENDIF
*   Evaluate the IF condition, if present.
       IF(LINREF(CURLIN,4).GT.0)THEN
            CALL TIMEL(GLBVAL(1))
            CALL ALGEXE(LINREF(CURLIN,4),GLBVAL,GLBMOD,NGLB,
     -           RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPXDO WARNING : Failure to'//
     -                ' figure out the value of the IF condition.'
                 IFCOND=.TRUE.
            ELSEIF(ABS(RES(1)).LT.1.0E-5)THEN
                 IFCOND=.FALSE.
            ELSEIF(ABS(1.0-RES(1)).LT.1.0E-5)THEN
                 IFCOND=.TRUE.
            ELSE
                 PRINT *,' !!!!!! INPXDO WARNING : The IF'//
     -                ' condition does not evaluate to a logical.'
                 IFCOND=.TRUE.
            ENDIF
       ELSE
            IFCOND=.TRUE.
       ENDIF
*   Make sure the line number is not negative.
       IF(CURLIN.LE.0)THEN
            PRINT *,' ###### INPXDO ERROR   : Negative line number'//
     -           ' encountered: ',CURLIN,'.'
            PRINT *,'                         Program bug -'//
     -           ' please report; all loops ended.'
            IFLAG=-1
            GOTO 3000
       ENDIF
*** Ordinary line, return to have it executed.
       IF(LINREF(CURLIN,1).EQ.0.AND.IFCOND)THEN
            CALL STRBUF('READ',LINREF(CURLIN,2),STRING,NC,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' ###### INPXDO ERROR   : Error retrieving'//
     -                ' a line of the DO loop nest; all loops ended.'
                 IFLAG=-1
                 GOTO 3000
            ENDIF
            IF(LINREF(CURLIN,4).EQ.0)THEN
                 IFLAG=0
            ELSE
                 IFLAG=+1
            ENDIF
            RETURN
*** Ordinary line, not to be executed.
       ELSEIF(LINREF(CURLIN,1).EQ.0)THEN
            GOTO 10
*** Start of a DO loop.
       ELSEIF(LINREF(CURLIN,1).EQ.1)THEN
*   Pick up the index of this DO loop.
            ILOOP=LINREF(CURLIN,3)
*   Maybe the whole DO loop shouldn't be executed.
            IF(.NOT.IFCOND)THEN
                 CURLIN=DOREF(ILOOP,7)
                 GOTO 10
            ENDIF
*   We will almost certainly need the time left.
            CALL TIMEL(GLBVAL(1))
**  In case of an In loop over a matrix, set initial value.
            IF(DOREF(ILOOP,9).GT.0.AND.DOREF(ILOOP,11).GT.0)THEN
*   Evaluate the matrix.
                 CALL ALGEXE(DOREF(ILOOP,11),GLBVAL,GLBMOD,NGLB,
     -                RES(11),MODRES(11),1,IFAIL11)
                 IF(IFAIL11.NE.0)THEN
                      PRINT *,' ###### INPXDO ERROR   : Error'//
     -                     ' evaluating the In matrix;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ELSEIF(MODRES(11).NE.5)THEN
                      PRINT *,' ###### INPXDO ERROR   : In'//
     -                     ' does not evaluate to a matrix;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ENDIF
*   Store the initial matrix pointer.
                 DOREF(ILOOP,12)=1
*   Compute the matrix reference.
                 IREF=MATSLT(NINT(RES(11)))
*   Ensure this value is within the matrix.
                 IF(DOREF(ILOOP,12).GT.MLEN(IREF))THEN
                      CURLIN=DOREF(ILOOP,7)
                      GOTO 10
                 ENDIF
*   Store the initial value.
                 GLBVAL(DOREF(ILOOP,9))=MVEC(MORG(IREF)+DOREF(ILOOP,12))
                 GLBMOD(DOREF(ILOOP,9))=2
**  In case of a From-Step-To loop with variable, handle initial value.
            ELSEIF(DOREF(ILOOP,9).GT.0)THEN
*   Evaluate initial value, step size and final value.
                 CALL ALGEXE(DOREF(ILOOP,1),GLBVAL,GLBMOD,NGLB,
     -                RES(1),MODRES(1),1,IFAIL1)
                 CALL ALGEXE(DOREF(ILOOP,2),GLBVAL,GLBMOD,NGLB,
     -                RES(2),MODRES(2),1,IFAIL2)
                 CALL ALGEXE(DOREF(ILOOP,5),GLBVAL,GLBMOD,NGLB,
     -                RES(5),MODRES(5),1,IFAIL5)
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL5.NE.0)THEN
                      PRINT *,' ###### INPXDO ERROR   : Error'//
     -                     ' evaluating From, Step and To;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ELSEIF(MODRES(1).NE.2.OR.MODRES(2).NE.2.OR.
     -                MODRES(5).NE.2)THEN
                      PRINT *,' ###### INPXDO ERROR   : From, Step'//
     -                     ' or To does not evaluate to a number;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ENDIF
*   Store initial value.
                 GLBVAL(DOREF(ILOOP,9))=RES(1)
                 GLBMOD(DOREF(ILOOP,9))=MODRES(1)
*   Check that we are between From and To.
                 IF((GLBVAL(DOREF(ILOOP,9)).GT.RES(5).AND.
     -                RES(2).GT.0.0).OR.
     -                (GLBVAL(DOREF(ILOOP,9)).LT.RES(5).AND.
     -                RES(2).LT.0.0))THEN
                      CURLIN=DOREF(ILOOP,7)
                      GOTO 10
                 ENDIF
            ENDIF
**  Evaluate the WHILE condition.
            CALL ALGEXE(DOREF(ILOOP,3),GLBVAL,GLBMOD,NGLB,
     -           RES(3),MODRES(3),1,IFAIL3)
            IF(IFAIL3.NE.0)THEN
                 PRINT *,' ###### INPXDO ERROR   : Error evaluating'//
     -                ' While; all loops ended.'
                 IFLAG=-1
                 GOTO 3000
            ELSEIF(MODRES(3).NE.3)THEN
                 PRINT *,' ###### INPXDO ERROR   : While condition'//
     -                ' does not evaluate to a logical; loops ended.'
                 IFLAG=-1
                 GOTO 3000
            ENDIF
*   Check WHILE is still satisfied.
            IF(ABS(RES(3)).LT.1.0E-3)THEN
                 CURLIN=DOREF(ILOOP,7)
                 GOTO 10
            ENDIF
*   Increment the level counter and keep the trace.
            CDOLVL=CDOLVL+1
            TRACDO(CDOLVL)=LINREF(CURLIN,3)
*   Read the first line of the loop.
            GOTO 10
*** LEAVE the loop altogether and condition satisfied.
       ELSEIF(LINREF(CURLIN,1).EQ.3.AND.IFCOND)THEN
*   Decrease the level counter.
            OLDLVL=CDOLVL
            DO 20 I=OLDLVL,1,-1
            IF(TRACDO(I).NE.LINREF(CURLIN,3))THEN
                 CDOLVL=CDOLVL-1
            ELSE
                 GOTO 30
            ENDIF
20          CONTINUE
            PRINT *,' !!!!!! INPXDO WARNING : LEAVE fails, the'//
     -           ' loop to be left is not in the stack.'
            IFLAG=-1
            GOTO 3000
30          CONTINUE
            CDOLVL=CDOLVL-1
*   Also set the new IF level.
            CIFLVL=DOREF(LINREF(CURLIN,3),10)
*   Next line to be read is just after the ENDDO.
            CURLIN=DOREF(LINREF(CURLIN,3),7)
*   Read that line.
            GOTO 10
*** LEAVE but IF condition not satisfied.
       ELSEIF(LINREF(CURLIN,1).EQ.3)THEN
            GOTO 10
*** Next iteration, either via an ITERATE or an ENDDO.
       ELSEIF((LINREF(CURLIN,1).EQ.2.AND.IFCOND).OR.
     -      LINREF(CURLIN,1).EQ.4)THEN
*   Decrease the level counter in case of an ITERATE.
            IF(LINREF(CURLIN,1).EQ.2)THEN
                 OLDLVL=CDOLVL
                 DO 40 I=OLDLVL,1,-1
                 IF(TRACDO(I).NE.LINREF(CURLIN,3))THEN
                      CDOLVL=CDOLVL-1
                 ELSE
                      GOTO 50
                 ENDIF
40               CONTINUE
                 PRINT *,' !!!!!! INPXDO WARNING : ITERATE fails,'//
     -                ' loop to be returned to is not in the stack.'
                 IFLAG=-1
                 GOTO 3000
50               CONTINUE
            ENDIF
*   Pick up the target loop index.
            ILOOP=LINREF(CURLIN,3)
*   Also set the new IF level.
            CIFLVL=DOREF(ILOOP,10)
*   We will almost certainly need the time left.
            CALL TIMEL(GLBVAL(1))
**  In-Loop with variable.
            IF(DOREF(ILOOP,9).GT.0.AND.DOREF(ILOOP,11).GT.0)THEN
*   Evaluate the matrix.
                 CALL ALGEXE(DOREF(ILOOP,11),GLBVAL,GLBMOD,NGLB,
     -                RES(11),MODRES(11),1,IFAIL11)
                 IF(IFAIL11.NE.0)THEN
                      PRINT *,' ###### INPXDO ERROR   : Error'//
     -                     ' evaluating the In matrix;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ELSEIF(MODRES(11).NE.5)THEN
                      PRINT *,' ###### INPXDO ERROR   : In'//
     -                     ' does not evaluate to a matrix;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ENDIF
*   Increment the matrix pointer.
                 DOREF(ILOOP,12)=DOREF(ILOOP,12)+1
*   Compute the matrix reference.
                 IREF=MATSLT(NINT(RES(11)))
*   Ensure this value is within the matrix.
                 IF(DOREF(ILOOP,12).GT.MLEN(IREF))THEN
                      CURLIN=DOREF(ILOOP,7)
                      CDOLVL=CDOLVL-1
                      GOTO 10
                 ENDIF
*   Store the new value.
                 GLBVAL(DOREF(ILOOP,9))=MVEC(MORG(IREF)+DOREF(ILOOP,12))
                 GLBMOD(DOREF(ILOOP,9))=2
*   Loop with variable: handle the loop variable.
            ELSEIF(DOREF(ILOOP,9).GT.0)THEN
*   Additionally evaluate increment and final value.
                 CALL ALGEXE(DOREF(ILOOP,2),GLBVAL,GLBMOD,NGLB,
     -                RES(2),MODRES(2),1,IFAIL2)
                 CALL ALGEXE(DOREF(ILOOP,5),GLBVAL,GLBMOD,NGLB,
     -                RES(5),MODRES(5),1,IFAIL5)
                 IF(IFAIL2.NE.0.OR.IFAIL5.NE.0)THEN
                      PRINT *,' ###### INPXDO ERROR   : Error'//
     -                     '  evaluating Step and To; all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ELSEIF(MODRES(2).NE.2.OR.MODRES(5).NE.2)THEN
                      PRINT *,' ###### INPXDO ERROR   : Step'//
     -                     ' or To does not evaluate to a number;'//
     -                     ' all loops ended.'
                      IFLAG=-1
                      GOTO 3000
                 ENDIF
*   Increment the loop variable.
                 GLBVAL(DOREF(ILOOP,9))=GLBVAL(DOREF(ILOOP,9))+RES(2)
*   Check the final value is not yet exceeded.
                 IF((GLBVAL(DOREF(ILOOP,9)).GT.RES(5).AND.
     -                RES(2).GT.0.0).OR.
     -                (GLBVAL(DOREF(ILOOP,9)).LT.RES(5).AND.
     -                RES(2).LT.0.0))THEN
                      CURLIN=DOREF(ILOOP,7)
                      CDOLVL=CDOLVL-1
                      GOTO 10
                 ENDIF
            ENDIF
*   Evaluate the WHILE and UNTIL portions, which are always needed.
            CALL ALGEXE(DOREF(ILOOP,3),GLBVAL,GLBMOD,NGLB,
     -           RES(3),MODRES(3),1,IFAIL3)
            CALL ALGEXE(DOREF(ILOOP,4),GLBVAL,GLBMOD,NGLB,
     -           RES(4),MODRES(4),1,IFAIL4)
            IF(IFAIL3.NE.0.OR.IFAIL4.NE.0)THEN
                 PRINT *,' ###### INPXDO ERROR   : Error evaluating'//
     -                ' While and Until; all loops ended.'
                 IFLAG=-1
                 GOTO 3000
            ELSEIF(MODRES(3).NE.3.OR.MODRES(4).NE.3)THEN
                 PRINT *,' ###### INPXDO ERROR   : While or Until'//
     -                ' does not evaluate to a logical; loops ended.'
                 IFLAG=-1
                 GOTO 3000
            ENDIF
*   Check the WHILE and UNTIL control expressions.
            IF(ABS(RES(3)).LT.1.0E-3.OR.ABS(RES(4)-1.0).LT.1.0E-3)THEN
                 CURLIN=DOREF(ILOOP,7)
                 CDOLVL=CDOLVL-1
                 GOTO 10
            ENDIF
*   Return to the first line of the loop if all else fails.
            CURLIN=DOREF(ILOOP,6)
            GOTO 10
*** ITERATE but condition not satisfied.
       ELSEIF(LINREF(CURLIN,1).EQ.2.AND..NOT.IFCOND)THEN
            GOTO 10
*** Start of an IF block.
       ELSEIF(LINREF(CURLIN,1).EQ.11)THEN
*   Pick up the block number for easier reference.
            IBLOCK=LINREF(CURLIN,6)
*   Set the new line depending on the value of the IF condition.
            IF(IFCOND)THEN
                 IFREF(IBLOCK,3)=1
            ELSE
                 IFREF(IBLOCK,3)=0
                 CURLIN=LINREF(CURLIN,5)-1
            ENDIF
*   We always go up by one level in the IF tree.
            CIFLVL=CIFLVL+1
            TRACIF(CIFLVL)=IBLOCK
            GOTO 10
*** An ELSEIF branch.
       ELSEIF(LINREF(CURLIN,1).EQ.12)THEN
*   Pick up the block number for easier reference.
            IBLOCK=LINREF(CURLIN,6)
*   Check whether we have already done one branch.
            IF(IFREF(IBLOCK,3).EQ.1)THEN
                 CURLIN=IFREF(IBLOCK,2)-1
                 GOTO 10
            ENDIF
*   Set the new line depending on the value of the IF condition.
            IF(IFCOND)THEN
                 IFREF(IBLOCK,3)=1
            ELSE
                 IFREF(IBLOCK,3)=0
                 CURLIN=LINREF(CURLIN,5)-1
            ENDIF
            GOTO 10
*** An ELSE branch.
       ELSEIF(LINREF(CURLIN,1).EQ.13)THEN
*   Pick up the block number for easier reference.
            IBLOCK=LINREF(CURLIN,6)
*   Check whether we have already done one branch.
            IF(IFREF(IBLOCK,3).EQ.1)THEN
                 CURLIN=IFREF(IBLOCK,2)-1
                 GOTO 10
            ENDIF
*   The next part should be executed anyhow.
            IFREF(IBLOCK,3)=1
            GOTO 10
*** The ENDIF part of the IF block, just decrement.
       ELSEIF(LINREF(CURLIN,1).EQ.14)THEN
            CIFLVL=CIFLVL-1
            GOTO 10
*** A GLOBAL variable is redefined.
       ELSEIF(LINREF(CURLIN,1).EQ.21)THEN
*   Check IF condition.
            IF(.NOT.IFCOND)GOTO 10
*   If satisfied, evaluate the Global.
            CALL TIMEL(GLBVAL(1))
            CALL ALGEXE(LINREF(CURLIN,8),GLBVAL,GLBMOD,NGLB,
     -           RES,MODRES,1,IFAIL)
            IF(LINREF(CURLIN,7).LE.0)THEN
                 IF(IFAIL.NE.0)PRINT *,' !!!!!! INPXDO WARNING :'//
     -                ' Sub-matrix assignment in Global statement'//
     -                ' has failed.'
            ELSEIF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPXDO WARNING : Error evaluating'//
     -                ' a GLOBAL expression; set to Undefined.'
                 CALL ALGREU(NINT(GLBVAL(LINREF(CURLIN,7))),
     -                GLBMOD(LINREF(CURLIN,7)),0)
                 GLBVAL(LINREF(CURLIN,7))=0
                 GLBMOD(LINREF(CURLIN,7))=0
            ELSE
                 CALL ALGREU(NINT(GLBVAL(LINREF(CURLIN,7))),
     -                GLBMOD(LINREF(CURLIN,7)),0)
                 GLBVAL(LINREF(CURLIN,7))=RES(1)
                 GLBMOD(LINREF(CURLIN,7))=MODRES(1)
            ENDIF
            GOTO 10
*** A CALL statement.
       ELSEIF(LINREF(CURLIN,1).EQ.22)THEN
*   Check IF condition.
            IF(.NOT.IFCOND)GOTO 10
*   If satisfied, execute the Call.
            CALL TIMEL(GLBVAL(1))
            CALL ALGEXE(LINREF(CURLIN,8),GLBVAL,GLBMOD,NGLB,
     -           RES,MODRES,1,IFAIL)
            IF(IFAIL.NE.0)PRINT *,' !!!!!! INPXDO WARNING : Error'//
     -           ' executing a CALL statement.'
            GOTO 10
*** Unrecognised instruction.
       ELSE
            PRINT *,' !!!!!! INPXDO WARNING : Unrecognised line'//
     -           ' type seen; loop is left.'
            IFLAG=-1
            GOTO 3000
       ENDIF
*** End of loop cleanup.
3000   CONTINUE
       CALL INPCDO
       END
