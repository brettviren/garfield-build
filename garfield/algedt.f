CDECK  ID>, ALGEDT.
       SUBROUTINE ALGEDT(VARLIS,NVAR,IENTRY,USE,NREXP)
*-----------------------------------------------------------------------
*   ALGEDT - Reads instructions relating to formula manipulation. It
*            serves as a section but will rarely be used as such by the
*            normal user.
*   (Last changed on 27/11/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IBUF(4),INPCMP,INPTYP,MODVAR(MXVAR),MODRES(10),ILIST1,
     -      ILIST,ILIST2,IFAIL,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IENT,IENUPD,
     -      IDEL,ICOPY,IENCLR,IEXEC,I,J,I1,I2,NCPRT,NC,NWORD,IENTRR,
     -      IENDSP,NVAR,NREXP,IENTRY,INS0,INSC,IPRINT,NNRES,IENTNO,
     -      NC1,NC2
       CHARACTER*10 VARLIS(MXVAR)
       CHARACTER*30 AUX1,AUX2
       CHARACTER*(MXINCH) STRING
       LOGICAL USE(MXVAR)
       REAL RES(10),VAR(MXVAR)
       EXTERNAL INPCMP,INPTYP
*** Define some output formats.
1010   FORMAT(' ',25X,'Reg(',I3,')=',E15.8:';  Reg(',I3,')=',E15.8)
*** Print a header for this section.
       WRITE(*,'(''1'')')
       PRINT *,' ------------------------------------------------'
       PRINT *,' ----------     Algebra subsection     ----------'
       PRINT *,' ------------------------------------------------'
       PRINT *,' '
*** Assign an entry point to the instruction list.
       IENTRY=IENTRL+1
       IENTRL=IENTRL+1
       IINS0=NINS+1
       ICONS0=NCONS-1
*   Check storage, perform a garbage collect if necessary.
       IF(NALGE+1.GT.MXALGE)THEN
            CALL ALGGBC
            IF(NALGE+1.GT.MXALGE)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Unable to allocate'//
     -                ' an entry point to the instruction list.'
                 PRINT *,'                         Increase MXALGE'//
     -                ' and recompile the program.'
                 IFAIL=1
                 IENTRY=-1
                 RETURN
            ENDIF
       ENDIF
       NALGE=NALGE+1
*   Initialise the entry point record.
       ALGENT(NALGE,1)=IENTRY
       ALGENT(NALGE,2)=1
       ALGENT(NALGE,3)=0
       ALGENT(NALGE,4)=0
       ALGENT(NALGE,5)=IINS0
       ALGENT(NALGE,6)=0
       ALGENT(NALGE,7)=NVAR
       ALGENT(NALGE,8)=ICONS0
       ALGENT(NALGE,9)=0
       ALGENT(NALGE,10)=0
*** Read instructions and make some simple checks.
       CALL INPPRM('Algebra','ADD-PRINT')
10     CONTINUE
       CALL INPGET
       CALL INPNUM(NWORD)
       CALL INPSTR(1,1,STRING,NC)
       IF(NWORD.EQ.0)GOTO 10
*** Avoid that this routine is left using '&'.
       IF(STRING(1:1).EQ.'&')THEN
            PRINT *,' !!!!!! ALGEDT WARNING : The section cannot be'//
     -           ' left at this point; first type EXIT.'
            GOTO 10
       ELSEIF(INDEX('$%?><!',STRING(1:1)).NE.0)THEN
            PRINT *,' !!!!!! ALGEDT WARNING : This command cannot be'//
     -           ' executed at the present level; first type EXIT.'
            GOTO 10
       ELSEIF(STRING(1:1).EQ.'*')THEN
            GOTO 10
*** Add an entry point.
       ELSEIF(INPCMP(1,'ADD-EN#TRY-#POINT').NE.0)THEN
*   Update the record for the current entry point.
            ALGENT(NALGE,3)=1
            ALGENT(NALGE,4)=1
            ALGENT(NALGE,6)=NINS-IINS0+1
            ALGENT(NALGE,10)=0
            DO 80 I=ALGENT(NALGE,5),ALGENT(NALGE,5)+ALGENT(NALGE,6)-1
            IF(INS(I,2).EQ.0)ALGENT(NALGE,10)=ALGENT(NALGE,10)+1
            IF(INS(I,2).EQ.7.OR.ABS(INS(I,2)).EQ.9)ALGENT(NALGE,4)=0
            IF(INS(I,2).NE.0.AND.INS(I,2).NE.6.AND.INS(I,2).NE.8.AND.
     -           INS(I,2).NE.9)NCONS=MIN(NCONS,INS(I,1))
            IF(ABS(INS(I,2)).NE.9)NCONS=MIN(NCONS,INS(I,3))
80          CONTINUE
            ALGENT(NALGE,9)=ICONS0-NCONS+1
            NREXP=ALGENT(NALGE,10)
*   Increment counters.
            IENTRY=IENTRL+1
            IENTRL=IENTRL+1
            IINS0=NINS+1
            ICONS0=NCONS-1
*   Check storage, perform a garbage collect if necessary.
            IF(NALGE+1.GT.MXALGE)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : No room for a new'//
     -                ' entry point; try a garbage collect.'
                 GOTO 10
            ENDIF
            NALGE=NALGE+1
*   Initialise the entry point record.
            ALGENT(NALGE,1)=IENTRY
            ALGENT(NALGE,2)=1
            ALGENT(NALGE,3)=0
            ALGENT(NALGE,4)=0
            ALGENT(NALGE,5)=IINS0
            ALGENT(NALGE,6)=0
            ALGENT(NALGE,7)=NVAR
            ALGENT(NALGE,8)=ICONS0
            ALGENT(NALGE,9)=0
            ALGENT(NALGE,10)=0
*   Tell the user which entry point was added.
            WRITE(LUNOUT,'(/''  New entry point has reference '',I4,/
     -           ''  and starts at line '',I4,''.'',/)') IENTRY,IINS0
*** Remove an entry point.
       ELSEIF(INPCMP(1,'CL#EAR-EN#TRY-#POINT').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 IENCLR=IENTRY
            ELSEIF(NWORD.EQ.2)THEN
                 IF(INPTYP(2).NE.1)THEN
                      CALL INPMSG(2,'Entry point is not an integer.')
                      IENCLR=0
                 ELSE
                      CALL INPCHK(2,1,IFAIL1)
                      CALL INPRDI(2,IENCLR,0)
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! ALGEDT WARNING : CLEAR-ENTRY-POINT'//
     -                ' has either 1 or no argument; nothing cleared.'
                 IENCLR=0
            ENDIF
            CALL ALGCLR(IENCLR)
*** Print the number of instructions.
       ELSEIF(0.NE.INPCMP(1,'C#OUNT'))THEN
            WRITE(LUNOUT,'(/''  Current number of instructions:'',I4,
     -           ''.''/)') NINS
*** Set or display the entry point.
       ELSEIF(0.NE.INPCMP(1,'D#ISPLAY-EN#TRY-#POINT'))THEN
*   Read the optional argument (entry point reference number).
            IENDSP=0
            IF(NWORD.EQ.1)THEN
                 IENDSP=IENTRY
            ELSEIF(NWORD.EQ.2)THEN
                 IF(INPTYP(2).NE.1)THEN
                      CALL INPMSG(2,'Entry point is not an integer.')
                 ELSE
                      CALL INPCHK(2,1,IFAIL1)
                      CALL INPRDI(2,IENTRR,0)
                      IENTNO=0
                      DO 50 I=1,NALGE
                      IF(ALGENT(I,1).EQ.IENTRR)IENTNO=I
50                    CONTINUE
                      IF(IENTNO.EQ.0)THEN
                           CALL INPMSG(2,
     -                          'Entry point does not exist.   ')
                      ELSE
                           IENDSP=IENTRR
                      ENDIF
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! ALGEDT WARNING : DISPLAY-ENTRY-'//
     -                'POINT has 1 or no arguments; statement ignored.'
                 IENDSP=0
            ENDIF
*   Attempt to locate the entry point in the table.
            IENTNO=0
            DO 40 I=1,NALGE
            IF(ALGENT(I,1).EQ.IENDSP)IENTNO=I
40          CONTINUE
*   Display the data if found.
            IF(IENTNO.NE.0)THEN
                 WRITE(LUNOUT,'(/''  ENTRY POINT DESCRIPTION:''//
     -                5X,''Reference number:           '',I4/
     -                5X,''In use (1) or not (0):      '',I4/
     -                5X,''Correct (1) or not (0):     '',I4/
     -                5X,''Sequential (1) or not (0):  '',I4/
     -                5X,''First instruction at line:  '',I4/
     -                5X,''Number of instructions:     '',I4/
     -                5X,''Number of registers used:   '',I4/
     -                5X,''First local constant at:    '',I4/
     -                5X,''Number of local constants:  '',I4/
     -                5X,''Number of results produced: '',I4/)')
     -                (ALGENT(IENTNO,I),I=1,10)
*   Display an error message if the entry point was not found.
            ELSEIF(IENDSP.NE.0)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Unable to find'//
     -                ' the entry point; make sure it is still defined.'
            ENDIF
*** Check whether routine execution can be finished.
       ELSEIF(0.NE.INPCMP(1,'EX#IT'))THEN
*   Find out which variables are effectively used.
            DO 20 I1=1,NVAR
            USE(I1)=.FALSE.
            DO 30 I2=1,NINS
            IF((INS(I2,1).EQ.I1.AND.INS(I2,2).NE.0.AND.INS(I2,2).NE.6)
     -           .OR.INS(I2,3).EQ.I1)USE(I1)=.TRUE.
30          CONTINUE
20          CONTINUE
*   Update the entry point record.
            IF(NALGE.GT.0.AND.NALGE.LT.MXALGE)THEN
                 ALGENT(NALGE,3)=1
                 ALGENT(NALGE,4)=1
                 ALGENT(NALGE,6)=NINS-IINS0+1
                 ALGENT(NALGE,10)=0
                 DO 70 I=ALGENT(NALGE,5),
     -                ALGENT(NALGE,5)+ALGENT(NALGE,6)-1
                 IF(INS(I,2).EQ.0)ALGENT(NALGE,10)=ALGENT(NALGE,10)+1
                 IF(INS(I,2).EQ.7.OR.ABS(INS(I,2)).EQ.9)
     -                ALGENT(NALGE,4)=0
                 IF(INS(I,2).NE.0.AND.INS(I,2).NE.6.AND.
     -                 INS(I,2).NE.8.AND.INS(I,2).NE.9)
     -                 NCONS=MIN(NCONS,INS(I,1))
                 IF(ABS(INS(I,2)).NE.9)NCONS=MIN(NCONS,INS(I,3))
70               CONTINUE
                 ALGENT(NALGE,9)=ICONS0-NCONS+1
                 NREXP=ALGENT(NALGE,10)
            ELSE
                 PRINT *,' !!!!!! ALGEDT WARNING : No instructions'//
     -                ' left on EXIT.'
                 NREXP=0
            ENDIF
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------   Algebra subsection end   ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
*   Reset the prompt.
            CALL INPPRM(' ','BACK-PRINT')
            RETURN
*** Provide a means to enter a function directly.
       ELSEIF(0.NE.INPCMP(1,'F#UNCTION'))THEN
            IF(NWORD.LE.1)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : No function'//
     -                ' provided; nothing done.'
            ELSE
                 CALL INPSTR(2,MXWORD,STRING,NC)
                 IENTRY=IENTRY-1
                 IENTRL=IENTRL-1
                 IINS0=ALGENT(NALGE,5)
                 ICONS0=ALGENT(NALGE,8)
                 NINS=IINS0-1
                 NCONS=ICONS0+1
                 NALGE=NALGE-1
                 CALL ALGPRE(STRING,NC,VARLIS,NVAR,NNRES,USE,IENTRY,
     -                IFAIL)
                 PRINT *,' '
                 IF(IFAIL.EQ.0)THEN
                      PRINT *,' Translation succeeded, ',NNRES,
     -                     ' results are produced.'
                 ELSE
                      PRINT *,' Translation did NOT succeed.'
                 ENDIF
                 PRINT *,' '
                 IF(NNRES.NE.NREXP.AND.NREXP.NE.0)PRINT *,' Note: the'//
     -                ' calling section expects ',NREXP,' results.'
            ENDIF
*** Garbage collect.
       ELSEIF(INPCMP(1,'GARB#AGE-#COLLECT').NE.0)THEN
            CALL ALGGBC
*** Insertion of instructions.
       ELSEIF(INPCMP(1,'I#NSERT').NE.0)THEN
            IF(NWORD.GT.2)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : INSERT needs 1'//
     -                ' argument ; the instruction is ignored.'
                 GOTO 10
            ELSEIF(NWORD.EQ.1)THEN
                 INS0=NINS+1
                 IFAIL=0
            ELSE
                 CALL INPCHK(2,1,IFAIL)
                 CALL INPRDI(2,INS0,NINS+1)
                 IF(INS0.LT.1.OR.INS0.GT.NINS+1)
     -                CALL INPMSG(2,'Argument out of range.        ')
                 CALL INPERR
            ENDIF
            IF(INS0.LT.1.OR.INS0.GT.NINS+1.OR.IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Incorrect syntax'//
     -                ' or value of argument for INSERT; line ignored.'
                 GOTO 10
            ENDIF
*   Make sure there is room to insert lines.
            IF(NINS.GE.MXINS)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : No room to insert'//
     -                ' new lines ; delete some or increase MXINS.'
                 GOTO 10
            ENDIF
*   Ask for the new lines, initialise the insert counter: INSC.
            INSC=NINS
            PRINT *,' ====== ALGEDT INPUT   : Please enter new'//
     -           ' lines, terminate with a blank line.'
            CALL INPPRM('Ins','ADD-NOPRINT')
200         CONTINUE
*   Check that the insert counter can still be incremented.
            IF(INSC+1.GT.MXINS)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : No further lines'//
     -                ' can be accepted; delete some or increase MXINS.'
                 GOTO 210
            ENDIF
*   Read the line to be inserted,
            CALL INPGET
            CALL INPNUM(NWORD)
            IF(NWORD.EQ.0)GOTO 210
*   and check that the types are correct.
            CALL INPCHK(1,1,IFAIL1)
            CALL INPCHK(2,1,IFAIL2)
            CALL INPCHK(3,1,IFAIL3)
            CALL INPCHK(4,1,IFAIL4)
            CALL INPERR
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0.OR.NWORD.NE.4)THEN
                 GOTO 200
            ENDIF
*   Read the contents of the line and check the syntax.
            INSC=INSC+1
            CALL INPRDI(1,INS(INSC,1),MXREG+1)
            CALL INPRDI(2,INS(INSC,2),     10)
            CALL INPRDI(3,INS(INSC,3),MXREG+1)
            CALL INPRDI(4,INS(INSC,4),MXREG+1)
            IF(ISYNCH.EQ.1.AND.
     -          ((INS(INSC,2).EQ.6.AND.(INS(INSC,1).GT.10.OR.
     -           INS(INSC,1).LT.-9)).OR.
     -           (INS(INSC,2).EQ.0.AND.INS(INSC,3).LT.0).OR.
     -           (INS(INSC,2).LT.0.OR.INS(INSC,2).GT.17.OR.
     -            (INS(INSC,2).GT.7.AND.INS(INSC,2).LT.10)).OR.
     -           (((INS(INSC,2).GE.1.AND.INS(INSC,2).LE.5).OR.
     -             (INS(INSC,2).GE.10.AND.INS(INSC,2).LE.17)).AND.
     -            (INS(INSC,1).LT.MXCONS.OR.INS(INSC,1).GT.MXREG)).OR.
     -           INS(INSC,3).LT.MXCONS.OR.INS(INSC,3).GT.MXREG.OR.
     -           INS(INSC,4).LT.MXCONS.OR.INS(INSC,4).GT.MXREG))THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Line is invalid'//
     -                ' in ALGEBRA mode; ignored.'
                 INSC=INSC-1
            ELSEIF(ISYNCH.EQ.2)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : PROCEDURE mode'//
     -                ' checking is not yet available; to to NONE.'
                 ISYNCH=0
            ENDIF
            GOTO 200
*   End of the list reached.
210         CONTINUE
*   Reset the prompt.
            CALL INPPRM(' ','BACK-PRINT')
*   Move the inserted lines to their new position.
            DO 230 I=1,INSC-NINS
            DO 240 J=1,4
            IBUF(J)=INS(NINS+I,J)
            INS(NINS+I,J)=INS(INS0+I-1+INSC-NINS,J)
            INS(INS0+I-1+INSC-NINS,J)=INS(INS0+I-1,J)
            INS(INS0+I-1,J)=IBUF(J)
240         CONTINUE
230         CONTINUE
            NINS=INSC
*** Handle the range of the instructions needing one.
       ELSEIF(INPCMP(1,'L#IST')+INPCMP(1,'PR#INT')+
     -      INPCMP(1,'DEL#ETE')+INPCMP(1,'EXEC#UTE').NE.0)THEN
            IF(NINS.EQ.0)THEN
                 PRINT *,' The instruction buffer is empty.'
                 GOTO 10
            ENDIF
            CALL INPSTR(1,1,STRING,NC)
            IF(NWORD.EQ.1)THEN
                 ILIST1=1
                 ILIST2=NINS
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL)
                 CALL INPRDI(2,ILIST1,1)
                 IF(IFAIL.NE.0)THEN
                      CALL INPERR
                      PRINT *,' !!!!!! ALGEDT WARNING : Incorrect'//
     -                     ' argument type for '//STRING(1:NC)//'.'
                      GOTO 10
                 ENDIF
                 IF(ILIST1.LE.0.OR.ILIST1.GT.NINS)THEN
                      PRINT *,' !!!!!! ALGEDT WARNING : The argument'//
     -                     ' is out of range for '//STRING(1:NC)//'.'
                      GOTO 10
                 ENDIF
                 ILIST2=ILIST1
            ELSEIF(NWORD.EQ.3)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,ILIST1,1)
                 IF(0.EQ.INPCMP(3,'L#AST'))THEN
                      CALL INPCHK(3,1,IFAIL2)
                      CALL INPRDI(3,ILIST2,NINS)
                 ELSE
                      IFAIL2=0
                      ILIST2=NINS
                 ENDIF
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                      CALL INPERR
                      PRINT *,' !!!!!! ALGEDT WARNING : Incorrect'//
     -                     ' argument type(s) for '//STRING(1:NC)//'.'
                      GOTO 10
                 ENDIF
                 IF(ILIST1.LE.0.OR.ILIST2.GT.NINS.OR.ILIST1.GT.ILIST2)
     -                PRINT *,' !!!!!! ALGEDT WARNING : Incorrect'//
     -                     ' argument range for '//STRING(1:NC)//
     -                     '; adjusted to the bounds.'
                 ILIST1=MIN(NINS,MXINS,MAX(1,ILIST1))
                 ILIST2=MAX(ILIST1,MIN(ILIST2,NINS,MXINS))
            ELSE
                 PRINT *,' !!!!!! ALGEDT WARNING : Number of'//
     -                ' arguments incorrect for '//STRING(1:NC)//'.'
                 GOTO 10
            ENDIF
*   Deleting of instructions, update the entry point also.
            IF(INPCMP(1,'DEL#ETE').NE.0)THEN
                 DO 140 IENT=1,NALGE
                 IF(ALGENT(IENT,5).LE.ILIST1.AND.(IENT.EQ.NALGE.OR.
     -                ALGENT(MIN(IENT+1,NALGE),5).GT.ILIST1))THEN
                      IF(ALGENT(IENT,2).EQ.0)THEN
                           PRINT *,' !!!!!! ALGEDT WARNING : The'//
     -                          ' lines to be deleted start in'//
     -                          ' a cleared entry point; ignored.'
                           GOTO 10
                      ELSEIF(ALGENT(IENT,6).GE.ILIST2-ILIST1+1)THEN
                           IENUPD=IENT
                           GOTO 150
                      ELSE
                           PRINT *,' !!!!!! ALGEDT WARNING : The'//
     -                          ' range of lines to be deleted spans'//
     -                          ' more than 1 entry point; ignored.'
                           GOTO 10
                      ENDIF
                 ENDIF
140              CONTINUE
                 PRINT *,' ###### ALGEDT ERROR   : Unable to find the'//
     -                ' entry point for the delete range; program bug.'
                 GOTO 10
150              CONTINUE
                 DO 110 IDEL=ILIST1,NINS-(ILIST2-ILIST1)-1
                 DO 120 ICOPY=1,4
                 INS(IDEL,ICOPY)=INS(IDEL+(ILIST2-ILIST1)+1,ICOPY)
120              CONTINUE
110              CONTINUE
                 IF(ILIST2.LT.IINS0)IINS0=IINS0-ILIST2+ILIST1-1
                 NINS=NINS-ILIST2+ILIST1-1
                 ALGENT(IENUPD,6)=ALGENT(IENUPD,6)-ILIST2+ILIST1-1
                 DO 160 IENT=IENUPD+1,NALGE
                 ALGENT(IENT,5)=ALGENT(IENT,5)-ILIST2+ILIST1-1
160              CONTINUE
*   Executing instructions.
            ELSEIF(INPCMP(1,'EXEC#UTE').NE.0)THEN
                 DO 130 IEXEC=ILIST1,ILIST2
                 IF(INS(IEXEC,2).EQ.0.OR.INS(IEXEC,2).EQ.7.OR.
     -                INS(IEXEC,2).EQ.8.OR.ABS(INS(IEXEC,2)).EQ.9)THEN
                      PRINT *,' The following instruction is not'//
     -                     ' executed:'
                      CALL ALGPRT(IEXEC,IEXEC)
                      GOTO 130
                 ELSE
                      CALL ALGEX2(IEXEC,IFAIL)
                      IF(IFAIL.NE.0)THEN
                           WRITE(LUNOUT,*) ' ++++++ ALGEDT DEBUG   :'//
     -                          ' Arithmetic error while evaluating:'
                           CALL ALGPRT(IEXEC,IEXEC)
                           IF(INS(IEXEC,2).EQ.6)WRITE(LUNOUT,1010)
     -                          INS(IEXEC,3),REG(INS(IEXEC,3))
                           IF(INS(I,2).NE.6)WRITE(LUNOUT,1010)
     -                          INS(IEXEC,1),REG(INS(IEXEC,1)),
     -                          INS(IEXEC,3),REG(INS(IEXEC,3))
                           WRITE(LUNOUT,'('' '')')
                      ENDIF
                 ENDIF
130              CONTINUE
*   Listing of instructions.
            ELSEIF(INPCMP(1,'L#IST').NE.0)THEN
                 WRITE(LUNOUT,'('' '')')
                 DO 100 ILIST=ILIST1,ILIST2
                 WRITE(LUNOUT,'(1X,I3,'' : '',4I4)')
     -                ILIST,(INS(ILIST,I),I=1,4)
100              CONTINUE
                 WRITE(LUNOUT,'('' '')')
*   Printing of the instructions.
            ELSEIF(INPCMP(1,'PR#INT').NE.0)THEN
                 CALL ALGPRT(ILIST1,ILIST2)
            ENDIF
*** Show memory occupation.
       ELSEIF(INPCMP(1,'MEM#ORY').NE.0)THEN
            WRITE(LUNOUT,'(/''  GLOBAL MEMORY USAGE:''//
     -           5X,''Number of registers in use:    '',I3/
     -           5X,''Number of constants in use:    '',I3/
     -           5X,''Number of instructions in use: '',I3/,
     -           5X,''Number of entry points in use: '',I3/)')
     -           NREG,-NCONS,NINS,NALGE
            IF(NALGE.GE.1)THEN
                 WRITE(LUNOUT,'(/''  USAGE PER ENTRY POINT:''//
     -                ''     Refno   Instructions      Registers'',
     -                ''      Constants  Comments'')')
                 DO 510 I=1,NALGE
                 NCPRT=0
                 STRING=' '
                 IF(ALGENT(I,2).EQ.0)THEN
                      STRING(NCPRT+1:NCPRT+9)='Cleared, '
                      NCPRT=NCPRT+9
                 ENDIF
                 IF(ALGENT(I,3).EQ.0)THEN
                      STRING(NCPRT+1:NCPRT+13)='Not useable, '
                      NCPRT=NCPRT+13
                 ENDIF
                 IF(NCPRT.LT.3)NCPRT=3
                 WRITE(LUNOUT,'(5X,I5,3I15,2X,A)') ALGENT(I,1),
     -                ALGENT(I,6),ALGENT(I,7),ALGENT(I,9),
     -                STRING(1:NCPRT-2)
510              CONTINUE
                 WRITE(LUNOUT,'('' '')')
            ELSE
                 WRITE(LUNOUT,'(/''  NO ENTRY POINTS IN USE.''/)')
            ENDIF
*** Take care of the options.
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/''  LOCAL OPTIONS CURRENTLY IN'',
     -                '' EFFECT:'')')
                 IF(ISYNCH.EQ.0)THEN
                      WRITE(LUNOUT,'(/''  Instruction list lines'',
     -                     '' are not checked.'')')
                 ELSEIF(ISYNCH.EQ.1)THEN
                      WRITE(LUNOUT,'(/''  Instruction list lines'',
     -                     '' are checked on ALGEBRA syntax.'')')
                 ELSEIF(ISYNCH.EQ.2)THEN
                      WRITE(LUNOUT,'(/''  Instruction list lines'',
     -                     '' are checked on PROCEDURE syntax.'')')
                 ENDIF
                 IF(LIGUND)THEN
                      WRITE(LUNOUT,'(''  Exponential underflow'',
     -                     '' is ignored.''/)')
                 ELSE
                      WRITE(LUNOUT,'(''  Exponential underflow'',
     -                     '' is signaled.''/)')
                 ENDIF
                 IF(LINUND)THEN
                      WRITE(LUNOUT,'(''  Underflow on input'',
     -                     '' is ignored.''/)')
                 ELSE
                      WRITE(LUNOUT,'(''  Underflow on input'',
     -                     '' is signaled.''/)')
                 ENDIF
            ENDIF
            DO 310 I=2,NWORD
            IF(INPCMP(I,'NO-SYN#TAX-#CHECK').NE.0)THEN
                 ISYNCH=0
            ELSEIF(INPCMP(I,'ALG#EBRA-SYN#TAX-#CHECK').NE.0)THEN
                 ISYNCH=1
            ELSEIF(INPCMP(I,'PRO#CEDURE-SYN#TAX-#CHECK').NE.0)THEN
                 ISYNCH=2
            ELSEIF(INPCMP(I,'I#GNORE-UND#ERFLOW')+
     -           INPCMP(I,'I#GNORE-EXP#ONENTIAL-UND#ERFLOW').NE.0)THEN
                 LIGUND=.TRUE.
            ELSEIF(INPCMP(I,'S#IGNAL-UND#ERFLOW')+
     -           INPCMP(I,'S#IGNAL-EXP#ONENTIAL-UND#ERFLOW').NE.0)THEN
                 LIGUND=.FALSE.
            ELSEIF(INPCMP(I,'I#GNORE-INP#UT-UND#ERFLOW').NE.0)THEN
                 LINUND=.TRUE.
            ELSEIF(INPCMP(I,'S#IGNAL-INP#UT-UND#ERFLOW').NE.0)THEN
                 LINUND=.FALSE.
            ELSE
                 CALL INPMSG(I,'The option is not known.      ')
            ENDIF
310         CONTINUE
            CALL INPERR
*** Set/show register values, if the keyword is REGISTER.
       ELSEIF(0.NE.INPCMP(1,'R#EGISTER'))THEN
            CALL INPCHK(2,1,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPERR
            IF(NWORD.LE.1.OR.NWORD.GT.3)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Incorrect number'//
     -                ' of arguments for the REGISTER instruction.'
            ELSEIF(NWORD.EQ.2.AND.IFAIL1.NE.0.OR.
     -             NWORD.EQ.3.AND.(IFAIL1.NE.0.OR.IFAIL2.NE.0))THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Incorrect argument'//
     -                ' type(s) for the REGISTER instruction.'
            ELSE
                 CALL INPRDI(2,I,1)
                 IF(I.LT.MXCONS.OR.I.GT.MXREG)THEN
                      PRINT *,' !!!!!! ALGEDT WARNING : The argument'//
     -                     ' to REGISTER is not a valid array index.'
                 ELSEIF(NWORD.EQ.2)THEN
                      CALL OUTFMT(REAL(I),2,AUX1,NC1,'LEFT')
                      CALL OUTFMT(REG(I),MODREG(I),AUX2,NC2,'LEFT')
                      WRITE(LUNOUT,'(''  Current value of register '',
     -                     A,'' is '',A,''.'')') AUX1(1:NC1),AUX2(1:NC2)
                 ELSEIF(NWORD.EQ.3)THEN
                      CALL INPRDR(3,REG(I),0.0)
                 ENDIF
            ENDIF
*** Reset the algebra system.
       ELSEIF(INPCMP(1,'RESE#T').NE.0)THEN
*   Initialise.
            CALL ALGINT
*   Assign a new entry point.
            NALGE=1
            IENTRY=IENTRL+1
            IENTRL=IENTRL+1
            ALGENT(NALGE,1)=IENTRY
            ALGENT(NALGE,2)=1
            ALGENT(NALGE,3)=0
            ALGENT(NALGE,4)=0
            ALGENT(NALGE,5)=IINS0
            ALGENT(NALGE,6)=0
            ALGENT(NALGE,7)=NVAR
            ALGENT(NALGE,8)=ICONS0
            ALGENT(NALGE,9)=0
            ALGENT(NALGE,10)=0
*** Print the number of results the calling section expects.
       ELSEIF(INPCMP(1,'RESU#LTS').NE.0)THEN
            IF(NREXP.NE.0)THEN
                 PRINT *,' The calling section expects ',NREXP,
     -                ' results.'
            ELSE
                 PRINT *,' The calling section did not specify the',
     -                ' number of expected results.'
            ENDIF
*** Simplify the instruction list.
       ELSEIF(INPCMP(1,'SIM#PLIFY').NE.0)THEN
            NREG=0
            NCONS=0
            DO 410 I=1,NINS
            IF(INS(I,2).NE.0.AND.INS(I,2).NE.6.AND.
     -           INS(I,2).NE.8.AND.INS(I,2).NE.9)THEN
                 IF(NREG.LT.INS(I,1))NREG=INS(I,1)
                 IF(NCONS.GT.INS(I,1))NCONS=INS(I,1)
            ENDIF
            IF(ABS(INS(I,2)).NE.9)THEN
                 IF(NREG.LT.INS(I,3))NREG=INS(I,3)
                 IF(NCONS.GT.INS(I,3))NCONS=INS(I,3)
            ENDIF
410         CONTINUE
            CALL ALGSIM(VARLIS,NVAR,USE,IFAIL)
*** Allow testing of the instruction list.
       ELSEIF(0.NE.INPCMP(1,'TEST'))THEN
            IF(NWORD.NE.1+NVAR)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Each parameter to'//
     -                ' the function must be specified when using TEST.'
            ELSE
                 IFAIL=0
                 DO 320 I=2,NWORD
                 CALL INPCHK(I,2,IFAIL1)
                 IF(IFAIL1.NE.0)IFAIL=1
                 CALL INPRDR(I,VAR(I-1),0.0)
                 MODVAR(I-1)=2
320              CONTINUE
                 CALL INPERR
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! ALGEDT WARNING : Syntax errors'//
     -                     ' in the test parameters ; line ignored.'
                      GOTO 10
                 ENDIF
                 DO 330 I=1,10
                 RES(I)=0.0
330              CONTINUE
                 CALL ALGEXE(IENTRY,VAR,MODVAR,NVAR,RES,MODRES,10,IFAIL)
                 WRITE(LUNOUT,'(/''  Elements in the result array'',
     -                '' which have been assigned a value.''/
     -                ''  ============================'',
     -                ''=================================''/)')
                 DO 340 I=1,10
                 IPRINT=0
                 DO 341 J=1,NINS
                 IF(INS(J,2).EQ.0.AND.INS(J,4).EQ.I)IPRINT=1
341              CONTINUE
                 IF(IPRINT.EQ.1)WRITE(LUNOUT,'(''  Result('',I3,
     -                '') = '',E15.8)') I,RES(I)
340              CONTINUE
                 IF(IFAIL.NE.0)THEN
                      WRITE(LUNOUT,'(/''  Note: an error has'',
     -                     '' been detected.''/)')
                 ELSE
                      WRITE(LUNOUT,'(/''  No errors detected.''/)')
                 ENDIF
            ENDIF
*** Show the variable names if VARIABLES is the keyword.
       ELSEIF(0.NE.INPCMP(1,'VAR#IABLES'))THEN
            WRITE(LUNOUT,'(/'' List of acceptable variable names:''/
     -           '' ==================================''/)')
            DO 300 I=1,NVAR
            WRITE(LUNOUT,'(5X,A10,'' --> Register('',I3,'')'')')
     -           VARLIS(I),I
300         CONTINUE
            WRITE(LUNOUT,'('' '')')
*** Unknown instruction.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! ALGEDT WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction; ignored.'
       ENDIF
*** Display error messages.
       CALL INPERR
       GOTO 10
       END
