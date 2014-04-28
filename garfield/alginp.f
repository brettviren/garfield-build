CDECK  ID>, ALGINP.
       SUBROUTINE ALGINP
*-----------------------------------------------------------------------
*   ALGINP - Serves as a subsection reading algebra command lines.
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
       LOGICAL LOOP
       INTEGER INPCMP,INPTYP,NWORD,I,NC,NC1,NC2,IKEY,
     -      NCPRT,IFAIL1,IFAIL2
       CHARACTER*(MXCHAR) STRING
       CHARACTER*30 AUX1,AUX2
       EXTERNAL INPCMP,INPTYP
*** Identify the subroutine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE ALGINP ///'
*** First pick up the number of words and the first word.
       CALL INPNUM(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Check it is a algebra command.
       IF(STRING(1:1).NE.'@')RETURN
*** Determine whether it is a single command or not.
       IF(NWORD.EQ.1.AND.NC.EQ.1)THEN
            LOOP=.TRUE.
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------     Algebra subsection     ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
            CALL INPPRM('Algebra','ADD-PRINT')
       ELSE
            LOOP=.FALSE.
       ENDIF
*** Return here if LOOP is .TRUE.
10     CONTINUE
       IF(LOOP)THEN
            CALL INPGET
            CALL INPNUM(NWORD)
       ENDIF
       CALL INPSTR(1,1,STRING,NC)
*** Skip blank lines and warn for section headers.
       IF(STRING(1:1).EQ.'&')THEN
            PRINT *,' !!!!!! ALGINP WARNING : The section cannot be'//
     -           ' left at this point; first type EXIT.'
            GOTO 1010
       ELSEIF(INDEX('$%?><!',STRING(1:1)).NE.0)THEN
            PRINT *,' !!!!!! ALGINP WARNING : This command cannot be'//
     -           ' executed at the present level; first type EXIT.'
            GOTO 1010
       ELSEIF(STRING(1:1).EQ.'*')THEN
            GOTO 1010
       ENDIF
       IF(LOOP.AND.(NWORD.EQ.0.OR.(NWORD.EQ.1.AND.NC.EQ.1.AND.
     -      STRING(1:1).EQ.'@')))GOTO 1010
       IF(.NOT.LOOP.AND.NC.EQ.1.AND.NWORD.EQ.1)RETURN
*** Set the position of the command.
       IF(NC.EQ.1.AND.STRING(1:1).EQ.'@')THEN
            IKEY=2
       ELSE
            IKEY=1
       ENDIF
*** Show memory occupation.
       IF(INPCMP(IKEY,'MEM#ORY')+INPCMP(IKEY,'@MEM#ORY').NE.0)THEN
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
       ELSEIF(INPCMP(IKEY,'OPT#IONS')+
     -      INPCMP(IKEY,'@OPT#IONS').NE.0)THEN
            IF(NWORD.EQ.IKEY)THEN
                 WRITE(LUNOUT,'(/''  ALGEBRA OPTIONS CURRENTLY IN'',
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
            DO 310 I=IKEY+1,NWORD
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
            CALL INPCHK(IKEY+1,1,IFAIL1)
            CALL INPCHK(IKEY+2,2,IFAIL2)
            CALL INPERR
            IF(NWORD.LE.IKEY.OR.NWORD.GT.IKEY+2)THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Incorrect number'//
     -                ' of arguments for the REGISTER instruction.'
            ELSEIF(NWORD.EQ.IKEY+1.AND.IFAIL1.NE.0.OR.
     -             NWORD.EQ.IKEY+2.AND.(IFAIL1.NE.0.OR.IFAIL2.NE.0))THEN
                 PRINT *,' !!!!!! ALGEDT WARNING : Incorrect argument'//
     -                ' type(s) for the REGISTER instruction.'
            ELSE
                 CALL INPRDI(IKEY+1,I,1)
                 IF(I.LT.MXCONS.OR.I.GT.MXREG)THEN
                      PRINT *,' !!!!!! ALGEDT WARNING : The argument'//
     -                     ' to REGISTER is not a valid array index.'
                 ELSEIF(NWORD.EQ.IKEY+1)THEN
                      CALL OUTFMT(REAL(I),2,AUX1,NC1,'LEFT')
                      CALL OUTFMT(REG(I),MODREG(I),AUX2,NC2,'LEFT')
                      WRITE(LUNOUT,'(''  Current value of register '',
     -                     A,'' is '',A,''.'')') AUX1(1:NC1),AUX2(1:NC2)
                 ELSEIF(NWORD.EQ.IKEY+2)THEN
                      CALL INPRDR(IKEY+2,REG(I),0.0)
                 ENDIF
            ENDIF
*** Check for the EXIT command.
       ELSEIF(INPCMP(IKEY,'EX#IT')+INPCMP(IKEY,'@EX#IT').NE.0)THEN
            PRINT *,' '
            PRINT *,' ------------------------------------------------'
            PRINT *,' ----------   Algebra subsection end   ----------'
            PRINT *,' ------------------------------------------------'
            PRINT *,' '
            CALL INPPRM(' ','BACK-PRINT')
            RETURN
*** Invalid option.
       ELSE
            CALL INPSTR(IKEY,IKEY,STRING,NC)
            PRINT *,' !!!!!! ALGINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid command; it is ignored.'
       ENDIF
*** Either read a new input line or return to the calling section.
1010   CONTINUE
*** Next command, if in a sub-section.
       IF(LOOP)GOTO 10
       END
