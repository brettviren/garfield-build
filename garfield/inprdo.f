CDECK  ID>, INPRDO.
       SUBROUTINE INPRDO(IFAIL)
*-----------------------------------------------------------------------
*   INPRDO - Reads a DO loop, stores the lines and prepares entries.
*   (Last changed on  1/11/12.)
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
       CHARACTER*(MXCHAR) BLANK,FROM,STEP,WHILE,UNTIL,TO,IN
       CHARACTER*13 PROMPT
       CHARACTER*10 FOR
       CHARACTER*8 TYPE
       CHARACTER*(MXINCH) STRING,INDSTR
       INTEGER INPCMP,NCSTR,NCIND,IENTNO,IEXTR,ILAST,MAXDOL,MAXIFL,NC,
     -      NNRES,IFAIL,NWORD,
     -      NCFOR,NCFROM,NCSTEP,NCWHIL,NCUNTL,NCTO,NCIN,
     -      I,J,I0,I1,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,IFAIL11,
     -      NRES1,NRES2,NRES3,NRES4,NRES5,NRES11,
     -      ILLCHR,IGLB,NCPRM,ITHEN
       LOGICAL OK,USE(MXVAR),LFROM,LSTEP,LWHILE,LUNTIL,LTO,LIN
       EXTERNAL INPCMP
*** Initialise the various level and line counters.
       NDOLIN=0
       CDOLVL=0
       TRACDO(0)=0
       CIFLVL=0
       TRACIF(0)=0
       MAXDOL=0
       MAXIFL=0
       NLOOP=0
       NIF=0
       OK=.TRUE.
*** Update the prompt.
       CALL INPPRM('Loop','ADD')
*** Carry on with the next line (passed on or read at end of loop).
10     CONTINUE
*** Increment the line counter.
       NDOLIN=NDOLIN+1
       IF(NDOLIN.GT.MXDLIN)THEN
            PRINT *,' !!!!!! INPRDO WARNING : DO loop contains too'//
     -           ' many lines; increase MXDLIN.'
            OK=.FALSE.
            NDOLIN=MXDLIN
       ENDIF
*** Usually no global variable definition.
       LINREF(NDOLIN,7)=0
       LINREF(NDOLIN,8)=0
*** Count words.
       CALL INPNUM(NWORD)
*   Locate the THEN, if there is one.
       DO 270 I=1,NWORD
       IF(INPCMP(I,'THEN').NE.0)THEN
            ITHEN=I
            GOTO 280
       ENDIF
270    CONTINUE
       ITHEN=0
280    CONTINUE
*** Process an IF cond THEN (expr) part at the start of the line.
       IF(INPCMP(1,'IF')+INPCMP(1,'ELSEIF').NE.0.AND.ITHEN.NE.0)THEN
*   Be sure that the condition clause is not empty.
            IF(ITHEN.LE.2)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Empty clause in'//
     -                ' an IF or ELSEIF line; False assumed.'
                 OK=.FALSE.
            ENDIF
*   Check an IF block does not begin here.
            IF(INPCMP(ITHEN+1,'IF')+INPCMP(ITHEN+1,'ELSE')+
     -           INPCMP(ITHEN+1,'ELSEIF')+
     -           INPCMP(ITHEN+1,'ENDIF').NE.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Parts of an IF'//
     -                ' block may not start on an IF line; use & (and).'
                 OK=.FALSE.
            ENDIF
*   Check this is not an ENDDO.
            IF(INPCMP(ITHEN+1,'ENDDO').NE.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : A DO block may'//
     -                ' not end on an IF line.'
                 OK=.FALSE.
            ENDIF
*   Pick up the condition, translate and store the entry.
            IF(ITHEN.GT.2)THEN
                 CALL INPSTR(2,ITHEN-1,STRING,NC)
            ELSE
                 STRING='FALSE'
                 NC=5
            ENDIF
            CALL ALGPRE(STRING(1:NC),NC,GLBVAR,NGLB,NNRES,USE,
     -           LINREF(NDOLIN,4),IFAIL)
            IF(IFAIL.NE.0.OR.NNRES.NE.1)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Unable to'//
     -                ' translate the condition.'
                 OK=.FALSE.
            ENDIF
*   Get rid of the IF clause before carrying on.
            IF(NWORD.GT.ITHEN)THEN
                 DO 290 I=ITHEN,1,-1
                 CALL INPDEL(I)
290              CONTINUE
                 NWORD=NWORD-ITHEN
            ENDIF
*   Does not start with an IF condition.
       ELSE
            LINREF(NDOLIN,4)=0
       ENDIF
*   Branching by default not used.
       LINREF(NDOLIN,5)=0
*** Start of a new DO loop.
       IF(INPCMP(NWORD,'DO').NE.0.AND.INPCMP(1,'FOR')+
     -      INPCMP(1,'WHILE')+INPCMP(1,'UNTIL')+INPCMP(1,'IN')+
     -      INPCMP(1,'STEP')+INPCMP(1,'DO').NE.0)THEN
*   Increment loop number, level counter and update calling tree.
            IF(NLOOP.GE.MXDLVL)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Number of DO loops'//
     -                ' exceeds storage capacity.'
                 OK=.FALSE.
            ELSE
                 NLOOP=NLOOP+1
            ENDIF
            MAXDOL=MAX(MAXDOL,CDOLVL+1)
            IF(CDOLVL.GE.MXDLVL)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : DO nesting deeper'//
     -                ' than length of loop trace.'
                 OK=.FALSE.
            ELSE
                 CDOLVL=CDOLVL+1
            ENDIF
            TRACDO(CDOLVL)=NLOOP
*   Store the type of this line with the loop reference number.
            LINREF(NDOLIN,1)=1
            LINREF(NDOLIN,3)=TRACDO(CDOLVL)
            LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*   Save the information also in the DO loop control block.
            DOREF(NLOOP,6)=NDOLIN
            DOREF(NLOOP,7)=0
            DOREF(NLOOP,8)=CDOLVL
            DOREF(NLOOP,10)=CIFLVL
*   Initial values for the loop control words.
            FOR=' '
            NCFOR=1
            FROM=' '
            NCFROM=1
            LFROM=.FALSE.
            STEP='1'
            NCSTEP=1
            LSTEP=.FALSE.
            IN=' '
            NCIN=1
            LIN=.FALSE.
            WHILE='TRUE'
            NCWHIL=4
            LWHILE=.FALSE.
            UNTIL='FALSE'
            NCUNTL=5
            LUNTIL=.FALSE.
            TO=' '
            NCTO=1
            LTO=.FALSE.
*   Pick up the DO loop control words: FOR, FROM, STEP, WHILE, UNTIL.
            IF(NWORD.NE.1.AND.(NWORD-1).NE.
     -           2*INT(0.1+REAL(NWORD-1)/2.0))THEN
                 PRINT *,' !!!!!! INPRDO WARNING : The number of'//
     -                ' words on the DO line is incorrect.'
                 OK=.FALSE.
            ENDIF
            DO 20 I=1,NWORD-2,2
*   Read the loop variable name.
            IF(INPCMP(I,'FOR').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
*   Check for illegal characters.
                 ILLCHR=0
                 DO 30 J=1,NC
                 IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',
     -                STRING(J:J)).NE.0)THEN
                      ILLCHR=ILLCHR+1
                      OK=.FALSE.
                 ENDIF
30               CONTINUE
                 IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     -                STRING(1:1)).EQ.0)THEN
                      CALL INPMSG(I+1,'Does not start with a letter. ')
                      OK=.FALSE.
                 ELSEIF(ILLCHR.EQ.1)THEN
                      CALL INPMSG(I+1,'Contains an illegal character.')
                 ELSEIF(ILLCHR.GT.1)THEN
                      CALL INPMSG(I+1,'Contains illegal characters.  ')
*   Check the name is not more than 10 characters long.
                 ELSEIF(NC.GT.LEN(FOR))THEN
                      CALL INPMSG(I+1,'Name longer than 10 characters')
                      OK=.FALSE.
*   Check the name is not empty.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty names are not permitted.')
                      OK=.FALSE.
*   Store the name.
                 ELSE
                      FOR=STRING(1:NC)
                      NCFOR=NC
                 ENDIF
*   Starting value.
            ELSEIF(INPCMP(I,'FROM').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 IF(NC.GT.80)THEN
                      CALL INPMSG(I+1,'Expression longer than 80 char')
                      OK=.FALSE.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty expression not permitted')
                      OK=.FALSE.
                 ELSE
                      FROM=' '
                      FROM=STRING(1:NC)
                      NCFROM=NC
                      LFROM=.TRUE.
                 ENDIF
*   Step size for the loop.
            ELSEIF(INPCMP(I,'STEP').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 IF(NC.GT.80)THEN
                      CALL INPMSG(I+1,'Expression longer than 80 char')
                      OK=.FALSE.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty expression not permitted')
                      OK=.FALSE.
                 ELSE
                      STEP=' '
                      STEP=STRING(1:NC)
                      NCSTEP=NC
                      LSTEP=.TRUE.
                 ENDIF
*   Condition to be satisfied, check at start of loop.
            ELSEIF(INPCMP(I,'WHILE').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 IF(NC.GT.80)THEN
                      CALL INPMSG(I+1,'Expression longer than 80 char')
                      OK=.FALSE.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty expression not permitted')
                      OK=.FALSE.
                 ELSE
                      WHILE=' '
                      WHILE=STRING(1:NC)
                      NCWHIL=NC
                      LWHILE=.TRUE.
                 ENDIF
*   Condition not to be satisfied, check at end of loop.
            ELSEIF(INPCMP(I,'UNTIL').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 IF(NC.GT.80)THEN
                      CALL INPMSG(I+1,'Expression longer than 80 char')
                      OK=.FALSE.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty expression not permitted')
                      OK=.FALSE.
                 ELSE
                      UNTIL=' '
                      UNTIL=STRING(1:NC)
                      NCUNTL=NC
                      LUNTIL=.TRUE.
                 ENDIF
*   Final value of the loop variable.
            ELSEIF(INPCMP(I,'TO').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 IF(NC.GT.80)THEN
                      CALL INPMSG(I+1,'Expression longer than 80 char')
                      OK=.FALSE.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty expression not permitted')
                      OK=.FALSE.
                 ELSE
                      TO=' '
                      TO=STRING(1:NC)
                      NCTO=NC
                      LTO=.TRUE.
                 ENDIF
*   Matrix or matrix expression to take elements from.
            ELSEIF(INPCMP(I,'IN').NE.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NC)
                 IF(NC.GT.80)THEN
                      CALL INPMSG(I+1,'Expression longer than 80 char')
                      OK=.FALSE.
                 ELSEIF(NC.LE.0)THEN
                      CALL INPMSG(I+1,'Empty expression not permitted')
                      OK=.FALSE.
                 ELSE
                      IN=' '
                      IN=STRING(1:NC)
                      NCIN=NC
                      LIN=.TRUE.
                 ENDIF
*   Anything else, not valid.
            ELSE
                 CALL INPMSG(I,'Not a known DO control word.  ')
                 CALL INPMSG(I+1,'See preceding message.        ')
            ENDIF
20          CONTINUE
**  Take care of the DO loop variable name.
            IF(FOR.NE.' ')THEN
*   Locate the loop variable in the table.
                 DO 40 I=1,NGLB
                 IF(GLBVAR(I).EQ.FOR(1:NCFOR))THEN
                      DOREF(NLOOP,9)=I
                      GOTO 50
                 ENDIF
40               CONTINUE
                 IF(NGLB.GE.MXVAR)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Ran out of'//
     -                     ' storage space for global variables.'
                      PRINT *,'                         Increase'//
     -                     ' MXVAR and recompile the program.'
                      DOREF(NLOOP,9)=0
                      OK=.FALSE.
                 ELSE
                      NGLB=NGLB+1
                      GLBVAR(NGLB)=FOR(1:NCFOR)
                      GLBMOD(NGLB)=0
                      DOREF(NLOOP,9)=NGLB
                 ENDIF
50               CONTINUE
*   Make sure the loop variable was not used before.
                 DO 60 I=1,NLOOP-1
                 IF(DOREF(I,9).LE.0.OR.DOREF(NLOOP,9).EQ.0)GOTO 60
                 IF(DOREF(I,6).LE.NDOLIN.AND.
     -                (DOREF(I,7).EQ.0.OR.DOREF(I,7).GT.NDOLIN).AND.
     -                GLBVAR(DOREF(I,9)).EQ.GLBVAR(DOREF(NLOOP,9)))THEN
                      PRINT *,' !!!!!! INPRDO WARNING : The DO loop'//
     -                     ' variable '//FOR(1:NCFOR)//' is already'//
     -                     ' used for an enclosing loop.'
                      OK=.FALSE.
                 ENDIF
60               CONTINUE
*   Assign <undefined> to the loop variable.
                 IF(DOREF(NLOOP,9).GT.0)THEN
                      GLBVAL(DOREF(NLOOP,9))=0
                      GLBMOD(DOREF(NLOOP,9))=0
                 ENDIF
*   No name specified, assign the dummy variable 0 to this loop.
            ELSE
                 DOREF(NLOOP,9)=0
            ENDIF
**  Translate the various expressions.
            IF(DOREF(NLOOP,9).NE.0)THEN
*   Verify that not both an IN and an FROM-STEP-TO are present.
                 IF(LIN.AND.(LFROM.OR.LSTEP.OR.LTO))THEN
                      PRINT *,' !!!!!! INPRDO WARNING : The DO loop'//
     -                     ' with variable "'//GLBVAR(DOREF(NLOOP,9))//
     -                     ' has both an In and a From-Step-To.'
                      OK=.FALSE.
                 ENDIF
*   Default step size if needed.
                 IF((.NOT.LIN).AND.STEP.EQ.' ')THEN
                      PRINT *,' ------ INPRDO MESSAGE : Default'//
     -                     ' step size 1 used for the loop of the'//
     -                     ' variable "'//GLBVAR(DOREF(NLOOP,9))//'"'
                      STEP='1'
                      NCSTEP=1
                 ENDIF
*   Process the IN part.
                 IF(IN.NE.' ')THEN
                      CALL ALGPRE(IN,NCIN,GLBVAR,NGLB,NRES11,USE,
     -                     DOREF(NLOOP,11),IFAIL11)
                      NRES1=1
                      IFAIL1=0
                      DOREF(NLOOP,1)=0
                      NRES2=1
                      IFAIL2=0
                      DOREF(NLOOP,2)=0
                      NRES5=1
                      IFAIL5=0
                      DOREF(NLOOP,5)=0
*   Process the FROM, STEP and TO parts.
                 ELSEIF(FROM.EQ.' '.OR.TO.EQ.' ')THEN
                      PRINT *,' !!!!!! INPRDO WARNING : The DO loop'//
     -                     ' with variable "'//GLBVAR(DOREF(NLOOP,9))//
     -                     '" misses a FROM or a TO.'
                      OK=.FALSE.
                      NRES1=1
                      IFAIL1=0
                      DOREF(NLOOP,1)=0
                      NRES2=1
                      IFAIL2=0
                      DOREF(NLOOP,2)=0
                      NRES5=1
                      IFAIL5=0
                      DOREF(NLOOP,5)=0
                      NRES11=1
                      IFAIL11=0
                      DOREF(NLOOP,11)=0
                 ELSE
                      CALL ALGPRE(FROM,NCFROM,GLBVAR,NGLB,NRES1,USE,
     -                     DOREF(NLOOP,1),IFAIL1)
                      CALL ALGPRE(STEP,NCSTEP,GLBVAR,NGLB,NRES2,USE,
     -                     DOREF(NLOOP,2),IFAIL2)
                      CALL ALGPRE(TO,NCTO,GLBVAR,NGLB,NRES5,USE,
     -                     DOREF(NLOOP,5),IFAIL5)
                      NRES11=1
                      IFAIL11=0
                      DOREF(NLOOP,11)=0
                 ENDIF
            ELSE
                 IFAIL1=0
                 IFAIL2=0
                 IFAIL5=0
                 IFAIL11=0
                 NRES1=1
                 NRES2=1
                 NRES5=1
                 NRES11=1
            ENDIF
*   Process the WHILE and UNTIL parts.
            IF(WHILE.EQ.' ')THEN
                 WHILE='TRUE'
                 NCWHIL=4
            ENDIF
            IF(UNTIL.EQ.' ')THEN
                 UNTIL='FALSE'
                 NCUNTL=5
            ENDIF
            CALL ALGPRE(WHILE,NCWHIL,GLBVAR,NGLB,NRES3,USE,
     -           DOREF(NLOOP,3),IFAIL3)
            CALL ALGPRE(UNTIL,NCUNTL,GLBVAR,NGLB,NRES4,USE,
     -           DOREF(NLOOP,4),IFAIL4)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0.OR.IFAIL5.NE.0.OR.IFAIL11.NE.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : One or more of the'//
     -                ' loop control expressions can''t be translated'//
     -                ' into an algebra list.'
                 OK=.FALSE.
            ENDIF
            IF(NRES1.NE.1.OR.NRES2.NE.1.OR.NRES3.NE.1.OR.NRES4.NE.1.OR.
     -           NRES5.NE.1.OR.NRES11.NE.1)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Incorrect number'//
     -                ' of results returned by loop control expression.'
                 OK=.FALSE.
            ENDIF
*** Go for another iteration cycle.
       ELSEIF(INPCMP(1,'ITERATE')+INPCMP(1,'CONTINUE').NE.0)THEN
            LINREF(NDOLIN,1)=2
*   First assign an invalid loop reference number to the statement.
            LINREF(NDOLIN,3)=0
*   The IF block number is known.
            LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*   Mark unused words.
            IF(NWORD.GT.2)THEN
                 DO 130 I=3,NWORD
                 CALL INPMSG(I,'Superfluous argument (ignored)')
130              CONTINUE
                 OK=.FALSE.
            ENDIF
*   Figure out which loop we have to carry out again.
            IF(NWORD.GE.2)THEN
                 CALL INPSTR(2,2,STRING,NC)
                 DO 140 I=1,NLOOP
                 IF(DOREF(I,9).EQ.0)GOTO 140
                 IF(GLBVAR(DOREF(I,9)).EQ.
     -                STRING(1:MAX(1,MIN(10,NC))))LINREF(NDOLIN,3)=I
140              CONTINUE
                 IF(LINREF(NDOLIN,3).EQ.0)THEN
                      CALL INPMSG(2,'Unidentified loop variable.   ')
                      OK=.FALSE.
                 ENDIF
*   No loop specified: carry out inner loop again.
            ELSE
                 LINREF(NDOLIN,3)=TRACDO(CDOLVL)
            ENDIF
*   Check this loop is part of the calling trace.
            DO 180 I=1,CDOLVL
            IF(LINREF(NDOLIN,3).EQ.TRACDO(I))GOTO 190
180         CONTINUE
            PRINT *,' !!!!!! INPRDO WARNING : The loop to be'//
     -           ' iterated is not part of the trace.'
            OK=.FALSE.
190         CONTINUE
*** Leave the loop earlier.
       ELSEIF(INPCMP(1,'LEAVE')+INPCMP(1,'BREAK').NE.0)THEN
            LINREF(NDOLIN,1)=3
*   First assign an invalid loop reference number to the statement.
            LINREF(NDOLIN,3)=0
*   The IF block is known.
            LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*   Mark unused words.
            IF(NWORD.GT.2)THEN
                 DO 110 I=3,NWORD
                 CALL INPMSG(I,'Superfluous argument (ignored)')
110              CONTINUE
                 OK=.FALSE.
            ENDIF
*   Figure out which loop we have to leave.
            IF(NWORD.GE.2)THEN
                 CALL INPSTR(2,2,STRING,NC)
                 DO 120 I=1,NLOOP
                 IF(DOREF(I,9).EQ.0)GOTO 120
                 IF(GLBVAR(DOREF(I,9)).EQ.
     -                STRING(1:MAX(1,MIN(10,NC))))LINREF(NDOLIN,3)=I
120              CONTINUE
                 IF(LINREF(NDOLIN,3).EQ.0)THEN
                      CALL INPMSG(2,'Unidentified loop variable.   ')
                      OK=.FALSE.
                 ENDIF
*   No loop specified: leave inner loop.
            ELSE
                 LINREF(NDOLIN,3)=TRACDO(CDOLVL)
            ENDIF
*   Check this loop is part of the calling trace.
            DO 170 I=1,CDOLVL
            IF(LINREF(NDOLIN,3).EQ.TRACDO(I))GOTO 175
170         CONTINUE
            PRINT *,' !!!!!! INPRDO WARNING : The loop to be left'//
     -           ' is not part of the trace.'
            OK=.FALSE.
175         CONTINUE
*** End of the DO loop.
       ELSEIF(INPCMP(1,'ENDDO').NE.0)THEN
*   Check there is a DO loop open.
            IF(CDOLVL.LE.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : There is no open'//
     -                ' DO loop, ENDDO invalid.'
                 OK=.FALSE.
*   Check the IF levels.
            ELSEIF(CIFLVL.NE.DOREF(TRACDO(CDOLVL),10))THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Incorrect nesting'//
     -                ' of an IF block and a DO loop.'
                 OK=.FALSE.
*   OK.
            ELSE
                 LINREF(NDOLIN,1)=4
                 LINREF(NDOLIN,3)=TRACDO(CDOLVL)
                 LINREF(NDOLIN,6)=TRACIF(CIFLVL)
                 DOREF(TRACDO(CDOLVL),7)=NDOLIN
                 CDOLVL=CDOLVL-1
            ENDIF
*** Start of an IF block.
       ELSEIF(NWORD.EQ.ITHEN.AND.INPCMP(1,'IF').NE.0.AND.ITHEN.NE.0)THEN
*   Store the information about the input line.
            LINREF(NDOLIN,1)=11
            LINREF(NDOLIN,3)=TRACDO(CDOLVL)
*   Check whether we can still increment the IF nesting.
            IF(NIF.GE.MXILVL)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Number of IF'//
     -                ' blocks exceeds storage capacity.'
                 OK=.FALSE.
            ELSE
                 NIF=NIF+1
            ENDIF
*   Check whether we can keep track of this IF block in the trace.
            MAXIFL=MAX(MAXIFL,CIFLVL+1)
            IF(CIFLVL.GE.MXILVL)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : IF nesting deeper'//
     -                ' than length of the trace.'
                 OK=.FALSE.
            ELSE
                 CIFLVL=CIFLVL+1
            ENDIF
*   Store part of the IF block reference information.
            IFREF(NIF,1)=1
            IFREF(NIF,2)=0
            IFREF(NIF,3)=NDOLIN
            IFREF(NIF,4)=CDOLVL
            IFREF(NIF,5)=CIFLVL
*   Keep track of the IF trace.
            TRACIF(CIFLVL)=NIF
            LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*** Branch of the ELSEIF type.
       ELSEIF(NWORD.EQ.ITHEN.AND.INPCMP(1,'ELSEIF').NE.0.AND.
     -      ITHEN.NE.0)THEN
*   Check that the usage of the IF structure is correct.
            IF(CIFLVL.EQ.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : No open IF block,'//
     -                ' use of ELSEIF is not valid.'
                 OK=.FALSE.
            ELSEIF(IFREF(TRACIF(CIFLVL),1).GE.3)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : An ELSEIF may not'//
     -                ' be preceded by an ELSE in the same block.'
                 OK=.FALSE.
            ELSEIF(CDOLVL.NE.IFREF(TRACIF(CIFLVL),4))THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Invalid nesting'//
     -                ' of a DO loop and an IF block.'
                 OK=.FALSE.
            ELSE
*   Line reference information.
                 LINREF(NDOLIN,1)=12
                 LINREF(NDOLIN,3)=TRACDO(CDOLVL)
                 LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*   Update the jump part for the previous branch.
                 LINREF(IFREF(TRACIF(CIFLVL),3),5)=NDOLIN
*   Prepare the next jump.
                 IFREF(TRACIF(CIFLVL),3)=NDOLIN
*   And remember we saw an ENDIF.
                 IFREF(TRACIF(CIFLVL),1)=2
            ENDIF
*** Branch of the ELSE type.
       ELSEIF(NWORD.EQ.1.AND.INPCMP(1,'ELSE').NE.0)THEN
*   Check that the usage of the IF structure is correct.
            IF(CIFLVL.EQ.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : No open IF block,'//
     -                ' use of ELSE is not valid.'
                 OK=.FALSE.
            ELSEIF(IFREF(TRACIF(CIFLVL),1).GE.3)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Two ELSE parts'//
     -                ' in the same block not allowed.'
                 OK=.FALSE.
            ELSEIF(CDOLVL.NE.IFREF(TRACIF(CIFLVL),4))THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Invalid nesting'//
     -                ' of a DO loop and an IF block.'
                 OK=.FALSE.
            ELSE
*   Line reference information.
                 LINREF(NDOLIN,1)=13
                 LINREF(NDOLIN,3)=TRACDO(CDOLVL)
                 LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*   Update the jump part for the previous branch.
                 LINREF(IFREF(TRACIF(CIFLVL),3),5)=NDOLIN
*   Prepare the next jump.
                 IFREF(TRACIF(CIFLVL),3)=NDOLIN
*   And remember we saw an ELSE.
                 IFREF(TRACIF(CIFLVL),1)=3
            ENDIF
*** End of an IF block.
       ELSEIF(NWORD.EQ.1.AND.INPCMP(1,'ENDIF').NE.0)THEN
*   Check that the usage of the IF structure is correct.
            IF(CIFLVL.EQ.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : No open IF block,'//
     -                ' use of ENDIF is not valid.'
                 OK=.FALSE.
            ELSEIF(CDOLVL.NE.IFREF(TRACIF(CIFLVL),4))THEN
                 PRINT *,' !!!!!! INPRDO WARNING : Invalid nesting'//
     -                ' of a DO loop and an IF block.'
                 OK=.FALSE.
            ELSE
*   Line reference information.
                 LINREF(NDOLIN,1)=14
                 LINREF(NDOLIN,3)=TRACDO(CDOLVL)
                 LINREF(NDOLIN,6)=TRACIF(CIFLVL)
*   Update the jump part for the previous branch.
                 LINREF(IFREF(TRACIF(CIFLVL),3),5)=NDOLIN
*   Store the line of the ENDIF in the IF reference block.
                 IFREF(TRACIF(CIFLVL),2)=NDOLIN
*   And remember we saw an ENDIF.
                 IFREF(TRACIF(CIFLVL),1)=4
*   Go back one step in the IF trace.
                 CIFLVL=CIFLVL-1
            ENDIF
*** An ordinary line.
       ELSE
*   Reference information.
            LINREF(NDOLIN,1)=0
            LINREF(NDOLIN,3)=TRACDO(CDOLVL)
            LINREF(NDOLIN,6)=TRACIF(CIFLVL)
       ENDIF
*** Check also for global variables.
       IF(INPCMP(1,'GL#OBALS').NE.0.AND.NWORD.GE.2)THEN
*   Ensure that there is no evaluation in the statement anywhere.
            CALL INPSTR(2,NWORD,STRING,NC)
            IF(INDEX(STRING(1:NC),'{')+INDEX(STRING(1:NC),'}').NE.0)
     -           GOTO 186
*   Assign the line type.
            LINREF(NDOLIN,1)=21
**  Fetch the name of the variable.
            IGLB=0
            CALL INPSTR(2,2,STRING,NC)
*   Find out whether this is a matrix indexing expression.
            IF(INDEX(STRING(1:NC),'[').GT.1.AND.
     -           STRING(NC:NC).EQ.']')THEN
                 NCSTR=INDEX(STRING(1:NC),'[')-1
                 INDSTR=STRING(NCSTR+1:NC)
                 NCIND=NC-NCSTR
            ELSE
                 NCSTR=NC
                 INDSTR=' '
                 NCIND=0
            ENDIF
*   Check for illegal characters.
            ILLCHR=0
            DO 185 J=1,NCSTR
            IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',STRING(J:J)).NE.0)THEN
                 ILLCHR=ILLCHR+1
                 OK=.FALSE.
            ENDIF
185         CONTINUE
            IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRING(1:1)).EQ.0)THEN
                 CALL INPMSG(2,'Does not start with a letter. ')
                 OK=.FALSE.
            ELSEIF(ILLCHR.EQ.1)THEN
                 CALL INPMSG(2,'Contains an illegal character.')
            ELSEIF(ILLCHR.GT.1)THEN
                 CALL INPMSG(2,'Contains illegal characters.  ')
*   Check the name is not more than 10 characters long.
            ELSEIF(NCSTR.GT.10)THEN
                 CALL INPMSG(2,'Name longer than 10 characters')
                 OK=.FALSE.
*   Check the name is not empty.
            ELSEIF(NCSTR.LE.0)THEN
                 CALL INPMSG(2,'Empty names are not permitted.')
                 OK=.FALSE.
            ELSE
*   Figure out which variable to redefine.
                 DO 150 I=1,NGLB
                 IF(GLBVAR(I).EQ.STRING(1:NCSTR))THEN
                      IGLB=I
                      GOTO 160
                 ENDIF
150              CONTINUE
*   See whether there still is space to store a new global.
                 IF(NGLB.GE.MXVAR)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Unable to'//
     -                     ' store global variable "'//STRING(1:NCSTR)//
     -                     '"; increase MXVAR and recompile.'
                      OK=.FALSE.
                      GOTO 186
*   Add the new global.
                 ELSE
                      NGLB=NGLB+1
                      GLBVAR(NGLB)=STRING(1:NCSTR)
                      GLBMOD(NGLB)=0
                 ENDIF
                 IGLB=NGLB
160              CONTINUE
*   Ensure that this variable is not a system variable.
                 IF(IGLB.LE.4)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : '//
     -                     STRING(1:NCSTR)//' may not be redefined;'//
     -                     ' definition ignored.'
                      OK=.FALSE.
                      GOTO 186
                 ENDIF
            ENDIF
*   Store the reference, -1 for indexed assignments (list takes care).
            IF(NCIND.EQ.0)THEN
                 LINREF(NDOLIN,7)=IGLB
            ELSE
                 LINREF(NDOLIN,7)=-1
            ENDIF
**  Fetch the expression.
            IF(NWORD.GE.3)THEN
                 CALL INPSTR(3,NWORD,STRING,NC)
            ELSE
                 STRING='NILL'
                 NC=4
            ENDIF
**  Translate the expression, first with indexing.
            IF(NCIND.NE.0)THEN
                 CALL ALGPRE('('//STRING(1:NC)//')'//INDSTR(1:NCIND),
     -                NC+NCIND+2,GLBVAR,NGLB,NNRES,USE,LINREF(NDOLIN,8),
     -                IFAIL)
*   Check validity.
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Unable to'//
     -                     ' process the indexing expression; global'//
     -                     ' not assigned.'
                      OK=.FALSE.
                      GOTO 186
                 ELSEIF(NNRES.NE.1)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Formula'//
     -                     ' doesn''t lead to 1 result; global not'//
     -                     ' assigned.'
                      OK=.FALSE.
                      GOTO 186
                 ENDIF
*   Locate the entry point number.
                 IENTNO=0
                 DO 70 I=1,NALGE
                 IF(ALGENT(I,1).EQ.LINREF(NDOLIN,8).AND.
     -                ALGENT(I,3).EQ.1)IENTNO=I
70               CONTINUE
                 IF(IENTNO.EQ.0)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : No valid'//
     -                     ' indexing entry point found; global'//
     -                     ' not assigned.'
                      OK=.FALSE.
                      GOTO 186
                 ENDIF
*   Locate the final EXTRACT_SUBMATRIX call.
                 DO 80 I=ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1,
     -                ALGENT(IENTNO,5)+2,-1
                 IF(INS(I,1).EQ.-80.AND.INS(I,2).EQ.9.AND.
     -                INS(I-1,2).EQ.8.AND.INS(I-2,2).EQ.8)THEN
                      IEXTR=I
                      GOTO 90
                 ENDIF
80               CONTINUE
                 PRINT *,' !!!!!! INPRDO WARNING : Instruction list'//
     -                ' tail not as expected.'
                 OK=.FALSE.
                 GOTO 186
90               CONTINUE
*   Store the location of the last instruction.
                 ILAST=ALGENT(IENTNO,5)+ALGENT(IENTNO,6)-1
*   Replace result and return by DELETE_MATRIX on temporary matrix.
                 INS(ILAST-1,1)=  0
                 INS(ILAST-1,2)=  8
                 INS(ILAST-1,3)=INS(IEXTR-2,3)
                 INS(ILAST-1,4)=  1
                 INS(ILAST  ,1)=-86
                 INS(ILAST  ,2)=  9
                 INS(ILAST  ,3)=  1
                 INS(ILAST  ,4)=  0
*   Replace EXTRACT_SUBMATRIX by STORE_SUBMATRIX.
                 INS(IEXTR  ,1)=-81
*   Exchange the in/out matrices, assign to global, fix protections.
                 INS(IEXTR-1,1)=  3
                 INS(IEXTR-1,3)=INS(IEXTR-2,3)
                 INS(IEXTR-2,1)=  0
                 INS(IEXTR-2,3)=IGLB
*** In debug mode, print the list.
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(''  ++++++ INPRDO DEBUG   : List'',
     -                     '' after processing indexing calls:'')')
                      CALL ALGPRT(ALGENT(IENTNO,5),ALGENT(IENTNO,5)+
     -                     ALGENT(IENTNO,6)-1)
                 ENDIF
**  Translate for the case without indexing.
            ELSE
                 CALL ALGPRE(STRING(1:NC),NC,
     -                GLBVAR,NGLB,NNRES,USE,LINREF(NDOLIN,8),IFAIL)
*   Check validity.
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Unable to'//
     -                     ' process the indexing expression; global'//
     -                     ' not assigned.'
                      OK=.FALSE.
                 ELSEIF(NNRES.NE.1)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Formula'//
     -                     ' doesn''t lead to 1 result; global not'//
     -                     ' assigned.'
                      OK=.FALSE.
                 ENDIF
            ENDIF
**  Resume here for non-translatable GLOBALs.
186         CONTINUE
*** Declare variables used in VECTOR statements.
       ELSEIF(INPCMP(1,'VECT#ORS-#READ')+
     -      INPCMP(1,'R#EAD-VECT#OR').NE.0)THEN
*   Ensure that there is no evaluation in the statement anywhere.
            CALL INPSTR(2,NWORD,STRING,NC)
            IF(INDEX(STRING(1:NC),'{')+INDEX(STRING(1:NC),'}').NE.0)
     -           GOTO 188
**  Loop over the vector names.
            DO 230 I=2,NWORD
*   Skip dummy fields.
            IF(INPCMP(I,'DUMMY').NE.0)GOTO 230
*   Fetch the variable name.
            CALL INPSTR(I,I,STRING,NCSTR)
*   Check for illegal characters.
            ILLCHR=0
            DO 240 J=1,NCSTR
            IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',STRING(J:J)).NE.0)THEN
                 ILLCHR=ILLCHR+1
                 OK=.FALSE.
            ENDIF
240         CONTINUE
            IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRING(1:1)).EQ.0)THEN
                 CALL INPMSG(I,'Does not start with a letter. ')
                 OK=.FALSE.
            ELSEIF(ILLCHR.EQ.1)THEN
                 CALL INPMSG(I,'Contains an illegal character.')
            ELSEIF(ILLCHR.GT.1)THEN
                 CALL INPMSG(I,'Contains illegal characters.  ')
*   Check the name is not more than 10 characters long.
            ELSEIF(NCSTR.GT.10)THEN
                 CALL INPMSG(I,'Name longer than 10 characters')
                 OK=.FALSE.
*   Check the name is not empty.
            ELSEIF(NCSTR.LE.0)THEN
                 CALL INPMSG(I,'Empty names are not permitted.')
                 OK=.FALSE.
            ELSE
*   Figure out whether this variable already exists.
                 IGLB=0
                 DO 250 J=1,NGLB
                 IF(GLBVAR(J).EQ.STRING(1:NCSTR))THEN
                      IGLB=J
                      GOTO 260
                 ENDIF
250              CONTINUE
*   See whether there still is space to store a new global.
                 IF(NGLB.GE.MXVAR)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : Unable to'//
     -                     ' store global variable "'//STRING(1:NCSTR)//
     -                     '"; increase MXVAR and recompile.'
                      OK=.FALSE.
                      GOTO 230
*   Add the new global.
                 ELSE
                      NGLB=NGLB+1
                      GLBVAR(NGLB)=STRING(1:NCSTR)
                      GLBMOD(NGLB)=0
                      WRITE(LUNOUT,'(''  ------ INPRDO MESSAGE : '',A,
     -                     '' declared as a global variable.'')')
     -                     STRING(1:NCSTR)
                 ENDIF
                 IGLB=NGLB
260              CONTINUE
*   Ensure that this variable is not a system variable.
                 IF(IGLB.LE.4)THEN
                      PRINT *,' !!!!!! INPRDO WARNING : '//
     -                     STRING(1:NCSTR)//' may not be redefined;'//
     -                     ' definition ignored.'
                      OK=.FALSE.
                      GOTO 230
                 ENDIF
            ENDIF
*   Next vector.
230         CONTINUE
*   Skip if there are { }.
188         CONTINUE
*** And for procedure calls.
       ELSEIF(INPCMP(1,'CALL').NE.0.AND.NWORD.GE.2)THEN
*   Ensure that there is no evaluation in the statement anywhere.
            CALL INPSTR(2,NWORD,STRING,NC)
            IF(INDEX(STRING(1:NC),'{')+INDEX(STRING(1:NC),'}').NE.0)
     -           GOTO 187
*   Assign the line type.
            LINREF(NDOLIN,1)=22
*   Generate an entry point.
            CALL INPCAL('STORE',LINREF(NDOLIN,8),IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! INPRDO WARNING : CALL statement'//
     -                ' could not be processed.'
                 OK=.FALSE.
            ENDIF
*   Resume here for non-translatable CALLs.
187         CONTINUE
       ENDIF
*** Ensure there is no input-redirect
       CALL INPSTR(1,1,STRING,NC)
       IF(STRING(1:1).EQ.'<'.AND..NOT.LINPRD)THEN
            PRINT *,' !!!!!! INPRDO WARNING : Input redirection is'//
     -           ' not permitted inside a loop; loop rejected.'
            OK=.FALSE.
       ENDIF
*** Store the line in the buffer, no matter the contents.
       CALL INPRAW(STRING)
       DO 300 I=MXINCH,1,-1
       IF(STRING(I:I).NE.' ')THEN
            I1=I
            GOTO 310
       ENDIF
300    CONTINUE
       I1=1
310    CONTINUE
       DO 320 I=1,I1
       IF(STRING(I:I).NE.' ')THEN
            I0=I
            GOTO 330
       ENDIF
320    CONTINUE
       I0=1
330    CONTINUE
       CALL STRBUF('STORE',LINREF(NDOLIN,2),STRING(I0:I1),I1-I0+1,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! INPRDO WARNING : Unable to store an'//
     -           ' input line.'
            OK=.FALSE.
       ENDIF
*** Dump the error messages.
       CALL INPERR
*** And read the next line, if we're still in the loop nest.
       IF(CDOLVL.GT.0)THEN
*   Format the prompt.
            PROMPT=' '
            IF(CIFLVL.GT.0.AND.CDOLVL.GT.0)THEN
                 WRITE(PROMPT,'(''Do_'',I3,''_If_'',I3)') CDOLVL,CIFLVL
            ELSEIF(CDOLVL.GT.0)THEN
                 WRITE(PROMPT,'(''Do_'',I3)') CDOLVL
            ELSEIF(CIFLVL.GT.0)THEN
                 WRITE(PROMPT,'(''If_'',I3)') CIFLVL
            ELSE
                 PROMPT='Loop'
            ENDIF
            NCPRM=0
            DO 400 I=1,13
            IF(PROMPT(I:I).NE.' ')THEN
                 NCPRM=NCPRM+1
                 PROMPT(NCPRM:NCPRM)=PROMPT(I:I)
            ENDIF
400         CONTINUE
            CALL INPPRM(' ','BACK')
            CALL INPPRM(PROMPT(1:MAX(1,NCPRM)),'ADD')
*   Read the new line.
            CALL INPGET
            GOTO 10
       ENDIF
*** End of the loop nest has been reached, debugging output.
       IF(LDEBUG)THEN
*   Header.
            WRITE(LUNOUT,'(/2X,''OVERVIEW OF THE DO LOOP NEST''//
     -           2X,''Number of input lines: '',I3/
     -           2X,''Deepest nesting level: '',I3,'' / '',I3//,
     -           2X,''Line Type     Loop Cond Jump'',
     -           ''   If  Glb Entr     Contents'')')
     -           NDOLIN,MAXDOL,MAXIFL
*   Listing.
            BLANK=' '
            CIFLVL=0
            CDOLVL=0
            DO 200 I=1,NDOLIN
            CALL STRBUF('READ',LINREF(I,2),STRING,NC,IFAIL)
            IF(LINREF(I,1).EQ.4)CDOLVL=CDOLVL-1
            IF(LINREF(I,1).EQ.12.OR.LINREF(I,1).EQ.13.OR.
     -           LINREF(I,1).EQ.14)CIFLVL=CIFLVL-1
            IF(LINREF(I,1).EQ.0)THEN
                 TYPE='        '
            ELSEIF(LINREF(I,1).EQ.1)THEN
                 TYPE='Do-block'
            ELSEIF(LINREF(I,1).EQ.2)THEN
                 TYPE='Iterate '
            ELSEIF(LINREF(I,1).EQ.3)THEN
                 TYPE='Leave   '
            ELSEIF(LINREF(I,1).EQ.4)THEN
                 TYPE='Enddo   '
            ELSEIF(LINREF(I,1).EQ.11)THEN
                 TYPE='If-block'
            ELSEIF(LINREF(I,1).EQ.12)THEN
                 TYPE='Elseif  '
            ELSEIF(LINREF(I,1).EQ.13)THEN
                 TYPE='Else    '
            ELSEIF(LINREF(I,1).EQ.14)THEN
                 TYPE='Endif   '
            ELSEIF(LINREF(I,1).EQ.21)THEN
                 TYPE='Global  '
            ELSEIF(LINREF(I,1).EQ.22)THEN
                 TYPE='Call    '
            ELSE
                 TYPE='Unknown '
            ENDIF
            IF(IFAIL.EQ.0)THEN
                 WRITE(LUNOUT,'(1X,I5,1X,A8,6I5,5X,A)')
     -                I,TYPE,(LINREF(I,J),J=3,8),
     -                BLANK(1:MIN(80,MAX(1,1+3*(CDOLVL+CIFLVL))))//
     -                STRING(1:NC)
            ELSE
                 WRITE(LUNOUT,'(1X,I5,1X,A8,6I5,5X,
     -                ''# Unable to retrieve'')')
     -                I,TYPE,(LINREF(I,J),J=3,8)
            ENDIF
            IF(LINREF(I,1).EQ.1)CDOLVL=CDOLVL+1
            IF(LINREF(I,1).EQ.11.OR.LINREF(I,1).EQ.12.OR.
     -           LINREF(I,1).EQ.13)CIFLVL=CIFLVL+1
200         CONTINUE
*   DO loops.
            IF(NLOOP.GE.1)THEN
                 WRITE(LUNOUT,'(/2X,''DO LOOP INDEX''//2X,
     -                '' No Variable    Init  Step While Until    To'',
     -                '' First  Last Level    If    In''/)')
                 DO 210 I=1,NLOOP
                 IF(DOREF(I,9).GT.0)THEN
                      WRITE(LUNOUT,'(2X,I3,1X,A10,10I6)') I,
     -                     GLBVAR(DOREF(I,9)),(DOREF(I,J),J=1,8),
     -                     DOREF(I,10),DOREF(I,11)
                 ELSE
                      WRITE(LUNOUT,'(2X,I3,1X,A10,12X,2I6,6X,4I6)') I,
     -                     ' < none > ',(DOREF(I,J),J=3,4),
     -                     (DOREF(I,J),J=6,8),DOREF(I,10)
                 ENDIF
210              CONTINUE
            ELSE
                 WRITE(LUNOUT,'(/2X,''NO DO LOOPS''/)')
            ENDIF
*   IF blocks.
            IF(NIF.GE.1)THEN
                 WRITE(LUNOUT,'(/2X,''IF BLOCK INDEX''//2X,
     -                '' No     State      Last    Do lvl    If lvl''
     -                /)')
                 DO 220 I=1,NIF
                 WRITE(LUNOUT,'(2X,I3,5I10)') I,IFREF(I,1),IFREF(I,2),
     -                IFREF(I,4),IFREF(I,5)
220              CONTINUE
            ELSE
                 WRITE(LUNOUT,'(/2X,''NO IF BLOCKS''/)')
            ENDIF
       ENDIF
*** Normal end of the routine.
       IF(OK)THEN
            IFAIL=0
            ISTATE=0
       ELSE
            PRINT *,' !!!!!! INPRDO WARNING : The DO loop nest is not'//
     -           ' executable as a result of the above errors.'
            IFAIL=1
            ISTATE=-1
            CALL INPCDO
       ENDIF
*   Reset the prompt.
       CALL INPPRM(' ','BACK')
       END
