CDECK  ID>, INPTMP.
       SUBROUTINE INPTMP(STRING,NCSTR,FORMAT,NCFMT,EXEC,IFAIL)
*-----------------------------------------------------------------------
*   INPTMP - Studies the template and the input string to assign the
*            global variables for the Parse instruction.
*   (Last changed on 10/11/00.)
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
       CHARACTER*(*) STRING,FORMAT
       CHARACTER*5 AUXSTR
       INTEGER MXELEM
       PARAMETER(MXELEM=100)
       REAL RES(1)
       INTEGER NCSTR,NCFMT,LIST(MXELEM,3),MODRES(1),IFAIL,NELEM,I0,I,I1,
     -      ILAST,INEXT,IOK,J,JSTART,JEND,JNEXT,NRES,IFAIL1,IENTRY,K,
     -      IGLB,IREF,IMODE
       LOGICAL USE(MXVAR),EXEC
*** Identify the routine for tracing purposes.
       IF(LIDENT)PRINT *,' /// ROUTINE INPTMP ///'
*** Initial debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ INPTMP DEBUG   : String: "'',
     -      A,''",''/26X,''Format: "'',A,''".'')')
     -      STRING(1:NCSTR),FORMAT(1:NCFMT)
*** Initialise.
       NELEM=0
       IFAIL=0
*** Read the fragments of the format.
       INEXT=1
       DO 10 I0=1,NCFMT
**  Skip if we have read further already.
       IF(I0.LT.INEXT)GOTO 10
**  Skip blanks.
       IF(FORMAT(I0:I0).EQ.' ')THEN
            GOTO 10
**  Full stop.
       ELSEIF(FORMAT(I0:I0).EQ.'.')THEN
            NELEM=NELEM+1
            IF(NELEM.GE.MXELEM)GOTO 3010
            LIST(NELEM,1)=3
            LIST(NELEM,2)=I0
            LIST(NELEM,3)=I0
            INEXT=I0+1
**  Start of a quoted portion.
       ELSEIF(FORMAT(I0:I0).EQ.''''.OR.
     -      FORMAT(I0:I0).EQ.'"'.OR.
     -      FORMAT(I0:I0).EQ.'`')THEN
*   Locate the end of the string.
            DO 20 I=I0+1,NCFMT
            IF(FORMAT(I:I).EQ.FORMAT(I0:I0))THEN
                 I1=I
                 INEXT=I1+1
                 GOTO 30
            ENDIF
20          CONTINUE
            INEXT=NCFMT+1
            I1=NCFMT+1
30          CONTINUE
*   Make sure that the quoted portion is not empty.
            IF(I0+1.GT.I1-1)GOTO 10
*   Store the string.
            NELEM=NELEM+1
            IF(NELEM.GE.MXELEM)GOTO 3010
            LIST(NELEM,1)=2
            LIST(NELEM,2)=I0+1
            LIST(NELEM,3)=I1-1
**  Start of a variable name.
       ELSE
            DO 40 I1=I0+1,NCFMT
            IF(INDEX(' .''"`',FORMAT(I1:I1)).NE.0)THEN
                 ILAST=I1-1
                 INEXT=I1
                 GOTO 50
            ENDIF
40          CONTINUE
            ILAST=NCFMT
            INEXT=NCFMT+1
50          CONTINUE
*   Check validity of the name.
            IOK=1
*   Check the name starts with a character.
            IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     -           FORMAT(I0:I0)).EQ.0)THEN
                 PRINT *,' !!!!!! INPTMP WARNING : The variable name '//
     -                '"',FORMAT(I0:ILAST),'" does not start with'//
     -                ' an uppercase letter.'
                 IFAIL=1
                 IOK=0
            ENDIF
*   Check for illegal characters.
            DO 60 I=I0,ILAST
            IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',FORMAT(I:I)).NE.0)THEN
                 PRINT *,' !!!!!! INPTMP WARNING : The variable name '//
     -                '"',FORMAT(I0:ILAST),'" contains the illegal'//
     -                ' character "',FORMAT(I:I),'".'
                 IFAIL=1
                 IOK=0
            ENDIF
60          CONTINUE
*   Make sure the name is not empty.
            IF(FORMAT(I0:ILAST).EQ.' '.OR.ILAST.LT.I0)THEN
                 PRINT *,' !!!!!! INPTMP WARNING : A variable name'//
     -                ' is empty.'
                 IFAIL=1
                 IOK=0
            ENDIF
*   Warn if the name is longer than 10 characters.
            IF(ILAST-I0+1.GT.10)PRINT *,' !!!!!! INPTMP WARNING :'//
     -           ' The variable name "',FORMAT(I0:ILAST),'" is'//
     -           ' truncated to the first 10 characters.'
*   Store the string.
            IF(IOK.EQ.1)THEN
                 NELEM=NELEM+1
                 IF(NELEM.GE.MXELEM)GOTO 3010
                 LIST(NELEM,1)=1
                 LIST(NELEM,2)=I0
                 LIST(NELEM,3)=ILAST
            ELSE
                 PRINT *,' !!!!!! INPTMP WARNING : Variable "',
     -                FORMAT(I0:ILAST),'" won''t be assigned a value.'
                 NELEM=NELEM+1
                 IF(NELEM.GE.MXELEM)GOTO 3010
                 LIST(NELEM,1)=3
                 LIST(NELEM,2)=I0
                 LIST(NELEM,3)=ILAST
            ENDIF
       ENDIF
**  Next character.
10     CONTINUE
*** End of loop over the format.
100    CONTINUE
*** Add an end-of-list marker just past the end of the list.
       LIST(MIN(NELEM+1,MXELEM),1)=4
       LIST(MIN(NELEM+1,MXELEM),2)=1
       LIST(MIN(NELEM+1,MXELEM),3)=NCFMT
*** Print the structure of the string.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ INPTMP DEBUG   : Structure of'',
     -           '' the format |'',A,''|: '')') FORMAT(1:NCFMT)
            DO 170 I=1,NELEM
            IF(LIST(I,1).EQ.1)THEN
                 WRITE(LUNOUT,'(9X,''Variable:  |'',A,''|'')')
     -                FORMAT(LIST(I,2):LIST(I,3))
            ELSEIF(LIST(I,1).EQ.2)THEN
                 WRITE(LUNOUT,'(9X,''String:    |'',A,''|'')')
     -                FORMAT(LIST(I,2):LIST(I,3))
            ELSEIF(LIST(I,1).EQ.3)THEN
                 WRITE(LUNOUT,'(9X,''Ignore:    |'',A,''|'')')
     -                FORMAT(LIST(I,2):LIST(I,3))
            ELSE
                 WRITE(LUNOUT,'(9X,''# Unknown: |'',A,''| #'')')
     -                FORMAT(LIST(I,2):LIST(I,3))
                 IFAIL=1
            ENDIF
170         CONTINUE
       ENDIF
*** Find the start of the input string.
       DO 210 J=1,NCSTR
       IF(STRING(J:J).NE.' ')THEN
            JNEXT=J
            GOTO 220
       ENDIF
210    CONTINUE
       JNEXT=NCSTR+1
220    CONTINUE
*** Loop over the elements to be assigned.
       DO 110 I=1,NELEM
**  Make sure we're not yet past the end of the string.
       IF(JNEXT.GT.NCSTR)THEN
            DO 160 J=I,NELEM
            IF(LIST(J,1).EQ.1)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ INPTMP DEBUG   :'',
     -                '' Variable '',A,'' not assigned.'')')
     -                FORMAT(LIST(J,2):LIST(J,3))
*   Locate the global variable and clear it if it is in use.
                 DO 230 K=1,NGLB
                 IF(GLBVAR(K).EQ.FORMAT(LIST(J,2):LIST(J,3)))THEN
                      IGLB=K
                      CALL ALGREU(NINT(GLBVAL(IGLB)),GLBMOD(IGLB),0)
                      GOTO 240
                 ENDIF
230              CONTINUE
                 IF(NGLB.GE.MXVAR)THEN
                      PRINT *,' !!!!!! INPTMP WARNING : No room for'//
     -                     ' a new global variable; definition of',
     -                     FORMAT(LIST(J,2):LIST(J,3)),' ignored.'
                      IFAIL=1
                      GOTO 160
                 ENDIF
                 NGLB=NGLB+1
                 IGLB=NGLB
                 GLBVAR(NGLB)=FORMAT(LIST(J,2):LIST(J,3))
                 GLBMOD(NGLB)=0
*   Ensure that this variable is not a system variable.
240              CONTINUE
                 IF(IGLB.LE.7)THEN
                      PRINT *,' !!!!!! INPTMP WARNING : Variable ',
     -                     FORMAT(LIST(J,2):LIST(J,3)),' may not be'//
     -                     ' modified by the user.'
                      IFAIL=1
                      GOTO 160
                 ENDIF
*   Assign to the global variable.
                 GLBVAL(IGLB)=0
                 GLBMOD(IGLB)=0
            ENDIF
160         CONTINUE
            GOTO 200
       ENDIF
**  Element is a variable name or a dot.
       IF(LIST(I,1).EQ.1.OR.LIST(I,1).EQ.3)THEN
*   Case 1: the variable is followed by a string.
            IF(LIST(I+1,1).EQ.2)THEN
*   Locate the string.
                 JEND=INDEX(STRING(JNEXT:NCSTR),
     -                FORMAT(LIST(I+1,2):LIST(I+1,3)))
                 IF(JEND.EQ.0)THEN
                      JEND=NCSTR
                 ELSE
                      JEND=JEND+JNEXT-2
                 ENDIF
*   Case 2: the variable is followed by another variable or a dot.
            ELSEIF(LIST(I+1,1).EQ.1.OR.LIST(I+1,1).EQ.3)THEN
*   Locate the blank separating the two variables.
                 JEND=INDEX(STRING(JNEXT:NCSTR),' ')
                 IF(JEND.EQ.0)THEN
                      JEND=NCSTR
                 ELSE
                      JEND=JEND+JNEXT-2
                 ENDIF
*   Case 3: the variable is not followed by anything.
            ELSEIF(LIST(I+1,1).EQ.4)THEN
*   Take all that remains.
                 JEND=NCSTR
*   Other cases: should not occur.
            ELSE
                 PRINT *,' !!!!!! INPTMP WARNING : Unrecognised'//
     -                ' format code received.'
                 JEND=NCSTR
                 IFAIL=1
            ENDIF
*   Evaluate the expression.
            IF((LIST(I+1,1).GE.1.AND.LIST(I+1,1).LE.4).AND.
     -           LIST(I,1).EQ.1)THEN
*   Start with debugging output.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ INPTMP DEBUG   :'',
     -                '' Evaluating '',A,'' for assignment to '',A,
     -                ''.'')') STRING(JNEXT:JEND),
     -                FORMAT(LIST(I,2):LIST(I,3))
**  In execution mode, evaluate the input expression.
                 IF(EXEC)THEN
*   Translation step.
                      CALL ALGPRE(STRING(JNEXT:JEND),JEND-JNEXT+1,
     -                     GLBVAR,NGLB,NRES,USE,IENTRY,IFAIL1)
*   Make sure that the formula was OK.
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! INPTMP WARNING :'//
     -                          ' Translating ',STRING(JNEXT:JEND),
     -                          ' failed; ',FORMAT(LIST(I,2):LIST(I,3)),
     -                          ' not assigned.'
                           IFAIL=1
                           CALL ALGCLR(IENTRY)
                           GOTO 300
*   Verify that we get indeed only one result.
                      ELSEIF(NRES.NE.1)THEN
                           PRINT *,' !!!!!! INPTMP WARNING :'//
     -                          ' Translating ',STRING(JNEXT:JEND),
     -                          ' does not yield 1 result;',
     -                          FORMAT(LIST(I,2):LIST(I,3)),
     -                          ' not assigned.'
                           CALL ALGCLR(IENTRY)
                           IFAIL=1
                           GOTO 300
                      ENDIF
*   Set the execution time.
                      CALL TIMEL(GLBVAL(1))
*   Evaluate the formula.
                      CALL ALGEXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,
     -                     MODRES,1,IFAIL1)
*   Check the return code of the evaluation.
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! INPTMP WARNING :'//
     -                          ' Evaluation of'//
     -                          ' expression ',STRING(JNEXT:JEND),
     -                          ' failed; ',FORMAT(LIST(I,2):LIST(I,3)),
     -                          ' not assigned.'
                           CALL ALGCLR(IENTRY)
                           IFAIL=1
                           GOTO 300
                      ENDIF
*   Print any evaluation errors.
                      CALL ALGERR
*   Remove the entry point of the formula.
                      CALL ALGCLR(IENTRY)
**  In non-execution mode, store the result according to type.
                 ELSE
*   Determine the type.
                      CALL ALGTYP(STRING(JNEXT:JEND),IMODE)
*   Take care of Undefined.
                      IF(IMODE.EQ.0)THEN
                           RES(1)=0.0
*   Take care of strings.
                      ELSEIF(IMODE.EQ.1)THEN
                           CALL STRBUF('STORE',IREF,STRING(JNEXT:JEND),
     -                          JEND-JNEXT+1,IFAIL1)
                           RES(1)=REAL(IREF)
                           IF(IFAIL1.NE.0)PRINT *,' !!!!!! INPTMP'//
     -                          ' WARNING : Unable to store the',
     -                          ' String ',STRING(JNEXT:JEND),'.'
*   Take care of numbers.
                      ELSEIF(IMODE.EQ.2)THEN
                           CALL INPRRC(STRING(JNEXT:JEND),RES(1),0.0,
     -                          IFAIL1)
                           IF(IFAIL1.NE.0)PRINT *,' !!!!!! INPTMP'//
     -                          ' WARNING : ',STRING(JNEXT:JEND),
     -                          ' is not a valid Number.'
*   Take care of logicals.
                      ELSEIF(IMODE.EQ.3)THEN
                           AUXSTR=STRING(JNEXT:JEND)
                           CALL CLTOU(AUXSTR)
                           IF(AUXSTR.EQ.'TRUE ')THEN
                                RES(1)=1.0
                           ELSEIF(AUXSTR.EQ.'FALSE')THEN
                                RES(1)=0.0
                           ELSE
                           IF(IFAIL1.NE.0)PRINT *,' !!!!!! INPTMP'//
     -                          ' WARNING : ',STRING(JNEXT:JEND),
     -                          ' is not a valid Logical.'
                                RES(1)=-6
                                IMODE=0
                           ENDIF
*   All the rest, we assign as Undefined.
                      ELSE
                           RES(1)=-6
                           IMODE=0
                           IF(IFAIL1.NE.0)PRINT *,' !!!!!! INPTMP'//
     -                          ' WARNING : ',STRING(JNEXT:JEND),
     -                          ' is not of a type valid with Parse.'
                      ENDIF
                      MODRES(1)=IMODE
                 ENDIF
**  Locate the global variable and clear it if it is in use.
                 DO 180 K=1,NGLB
                 IF(GLBVAR(K).EQ.FORMAT(LIST(I,2):LIST(I,3)))THEN
                      IGLB=K
                      CALL ALGREU(NINT(GLBVAL(IGLB)),GLBMOD(IGLB),0)
                      GOTO 190
                 ENDIF
180              CONTINUE
                 IF(NGLB.GE.MXVAR)THEN
                      PRINT *,' !!!!!! INPTMP WARNING : No room for'//
     -                     ' a new global variable; definition of',
     -                     FORMAT(LIST(I,2):LIST(I,3)),' ignored.'
                      IFAIL=1
                      GOTO 300
                 ENDIF
                 NGLB=NGLB+1
                 IGLB=NGLB
                 GLBVAR(NGLB)=FORMAT(LIST(I,2):LIST(I,3))
                 GLBMOD(NGLB)=0
*   Ensure that this variable is not a system variable.
190              CONTINUE
                 IF(IGLB.LE.7)THEN
                      PRINT *,' !!!!!! INPTMP WARNING : Variable ',
     -                     FORMAT(LIST(I,2):LIST(I,3)),' may not be'//
     -                     ' modified by the user.'
                      IFAIL=1
                      GOTO 300
                 ENDIF
*   Assign to the global variable.
                 GLBVAL(IGLB)=RES(1)
                 GLBMOD(IGLB)=MODRES(1)
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ INPTMP DEBUG   :'',
     -                '' Ignoring '',A,''.'')') STRING(JNEXT:JEND)
            ENDIF
*   Update the pointer.
300         CONTINUE
            DO 140 J=JEND+1,NCSTR
            IF(STRING(J:J).NE.' ')THEN
                 JNEXT=J
                 GOTO 150
            ENDIF
140         CONTINUE
            JNEXT=NCSTR+1
150         CONTINUE
**  Element is a string.
       ELSEIF(LIST(I,1).EQ.2)THEN
*   Locate the string.
            JSTART=INDEX(STRING(JNEXT:NCSTR),
     -           FORMAT(LIST(I,2):LIST(I,3)))
            IF(JSTART.EQ.0)THEN
                 JSTART=NCSTR
            ELSE
                 JSTART=JSTART+JNEXT-2
            ENDIF
*   Update pointer.
            DO 120 J=JSTART+LIST(I,3)-LIST(I,2)+2,NCSTR
            IF(STRING(J:J).NE.' ')THEN
                 JNEXT=J
                 GOTO 130
            ENDIF
120         CONTINUE
            JNEXT=NCSTR+1
130         CONTINUE
**  Anything else is not valid.
       ELSE
            PRINT *,' !!!!!! INPTMP WARNING : Invalid format code'//
     -           ' received.'
            IFAIL=1
       ENDIF
110    CONTINUE
*** End of the loop over the format elements.
200    CONTINUE
*** Normally the end of the routine.
       RETURN
*** Handle table overflow.
3010   CONTINUE
*   Print error message.
       PRINT *,' !!!!!! INPTMP WARNING : Too many elements in the'//
     -      ' format; excess ignored.'
*   Remember that something went wrong.
       IFAIL=1
*   Reduce element counter by 1.
       NELEM=MXELEM-1
*   Place an end-of-list marker in element MXELEM
       LIST(MXELEM,1)=4
       LIST(MXELEM,2)=1
       LIST(MXELEM,3)=NCFMT
*   With this truncated list, identify the words.
       GOTO 100
       END
