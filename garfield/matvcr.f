CDECK  ID>, MATVCR.
       SUBROUTINE MATVCR(IFAIL)
*-----------------------------------------------------------------------
*   MATVCR - Reads vectors from input.
*   VARIABLES : IBLOCK     - Block size for matrix allocation.
*   (Last changed on  2/ 8/09.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
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
       CHARACTER*(MXCHAR) STRING
       CHARACTER*80 OUT
       CHARACTER*20 AUX
       INTEGER ISIZ(MXMDIM),IBLOCK,ISLOT(MXWORD),IREF(MXWORD),NVECT,
     -      NREAD(MXWORD),IGLB(MXWORD),MATSLT,IFAIL,IWORD,NWORD,NC,
     -      I,J,K,
     -      IFAIL1,IFAIL2,INPCMP,NCAUX,IMOD,IDUM,NDUM,NCOUT,ISAME,NSAME
       REAL ELEM
       LOGICAL EXTEND(MXWORD),BLOCK(MXWORD),LPRINT,DONE(MXWORD),OK
       EXTERNAL MATSLT,INPCMP
       PARAMETER(IBLOCK=100,LPRINT=.TRUE.)
*** Assume the routine will fail.
       IFAIL=1
*** Get the number of words.
       CALL INPNUM(NWORD)
*** Set the number of vectors to read.
       NVECT=NWORD-1
       IF(NVECT.LT.1)THEN
            CALL MATADM('LIST',IDUM,NDUM,NDUM,NDUM,IFAIL1)
            RETURN
       ENDIF
*** Read a word at the time.
       OK=.TRUE.
       DO 10 IWORD=2,NWORD
**  Fetch the name of the global.
       CALL INPSTR(IWORD,IWORD,STRING,NC)
*   Skip if the name is "dummy".
       IF(INPCMP(IWORD,'DUMMY').NE.0)THEN
            BLOCK(IWORD-1)=.TRUE.
            IREF(IWORD-1)=-1
            EXTEND(IWORD-1)=.FALSE.
            GOTO 10
       ELSE
            BLOCK(IWORD-1)=.FALSE.
       ENDIF
*   Make sure the name is not empty.
       IF(STRING.EQ.' '.OR.NC.LT.1)THEN
            PRINT *,' !!!!!! MATVCR WARNING : A vector name'//
     -           ' is empty; definition is ignored.'
            BLOCK(IWORD-1)=.TRUE.
            IREF(IWORD-1)=-1
            OK=.FALSE.
            GOTO 10
       ENDIF
*   Check the name starts with a character.
       IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',STRING(1:1)).EQ.0)THEN
            PRINT *,' !!!!!! MATVCR WARNING : The vector name '//
     -           STRING(1:NC)//' does not start with a character.'
            BLOCK(IWORD-1)=.TRUE.
            IREF(IWORD-1)=-1
            OK=.FALSE.
            GOTO 10
       ENDIF
*   Check for illegal characters.
       DO 20 I=1,NC
       IF(INDEX('+-*/&|<=#>^ ,.:;([{)]}''"`',STRING(I:I)).NE.0)THEN
            PRINT *,' !!!!!! MATVCR WARNING : The vector name '//
     -           STRING(1:NC)//' contains at least 1 illegal'//
     -           ' character; ignored.'
            BLOCK(IWORD-1)=.TRUE.
            IREF(IWORD-1)=-1
            OK=.FALSE.
            GOTO 10
       ENDIF
20     CONTINUE
*   Warn if the name is longer than 10 characters.
       IF(NC.GT.10)PRINT *,' ------ MATVCR MESSAGE : The vector name '//
     -      STRING(1:NC)//' is truncated to the first 10 characters.'
**  Scan the list of globals, add an entry if needed.
       DO 30 I=1,NGLB
       IF(GLBVAR(I).EQ.STRING(1:MAX(1,MIN(10,NC))))THEN
            IGLB(IWORD-1)=I
            GOTO 40
       ENDIF
30     CONTINUE
       IF(NGLB.GE.MXVAR)THEN
            PRINT *,' !!!!!! MATVCR WARNING : No room to add '//
     -           STRING(1:NC)//' to the list of global variables;'//
     -           ' definition ignored.'
            BLOCK(IWORD-1)=.TRUE.
            IREF(IWORD-1)=-1
            OK=.FALSE.
            GOTO 10
       ENDIF
       NGLB=NGLB+1
       IGLB(IWORD-1)=NGLB
       GLBVAR(IGLB(IWORD-1))=STRING(1:MAX(1,MIN(10,NC)))
       GLBMOD(IGLB(IWORD-1))=0
**  Ensure that this variable is not a system variable.
40     CONTINUE
       IF(IGLB(IWORD-1).LE.4)THEN
            PRINT *,' !!!!!! MATVCR WARNING : Variable '//STRING(1:NC)//
     -           ' may not be user redefined.'
            BLOCK(IWORD-1)=.TRUE.
            IREF(IWORD-1)=-1
            IGLB(IWORD-1)=-1
            OK=.FALSE.
            GOTO 10
       ENDIF
**  If this is not a matrix, generate one.
       IF(GLBMOD(IGLB(IWORD-1)).NE.5)THEN
*   Erase the current contents.
            CALL ALGREU(NINT(GLBVAL(IGLB(IWORD-1))),
     -           GLBMOD(IGLB(IWORD-1)),0)
*   Create a new matrix for it.
            ISIZ(1)=IBLOCK
            IMOD=2
            CALL MATADM('ALLOCATE',IREF(IWORD-1),1,ISIZ,IMOD,IFAIL1)
*   Quit if the matrix could not be created.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MATVCR WARNING : Unable to allocate'//
     -                ' storage for '//STRING(1:NC)//'; not read.'
                 BLOCK(IWORD-1)=.TRUE.
                 IREF(IWORD-1)=-1
                 OK=.FALSE.
                 GOTO 10
            ENDIF
*   Otherwise register the array with the global variable.
            GLBVAL(IGLB(IWORD-1))=IREF(IWORD-1)
            GLBMOD(IGLB(IWORD-1))=5
*   These can be extended if desired.
            EXTEND(IWORD-1)=.TRUE.
*   If already a matrix, then do/don't extend.
       ELSE
            EXTEND(IWORD-1)=.FALSE.
            IREF(IWORD-1)=NINT(GLBVAL(IGLB(IWORD-1)))
       ENDIF
10     CONTINUE
*** Find the slots for the matrices and open all of them.
       DO 50 I=1,NVECT
       NREAD(I)=0
       IF(IREF(I).LE.0)GOTO 50
       ISLOT(I)=MATSLT(IREF(I))
       IF(ISLOT(I).LE.0)THEN
            PRINT *,' !!!!!! MATVCR WARNING : Matrix '//STRING(1:NC)//
     -           ' can not be located in memory.'
            BLOCK(I)=.TRUE.
            IREF(I)=-1
            OK=.FALSE.
            GOTO 50
       ENDIF
       BLOCK(I)=.FALSE.
50     CONTINUE
*** Check the current error conditions.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### MATVCR ERROR   : Vectors will not be'//
     -           ' read because of the above warnings.'
            DO 60 I=1,NVECT
            IF(IREF(I).GT.0.AND.EXTEND(I))THEN
                 CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! MATVCR WARNING :'//
     -                ' Error clearing up temporary matrices.'
            ENDIF
60          CONTINUE
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### MATVCR ERROR   : Program terminated'//
     -           ' because of the above warnings.'
            CALL QUIT
            RETURN
       ENDIF
*** Read the contents, line by line.
       CALL INPPRM('Matrix','ADD-NOPRINT')
100    CONTINUE
*   Read a line.
       CALL INPWRD(NWORD)
*   Quit if the line is empty.
       IF(NWORD.EQ.0)GOTO 200
*   Make sure no attempt is made to leave the section here.
       CALL INPSTR(1,1,STRING,NC)
       IF(STRING(1:1).EQ.'&')THEN
            PRINT *,' !!!!!! MATVCR WARNING : The section can'//
     -           ' not be left at this point ; line ignored.'
            GOTO 100
       ENDIF
**  If only 1 vector, store the words.
       IF(NVECT.EQ.1)THEN
*   Read each word in turn.
            DO 110 I=1,NWORD
*   Skip the rest if the array is full.
            IF(BLOCK(1))GOTO 110
*   See whether there is need to adjust array length.
            IF(NREAD(1)+1.GT.MLEN(ISLOT(1)))THEN
                 IF(EXTEND(1))THEN
                      ISIZ(1)=MLEN(ISLOT(1))+IBLOCK
                      CALL MATADJ(IREF(1),1,ISIZ,0.0,IFAIL1)
                      ISLOT(1)=MATSLT(IREF(1))
                      IF(ISLOT(1).LE.0)THEN
                           PRINT *,' !!!!!! MATVCR WARNING : Matrix'//
     -                          ' has not been found; program bug.'
                           RETURN
                      ENDIF
                 ELSE
                      IFAIL1=1
                 ENDIF
*   Warn if adjust failed.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! MATVCR WARNING : Vector too'//
     -                     ' short or not extendable; reading stopped.'
                      BLOCK(1)=.TRUE.
                 ENDIF
            ENDIF
*   Store the elements.
            IF(.NOT.BLOCK(1))THEN
                 NREAD(1)=NREAD(1)+1
                 CALL INPCHK(I,2,IFAIL2)
                 CALL INPRDR(I,ELEM,0.0)
                 MVEC(MORG(ISLOT(1))+NREAD(1))=ELEM
            ENDIF
110         CONTINUE
            CALL INPERR
**  Only if only 1 vector is to be read, accept any number of words.
       ELSEIF(NWORD.NE.NVECT)THEN
            PRINT *,' !!!!!! MATVCR WARNING : The # of words'//
     -           ' differs from the # of vectors ; line ignored.'
            GOTO 100
**  More than 1 word: each word on the line goes to a vector.
       ELSE
            DO 120 I=1,NWORD
            IF(BLOCK(I))GOTO 120
*   If not long enough.
            IF(NREAD(I)+1.GT.MLEN(ISLOT(I)))THEN
*   If extendable, try to extend.
                 IF(EXTEND(I))THEN
                      ISIZ(1)=MLEN(ISLOT(I))+IBLOCK
                      CALL MATADJ(IREF(I),1,ISIZ,0.0,IFAIL1)
*   Relocate all matrices.
                      DO 160 J=1,NVECT
                      IF(IREF(J).LE.0)GOTO 160
                      ISLOT(J)=MATSLT(IREF(J))
                      IF(ISLOT(J).LE.0)THEN
                           PRINT *,' !!!!!! MATVCR WARNING : Matrix'//
     -                          ' to be read has not been found.'
                           BLOCK(J)=.TRUE.
                      ENDIF
160                   CONTINUE
*   If not extendable, nothing much can be done.
                 ELSE
                      IFAIL1=1
                 ENDIF
*   Process the errors.
                 IF(IFAIL1.NE.0)THEN
                      NC=1
                      DO 165 K=1,LEN(GLBVAR(IGLB(I)))
                      IF(GLBVAR(IGLB(I))(K:K).NE.' ')NC=K
165                   CONTINUE
                      PRINT *,' !!!!!! MATVCR WARNING : Vector '//
     -                     GLBVAR(IGLB(I))(1:NC)//' is too short or'//
     -                     ' not extendable; reading stopped.'
                      BLOCK(I)=.TRUE.
                 ENDIF
            ENDIF
*   If still open, read the word.
            IF(.NOT.BLOCK(I))THEN
                 NREAD(I)=NREAD(I)+1
                 CALL INPCHK(I,2,IFAIL2)
                 CALL INPRDR(I,ELEM,0.0)
                 MVEC(MORG(ISLOT(I))+NREAD(I))=ELEM
            ENDIF
*   Next word.
120         CONTINUE
*   Print error messages.
            CALL INPERR
       ENDIF
*   New line of input.
       GOTO 100
200    CONTINUE
*   Reset the prompt.
       CALL INPPRM(' ','BACK-PRINT')
*** Truncate the newly created extendable vectors to their real length.
       DO 210 I=1,NVECT
*   Skip dummy fields.
       IF(IREF(I).LE.0)GOTO 210
*   Resize if appropriate.
       IF(EXTEND(I))THEN
            ISIZ(1)=NREAD(I)
            CALL MATADJ(IREF(I),1,ISIZ,0.0,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 NC=1
                 DO 215 K=1,LEN(GLBVAR(IGLB(I)))
                 IF(GLBVAR(IGLB(I))(K:K).NE.' ')NC=K
215              CONTINUE
                 PRINT *,' !!!!!! MATVCR WARNING : Unable to truncate'//
     -                ' matrix '//GLBVAR(IGLB(I))(1:NC)//' to the'//
     -                ' correct size; left with spurious elements.'
            ENDIF
       ENDIF
210    CONTINUE
*** Show number of new elements.
       IF(LPRINT)THEN
*   Initialise the DONE vector.
            DO 220 I=1,NVECT
            IF(IREF(I).LE.0)THEN
                 DONE(I)=.TRUE.
            ELSE
                 DONE(I)=.FALSE.
            ENDIF
220         CONTINUE
*   Initialise the number of output characters.
            OUT=' '
            NCOUT=0
*   Loop over the matrices.
            DO 230 I=1,NVECT
            IF(DONE(I))GOTO 230
*   Count similar vectors.
            NSAME=1
            DO 240 J=I+1,NVECT
            IF(NREAD(J).EQ.NREAD(I))NSAME=NSAME+1
240         CONTINUE
*   Format the number of words.
            CALL OUTFMT(REAL(NREAD(I)),2,AUX,NCAUX,'LEFT')
*   Print the line if full.
            IF(NCOUT+NCAUX+39.GT.LEN(OUT))THEN
                 WRITE(LUNOUT,'(2X,A)') OUT(1:NCOUT)
                 NCOUT=0
            ENDIF
*   Start of line.
            IF(NREAD(I).EQ.0)THEN
                 IF(NSAME.EQ.1)THEN
                      OUT(NCOUT+1:NCOUT+39)=
     -                     'No numbers have been entered in matrix '
                      NCOUT=NCOUT+39
                 ELSE
                      OUT(NCOUT+1:NCOUT+41)=
     -                     'No numbers have been entered in matrices '
                      NCOUT=NCOUT+41
                 ENDIF
            ELSEIF(NREAD(I).EQ.1)THEN
                 IF(NSAME.EQ.1)THEN
                      OUT(NCOUT+1:NCOUT+38)=
     -                     'One number has been entered in matrix '
                      NCOUT=NCOUT+38
                 ELSE
                      OUT(NCOUT+1:NCOUT+40)=
     -                     'One number has been entered in matrices '
                      NCOUT=NCOUT+40
                 ENDIF
            ELSE
                 IF(NSAME.EQ.1)THEN
                      OUT(NCOUT+1:NCOUT+NCAUX+37)=AUX(1:NCAUX)//
     -                     ' numbers have been entered in matrix '
                      NCOUT=NCOUT+NCAUX+37
                 ELSE
                      OUT(NCOUT+1:NCOUT+NCAUX+39)=AUX(1:NCAUX)//
     -                     ' numbers have been entered in matrices '
                      NCOUT=NCOUT+NCAUX+39
                 ENDIF
            ENDIF
*   Loop again over the vectors.
            ISAME=0
            DO 250 J=I,NVECT
            IF(IREF(J).LE.0.OR.NREAD(J).NE.NREAD(I))GOTO 250
            ISAME=ISAME+1
*   Format the variable name.
            NC=1
            DO 260 K=1,LEN(GLBVAR(IGLB(J)))
            IF(GLBVAR(IGLB(J))(K:K).NE.' ')NC=K
260         CONTINUE
*   Print the line if full.
            IF(NCOUT+NC+5.GT.LEN(OUT))THEN
                 WRITE(LUNOUT,'(2X,A)') OUT(1:NCOUT)
                 NCOUT=0
            ENDIF
*   Add the new piece.
            IF(ISAME.LT.NSAME-1)THEN
                 OUT(NCOUT+1:NCOUT+NC+2)=GLBVAR(IGLB(J))(1:NC)//', '
                 NCOUT=NCOUT+NC+2
            ELSEIF(ISAME.EQ.NSAME-1)THEN
                 OUT(NCOUT+1:NCOUT+NC+5)=GLBVAR(IGLB(J))(1:NC)//' and '
                 NCOUT=NCOUT+NC+5
            ELSEIF(ISAME.EQ.NSAME)THEN
                 OUT(NCOUT+1:NCOUT+NC+5)=GLBVAR(IGLB(J))(1:NC)//'.'
                 NCOUT=NCOUT+NC+1
            ENDIF
*   Mark as done.
            DONE(J)=.TRUE.
250         CONTINUE
*   Write out the remainder of the line.
            IF(NCOUT.GT.0)THEN
                 WRITE(LUNOUT,'(2X,A)') OUT(1:NCOUT)
                 NCOUT=0
            ENDIF
230         CONTINUE
       ENDIF
       END
