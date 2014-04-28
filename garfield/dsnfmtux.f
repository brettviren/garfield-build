CDECK  ID>, DSNFMTUX.
       SUBROUTINE DSNFMT(F_IN,NC_IN,F_OUT,NC_OUT,ACCESS,IFAIL)
*-----------------------------------------------------------------------
*   DSNFMT - Searches for the full file name specification, taking the
*            environment variables into account.
*   (Last changed on  7/12/95.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXNAME) F_OUT
       CHARACTER*80 AUX
       CHARACTER*(*) F_IN,ACCESS
       INTEGER INPCMP,NC_IN,NC_OUT,IFAIL,I,J,INEXT,IEND,ICASE
       EXTERNAL INPCMP
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DSNFMT (Unix+Cygwin) ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DSNFMT DEBUG   : Input'',
     -      '' file name:  '',A,'' (length='',I3,'').'')')
     -      F_IN(1:MAX(1,NC_IN)),NC_IN
*** Initialisation.
       F_OUT=' '
       NC_OUT=0
       IFAIL=0
*** Loop over the input string.
       INEXT=1
       DO 10 I=1,NC_IN
**  Skip parts already processed.
       IF(I.LT.INEXT)THEN
            GOTO 10
**  Skip blanks.
       ELSEIF(F_IN(I:I).EQ.' ')THEN
            GOTO 10
**  Look for back slashes (copy the next character literally).
       ELSEIF(F_IN(I:I).EQ.'\\')THEN
            IF(I+1.LT.NC_IN)THEN
                 F_OUT(NC_OUT+1:NC_OUT+1)=F_IN(I+1:I+1)
                 NC_OUT=NC_OUT+1
                 INEXT=I+2
            ENDIF
**  Look for an initial tilde.
       ELSEIF(F_IN(I:I).EQ.'~'.AND.NC_OUT.EQ.0)THEN
*   Get hold of the HOME environment variable.
            CALL GETENV('HOME',AUX)
*   Determine how the tilde should be interpreted.
            IF(I.GE.NC_IN)THEN
                 ICASE=1
            ELSEIF(F_IN(I+1:I+1).NE.'/')THEN
                 ICASE=2
            ELSE
                 ICASE=1
            ENDIF
*   Get rid of blanks and copy the relevant part.
            DO 20 J=LEN(AUX),1,-1
            IF(AUX(J:J).NE.' ')THEN
                 IF((ICASE.EQ.1.AND.NC_OUT+J.GT.LEN(F_OUT)).OR.
     -                (ICASE.EQ.2.AND.NC_OUT+J+9.GT.LEN(F_OUT)))THEN
                      PRINT *,' !!!!!! DSNFMT WARNING : Receiving'//
     -                     ' string to short for substitutions.'
                      IFAIL=1
                      RETURN
                 ELSEIF(ICASE.EQ.1)THEN
                      F_OUT=AUX(1:J)
                      NC_OUT=J
                      GOTO 10
                 ELSE
                      F_OUT=AUX(1:J)//'/../../'//F_IN(I+1:I+1)//'/'
                      NC_OUT=J+9
                      GOTO 10
                 ENDIF
            ENDIF
20          CONTINUE
*   Warn if HOME is empty.
            PRINT *,' !!!!!! DSNFMT WARNING : The HOME environment'//
     -           ' variable is blank or absent; tilde not substituted.'
            IFAIL=1
**  Look for dollars.
       ELSEIF(F_IN(I:I).EQ.'$')THEN
*   Search for the end of the environment variable.
            DO 30 J=I+1,NC_IN
            IF(INDEX('/$ ',F_IN(J:J)).NE.0)THEN
                 IF(J.LE.I+1)THEN
                      PRINT *,' !!!!!! DSNFMT WARNING : No name found'//
     -                     ' between $ and delimiter ; no substitution.'
                      IFAIL=1
                      INEXT=J
                      GOTO 10
                 ELSE
                      IEND=J-1
                      INEXT=J
                      GOTO 40
                 ENDIF
            ENDIF
30          CONTINUE
*   If no end found, take until end of string.
            IF(NC_IN.LT.I+1)THEN
                 PRINT *,' !!!!!! DSNFMT WARNING : No name found'//
     -                ' between $ and end-of-string ; no substitution.'
                 IFAIL=1
                 INEXT=NC_IN+1
                 GOTO 10
            ELSE
                 IEND=NC_IN
                 INEXT=NC_IN+1
            ENDIF
*   Retrieve the environment variable.
40          CONTINUE
            CALL GETENV(F_IN(I+1:IEND),AUX)
*   Get rid of blanks and copy the relevant bit.
            DO 50 J=LEN(AUX),1,-1
            IF(AUX(J:J).NE.' ')THEN
                 IF(NC_OUT+J.GT.LEN(F_OUT))THEN
                      PRINT *,' !!!!!! DSNFMT WARNING : Receiving'//
     -                     ' string too short for substitutions.'
                      IFAIL=1
                      RETURN
                 ELSE
                      F_OUT(NC_OUT+1:NC_OUT+J)=AUX(1:J)
                      NC_OUT=NC_OUT+J
                      GOTO 10
                 ENDIF
            ENDIF
50          CONTINUE
*   Warn if the variable is empty or not known.
            PRINT *,' !!!!!! DSNFMT WARNING : The ',
     -           F_IN(I+1:IEND),' environment variable is'//
     -           ' blank or absent; not substituted.'
            IFAIL=1
**  Anything else should simply be copied.
       ELSE
            IF(NC_OUT+1.GT.LEN(F_OUT))THEN
                 PRINT *,' !!!!!! DSNFMT WARNING : Receiving'//
     -                ' string to short to receive file name.'
                 IFAIL=1
                 RETURN
            ELSE
                 F_OUT(NC_OUT+1:NC_OUT+1)=F_IN(I:I)
                 NC_OUT=NC_OUT+1
                 INEXT=I+1
                 GOTO 10
            ENDIF
       ENDIF
**  Next character.
10     CONTINUE
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DSNFMT DEBUG   : Output'',
     -      '' file name: '',A,'' (length='',I3,'').'')')
     -      F_OUT(1:MAX(1,NC_OUT)),NC_OUT
       END
