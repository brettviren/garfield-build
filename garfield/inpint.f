CDECK  ID>, INPINT.
       SUBROUTINE INPINT
*-----------------------------------------------------------------------
*   INPINT - Initialises the input routines. Determines the character
*            set being used (courtesy Carlo Mekenkamp, Leiden).
*   (Last changed on 26/10/07.)
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
       CHARACTER*(MXINCH+1) STRING
       CHARACTER*(MXINCH)   ARGSTR
       CHARACTER*30         ERRCDE(MXWORD)
       CHARACTER*(MXCHAR)   WORD(MXWORD)
       CHARACTER*80         PROMPT,EOFSTR,SHELL
       CHARACTER            ESCAPE
       CHARACTER*(MXNAME)   FNINP,FNOUT
       INTEGER NCHAR(MXWORD),INDWRD(MXWORD),ICHSET,LUNSTR(5:MXLUN,3),
     -      NWORD,LUN,NCPROM,NCEOF,NCSH,NCARG,NCFNI,NCFNO
       LOGICAL ERRPRT(MXWORD),LPROM,DOEXEC,DOREAD,LINREC
       COMMON /INPCOM/ NCHAR,INDWRD,LUNSTR,NWORD,LUN,ICHSET,NCPROM,
     -      ERRPRT,LPROM,DOEXEC,DOREAD,NCEOF,LINREC,NCSH,NCARG,
     -      NCFNI,NCFNO
       COMMON /INPCHR/ ERRCDE,STRING,WORD,PROMPT,EOFSTR,ESCAPE,SHELL,
     -      ARGSTR,FNINP,FNOUT
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       LOGICAL EXIST
       INTEGER NCFILE,IFAIL
       CHARACTER*100 INFILE
       INTEGER I,NCHOME
       CHARACTER*80 HOME
*** Initial input logical unit, first input file.
       LUN=5
       IF(FNINP.EQ.' '.OR.NCFNI.LT.1)THEN
            CALL STRBUF('STORE',LUNSTR(LUN,1),'Standard input',14,IFAIL)
       ELSE
            CLOSE(UNIT=LUN,ERR=2030)
            OPEN(UNIT=LUN,FILE=FNINP(1:NCFNI),ERR=2020)
            CALL STRBUF('STORE',LUNSTR(LUN,1),FNINP(1:NCFNI),NCFNI,
     -           IFAIL)
       ENDIF
       GLBVAR(6)='INPUT     '
       GLBMOD(6)=1
       GLBVAL(6)=LUNSTR(LUN,1)
*   EOF string.
       EOFSTR='EOF'
       NCEOF=3
       CALL STRBUF('STORE',LUNSTR(5,2),EOFSTR(1:NCEOF),NCEOF,IFAIL)
*   Input arguments have been set inside INIT.
       CALL STRBUF('STORE',LUNSTR(5,3),ARGSTR(1:NCARG),NCARG,IFAIL)
*** Look for initialisation file.
       CALL GETENV('HOME',HOME)
       DO 10 I=LEN(HOME),1,-1
       IF(HOME(I:I).NE.' ')THEN
            NCHOME=I
            GOTO 20
       ENDIF
10     CONTINUE
       NCHOME=1
20     CONTINUE
       INFILE='garfinit'
       NCFILE=8
       CALL DSNINQ(INFILE,NCFILE,EXIST)
       IF(.NOT.EXIST)THEN
            INFILE=HOME(1:NCHOME)//'/garfinit'
            NCFILE=MIN(LEN(INFILE),NCHOME+9)
            CALL DSNINQ(INFILE,NCFILE,EXIST)
       ENDIF
       IF(.NOT.EXIST)THEN
            INFILE=HOME(1:NCHOME)//'/.garfinit'
            NCFILE=MIN(LEN(INFILE),NCHOME+10)
            CALL DSNINQ(INFILE,NCFILE,EXIST)
       ENDIF
       IF(.NOT.EXIST)THEN
            INFILE=HOME(1:NCHOME)//'/Garfield/Files/garfinit'
            NCFILE=MIN(LEN(INFILE),NCHOME+24)
            CALL DSNINQ(INFILE,NCFILE,EXIST)
       ENDIF
       IF(.NOT.EXIST)THEN
            INFILE=HOME(1:NCHOME)//'/Garfield/Files/.garfinit'
            NCFILE=MIN(LEN(INFILE),NCHOME+25)
            CALL DSNINQ(INFILE,NCFILE,EXIST)
       ENDIF
       IF(EXIST.AND.LPROF)THEN
            LUN=20
            CALL DSNOPN(INFILE,NCFILE,LUN,'READ-FILE',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPINT WARNING : Opening '//
     -                INFILE(1:NCFILE)//' failed; initialisation'//
     -                'not performed.'
                 LUN=5
            ELSE
                 CALL STRBUF('STORE',LUNSTR(20,1),INFILE,NCFILE,IFAIL)
                 GLBVAL(6)=LUNSTR(LUN,1)
                 EOFSTR='EOF'
                 NCEOF=3
                 CALL STRBUF('STORE',LUNSTR(20,2),EOFSTR(1:NCEOF),NCEOF,
     -                IFAIL)
                 ARGSTR=' '
                 NCARG=1
                 CALL STRBUF('STORE',LUNSTR(20,3),ARGSTR(1:NCARG),NCARG,
     -                IFAIL)
                 CALL DSNLOG(INFILE(1:NCFILE),'Profile   ',
     -                'Sequential','Read only ')
            ENDIF
       ENDIF
*** Determine the character set being used by the computer:
       ICHSET=1
*** Translation table initialisation.
       CALL INPTRI
*** Default shell.
       CALL GETENV('SHELL',HOME)
       IF(HOME.EQ.' ')CALL GETENV('shell',HOME)
       DO 30 I=LEN(HOME),1,-1
       IF(HOME(I:I).NE.' ')THEN
            SHELL=HOME(1:I)
            NCSH=I
            GOTO 40
       ENDIF
30     CONTINUE
       SHELL='tcsh'
       NCSH=4
40     CONTINUE
*** Escape character (double because \ is a Unix escape).
       ESCAPE='\\'
*** Initialise the prompt.
       PROMPT='Main'
       LPROM=.TRUE.
       NCPROM=4
*** Start reading normal input and allow substitution.
       DOEXEC=.FALSE.
       DOREAD=.FALSE.
*** Input recording.
       IF(LINREC)THEN
            CALL DSNOPN('garflast.dat',12,18,'WRITE-FILE',IFAIL)
            CALL DSNLOG('garflast.dat','Recording ',
     -           'Sequential','Write     ')
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! INPINT WARNING : Opening the'//
     -                ' recording file failed; recording cancelled.'
                 LINREC=.FALSE.
            ELSEIF(LDEBUG)THEN
                 PRINT *,' ++++++ INPINT DEBUG   :'//
     -                ' Recording has been enabled.'
            ENDIF
       ELSE
            IF(LDEBUG)PRINT *,' ++++++ INPINT DEBUG   : Recording'//
     -           ' has been disabled.'
       ENDIF
*** I/O error processing.
       RETURN
2020   CONTINUE
       PRINT *,' ###### INPINT ERROR   : Error opening a file; quit.'
       CALL QUIT
       RETURN
2030   CONTINUE
       PRINT *,' ###### INPINT ERROR   : Error closing a file; quit.'
       CALL QUIT
       END
