CDECK  ID>, ALGCAL.
       SUBROUTINE ALGCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   ALGCAL - Handles external CALL statements in instruction lists.
*   (Last changed on  1/11/11.)
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
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       CHARACTER*(MXINCH) STRING
       CHARACTER*80 TITLE,FILE
       CHARACTER*29 REMARK
       CHARACTER*8 MEMBER,TYPE,DATE,TIME
       REAL PAR(MXFPAR),EPAR(MXFPAR),K3,DT
       LOGICAL EXIST
       INTEGER INSTR,IFAIL,IFAIL1,IFAIL2,IFAIL3,I,IAUX,NARG,IPROC,NC,
     -      NC1,NCFILE,NCTYPE,NCREM,NCMEMB,MATSLT,ISY,IREY,ISEY,
     -      ISIZ(1),IOS,NPAR,IA(MXVAR),IE(MXVAR)
       EXTERNAL MATSLT
*** Assume the CALL will fail.
       IFAIL=1
*** Ensure the statement is a legitimate CALL.
       IF(INS(INSTR,2).NE.9.OR.
     -      INS(INSTR,3).LT.0.OR.INS(INSTR,3).GT.MXARG)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ ALGCAL DEBUG   : '',
     -           '' Syntax of CALL statement '',I3,'' not valid'')')
     -           INSTR
            RETURN
       ENDIF
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Execute the statements, first PRINT.
       IF(IPROC.EQ.-1)THEN
            WRITE(LUNOUT,'(/''  PRINT: ''/)')
            DO 10 I=1,NARG
            CALL OUTFMT(ARG(I),MODARG(I),STRING,NC,'LEFT')
            WRITE(LUNOUT,'(''  Arg '',I3,'': '',A)') I,STRING(1:NC)
10          CONTINUE
            IF(NARG.EQ.0)WRITE(LUNOUT,'(''  No arguments.'')')
*** Time delay
       ELSEIF(IPROC.EQ.-2)THEN
*   Check arguments.
            IF(NARG.LE.0)THEN
                 CALL TIMED(DT)
                 PRINT *,' Time delay since previous call: ',DT,' sec.'
            ELSEIF(NARG.EQ.1.AND.ARGREF(1,1).LT.2)THEN
                 CALL TIMED(ARG(1))
                 MODARG(1)=2
            ELSE
                 PRINT *,' !!!!!! ALGCAL WARNING : Can not'//
     -                ' return the time delay.'
                 RETURN
            ENDIF
*** Cell procedures.
       ELSEIF(IPROC.LE.-11.AND.IPROC.GT.-20)THEN
            CALL CELCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a cell procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Gas procedures.
       ELSEIF(IPROC.LE.-201.AND.IPROC.GT.-300)THEN
            CALL GASCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a gas procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Electric field procedures.
       ELSEIF(IPROC.LE.-301.AND.IPROC.GE.-400)THEN
            CALL EFCCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a field procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Time and progress logging.
       ELSEIF(IPROC.EQ.-401)THEN
            IF(NARG.NE.1.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect'//
     -                ' argument for TIME_LOGGING.'
            ELSE
                 CALL STRBUF('READ',NINT(ARG(1)),STRING,NC1,IFAIL1)
                 CALL TIMLOG(STRING(1:NC1))
            ENDIF
*** Sleep
       ELSEIF(IPROC.EQ.-404)THEN
            IF(NARG.NE.1.OR.MODARG(1).NE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect'//
     -                ' argument for SLEEP.'
            ELSE
                 CALL SLEEP(NINT(ARG(1)))
            ENDIF
*** Drift line procedures.
       ELSEIF(IPROC.LE.-501.AND.IPROC.GE.-600)THEN
            CALL DLCCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a transport procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Histogram procedures.
       ELSEIF(IPROC.LE.-601.AND.IPROC.GT.-700)THEN
            CALL HISCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a histogram procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Utility procedures.
       ELSEIF(IPROC.LE.-701.AND.IPROC.GT.-800)THEN
            CALL ROUCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a  procedure call.'
                 RETURN
            ENDIF
*** Plotting calls.
       ELSEIF(IPROC.LE.-801.AND.IPROC.GE.-900)THEN
            CALL GRACAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a graphics procedure call.'
                 RETURN
            ENDIF
*** String calls.
       ELSEIF(IPROC.LE.-901.AND.IPROC.GE.-1000)THEN
            CALL STRCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a string procedure call.'
                 RETURN
            ENDIF
*** Determine type of a variable.
       ELSEIF(IPROC.EQ.-50)THEN
*   Check arguments.
            IF(NARG.NE.2.OR.ARGREF(2,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect set'//
     -                ' of arguments for INQUIRE_TYPE.'
                 RETURN
            ENDIF
*   Set string depending on the argument type.
            IF(MODARG(1).EQ.1)THEN
                 STRING='String'
                 NC=6
            ELSEIF(MODARG(1).EQ.2)THEN
                 STRING='Number'
                 NC=6
            ELSEIF(MODARG(1).EQ.3)THEN
                 STRING='Logical'
                 NC=7
            ELSEIF(MODARG(1).EQ.4)THEN
                 STRING='Histogram'
                 NC=9
            ELSEIF(MODARG(1).EQ.5)THEN
                 STRING='Matrix'
                 NC=6
            ELSEIF(MODARG(1).EQ.0)THEN
                 STRING='Undefined'
                 NC=9
            ELSE
                 STRING='# Invalid'
                 NC=9
            ENDIF
*   Store the string.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            CALL STRBUF('STORE',IAUX,STRING(1:NC),NC,IFAIL1)
            ARG(2)=REAL(IAUX)
            MODARG(2)=1
*   Error processing.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! ALGCAL WARNING : Unable'//
     -           ' to store the variable type.'
*** Determine whether a file exists.
       ELSEIF(IPROC.EQ.-51)THEN
*   Check arguments.
            IF(NARG.NE.2.OR.ARGREF(2,1).GE.2.OR.MODARG(1).NE.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect set'//
     -                ' of arguments for INQUIRE_FILE.'
                 RETURN
            ENDIF
*   Fetch the file name.
            CALL STRBUF('READ',NINT(ARG(1)),FILE,NCFILE,IFAIL1)
*   Determine whether the file exists.
            IF(IFAIL1.EQ.0)THEN
                 CALL DSNINQ(FILE,NCFILE,EXIST)
            ELSE
                 PRINT *,' !!!!!! ALGCAL WARNING : Unable'//
     -                ' to fetch the file name.'
                 EXIST=.FALSE.
            ENDIF
*   Clear the storage space previously occupied by Arg 2.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
*   Set the result.
            IF(EXIST)THEN
                 ARG(2)=1
            ELSE
                 ARG(2)=0
            ENDIF
            MODARG(2)=3
*** Determine whether a member exists.
       ELSEIF(IPROC.EQ.-52)THEN
*   Check arguments.
            IF(NARG.LT.4.OR.NARG.GT.7.OR.
     -           MODARG(1).NE.1.OR.MODARG(2).NE.1.OR.MODARG(3).NE.1.OR.
     -           ARGREF(4,1).GE.2.OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect set'//
     -                ' of arguments for INQUIRE_MEMBER.'
                 RETURN
            ENDIF
*   Fetch the file, member and type.
            CALL STRBUF('READ',NINT(ARG(1)),FILE,NCFILE,IFAIL1)
            CALL STRBUF('READ',NINT(ARG(2)),MEMBER,NCMEMB,IFAIL2)
            CALL STRBUF('READ',NINT(ARG(3)),TYPE,NCTYPE,IFAIL3)
            CALL CLTOU(TYPE)
*   Preset the remark, date and time.
            REMARK='< none >'
            NCREM=8
            DATE='Unknown'
            TIME='Unknown'
*   Determine whether the file exists.
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                 CALL DSNINQ(FILE,NCFILE,EXIST)
            ELSE
                 PRINT *,' !!!!!! ALGCAL WARNING : Unable to fetch'//
     -                ' file, member or type; declared not to exist.'
                 EXIST=.FALSE.
            ENDIF
*   Open the file and see whether the member exists.
            IF(EXIST)THEN
                 CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' open the file; declared not to exist.'
                      EXIST=.FALSE.
                 ELSE
                      CALL DSNLOC(MEMBER,NCMEMB,TYPE,12,EXIST,'RESPECT')
                      IF(EXIST)THEN
                           READ(12,'(10X,A8,4X,A8,1X,A8,11X,A29,1X)',
     -                          END=2000,ERR=2010,IOSTAT=IOS)
     -                          DATE,TIME,MEMBER,REMARK
                           DO 20 I=LEN(REMARK),1,-1
                           IF(REMARK(I:I).NE.' ')THEN
                                NCREM=I
                                GOTO 30
                           ENDIF
20                         CONTINUE
                           NCREM=1
30                         CONTINUE
                           DO 40 I=LEN(MEMBER),1,-1
                           IF(MEMBER(I:I).NE.' ')THEN
                                NCMEMB=I
                                GOTO 50
                           ENDIF
40                         CONTINUE
                           NCMEMB=1
50                         CONTINUE
                      ENDIF
                 ENDIF
                 CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            ENDIF
*   Clear the storage space.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            IF(EXIST)THEN
                 IF(ARGREF(2,1).LE.1)
     -                CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
                 IF(NARG.GE.5)
     -                CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
                 IF(NARG.GE.6)
     -                CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
                 IF(NARG.GE.7)
     -                CALL ALGREU(NINT(ARG(7)),MODARG(7),ARGREF(7,1))
            ENDIF
*   Set the result, first the updated member name.
            IF(EXIST.AND.ARGREF(2,1).LE.1)THEN
                 CALL STRBUF('STORE',IAUX,MEMBER(1:NCMEMB),NCMEMB,
     -                IFAIL1)
                 ARG(2)=REAL(IAUX)
                 MODARG(2)=1
            ENDIF
*   The existence flag.
            IF(EXIST)THEN
                 ARG(4)=1
            ELSE
                 ARG(4)=0
            ENDIF
            MODARG(4)=3
*   The remark.
            IF(EXIST.AND.NARG.GE.5)THEN
                 CALL STRBUF('STORE',IAUX,REMARK(1:NCREM),NCREM,IFAIL1)
                 ARG(5)=REAL(IAUX)
                 MODARG(5)=1
            ENDIF
*   Date and time.
            IF(EXIST.AND.NARG.GE.6)THEN
                 CALL STRBUF('STORE',IAUX,DATE,8,IFAIL1)
                 ARG(6)=REAL(IAUX)
                 MODARG(6)=1
            ENDIF
            IF(EXIST.AND.NARG.GE.7)THEN
                 CALL STRBUF('STORE',IAUX,TIME,8,IFAIL1)
                 ARG(7)=REAL(IAUX)
                 MODARG(7)=1
            ENDIF
*** List objects.
       ELSEIF(IPROC.EQ.-53)THEN
            IF(NARG.NE.0)PRINT *,' !!!!!! ALGCAL WARNING : The'//
     -           ' LIST_OBJECTS procedure has no arguments; ignored.'
            CALL BOOK('LIST',' ',' ',IFAIL)
*** Fit a Gaussian to a histogram.
       ELSEIF(IPROC.EQ.-60.AND.MODARG(1).EQ.4)THEN
*   Check number and type of arguments.
            IF(ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.MODARG(8).NE.1).OR.
     -           NARG.LT.4.OR.NARG.GT.8)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_GAUSSIAN.'
                 RETURN
            ENDIF
*   Fetch the option string.
            IF(NARG.GE.8)THEN
                 CALL STRBUF('READ',NINT(ARG(8)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            IF(NARG.GE.5)CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            IF(NARG.GE.6)CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
            IF(NARG.GE.7)CALL ALGREU(NINT(ARG(7)),MODARG(7),ARGREF(7,1))
*   Perform the fit.
            CALL HISFNR(NINT(ARG(1)),TITLE(1:NC),
     -           ARG(2),ARG(3),ARG(4),ARG(5),ARG(6),ARG(7),IFAIL1)
            IF(IFAIL1.EQ.0)THEN
                 MODARG(2)=2
                 MODARG(3)=2
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
            ELSE
                 MODARG(2)=0
                 MODARG(3)=0
                 MODARG(4)=0
                 MODARG(5)=0
                 MODARG(6)=0
                 MODARG(7)=0
            ENDIF
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Fit a Gaussian to a set of matrices.
       ELSEIF(IPROC.EQ.-60.AND.MODARG(1).EQ.5)THEN
*   Check number and type of arguments.
            IF(NARG.LT.6.OR.NARG.GT.10.OR.
     -           MODARG(2).NE.5.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           ARGREF(4,1).GE.2.OR.
     -           ARGREF(5,1).GE.2.OR.ARGREF(6,1).GE.2.OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.GE.10.AND.MODARG(10).NE.1))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_GAUSSIAN.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(NARG.GE.10)THEN
                 CALL STRBUF('READ',NINT(ARG(10)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
            IF(NARG.GE.7)CALL ALGREU(NINT(ARG(7)),MODARG(7),ARGREF(7,1))
            IF(NARG.GE.8)CALL ALGREU(NINT(ARG(8)),MODARG(8),ARGREF(8,1))
            IF(NARG.GE.9)CALL ALGREU(NINT(ARG(9)),MODARG(9),ARGREF(9,1))
*   Expand the error, if required, taking dimensions from the Y vector.
            IF(MODARG(3).EQ.2)THEN
                 ISY=MATSLT(NINT(ARG(2)))
                 IF(ISY.GE.0)THEN
                      ISIZ(1)=MLEN(ISY)
                 ELSE
                      ISIZ(1)=1
                 ENDIF
                 CALL MATADM('ALLOCATE',IREY,1,ISIZ,MODARG(3),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' allocate an error array; no fit.'
                      RETURN
                 ENDIF
                 ISEY=MATSLT(IREY)
                 IF(ISEY.LE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' locate an error array; no fit.'
                      RETURN
                 ENDIF
                 DO 67 I=1,ISIZ(1)
                 MVEC(MORG(ISEY)+I)=ARG(3)
67               CONTINUE
            ELSE
                 IREY=NINT(ARG(3))
            ENDIF
*   Perform the fit.
            CALL MATFNR(NINT(ARG(1)),NINT(ARG(2)),IREY,
     -           TITLE(1:NC),ARG(4),ARG(5),ARG(6),ARG(7),ARG(8),ARG(9),
     -           IFAIL1)
            IF(IFAIL1.EQ.0)THEN
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=2
                 MODARG(9)=2
            ELSE
                 MODARG(4)=0
                 MODARG(5)=0
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
                 MODARG(9)=0
            ENDIF
*   Delete the error array after use.
            IF(MODARG(3).EQ.2)
     -           CALL MATADM('DELETE',IREY,1,ISIZ,MODARG(3),IFAIL2)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Attempt to fit a Gaussian to something else.
       ELSEIF(IPROC.EQ.-60)THEN
            PRINT *,' !!!!!! ALGCAL WARNING : Incorrect data type for'//
     -           ' a Gaussian fit ; no fit.'
            RETURN
*** Fit a polynomial to a histogram.
       ELSEIF(IPROC.EQ.-61.AND.MODARG(1).EQ.4)THEN
*   Check number and type of arguments.
            IF(NARG.LT.3.OR.
     -           (MODARG(NARG).EQ.1.AND.NARG.NE.2*(NARG/2)).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.EQ.2*(NARG/2)))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_POLYNOMIAL.'
                 RETURN
            ENDIF
*   Establish number of parameters.
            IF(MODARG(NARG).EQ.1)THEN
                 NPAR=NARG/2-1
            ELSE
                 NPAR=(NARG-1)/2
            ENDIF
            IF(NPAR.GT.MXFPAR.OR.NPAR.LT.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Number of fit'//
     -                ' parameters out of range; no fit.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            DO 60 I=2,1+2*NPAR
            IF(ARGREF(I,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : An output argument'//
     -                ' of FIT_POLYNOMIAL can not be modified; no fit.'
                 RETURN
            ENDIF
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
60          CONTINUE
*   Perform the fit.
            CALL HISFPL(NINT(ARG(1)),TITLE(1:NC),PAR,EPAR,NPAR,IFAIL1)
*   Return the results.
            DO 70 I=1,NPAR
            IF(IFAIL1.EQ.0)THEN
                 ARG(1+I)=PAR(I)
                 MODARG(1+I)=2
                 ARG(NPAR+1+I)=EPAR(I)
                 MODARG(NPAR+1+I)=2
            ELSE
                 ARG(1+I)=0
                 MODARG(1+I)=0
                 ARG(NPAR+1+I)=0
                 MODARG(NPAR+1+I)=0
            ENDIF
70          CONTINUE
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Fit a polynomial to a set of matrices.
       ELSEIF(IPROC.EQ.-61.AND.MODARG(1).EQ.5)THEN
*   Check number and type of arguments.
            IF(NARG.LT.5.OR.
     -           MODARG(2).NE.5.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           (MODARG(NARG).EQ.1.AND.NARG.NE.2*(NARG/2)).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.EQ.2*(NARG/2)))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_POLYNOMIAL.'
                 RETURN
            ENDIF
*   Establish number of parameters.
            IF(MODARG(NARG).EQ.1)THEN
                 NPAR=NARG/2-2
            ELSE
                 NPAR=(NARG-1)/2-1
            ENDIF
            IF(NPAR.GT.MXFPAR.OR.NPAR.LT.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Number of fit'//
     -                ' parameters out of range; no fit.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            DO 65 I=4,3+2*NPAR
            IF(ARGREF(I,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : An output argument'//
     -                ' of FIT_POLYNOMIAL can not be modified; no fit.'
                 RETURN
            ENDIF
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
65          CONTINUE
*   Expand the error, if required, taking dimensions from the Y vector.
            IF(MODARG(3).EQ.2)THEN
                 ISY=MATSLT(NINT(ARG(2)))
                 IF(ISY.GE.0)THEN
                      ISIZ(1)=MLEN(ISY)
                 ELSE
                      ISIZ(1)=1
                 ENDIF
                 CALL MATADM('ALLOCATE',IREY,1,ISIZ,MODARG(3),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' allocate an error array; no fit.'
                      RETURN
                 ENDIF
                 ISEY=MATSLT(IREY)
                 IF(ISEY.LE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' locate an error array; no fit.'
                      RETURN
                 ENDIF
                 DO 66 I=1,ISIZ(1)
                 MVEC(MORG(ISEY)+I)=ARG(3)
66               CONTINUE
            ELSE
                 IREY=NINT(ARG(3))
            ENDIF
*   Perform the fit.
            CALL MATFPL(NINT(ARG(1)),NINT(ARG(2)),IREY,
     -           TITLE(1:NC),PAR,EPAR,NPAR,IFAIL1)
*   Return the results.
            DO 75 I=1,NPAR
            IF(IFAIL1.EQ.0)THEN
                 ARG(3+I)=PAR(I)
                 MODARG(3+I)=2
                 ARG(NPAR+3+I)=EPAR(I)
                 MODARG(NPAR+3+I)=2
            ELSE
                 ARG(3+I)=0
                 MODARG(3+I)=0
                 ARG(NPAR+3+I)=0
                 MODARG(NPAR+3+I)=0
            ENDIF
75          CONTINUE
*   Delete the error array after use.
            IF(MODARG(3).EQ.2)
     -           CALL MATADM('DELETE',IREY,1,ISIZ,MODARG(3),IFAIL2)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Attempt to fit a polynomial to something else.
       ELSEIF(IPROC.EQ.-61)THEN
            PRINT *,' !!!!!! ALGCAL WARNING : Incorrect data type for'//
     -           ' a polynomial fit ; no fit.'
            RETURN
*** Fit an exponential of a polynomial to a histogram.
       ELSEIF(IPROC.EQ.-62.AND.MODARG(1).EQ.4)THEN
*   Check number and type of arguments.
            IF(NARG.LT.3.OR.
     -           (MODARG(NARG).EQ.1.AND.NARG.NE.2*(NARG/2)).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.EQ.2*(NARG/2)))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_EXPONENTIAL.'
                 RETURN
            ENDIF
*   Establish number of parameters.
            IF(MODARG(NARG).EQ.1)THEN
                 NPAR=NARG/2-1
            ELSE
                 NPAR=(NARG-1)/2
            ENDIF
            IF(NPAR.GT.MXFPAR.OR.NPAR.LT.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Number of fit'//
     -                ' parameters out of range; no fit.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            DO 260 I=2,1+2*NPAR
            IF(ARGREF(I,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : An output argument'//
     -                ' of FIT_EXPONENTIAL can not be modified; no fit.'
                 RETURN
            ENDIF
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
260         CONTINUE
*   Perform the fit.
            CALL HISFEX(NINT(ARG(1)),TITLE(1:NC),PAR,EPAR,NPAR,IFAIL1)
*   Return the results.
            DO 270 I=1,NPAR
            IF(IFAIL1.EQ.0)THEN
                 ARG(1+I)=PAR(I)
                 MODARG(1+I)=2
                 ARG(NPAR+1+I)=EPAR(I)
                 MODARG(NPAR+1+I)=2
            ELSE
                 ARG(1+I)=0
                 MODARG(1+I)=0
                 ARG(NPAR+1+I)=0
                 MODARG(NPAR+1+I)=0
            ENDIF
270         CONTINUE
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Fit an exponential of a polynomial to a set of matrices.
       ELSEIF(IPROC.EQ.-62.AND.MODARG(1).EQ.5)THEN
*   Check number and type of arguments.
            IF(NARG.LT.5.OR.
     -           MODARG(2).NE.5.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           (MODARG(NARG).EQ.1.AND.NARG.NE.2*(NARG/2)).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.EQ.2*(NARG/2)))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_EXPONENTIAL.'
                 RETURN
            ENDIF
*   Establish number of parameters.
            IF(MODARG(NARG).EQ.1)THEN
                 NPAR=NARG/2-2
            ELSE
                 NPAR=(NARG-1)/2-1
            ENDIF
            IF(NPAR.GT.MXFPAR.OR.NPAR.LT.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Number of fit'//
     -                ' parameters out of range; no fit.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            DO 265 I=4,3+2*NPAR
            IF(ARGREF(I,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : An output argument'//
     -                ' of FIT_EXPONENTIAL can not be modified; no fit.'
                 RETURN
            ENDIF
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
265         CONTINUE
*   Expand the error, if required, taking dimensions from the Y vector.
            IF(MODARG(3).EQ.2)THEN
                 ISY=MATSLT(NINT(ARG(2)))
                 IF(ISY.GE.0)THEN
                      ISIZ(1)=MLEN(ISY)
                 ELSE
                      ISIZ(1)=1
                 ENDIF
                 CALL MATADM('ALLOCATE',IREY,1,ISIZ,MODARG(3),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' allocate an error array; no fit.'
                      RETURN
                 ENDIF
                 ISEY=MATSLT(IREY)
                 IF(ISEY.LE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' locate an error array; no fit.'
                      RETURN
                 ENDIF
                 DO 266 I=1,ISIZ(1)
                 MVEC(MORG(ISEY)+I)=ARG(3)
266              CONTINUE
            ELSE
                 IREY=NINT(ARG(3))
            ENDIF
*   Perform the fit.
            CALL MATFEX(NINT(ARG(1)),NINT(ARG(2)),IREY,
     -           TITLE(1:NC),PAR,EPAR,NPAR,IFAIL1)
*   Return the results.
            DO 275 I=1,NPAR
            IF(IFAIL1.EQ.0)THEN
                 ARG(3+I)=PAR(I)
                 MODARG(3+I)=2
                 ARG(NPAR+3+I)=EPAR(I)
                 MODARG(NPAR+3+I)=2
            ELSE
                 ARG(3+I)=0
                 MODARG(3+I)=0
                 ARG(NPAR+3+I)=0
                 MODARG(NPAR+3+I)=0
            ENDIF
275         CONTINUE
*   Delete the error array after use.
            IF(MODARG(3).EQ.2)
     -           CALL MATADM('DELETE',IREY,1,ISIZ,MODARG(3),IFAIL2)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Attempt to fit an exponential of a polynomial to something else.
       ELSEIF(IPROC.EQ.-62)THEN
            PRINT *,' !!!!!! ALGCAL WARNING : Incorrect data type for'//
     -           ' an exponential polynomial fit ; no fit.'
            RETURN
*** Fit a Polya distribution to a histogram.
       ELSEIF(IPROC.EQ.-63.AND.MODARG(1).EQ.4)THEN
*   Check number and type of arguments.
            IF(NARG.LT.9.OR.NARG.GT.10.AND.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.EQ.10.AND.MODARG(NARG).NE.1))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_POLYA.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            DO 261 I=2,9
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
261         CONTINUE
*   Perform the fit.
            CALL HISFPR(NINT(ARG(1)),TITLE(1:NC),ARG(2),ARG(3),ARG(4),
     -           ARG(5),ARG(6),ARG(7),ARG(8),ARG(9),IFAIL1)
            IF(IFAIL1.EQ.0)THEN
                 MODARG(2)=2
                 MODARG(3)=2
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=2
                 MODARG(9)=2
            ELSE
                 MODARG(2)=0
                 MODARG(3)=0
                 MODARG(4)=0
                 MODARG(5)=0
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
                 MODARG(9)=0
            ENDIF
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Fit a Polya distribution to a set of matrices.
       ELSEIF(IPROC.EQ.-63.AND.MODARG(1).EQ.5)THEN
*   Check number and type of arguments.
            IF(NARG.LT.11.OR.NARG.GT.12.AND.
     -           MODARG(2).NE.5.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.GE.10.AND.ARGREF(10,1).GE.2).OR.
     -           (NARG.GE.11.AND.ARGREF(11,1).GE.2).OR.
     -           (NARG.EQ.12.AND.MODARG(NARG).NE.1))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_POLYA.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Clear previous use of storage for the results.
            DO 267 I=4,11
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
267         CONTINUE
*   Expand the error, if required, taking dimensions from the Y vector.
            IF(MODARG(3).EQ.2)THEN
                 ISY=MATSLT(NINT(ARG(2)))
                 IF(ISY.GE.0)THEN
                      ISIZ(1)=MLEN(ISY)
                 ELSE
                      ISIZ(1)=1
                 ENDIF
                 CALL MATADM('ALLOCATE',IREY,1,ISIZ,MODARG(3),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' allocate an error array; no fit.'
                      RETURN
                 ENDIF
                 ISEY=MATSLT(IREY)
                 IF(ISEY.LE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' locate an error array; no fit.'
                      RETURN
                 ENDIF
                 DO 268 I=1,ISIZ(1)
                 MVEC(MORG(ISEY)+I)=ARG(3)
268              CONTINUE
            ELSE
                 IREY=NINT(ARG(3))
            ENDIF
*   Perform the fit.
            CALL MATFPR(NINT(ARG(1)),NINT(ARG(2)),IREY,TITLE(1:NC),
     -           ARG(4),ARG(5),ARG(6),ARG(7),
     -           ARG(8),ARG(9),ARG(10),ARG(11),IFAIL1)
            IF(IFAIL1.EQ.0)THEN
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=2
                 MODARG(9)=2
                 MODARG(10)=2
                 MODARG(11)=2
            ELSE
                 MODARG(4)=0
                 MODARG(5)=0
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
                 MODARG(9)=0
                 MODARG(10)=0
                 MODARG(11)=0
            ENDIF
*   Delete the error array after use.
            IF(MODARG(3).EQ.2)
     -           CALL MATADM('DELETE',IREY,1,ISIZ,MODARG(3),IFAIL2)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Attempt to fit an exponential of a polynomial to something else.
       ELSEIF(IPROC.EQ.-63)THEN
            PRINT *,' !!!!!! ALGCAL WARNING : Incorrect data type for'//
     -           ' a Polya fit ; no fit.'
            RETURN
*** Fit a function to an histogram.
       ELSEIF(IPROC.EQ.-64.AND.MODARG(1).EQ.4)THEN
*   Check number and type of arguments.
            IF(NARG.LT.4.OR.MODARG(2).NE.1.OR.
     -           (MODARG(NARG).EQ.1.AND.NARG.EQ.2*(NARG/2)).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.NE.2*(NARG/2)))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_FUNCTION.'
                 RETURN
            ENDIF
*   Establish number of parameters.
            IF(MODARG(NARG).EQ.1)THEN
                 NPAR=(NARG-3)/2
            ELSE
                 NPAR=(NARG-2)/2
            ENDIF
            IF(NPAR.GT.MXFPAR.OR.NPAR.LT.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Number of fit'//
     -                ' parameters out of range; no fit.'
                 RETURN
            ENDIF
*   Fetch the function string.
            CALL STRBUF('READ',NINT(ARG(2)),FILE,NCFILE,IFAIL1)
            IF(NCFILE.LE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Null string not'//
     -                ' suitable as function; no fit.'
                 RETURN
            ENDIF
            CALL CLTOU(FILE(1:NCFILE))
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Determine the origin of the variables.
            DO 310 I=1,NPAR
            IF(ARGREF(2+I,1).GE.2.OR.ARGREF(2+NPAR+I,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : An output argument'//
     -                ' of FIT_FUNCTION can not be modified; no fit.'
                 RETURN
            ENDIF
            IA(I)=ARGREF(2+I,2)
            IE(I)=ARGREF(2+NPAR+I,2)
            CALL ALGREU(NINT(ARG(2+NPAR+I)),MODARG(2+NPAR+I),
     -           ARGREF(2+NPAR+I,1))
310         CONTINUE
*   Perform the fit.
            CALL HISFFU(NINT(ARG(1)),FILE(1:NCFILE),TITLE(1:NC),
     -           IA,IE,NPAR,IFAIL1)
*   And ensure that the argument vector matches the globals list.
            DO 320 I=3,2+2*NPAR
            IF(IFAIL1.EQ.0)THEN
                 ARG(I)=GLBVAL(ARGREF(I,2))
                 MODARG(I)=2
            ELSE
                 ARG(I)=0
                 MODARG(I)=0
            ENDIF
320         CONTINUE
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Fit a function to a set of matrices.
       ELSEIF(IPROC.EQ.-64.AND.MODARG(1).EQ.5)THEN
*   Check number and type of arguments.
            IF(NARG.LT.6.OR.MODARG(4).NE.1.OR.
     -           MODARG(2).NE.5.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.NE.2*(NARG/2)).OR.
     -           (MODARG(NARG).EQ.1.AND.NARG.EQ.2*(NARG/2)))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_FUNCTION.'
                 RETURN
            ENDIF
*   Establish number of parameters.
            IF(MODARG(NARG).EQ.1)THEN
                 NPAR=(NARG-5)/2
            ELSE
                 NPAR=(NARG-4)/2
            ENDIF
            IF(NPAR.GT.MXFPAR.OR.NPAR.LT.1)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Number of fit'//
     -                ' parameters out of range; no fit.'
                 RETURN
            ENDIF
*   Fetch the function string.
            CALL STRBUF('READ',NINT(ARG(4)),FILE,NCFILE,IFAIL1)
            IF(NCFILE.LE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Null string not'//
     -                ' suitable as function; no fit.'
                 RETURN
            ENDIF
            CALL CLTOU(FILE(1:NCFILE))
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Determine the origin of the variables.
            DO 330 I=1,NPAR
            IF(ARGREF(4+I,1).GE.2.OR.ARGREF(4+NPAR+I,1).GE.2)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : An output argument'//
     -                ' of FIT_FUNCTION can not be modified; no fit.'
                 RETURN
            ENDIF
            IA(I)=ARGREF(4+I,2)
            IE(I)=ARGREF(4+NPAR+I,2)
            CALL ALGREU(NINT(ARG(4+NPAR+I)),MODARG(4+NPAR+I),
     -           ARGREF(4+NPAR+I,1))
330         CONTINUE
*   Expand the error, if required, taking dimensions from the Y vector.
            IF(MODARG(3).EQ.2)THEN
                 ISY=MATSLT(NINT(ARG(2)))
                 IF(ISY.GE.0)THEN
                      ISIZ(1)=MLEN(ISY)
                 ELSE
                      ISIZ(1)=1
                 ENDIF
                 CALL MATADM('ALLOCATE',IREY,1,ISIZ,MODARG(3),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' allocate an error array; no fit.'
                      RETURN
                 ENDIF
                 ISEY=MATSLT(IREY)
                 IF(ISEY.LE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' locate an error array; no fit.'
                      RETURN
                 ENDIF
                 DO 350 I=1,ISIZ(1)
                 MVEC(MORG(ISEY)+I)=ARG(3)
350              CONTINUE
            ELSE
                 IREY=NINT(ARG(3))
            ENDIF
*   Perform the fit.
            CALL MATFFU(NINT(ARG(1)),NINT(ARG(2)),IREY,FILE(1:NCFILE),
     -           TITLE(1:NC),IA,IE,NPAR,IFAIL1)
*   And ensure that the argument vector matches the globals list.
            DO 340 I=5,4+2*NPAR
            IF(IFAIL1.EQ.0)THEN
                 ARG(I)=GLBVAL(ARGREF(I,2))
                 MODARG(I)=2
            ELSE
                 ARG(I)=0
                 MODARG(I)=0
            ENDIF
340         CONTINUE
*   Delete the error array after use.
            IF(MODARG(3).EQ.2)
     -           CALL MATADM('DELETE',IREY,1,ISIZ,MODARG(3),IFAIL2)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Attempt to fit a function to something else.
       ELSEIF(IPROC.EQ.-64)THEN
            PRINT *,' !!!!!! ALGCAL WARNING : Incorrect data type for'//
     -           ' a function fit ; no fit.'
            RETURN
*** Fit a Mathieson distribution to an histogram.
       ELSEIF(IPROC.EQ.-65.AND.MODARG(1).EQ.4)THEN
*   Check number and type of arguments.
            IF((MODARG(NARG).EQ.1.AND.NARG.NE.9).OR.
     -           (MODARG(NARG).NE.1.AND.NARG.NE.8).OR.
     -           NARG.LT.8.OR.NARG.GT.9.OR.
     -           MODARG(2).NE.2.OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_MATHIESON.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
                 IF(INDEX(TITLE(1:NC),'NOFITK3').NE.0.AND.
     -                MODARG(5).NE.2)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : The K3'//
     -                     ' parameter is fixed but not numeric ;'//
     -                     ' fit not performed.'
                      RETURN
                 ENDIF
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Initial setting of K3.
            IF(MODARG(5).EQ.2)THEN
                 K3=ARG(5)
            ELSE
                 K3=0.5
            ENDIF
*   Clear up memory associated with modifiable variables.
            DO 269 I=3,8
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
269         CONTINUE
*   Perform the fit.
            CALL HISFMS(NINT(ARG(1)),TITLE(1:NC),ARG(2),
     -           ARG(4),ARG(3),K3,ARG(7),ARG(6),ARG(8),IFAIL1)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 MODARG(3)=0
                 MODARG(4)=0
                 MODARG(5)=0
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 MODARG(3)=2
                 MODARG(4)=2
                 ARG(5)=K3
                 MODARG(5)=2
                 MODARG(6)=2
                 MODARG(7)=2
                 MODARG(8)=2
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Fit a Mathieson distribution to a set of matrices.
       ELSEIF(IPROC.EQ.-65.AND.MODARG(1).EQ.5)THEN
*   Check number and type of arguments.
            IF((MODARG(NARG).NE.1.AND.NARG.EQ.11).OR.
     -           NARG.LT.10.OR.NARG.GT.11.OR.
     -           MODARG(2).NE.5.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           MODARG(4).NE.2.OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.GE.10.AND.ARGREF(10,1).GE.2))THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Incorrect argument'//
     -                ' list provided for FIT_MATHIESON.'
                 RETURN
            ENDIF
*   Fetch the option string, if present.
            IF(MODARG(NARG).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 CALL CLTOU(TITLE(1:NC))
                 IF(INDEX(TITLE(1:NC),'NOFITK3').NE.0.AND.
     -                MODARG(7).NE.2)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : The K3'//
     -                     ' parameter is fixed but not numeric ;'//
     -                     ' fit not performed.'
                      RETURN
                 ENDIF
            ELSE
                 TITLE=' '
                 NC=1
            ENDIF
*   Initial setting of K3.
            IF(MODARG(7).EQ.2)THEN
                 K3=ARG(7)
            ELSE
                 K3=0.5
            ENDIF
*   Clear up memory associated with modifiable variables.
            DO 271 I=5,10
            IF(I.LE.NARG)
     -           CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
271         CONTINUE
*   Expand the error, if required, taking dimensions from the Y vector.
            IF(MODARG(3).EQ.2)THEN
                 ISY=MATSLT(NINT(ARG(2)))
                 IF(ISY.GE.0)THEN
                      ISIZ(1)=MLEN(ISY)
                 ELSE
                      ISIZ(1)=1
                 ENDIF
                 CALL MATADM('ALLOCATE',IREY,1,ISIZ,MODARG(3),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' allocate an error array; no fit.'
                      RETURN
                 ENDIF
                 ISEY=MATSLT(IREY)
                 IF(ISEY.LE.0)THEN
                      PRINT *,' !!!!!! ALGCAL WARNING : Unable to'//
     -                     ' locate an error array; no fit.'
                      RETURN
                 ENDIF
                 DO 272 I=1,ISIZ(1)
                 MVEC(MORG(ISEY)+I)=ARG(3)
272              CONTINUE
            ELSE
                 IREY=NINT(ARG(3))
            ENDIF
*   Perform the fit.
            CALL MATFMS(NINT(ARG(1)),NINT(ARG(2)),IREY,
     -           TITLE(1:NC),
     -           ARG(4),ARG(6),ARG(5),K3,ARG(9),ARG(8),ARG(10),IFAIL1)
*   Check the error flag.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : The fit'//
     -                ' was not successful.'
                 MODARG(5)=0
                 MODARG(6)=0
                 MODARG(7)=0
                 MODARG(8)=0
                 MODARG(9)=0
                 MODARG(10)=0
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 MODARG(5)=2
                 MODARG(6)=2
                 ARG(7)=K3
                 MODARG(7)=2
                 MODARG(8)=2
                 MODARG(9)=2
                 MODARG(10)=2
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Mathieson fit on other data types.
       ELSEIF(IPROC.EQ.-65)THEN
            PRINT *,' !!!!!! ALGCAL WARNING : Mathieson fits are'//
     -           ' available for matrices and histograms; no fit.'
            RETURN
*** Signal procedures.
       ELSEIF(IPROC.LE.-70.AND.IPROC.GT.-80)THEN
            CALL SIGCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a signal procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Matrix procedures.
       ELSEIF(IPROC.LE.-80.AND.IPROC.GT.-110)THEN
            CALL MATCAL(INSTR,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! ALGCAL WARNING : Failure executing'//
     -                ' a matrix procedure call.'
                 CALL LOGSAV(.FALSE.,'OK',IFAIL1)
                 RETURN
            ELSE
                 CALL LOGSAV(.TRUE.,'OK',IFAIL1)
            ENDIF
*** Other procedures are not known.
       ELSE
            PRINT *,' !!!!!! ALGCAL WARNING : Unknown procedure code'//
     -           ' received.'
            CALL LOGSAV(.FALSE.,'OK',IFAIL1)
            RETURN
       ENDIF
*** Things worked fine.
       IFAIL=0
       RETURN
*** I/O error handling.
2000   CONTINUE
       PRINT *,' !!!!!! ALGCAL WARNING : Unexpected EOF seen.'
       CALL INPIOS(IOS)
       RETURN
2010   CONTINUE
       PRINT *,' !!!!!! ALGCAL WARNING : I/O error encountered.'
       CALL INPIOS(IOS)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! ALGCAL WARNING : Error closing a file.'
       CALL INPIOS(IOS)
       END
