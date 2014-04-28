CDECK  ID>, ROUCAL.
       SUBROUTINE ROUCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   ROUCAL - Interface to some routines.
*   (Last changed on 27/ 6/07.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER ISIZ(MXMDIM),NARG,IPROC,INSTR,IFAIL,MATSLT,IFAIL1,IFAIL2,
     -      ISLOT1,ISLOT2,ISLOT3,ISLOT4,IREF3,IREF4,NITMAX,
     -      NDIM,IMOD,LENGTH,J,NC,IENTRY,NNRES,NCOPT
       REAL CUMRNF(200),FRNDFU,EPSX,EPSF,FVAVIL,RNDVAV,RNDVVL
       CHARACTER*(MXCHAR) STRING,OPTION
       CHARACTER*10 VARLIS(MXVAR)
       LOGICAL USE(MXVAR),FUNSET
       EXTERNAL MATSLT,FRNDFU,FVAVIL,RNDVAV,RNDVVL
       COMMON /RNDFCM/ IENTRY,FUNSET,CUMRNF
*** Assume the CALL will fail.
       IFAIL=1
       CALL LOGSAV(.FALSE.,'OK',IFAIL1)
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Cartesian, Polar, Internal to one of the others.
       IF(IPROC.LE.-701.AND.IPROC.GE.-706)THEN
*   Warn if there are arguments.
            IF(NARG.NE.4.OR.
     -           (MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5).OR.
     -           MODARG(1).NE.MODARG(2).OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : The mapping'//
     -                ' procedure got wrong arguments; no mapping.'
                 RETURN
            ENDIF
*   Clear up any storage associated with the output arguments.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
**  If the arguments are simple numbers ...
            IF(MODARG(1).EQ.2)THEN
                 IF(IPROC.EQ.-701)THEN
                      CALL CFMCTP(ARG(1),ARG(2),ARG(3),ARG(4),1)
                 ELSEIF(IPROC.EQ.-702)THEN
                      CALL CFMCTR(ARG(1),ARG(2),ARG(3),ARG(4),1)
                 ELSEIF(IPROC.EQ.-703)THEN
                      CALL CFMPTC(ARG(1),ARG(2),ARG(3),ARG(4),1)
                 ELSEIF(IPROC.EQ.-704)THEN
                      CALL CFMPTR(ARG(1),ARG(2),ARG(3),ARG(4),1,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! ROUCAL WARNING : Invalid'//
     -                          ' polar coordinates; no conversion.'
                           RETURN
                      ENDIF
                 ELSEIF(IPROC.EQ.-705)THEN
                      CALL CFMRTC(ARG(1),ARG(2),ARG(3),ARG(4),1)
                 ELSEIF(IPROC.EQ.-706)THEN
                      CALL CFMRTP(ARG(1),ARG(2),ARG(3),ARG(4),1)
                 ENDIF
*   And make sure the output is registered as a number.
                 MODARG(3)=2
                 MODARG(4)=2
**  If the arguments are matrices.
            ELSE
*   Locate the input matrices.
                 ISLOT1=MATSLT(NINT(ARG(1)))
                 ISLOT2=MATSLT(NINT(ARG(2)))
                 IF(ISLOT1.LE.0.OR.ISLOT2.LE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' locate input matrices; no conversion.'
                      RETURN
                 ELSEIF(MLEN(ISLOT1).NE.MLEN(ISLOT2).OR.
     -                MLEN(ISLOT1).LT.1)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Matrices have'//
     -                     ' different or zero size; no conversion.'
                      RETURN
                 ENDIF
*   Store the length.
                 LENGTH=MLEN(ISLOT1)
*   Create output matrices of the size of the input matrices.
                 DO 10 J=1,MDIM(ISLOT1)
                 ISIZ(J)=MSIZ(ISLOT1,J)
10               CONTINUE
                 NDIM=MDIM(ISLOT1)
                 IMOD=MMOD(ISLOT1)
                 CALL MATADM('ALLOCATE',IREF3,NDIM,ISIZ,IMOD,IFAIL1)
                 CALL MATADM('ALLOCATE',IREF4,NDIM,ISIZ,IMOD,IFAIL2)
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' allocate output matrices; no conversion.'
                      RETURN
                 ENDIF
*   Now locate all matrices again (they can have been relocated).
                 ISLOT1=MATSLT(NINT(ARG(1)))
                 ISLOT2=MATSLT(NINT(ARG(2)))
                 ISLOT3=MATSLT(IREF3)
                 ISLOT4=MATSLT(IREF4)
                 IF(ISLOT1.LE.0.OR.ISLOT2.LE.0.OR.
     -                ISLOT3.LE.0.OR.ISLOT4.LE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' locate a matrix; no conversion.'
                      RETURN
                 ENDIF
*   And carry out the conversion.
                 IF(IPROC.EQ.-701)THEN
                      CALL CFMCTP(MVEC(MORG(ISLOT1)+1),
     -                     MVEC(MORG(ISLOT2)+1),MVEC(MORG(ISLOT3)+1),
     -                     MVEC(MORG(ISLOT4)+1),LENGTH)
                 ELSEIF(IPROC.EQ.-702)THEN
                      CALL CFMCTR(MVEC(MORG(ISLOT1)+1),
     -                     MVEC(MORG(ISLOT2)+1),MVEC(MORG(ISLOT3)+1),
     -                     MVEC(MORG(ISLOT4)+1),LENGTH)
                 ELSEIF(IPROC.EQ.-703)THEN
                      CALL CFMPTC(MVEC(MORG(ISLOT1)+1),
     -                     MVEC(MORG(ISLOT2)+1),MVEC(MORG(ISLOT3)+1),
     -                     MVEC(MORG(ISLOT4)+1),LENGTH)
                 ELSEIF(IPROC.EQ.-704)THEN
                      CALL CFMPTR(MVEC(MORG(ISLOT1)+1),
     -                     MVEC(MORG(ISLOT2)+1),MVEC(MORG(ISLOT3)+1),
     -                     MVEC(MORG(ISLOT4)+1),LENGTH,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! ROUCAL WARNING : Invalid'//
     -                          ' polar coordinates; no conversion.'
                           RETURN
                      ENDIF
                 ELSEIF(IPROC.EQ.-705)THEN
                      CALL CFMRTC(MVEC(MORG(ISLOT1)+1),
     -                     MVEC(MORG(ISLOT2)+1),MVEC(MORG(ISLOT3)+1),
     -                     MVEC(MORG(ISLOT4)+1),LENGTH)
                 ELSEIF(IPROC.EQ.-706)THEN
                      CALL CFMRTP(MVEC(MORG(ISLOT1)+1),
     -                     MVEC(MORG(ISLOT2)+1),MVEC(MORG(ISLOT3)+1),
     -                     MVEC(MORG(ISLOT4)+1),LENGTH)
                 ENDIF
*   Update the output arrays.
                 ARG(3)=IREF3
                 ARG(4)=IREF4
                 MODARG(3)=5
                 MODARG(4)=5
            ENDIF
*** Random numbers according to a function: preparation.
       ELSEIF(IPROC.EQ.-710)THEN
*   Check the arguments.
            IF(NARG.NE.3.OR.MODARG(1).NE.1.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING :'//
     -                ' PREPARE_RND_FUNCTION received an incorrect'//
     -                ' argument list; not executed.'
                 FUNSET=.FALSE.
                 RETURN
            ENDIF
*   Fetch the function.
            CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Unable to retrieve'//
     -                ' the PREPARE_RND_FUNCTION function; call not'//
     -                ' executed.'
                 FUNSET=.FALSE.
                 RETURN
            ENDIF
            CALL CLTOU(STRING(1:NC))
*   Translate the function.
            VARLIS(1)='X'
            CALL ALGPRE(STRING(1:NC),NC,VARLIS,1,NNRES,USE,IENTRY,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                ' translate '//STRING(1:NC)//' ; no random'//
     -                ' numbers.'
                 FUNSET=.FALSE.
                 RETURN
            ELSEIF(NNRES.NE.1)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : '//STRING(1:NC)//
     -                ' does not return 1 result; no random numbers.'
                 CALL ALGCLR(IENTRY)
                 FUNSET=.FALSE.
                 RETURN
            ENDIF
*   Prepare the function with FUGLXF.
            CALL FUGLXP(FRNDFU,CUMRNF,ARG(2),ARG(3),IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Preparing '//
     -                STRING(1:NC)//' for random number generation'//
     -                ' failed; no random numbers.'
                 CALL ALGCLR(IENTRY)
                 FUNSET=.FALSE.
                 RETURN
            ENDIF
*   If we get this far, preparation was successful.
            FUNSET=.TRUE.
*** Extremum search.
       ELSEIF(IPROC.EQ.-711)THEN
**  Syntax for a function argument.
            IF(MODARG(1).EQ.1)THEN
*   Check argument list.
                 IF(NARG.LT.4.OR.NARG.GT.8.OR.
     -                (ARGREF(2,2).LT.1.OR.ARGREF(2,2).GT.NGLB).OR.
     -                MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -                (NARG.GE.5.AND.MODARG(5).NE.1).OR.
     -                (NARG.GE.6.AND.MODARG(6).NE.2).OR.
     -                (NARG.GE.7.AND.MODARG(7).NE.2).OR.
     -                (NARG.GE.8.AND.MODARG(8).NE.2))THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Incorrect'//
     -                     ' argument list for EXTREMUM; not called.'
                      RETURN
                 ENDIF
*   Retrieve the parameters, first the function.
                 CALL STRBUF('READ',NINT(ARG(1)),STRING,NC,IFAIL)
                 IF(IFAIL.NE.0.OR.NC.LT.1)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' retrieve the function for EXTREMUM;'//
     -                     ' not called.'
                      RETURN
                 ENDIF
                 CALL CLTOU(STRING(1:NC))
*   Convergence.
                 IF(NARG.GE.6)THEN
                      EPSX=ARG(6)
                 ELSE
                      EPSX=1.0E-4
                 ENDIF
                 IF(NARG.GE.7)THEN
                      EPSF=ARG(7)
                 ELSE
                      EPSF=1.0E-4
                 ENDIF
                 IF(NARG.GE.6)THEN
                      NITMAX=NINT(ARG(8))
                 ELSE
                      NITMAX=20
                 ENDIF
*   Options.
                 IF(NARG.GE.5)THEN
                      CALL STRBUF('READ',NINT(ARG(5)),OPTION,NCOPT,
     -                     IFAIL)
                      IF(IFAIL.NE.0)THEN
                           PRINT *,' !!!!!! ROUCAL WARNING : Unable'//
     -                          ' to retrieve the options for'//
     -                          '  EXTREMUM; not called.'
                           RETURN
                      ENDIF
                      IF(NCOPT.LT.1)THEN
                           OPTION=' '
                           NCOPT=1
                      ENDIF
                      CALL CLTOU(OPTION(1:NCOPT))
                 ELSE
                      OPTION=' '
                      NCOPT=1
                 ENDIF
*   Call the procedure.
                 CALL FUNEXT(STRING(1:NC),NC,ARGREF(2,2),ARG(3),ARG(4),
     -                OPTION(1:NCOPT),EPSX,EPSF,NITMAX,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Extremum'//
     -                     ' search failed; global not updated.'
                      RETURN
                 ENDIF
*   Return the result.
                 ARG(2)=GLBVAL(ARGREF(2,2))
                 MODARG(2)=2
**  Matrix arguments.
            ELSEIF(MODARG(1).EQ.5.AND.MODARG(2).EQ.5)THEN
*   Check argument list.
                 IF(NARG.LT.3.OR.NARG.GT.7.OR.
     -                (ARGREF(3,2).LT.1.OR.ARGREF(3,2).GT.NGLB).OR.
     -                (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -                (NARG.GE.5.AND.MODARG(5).NE.2).OR.
     -                (NARG.GE.6.AND.MODARG(6).NE.2).OR.
     -                (NARG.GE.7.AND.MODARG(7).NE.2))THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Incorrect'//
     -                     ' argument list for EXTREMUM; not called.'
                      RETURN
                 ENDIF
*   Convergence.
                 IF(NARG.GE.5)THEN
                      EPSX=ARG(5)
                 ELSE
                      EPSX=1.0E-4
                 ENDIF
                 IF(NARG.GE.6)THEN
                      EPSF=ARG(6)
                 ELSE
                      EPSF=1.0E-4
                 ENDIF
                 IF(NARG.GE.7)THEN
                      NITMAX=NINT(ARG(7))
                 ELSE
                      NITMAX=20
                 ENDIF
*   Options.
                 IF(NARG.GE.4)THEN
                      CALL STRBUF('READ',NINT(ARG(4)),OPTION,NCOPT,
     -                     IFAIL)
                      IF(IFAIL.NE.0)THEN
                           PRINT *,' !!!!!! ROUCAL WARNING : Unable'//
     -                          ' to retrieve the options for'//
     -                          '  EXTREMUM; not called.'
                           RETURN
                      ENDIF
                      IF(NCOPT.LT.1)THEN
                           OPTION=' '
                           NCOPT=1
                      ENDIF
                      CALL CLTOU(OPTION(1:NCOPT))
                 ELSE
                      OPTION=' '
                      NCOPT=1
                 ENDIF
*   Call the procedure.
                 CALL MATEXT(NINT(ARG(1)),NINT(ARG(2)),ARG(3),
     -                OPTION(1:NCOPT),EPSX,EPSF,NITMAX,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Extremum'//
     -                     ' search failed; global not updated.'
                      RETURN
                 ENDIF
                 MODARG(3)=2
            ELSE
                 PRINT *,' !!!!!! ROUCAL WARNING : Unknown argument'//
     -                ' type for EXTREMUM; not called.'
                 RETURN
            ENDIF
*** Random number initialisation.
      ELSEIF(IPROC.EQ.-712)THEN
*   Check number of arguments.
            IF(NARG.NE.1)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Incorrect number'//
     -                ' of arguments for INITIALISE_GENERATORS.'
                 RETURN
            ENDIF
*   Call the procedure.
            CALL RNDINI(NINT(ARG(1)))
*** Vavilov function
      ELSEIF(IPROC.EQ.-713)THEN
*   Check number of arguments.
            IF(NARG.NE.4.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           ARGREF(4,1).GE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Incorrect number'//
     -                ' of arguments for VAVILOV; no value returned'
                 RETURN
            ENDIF
*   Reclaim argument space.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Number as argument.
            IF(MODARG(3).EQ.2)THEN
                 ARG(4)=FVAVIL(ARG(1),ARG(2),ARG(3))
                 MODARG(4)=2
*   Matrix argument
            ELSE
*   Locate the input matrices.
                 ISLOT3=MATSLT(NINT(ARG(3)))
                 IF(ISLOT3.LE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' locate input matrix; no Vavilov value.'
                      RETURN
                 ENDIF
*   Store the length.
                 LENGTH=MLEN(ISLOT3)
*   Create output matrices of the size of the input matrices.
                 DO 20 J=1,MDIM(ISLOT3)
                 ISIZ(J)=MSIZ(ISLOT3,J)
20               CONTINUE
                 NDIM=MDIM(ISLOT3)
                 IMOD=MMOD(ISLOT3)
                 CALL MATADM('ALLOCATE',IREF4,NDIM,ISIZ,IMOD,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' allocate output matrix; no Vavilov.'
                      RETURN
                 ENDIF
*   Now locate all matrices again (they can have been relocated).
                 ISLOT3=MATSLT(NINT(ARG(3)))
                 ISLOT4=MATSLT(IREF4)
                 IF(ISLOT3.LE.0.OR.ISLOT4.LE.0)THEN
                      PRINT *,' !!!!!! ROUCAL WARNING : Unable to'//
     -                     ' locate a matrix; no Vavilov values.'
                      RETURN
                 ENDIF
*   Evaluate.
                 DO 30 J=1,LENGTH
                 MVEC(MORG(ISLOT4)+J)=
     -                FVAVIL(ARG(1),ARG(2),MVEC(MORG(ISLOT3)+J))
30               CONTINUE
                 ARG(4)=REAL(IREF4)
                 MODARG(4)=5
            ENDIF
*** Vavilov random number (fast).
      ELSEIF(IPROC.EQ.-714)THEN
*   Check number of arguments.
            IF(NARG.NE.3.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           ARGREF(3,1).GE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Incorrect number'//
     -                ' of arguments for RND_VAVILOV_FAST;'//
     -                ' no value returned'
                 RETURN
            ENDIF
*   Reclaim argument space.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Obtain a value
            ARG(3)=RNDVAV(ARG(1),ARG(2))
            MODARG(3)=2
*** Vavilov random number (precise).
      ELSEIF(IPROC.EQ.-715)THEN
*   Check number of arguments.
            IF(NARG.NE.3.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           ARGREF(3,1).GE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Incorrect number'//
     -                ' of arguments for RND_VAVILOV; no value returned'
                 RETURN
            ENDIF
*   Reclaim argument space.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Obtain a value
            ARG(3)=RNDVVL(ARG(1),ARG(2))
            MODARG(3)=2
*** Ionisation energy
      ELSEIF(IPROC.EQ.-716)THEN
*   Check number of arguments.
            IF(NARG.NE.3.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           ARGREF(3,1).GE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Incorrect number'//
     -                ' of arguments for RND_IONISATION_ENERGY;'//
     -                ' no value returned'
                 RETURN
            ENDIF
*   Reclaim argument space.
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Obtain a value
            CALL RNDHWF(ARG(1),ARG(2),ARG(3))
            MODARG(3)=2
*** Unit sphere
      ELSEIF(IPROC.EQ.-717)THEN
*   Check number of arguments.
            IF(NARG.NE.3.OR.
     -           ARGREF(1,1).GE.2.OR.ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2)THEN
                 PRINT *,' !!!!!! ROUCAL WARNING : Incorrect'//
     -                ' of arguments for RND_UNIT_SPHERE;'//
     -                ' no values returned'
                 RETURN
            ENDIF
*   Reclaim argument space.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
*   Obtain a value
            CALL RNDSPH(ARG(1),ARG(2),ARG(3))
            MODARG(1)=2
            MODARG(2)=2
            MODARG(3)=2
*** Unknown routine.
       ELSE
            PRINT *,' !!!!!! ROUCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       CALL LOGSAV(.TRUE.,'OK',IFAIL1)
       IFAIL=0
       END
