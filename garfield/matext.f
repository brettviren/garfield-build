CDECK  ID>, MATEXT.
       SUBROUTINE MATEXT(IRX,IRF,XEXT,OPTION,EEPSX,EEPSF,NITMAX,IFAIL)
*-----------------------------------------------------------------------
*   MATEXT - Searches for extrema of a matrix interpolation.
*   VARIABLES :
*   (Last changed on  5/11/01.)
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
       CHARACTER*(*) OPTION
       CHARACTER*20 AUX1,AUX2
       CHARACTER*10 NAMEX,NAMEF
       INTEGER NITMAX,IFAIL,I,NRNDM,
     -      NC1,NC2,MATSLT,IRX,IRF,ISX,ISF,IORDER
       REAL XMIN,XMAX,RNDUNI,XPL(MXLIST),YPL(MXLIST),
     -      EEPSX,EEPSF,XEXT,DIVDIF
       DOUBLE PRECISION X1,X2,X3,F1,F2,F3,XPARA,FPARA,EPSX,EPSF,FTRY,
     -      XTRY,FMIN,FMAX
       LOGICAL SET1,SET2,SET3,LPRINT,LPLOT,SMIN,SMAX,SKIP
       EXTERNAL RNDUNI,MATSLT,DIVDIF
*** Identification.
       IF(LIDENT)PRINT *,' /// ROUTINE MATEXT ///'
*** Assume this will not work.
       IFAIL=1
*** Find the matrices.
       ISX=MATSLT(IRX)
       ISF=MATSLT(IRF)
*   Ensure they both exist.
       IF(ISX.EQ.0.OR.ISF.EQ.0)THEN
            PRINT *,' !!!!!! MATEXT WARNING : Ordinate or function'//
     -           ' matrix not found; no extremum search.'
            RETURN
*   The matrices must have the same size > 1.
       ELSEIF(MLEN(ISX).NE.MLEN(ISF))THEN
            PRINT *,' !!!!!! MATEXT WARNING : Ordinate and function'//
     -           ' matrices have different length; no extremum search.'
            RETURN
       ELSEIF(MLEN(ISX).LE.1)THEN
            PRINT *,' !!!!!! MATEXT WARNING : Ordinate and function'//
     -           ' matrices have length<2; no extremum search.'
            RETURN
*   The matrices must be 1-dimensional.
       ELSEIF(MDIM(ISX).NE.1)THEN
            PRINT *,' !!!!!! MATEXT WARNING : Ordinate or function'//
     -           ' matrix not 1-dimensional; no extremum search.'
            RETURN
       ENDIF
*** Verify that the ordinate matrix is well ordered.
       IF(MVEC(MORG(ISX)+2).GT.MVEC(MORG(ISX)+1))THEN
            DO 40 I=2,MLEN(ISX)
            IF(MVEC(MORG(ISX)+I).LE.MVEC(MORG(ISX)+I-1))THEN
                 PRINT *,' !!!!!! MATEXT WARNING : The ordinate'//
     -                ' vector is not strictly ordered; no extremum'//
     -                ' search.'
                 RETURN
            ENDIF
40          CONTINUE
       ELSEIF(MVEC(MORG(ISX)+2).LT.MVEC(MORG(ISX)+1))THEN
            DO 50 I=2,MLEN(ISX)
            IF(MVEC(MORG(ISX)+I).GE.MVEC(MORG(ISX)+I-1))THEN
                 PRINT *,' !!!!!! MATEXT WARNING : The ordinate'//
     -                ' vector is not strictly ordered; no extremum'//
     -                ' search.'
                 RETURN
            ENDIF
50          CONTINUE
       ELSE
            PRINT *,' !!!!!! MATEXT WARNING : The ordinate vector'//
     -           ' is not strictly ordered; no extremum search.'
            RETURN
       ENDIF
*** Decode options.
       LPLOT=.FALSE.
       IF(INDEX(OPTION,'NOPLOT').NE.0)THEN
            LPLOT=.FALSE.
       ELSEIF(INDEX(OPTION,'PLOT').NE.0)THEN
            LPLOT=.TRUE.
       ENDIF
       LPRINT=.FALSE.
       IF(INDEX(OPTION,'NOPRINT').NE.0)THEN
            LPRINT=.FALSE.
       ELSEIF(INDEX(OPTION,'PRINT').NE.0)THEN
            LPRINT=.TRUE.
       ENDIF
       SMIN=.TRUE.
       SMAX=.FALSE.
       IF(INDEX(OPTION,'MIN').NE.0)THEN
            SMIN=.TRUE.
            SMAX=.FALSE.
       ELSEIF(INDEX(OPTION,'MAX').NE.0)THEN
            SMIN=.FALSE.
            SMAX=.TRUE.
       ENDIF
       IORDER=MIN(2,MLEN(ISX))
       IF(INDEX(OPTION,'LINEAR').NE.0)THEN
            IORDER=1
       ELSEIF(INDEX(OPTION,'QUAD').NE.0)THEN
            IF(MLEN(ISX).LT.3)THEN
                 PRINT *,' !!!!!! MATEXT WARNING : Vectors are too'//
     -                ' short for quadratic interpolation; no'//
     -                ' extremum search.'
                 RETURN
            ELSE
                 IORDER=2
            ENDIF
       ELSEIF(INDEX(OPTION,'CUBIC').NE.0)THEN
            IF(MLEN(ISX).LT.4)THEN
                 PRINT *,' !!!!!! MATEXT WARNING : Vectors are too'//
     -                ' short for cubic interpolation; no extremum'//
     -                ' search.'
                 RETURN
            ELSE
                 IORDER=3
            ENDIF
       ENDIF
*** Set the range.
       XMIN=MVEC(MORG(ISX)+1)
       XMAX=MVEC(MORG(ISX)+MLEN(ISX))
*** Accuracy settings.
       EPSX=DBLE(EEPSX)
       EPSF=DBLE(EEPSF)
       NRNDM=100
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATEXT DEBUG   : '',
     -      ''Ordinate and function vectors:   '',I5,1X,I5/26X,
     -      ''Interpolation order:             '',I5/26X,
     -      ''Range to be searched:            '',2E15.8/26X,
     -      ''Minimum / Maximum:               '',2L15/26X,
     -      ''Location / function convergence: '',2F15.8/26X,
     -      ''Random cycles / max iterations:  '',2I15)')
     -      ISX,ISF,IORDER,XMIN,XMAX,SMIN,SMAX,EPSX,EPSF,NRNDM,NITMAX
*** Check the parameters.
       IF(EPSX.LE.0.OR.EPSF.LE.0.OR.NITMAX.LT.1)THEN
            PRINT *,' !!!!!! MATEXT WARNING : Received incorrect'//
     -           ' convergence criteria; no search.'
            RETURN
       ENDIF
*** Get hold of the names.
       NAMEX='temporary'
       NAMEF='temporary'
       DO 60 I=1,NGLB
       IF(NINT(GLBVAL(I)).EQ.IRX.AND.GLBMOD(I).EQ.5)THEN
            NAMEX=GLBVAR(I)
       ELSEIF(NINT(GLBVAL(I)).EQ.IRF.AND.GLBMOD(I).EQ.5)THEN
            NAMEF=GLBVAR(I)
       ENDIF
60     CONTINUE
*** Print output.
       IF(LPRINT)THEN
            IF(SMIN)THEN
                 WRITE(LUNOUT,'(''  Searching for the minimum of '',A,
     -                '' vs '',A)') NAMEF,NAMEX
            ELSEIF(SMAX)THEN
                 WRITE(LUNOUT,'(''  Searching for the maximum of '',A,
     -                '' vs '',A)') NAMEF,NAMEX
            ENDIF
            CALL OUTFMT(XMIN,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(XMAX,2,AUX2,NC2,'LEFT')
            WRITE(LUNOUT,'(''  Search range: '',A,'' to '',A)')
     -           AUX1(1:NC1),AUX2(1:NC2)
            CALL OUTFMT(REAL(EPSX),2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  Convergence declared for relative'',
     -           '' position changes less than '',A)') AUX1(1:NC1)
            CALL OUTFMT(REAL(EPSF),2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  and for relative function value'',
     -           '' variations less than '',A,''.'')') AUX1(1:NC1)
            CALL OUTFMT(REAL(NRNDM),2,AUX1,NC1,'LEFT')
            CALL OUTFMT(REAL(NITMAX),2,AUX2,NC2,'LEFT')
            WRITE(LUNOUT,'(''  Doing '',A,'' random cycles and at'',
     -           '' most '',A,'' parabolic searches.''/)') AUX1(1:NC1),
     -            AUX2(1:NC2)
       ENDIF
*** Start a plot, if requested.
       IF(LPLOT)THEN
            DO 30 I=1,MXLIST
            XPL(I)=XMIN+REAL(I-1)*(XMAX-XMIN)/REAL(MXLIST-1)
            YPL(I)=DIVDIF(MVEC(MORG(ISF)+1),MVEC(MORG(ISX)+1),
     -           MLEN(ISX),XPL(I),IORDER)
30          CONTINUE
            CALL GRGRPH(XPL,YPL,MXLIST,NAMEX,NAMEF,
     -           'Matrix interpolation extrema search')
       ENDIF
*** Random search for the 3 extreme points.
       SET1=.FALSE.
       SET2=.FALSE.
       SET3=.FALSE.
       X1=0
       X2=0
       X3=0
       F1=0
       F2=0
       F3=0
       DO 10 I=1,NRNDM
*   Evaluate function.
       XTRY=DBLE(XMIN+RNDUNI(1.0)*(XMAX-XMIN))
       FTRY=DBLE(DIVDIF(MVEC(MORG(ISF)+1),MVEC(MORG(ISX)+1),
     -      MLEN(ISX),REAL(XTRY),IORDER))
*   Keep track of the 3 smallest numbers.
       IF((SMIN.AND.FTRY.LT.F1).OR.(SMAX.AND.FTRY.GT.F1).OR.
     -      .NOT.SET1)THEN
            F3=F2
            X3=X2
            IF(SET2)SET3=.TRUE.
            F2=F1
            X2=X1
            IF(SET1)SET2=.TRUE.
            F1=FTRY
            X1=XTRY
            SET1=.TRUE.
       ELSEIF((SMIN.AND.FTRY.LT.F2).OR.(SMAX.AND.FTRY.GT.F2).OR.
     -      .NOT.SET2)THEN
            F3=F2
            X3=X2
            IF(SET2)SET3=.TRUE.
            F2=FTRY
            X2=XTRY
            SET2=.TRUE.
       ELSEIF((SMIN.AND.FTRY.LT.F3).OR.(SMAX.AND.FTRY.GT.F3).OR.
     -      .NOT.SET3)THEN
            F3=FTRY
            X3=XTRY
            SET3=.TRUE.
       ENDIF
*   Keep track of function range.
       IF(LPLOT)THEN
            IF(I.EQ.1)THEN
                 FMIN=FTRY
                 FMAX=FTRY
            ELSE
                 FMIN=MIN(FTRY,FMIN)
                 FMAX=MAX(FTRY,FMAX)
            ENDIF
       ENDIF
*   Next random cycle.
10     CONTINUE
*   Print result of random search.
       IF(LPRINT)WRITE(LUNOUT,'(''  Random search finds an extreme'',
     -      '' value at x='',E15.8,'' f='',E15.8)') X1,F1
*** Compare with the boundary values.
       SKIP=.FALSE.
       FTRY=DIVDIF(MVEC(MORG(ISF)+1),MVEC(MORG(ISX)+1),
     -      MLEN(ISX),XMIN,IORDER)
       IF((SMIN.AND.FTRY.LT.F1).OR.(SMAX.AND.FTRY.GT.F1))THEN
            X1=XMIN
            F1=FTRY
            SKIP=.TRUE.
            IF(LPRINT)WRITE(LUNOUT,'(''  Function value at lower'',
     -           '' range limit is better: f='',E15.8)') FTRY
       ENDIF
       FTRY=DIVDIF(MVEC(MORG(ISF)+1),MVEC(MORG(ISX)+1),
     -      MLEN(ISX),XMAX,IORDER)
       IF((SMIN.AND.FTRY.LT.F1).OR.(SMAX.AND.FTRY.GT.F1))THEN
            X1=XMAX
            F1=FTRY
            SKIP=.TRUE.
            IF(LPRINT)WRITE(LUNOUT,'(''  Function value at upper'',
     -           '' range limit is better: f='',E15.8)') FTRY
       ENDIF
       IF(SKIP)THEN
            XEXT=X1
            GOTO 3000
       ENDIF
*** Refine the estimate by parabolic extremum search.
       DO 20 I=1,NITMAX
*   Estimate parabolic extremum.
       XPARA=(  (F1-F2)*X3**2+(F3-F1)*X2**2+(F2-F3)*X1**2)/
     -      (2*((F1-F2)*X3   +(F3-F1)*X2   +(F2-F3)*X1))
       FPARA=-(4*((F1*X2**2-F2*X1**2)*X3-(F1*X2-F2*X1)*X3**2-
     -      X2**2*F3*X1+X2*F3*X1**2)*((F1-F2)*X3-F1*X2+
     -      X2*F3+F2*X1-F3*X1)+((F1-F2)*X3**2-F1*X2**2+X2**2*F3+
     -      F2*X1**2-F3*X1**2)**2)/(4*((F1-F2)*X3-F1*X2+
     -      X2*F3+F2*X1-F3*X1)*(X3-X2)*(X3-X1)*(X2-X1))
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATEXT DEBUG   :'',
     -      '' Start of iteration '',I3//
     -      26X,''Point 1:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 2:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 3:  x='',E15.8,'' f='',E15.8//
     -      26X,''Parabola: x='',E15.8,'' f='',E15.8)')
     -      I,X1,F1,X2,F2,X3,F3,XPARA,FPARA
*   Check that the parabolic estimate is within range.
       IF((XMIN-XPARA)*(XPARA-XMAX).LT.0)THEN
            PRINT *,' !!!!!! MATEXT WARNING : Estimated parabolic'//
     -           ' extremum is located outside curve range.'
            IFAIL=1
            GOTO 3000
       ENDIF
*   Check that the new estimate doesn't coincide with an old point.
       IF(ABS(XPARA-X1).LT.EPSX*(EPSX+ABS(XPARA)).OR.
     -      ABS(XPARA-X2).LT.EPSX*(EPSX+ABS(XPARA)).OR.
     -      ABS(XPARA-X3).LT.EPSX*(EPSX+ABS(XPARA)))THEN
            IF(LPRINT)WRITE(LUNOUT,'(/''  Location convergence'',
     -           '' criterion satisfied.''/)')
            IF(LPLOT)THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 IF(SMIN)THEN
                      CALL GRARRO(REAL(XPARA),
     -                     REAL(FPARA+0.1*(FMAX-FMIN)),
     -                     REAL(XPARA),REAL(FPARA))
                 ELSEIF(SMAX)THEN
                      CALL GRARRO(REAL(XPARA),
     -                     REAL(FPARA-0.1*(FMAX-FMIN)),
     -                     REAL(XPARA),REAL(FPARA))
                 ENDIF
            ENDIF
            GOTO 3000
       ENDIF
*   Evaluate things over there.
       XEXT=REAL(XPARA)
       FPARA=DBLE(DIVDIF(MVEC(MORG(ISF)+1),MVEC(MORG(ISX)+1),
     -      MLEN(ISX),REAL(XPARA),IORDER))
*   Normal printout.
       IF(LPRINT)WRITE(LUNOUT,'(''  Iteration '',I3,'' x='',E15.8,
     -      '': f = '',E15.8,''.'')') I,XPARA,FPARA
       IF(LPLOT)THEN
            CALL GRATTS('FUNCTION-2','POLYLINE')
            IF(SMIN)THEN
                 CALL GRARRO(REAL(XPARA),REAL(FPARA+0.1*(FMAX-FMIN)),
     -                REAL(XPARA),REAL(FPARA))
            ELSEIF(SMAX)THEN
                 CALL GRARRO(REAL(XPARA),REAL(FPARA-0.1*(FMAX-FMIN)),
     -                REAL(XPARA),REAL(FPARA))
            ENDIF
       ENDIF
*   Check convergence.
       IF(ABS(FPARA-F1).LT.EPSF*(ABS(FPARA)+ABS(F1)+EPSF))THEN
            IF(LPRINT)WRITE(LUNOUT,'(/''  Function value convergence'',
     -           '' criterion satisfied.''/)')
            GOTO 3000
       ENDIF
*   Store the value in the table.
       IF((SMIN.AND.FPARA.LT.F1).OR.(SMAX.AND.FPARA.GT.F1))THEN
            F3=F2
            X3=X2
            F2=F1
            X2=X1
            F1=FPARA
            X1=XPARA
       ELSEIF((SMIN.AND.FPARA.LT.F2).OR.(SMAX.AND.FPARA.GT.F2))THEN
            F3=F2
            X3=X2
            F2=FPARA
            X2=XPARA
       ELSEIF((SMIN.AND.FPARA.LT.F3).OR.(SMAX.AND.FPARA.GT.F3))THEN
            F3=FPARA
            X3=XPARA
       ELSE
            PRINT *,' !!!!!! MATEXT WARNING : Parabolic extremum'//
     -           ' is outside current search range; search stopped.'
            IFAIL=1
            GOTO 3000
       ENDIF
20     CONTINUE
*** No convergence.
       PRINT *,' !!!!!! MATEXT WARNING : No convergence after maximum'//
     -      ' number of steps.'
       PRINT *,'                         Current extremum f=',F1
       PRINT *,'                         Found for        x=',X1
*** Clean up.
3000   CONTINUE
*   Close graphics, if active.
       IF(LPLOT)CALL GRNEXT
*   Seems to have worked.
       IFAIL=0
       END
