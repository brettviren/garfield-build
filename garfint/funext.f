CDECK  ID>, FUNEXT.
       SUBROUTINE FUNEXT(FUN,NC,IGLB,XMIN,XMAX,OPTION,EEPSX,EEPSF,
     -      NITMAX,IFAIL)
*-----------------------------------------------------------------------
*   FUNEXT - Searches for extrema of a function.
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
       CHARACTER*(*) FUN,OPTION
       CHARACTER*20 AUX1,AUX2
       INTEGER NC,IENTRY,MODSAV,NITMAX,IGLB,IFAIL,IFAIL1,I,NRNDM,
     -      MODRES(1),NRES,NREXP,NC1,NC2
       REAL XMIN,XMAX,VALSAV,RES(1),RNDUNI,XPL(MXLIST),YPL(MXLIST),
     -      EEPSX,EEPSF
       DOUBLE PRECISION X1,X2,X3,F1,F2,F3,XPARA,FPARA,EPSX,EPSF,FTRY,
     -      XTRY,FMIN,FMAX
       LOGICAL SET1,SET2,SET3,USE(MXVAR),LPRINT,LPLOT,SMIN,SMAX,SKIP
       EXTERNAL RNDUNI
*** Identification.
       IF(LIDENT)PRINT *,' /// ROUTINE FUNEXT ///'
*** Assume this will work.
       IFAIL=0
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
*** Accuracy settings.
       EPSX=DBLE(EEPSX)
       EPSF=DBLE(EEPSF)
       NRNDM=100
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ FUNEXT DEBUG   : '',
     -      ''Function to be searched:         '',A/26X,
     -      ''Range to be searched:            '',2E15.8/26X,
     -      ''Minimum / Maximum:               '',2L15/26X,
     -      ''Location / function convergence: '',2F15.8/26X,
     -      ''Random cycles / max iterations:  '',2I15)')
     -      FUN(1:NC),XMIN,XMAX,SMIN,SMAX,EPSX,EPSF,NRNDM,NITMAX
*** Check the parameters.
       IF(EPSX.LE.0.OR.EPSF.LE.0.OR.NITMAX.LT.1)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Received incorrect'//
     -           ' convergence criteria; no search.'
            RETURN
       ENDIF
*** Print output.
       IF(LPRINT)THEN
            IF(SMIN)THEN
                 WRITE(LUNOUT,'(''  Searching for the minimum of '',A)')
     -                FUN(1:NC)
            ELSEIF(SMAX)THEN
                 WRITE(LUNOUT,'(''  Searching for the maximum of '',A)')
     -                FUN(1:NC)
            ENDIF
            CALL OUTFMT(XMIN,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(XMAX,2,AUX2,NC2,'LEFT')
            WRITE(LUNOUT,'(''  Search range: '',A,'' < '',A,'' < '',A)')
     -           AUX1(1:NC1),GLBVAR(IGLB),AUX2(1:NC2)
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
*** Check the global variable index.
       IF(IGLB.LE.0.OR.IGLB.GT.NGLB)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Global variable'//
     -           ' reference is out of range; no extrema search.'
            IFAIL=1
            RETURN
       ENDIF
*** Save current value in case minimisation fails.
       MODSAV=GLBMOD(IGLB)
       VALSAV=GLBVAL(IGLB)
*** Prepare the function.
       CALL ALGPRE(FUN(1:NC),NC,GLBVAR,NGLB,NRES,USE,IENTRY,IFAIL1)
*   Verify that the translation worked.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : The function ',FUN(1:NC),
     -           ' can not be translated; no extrema search.'
            IFAIL=1
            CALL ALGCLR(IENTRY)
            RETURN
*   Ensure there is only 1 result.
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : The function ',FUN(1:NC),
     -           ' does not return 1 result; no extrema search.'
            IFAIL=1
            CALL ALGCLR(IENTRY)
            RETURN
*   Ensure that the function depends on the parameter.
       ELSEIF(.NOT.USE(IGLB))THEN
            PRINT *,' !!!!!! FUNEXT WARNING : The function ',FUN(1:NC),
     -           ' does not depend on global ',GLBVAR(IGLB),
     -           '; no extrema search.'
            IFAIL=1
            CALL ALGCLR(IENTRY)
            RETURN
       ENDIF
*** Start a plot, if requested.
       IF(LPLOT)THEN
            DO 30 I=1,MXLIST
            XPL(I)=XMIN+REAL(I-1)*(XMAX-XMIN)/REAL(MXLIST-1)
            NREXP=1
            GLBVAL(IGLB)=XPL(I)
            GLBMOD(IGLB)=2
            CALL AL2EXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,NREXP,
     -           IFAIL1)
            IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
                 PRINT *,' !!!!!! FUNEXT WARNING : Error evaluating'//
     -                ' the function ; no extremum search.'
                 IFAIL=1
                 GOTO 3000
            ENDIF
            YPL(I)=RES(1)
30          CONTINUE
            CALL GRGRPH(XPL,YPL,MXLIST,GLBVAR(IGLB),FUN(1:NC),
     -           'Function extrema search')
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
       XTRY=XMIN+RNDUNI(1.0)*(XMAX-XMIN)
       NREXP=1
       GLBVAL(IGLB)=REAL(XTRY)
       GLBMOD(IGLB)=2
       CALL AL2EXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,NREXP,IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Error evaluating the'//
     -           ' function ; no extremum search.'
            IFAIL=1
            GOTO 3000
       ENDIF
       FTRY=RES(1)
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
       NREXP=1
       GLBVAL(IGLB)=XMIN
       GLBMOD(IGLB)=2
       CALL AL2EXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,NREXP,
     -      IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Error evaluating'//
     -           ' the function ; no extremum search.'
            IFAIL=1
            GOTO 3000
       ENDIF
       IF((SMIN.AND.RES(1).LT.F1).OR.(SMAX.AND.RES(1).GT.F1))THEN
            X1=XMIN
            F1=RES(1)
            SKIP=.TRUE.
            IF(LPRINT)WRITE(LUNOUT,'(''  Function value at lower'',
     -           '' range limit is better: f='',E15.8)') RES(1)
       ENDIF
       NREXP=1
       GLBVAL(IGLB)=XMAX
       GLBMOD(IGLB)=2
       CALL AL2EXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,NREXP,
     -      IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Error evaluating'//
     -           ' the function ; no extremum search.'
            IFAIL=1
            GOTO 3000
       ENDIF
       IF((SMIN.AND.RES(1).LT.F1).OR.(SMAX.AND.RES(1).GT.F1))THEN
            X1=XMAX
            F1=RES(1)
            SKIP=.TRUE.
            IF(LPRINT)WRITE(LUNOUT,'(''  Function value at upper'',
     -           '' range limit is better: f='',E15.8)') RES(1)
       ENDIF
       IF(SKIP)THEN
            GLBVAL(IGLB)=X1
            GLBMOD(IGLB)=2
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
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ FUNEXT DEBUG   :'',
     -      '' Start of iteration '',I3//
     -      26X,''Point 1:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 2:  x='',E15.8,'' f='',E15.8/
     -      26X,''Point 3:  x='',E15.8,'' f='',E15.8//
     -      26X,''Parabola: x='',E15.8,'' f='',E15.8)')
     -      I,X1,F1,X2,F2,X3,F3,XPARA,FPARA
*   Check that the parabolic estimate is within range.
       IF((XMIN-XPARA)*(XPARA-XMAX).LT.0)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Estimated parabolic'//
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
            GOTO 3000
       ENDIF
*   Evaluate things over there.
       NREXP=1
       GLBVAL(IGLB)=REAL(XPARA)
       GLBMOD(IGLB)=2
       CALL AL2EXE(IENTRY,GLBVAL,GLBMOD,NGLB,RES,MODRES,NREXP,IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
            PRINT *,' !!!!!! FUNEXT WARNING : Error evaluating the'//
     -           ' function ; no extremum search.'
            IFAIL=1
            GOTO 3000
       ENDIF
       FPARA=RES(1)
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
            PRINT *,' !!!!!! FUNEXT WARNING : Parabolic extremum'//
     -           ' is outside current search range; search stopped.'
            IFAIL=1
            GOTO 3000
       ENDIF
20     CONTINUE
*** No convergence.
       PRINT *,' !!!!!! FUNEXT WARNING : No convergence after maximum'//
     -      ' number of steps.'
       PRINT *,'                         Current extremum f=',F1
       PRINT *,'                         Found for        x=',X1
*** Clean up.
3000   CONTINUE
*   Display number of algebra errors.
       CALL ALGERR
*   Kill algebra entry points.
       CALL ALGCLR(IENTRY)
*   Close graphics, if active.
       IF(LPLOT)CALL GRNEXT
*   Restore original results in case of failure.
       IF(IFAIL.NE.0)THEN
            GLBVAL(IGLB)=VALSAV
            GLBMOD(IGLB)=MODSAV
       ENDIF
       END
