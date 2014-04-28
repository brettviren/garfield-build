CDECK  ID>, LSQFIT.
       SUBROUTINE LSQFIT(F,A,EA,N,X,Y,EY,M,
     -      KMAX,DIFF,CHI2,EPS,LFITPR,IFAIL)
*-----------------------------------------------------------------------
*   LSQFIT - Subroutine fitting the parameters A in the routine F to
*            the data points (X,Y) using a least squares method.
*            Translated from an Algol routine written by Geert Jan van
*            Oldenborgh and Rob Veenhof, based on Stoer + Bulirsch.
*   VARIABLES : F( . ,A,VAL) : Subroutine to be fitted.
*               (X,Y)        : Input data.
*               D            : Derivative matrix.
*               R            : Difference vector between Y and F(X,A).
*               S            : Correction vector for A.
*               EPSDIF       : Used for differentiating.
*               EPS          : Numerical resolution.
*               NFC          : Number of function calls.
*   (Last updated on 23/ 5/11.)
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
       DOUBLE PRECISION DA
       INTEGER NDA
       COMMON /LSQPAR/ DA(MXFPAR,MXFPAR),NDA
       DOUBLE PRECISION X(*),Y(*),EY(*),A(*),EA(*),R(MXFPNT),
     -       S(MXFPAR),D(MXFPNT,MXFPAR),EPS,EPSDIF,
     -       DIFF,DIFFC,CHI2,CHI2L,SIGMA,BETA,SUM,SCALE,VAL,
     -       AUXPR(MXFPAR)
       INTEGER IR(MXFPAR),N,M,KMAX,IFAIL,IFAIL1,NFC,I,J,K,ITER
       LOGICAL LFITPR
       EXTERNAL F
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE LSQFIT ///'
*** Initialise IFAIL and NFC.
       IFAIL=1
       NFC=0
*** Check that the dimensions are sufficient to handle the problem.
       IF(N.GT.MXFPAR.OR.M.GT.MXFPNT)THEN
            PRINT *,' ###### LSQFIT ERROR   : Array dimensions not'//
     -              ' sufficient to handle the problem ; increase'
            PRINT *,'                         MXFPAR to at least ',
     -              N,' and MXFPNT to ',M,' and recompile the program.'
            RETURN
       ENDIF
*** Make sure that the # degrees of freedom < the number of data points.
       IF(N.GT.M)THEN
            PRINT *,' !!!!!! LSQFIT WARNING : The number of parameters',
     -           ' to be varied exceeds the number of data points.'
            IFAIL=1
            RETURN
       ENDIF
*** Initialise CHI2, the difference vector R and the correction S.
       CHI2=0
       CHI2L=0
       DO 10 I=1,M
*   Check each of the errors.
       IF(EY(I).LE.0)THEN
            PRINT *,' !!!!!! LSQFIT WARNING : Error for point ',I,
     -           ' is not > 0; no fit done.'
            IFAIL=1
            RETURN
       ENDIF
*   Compute initial residuals.
       CALL F(X(I),A,VAL)
       R(I)=(Y(I)-VAL)/EY(I)
       NFC=NFC+1
*   Compute initial maximum difference.
       IF(I.EQ.1)DIFFC=ABS(R(I))
       IF(I.GT.1.AND.DIFFC.LT.ABS(R(I)))DIFFC=ABS(R(I))
*   And compute initial chi2.
       CHI2=CHI2+R(I)**2
10     CONTINUE
*   Set initial parameter error and correction vectors.
       DO 50 I=1,N
       S(I)=0
       EA(I)=0
50     CONTINUE
*** Print a table of the input if debug is on.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ LSQFIT DEBUG   : Start of debug'',
     -           '' output'',//,26X,''Number of input data points= '',
     -           I4,//,30X,''I            X(I)            Y(I)'',
     -           ''          Weight          Y-F(X)'')') M
            DO 30 I=1,M
            WRITE(LUNOUT,'(26X,I5,4(1X,E15.8))')
     -           I,X(I),Y(I),EY(I),R(I)
30          CONTINUE
            WRITE(LUNOUT,'(26X,''Number of parameters to optimise ='',
     -           I2/26X,''Initial parameter values:''/
     -           30X,''I            A(I)'')') N
            DO 40 I=1,N
            WRITE(LUNOUT,'(26X,I5,1X,E15.8)') I,A(I)
40          CONTINUE
            WRITE(LUNOUT,'(26X,''Initial CHI2 '',E12.5,
     -           '', initial DIFF '',E12.5,/,26X,''required DIFF is '',
     -           E12.5/)') CHI2,DIFFC,DIFF
       ENDIF
*** Print some summary information if LFITPR is on.
       IF(LFITPR)THEN
            WRITE(LUNOUT,'(/''  MINIMISATION SUMMARY''/)')
            WRITE(LUNOUT,'(''  Initial situation:'',/,5X,''largest '',
     -           ''difference between field and target function   : '',
     -           E15.8)') DIFFC
            WRITE(LUNOUT,'(5X,''sum of squares of these differences '',
     -           '' (chi-squared)     : '',E15.8/)') CHI2
            WRITE(LUNOUT,'(''  Stopping criteria:'',/,5X,''difference'',
     -           '' between field and target function less than : '',
     -           E15.8)') DIFF
            WRITE(LUNOUT,'(5X,''the relative chi-squared variation'',
     -           '' becomes less than   : '',E15.8)') EPS
            WRITE(LUNOUT,'(5X,''the number of iterations exceeds the'',
     -           '' maximum           : '',I3/)') KMAX
       ENDIF
*** Start optimising loop.
       DO 20 ITER=1,KMAX
**  Check the stopping criteria: (1) max norm, (2) change in CHI2.
       IF((DIFFC.LT.DIFF).OR.
     -      (ITER.GT.1.AND.ABS(CHI2L-CHI2).LT.EPS*CHI2))THEN
            IFAIL=0
            IF(LDEBUG.AND.DIFFC.LT.DIFF)THEN
                 WRITE(LUNOUT,'(26X,''Maximum difference stopping'',
     -                '' criterion satisfied.'',/)')
            ELSEIF(LDEBUG)THEN
                 WRITE(LUNOUT,'(26X,''Relative change in CHI2 has'',
     -                ''dropped below '',E10.3,''.''/)') EPS
            ENDIF
            IF(LFITPR.AND.DIFFC.LT.DIFF)THEN
                 WRITE(LUNOUT,'(/,''  The maximum difference stopping'',
     -                '' criterion is satisfied.'')')
            ELSEIF(LFITPR)THEN
                 WRITE(LUNOUT,'(/,''  The relative change in chi-'',
     -                ''squared has dropped below the threshold.'')')
            ENDIF
            GOTO 600
       ENDIF
**  Calculate the derivative matrix.
       DO 100 J=1,N
       EPSDIF=EPS*(1+ABS(A(J)))
       A(J)=A(J)+EPSDIF/2
       DO 110 I=1,M
       CALL F(X(I),A,D(I,J))
       NFC=NFC+1
110    CONTINUE
       A(J)=A(J)-EPSDIF
       DO 120 I=1,M
       CALL F(X(I),A,VAL)
       D(I,J)=(D(I,J)-VAL)/(EPSDIF*EY(I))
       NFC=NFC+1
120    CONTINUE
       A(J)=A(J)+EPSDIF/2
100    CONTINUE
**  Invert the matrix in Householder style.
       DO 200 J=1,N
       SIGMA=0.0
       DO 210 I=J,M
       SIGMA=SIGMA+D(I,J)**2
210    CONTINUE
       IF(SIGMA.EQ.0.OR.SQRT(SIGMA).LT.1E-8*ABS(D(J,J)))THEN
            PRINT *,' !!!!!! LSQFIT WARNING : Householder matrix'//
     -           ' (nearly) singular; no further optimisation.'
            PRINT *,'                         Ensure the function'//
     -           ' depends on the parameters'
            PRINT *,'                         and try to supply'//
     -           ' reasonable starting values.'
            GOTO 600
       ENDIF
       IF(D(J,J).LT.0.0)THEN
            SIGMA=SQRT(SIGMA)
       ELSE
            SIGMA=-SQRT(SIGMA)
       ENDIF
       BETA=1/(SIGMA*D(J,J)-SIGMA**2)
       D(J,J)=D(J,J)-SIGMA
       SUM=0
       DO 220 I=J,M
       SUM=SUM+D(I,J)*R(I)
220    CONTINUE
       SUM=SUM*BETA
       DO 230 I=J,M
       R(I)=R(I)+SUM*D(I,J)
230    CONTINUE
       DO 240 K=J+1,N
       SUM=0
       DO 250 I=J,M
       SUM=SUM+D(I,J)*D(I,K)
250    CONTINUE
       SUM=SUM*BETA
       DO 260 I=J,M
       D(I,K)=D(I,K)+D(I,J)*SUM
260    CONTINUE
240    CONTINUE
       D(J,J)=SIGMA
200    CONTINUE
**  Solve the system of equations.
       DO 300 I=N,1,-1
       SUM=0
       DO 310 J=N,I+1,-1
       SUM=SUM+D(I,J)*S(J)
310    CONTINUE
       S(I)=(R(I)-SUM)/D(I,I)
300    CONTINUE
**  Generate some debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(26X,''Correction vector in minimisation'',
     -           '' loop '',I3)') ITER
            DO 320 I=1,N
            WRITE(LUNOUT,'(26X,I5,1X,E15.8)') I,S(I)
320         CONTINUE
       ENDIF
**  Add part of the correction vector to the estimate to improve CHI2.
       CHI2L=CHI2
       DO 400 I=1,N
       A(I)=A(I)+S(I)*2
400    CONTINUE
       CHI2=2.0*CHI2L
       DO 410 I=0,10
       IF(CHI2.GT.CHI2L)THEN
            IF(ABS(CHI2L-CHI2).LT.EPS*CHI2)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Too little'',
     -                '' improvement, reduction loop halted.'')')
                 GOTO 440
            ENDIF
            CHI2=0.0
            DO 420 J=1,N
            A(J)=A(J)-S(J)/2**I
420         CONTINUE
            DO 430 J=1,M
            CALL F(X(J),A,VAL)
            R(J)=(Y(J)-VAL)/EY(J)
            NFC=NFC+1
            CHI2=CHI2+R(J)**2
430         CONTINUE
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Reduction loop '',I2,
     -           '' produces a CHI2 of '',E15.8)') I,CHI2
       ELSE
            GOTO 440
       ENDIF
410    CONTINUE
440    CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''shortening the correction'',
     -      '' vector by a factor of '',I4)') 2**(I-1)
*   Calculate the max norm.
       DIFFC=ABS(R(1))
       DO 450 I=2,M
       IF(DIFFC.LT.ABS(R(I)))DIFFC=ABS(R(I))
450    CONTINUE
**  Print some debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(26X,
     -           ''Values of the parameters after the step'')')
            DO 500 I=1,N
            WRITE(LUNOUT,'(26X,I5,1X,E15.8)') I,A(I)
500         CONTINUE
            WRITE(LUNOUT,'(26X,''for which CHI2='',E15.8,
     -           '' and DIFF='',E15.8/)') CHI2,DIFFC
       ENDIF
**  And some logging output.
       IF(LFITPR)WRITE(LUNOUT,'(''  Iteration '',I3,'': largest '',
     -      ''deviation = '',E15.8,'', Chi2='',E15.8)') ITER,DIFFC,CHI2
*** End of optimisation loop.
20     CONTINUE
       IF(LFITPR)THEN
            WRITE(LUNOUT,'(/''  The maximum number of iterations has'',
     -           '' been reached.'')')
       ELSE
            PRINT *,' !!!!!! LSQFIT WARNING : Maximum number of'//
     -           ' iterations reached, stopping criteria not satisfied.'
       ENDIF
*** End of fit, perform error calculation.
600    CONTINUE
*   Calculate the derivative matrix for the final settings.
       DO 800 J=1,N
       EPSDIF=EPS*(1+ABS(A(J)))
       A(J)=A(J)+EPSDIF/2
       DO 810 I=1,M
       CALL F(X(I),A,D(I,J))
       NFC=NFC+1
810    CONTINUE
       A(J)=A(J)-EPSDIF
       DO 820 I=1,M
       CALL F(X(I),A,VAL)
       D(I,J)=(D(I,J)-VAL)/(EPSDIF*EY(I))
       NFC=NFC+1
820    CONTINUE
       A(J)=A(J)+EPSDIF/2
800    CONTINUE
*   Calculate the error matrix.
       DO 830 I=1,N
       DO 840 J=1,N
       DA(I,J)=0
       DO 850 K=1,M
       DA(I,J)=DA(I,J)+D(K,I)*D(K,J)
850    CONTINUE
840    CONTINUE
830    CONTINUE
*   Compute the scaling factor for the errors.
       IF(M.GT.N)THEN
            SCALE=CHI2/DBLE(M-N)
       ELSE
            SCALE=1
       ENDIF
*   Invert it to get the covariance matrix.
       CALL DINV(N,DA,MXFPAR,IR,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! LSQINV WARNING : Singular covariance'//
     -           ' matrix ; no error calculation.'
            DO 860 I=1,N
            EA(I)=0
860         CONTINUE
       ELSE
            DO 870 I=1,N
            DO 880 J=1,N
            DA(I,J)=SCALE*DA(I,J)
880         CONTINUE
            EA(I)=SQRT(MAX(0.0D0,DA(I,I)))
870         CONTINUE
       ENDIF
*   Save array size
       NDA=N
*** Print results.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(26X,''Comparison between input and fit'',/,
     -           30X,''I            X(I)            Y(I)'',
     -           ''            F(X)'')')
            DO 610 I=1,M
            CALL F(X(I),A,VAL)
            NFC=NFC+1
            WRITE(LUNOUT,'(26X,I5,3(1X,E15.8))') I,X(I),Y(I),VAL
610         CONTINUE
            WRITE(LUNOUT,'(/26X,''Number of function calls '',I4,/
     -           ''  ++++++ LSQFIT DEBUG   : End of debug output.'')')
     -           NFC
       ENDIF
       IF(LFITPR)THEN
            WRITE(LUNOUT,'(/''  Final values of the fit parameters:''/
     -           ''  Parameter            Value            Error''/)')
            DO 640 I=1,N
            WRITE(LUNOUT,'(2X,I9,2X,E15.8,2X,E15.8)') I,A(I),EA(I)
640         CONTINUE
            WRITE(LUNOUT,'(/''  The errors have been scaled by a'',
     -           '' factor of '',E15.8,''.'')') SQRT(SCALE)
            WRITE(LUNOUT,'(/''  Covariance matrix:''/)')
            DO 620 I=1,N
            WRITE(LUNOUT,'(1X,8(1X,E15.8):(/17X,7(1X,E15.8)))')
     -           (DA(I,J),J=1,N)
620         CONTINUE
            WRITE(LUNOUT,'(/''  Correlation matrix:''/)')
            DO 630 I=1,N
            DO 650 J=1,N
            IF(DA(I,I).GT.0.AND.DA(J,J).GT.0)THEN
                 AUXPR(J)=DA(I,J)/SQRT(DA(I,I)*DA(J,J))
            ELSE
                 AUXPR(J)=0
            ENDIF
650         CONTINUE
            WRITE(LUNOUT,'(1X,8(1X,F15.8):(/17X,7(1X,F15.8)))')
     -           (AUXPR(J),J=1,N)
630         CONTINUE
            WRITE(LUNOUT,'(/''  Minimisation finished.'')')
       ENDIF
       END
