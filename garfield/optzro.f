CDECK  ID>, OPTZRO.
       SUBROUTINE OPTZRO(F,X,N,IFAIL)
*-----------------------------------------------------------------------
*   OPTZRO - Tries to find zeroes of a set of functions F. Uses the
*            Broyden rank-1 update variant of an n-dimensional Newton-
*            Raphson zero search in most steps, except every 5th step
*            and whenever the step length update becomes less than 0.5,
*            when a new derivative is computed.
*   (Last changed on 29/ 4/96.)
*-----------------------------------------------------------------------
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION FX(MXGRID,MXGRID),FY(MXGRID,MXGRID),
     -      XSCAN(MXGRID),YSCAN(MXGRID),EPS,EPSX,EPSF,STEP
       REAL XORIG(MXWIRE),YORIG(MXWIRE),XOFF(MXWIRE),YOFF(MXWIRE)
       INTEGER NITMAX,NSHOT,NSTEP,IW,NSCANX,NSCANY,JSORD,NFITER
       LOGICAL LFGRAV,LFELEC,LFEXTR,LFWARN,LZROPR,LFITER
       COMMON /SHPDAT/ FX,FY,XSCAN,YSCAN,EPS,EPSX,EPSF,STEP,
     -      XORIG,YORIG,XOFF,YOFF,
     -      NITMAX,NSHOT,NSTEP,IW,NSCANX,NSCANY,JSORD,NFITER,
     -      LFGRAV,LFELEC,LFEXTR,LFWARN,LZROPR,LFITER
       INTEGER N,IFAIL,IFAIL1,IWORK(MXZPAR),NBSMAX,NIT,NFC
       DOUBLE PRECISION X(*),B(MXZPAR,MXZPAR),BB(MXZPAR,MXZPAR),EPSDIF,
     -      AUX1(MXZPAR),AUX2(MXZPAR),AUX3(MXZPAR),FOLD(MXZPAR),
     -      SCALE,XNORM,DXNORM,FNORM,FNORML,DFNORM
       EXTERNAL F
       PARAMETER(NBSMAX=10)
*** Identification and debugging output.
       IF(LIDENT)PRINT *,' /// ROUTINE OPTZRO ///'
*** Assume this will fail.
       IFAIL=1
*** Extrapolation warning.
       LFWARN=.FALSE.
*** Check the value of N.
       IF(N.LT.1.OR.N.GT.MXZPAR)THEN
            PRINT *,' !!!!!! OPTZRO WARNING : Number of points not'//
     -           ' in the range [1,MXZPAR]; no zero search.'
            RETURN
       ENDIF
*** Initial deviation.
       FNORML=0
       CALL F(X,FOLD,N)
       IF(LFWARN.AND..NOT.LFEXTR)THEN
            PRINT *,' !!!!!! OPTZRO WARNING : Zero search stopped:'//
     -           ' initial position outside scanning area.'
            RETURN
       ENDIF
       DO 70 I=1,N
       FNORML=FNORML+FOLD(I)**2
70     CONTINUE
*** Debugging output for initial situation.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ OPTZRO DEBUG   : Start of'',
     -           '' zero search.''//
     -           26X,''Number of parameters:      '',I4/
     -           26X,''Maximum bisections:        '',I4/
     -           26X,''Maximum iterations:        '',I4/
     -           26X,''Epsilon differentation:    '',E12.5/
     -           26X,''Required location change:  '',E12.5/
     -           26X,''Required function norm:    '',E12.5//
     -           26X,''Initial function norm: '',E12.5//
     -           26X,''Parameter        Value     Function'')')
     -           N,NBSMAX,NITMAX,EPS,EPSX,EPSF,SQRT(FNORML)
            DO 300 I=1,N
            WRITE(LUNOUT,'(26X,I9,1X,E12.5,1X,E12.5)')
     -           I,X(I),FOLD(I)
300         CONTINUE
       ENDIF
*** Set number of iterations.
       NIT=0
*** Set number of function calls.
       NFC=0
*** Compute derivative matrix.
200    CONTINUE
       DO 10 I=1,N
       EPSDIF=EPS*(1+ABS(X(I)))
       X(I)=X(I)+EPSDIF/2
       CALL F(X,AUX1,N)
       X(I)=X(I)-EPSDIF
       CALL F(X,AUX2,N)
       X(I)=X(I)+EPSDIF/2
       IF(LFWARN.AND..NOT.LFEXTR)THEN
            PRINT *,' !!!!!! OPTZRO WARNING : Zero search stopped:'//
     -           ' differential matrix requires a point outside'//
     -           ' scanning area.'
            RETURN
       ENDIF
       DO 20 J=1,N
       B(J,I)=(AUX1(J)-AUX2(J))/EPSDIF
20     CONTINUE
10     CONTINUE
210    CONTINUE
       NFC=NFC+2*N
*** Next iteration.
       NIT=NIT+1
       IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Start of iteration '',I5)') NIT
       IF(LZROPR)THEN
            WRITE(LUNOUT,'(''  Start of iteration '',I5)') NIT
            WRITE(LUNOUT,'(''  x ='',5E12.5:(/5X,5E12.5))')
     -           (X(3+4*I),I=0,NSHOT-1)
            WRITE(LUNOUT,'(''  y ='',5E12.5:(/5X,5E12.5))')
     -           (X(4+4*I),I=0,NSHOT-1)
       ENDIF
*** Find the correction vector to 0th order, AUX1: f.
       DO 30 I=1,N
       AUX1(I)=FOLD(I)
       DO 35 J=1,N
       BB(I,J)=B(I,J)
35     CONTINUE
30     CONTINUE
       CALL DEQN(N,BB,MXZPAR,IWORK,IFAIL1,1,AUX1)
*   Check error condition, AUX1: correction vector.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! OPTZRO WARNING : Solving the update'//
     -           ' equation failed; zero search stopped.'
            GOTO 1000
       ENDIF
       IF(LZROPR)THEN
            WRITE(LUNOUT,'(''  dx='',5E12.5:(/5X,5E12.5))')
     -           (AUX1(3+4*I),I=0,NSHOT-1)
            WRITE(LUNOUT,'(''  dy='',5E12.5:(/5X,5E12.5))')
     -           (AUX1(4+4*I),I=0,NSHOT-1)
       ENDIF
*** Scale the correction vector to improve FNORM, AUX3: f.
       SCALE=1
       DO 60 ITER=1,NBSMAX
       DO 40 I=1,N
       AUX2(I)=X(I)-SCALE*AUX1(I)
40     CONTINUE
       CALL F(AUX2,AUX3,N)
       IF(LFWARN.AND..NOT.LFEXTR)THEN
            PRINT *,' !!!!!! OPTZRO WARNING : Zero search stopped:'//
     -           ' step update leads to a point outside the'//
     -           ' scanning area.'
            RETURN
       ENDIF
       NFC=NFC+1
       FNORM=0
       DO 50 I=1,N
       FNORM=FNORM+AUX3(I)**2
50     CONTINUE
       IF(FNORM.LE.FNORML)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Scaling factor: '',E12.5)')
     -           SCALE
            GOTO 80
       ENDIF
       SCALE=SCALE/2
60     CONTINUE
       PRINT *,' !!!!!! OPTZRO WARNING : Bisection search for scaling'//
     -      ' factor did not converge ; zero search stopped.'
       GOTO 1000
*** Update the estimate, AUX1: dx, AUX2: df, AUX3: f_new.
80     CONTINUE
*   Initial values of norms.
       XNORM=0
       DXNORM=0
       DFNORM=0
*   Loop over the vectors.
       DO 90 I=1,N
       AUX1(I)=AUX2(I)-X(I)
       DXNORM=DXNORM+AUX1(I)**2
       X(I)=AUX2(I)
       XNORM=XNORM+X(I)**2
       AUX2(I)=AUX3(I)-FOLD(I)
       DFNORM=DFNORM+AUX2(I)**2
       FOLD(I)=AUX3(I)
90     CONTINUE
*   Debugging output to show current status.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''After this iteration, ''/
     -      26X,''Norm and change of position: '',2E12.5/
     -      26X,''Norm and change of function: '',2E12.5)')
     -      SQRT(XNORM),SQRT(DXNORM),SQRT(FNORM),SQRT(DFNORM)
*** See whether convergence has been achieved.
       IF(SQRT(DXNORM).LT.EPSX*SQRT(XNORM))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Positional convergence'',
     -           '' criterion is satisfied.'')')
            IFAIL=0
            GOTO 1000
       ELSEIF(SQRT(FNORM).LT.EPSF)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Function value'',
     -           '' convergence criterion is satisfied.'')')
            IFAIL=0
            GOTO 1000
       ENDIF
*** Update the difference.
       FNORML=FNORM
*** If the scaling factor is small, then update (rank-1 Broyden).
       IF(SCALE.GT.0.4.AND.NIT.NE.5*(NIT/5))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Performing a Broyden'',
     -           '' rank-1 update.'')')
*   Compute the "df - B dx" term, "dx" is still in AUX1
            DO 100 I=1,N
            AUX3(I)=AUX2(I)
            DO 110 J=1,N
            AUX3(I)=AUX3(I)-B(I,J)*AUX1(J)
110         CONTINUE
100         CONTINUE
*   Update the matrix.
            DO 120 I=1,N
            DO 130 J=1,N
            B(I,J)=B(I,J)+AUX3(I)*AUX1(J)/DXNORM
130         CONTINUE
120         CONTINUE
*   And restart the iteration from the matrix solution.
            IF(NIT.LE.NITMAX)GOTO 210
*** Otherwise, recompute the differential.
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Recomputing the covariance'',
     -           '' matrix.'')')
            IF(NIT.LE.NITMAX)GOTO 200
       ENDIF
*** Ending here means that the process didn't converge.
       PRINT *,' !!!!!! OPTZRO WARNING : Zero search did not'//
     -      ' convergence in maximum number of loops.'
*** Final debugging output.
1000   CONTINUE
       IF(LDEBUG)THEN
            CALL F(X,AUX1,N)
            NFC=NFC+1
            WRITE(LUNOUT,'(26X,''Final values: ''//
     -           26X,''Parameter        Value     Function'')')
            DO 1010 I=1,N
            WRITE(LUNOUT,'(26X,I9,1X,E12.5,1X,E12.5)')
     -           I,X(I),AUX1(I)
1010        CONTINUE
            WRITE(LUNOUT,'(26X,''Total number of function calls: '',I5/
     -           26X,''End of debugging output.'')') NFC
       ENDIF
       END
