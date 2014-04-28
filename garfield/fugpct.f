CDECK  ID>, FUGPCT.
       SUBROUTINE FUGPCT(FUNC,XLOW,XHIGH,XFCUM,NLO,NBINS,TFTOT,IERR)
*-----------------------------------------------------------------------
*   FUGPCT - Array XFCUM is filled from NLO to NLO+NBINS, which makes
*            the number of values NBINS+1, or the number of bins NBINS
*-----------------------------------------------------------------------
       implicit none
       EXTERNAL FUNC
       REAL XFCUM(*),XLOW,XHIGH,TFTOT,RTEPS,PRECIS,TPCTIL,TZ,TZMAX,X,F,
     -      X1,X2,F1,TINCR,XINCR,FUNC,DXMAX,TCUM,XBEST,DTBEST,DTABS,
     -      TPART,TPART2,DTPAR2,REFX,UNCERT,ABERR,FMIN,FMINZ
       INTEGER NLO,NBINS,NZ,MAXZ,IZ,IHOME,NITMAX,IBIN,IERR
       PARAMETER (RTEPS=0.005, NZ=10, MAXZ=20, NITMAX=6,PRECIS=1.E-6)
*** Set error flag to 'success'.
       IERR = 0
*** Check for integral.
       IF (TFTOT .LE. 0.) GOTO 900
*** Coarse estimate of percentiles.
       TPCTIL = TFTOT/NBINS
       TZ = TPCTIL/NZ
       TZMAX = TZ * 2.
       XFCUM(NLO) = XLOW
       XFCUM(NLO+NBINS) = XHIGH
       X = XLOW
       F = FUNC(X)
       IF (F .LT. 0.) GOTO 900
*** Loop over percentile bins
       DO 600 IBIN = NLO, NLO+NBINS-2
       TCUM = 0.
       X1 = X
       F1 = F
       DXMAX = (XHIGH -X) / NZ
       FMIN = TZ/DXMAX
       FMINZ = FMIN
*** Loop over trapezoids within a supposed percentile
       DO 500 IZ= 1, MAXZ
       XINCR = TZ/MAX(F1,FMIN,FMINZ)
  350  X = X1 + XINCR
       F = FUNC(X)
       IF (F .LT. 0.) GOTO 900
       TINCR = (X-X1) * 0.5 * (F+F1)
       IF (TINCR .LT. TZMAX) GOTO 370
       XINCR = XINCR * 0.5
       GOTO 350
  370  CONTINUE
       TCUM = TCUM + TINCR
       IF (TCUM .GE. TPCTIL*0.99) GOTO 520
       FMINZ = TZ*F/ (TPCTIL-TCUM)
       F1 = F
       X1 = X
  500  CONTINUE
       PRINT *,' !!!!!! FUGPCT WARNING : Insufficient trapezoid'//
     -      ' accuracy over a percentile; inaccurate results.'
       IERR=1
       RETURN
*** Adjust, Gaussian integration with Newton corr, F is the derivative.
  520  CONTINUE
       X1 = XFCUM(IBIN)
       XBEST = X
       DTBEST = TPCTIL
       TPART = TPCTIL
*** Allow for maximum NITMAX more iterations on RADAPT
       DO 550 IHOME= 1, NITMAX
  535  XINCR = (TPCTIL-TPART) / MAX(F,FMIN)
       X = XBEST + XINCR
       X2 = X
       IF (IHOME .GT. 1 .AND. X2 .EQ. XBEST) THEN
            PRINT *,' !!!!!! FUGPCT WARNING : Insufficient Gauss'//
     -           ' precision at X=',X,'; inaccurate results.'
            GOTO 580
       ENDIF
       REFX = ABS(X)+PRECIS
       CALL RADAPT(FUNC,X1,X2,1,RTEPS,0.,TPART2,UNCERT)
       DTPAR2 = TPART2-TPCTIL
       DTABS = ABS(DTPAR2)
       IF(ABS(XINCR)/REFX .LT. PRECIS) GOTO 545
       IF(DTABS .LT. DTBEST) GOTO 545
       XINCR = XINCR * 0.5
       GOTO 535
  545  DTBEST = DTABS
       XBEST = X
       TPART = TPART2
       F = FUNC(X)
       IF(F .LT. 0.) GOTO 900
       IF(DTABS .LT. RTEPS*TPCTIL) GOTO 580
  550  CONTINUE
       PRINT *,' !!!!!! FUGPCT WARNING : No convergence in bin ',IBIN,
     -      ' ; inaccurate results.'
       IERR=1
       RETURN
*** < none >
  580  CONTINUE
       XINCR = (TPCTIL-TPART) / MAX(F,FMIN)
       X = XBEST + XINCR
       XFCUM(IBIN+1) = X
       F = FUNC(X)
       IF(F .LT. 0.) GOTO 900
  600  CONTINUE
*** End of loop over bins
       X1 = XFCUM(NLO+NBINS-1)
       X2 = XHIGH
       CALL RADAPT(FUNC,X1,X2,1,RTEPS,0.,TPART ,UNCERT)
       ABERR = ABS(TPART-TPCTIL)/TFTOT
       IF(ABERR .GT. RTEPS)PRINT *,' !!!!!! FUGPCT WARNING :'//
     -      ' Relative error in cumulative distribution may be as big'//
     -      ' as ',ABERR
*** Normal return.
       RETURN
*** Error processing.
  900  CONTINUE
       PRINT *,' ###### FUGPCT WARNING : Function negative at x=',X,
     -      ' f=',F
       IERR = 1
       END
