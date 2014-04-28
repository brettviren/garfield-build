CDECK  ID>, FUGLZ.
       SUBROUTINE FUGLZ(FUNC,X2LOW,X2HIGH,XLOW,XHIGH)
*-----------------------------------------------------------------------
*   FUGLZ  - Find range where func is non-zero.
*   Origin: V152, Fred James (1980,
*-----------------------------------------------------------------------
       implicit none
       REAL FUNC,X2LOW,X2HIGH,XLOW,XHIGH,XMID,XH,XL,XNEW
       INTEGER LOGN,NSLICE,K,I
       EXTERNAL FUNC
*** Set initial limits.
       XLOW = X2LOW
       XHIGH = X2HIGH
*** Find out if function is zero at one end or both.
       XMID = XLOW
       IF (FUNC(XLOW) .GT. 0.) GOTO 120
       XMID = XHIGH
       IF (FUNC(XHIGH) .GT. 0.)  GOTO 50
*** Function is zero at both ends, look for place where it is non-zero.
       DO 30 LOGN= 1, 7
       NSLICE = 2**LOGN
       DO 20 I= 1, NSLICE, 2
       XMID = XLOW + I * (XHIGH-XLOW) / NSLICE
       IF (FUNC(XMID) .GT. 0.)  GOTO 50
   20  CONTINUE
   30  CONTINUE
*** Falling through loop means cannot find non-zero value
       PRINT *,' !!!!!! FUGLZ  WARNING : Cannot find positive'//
     -      ' function values in the range ',XLOW,XHIGH
       XLOW = 0.
       XHIGH = 0.
       GOTO 220
   50  CONTINUE
*** Delete 'leading' zero range.
       XH = XMID
       XL = XLOW
       DO 70 K= 1, 20
       XNEW = 0.5*(XH+XL)
       IF (FUNC(XNEW) .EQ. 0.) GOTO 68
       XH = XNEW
       GOTO 70
   68  XL = XNEW
   70  CONTINUE
       XLOW = XL
       PRINT *,' !!!!!! FUGLZ  WARNING : Cannot find positive'//
     -      ' function values in the range ',X2LOW,XLOW
  120  CONTINUE
       IF (FUNC(XHIGH) .GT. 0.) GOTO 220
*** Delete 'trailing' range of zeroes.
       XL = XMID
       XH = XHIGH
       DO 170 K= 1, 20
       XNEW = 0.5*(XH+XL)
       IF (FUNC(XNEW) .EQ. 0.) GOTO 168
       XL = XNEW
       GOTO 170
  168  XH = XNEW
  170  CONTINUE
       XHIGH = XH
       PRINT *,' !!!!!! FUGLZ  WARNING : Cannot find positive'//
     -      ' function values in the range ',XHIGH,X2HIGH
  220  CONTINUE
       END
