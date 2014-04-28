CDECK  ID>, HISRAD.
       SUBROUTINE HISRAD(Y,N,XLO,XWID,XRAN)
*-----------------------------------------------------------------------
*   HISRAD - Subroutine to generate random numbers according to an
*            empirical distribution supplied by the user in the form of
*            a histogram.
*   Author:  F. James, modified for DOUBLE PRECISION usage.
*   (Last changed on 26/10/07.)
*-----------------------------------------------------------------------
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       PARAMETER(NVEC=100)
       DOUBLE PRECISION Y(*),RVEC(NVEC),XLO,XWID,XRAN,YR
       INTEGER L,IVEC,LOCATD
       EXTERNAL LOCATD
       SAVE RVEC,IVEC
*** Initial value.
       DATA IVEC/0/
*** Make sure that the histogram has been prepared.
       IF(Y(N).NE.1)THEN
            PRINT *,' !!!!!! HISRAD WARNING : HISPRD has apparently'//
     -           ' not been called; calling it now.'
            CALL HISPRD(Y,N)
       ENDIF
*** Now generate random number between 0 and one.
       IF(IVEC.EQ.0.OR.IVEC.GE.NVEC)THEN
            CALL RM48(RVEC,NVEC)
            IVEC=1
       ELSE
            IVEC=IVEC+1
       ENDIF
       YR = RVEC(IVEC)
*   Verify random number.
       IF(YR.LE.0.OR.YR.GT.1)PRINT *,' !!!!!! HISRAD WARNING :'//
     -      ' Received ',YR,' from RM48 - please ensure you have'//
     -      ' an up to date version of CERNLIB.'
*   and transform it into the corresponding x-value
       L = LOCATD(Y,N,YR)
*   point falls in first bin.  special case
       IF(L.EQ.0)THEN
            IF(Y(1).LE.0)THEN
                 XRAN = XLO + XWID / 2
            ELSE
                 XRAN = XLO + XWID * (YR/Y(1))
            ENDIF
*   guard against special case of falling on empty bin
       ELSEIF(L.GT.0)THEN
            XRAN = XLO + L * XWID
*   usually come here.
       ELSE
            L = ABS(L)
            IF(Y(L+1)-Y(L).LE.0)THEN
                 XRAN = XLO + XWID * (L + 0.5)
            ELSE
                 XRAN = XLO + XWID * (L +((YR-Y(L))/(Y(L+1)-Y(L))))
            ENDIF
       ENDIF
       END
