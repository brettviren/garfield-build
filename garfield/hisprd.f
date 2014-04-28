CDECK  ID>, HISPRD.
       SUBROUTINE HISPRD(Y,N)
*-----------------------------------------------------------------------
*   HISPRD - Initialize histogram to form cumulative distribution.
*   Author:  F. James, modified for double precision.
*   (Last changed on 17/10/95.)
*-----------------------------------------------------------------------
       IMPLICIT DOUBLE PRECISION (A-H,O-Z)
       DOUBLE PRECISION Y(*)
       INTEGER N
*** Form cumulative distribution.
       YTOT = 0
       DO 100 I= 1, N
       IF(Y(I).LT.0)THEN
            PRINT *,' !!!!!! HISPRD WARNING : Found a negative'//
     -           ' probability in bin ',I,'; set to 0.'
       ELSE
            YTOT = YTOT + Y(I)
            Y(I) = YTOT
       ENDIF
100    CONTINUE
       IF(YTOT.LE.0)THEN
            PRINT *,' !!!!!! HISPRD WARNING : Histogram has a zero'//
     -           ' integral ; not useable.'
            YTOT=1
       ENDIF
*** Normalise the distribution.
       YINV = 1/YTOT
       DO 110 I= 1, N
       Y(I) = Y(I) * YINV
110    CONTINUE
       Y(N) = 1.0
       END
