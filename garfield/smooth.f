CDECK  ID>, SMOOTH.
       SUBROUTINE SMOOTH(X,N,NW,SIGMA)
*-----------------------------------------------------------------------
*   SMOOTH - Gaussian smoothing
*   (Last changed on 10/ 6/12.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXW,MXN
       PARAMETER(MXW=100, MXN=10000)
       REAL X(N), SIGMA, W(-MXW:MXW), WSUM, XNEW(MXN)
       INTEGER N, NW, I, J
*** Check values
       IF(NW.GT.MXW)THEN
            PRINT *,' !!!!!! SMOOTH WARNING : Smoothing width'//
     -           ' is larger than MXW; rejected.'
            RETURN
       ELSEIF(N.GT.MXN)THEN
            PRINT *,' !!!!!! SMOOTH WARNING : Number of points to be'//
     -           ' smoothed is larger than MXN; rejected.'
            RETURN
       ENDIF
*** Weighting factors
       DO 10 I=-NW, NW
       W(I) = EXP(-0.5*(REAL(I)/SIGMA)**2)
 10    CONTINUE
*** Smoothen
       DO 20 I=1, N
       XNEW(I) = 0
       WSUM = 0
       DO 30 J=-NW, NW
       IF(I+J.GE.1 .AND. I+J.LE.N .AND.
     -    I-J.GE.1 .AND. I-J.LE.N)THEN
            XNEW(I) = XNEW(I) + W(J)*X(I+J)
            WSUM = WSUM + W(J)
       ENDIF
 30    CONTINUE
       XNEW(I)=XNEW(I)/WSUM
 20    CONTINUE
*** Copy back
       DO 40 I=1,N
       X(I)=XNEW(I)
 40    CONTINUE
       END
