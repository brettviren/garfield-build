CDECK  ID>, DIVDF2C.
      DOUBLE PRECISION FUNCTION DIVDF2(F,A,NN,X,MM)
*-----------------------------------------------------------------------
*   DIVDF2 - Double precision version of DIVDIF (CERN program library
*            E105) which performs tabular interpolation using
*            symmetrically placed argument points. Added a check on
*            X values located on the limits.
*   (Last changed on 20/ 8/02.)
*-----------------------------------------------------------------------
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION A(*),F(*),T(20),D(20)
      LOGICAL EXTRA
      DATA MMAX/10/
*** Check the arguments.
      IF( (NN.LT.2) .OR. (MM.LT.1) ) THEN
           PRINT *,' ###### DIVDF2 ERROR   : Invalid dimensions'//
     -          ' received for the arguments.'
           GO TO 20
      ENDIF
*** Deal with the case that X is located at A(1) or A(N).
      IF(ABS(X-A(1)).LE.1E-6*(ABS(A(1))+ABS(A(NN))))THEN
C           print *,' x at lower limit ',x,a(1),a(nn)
           DIVDF2=F(1)
           RETURN
      ELSEIF(ABS(X-A(NN)).LE.1E-6*(ABS(A(1))+ABS(A(NN))))THEN
C           print *,' x at upper limit ',x,a(1),a(nn)
           DIVDF2=F(NN)
           RETURN
      ENDIF
C      if(x.lt.a(1).or.x.gt.a(nn))print *,'x out of range: ',x
*** Find subscript IX of X in array A.
      N=NN
      M=MIN0(MM,MMAX,N-1)
      MPLUS=M+1
      IX=0
      IY=N+1
      IF(A(1).GT.A(N)) GO TO 4
*** Search increasing arguments.
    1    MID=(IX+IY)/2
         IF(X.GE.A(MID)) GO TO 2
            IY=MID
            GO TO 3
*** If true.
    2       IX=MID
    3    IF(IY-IX.GT.1) GO TO 1
         GO TO 7
*** Search decreasing arguments.
    4    MID=(IX+IY)/2
         IF(X.LE.A(MID)) GO TO 5
            IY=MID
            GO TO 6
C        (IF TRUE.)
    5       IX=MID
    6    IF(IY-IX.GT.1) GO TO 4
C
C  Copy reordered interpolation points into (T(I),D(I)), setting
C  *EXTRA* to TRUE if M+2 points to be used.
C
    7 NPTS=M+2-MOD(M,2)
      IP=0
      L=0
      GO TO 9
    8    L=-L
         IF(L.GE.0) L=L+1
    9    ISUB=IX+L
         IF((1.LE.ISUB).AND.(ISUB.LE.N)) GO TO 10
*** skip point.
            NPTS=MPLUS
            GO TO 11
*** Insert point.
   10       IP=IP+1
            T(IP)=A(ISUB)
            D(IP)=F(ISUB)
   11    IF(IP.LT.NPTS) GO TO 8
      EXTRA=NPTS.NE.MPLUS
C
C  Replace d by the leading diagonal of a divided-difference table, sup-
C  plemented by an extra line if *EXTRA* is true.
C
      DO 14 L=1,M
         IF(.NOT.EXTRA) GO TO 12
            ISUB=MPLUS-L
            D(M+2)=(D(M+2)-D(M))/(T(M+2)-T(ISUB))
   12    I=MPLUS
         DO 13 J=L,M
            ISUB=I-L
            D(I)=(D(I)-D(I-1))/(T(I)-T(ISUB))
            I=I-1
   13    CONTINUE
   14 CONTINUE
C
C  Evaluate the Newton interpolation formula at X, averaging two values
C  of last difference if *EXTRA* is TRUE.
C
      SUM=D(MPLUS)
      IF(EXTRA) SUM=0.5*(SUM+D(M+2))
      J=M
      DO 15 L=1,M
         SUM=D(J)+(X-T(J))*SUM
         J=J-1
   15 CONTINUE
      DIVDF2=SUM
      RETURN
*** Error processing.
   20 CONTINUE
      DIVDF2=0
      END
