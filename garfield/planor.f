CDECK  ID>, PLANOR.
       SUBROUTINE PLANOR(N,X,Y,Z,A,B,C,D,IFAIL)
*-----------------------------------------------------------------------
*   PLANOR - Computes a normal vector to a polygon.
*   (Last changed on 16/11/02.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER N,I,J,K,IFAIL
       DOUBLE PRECISION X(N),Y(N),Z(N),A,B,C,D,AA,BB,CC,FNORM
*** Initial settings.
       A=0
       B=0
       C=0
*** Check number of points.
       IF(N.LT.3)THEN
            PRINT *,' !!!!!! PLANOR WARNING : Insufficient number of'//
     -           ' points to compute a normal vector.'
            IFAIL=1
            RETURN
       ENDIF
*** Loop over all cross products.
       DO 10 I=1,N
       DO 20 J=1,N-1
       IF(I.EQ.J)GOTO 20
       DO 30 K=J+1,N
       IF(I.EQ.K.OR.J.EQ.K)GOTO 30
       AA=(Y(J)-Y(I))*(Z(K)-Z(I))-(Y(K)-Y(I))*(Z(J)-Z(I))
       BB=(Z(J)-Z(I))*(X(K)-X(I))-(Z(K)-Z(I))*(X(J)-X(I))
       CC=(X(J)-X(I))*(Y(K)-Y(I))-(X(K)-X(I))*(Y(J)-Y(I))
       IF(  (AA.LT.0.AND.BB.LT.0).OR.
     -      (AA.LT.0.AND.CC.LT.0).OR.
     -      (BB.LT.0.AND.CC.LT.0))THEN
            A=A-AA
            B=B-BB
            C=C-CC
       ELSE
            A=A+AA
            B=B+BB
            C=C+CC
       ENDIF
30     CONTINUE
20     CONTINUE
10     CONTINUE
*** Normalise the sum.
       FNORM=A**2+B**2+C**2
       IF(FNORM.LE.0)THEN
            PRINT *,' !!!!!! PLANOR WARNING : Computed normal vector'//
     -           ' has zero norm; probably degenerate curve.'
            A=0
            B=0
            C=0
            IFAIL=1
       ELSE
            A=A/SQRT(FNORM)
            B=B/SQRT(FNORM)
            C=C/SQRT(FNORM)
            IFAIL=0
       ENDIF
*** Find the offset.
       D=0
       DO 40 I=1,N
       D=D+A*X(I)+B*Y(I)+C*Z(I)
40     CONTINUE
       D=D/N
       END
