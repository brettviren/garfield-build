CDECK  ID>, RNDCOV.
       SUBROUTINE RNDCOV(COV,VEC,IFAIL)
*-----------------------------------------------------------------------
*   RNDCOV - Random 3D Gaussian numbers according to the covariance
*            matrix COV. Uses the Cholesky decomposition of COV into
*            CHOL CHOL' = COV and then multiplies a normal Gaussian
*            vector with the Cholesky root CHOL.
*            Covariance has to be positive semi-definite.
*            Uses pivot search for the diagonal of L.
*   Variables: COV         : Covariance matrix
*              VEC         : N-Vector of random numbers.
*              IFAIL       : Set to 1 if COV not pos. def.
*   (Last changed on 28/ 1/02.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER N
       PARAMETER(N=3)
       REAL COV(N,N),CHOL(N,N),VEC(N),VNOR(N),SUM,SUMAUX,RNDNOR,EPS
C      real check(n,n)
       INTEGER I,J,K,IFAIL,P(N),IP,IAUX
       EXTERNAL RNDNOR
*** Initialise Lower diagonal matrix, output vector and tolerance.
       EPS=0
       DO 10 I=1,N
       DO 20 J=1,N
       CHOL(I,J)=0
       IF(ABS(COV(I,J)).GT.EPS)EPS=ABS(COV(I,J))
20     CONTINUE
       VEC(I)=0
10     CONTINUE
       EPS=1E-5*EPS
*** Initialise the pivot vector.
       DO 100 I=1,N
       P(I)=I
100    CONTINUE
*** Cholesky decomposition.
       DO 30 I=1,N
*   Find the p element.
       IP=I
       SUM=COV(P(I),P(I))
       DO 130 K=1,I-1
       SUM=SUM-CHOL(P(I),P(K))**2
130    CONTINUE
       DO 110 J=I+1,N
       SUMAUX=COV(P(J),P(J))
       DO 140 K=1,I-1
       SUMAUX=SUMAUX-CHOL(P(J),P(K))**2
140    CONTINUE
       IF(SUMAUX.GT.SUM)THEN
            SUM=SUMAUX
            IP=J
       ENDIF
110    CONTINUE
*   Reorder the p vector.
       IAUX=P(IP)
       DO 120 J=IP,I+1,-1
       P(J)=P(J-1)
120    CONTINUE
       P(I)=IAUX
*   Verify that the matrix is positive-definite.
       IF(SUM.LE.-EPS)THEN
            PRINT *,' !!!!!! RNDCOV WARNING : Covariance matrix is'//
     -           ' not positive definite; no random numbers.'
            IFAIL=1
            RETURN
*   If semi-positive definite, set the remainder to 0.
       ELSEIF(ABS(SUM).LE.EPS)THEN
C            print *,' Semi positive definite matrix, sum=',sum
            GOTO 50
       ENDIF
*   Assign diagonal elements.
       CHOL(P(I),P(I))=SQRT(SUM)
*   Compute off-diagonal elements.
       DO 40 J=I+1,N
       SUM=COV(P(J),P(I))
       DO 60 K=1,I-1
       SUM=SUM-CHOL(P(J),P(K))*CHOL(P(I),P(K))
60     CONTINUE
       CHOL(P(J),P(I))=SUM/CHOL(P(I),P(I))
40     CONTINUE
30     CONTINUE
*** Multiply L with a normal Gaussian vector.
50     CONTINUE
       DO 70 I=1,N
       VNOR(I)=RNDNOR(0.0,1.0)
70     CONTINUE
       DO 80 I=1,N
       VEC(I)=0
       DO 90 J=1,N
       VEC(I)=VEC(I)+CHOL(I,J)*VNOR(J)
90     CONTINUE
80     CONTINUE
*** Seems to have worked.
       IFAIL=0
*   Check the product.
C       print *,' Pivot vector: ',p
C       print *,' L'
C       do i=1,N
C       print '(10f12.5)',(chol(i,j),j=1,N)
C       enddo
C       do i=1,N
C       do j=1,N
C       check(p(i),p(j))=0
C       do k=1,N
C       if(i.ge.k.and.j.ge.k)
C     -      check(p(i),p(j))=check(p(i),p(j))+
C     -      chol(p(i),p(k))*chol(p(j),p(k))
C       enddo
C       enddo
C       enddo
C       print *,' Check: '
C       do i=1,N
C       print '(10f12.5)',(check(i,j),j=1,N)
C       enddo
C       print *,' Origin:'
C       do i=1,N
C       print '(10f12.5)',(cov(i,j),j=1,N)
C       enddo
       END
