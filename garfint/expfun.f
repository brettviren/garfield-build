CDECK  ID>, EXPFUN.
       SUBROUTINE EXPFUN(X,A,F)
*-----------------------------------------------------------------------
*   EXPFUN - Auxiliary function for fitting an exponential polynomial.
*   (Last changed on  9/ 5/96.)
*-----------------------------------------------------------------------
       DOUBLE PRECISION A(*),X,F
       INTEGER NNA
       COMMON /PFDAT/ NNA
*** Sum the polynomial.
       F=0
       DO 10 I=NNA,1,-1
       F=F*X+A(I)
10     CONTINUE
*** Take an exponential.
       IF(F.LT.-50)THEN
            F=0
       ELSE
            F=EXP(MIN(30.0D0,F))
       ENDIF
       END
