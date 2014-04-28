CDECK  ID>, POLFUN.
       SUBROUTINE POLFUN(X,A,F)
*-----------------------------------------------------------------------
*   POLFUN - Auxiliary function for fitting a polynomial.
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
       END
