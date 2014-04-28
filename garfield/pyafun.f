CDECK  ID>, PYAFUN.
       SUBROUTINE PYAFUN(X,A,F)
*-----------------------------------------------------------------------
*   PYAFUN - Auxiliary function for fitting a Polya distribution.
*   (Last changed on 19/ 8/96.)
*-----------------------------------------------------------------------
       DOUBLE PRECISION A(*),X,F,DGAMMF
       EXTERNAL DGAMMF
*** Compute Polya function.
       IF(A(3)+A(4)*X.LE.0)THEN
            F=0
       ELSEIF(A(2).LE.-1)THEN
            F=0
       ELSEIF(ABS((A(2)+1)*(A(3)+A(4)*X)).GT.30)THEN
            F=0
       ELSE
            F=A(1)*A(4)*(A(2)+1)**(A(2)+1)/DGAMMF(A(2)+1)*
     -           (A(3)+A(4)*X)**A(2)*
     -           EXP(-(A(2)+1)*(A(3)+A(4)*X))
       ENDIF
       END
