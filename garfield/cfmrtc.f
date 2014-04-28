CDECK  ID>, CFMRTC.
       SUBROUTINE CFMRTC(RHO,PHI,X,Y,N)
*-----------------------------------------------------------------------
*   CFMRTC - Routine transforming (rho,phi) to (x,y) via the conformal
*            map (x,y)=exp(rho,phi). This routine may in principle be
*            replaced by any conformal mapping routine.
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       REAL X(*),Y(*),RHO(*),PHI(*),XI,YI
       INTEGER N,I
       COMPLEX Z
*** Loop over the points.
       DO 10 I=1,N
       Z=EXP(CMPLX(RHO(I),PHI(I)))
       XI=REAL(Z)
       YI=AIMAG(Z)
       X(I)=XI
       Y(I)=YI
10     CONTINUE
       END
