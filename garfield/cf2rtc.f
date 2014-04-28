CDECK  ID>, CF2RTC.
       SUBROUTINE CF2RTC(RHO,PHI,X,Y,N)
*-----------------------------------------------------------------------
*   CF2RTC - Routine transforming (rho,phi) to (x,y) via the conformal
*            map (x,y)=exp(rho,phi). This routine may in principle be
*            replaced by any conformal mapping routine.
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X(*),Y(*),RHO(*),PHI(*),XI,YI
       INTEGER I,N
*** Loop over the points.
       DO 10 I=1,N
       XI=EXP(RHO(I))*COS(PHI(I))
       YI=EXP(RHO(I))*SIN(PHI(I))
       X(I)=XI
       Y(I)=YI
10     CONTINUE
       END
