CDECK  ID>, ERFIR.
       DOUBLE PRECISION FUNCTION ERFIR(X)
*-----------------------------------------------------------------------
*   ERFIR  - Error function of imaginary argument erfi, divided by the
*            exponential term exp(x**2).
*
*            Repeated fraction and asymptotic series expansion from
*            Mathematica and Mathworld documentation.
*   (Last changed on 26/ 2/08.)
*-----------------------------------------------------------------------
       implicit none
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       DOUBLE PRECISION X,X2,FRAC
       INTEGER I
*** Special cases
       IF(X.EQ.0)THEN
            ERFIR=0
*** Asymptotic power series
      ELSEIF(ABS(X).GT.5)THEN
           ERFIR = (1.0/X + 1.0/(2.0*X**3) + 3.0/(4.0*X**5) +
     -          15.0/(8.0*X**7) + 105.0/(16.0*X**9) +
     -          945.0/(32.0*X**11) + 10395.0/(64.0*X**13) +
     -          135135.0/(132.0*X**15))/SQRT(PI)
*** Continued fraction
       ELSE
            X2 = X**2
            FRAC=1.0
            DO 10 I=52,4,-4
            FRAC=DBLE(I-3)+DBLE(I-2)*X2/(DBLE(I-1)-DBLE(I)*X2/FRAC)
10          CONTINUE
            ERFIR=2.0*X/(SQRT(PI)*FRAC)
       ENDIF
       END
