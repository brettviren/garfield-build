CDECK  ID>, INTUBE.
       SUBROUTINE INTUBE(X,Y,A,N,ILOC)
*-----------------------------------------------------------------------
*   INTUBE - Determines whether a point is located inside a polygon.
*            ILOC is set to +1 if outside, 0 if inside and -1 if the
*            arguments are not valid.
*   (Last changed on 18/ 3/01.)
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
       REAL X,Y,A,PHI
       INTEGER N,ILOC
*** Special case: x=y=0
       IF(X.EQ.0.AND.Y.EQ.0)THEN
            ILOC=0
*** Special case: round tube.
       ELSEIF(N.EQ.0)THEN
            IF(X**2+Y**2.GT.A**2)THEN
                 ILOC=1
            ELSE
                 ILOC=0
            ENDIF
*** Illegal number of edges.
       ELSEIF(N.LT.0.OR.N.EQ.1.OR.N.EQ.2)THEN
            PRINT *,' ###### INTUBE ERROR   : Invalid number of'//
     -           ' edges received (N=',N,').'
            ILOC=-1
*** Truely polygonal tubes.
       ELSE
*   Reduce angle to the first sector.
            PHI=ATAN2(Y,X)
            IF(PHI.LT.0.0)PHI=PHI+2*PI
            PHI=PHI-REAL(2)*PI*INT(0.5*N*PHI/PI)/REAL(N)
*   Compare the length to the local radius.
            IF((X**2+Y**2)*COS(PI/REAL(N)-PHI)**2.GT.
     -           A**2*COS(PI/REAL(N))**2)THEN
                 ILOC=1
            ELSE
                 ILOC=0
            ENDIF
       ENDIF
       END
