CDECK  ID>, CRTUBE.
       SUBROUTINE CRTUBE(X0,Y0,Z0,X1,Y1,Z1,
     -      XX0,YY0,ZZ0,XX1,YY1,ZZ1,R,IFAIL)
*-----------------------------------------------------------------------
*   CRTUBE - Computes the crossing points of a tube with a line segment.
*   (Last changed on 25/ 3/96.)
*-----------------------------------------------------------------------
       DOUBLE PRECISION C0,C1,C2,DET2,P1,P2
       REAL X0,Y0,Z0,X1,Y1,Z1,XX0,YY0,ZZ0,XX1,YY1,ZZ1,R
       INTEGER IFAIL
*** Initial values.
       XX0=X0
       XX1=X1
       YY0=Y0
       YY1=Y1
       ZZ0=Z0
       ZZ1=Z1
       IFAIL=1
*** Polynomial coefficients.
       C2=(X1-X0)**2+(Y1-Y0)**2
       C1=2*X0*(X1-X0)+2*Y0*(Y1-Y0)
       C0=X0**2+Y0**2-R**2
*** Determinant.
       DET2=C1**2-4*C0*C2
*** Solutions.
       IF(DET2.LT.0)THEN
            PRINT *,' !!!!!! CRTUBE WARNING : The line segment does'//
     -           ' not cross the tube.'
            RETURN
       ELSEIF(DET2.EQ.0)THEN
C           P1=-C1/(2*C2)
C           P2=-C1/(2*C2)
            PRINT *,' !!!!!! CRTUBE WARNING : The line segment'//
     -           ' touches the tube or has length 0.'
            RETURN
       ELSE
            P1=(-C1-SQRT(DET2))/(2*C2)
            P2=(-C1+SQRT(DET2))/(2*C2)
       ENDIF
       IF((P1.LT.0.AND.P2.LT.0).OR.(P1.GT.1.AND.P2.GT.1))THEN
            PRINT *,' !!!!!! CRTUBE WARNING : The line segment is'//
     -           ' located outside the tube.'
            RETURN
       ENDIF
*** Slightly shorten the line segment.
       IF(P1.LT.0)THEN
            P1=0
       ELSEIF(P1.GT.1)THEN
            P1=1
       ELSEIF(P1.GT.0.5)THEN
            P1=0.999*P1
       ELSE
            P1=1.001*P1
       ENDIF
       IF(P2.LT.0)THEN
            P2=0
       ELSEIF(P2.GT.1)THEN
            P2=1
       ELSEIF(P2.GT.0.5)THEN
            P2=0.999*P2
       ELSE
            P2=1.001*P2
       ENDIF
*** And establish the new end points.
       XX0=X0+P1*(X1-X0)
       YY0=Y0+P1*(Y1-Y0)
       ZZ0=Z0+P1*(Z1-Z0)
       XX1=X0+P2*(X1-X0)
       YY1=Y0+P2*(Y1-Y0)
       ZZ1=Z0+P2*(Z1-Z0)
*** Things worked, reset IFAIL to 0.
       IFAIL=0
       END
