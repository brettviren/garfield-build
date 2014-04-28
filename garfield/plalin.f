CDECK  ID>, PLALIN.
       SUBROUTINE PLALIN(X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0,A,B,C,
     -      XCUT,YCUT,ZCUT,IFAIL)
*-----------------------------------------------------------------------
*   PLALIN - Cuts an arbitrary plane with a line.
*   Variables : (X1,Y1,Z1) : starting point of the line
*               (X2,Y2,Z2) : end point of the line
*               (X0,Y0,Z0) : point on the plane
*               (A,B,C)    : parameters of the plane
*   (Last changed on 31/ 1/98.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X1,Y1,Z1,X2,Y2,Z2,X0,Y0,Z0,A,B,C,
     -      XCUT,YCUT,ZCUT,XLAM,PROD1,PROD2,EPS
       INTEGER IFAIL
*** Initial values for the return parameters.
       XCUT=0
       YCUT=0
       ZCUT=0
*** Form the two products.
       PROD1=(X0-X1)*A+(Y0-Y1)*B+(Z0-Z1)*C
       PROD2=(X2-X1)*A+(Y2-Y1)*B+(Z2-Z1)*C
*** Set a tolerance for lambda.
       EPS=1.0D-5
*** Check the products are non-zero.
       IF(ABS(PROD2).GT.1.0D-6*SQRT((A**2+B**2+C**2)*
     -      (X2-X1)**2+(Y2-Y1)**2+(Z2-Z1)**2))THEN
            XLAM=PROD1/PROD2
            IF(XLAM.GE.-EPS.AND.XLAM.LE.1.0D0+EPS)THEN
                 IFAIL=0
            ELSE
                 IFAIL=1
            ENDIF
            XLAM=MAX(0.0D0,MIN(1.0D0,XLAM))
            XCUT=X1+XLAM*(X2-X1)
            YCUT=Y1+XLAM*(Y2-Y1)
            ZCUT=Z1+XLAM*(Z2-Z1)
C      print *,' PLALIN - Line crosses plane'
C      print *,'          abc  =',a,b,c
C      print *,'          line =',x1,y1,z1
C      print *,'          line =',x2,y2,z2
C      print *,'          prod2=',prod2
       ELSE
            XCUT=0
            YCUT=0
            ZCUT=0
            IFAIL=1
C      print *,' PLALIN -  Line does not cross plane'
       ENDIF
       END
