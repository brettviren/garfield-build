CDECK  ID>, CF2CTR.
       SUBROUTINE CF2CTR(X,Y,RHO,PHI,N)
*-----------------------------------------------------------------------
*   CF2CTR - Routine transforming (x,y) to (rho,phi) via the conformal
*            map (x,y)=exp(rho,phi). This routine may in principle be
*            replaced by any conformal mapping routine.
*   (Last changed on  3/10/98.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION X(*),Y(*),RHO(*),PHI(*),RHOI,PHII
       INTEGER I,N
*** Loop over the points.
       DO 10 I=1,N
       IF(X(I).EQ.0.AND.Y(I).EQ.0)THEN
            RHOI=-25.0
            PHII=0.0
       ELSE
            RHOI=0.5*LOG(X(I)**2+Y(I)**2)
            PHII=ATAN2(Y(I),X(I))
       ENDIF
       RHO(I)=RHOI
       PHI(I)=PHII
10     CONTINUE
       END
