CDECK  ID>, CFMCTR.
       SUBROUTINE CFMCTR(X,Y,RHO,PHI,N)
*-----------------------------------------------------------------------
*   CFMCTR - Routine transforming (x,y) to (rho,phi) via the conformal
*            map (x,y)=exp(rho,phi). This routine may in principle be
*            replaced by any conformal mapping routine.
*   (Last changed on 14/ 2/97.)
*-----------------------------------------------------------------------
       implicit none
       REAL X(*),Y(*),RHO(*),PHI(*),RHOI,PHII
       INTEGER I,N
       COMPLEX Z
*** Loop over the points.
       DO 10 I=1,N
       IF(X(I).EQ.0.AND.Y(I).EQ.0)THEN
            RHOI=-25.0
            PHII=0.0
       ELSE
            Z=LOG(CMPLX(X(I),Y(I)))
            RHOI=REAL(Z)
            PHII=AIMAG(Z)
       ENDIF
       RHO(I)=RHOI
       PHI(I)=PHII
10     CONTINUE
       END
