CDECK  ID>, FCHK6.
       SUBROUTINE FCHK6(M,U1,F1,X)
*-----------------------------------------------------------------------
*   FCHK6  - One of 2 auxiliary routines for calculating a flux.
*   (Last changed on 13/ 5/99.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION U1(*),F1(*),X(2),
     -      X0,Y0,Z0,X1,Y1,Z1,XP,YP,ZP
       REAL XF,YF,ZF,EX,EY,EZ,ETOT,VOLT
       INTEGER ILOC,L,M,NU,ISIGN
       COMMON /FCHDA6/ X0,Y0,Z0,X1,Y1,Z1,XP,YP,ZP,NU,ISIGN
*** Loop over the positions.
       DO 10 L=1,M
       X(1)=U1(L)
       XF=X0+X(1)*(X1-X0)
       YF=Y0+X(1)*(Y1-Y0)
       ZF=Z0+X(1)*(Z1-Z0)
       CALL EFIELD(XF,YF,ZF,EX,EY,EZ,ETOT,VOLT,0,ILOC)
       IF(ISIGN.EQ.0)THEN
            F1(L)=EX*XP+EY*YP+EZ*ZP
       ELSEIF(ISIGN*(EX*XP+EY*YP+EZ*ZP).GT.0)THEN
            F1(L)=ABS(EX*XP+EY*YP+EZ*ZP)
       ELSE
            F1(L)=-1
       ENDIF
10     CONTINUE
       END
