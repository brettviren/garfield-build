CDECK  ID>, FCHK3.
       SUBROUTINE FCHK3(M,U1,F1,X)
*-----------------------------------------------------------------------
*   FCHK3  - One of 2 auxiliary routines for verifying that space
*            charges indeed have the proper charge.
*   (Last changed on  8/ 4/98.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION U1(*),F1(*),X(2),XC,YC,ZC,RC
       REAL XF,YF,EX,EY,EZ,ETOT,VOLT
       INTEGER ILOC,L,M
       COMMON /FCHDAT/ XC,YC,ZC,RC
*** Loop over the positions.
       DO 10 L=1,M
       X(1)=U1(L)
       XF=XC+COS(X(1))*RC
       YF=YC+SIN(X(1))*RC
       CALL EFIELD(XF,YF,0.0,EX,EY,EZ,ETOT,VOLT,0,ILOC)
       F1(L)=RC*DBLE(EX*COS(X(1))+EY*SIN(X(1)))
10     CONTINUE
       END
