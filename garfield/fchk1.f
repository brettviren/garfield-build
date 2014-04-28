CDECK  ID>, FCHK1.
       SUBROUTINE FCHK1(M,U1,F1,X)
*-----------------------------------------------------------------------
*   FCHK1  - One of 2 auxiliary routines for verifying that space
*            charges indeed have the proper charge.
*   (Last changed on  8/ 4/98.)
*-----------------------------------------------------------------------
       implicit none
       DOUBLE PRECISION U1(*),F1(*),X(2),XC,YC,ZC,RC
       REAL XF,YF,ZF,EX,EY,EZ,ETOT,VOLT
       INTEGER ILOC,L,M
       COMMON /FCHDAT/ XC,YC,ZC,RC
*** Loop over the positions.
       DO 10 L=1,M
       X(1)=U1(L)
       XF=XC+COS(X(1))*COS(X(2))*RC
       YF=YC+SIN(X(1))*COS(X(2))*RC
       ZF=ZC+          SIN(X(2))*RC
       CALL EFIELD(XF,YF,ZF,EX,EY,EZ,ETOT,VOLT,0,ILOC)
       F1(L)=DBLE((EX*COS(X(1))+EY*SIN(X(1)))*COS(X(2))+EZ*SIN(X(2)))
10     CONTINUE
       END
