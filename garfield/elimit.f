CDECK  ID>, ELIMIT.
      SUBROUTINE ELIMIT(IELOW)
      IMPLICIT REAL*8 (A-H,O-Z)
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       DOUBLE PRECISION PIR2,ECHARG,EMASS,AMU,BOLTZ,BOLTZJ,
     -      AWB,ALOSCH,ABZERO,ATMOS
       PARAMETER(PIR2=8.79735534D-17)
       PARAMETER(ECHARG=1.602176462D-19)
       PARAMETER(EMASS=9.10938188D-31)
       PARAMETER(AMU=1.66053873D-27)
       PARAMETER(BOLTZ=8.617342D-5)
       PARAMETER(BOLTZJ=1.3806503D-23)
       PARAMETER(AWB=1.758820174D10)
       PARAMETER(ALOSCH=2.6867775D19)
       PARAMETER(ABZERO=273.15D0)
       PARAMETER(ATMOS=760.0D0)
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
C -------------------------------------------------------------------
C   CALCULATES COLLISION EVENTS AND TESTS TO FIND IF THE UPPER ENERGY
C   LIMIT FOR THE ELECTRON ENERGY IS EXCEEDED.
C    IF ENERGY LIMIT IS OK       IELOW = 0
C    IF ENERGY LIMIT IS EXCEEDED IELOW = 1
C   THE TEST IS CARRIED OUT FOR A SAMPLE OF COLLISIONS THAT ARE
C   SMALLER THAN THE FULL SAMPLE BY A FACTOR OF 1/ISAMP
C
C   USED WITH MAGNETIC FIELD B =0.0   ELECTRIC FIELD IN Z DIRECTION.
C -------------------------------------------------------------------
      ISAMP=10
      SMALL=1.0D-20
      RDUM=RSTART
      E1=ESTART
      INTEM=8
      TDASH=0.0D0
C
C     INITIAL DIRECTION COSINES
C
      DCZ1=COS(THETA)
      DCX1=SIN(THETA)*COS(PHI)
      DCY1=SIN(THETA)*SIN(PHI)
C
      BP=EMAG*EMAG*CONST1
      F1=EMAG*CONST2
      F2=EMAG*CONST3
      F4=2.0D0*ACOS(-1.0D0)
      DELTAE=EFINAL/DBLE(INTEM)
      J2M=NMAX/ISAMP
C MAIN LOOP
      DO 210 J1=1,J2M
    1 R1=drand48(RDUM)
      I=INT(E1/DELTAE)+1
      I=MIN(I,INTEM)
      TLIM=TCFMAX(I)
      T=-LOG(R1)/TLIM+TDASH
      TDASH=T
      AP=DCZ1*F2*SQRT(E1)
      E=E1+(AP+BP*T)*T
      IE=INT(E/ESTEP)+1
      IE=MIN(IE,2048)
      IF(TCF(IE).GT.TLIM) THEN
       TDASH=TDASH+LOG(R1)/TLIM
       TCFMAX(I)=1.05D0*TCFMAX(I)
       GO TO 1
      ENDIF
C
C     TEST FOR REAL OR NULL COLLISION
C
      R5=drand48(RDUM)
      TLIM=TCF(IE)/TLIM
      IF(R5.GT.TLIM) GO TO 1
C
C  CALCULATE DIRECTION COSINES AT INSTANT BEFORE COLLISION
C
      IF(IE.EQ.2048) THEN
C ELECTRON ENERGY OUT OF RANGE
       IELOW=1
       RETURN
      ENDIF
      TDASH=0.0D0
      CONST6=SQRT(E1/E)
      DCX2=DCX1*CONST6
      DCY2=DCY1*CONST6
      DCZ2=DCZ1*CONST6+EMAG*T*CONST5/SQRT(E)
C ---------------------------------------------------------------------
C     DETERMINATION OF REAL COLLISION TYPE
C ---------------------------------------------------------------------
      R2=drand48(RDUM)
C FIND LOCATION WITHIN 4 UNITS IN COLLISION ARRAY
      CALL SORT(I,R2,IE)
  140 I=I+1
      IF(CF(IE,I).LT.R2) GO TO 140
      S1=RGAS(I)
      EI=EIN(I)
      IF(IPN(I).LE.0) GO TO 666
      R9=drand48(RDUM)
      EXTRA=R9*(E-EI)
      EI=EXTRA+EI
C
C  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER
C   COLLISION ALSO UPDATE ENERGY OF ELECTRON.
C
  666 IPT=IARRY(I)
      IF(E.LT.EI) THEN
       EI=E-0.0001D0
      ENDIF
      S2=(S1*S1)/(S1-1.0D0)
      IF(INDEX(I).NE.0) THEN
       R31=drand48(RDUM)
       R3=drand48(RDUM)
       F3=1.0D0-R3*ANGCT(IE,I)
       IF(R31.GT.PSCT(IE,I))  F3=-F3
      ELSE
       R3=drand48(RDUM)
       F3=1.0D0-2.0D0*R3
      ENDIF
      THETA0=ACOS(F3)
      R4=drand48(RDUM)
      PHI0=F4*R4
      F8=SIN(PHI0)
      F9=COS(PHI0)
      ARG1=1.0D0-S1*EI/E
      ARG1=MAX(ARG1,SMALL)
      D=1.0D0-F3*SQRT(ARG1)
      E1=E*(1.0D0-EI/(S1*E)-2.0D0*D/S2)
      E1=MAX(E1,SMALL)
      Q=SQRT((E/E1)*ARG1)/S1
      Q=MIN(Q,1.0D0)
      THETA=ASIN(Q*SIN(THETA0))
      F6=COS(THETA)
      U=(S1-1.0D0)*(S1-1.0D0)/ARG1
      CSQD=F3*F3
      IF(F3.LT.0.0D0.AND.CSQD.GT.U) F6=-1.0D0*F6
      F5=SIN(THETA)
      DCZ2=MIN(DCZ2,1.0D0)
      ARGZ=SQRT(DCX2*DCX2+DCY2*DCY2)
      IF(ARGZ.EQ.0.0D0) THEN
       DCZ1=F6
       DCX1=F9*F5
       DCY1=F8*F5
       GO TO 210
      ENDIF
      DCZ1=DCZ2*F6+ARGZ*F5*F8
      DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
      DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
C LOOP
  210 CONTINUE
      IELOW=0
      END
