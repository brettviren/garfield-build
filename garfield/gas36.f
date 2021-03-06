CDECK  ID>, GAS36.
      SUBROUTINE GAS36(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
     /,PEQEL,PEQIN,KEL,KIN,SCRPT)
      IMPLICIT REAL*8 (A-H,O-Z)
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
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
      DIMENSION PEQEL(6,2048),PEQIN(220,2048),KIN(220),KEL(6)
      DIMENSION Q(6,2048),QIN(220,2048),E(6),EIN(220)
      DIMENSION XEN(24),YXSEC(24),XVIBH(18),YVIBH(18),XION(46),YION(46),
     /XATT(17),YATT(17),XEXC(25),YEXC(25),XEXC1(23),YEXC1(23),
     /XEXC2(19),YEXC2(19)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XEN/0.00,.001,0.01,0.10,0.40,1.00,1.50,2.00,3.00,
     /4.00,5.00,7.50,10.0,12.0,15.0,20.0,30.0,60.0,100.,
     /200.0,500.0,1000.0,10000.,100000./
      DATA YXSEC/2160.,2160.,1840.,184.,44.5,17.0,14.0,13.0,14.0,
     /21.0,26.0,33.5,33.5,31.5,26.5,20.5,15.5,6.50,3.70,
     /1.30,0.45,0.21,.015,.0012/
      DATA XVIBH/0.00,2.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,10.0,
     /11.0,12.0,15.0,20.0,100.,1000.,10000.,100000./
      DATA YVIBH/0.00,0.00,.034,0.34,0.89,1.19,1.42,1.48,1.42,1.19,
     /0.89,0.68,0.45,0.25,.030,.0015,.00015,.000015/
      DATA XION/10.18,10.7,12.0,13.0,14.0,16.5,19.5,25.0,30.0,35.0,
     /40.0,45.0,50.0,60.0,70.0,80.0,90.0,100.,125.,150.,
     /175.,200.,250.,300.,350.,400.,450.,500.,600.,700.,
     /800.,900.,1000.,1250.,1500.,1750.,2000.,2500.,3000.,5000.,
     /7000.,10000.,15000.,30000.,60000.,100000./
      DATA YION/0.00,0.26,0.59,0.96,1.43,2.91,4.17,6.57,8.16,9.30,
     /10.1,10.8,11.6,12.3,12.7,12.9,12.9,12.9,12.5,11.8,
     /11.2,10.6,9.80,8.63,7.88,7.29,6.64,6.22,5.46,5.04,
     /4.63,4.12,3.85,3.33,2.86,2.60,2.37,2.05,1.76,1.16,
     /0.87,0.64,0.46,0.25,0.11,.083/
      DATA XATT/5.00,5.50,6.00,7.00,7.50,8.00,8.50,9.00,9.50,10.0,
     /10.4,11.0,12.0,13.0,14.0,15.0,20.0/
      DATA YATT/0.00,.145,0.44,0.38,0.32,0.24,0.26,0.36,0.66,1.24,
     /2.00,1.08,0.30,0.20,0.16,0.12,0.00/
      DATA XEXC/7.00,9.00,10.0,11.0,12.0,14.0,16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC/0.00,1.07,1.57,1.69,1.74,1.80,1.80,1.80,1.80,1.80,
     /1.85,1.85,1.80,1.69,1.41,1.30,1.09,1.02,0.86,0.56,
     /0.27,0.13,.055,.028,.005/
      DATA XEXC1/9.00,10.0,11.0,13.0,16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC1/0.00,0.19,0.40,0.75,1.14,1.53,1.80,1.94,
     /2.11,2.18,2.11,1.98,1.66,1.53,1.28,1.20,1.02,0.67,
     /0.32,0.16,.064,.033,.007/
      DATA XEXC2/16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC2/0.00,0.42,0.92,1.28,
     /1.80,2.11,2.11,1.98,1.66,1.53,1.28,1.20,1.02,0.67,
     /0.32,0.16,.064,.033,.007/
C
      NAME='2-propanol 1999'
C --------------------------------------------------------------------
C   X-SECTIONS FROM SCALING ETHANOL X-SECTIONS  AT LOW ENERGY AND
C   FITS TO DRIFT VELOCITY OF CHRISTOPHOROU AND CHRISTODOULIDES.
C ---------------------------------------------------------------------
      NIN=9
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=24
      NVIBH=18
      NION=46
      NATT=17
      NEXC=25
      NEXC1=23
      NEXC2=19
      E(1)=0.0
      E(2)=2.0*EMASS/(60.09592*AMU)
      E(3)=10.18
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=10.18
      EIN(1)=-0.025
      EIN(2)=0.025
      EIN(3)=-0.109
      EIN(4)=0.109
      EIN(5)=0.1668
      EIN(6)=0.3527
      EIN(7)=7.00
      EIN(8)=9.00
      EIN(9)=16.0
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       PROPANOL       '
      SCRPT(3)=' IONISATION    ELOSS= 10.18   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' ROT           ELOSS= -0.025  '
      SCRPT(8)=' ROT           ELOSS=  0.025  '
      SCRPT(9)=' VIB V8        ELOSS= -0.109  '
      SCRPT(10)=' VIB V8        ELOSS=  0.109  '
      SCRPT(11)=' VIB V6        ELOSS=  0.1668 '
      SCRPT(12)=' VIB V3        ELOSS=  0.3527 '
      SCRPT(13)=' EXC           ELOSS=  7.00   '
      SCRPT(14)=' EXC           ELOSS=  9.00   '
      SCRPT(15)=' EXC           ELOSS= 16.0    '
      APOP1=EXP(EIN(1)/AKT)
      APOP2=EXP(EIN(3)/AKT)
      EN=-ESTEP/2.0D0
      DO 900 I=1,NSTEP
      EN=EN+ESTEP
      Q(2,I)=0.0D0
C USE LOG INTERPOLATION BECAUSE OF RAPID CHANGE IN X-SEC
      IF(EN.LE.XEN(2)) THEN
       Q(2,I)=YXSEC(2)*1.D-16
       GO TO 30
      ENDIF
      DO 10 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 20
   10 CONTINUE
      J=NDATA
   20 YXJ=LOG(YXSEC(J))
      YXJ1=LOG(YXSEC(J-1))
      XNJ=LOG(XEN(J))
      XNJ1=LOG(XEN(J-1))
      A=(YXJ-YXJ1)/(XNJ-XNJ1)
      B=(XNJ1*YXJ-XNJ*YXJ1)/(XNJ1-XNJ)
      Q(2,I)=EXP(A*LOG(EN)+B)*1.D-16
C
   30 Q(3,I)=0.0D0
      IF(EN.LT.E(3)) GO TO 40
      DO 31 J=2,NION
      IF(EN.LE.XION(J)) GO TO 32
   31 CONTINUE
      J=NION
   32 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
   40 CONTINUE
C
      Q(4,I)=0.0D0
      IF(EN.LT.XATT(1)) GO TO 50
      IF(EN.GE.XATT(NATT)) GO TO 50
      DO 41 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 42
   41 CONTINUE
      J=NATT
   42 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=(A*EN+B)*1.D-19
   50 CONTINUE
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
C  SUPERELASTIC  ROT1
C
      QIN(1,I)=0.0D0
      IF(EN.LE.0.0) GO TO 150
      EFAC=SQRT(1.0-(EIN(1)/EN))
      QIN(1,I)=0.7*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(1,I)=QIN(1,I)*APOP1/(1.0+APOP1)*1.D-16
C  ROT1
  150 QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 200
      EFAC=SQRT(1.0-(EIN(2)/EN))
      QIN(2,I)=0.7*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(2,I)=QIN(2,I)/(1.0+APOP1)*1.D-16
C
C  SUPERELASTIC V1
C
  200 QIN(3,I)=0.0D0
      IF(EN.LE.0.0) GO TO 250
      EFAC=SQRT(1.0-(EIN(3)/EN))
      QIN(3,I)=0.443*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      DO 220 J=2,NVIBH
      IF((EN+EIN(4)).LE.XVIBH(J)) GO TO 230
  220 CONTINUE
      J=NVIBH
  230 A=(YVIBH(J)-YVIBH(J-1))/(XVIBH(J)-XVIBH(J-1))
      B=(XVIBH(J-1)*YVIBH(J)-XVIBH(J)*YVIBH(J-1))/(XVIBH(J-1)-XVIBH(J))
      QIN(3,I)=QIN(3,I)+(EN+EIN(4))*(A*(EN+EIN(4))+B)/EN
      QIN(3,I)=QIN(3,I)*APOP2/(1.0+APOP2)*1.D-16
C INELASTIC V1
  250 QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 300
      EFAC=SQRT(1.0-(EIN(4)/EN))
      QIN(4,I)=0.443*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 270 J=2,NVIBH
      IF(EN.LE.XVIBH(J)) GO TO 280
  270 CONTINUE
      J=NVIBH
  280 A=(YVIBH(J)-YVIBH(J-1))/(XVIBH(J)-XVIBH(J-1))
      B=(XVIBH(J-1)*YVIBH(J)-XVIBH(J)*YVIBH(J-1))/(XVIBH(J-1)-XVIBH(J))
      QIN(4,I)=QIN(4,I)+(A*EN+B)
      QIN(4,I)=QIN(4,I)/(1.0+APOP2)*1.D-16
C  VIB 2
  300 QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 400
      EFAC=SQRT(1.0-(EIN(5)/EN))
      QIN(5,I)=0.465*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 310 J=2,NVIBH
      IF(EN.LE.XVIBH(J)) GO TO 320
  310 CONTINUE
      J=NVIBH
  320 A=(YVIBH(J)-YVIBH(J-1))/(XVIBH(J)-XVIBH(J-1))
      B=(XVIBH(J-1)*YVIBH(J)-XVIBH(J)*YVIBH(J-1))/(XVIBH(J-1)-XVIBH(J))
      QIN(5,I)=(QIN(5,I)+(A*EN+B))*1.D-16
  400 CONTINUE
C  VIB 3
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 500
      EFAC=SQRT(1.0-(EIN(6)/EN))
      QIN(6,I)=0.92*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 410 J=2,NVIBH
      IF(EN.LE.XVIBH(J)) GO TO 420
  410 CONTINUE
      J=NVIBH
  420 A=(YVIBH(J)-YVIBH(J-1))/(XVIBH(J)-XVIBH(J-1))
      B=(XVIBH(J-1)*YVIBH(J)-XVIBH(J)*YVIBH(J-1))/(XVIBH(J-1)-XVIBH(J))
      QIN(6,I)=(QIN(6,I)+(A*EN+B))*1.D-16
  500 CONTINUE
C EXC
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 600
      DO 510 J=2,NEXC
      IF(EN.LE.XEXC(J)) GO TO 520
  510 CONTINUE
      J=NEXC
  520 A=(YEXC(J)-YEXC(J-1))/(XEXC(J)-XEXC(J-1))
      B=(XEXC(J-1)*YEXC(J)-XEXC(J)*YEXC(J-1))/(XEXC(J-1)-XEXC(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  600 CONTINUE
C  EXC 1
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 700
      DO 610 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 620
  610 CONTINUE
      J=NEXC1
  620 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  700 CONTINUE
C EXC 2
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 800
      DO 710 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 720
  710 CONTINUE
      J=NEXC2
  720 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(9,I)=(A*EN+B)*1.D-16
  800 CONTINUE
C---------------------------------------------------------------------
C    SUBTRACT ROTATIONAL XSEC TO GET CORRECT ELASTIC XSEC.
      Q(2,I)=Q(2,I)-QIN(1,I)-QIN(2,I)
C     IF(Q(2,I).LE.0.0 .and. LBMCPR)WRITE(LUNOUT,966) Q(2,I),I
C 966 FORMAT(3X,' ERROR IN GAS 27 Q(2,I)=',D12.3,'  I=',I5)
C  TOTAL
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+
     /QIN(3,I)+QIN(4,I)+QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)+QIN(9,I)
  900 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(9)) NIN=8
      IF(EFINAL.LE.EIN(8)) NIN=7
      IF(EFINAL.LE.EIN(7)) NIN=6
      IF(EFINAL.LE.EIN(6)) NIN=5
      IF(EFINAL.LE.EIN(5)) NIN=4
      IF(EFINAL.LE.EIN(4)) NIN=3
      IF(EFINAL.LE.EIN(3)) NIN=2
      IF(EFINAL.LE.EIN(2)) NIN=1
      IF(EFINAL.LE.EIN(1)) NIN=0
C
      END
