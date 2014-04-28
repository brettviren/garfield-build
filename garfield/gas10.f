CDECK  ID>, GAS10.
      SUBROUTINE GAS10(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(59),YXSEC(59),XION(46),YION(46),XATT(16),YATT(16),
     /XVIB1(28),YVIB1(28),XVIB2(28),YVIB2(28),XVIB3(25),YVIB3(25),
     /XVIB4(19),YVIB4(19),XEXC1(25),YEXC1(25),XEXC2(23),YEXC2(23),
     /XEXC3(19),YEXC3(19)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XEN/0.00,0.001,0.002,0.003,0.004,0.005,0.007,0.01,0.014,0.02,
     /0.03,0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.11,0.12,
     /0.13,0.14,0.16,0.18,0.20,0.24,0.30,0.40,0.50,
     /0.60,0.80,1.00,1.40,2.00,3.00,4.00,5.00,6.00,7.50,
     /8.50,10.0,15.0,20.0,30.0,40.0,70.0,100.,140.,200.,
     /250.,300.,500.,1000.,1500.,3000.,6000.,10000.,20000.,100000./
      DATA YXSEC/55.0,55.0,46.0,40.0,36.0,32.0,27.5,22.5,19.5,16.5,
     /14.2,12.5,11.2,9.80,8.20,6.70,5.30,3.80,3.00,2.65,
     /2.60,2.60,2.90,3.40,4.30,6.10,8.40,10.0,11.2,
     /12.0,12.5,13.0,13.7,15.5,17.7,22.0,25.4,27.7,30.0,
     /26.0,23.1,16.7,13.0,9.00,6.80,4.00,2.88,1.70,1.05,
     /0.75,0.62,0.35,.155,0.10,.045,0.02,.012,.005,.001/
      DATA XION/10.95,12.0,13.0,14.0,15.0,17.5,20.0,25.0,30.0,35.0,
     /40.0,45.0,50.0,60.0,70.0,80.0,90.0,100.,125.,150.,
     /175.,200.,250.,300.,350.,400.,450.,500.,600.,700.,
     /800.,900.,1000.,1250.,1500.,1750.,2000.,2500.,3000.,5000.,
     /7000.,10000.,15000.,30000.,60000.,100000./
      DATA YION/0.00,0.21,0.47,0.76,1.14,2.30,3.31,5.21,6.47,7.37,
     /8.00,8.54,9.22,9.79,10.1,10.2,10.2,10.2,9.90,9.36,
     /8.84,8.35,7.80,6.84,6.25,5.78,5.26,4.93,4.33,3.99,
     /3.67,3.27,3.05,2.64,2.27,2.06,1.88,1.62,1.39,0.92,
     /0.69,0.51,0.36,.195,.105,.066/
      DATA XATT/6.85,7.00,7.20,7.50,8.00,8.50,9.00,9.50,10.0,10.5,
     /11.0,11.5,12.0,12.5,13.0,13.2/
      DATA YATT/0.00,0.90,1.48,2.23,3.78,5.94,8.91,13.9,19.8,16.6,
     /13.1,8.37,4.72,1.76,0.67,0.00/
      DATA XVIB1/.108,.125,0.15,0.20,0.23,0.25,0.30,0.40,0.50,0.70,
     /1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.50,8.50,10.0,
     /15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB1/0.00,0.16,0.31,0.42,0.43,0.43,0.39,0.33,0.29,0.24,
     /0.19,0.19,0.23,0.37,0.55,0.72,0.93,1.22,1.22,1.00,
     /0.69,0.39,0.21,0.13,0.03,.003,.0003,.00003/
      DATA XVIB2/.173,0.18,0.19,0.20,0.23,0.25,0.30,0.40,0.50,0.70,
     /1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.50,8.50,10.0,
     /15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB2/0.00,0.10,0.21,0.29,0.38,0.41,0.43,0.41,0.38,0.32,
     /0.26,0.24,0.25,0.37,0.55,0.72,0.93,1.22,1.22,1.00,
     /0.69,0.39,0.21,0.13,0.03,.003,.0003,.00003/
      DATA XVIB3/.363,0.40,0.45,0.50,0.60,0.70,0.80,1.00,1.50,2.00,
     /3.00,4.00,5.00,6.00,7.50,8.50,10.0,15.0,20.0,30.0,
     /40.0,100.,1000.,10000.,100000./
      DATA YVIB3/0.00,0.33,0.44,0.49,0.52,0.52,0.49,0.46,0.44,0.48,
     /0.70,1.00,1.30,1.68,1.85,1.60,1.18,0.68,0.30,0.17,
     /0.10,0.02,.002,.0002,.00002/
      DATA XVIB4/.519,1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.50,8.50,
     /10.0,15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB4/0.00,.001,0.01,.020,.050,.094,0.12,0.16,0.18,0.15,
     /.114,.066,.028,.016,.010,.002,.0002,.00002,.000002/
      DATA XEXC1/7.70,9.00,10.0,11.0,12.0,14.0,16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC1/0.00,1.00,1.45,1.55,1.60,1.65,1.65,1.65,1.65,1.65,
     /1.70,1.70,1.65,1.55,1.30,1.20,1.00,0.94,0.80,0.52,
     /0.25,0.13,0.05,.026,.005/
      DATA XEXC2/10.0,11.0,12.0,14.0,16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC2/0.00,0.15,0.31,0.58,0.89,1.20,1.40,1.52,
     /1.65,1.70,1.65,1.55,1.30,1.20,1.00,0.94,0.80,0.52,
     /0.25,0.13,0.05,.026,.005/
      DATA XEXC3/17.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC3/0.00,0.33,0.72,1.00,
     /1.40,1.65,1.65,1.55,1.30,1.20,1.00,0.94,0.80,0.52,
     /0.25,0.13,0.05,.026,.005/
C
      NAME='C3H8 (1999)'
C ---------------------------------------------------------------------
      NIN=8
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=59
      NION=46
      NATT=16
      NVIB1=28
      NVIB2=28
      NVIB3=25
      NVIB4=19
      NEXC1=25
      NEXC2=23
      NEXC3=19
      E(1)=0.0
      E(2)=2.0*EMASS/(44.09652*AMU)
      E(3)=10.95
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=10.95
      EIN(1)=-0.108
      EIN(2)=0.108
      EIN(3)=0.173
      EIN(4)=0.363
      EIN(5)=0.519
      EIN(6)=7.7
      EIN(7)=10.0
      EIN(8)=17.0
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       PROPANE        '
      SCRPT(3)=' IONISATION    ELOSS=10.95    '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB           ELOSS= -0.108  '
      SCRPT(8)=' VIB           ELOSS=  0.108  '
      SCRPT(9)=' VIB           ELOSS=  0.173  '
      SCRPT(10)=' VIB           ELOSS=  0.363  '
      SCRPT(11)=' VIB           ELOSS=  0.519  '
      SCRPT(12)=' EXC           ELOSS=  7.70   '
      SCRPT(13)=' EXC           ELOSS= 10.0    '
      SCRPT(14)=' EXC           ELOSS= 17.0    '
      APOP=EXP(EIN(1)/AKT)
      EN=-ESTEP/2.0D0
      DO 1000 I=1,NSTEP
      EN=EN+ESTEP
      DO 10 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 20
   10 CONTINUE
      J=NDATA
   20 A=(YXSEC(J)-YXSEC(J-1))/(XEN(J)-XEN(J-1))
      B=(XEN(J-1)*YXSEC(J)-XEN(J)*YXSEC(J-1))/(XEN(J-1)-XEN(J))
      Q(2,I)=(A*EN+B)*1.0D-16
      Q(3,I)=0.0D0
      IF(EN.LT.E(3)) GO TO 200
      DO 110 J=2,NION
      IF(EN.LE.XION(J)) GO TO 120
  110 CONTINUE
      J=NION
  120 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
  200 Q(4,I)=0.0D0
      IF(EN.LT.XATT(1)) GO TO 300
      IF(EN.GT.XATT(NATT)) GO TO 300
      DO 210 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 220
  210 CONTINUE
      J=NATT
  220 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=(A*EN+B)*1.D-21
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
      QIN(1,I)=0.0D0
      IF(EN.LE.0.0) GO TO 1100
      DO 1010 J=2,NVIB1
      IF((EN+EIN(2)).LE.XVIB1(J)) GO TO 1020
 1010 CONTINUE
      J=NVIB1
 1020 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(1,I)=(EN+EIN(2))*(A*(EN+EIN(2))+B)*1.D-16/EN
      QIN(1,I)=QIN(1,I)*APOP/(1.0+APOP)
 1100 CONTINUE
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 400
      DO 310 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 320
  310 CONTINUE
      J=NVIB1
  320 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(2,I)=(A*EN+B)*1.D-16
      QIN(2,I)=QIN(2,I)/(1.0+APOP)
  400 CONTINUE
      QIN(3,I)=0.0D0
      IF(EN.LE.EIN(3)) GO TO 500
      DO 410 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(3,I)=(A*EN+B)*1.D-16
  500 CONTINUE
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 600
      DO 510 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 520
  510 CONTINUE
      J=NVIB3
  520 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(4,I)=(A*EN+B)*1.D-16
  600 CONTINUE
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 700
      DO 610 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 620
  610 CONTINUE
      J=NVIB4
  620 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(5,I)=(A*EN+B)*1.D-16
  700 CONTINUE
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 800
      DO 710 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 720
  710 CONTINUE
      J=NEXC1
  720 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(6,I)=(A*EN+B)*1.D-16
  800 CONTINUE
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 900
      DO 810 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 820
  810 CONTINUE
      J=NEXC2
  820 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  900 CONTINUE
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 990
      DO 910 J=2,NEXC3
      IF(EN.LE.XEXC3(J)) GO TO 920
  910 CONTINUE
      J=NEXC3
  920 A=(YEXC3(J)-YEXC3(J-1))/(XEXC3(J)-XEXC3(J-1))
      B=(XEXC3(J-1)*YEXC3(J)-XEXC3(J)*YEXC3(J-1))/(XEXC3(J-1)-XEXC3(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  990 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)
 1000 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(8)) NIN=7
      IF(EFINAL.LE.EIN(7)) NIN=6
      IF(EFINAL.LE.EIN(6)) NIN=5
      IF(EFINAL.LE.EIN(5)) NIN=4
      IF(EFINAL.LE.EIN(4)) NIN=3
      IF(EFINAL.LE.EIN(3)) NIN=2
      IF(EFINAL.LE.EIN(2)) NIN=1
      IF(EFINAL.LE.EIN(1)) NIN=0
      END
