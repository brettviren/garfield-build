CDECK  ID>, GAS56.
      SUBROUTINE GAS56(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(57),YXSEC(57),XION(46),YION(46),XATT(16),YATT(16),
     /XVIB1(34),YVIB1(34),XVIB2(28),YVIB2(28),XVIB3(28),YVIB3(28),
     /XVIB4(25),YVIB4(25),XVIB5(19),YVIB5(19),XEXC1(25),YEXC1(25),
     /XEXC2(23),YEXC2(23),XEXC3(19),YEXC3(19)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XEN/0.00,.001,.002,.004,.007,0.01,.015,0.02,.025,0.03,
     /0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.12,0.14,0.17,
     /0.20,0.25,0.30,0.35,0.40,0.50,0.60,0.80,1.00,1.50,
     /2.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,10.0,15.0,
     /20.0,30.0,40.0,70.0,100.,140.,200.,250.,300.,500.,
     /1000.,1500.,3000.,6000.,10000.,20000.,100000./
      DATA YXSEC/60.0,59.0,56.0,54.0,51.0,45.0,35.0,27.5,22.5,19.0,
     /14.0,10.7,9.40,7.80,6.90,6.00,5.50,5.30,5.55,6.55,
     /8.05,11.5,13.5,14.5,15.5,16.5,17.5,18.5,19.5,21.0,
     /22.5,25.0,29.0,31.0,34.0,36.0,36.0,34.0,31.0,21.5,
     /17.0,11.5,8.80,5.20,3.75,2.21,1.36,0.98,0.81,0.46,
     /0.20,0.13,0.06,.026,.016,.0065,.0013/
      DATA XION/10.67,11.2,12.7,13.7,14.7,17.2,20.0,25.0,30.0,35.0,
     /40.0,45.0,50.0,60.0,70.0,80.0,90.0,100.,125.,150.,
     /175.,200.,250.,300.,350.,400.,450.,500.,600.,700.,
     /800.,900.,1000.,1250.,1500.,1750.,2000.,2500.,3000.,5000.,
     /7000.,10000.,15000.,30000.,60000.,100000./
      DATA YION/0.00,0.27,0.61,0.99,1.48,3.00,4.30,6.77,8.41,9.58,
     /10.4,11.1,12.0,12.7,13.1,13.3,13.3,13.3,12.9,12.2,
     /11.5,10.9,10.1,8.89,8.12,7.51,6.84,6.41,5.63,5.19,
     /4.77,4.25,3.97,3.43,2.95,2.68,2.44,2.11,1.81,1.20,
     /0.90,0.66,0.47,.254,.136,.086/
      DATA XATT/6.85,7.00,7.20,7.50,8.00,8.50,9.00,9.50,10.0,10.5,
     /11.0,11.5,12.0,12.5,13.0,13.2/
      DATA YATT/0.00,1.15,1.92,2.90,4.90,7.72,11.6,18.1,25.7,21.6,
     /17.0,10.9,6.14,2.30,0.87,0.00/
      DATA XVIB1/.052,.055,.060,.065,.070,.075,0.08,0.10,0.12,0.14,
     /0.20,0.25,0.30,0.40,0.50,0.70,1.00,1.50,2.00,3.00,
     /4.00,5.00,6.00,7.50,8.50,10.0,15.0,20.0,30.0,40.0,
     /100.,1000.,10000.,100000./
      DATA YVIB1/0.00,.014,.021,.024,.026,.027,.028,.028,.027,.025,
     /.021,.018,.016,.014,.012,.009,.008,.012,.015,.024,
     /.036,.047,.060,.079,.079,.065,.045,.025,.014,.008,
     /.002,.0002,.00002,.00002/
      DATA XVIB2/.108,.125,0.15,0.20,0.23,0.25,0.30,0.40,0.50,0.70,
     /1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.50,8.50,10.0,
     /15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB2/0.00,0.27,0.52,0.71,0.73,0.73,0.66,0.56,0.49,0.41,
     /0.32,0.32,0.39,0.63,0.93,1.22,1.57,2.06,2.06,1.69,
     /1.17,0.66,0.35,0.22,0.05,.005,.0005,.00005/
      DATA XVIB3/.173,0.18,0.19,0.20,0.23,0.25,0.30,0.40,0.50,0.70,
     /1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.50,8.50,10.0,
     /15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB3/0.00,0.13,0.27,0.38,0.49,0.53,0.56,0.53,0.49,0.42,
     /0.34,0.31,0.33,0.48,0.72,0.94,1.21,1.59,1.59,1.30,
     /0.90,0.51,0.27,0.17,0.04,.004,.0004,.00004/
      DATA XVIB4/.363,0.40,0.45,0.50,0.60,0.70,0.80,1.00,1.50,2.00,
     /3.00,4.00,5.00,6.00,7.50,8.50,10.0,15.0,20.0,30.0,
     /40.0,100.,1000.,10000.,100000./
      DATA YVIB4/0.00,0.47,0.63,0.70,0.74,0.74,0.70,0.66,0.63,0.69,
     /1.00,1.43,1.86,2.40,2.65,2.29,1.69,0.97,0.43,0.24,
     /0.14,0.03,.003,.0003,.00003/
      DATA XVIB5/.519,1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.50,8.50,
     /10.0,15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB5/0.00,.001,0.01,.033,.085,0.16,0.20,0.27,0.30,0.25,
     /.193,.112,.047,.027,.017,.003,.0003,.00003,.000003/
      DATA XEXC1/7.40,8.70,9.70,11.0,12.0,14.0,16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC1/0.00,1.30,1.89,2.02,2.08,2.15,2.15,2.15,2.15,2.15,
     /2.21,2.21,2.15,2.02,1.69,1.56,1.30,1.22,1.04,0.68,
     /0.33,0.17,0.06,.034,.007/
      DATA XEXC2/9.70,10.7,11.7,14.0,16.0,20.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,300.,400.,600.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YEXC2/0.00,0.19,0.40,0.75,1.16,1.56,1.82,1.98,
     /2.15,2.21,2.15,2.02,1.69,1.56,1.30,1.22,1.04,0.68,
     /0.32,0.17,0.06,.034,.006/
      DATA XEXC3/17.0,20.0,25.0,30.0,40.0,60.0,80.0,100.,150.,200.,
     /300.,400.,600.,1000.,2000.,4000.,10000.,20000.,100000./
      DATA YEXC3/0.00,0.43,0.94,1.30,1.82,2.15,2.15,2.02,1.69,1.56,
     /1.30,1.22,1.04,0.68,0.33,0.17,0.07,.034,.006/
C--------------------------------------------------------
      NAME='n-C4H10 (2003)'
C ---------------------------------------------------------------------
C  NO EXPERIMENTAL DATA AVAILABLE ON DIFFUSION . DRIFT VELOCITY DATA
C  FROM FLORIANO,GEE AND FREEMAN USED.
C  ANALYSIS : FIXED INELASTIC X-SECTIONS TO ISOBUTANE VALUES AND VARIED
C  ELASTIC X-SECTION TO OBTAIN FIT TO DRIFT VELOCITY.
C  NO USEFUL ELECTRON SCATTERING DATA AVAILABLE.
C ----------------------------------------------------------------------
      NIN=10
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=57
      NION=46
      NATT=16
      NVIB1=34
      NVIB2=28
      NVIB3=28
      NVIB4=25
      NVIB5=19
      NEXC1=25
      NEXC2=23
      NEXC3=19
      E(1)=0.0
      E(2)=2.0*EMASS/(58.1234*AMU)
      E(3)=10.67
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=10.67
      EIN(1)=-0.052
      EIN(2)=0.052
      EIN(3)=-0.108
      EIN(4)=0.108
      EIN(5)=0.173
      EIN(6)=0.363
      EIN(7)=0.519
      EIN(8)=7.4
      EIN(9)=9.70
      EIN(10)=17.0
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       N-BUTANE       '
      SCRPT(3)=' IONISATION    ELOSS= 10.67   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB           ELOSS= -0.052  '
      SCRPT(8)=' VIB           ELOSS=  0.052  '
      SCRPT(9)=' VIB           ELOSS= -0.108  '
      SCRPT(10)=' VIB           ELOSS=  0.108  '
      SCRPT(11)=' VIB           ELOSS=  0.173  '
      SCRPT(12)=' VIB           ELOSS=  0.363  '
      SCRPT(13)=' VIB           ELOSS=  0.519  '
      SCRPT(14)=' EXC           ELOSS=  7.4    '
      SCRPT(15)=' EXC           ELOSS=  9.70   '
      SCRPT(16)=' EXC           ELOSS= 17.0    '
      APOP=EXP(EIN(1)/AKT)
      HPOP=EXP(EIN(3)/AKT)
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
C
      Q(3,I)=0.0D0
      IF(EN.LT.E(3)) GO TO 200
      DO 110 J=2,NION
      IF(EN.LE.XION(J)) GO TO 120
  110 CONTINUE
      J=NION
  120 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
C
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
C SUPERELASTIC VIB
      QIN(1,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 305
      DO 301 J=2,NVIB1
      IF((EN+EIN(2)).LE.XVIB1(J)) GO TO 302
  301 CONTINUE
      J=NVIB1
  302 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(1,I)=(EN+EIN(2))*(A*(EN+EIN(2))+B)*1.D-16/EN
      QIN(1,I)=APOP*QIN(1,I)/(1.0+APOP)
C
  305 QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 4000
      DO 4100 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 4200
 4100 CONTINUE
      J=NVIB1
 4200 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(2,I)=(A*EN+B)*1.D-16/(1.0+APOP)
 4000 CONTINUE
      QIN(3,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 1100
      DO 307 J=2,NVIB2
      IF((EN+EIN(4)).LE.XVIB2(J)) GO TO 308
  307 CONTINUE
      J=NVIB2
  308 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(3,I)=(EN+EIN(4))*(A*(EN+EIN(4))+B)*1.D-16/EN
      QIN(3,I)=HPOP*QIN(3,I)/(1.0+HPOP)
 1100 QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 400
      DO 310 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 320
  310 CONTINUE
      J=NVIB2
  320 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(4,I)=(A*EN+B)*1.D-16/(1.0+HPOP)
  400 CONTINUE
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 500
      DO 410 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 420
  410 CONTINUE
      J=NVIB3
  420 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(5,I)=(A*EN+B)*1.D-16
  500 CONTINUE
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 600
      DO 510 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 520
  510 CONTINUE
      J=NVIB4
  520 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(6,I)=(A*EN+B)*1.D-16
  600 CONTINUE
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 700
      DO 610 J=2,NVIB5
      IF(EN.LE.XVIB5(J)) GO TO 620
  610 CONTINUE
      J=NVIB5
  620 A=(YVIB5(J)-YVIB5(J-1))/(XVIB5(J)-XVIB5(J-1))
      B=(XVIB5(J-1)*YVIB5(J)-XVIB5(J)*YVIB5(J-1))/(XVIB5(J-1)-XVIB5(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  700 CONTINUE
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 800
      DO 710 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 720
  710 CONTINUE
      J=NEXC1
  720 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  800 CONTINUE
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 900
      DO 810 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 820
  810 CONTINUE
      J=NEXC2
  820 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(9,I)=(A*EN+B)*1.D-16
  900 CONTINUE
      QIN(10,I)=0.0D0
      IF(EN.LE.EIN(10)) GO TO 990
      DO 910 J=2,NEXC3
      IF(EN.LE.XEXC3(J)) GO TO 920
  910 CONTINUE
      J=NEXC3
  920 A=(YEXC3(J)-YEXC3(J-1))/(XEXC3(J)-XEXC3(J-1))
      B=(XEXC3(J-1)*YEXC3(J)-XEXC3(J)*YEXC3(J-1))/(XEXC3(J-1)-XEXC3(J))
      QIN(10,I)=(A*EN+B)*1.D-16
  990 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)+QIN(9,I)+QIN(10,I)
 1000 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(10)) NIN=9
      IF(EFINAL.LE.EIN(9)) NIN=8
      IF(EFINAL.LE.EIN(8)) NIN=7
      IF(EFINAL.LE.EIN(7)) NIN=6
      IF(EFINAL.LE.EIN(6)) NIN=5
      IF(EFINAL.LE.EIN(5)) NIN=4
      IF(EFINAL.LE.EIN(4)) NIN=3
      END
