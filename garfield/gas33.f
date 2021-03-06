CDECK  ID>, GAS33.
      SUBROUTINE GAS33(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(49),YXSEC(49),XVIB1(32),YVIB1(32),XVIB2(31),
     /YVIB2(31),XVIB3(15),YVIB3(15),XVIB4(28),YVIB4(28),XVIB5(21),
     /YVIB5(21),XEXC1(23),YEXC1(23),XEXC2(20),YEXC2(20),
     /XION(46),YION(46),XATT(16),YATT(16)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XEN/0.00,0.01,.014,0.02,0.03,0.04,0.05,0.06,0.07,0.08,
     /0.09,0.10,0.11,0.12,0.14,0.16,0.18,0.20,0.25,0.30,
     /0.40,0.50,0.60,0.80,1.00,1.40,2.00,3.00,4.00,5.00,
     /6.00,7.50,8.50,10.0,15.0,20.0,30.0,40.0,70.0,
     /100.,140.,200.,250.,300.,500.,1000.,1500.,10000.,100000./
      DATA YXSEC/13.0,11.0,10.5,9.80,7.80,5.60,4.20,2.90,2.10,2.00,
     /2.20,2.65,3.25,3.90,5.65,7.30,9.15,10.8,14.2,16.8,
     /20.0,21.5,22.0,22.5,22.7,22.8,22.9,23.0,23.5,25.5,
     /27.0,29.0,27.0,22.0,15.4,12.0,8.31,6.28,3.69,
     /2.66,1.57,0.97,0.70,0.57,0.32,.143,.092,.011,.001/
      DATA XVIB1/.107,1.00,1.20,1.40,1.60,1.80,2.00,2.20,2.40,2.60,
     /3.00,4.00,4.50,5.00,5.50,6.00,6.50,7.00,8.00,9.00,
     /10.0,11.0,15.0,20.0,25.0,30.0,50.0,100.,200.,1000.,
     /10000.,100000./
      DATA YVIB1/0.0,.001,.022,.040,.080,.080,.080,.085,.085,.085,
     /0.13,0.22,0.70,1.10,1.25,1.15,0.75,0.60,0.71,0.77,
     /0.71,0.64,0.31,0.25,0.18,0.12,0.06,.025,0.01,.001,
     /.00003,.000003/
      DATA XVIB2/.178,1.00,1.40,1.60,1.80,2.00,2.20,2.40,2.60,2.80,
     /3.00,3.20,3.40,4.00,5.00,6.00,7.00,8.00,9.00,10.0,
     /11.0,15.0,20.0,25.0,30.0,50.0,100.,200.,1000.,10000.,
     /100000./
      DATA YVIB2/0.00,.001,0.15,0.25,0.40,0.45,0.47,0.50,0.52,0.55,
     /0.57,0.60,0.62,0.66,0.74,0.90,1.14,1.33,1.38,1.23,
     /1.01,0.56,0.44,0.34,0.25,0.14,.059,.025,.003,.0001,
     /.00001/
      DATA XVIB3/.295,1.00,3.00,4.00,4.50,5.00,5.50,6.00,6.50,7.00,
     /10.0,100.0,1000.,10000.,100000./
      DATA YVIB3/0.00,.001,0.01,0.01,0.05,0.10,0.15,0.10,0.05,0.01,
     /.001,.0001,.00001,.000001,.0000001/
      DATA XVIB4/.374,1.00,1.40,1.60,1.80,2.00,2.20,2.40,2.60,3.00,
     /4.00,5.00,6.00,7.00,8.00,9.00,10.0,11.0,15.0,20.0,
     /25.0,30.0,50.0,100.,200.,1000.,10000.,100000./
      DATA YVIB4/0.00,.001,.029,.049,0.30,0.44,0.47,0.50,0.55,0.70,
     /0.75,1.15,1.40,1.70,1.80,1.70,1.50,1.40,0.90,0.66,
     /0.57,0.40,0.22,0.92,0.04,.004,.0004,.00004/
      DATA XVIB5/.748,1.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,10.0,
     /11.0,15.0,20.0,25.0,30.0,50.0,100.,200.,1000.,10000.,
     /100000./
      DATA YVIB5/0.00,.0001,.002,.030,.052,.088,0.11,0.12,0.10,.084,
     /.065,.035,.025,.020,.016,.009,.004,.0014,.0002,.000005,
     /.0000005/
      DATA XEXC1/7.30,7.50,8.00,8.50,9.00,10.0,11.0,14.0,20.0,25.0,
     /30.0,40.0,60.0,80.0,100.,150.,200.,400.,1000.,2000.,
     /10000.,20000.,100000./
      DATA YEXC1/0.00,.026,0.21,0.36,0.65,1.11,1.70,2.38,2.74,2.81,
     /2.86,2.81,2.69,2.55,2.38,2.14,1.87,1.46,0.82,0.41,
     /0.09,.044,.009/
      DATA XEXC2/9.00,10.0,11.0,14.0,16.0,20.0,25.0,30.0,40.0,60.0,
     /80.0,100.,150.,200.,400.,1000.,2000.,10000.,20000.,100000./
      DATA YEXC2/0.00,0.43,1.11,2.04,2.30,2.64,2.81,2.86,2.81,2.69,
     /2.55,2.38,2.14,1.87,1.46,0.82,0.41,0.09,.044,.009/
      DATA XION/9.86,11.0,12.0,13.0,14.0,16.5,19.0,24.0,30.0,35.0,
     /40.0,45.0,50.0,60.0,70.0,80.0,90.0,100.,125.,150.,
     /175.,200.,250.,300.,350.,400.,450.,500.,600.,700.,
     /800.,900.,1000.,1250.,1500.,1750.,2000.,2500.,3000.,5000.,
     /7000.,10000.,15000.,30000.,60000.,100000./
      DATA YION/0.00,0.19,0.43,0.70,1.05,2.12,3.06,4.81,5.97,6.80,
     /7.38,7.88,8.51,9.04,9.32,9.42,9.42,9.42,9.14,8.64,
     /8.16,7.71,7.20,6.31,5.77,5.34,4.86,4.55,4.00,3.68,
     /3.39,3.02,2.82,2.44,2.10,1.90,1.74,1.50,1.28,0.85,
     /0.64,0.47,0.33,0.18,.097,.061/
      DATA XATT/6.85,7.00,7.20,7.50,8.00,8.50,9.00,9.50,10.0,10.5,
     /11.0,11.5,12.0,12.5,13.0,13.2/
      DATA YATT/0.00,0.67,1.10,1.65,2.80,4.40,6.60,10.3,14.7,12.3,
     /9.70,6.20,3.50,1.30,0.50,0.00/
      NAME='cyclo-C3H6 (99)'
C ---------------------------------------------------------------------
C  1999 INCLUDED VIBRATIONAL RESONACE SHAPES FROM ALLEN (ERHARDT AND
C   MORGAN) AND ASLO BOESTEN AND TANAKA XIX ICPEAC
C   FIT TO  SCHMIDTS ,GEE+FREEMAN AND BOWMAN+GORDON DATA IN
C   PURE CYCLO - PROPANE AND SCHMIDT IN HELIUM/CYCLOPROPANE.
C   NO GOOD DATA AT HIGH FIELD THEREFORE X-SECTIONS ABOVE 1 EV ARE
C   DERIVED FROM SYSTEMATICS IN THE HYDROCARBONS AND ABOVE REFS.
C ---------------------------------------------------------------------
      NIN=9
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=49
      NVIB1=32
      NVIB2=31
      NVIB3=15
      NVIB4=28
      NVIB5=21
      NEXC1=23
      NEXC2=20
      NION=46
      NATT=16
      E(1)=0.0
      E(2)=2.0*EMASS/(42.08064*AMU)
      E(3)=9.86
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=9.86
      EIN(1)=-0.107
      EIN(2)=0.107
      EIN(3)=-0.178
      EIN(4)=0.178
      EIN(5)=0.295
      EIN(6)=0.374
      EIN(7)=0.748
      EIN(8)=7.30
      EIN(9)=9.00
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       CYCLO PROPANE  '
      SCRPT(3)=' IONISATION    ELOSS=  9.86   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB V11       ELOSS= -0.107  '
      SCRPT(8)=' VIB V11       ELOSS=  0.107  '
      SCRPT(9)=' VIB           ELOSS= -0.178  '
      SCRPT(10)=' VIB           ELOSS=  0.178  '
      SCRPT(11)=' VIB 2V3       ELOSS=  0.295  '
      SCRPT(12)=' VIB           ELOSS=  0.374  '
      SCRPT(13)=' VIB HAR       ELOSS=  0.748  '
      SCRPT(14)=' EXC           ELOSS=  7.30   '
      SCRPT(15)=' EXC           ELOSS=  9.00   '
      AMP1=0.120
      AMP2=0.090
      AMP3=0.109
      APOP=EXP(EIN(1)/AKT)
      APOPH=EXP(EIN(3)/AKT)
      EN=-ESTEP/2.0D0
      DO 900 I=1,NSTEP
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
C
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
C V7 SUPERELASTIC
      QIN(1,I)=0.0D0
      IF(EN.LE.0.0) GO TO 350
      EFAC=SQRT(1.0-(EIN(1)/EN))
      QIN(1,I)=AMP1*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      DO 310 J=2,NVIB1
      IF((EN+EIN(2)).LE.XVIB1(J)) GO TO 320
  310 CONTINUE
      J=NVIB1
  320 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(1,I)=QIN(1,I)+(EN+EIN(2))*(A*(EN+EIN(2))+B)/EN
      QIN(1,I)=QIN(1,I)*APOP/(1.0+APOP)*1.D-16
  350 CONTINUE
C
C  V11 + V3
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 400
      EFAC=SQRT(1.0-(EIN(2)/EN))
      QIN(2,I)=AMP1*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 360 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 370
  360 CONTINUE
      J=NVIB1
  370 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(2,I)=QIN(2,I)+(A*EN+B)
      QIN(2,I)=QIN(2,I)/(1.0+APOP)*1.D-16
  400 CONTINUE
C
C SUPERELASTIC
      QIN(3,I)=0.0D0
      IF(EN.LE.0.0) GO TO 4150
      EFAC=SQRT(1.0-(EIN(3)/EN))
      QIN(3,I)=AMP2*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      DO 4110 J=2,NVIB2
      IF((EN+EIN(4)).LE.XVIB2(J)) GO TO 4120
 4110 CONTINUE
      J=NVIB2
 4120 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(3,I)=QIN(3,I)+(EN+EIN(4))*(A*(EN+EIN(4))+B)/EN
      QIN(3,I)=QIN(3,I)*APOPH/(1.0+APOPH)*1.D-16
 4150 CONTINUE
C  V9 + V2 (SUM OF VIBRATIONS AT 179 AND 183 MV)
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 450
      EFAC=SQRT(1.0-(EIN(4)/EN))
      QIN(4,I)=AMP2*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 410 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(4,I)=QIN(4,I)+(A*EN+B)
      QIN(4,I)=QIN(4,I)/(1.0+APOPH)*1.D-16
  450 CONTINUE
C
C  2V3 (HARMONICS)
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 500
      DO 460 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 470
  460 CONTINUE
      J=NVIB3
  470 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(5,I)=(A*EN+B)*1.D-16
  500 CONTINUE
C
C  V1 + V8 + V12
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 550
      EFAC=SQRT(1.0-(EIN(6)/EN))
      QIN(6,I)=AMP3*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 510 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 520
  510 CONTINUE
      J=NVIB4
  520 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(6,I)=(QIN(6,I)+(A*EN+B))*1.D-16
  550 CONTINUE
C
C 2V1 (HARMONIC)
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 600
      DO 560 J=2,NVIB5
      IF(EN.LE.XVIB5(J)) GO TO 570
  560 CONTINUE
      J=NVIB5
  570 A=(YVIB5(J)-YVIB5(J-1))/(XVIB5(J)-XVIB5(J-1))
      B=(XVIB5(J-1)*YVIB5(J)-XVIB5(J)*YVIB5(J-1))/(XVIB5(J-1)-XVIB5(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  600 CONTINUE
C
C
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 850
      DO 810 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 820
  810 CONTINUE
      J=NEXC1
  820 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  850 CONTINUE
C
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 899
      DO 860 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 870
  860 CONTINUE
      J=NEXC2
  870 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(9,I)=(A*EN+B)*1.D-16
  899 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)+QIN(9,I)
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
