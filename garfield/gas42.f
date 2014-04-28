CDECK  ID>, GAS42.
      SUBROUTINE GAS42(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(62),YXSEC(62),XVIBH(15),YVIBH(15),
     /XVIB1(15),YVIB1(15),XVIB3(15),YVIB3(15),XEXC(34),YEXC(34),
     /XION(71),YION(71),XATT(33),YATT(33)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C
      DATA XEN/0.0,.001,0.01,0.02,0.03,0.04,0.05,0.06,0.07,0.08,
     /0.09,0.10,0.12,0.14,0.16,0.18,0.20,0.24,0.30,0.35,
     /0.40,0.50,0.60,0.70,0.80,1.00,1.50,2.00,2.50,3.00,
     /3.50,4.00,5.00,6.00,7.00,8.00,10.0,12.0,14.0,17.0,
     /20.0,24.0,28.0,32.0,36.0,40.0,45.0,50.0,60.0,75.0,
     /100.,150.,200.,300.,500.,700.,1000.,2000.,4000.,10000.,
     /20000.,100000./
C
      DATA YXSEC/100.,80.0,50.3,43.0,39.0,35.5,33.0,31.0,29.4,27.8,
     /26.8,25.5,23.5,21.0,18.5,16.0,14.0,10.5,7.20,5.65,
     /4.25,3.15,2.70,2.70,3.30,4.30,6.20,7.80,9.30,10.4,
     /11.1,11.3,11.3,10.9,10.5,10.0,9.00,8.50,8.00,7.50,
     /7.20,6.80,6.50,6.40,6.30,6.20,6.00,5.75,5.05,4.50,
     /3.75,2.70,1.75,1.00,0.57,0.38,0.24,0.11,0.05,0.02,
     /0.01,.002/
C
C  VIBRATION V1
      DATA XVIB1/0.110,1.00,1.50,2.00,2.50,3.00,3.50,4.00,5.00,6.00,
     /10.0,100.,1000.,10000.,100000./
      DATA YVIB1/0.0,.00001,0.20,0.48,0.72,0.80,0.72,0.48,0.32,0.12,
     /.0016,.001,.0001,.00001,.000001/
C  VIBRATION V3
      DATA XVIB3/0.180,1.00,1.50,2.00,2.50,3.00,3.50,4.00,5.00,6.00,
     /10.0,100.,1000.,10000.,100000./
      DATA YVIB3/0.0,.00001,0.40,0.96,1.44,1.60,1.44,0.96,0.64,0.24,
     /.0032,.001,.0001,.00001,.000001/
C  VIBRATION HARMONIC (2V1+2V3 AND HIGHER HARMONICS)
      DATA XVIBH/0.360,1.00,1.50,2.00,2.50,3.00,3.50,4.00,5.00,6.00,
     /10.0,100.,1000.,10000.,100000./
      DATA YVIBH/0.0,.00001,0.21,0.54,0.78,0.90,0.78,0.54,0.36,0.18,
     /.0024,.001,.0001,.00001,.000001/
C
      DATA XION/15.56,16.5,17.0,17.5,18.0,18.5,19.0,19.5,20.0,20.5,
     /21.0,22.0,23.0,24.0,26.0,28.0,30.0,32.0,34.0,36.0,
     /38.0,40.0,45.0,50.0,55.0,60.0,65.0,70.0,75.0,80.0,
     /85.0,90.0,95.0,100.,105.,110.,115.,120.,125.,130.,
     /135.,140.,150.,160.,170.,180.,200.,220.,250.,300.,
     /350.,400.,450.,500.,600.,700.,800.,900.,1000.,1200.,
     /1400.,2000.,2500.,3000.,4000.,5000.,6000.,8000.,10000.,20000.,
     /100000./
      DATA YION/0.0,0.045,.064,.079,.130,.183,.236,.295,.356,.419,
     /.493,.645,0.80,0.96,1.26,1.54,1.80,2.03,2.25,2.45,
     /2.63,2.79,3.15,3.48,3.76,3.99,4.19,4.35,4.48,4.58,
     /4.67,4.74,4.80,4.84,4.88,4.90,4.92,4.93,4.93,4.93,
     /4.93,4.92,4.89,4.86,4.81,4.76,4.66,4.54,4.37,4.08,
     /3.83,3.59,3.38,3.20,2.88,2.62,2.41,2.23,2.07,1.85,
     /1.66,1.37,1.15,1.02,0.82,0.67,0.58,0.45,0.36,0.21,
     /.06/
C  ATTACHMENT
      DATA XATT/10.0,10.4,10.5,10.6,10.7,10.8,10.9,11.0,11.1,11.2,
     /11.3,11.4,11.5,11.6,11.7,11.8,11.9,12.0,12.1,12.2,
     /12.3,12.4,12.5,12.6,12.7,12.8,12.9,13.0,20.0,100.,
     /1000.,10000.,100000./
      DATA YATT/0.00,.0015,.0032,.0046,.0063,.0084,.010,.014,.017,.020,
     /.022,.024,.025,.025,.023,.021,.018,.015,.012,.0097,
     /.0069,.0048,.0033,.0022,.0015,.00092,.00061,.00024,.0002,.0001,
     /.00001,.000001,.0000001/
C  DISOCIATION X-SECTION EXCLUDING DISOCIATIVE IONISATION X-SECTION
      DATA XEXC/10.0,10.1,10.6,11.1,11.6,12.1,12.6,13.1,13.6,14.1,
     /14.6,15.1,16.2,17.2,18.2,20.2,22.2,24.2,27.2,30.3,
     /40.0,50.0,100.,200.,300.,400.,500.,600.,1000.,2000.,
     /4000.,10000.,20000.,100000./
      DATA YEXC/0.00,0.01,0.11,0.21,0.39,0.58,0.65,0.73,0.82,0.89,
     /0.97,1.03,1.15,1.24,1.33,1.49,1.61,1.68,1.78,1.82,
     /1.81,1.83,1.88,1.88,1.70,1.40,1.10,0.88,0.49,0.22,
     /0.11,0.05,0.03,.008/
C ----------------------------------------------------------------
C ---------------------------------------------------------------
      NAME='BF3 (2001)'
C
      NIN=9
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
C ANISOTROPIC SCATTERING FOR LEVELS 6 AND 7
      KIN(6)=1
      KIN(7)=1
C
      NDATA=62
      NVIB1=15
      NVIB3=15
      NVIBH=15
      NION=71
      NATT=33
      NEXC=34
      E(1)=0.0
      E(2)=2.0*EMASS/(67.8062*AMU)
      E(3)=15.56
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=15.56
      EIN(1)=-0.0596
      EIN(2)=-0.086
      EIN(3)=-0.110
      EIN(4)=0.0596
      EIN(5)=0.086
      EIN(6)=0.110
      EIN(7)=0.180
      EIN(8)=0.360
      EIN(9)=10.0
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       BF3            '
      SCRPT(3)=' IONISATION    ELOSS= 15.56   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB V4        ELOSS= -0.0596 '
      SCRPT(8)=' VIB V2        ELOSS= -0.086  '
      SCRPT(9)=' VIB V1        ELOSS= -0.110  '
      SCRPT(10)=' VIB V4        ELOSS=  0.0596 '
      SCRPT(11)=' VIB V2        ELOSS=  0.086  '
      SCRPT(12)=' VIB V1 (ANIS) ELOSS=  0.110  '
      SCRPT(13)=' VIB V3 (ANIS) ELOSS=  0.180  '
      SCRPT(14)=' VIB HAR       ELOSS=  0.360  '
      SCRPT(15)=' EXC           ELOSS= 10.0    '
      APOP1=EXP(EIN(1)/AKT)
      APOP2=EXP(EIN(2)/AKT)
      APOP3=EXP(EIN(3)/AKT)
      EN=-ESTEP/2.0D0
      DO 9000 I=1,NSTEP
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
      Q(4,I)=(A*EN+B)*1.D-16
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
C SUPERELASTIC OF VIBRATION V4
C
      QIN(1,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 305
      EFAC=SQRT(1.0-(EIN(1)/EN))
      QIN(1,I)=0.018*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(1,I)=QIN(1,I)*APOP1/(1.0+APOP1)*1.D-16
  305 CONTINUE
C
C SUPERELASTIC OF VIBRATION V2
      QIN(2,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 400
      EFAC=SQRT(1.0-(EIN(2)/EN))
      QIN(2,I)=0.045*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(2,I)=QIN(2,I)*APOP2/(1.0+APOP2)*1.D-16
  400 CONTINUE
C SUPERELASTIC OF VIBRATION V1
      QIN(3,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 500
      EFAC=SQRT(1.0-(EIN(3)/EN))
      QIN(3,I)=0.37*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(3,I)=QIN(3,I)*APOP3/(1.0+APOP3)*1.D-16
  500 CONTINUE
C   V4
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 600
      EFAC=SQRT(1.0-(EIN(4)/EN))
      QIN(4,I)=0.018*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(4,I)=QIN(4,I)*1.0/(1.0+APOP1)*1.D-16
  600 CONTINUE
C   V2
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 700
      EFAC=SQRT(1.0-(EIN(5)/EN))
      QIN(5,I)=0.045*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(5,I)=QIN(5,I)*1.0/(1.0+APOP2)*1.D-16
  700 CONTINUE
C   V1
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 800
      DO 710 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 720
  710 CONTINUE
      J=NVIB1
  720 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      EFAC=SQRT(1.0-(EIN(6)/EN))
      QIN(6,I)=0.37*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      ELF=EN-EIN(6)
      FWD=LOG((EN+ELF)/(EN+ELF-2.0*SQRT(EN*ELF)))
      BCK=LOG((EN+ELF+2.0*SQRT(EN*ELF))/(EN+ELF))
C ASSUME RATIO MOM T./ TOT X-SECT FOR RESONANCE PART = RAT4
      RAT4=0.58
      XMT=((1.5-FWD/(FWD+BCK))*QIN(6,I)+RAT4*(A*EN+B))*1.D-16
      QIN(6,I)=(QIN(6,I)+(A*EN+B))*1.D-16
      PEQIN(6,I)=0.5+(QIN(6,I)-XMT)/QIN(6,I)
      QIN(6,I)=QIN(6,I)*1.0/(1.0+APOP3)
  800 CONTINUE
C  V3
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 900
      DO 810 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 820
  810 CONTINUE
      J=NVIB3
  820 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      EFAC=SQRT(1.0-(EIN(7)/EN))
      QIN(7,I)=0.74*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      ELF=EN-EIN(7)
      FWD=LOG((EN+ELF)/(EN+ELF-2.0*SQRT(EN*ELF)))
      BCK=LOG((EN+ELF+2.0*SQRT(EN*ELF))/(EN+ELF))
C ASSUME RATIO MOM T./ TOT X-SECT FOR RESONANCE PART = RAT4
      RAT4=0.58
      XMT=((1.5-FWD/(FWD+BCK))*QIN(7,I)+RAT4*(A*EN+B))*1.D-16
      QIN(7,I)=(QIN(7,I)+(A*EN+B))*1.D-16
      PEQIN(7,I)=0.5+(QIN(7,I)-XMT)/QIN(7,I)
  900 CONTINUE
C
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 1000
      DO 910 J=2,NVIBH
      IF(EN.LE.XVIBH(J)) GO TO 920
  910 CONTINUE
      J=NVIBH
  920 A=(YVIBH(J)-YVIBH(J-1))/(XVIBH(J)-XVIBH(J-1))
      B=(XVIBH(J-1)*YVIBH(J)-XVIBH(J)*YVIBH(J-1))/(XVIBH(J-1)-XVIBH(J))
      QIN(8,I)=(A*EN+B)*1.D-16
 1000 CONTINUE
C
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 1100
      DO 1010 J=2,NEXC
      IF(EN.LE.XEXC(J)) GO TO 1020
 1010 CONTINUE
      J=NEXC
 1020 A=(YEXC(J)-YEXC(J-1))/(XEXC(J)-XEXC(J-1))
      B=(XEXC(J-1)*YEXC(J)-XEXC(J)*YEXC(J-1))/(XEXC(J-1)-XEXC(J))
      QIN(9,I)=(A*EN+B)*1.D-16
 1100 CONTINUE
C
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)+QIN(9,I)
 9000 CONTINUE
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
      END
