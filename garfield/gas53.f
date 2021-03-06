CDECK  ID>, GAS53.
      SUBROUTINE GAS53(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(30),YXSEC(30),XION(104),YION(104),
     /XVIB1(18),YVIB1(18),XVIB2(18),YVIB2(18),XVIB3(16),YVIB3(16),
     /XVIB4(16),YVIB4(16),XVIB5(16),YVIB5(16),
     /XEXC1(15),YEXC1(15),XEXC2(14),YEXC2(14),XEXC3(15),YEXC3(15),
     /XATT(25),YAT1(25),YAT2(25)
      CHARACTER*15 NAME
      CHARACTER*30 SCRPT(226)
      DATA XEN/1.D-6,.001,0.01,0.10,0.20,0.40,0.70,1.00,2.00,3.00,
     /4.00,5.00,6.00,7.00,8.00,10.0,15.0,20.0,30.0,50.0,
     /100.,200.,300.,600.,1000.,2000.,3000.,6000.,10000.,100000./
      DATA YXSEC/3000.,1500.,150.,19.0,14.0,12.0,12.0,12.0,12.0,12.0,
     /12.0,12.0,12.0,12.0,12.0,12.0,11.5,11.0,10.0,7.40,
     /4.70,1.99,1.16,0.41,.185,.058,.028,.0081,.0031,.0003/
C  VIBRATION V2  BEND MODE
C  RESONANCE ONLY , DIPOLE ANALYTICAL
      DATA XVIB1/.0869,0.50,1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.00,
     /8.00,9.00,10.0,12.0,15.0,100.,1000.,100000./
      DATA YVIB1/0.00,.0005,.001,.005,.010,0.12,.265,0.24,0.13,0.07,
     /0.03,0.02,0.01,.001,.0001,.00001,.000001,.0000001/
C  VIBRATION SUM OF V1 AND V3 STRETCH MODES
C  RESONANCE ONLY , DIPOLE ANALYTICAL
      DATA XVIB2/.1292,0.50,1.00,1.50,2.00,3.00,4.00,5.00,6.00,7.00,
     /8.00,9.00,10.0,12.0,14.0,100.,1000.,100000./
      DATA YVIB2/0.00,0.01,0.015,0.02,0.05,0.43,0.83,0.74,0.56,0.44,
     /0.25,0.14,0.05,0.01,.001,.0001,.00001,.000001/
C VIBRATION HARMONIC (V12 AND V23 )
      DATA XVIB3/0.2161,1.00,2.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,
     /10.0,12.0,20.0,100.,1000.,100000./
      DATA YVIB3/0.00,.001,0.01,.077,.165,.140,.068,.034,.022,.020,
     /.020,.010,.005,.001,.0001,.000001/
C VIBRATION HARMONIC (V13 2V1 AND 2V3)
      DATA XVIB4/0.2660,1.00,2.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,
     /10.0,12.0,20.0,100.,1000.,100000./
      DATA YVIB4/0.00,.003,0.04,.140,.278,.271,.238,.210,.140,.075,
     /.029,.010,.005,.001,.0001,.000001/
C VIBRATION HARMONIC ( SUM OF HIGHER HARMONICS )
      DATA XVIB5/0.38,1.00,2.00,3.00,4.00,5.00,6.00,7.00,8.00,9.00,
     /10.0,12.0,20.0,100.,1000.,100000./
      DATA YVIB5/0.00,.001,0.02,.070,.139,.135,.119,.105,.070,.038,
     /.014,.005,.002,.001,.0001,.000001/
      DATA XION/12.75,13.0,13.5,14.0,14.5,15.0,15.5,16.0,16.5,17.0,
     /17.5,18.0,18.5,19.0,19.5,20.0,20.5,21.0,21.5,22.0,
     /22.5,23.0,23.5,24.0,26.0,28.0,30.0,32.0,34.0,36.0,
     /38.0,40.0,45.0,50.0,55.0,60.0,65.0,70.0,75.0,80.0,
     /85.0,90.0,95.0,100.,105.,110.,115.,120.,125.,130.,
     /135.,140.,145.,150.,160.,170.,180.,190.,200.,210.,
     /220.,230.,240.,250.,300.,350.,400.,450.,500.,550.,
     /600.,650.,700.,750.,800.,850.,900.,950.,1000.,1100.,
     /1250.,1500.,1750.,2000.,2250.,2500.,2750.,3000.,3250.,3500.,
     /3750.,4000.,4500.,5000.,6000.,7000.,8000.,9000.,10000.,15000.,
     /20000.,30000.,50000.,100000./
      DATA YION/0.00,.011,.035,.059,.083,.118,.162,.216,.270,.324,
     /.378,.431,.484,.535,.586,.636,.685,.732,.779,.824,
     /.869,.921,.975,1.03,1.25,1.46,1.66,1.84,2.01,2.17,
     /2.31,2.44,2.72,2.95,3.14,3.30,3.42,3.52,3.60,3.66,
     /3.71,3.74,3.77,3.78,3.79,3.80,3.80,3.79,3.78,3.77,
     /3.76,3.74,3.72,3.70,3.66,3.61,3.56,3.51,3.46,3.41,
     /3.35,3.30,3.25,3.20,2.97,2.76,2.57,2.41,2.27,2.14,
     /2.03,1.93,1.84,1.76,1.68,1.62,1.55,1.50,1.44,1.35,
     /1.23,1.07,.950,.856,.780,.717,.664,.619,.580,.546,
     /.516,.489,.443,.406,.348,.305,.272,.246,.225,.158,
     /.123,.086,.054,.029/
C  DISOCIATIVE ATTACHMENT :  O3 + E- = O2 + O-
      DATA XATT/0.00,0.20,0.40,0.60,0.80,1.00,1.20,1.40,1.60,1.80,
     /2.00,2.40,2.60,3.00,3.50,4.00,5.00,6.00,7.00,7.50,
     /8.00,9.00,10.0,20.0,100000./
      DATA YAT1/0.00,.032,.070,0.14,0.22,0.29,0.36,0.37,0.36,0.26,
     /0.21,0.12,0.10,0.09,0.08,0.06,0.02,0.02,0.05,0.07,
     /0.05,0.01,.005,.001,.00000001/
C DISOCIATIVE ATTACHMENT :  O3 + E- = O + O2-
      DATA YAT2/0.00,0.00,0.00,0.01,0.08,0.13,0.17,0.15,0.11,.055,
     /.025,.006,.005,.002,.002,.0015,.001,.002,.003,.003,
     /.0025,.001,.0005,.0001,.00000001/
C  CHAPPUIS BAND
      DATA XEXC1/1.50,2.00,3.00,4.00,5.00,6.00,8.00,10.0,12.0,14.0,
     /20.0,40.0,100.,1000.,100000./
      DATA YEXC1/0.00,0.01,0.04,0.12,0.12,0.10,0.08,0.06,0.05,0.04,
     /0.02,0.01,.004,.0004,.000004/
C   HARTLEY BAND
      DATA XEXC2/4.85,6.00,7.00,8.00,9.00,10.0,12.0,14.0,16.0,20.0,
     /40.0,100.,1000.,100000./
      DATA YEXC2/0.00,0.26,0.63,0.75,0.68,0.65,0.58,0.47,0.37,0.27,
     /0.13,0.05,.005,.000005/
C    SUM OF OTHER STATES HIGHER THAN 9.0 EV
      DATA XEXC3/9.00,10.0,12.0,14.0,16.0,18.0,20.0,40.0,100.,150.,
     /200.,400.,1000.,10000.,100000./
      DATA YEXC3/0.00,0.52,1.50,1.50,1.40,1.40,1.20,1.00,0.80,0.70,
     /0.60,0.30,0.12,.012,.0012/
C ---------------------------------------------------------------------
C  OZONE
C      USED BEB (THEORETICAL VALUES) FOR IONIZATION X-SECTION
C      VIB.RESONANCES:  ALLAN ET AL      J. PHYS. B   29(1996)4727
C      ATTACHMENT:      RANGWALA ET AL   J. PHYS. B   32(1999)3795
C      EXCITATION:      ALLEN ET AL      J.CHEM.PHYS.105(1996)5665
C      ELASTIC   :      SHYN AND SWEENEY  PHYS REV  47A (1993)2919
C                       GULLEY ET AL     J. PHYS. B  31 (1998)5197
C                       PABLOS ET AL     J. PHYS. B  35 (2002)865
C      GOOD FIT TO EXPERIMENTAL ATTACHMENT RATE MEASUREMENTS :
C              STELMAN,MORUZZI AND PHELPS  J.CHEM.PHYS 56(1972)4183
C      N.B. ATTACHMENT RATE MEASUREMENTS OF PHELPS NEED TO BE CORRECTED
C           FOR DETACHMENT COLLISIONS . CORRECTION FACTOR TAKEN FROM
C           KLOPOVSKII ET AL   PLASMA PHYSICS REPORTS 23(1997) 165-171
C ---------------------------------------------------------------------
C
      NAME='O3 (2002)'
C
      NIN=11
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=30
      NVIB1=18
      NVIB2=18
      NVIB3=16
      NVIB4=16
      NVIB5=16
      NION=104
      NATT=25
      NEXC1=15
      NEXC2=14
      NEXC3=15
      E(1)=0.0
      E(2)=2.0*EMASS/(47.9982*AMU)
      E(3)=12.75
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
C SET OPAL AND BEATY ENERGY SPLITTING TO EION
      EOBY=E(3)
      EIN(1)=-0.005
      EIN(2)=0.005
      EIN(3)=-0.0869
      EIN(4)=0.0869
      EIN(5)=0.1292
      EIN(6)=0.2161
      EIN(7)=0.2660
      EIN(8)=0.380
      EIN(9)=1.50
      EIN(10)=4.85
      EIN(11)=9.00
      APOPR=EXP(EIN(1)/AKT)
      APOPV=EXP(EIN(3)/AKT)
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC        OZONE         '
      SCRPT(3)=' IONISATION    ELOSS= 12.75   '
      SCRPT(4)=' DISOCIATIVE ATTACHMENT       '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' ROT           ELOSS= -0.005  '
      SCRPT(8)=' ROT           ELOSS=  0.005  '
      SCRPT(9)=' VIB2  BEND    ELOSS= -0.0869 '
      SCRPT(10)=' VIB2 BEND     ELOSS=  0.0869 '
      SCRPT(11)=' VIB3+VIB1     ELOSS=  0.1292 '
      SCRPT(12)=' V12+V23       ELOSS=  0.2161 '
      SCRPT(13)=' V13+2V1+2V3   ELOSS=  0.2660 '
      SCRPT(14)=' SUM HIGH VIB  ELOSS=  0.380  '
      SCRPT(15)=' EXC CHAPPUIS  ELOSS=  1.50   '
      SCRPT(16)=' EXC HARTLEY   ELOSS=  4.85   '
      SCRPT(17)=' EXC           ELOSS=  9.00   '
      EN=-ESTEP/2.0D0
      DO 9000 I=1,NSTEP
      EN=EN+ESTEP
      IF(EN.EQ.0.0) Q(2,I)=3000.D-16
      IF(EN.EQ.0.0) GO TO 30
      DO 10 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 20
   10 CONTINUE
      J=NDATA
C USE LOG INTERPOLATION
   20 Y1=LOG(YXSEC(J-1))
      Y2=LOG(YXSEC(J))
      X1=LOG(XEN(J-1))
      X2=LOG(XEN(J))
      A=(Y2-Y1)/(X2-X1)
      B=(X1*Y2-X2*Y1)/(X1-X2)
      Q(2,I)=EXP((A*LOG(EN)+B))*1.0D-16
C
   30 Q(3,I)=0.0D0
      IF(EN.LT.E(3)) GO TO 200
      DO 110 J=2,NION
      IF(EN.LE.XION(J)) GO TO 120
  110 CONTINUE
      J=NION
  120 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
C   SUM OF DISOCIATIVE ATTACHMENTS TO O- AND O2-
  200 Q(4,I)=0.0D0
      IF(EN.LT.XATT(1)) GO TO 300
      IF(EN.GT.XATT(NATT)) GO TO 300
      DO 210 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 220
  210 CONTINUE
      J=NATT
  220 A1=(YAT1(J)-YAT1(J-1))/(XATT(J)-XATT(J-1))
      B1=(XATT(J-1)*YAT1(J)-XATT(J)*YAT1(J-1))/(XATT(J-1)-XATT(J))
      A2=(YAT2(J)-YAT2(J-1))/(XATT(J)-XATT(J-1))
      B2=(XATT(J-1)*YAT2(J)-XATT(J)*YAT2(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=((A1+A2)*EN+B1+B2)*1.D-16
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
C SUPERELASTIC EFFECTIVE ROTATION
C
      QIN(1,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 305
      EFAC=SQRT(1.0-(EIN(1)/EN))
      QIN(1,I)=0.450*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(1,I)=QIN(1,I)*APOPR/(1.0+APOPR)*1.D-16
C
C EFFECTIVE ROTATION
  305 CONTINUE
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 350
      EFAC=SQRT(1.0-(EIN(2)/EN))
      QIN(2,I)=0.450*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(2,I)=QIN(2,I)*1.0/(1.0+APOPR)*1.D-16
  350 CONTINUE
C
C SUPERELASTIC  VIBRATION V2 (BEND MODE)
C
      QIN(3,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 365
      EFAC=SQRT(1.0-(EIN(3)/EN))
      QIN(3,I)=0.0133*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(3,I)=QIN(3,I)*APOPV/(1.0+APOPV)*1.D-16
C
C VIBRATION V2 (BEND MODE)
  365 CONTINUE
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 400
      DO 370 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 380
  370 CONTINUE
      J=NVIB1
  380 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      EFAC=SQRT(1.0-(EIN(4)/EN))
      QIN(4,I)=0.0133*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(4,I)=(A*EN+B)+QIN(4,I)
      QIN(4,I)=QIN(4,I)*1.0/(1.0+APOPV)*1.D-16
  400 CONTINUE
C
C  V1 + V3   ( STRETCH MODES )
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 500
      DO 410 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      EFAC=SQRT(1.0-(EIN(5)/EN))
      QIN(5,I)=0.090*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(5,I)=((A*EN+B)+QIN(5,I))*1.D-16
  500 CONTINUE
C
C V12 +V23
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 600
      DO 510 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 520
  510 CONTINUE
      J=NVIB3
  520 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(6,I)=(A*EN+B)*1.D-16
  600 CONTINUE
C
C  V13+2V1+2V3
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 700
      DO 610 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 620
  610 CONTINUE
      J=NVIB4
  620 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  700 CONTINUE
C
C  HIGHER HARMONICS
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 800
      DO 710 J=2,NVIB5
      IF(EN.LE.XVIB5(J)) GO TO 720
  710 CONTINUE
      J=NVIB5
  720 A=(YVIB5(J)-YVIB5(J-1))/(XVIB5(J)-XVIB5(J-1))
      B=(XVIB5(J-1)*YVIB5(J)-XVIB5(J)*YVIB5(J-1))/(XVIB5(J-1)-XVIB5(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  800 CONTINUE
C
C   EXCITATION  CHAPPUIS BAND
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 900
      DO 810 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 820
  810 CONTINUE
      J=NEXC1
  820 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(9,I)=(A*EN+B)*1.D-16
  900 CONTINUE
C
C   EXCITATION  HARTLEY BAND
      QIN(10,I)=0.0D0
      IF(EN.LE.EIN(10)) GO TO 1000
      DO 910 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 920
  910 CONTINUE
      J=NEXC2
  920 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(10,I)=(A*EN+B)*1.D-16
 1000 CONTINUE
C
C   EXCITATION
      QIN(11,I)=0.0D0
      IF(EN.LE.EIN(11)) GO TO 1100
      DO 1010 J=2,NEXC3
      IF(EN.LE.XEXC3(J)) GO TO 1020
 1010 CONTINUE
      J=NEXC3
 1020 A=(YEXC3(J)-YEXC3(J-1))/(XEXC3(J)-XEXC3(J-1))
      B=(XEXC3(J-1)*YEXC3(J)-XEXC3(J)*YEXC3(J-1))/(XEXC3(J-1)-XEXC3(J))
      QIN(11,I)=(A*EN+B)*1.D-16
 1100 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)+QIN(9,I)+QIN(10,I)+QIN(11,I)
 9000 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(11)) NIN=10
      IF(EFINAL.LE.EIN(10)) NIN=9
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
