CDECK  ID>, GAS55.
      SUBROUTINE GAS55(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEL(25),YEL(25),XVIB1(16),YVIB1(16),XVIB2(16),YVIB2(16),
     /XVIB3(15),YVIB3(15),XVIB4(15),YVIB4(15),XEXC(18),YEXC(18),
     /XION(66),YION(66),XATT(26),YATT(26)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C ELASTIC + EFFECTIVE ROTATION
      DATA XEL/0.00,.001,0.01,0.10,0.40,0.60,0.80,1.00,1.20,1.50,
     /1.75,2.00,2.30,3.00,5.00,7.00,10.0,15.0,20.0,30.0,
     /50.0,100.,1000.,10000.,100000./
      DATA YEL/1900.,1900.,1600.,160.,40.0,23.5,13.0,8.10,7.50,9.00,
     /15.0,18.9,21.0,17.0,21.8,21.8,12.8,8.30,5.20,3.30,
     /1.75,0.75,0.07,.007,.0007/
C VIBRATION
      DATA XVIB1/.1466,0.80,1.00,1.50,2.00,2.30,2.70,3.00,5.00,7.00,
     /10.0,20.0,100.,1000.,10000.,100000./
      DATA YVIB1/0.00,.0001,0.02,0.22,0.55,0.60,0.60,0.56,0.33,0.20,
     /0.10,.05,.001,.0001,.00001,.000001/
      DATA XVIB2/.3242,0.80,1.00,1.50,2.00,2.30,2.70,3.00,5.00,7.00,
     /10.0,20.0,100.,1000.,10000.,100000./
      DATA YVIB2/0.00,.0001,0.10,1.20,2.30,2.15,1.50,1.20,0.30,0.10,
     /0.05,.025,.001,.0001,.00001,.000001/
      DATA XVIB3/.4708,0.90,1.00,1.50,2.00,2.30,2.70,3.00,5.00,7.00,
     /10.0,100.,1000.,10000.,100000./
      DATA YVIB3/0.00,.001,.006,0.24,0.42,0.50,0.42,0.25,0.05,0.01,
     /.001,.0001,.00001,.000001,.0000001/
      DATA XVIB4/.6484,0.90,1.00,1.50,2.00,2.30,2.70,3.00,5.00,7.00,
     /10.0,100.,1000.,10000.,100000./
      DATA YVIB4/0.00,.001,0.01,1.15,1.40,1.45,1.40,0.80,0.16,0.03,
     /.001,.0001,.00001,.000001,.0000001/
      DATA XION/10.48,11.0,11.5,12.0,12.5,13.0,13.5,14.0,14.5,15.0,
     /16.0,17.0,18.0,19.0,20.0,22.0,24.0,26.0,28.0,30.0,
     /32.0,34.0,36.0,38.0,40.0,45.0,50.0,55.0,60.0,65.0,
     /70.0,80.0,90.0,100.,110.,120.,130.,150.,170.,200.,
     /250.,300.,350.,400.,450.,500.,550.,600.,650.,700.,
     /750.,800.,900.,1000.,1200.,1500.,2000.,3000.,4000.,7000.,
     /10000.,14000.,20000.,40000.,60000.,100000./
      DATA YION/0.00,.088,.175,.262,.349,.435,.518,.621,.724,.825,
     /1.02,1.22,1.42,1.62,1.79,2.11,2.38,2.62,2.82,3.01,
     /3.17,3.30,3.42,3.52,3.61,3.77,3.88,3.94,3.98,3.99,
     /3.99,3.95,3.88,3.80,3.71,3.61,3.52,3.33,3.16,2.93,
     /2.61,2.36,2.15,1.98,1.83,1.71,1.60,1.51,1.42,1.35,
     /1.28,1.22,1.12,1.04,.903,.758,.603,.433,.341,.212,
     /.155,.116,.085,.046,.032,.020/
      DATA XATT/1.50,1.75,2.00,2.25,2.50,2.75,3.00,3.25,4.75,5.00,
     /5.25,5.50,5.65,6.00,6.25,6.50,7.00,7.50,8.00,8.50,
     /9.00,9.50,10.0,10.5,11.0,11.5/
      DATA YATT/0.00,.0023,.0088,.019,.018,.012,.003,.0001,.0001,.0007,
     /.005,.010,.010,.0048,.0012,.0001,.0001,.0018,.0024,.0018,
     /.0018,.003,.0033,.001,.0001,.0000001/
      DATA XEXC/7.85,8.00,9.00,10.0,12.0,15.0,20.0,25.0,30.0,40.0,
     /60.0,100.,150.,200.,300.,1000.,10000.,100000./
      DATA YEXC/0.00,0.40,2.00,3.00,3.80,4.20,4.60,4.60,4.20,3.80,
     /2.90,2.30,1.70,1.40,1.15,0.55,.055,.0055/
C------------------------------------------------------------------
      NAME='H2S (2003)'
C --------------------------------------------------------------------
C   NO DRIFT VELOCITY AVAILABLE IN PURE H2S USED DATA OF MIXTURE IN C2H4
C   FROM HURST ET AL. AND DIFFUSION FROM MILLICAN AND WALKER.
C   ELASTIC AND VIBRATION XSECTIONS FROM ELECTRON SCATTERING BY :
C   GULLEY ET AL AND ROHR . ATTACHMENT FROM AZRIA ET AL.
C   IONISATION : BEB X-SECTIONS OF KIM
C    NB. DT OF MILLICAN AND WALKER AFFECTED BY ATTACHMENT ABOVE 40 TD.
C ---------------------------------------------------------------------
      NIN=9
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NEL=25
      NVIB1=16
      NVIB2=16
      NVIB3=15
      NVIB4=15
      NION=66
      NATT=26
      NEXC=18
      AMP1=0.1875
      AMP2=0.1725
      AMPVIB1=0.075
      AMPVIB2=0.375
      E(1)=0.0
      E(2)=2.0*EMASS/(34.08088*AMU)
      E(3)=10.48
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=10.48
      EIN(1)=-0.025
      EIN(2)=0.025
      EIN(3)=-0.075
      EIN(4)=0.075
      EIN(5)=0.1466
      EIN(6)=0.3242
      EIN(7)=0.4708
      EIN(8)=0.6484
      EIN(9)=7.85
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       H2S            '
      SCRPT(3)=' IONISATION    ELOSS= 10.48   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' ROT R1        ELOSS= -0.025  '
      SCRPT(8)=' ROT R1        ELOSS=  0.025  '
      SCRPT(9)=' ROT R2        EL0SS= -0.075  '
      SCRPT(10)=' ROT R2        ELOSS=  0.075  '
      SCRPT(11)=' VIB V2        ELOSS=  0.1466 '
      SCRPT(12)=' VIB V13       ELOSS=  0.3242 '
      SCRPT(13)=' (V13+V2)+HIGH ELOSS=  0.4708 '
      SCRPT(14)=' 2V13+HIGH     ELOSS=  0.6484 '
      SCRPT(15)=' EXC           ELOSS=  7.85   '
      APOP1=EXP(EIN(1)/AKT)
      APOP2=EXP(EIN(3)/AKT)
      EN=-ESTEP/2.0D0
      DO 900 I=1,NSTEP
      EN=EN+ESTEP
      Q(2,I)=0.0D0
      IF(EN.LE.XEL(2)) THEN
       Q(2,I)=YEL(2)*1.D-16
       GO TO 30
      ENDIF
      DO 10 J=2,NEL
      IF(EN.LE.XEL(J)) GO TO 20
   10 CONTINUE
      J=NEL
   20 Y1=LOG(YEL(J))
      Y2=LOG(YEL(J-1))
      X1=LOG(XEL(J))
      X2=LOG(XEL(J-1))
      A=(Y1-Y2)/(X1-X2)
      B=(X2*Y1-X1*Y2)/(X2-X1)
      Q(2,I)=EXP(A*LOG(EN)+B)*1.0D-16
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
  200 CONTINUE
C
      Q(4,I)=0.0D0
      IF(EN.LT.XATT(1).OR.EN.GT.XATT(NATT)) GO TO 300
      DO 210 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 220
  210 CONTINUE
      J=NATT
  220 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=(A*EN+B)*1.D-16
  300 CONTINUE
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
C  SUPERELASTIC  ROT1
C
      QIN(1,I)=0.0D0
      IF(EN.LE.0.0) GO TO 1300
      EFAC=SQRT(1.0-(EIN(1)/EN))
      QIN(1,I)=AMP1*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(1,I)=QIN(1,I)*APOP1/(1.0+APOP1)*1.D-16
C  ROT1
 1300 QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 1400
      EFAC=SQRT(1.0-(EIN(2)/EN))
      QIN(2,I)=AMP1*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(2,I)=QIN(2,I)/(1.0+APOP1)*1.D-16
C
C  SUPERELASTIC ROT2
C
 1400 QIN(3,I)=0.0D0
      IF(EN.LE.0.0) GO TO 1500
      EFAC=SQRT(1.0-(EIN(3)/EN))
      QIN(3,I)=AMP2*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(3,I)=QIN(3,I)*APOP2/(1.0+APOP2)*1.D-16
C  ROT2
 1500 QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 1600
      EFAC=SQRT(1.0-(EIN(4)/EN))
      QIN(4,I)=AMP2*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(4,I)=QIN(4,I)/(1.0+APOP2)*1.D-16
C  VIB V2
 1600 QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 400
      EFAC=SQRT(1.0-(EIN(5)/EN))
      QIN(5,I)=AMPVIB1*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 310 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 320
  310 CONTINUE
      J=NVIB1
  320 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(5,I)=(QIN(5,I)+(A*EN+B))*1.D-16
  400 CONTINUE
C  VIB V13 COMPOSITE
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 500
      EFAC=SQRT(1.0-(EIN(6)/EN))
      QIN(6,I)=AMPVIB2*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      DO 410 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(6,I)=(QIN(6,I)+(A*EN+B))*1.D-16
  500 CONTINUE
C  VIB V2+V13 AND HIGHER SERIES
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 600
      DO 510 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 520
  510 CONTINUE
      J=NVIB3
  520 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  600 CONTINUE
C  VIB 2V13 AND HIGHER SERIES
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 700
      DO 610 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 620
  610 CONTINUE
      J=NVIB4
  620 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  700 CONTINUE
C  SINGLE EFFECTIVE EXCITATION LEVEL
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 800
      DO 710 J=2,NEXC
      IF(EN.LE.XEXC(J)) GO TO 720
  710 CONTINUE
      J=NEXC
  720 A=(YEXC(J)-YEXC(J-1))/(XEXC(J)-XEXC(J-1))
      B=(XEXC(J-1)*YEXC(J)-XEXC(J)*YEXC(J-1))/(XEXC(J-1)-XEXC(J))
      QIN(9,I)=(A*EN+B)*1.D-16
  800 CONTINUE
C---------------------------------------------------------------------
C   GET ELASTIC FROM ELASTIC + ROTATION  X-SECTION
      Q(2,I)=Q(2,I)-QIN(1,I)-QIN(2,I)-QIN(3,I)-QIN(4,I)
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(7,I)+QIN(8,I)+
     /QIN(9,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+QIN(5,I)+QIN(6,I)
  900 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(9)) NIN=8
      IF(EFINAL.LE.EIN(8)) NIN=7
      IF(EFINAL.LE.EIN(7)) NIN=6
      IF(EFINAL.LE.EIN(6)) NIN=5
      IF(EFINAL.LE.EIN(5)) NIN=4
C
      END
