CDECK  ID>, GAS8.
      SUBROUTINE GAS8(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(73),YXSEC(73),XVIB1(24),YVIB1(24),XVIB2(22),YVIB2(22
     /),XION(82),YION(82),XATT(14),YATT(14),XDIS1(32),YDIS1(32),
     /XDIS2(32),YDIS2(32),XDIS3(32),YDIS3(32),XDIS4(32),YDIS4(32),
     /YELAT(73),XVIB3(19),YVIB3(19),XVIB4(19),YVIB4(19)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XEN/0.00,.0001,.001,.004,.007,0.01,.012,.014,.017,0.02,
     /.025,0.03,.035,0.04,0.05,0.06,0.07,0.08,0.09,0.10,
     /0.12,0.14,0.17,0.20,0.24,0.28,0.32,0.36,0.40,0.45,
     /0.50,0.60,0.70,0.80,1.00,1.20,1.40,1.70,2.00,2.50,
     /3.00,3.50,4.00,5.00,6.00,7.00,8.00,9.00,10.0,12.0,
     /15.0,20.0,30.0,40.0,50.0,60.0,80.0,100.,150.,200.,
     /300.,400.,500.,600.,800.,1000.,2000.,4000.,6000.,8000.,
     /10000.,20000.,100000./
C ELASTIC MOMENTUM TRANSFER X-SECTION
      DATA YXSEC/26.7,25.4,22.7,18.9,16.6,14.9,14.0,13.1,12.1,11.1,
     /9.80,8.71,7.78,6.98,5.68,4.70,3.85,3.31,2.75,2.32,
     /1.72,1.23,0.78,.500,.330,.315,.340,.375,.430,.500,
     /.600,.810,1.05,1.29,1.80,2.15,2.55,3.25,4.05,5.80,
     /7.90,10.1,11.7,14.5,16.3,17.2,17.6,17.6,17.0,15.0,
     /13.0,8.50,4.70,3.40,2.50,2.10,1.55,1.20,0.66,0.44,
     /0.25,0.16,0.12,0.09,0.06,.045,.016,.006,.003,.002,
     /.001,.00025,.000015/
C ELASTIC TOTAL X-SECTION
      DATA YELAT/26.7,25.6,23.3,19.9,17.9,16.4,15.5,14.8,13.8,12.9,
     /11.6,10.6,9.67,8.89,7.60,6.57,5.60,4.90,4.20,3.70,
     /2.80,2.20,1.62,1.23,0.95,0.82,0.75,0.72,0.71,0.73,
     /0.77,0.95,1.10,1.28,1.72,2.25,3.00,4.00,5.00,7.32,
     /8.81,11.0,13.3,17.6,21.2,23.2,24.0,23.8,23.1,21.4,
     /19.7,15.6,11.2,8.55,7.20,6.09,4.74,3.89,2.55,2.00,
     /1.35,1.05,0.78,0.65,0.50,0.42,0.23,0.14,0.10,0.08,
     /.064,.030,.007/
      DATA XVIB1/.1625,0.20,0.30,0.40,0.50,0.60,0.80,1.00,2.00,3.00,
     /5.00,6.00,7.00,8.00,9.00,10.0,12.5,15.0,20.0,50.0,
     /100.0,1000.,10000.,100000./
      DATA YVIB1/0.00,.0001,.056,.067,.072,.075,.078,.079,.119,.152,
     /0.50,0.70,0.80,0.75,0.65,0.55,0.39,0.33,0.19,.077,
     /.044,0.004,.0004,.00004/
      DATA XVIB2/.3743,0.40,0.50,0.60,0.80,1.00,2.00,3.00,5.00,6.00,
     /7.00,8.00,9.00,10.0,12.5,15.0,20.0,50.0,100.,1000.,
     /10000.,100000./
      DATA YVIB2/0.00,.006,.011,.012,.013,.014,.033,.090,0.50,0.70,
     /0.80,0.75,0.65,0.50,0.25,0.19,0.10,0.04,0.02,0.01,
     /.001,.0001/
      DATA XVIB3/.544,1.00,2.00,3.00,5.00,6.00,7.00,8.00,9.00,10.0,
     /12.5,15.0,17.5,20.0,50.0,100.,1000.,10000.,100000./
      DATA YVIB3/0.00,.001,.005,.027,.095,.125,.135,.135,.110,.080,
     /.055,.037,.028,.020,.008,.003,.0003,.00003,.000003/
      DATA XVIB4/.736,1.00,2.00,3.00,5.00,6.00,7.00,8.00,9.00,10.0,
     /12.5,15.0,17.5,20.0,50.0,100.,1000.,10000.,100000./
      DATA YVIB4/0.00,.0008,.004,.024,.080,.105,.115,.115,.095,.070,
     /.045,.027,.018,.010,.007,.003,.0003,.00003,.000003/
      DATA XION/12.99,13.5,14.0,14.5,15.0,15.5,16.0,16.5,17.0,17.5,
     /18.0,18.5,19.0,19.5,21.0,21.5,22.0,22.5,23.0,23.5,
     /24.0,26.0,28.0,30.0,32.0,34.0,36.0,38.0,40.0,45.0,
     /50.0,55.0,60.0,65.0,70.0,75.0,80.0,85.0,90.0,95.0,
     /100.,105.,110.,115.,120.,125.,130.,135.,140.,145.,
     /150.,160.,180.,200.,250.,300.,350.,400.,450.,500.,
     /550.,600.,650.,700.,750.,800.,850.,900.,950.,1000.,
     /1500.,2000.,3000.,4000.,5000.,7000.,10000.,12000.,15000.,20000.,
     /40000.,100000./
      DATA YION/0.00,.034,.074,0.13,.198,.278,.361,.445,.530,.610,
     /.706,.793,.880,.977,1.24,1.34,1.42,1.50,1.57,1.65,
     /1.72,1.97,2.20,2.38,2.54,2.68,2.79,2.91,3.02,3.21,
     /3.36,3.47,3.56,3.62,3.66,3.68,3.69,3.70,3.69,3.68,
     /3.66,3.63,3.62,3.59,3.55,3.52,3.48,3.45,3.41,3.38,
     /3.33,3.25,3.11,3.01,2.72,2.49,2.27,2.09,1.94,1.83,
     /1.72,1.63,1.54,1.47,1.40,1.34,1.28,1.24,1.20,1.18,
     /0.82,0.66,0.47,0.37,0.31,.235,.175,.151,.127,0.10,
     /.058,.028/
      DATA XATT/7.00,7.50,8.00,8.50,9.00,9.50,10.0,10.5,11.0,11.5,
     /12.0,12.5,13.0,13.5/
      DATA YATT/0.00,0.005,0.12,0.51,0.75,0.85,0.96,0.91,0.72,0.49,
     /0.27,0.13,0.06,0.00/
      DATA XDIS1/9.00,10.0,13.0,15.0,17.0,20.0,22.0,25.0,30.0,40.0,
     /50.0,60.0,70.0,80.0,100.,150.,200.,300.,400.,500.,
     /600.,800.,1000.,1500.,2000.,3000.,4000.,6000.,8000.,10000.,
     /20000.,100000./
      DATA YDIS1/0.00,0.27,0.27,0.36,0.45,0.53,0.58,0.59,0.58,0.57,
     /0.56,0.55,0.54,0.53,0.52,0.50,0.46,0.40,0.33,0.28,
     /0.24,0.18,0.14,.098,.075,.055,.040,.029,.022,.018,
     /.009,.0018/
      DATA XDIS2/10.0,11.0,13.0,15.0,17.0,20.0,22.0,25.0,30.0,40.0,
     /50.0,60.0,70.0,80.0,100.,150.,200.,300.,400.,500.,
     /600.,800.,1000.,1500.,2000.,3000.,4000.,6000.,8000.,10000.,
     /20000.,100000./
      DATA YDIS2/0.00,0.27,0.27,0.36,0.45,0.53,0.58,0.59,0.58,0.57,
     /0.56,0.55,0.54,0.53,0.52,0.50,0.46,0.40,0.33,0.28,
     /0.24,0.18,0.14,.098,.075,.055,.040,.029,.022,.018,
     /.009,.0018/
      DATA XDIS3/11.0,12.0,13.0,15.0,17.0,20.0,22.0,25.0,30.0,40.0,
     /50.0,60.0,70.0,80.0,100.,150.,200.,300.,400.,500.,
     /600.,800.,1000.,1500.,2000.,3000.,4000.,6000.,8000.,10000.,
     /20000.,100000./
      DATA YDIS3/0.00,0.27,0.27,0.36,0.45,0.53,0.58,0.59,0.58,0.57,
     /0.56,0.55,0.54,0.53,0.52,0.50,0.46,0.40,0.33,0.28,
     /0.24,0.18,0.14,.098,.075,.055,.040,.029,.022,.018,
     /.009,.0018/
      DATA XDIS4/11.8,12.0,13.0,15.0,17.0,20.0,22.0,25.0,30.0,40.0,
     /50.0,60.0,70.0,80.0,100.,150.,200.,300.,400.,500.,
     /600.,800.,1000.,1500.,2000.,3000.,4000.,6000.,8000.,10000.,
     /20000.,100000./
      DATA YDIS4/0.00,.045,0.27,0.36,0.45,0.53,0.58,0.59,0.58,0.57,
     /0.56,0.55,0.54,0.53,0.52,0.50,0.46,0.40,0.33,0.28,
     /0.24,0.18,0.14,.098,.075,.055,.040,.029,.022,.018,
     /.009,.0018/
      NAME='CH4 (2004)'
      NIN=9
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
C USE ANISOTROPIC SCATTERING FOR ELASTIC AND COPY (OFFSET) TO IONISATION
      KEL(2)=1
      KEL(3)=1
C USE ANISOTROPIC SCATTERING FOR LEVEL 2 AND 3
      KIN(2)=1
      KIN(3)=1
C
      RAT=0.8
      NDATA=73
      NVIB1=24
      NVIB2=22
      NVIB3=19
      NVIB4=19
      NION=82
      NATT=14
      NDIS1=32
      NDIS2=32
      NDIS3=32
      NDIS4=32
      E(1)=0.0
      E(2)=2.0*EMASS/(16.0426*AMU)
      E(3)=12.99
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=7.3
      IOFF=INT(0.5+E(3)/ESTEP)
      EIN(1)=-0.1625
      EIN(2)=0.1625
      EIN(3)=0.3743
      EIN(4)=0.544
      EIN(5)=0.736
      EIN(6)=9.0
      EIN(7)=10.0
      EIN(8)=11.0
      EIN(9)=11.8
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC (ANIS) METHANE       '
      SCRPT(3)=' IONISATION    ELOSS= 12.99   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB V2+V4     ELOSS= -0.1625 '
      SCRPT(8)=' VIB V2+V4     ELOSS=  0.1625 '
      SCRPT(9)=' VIB V1+V3     ELOSS=  0.3743 '
      SCRPT(10)=' VIB  HAR      ELOSS=  0.544  '
      SCRPT(11)=' VIB  HAR      ELOSS=  0.736  '
      SCRPT(12)=' EXC DISOCIATN ELOSS=  9.0    '
      SCRPT(13)=' EXC DISOCIATN ELOSS= 10.0    '
      SCRPT(14)=' EXC DISOCIATN ELOSS= 11.0    '
      SCRPT(15)=' EXC DISOCIATN ELOSS= 11.8    '
      APOP=EXP(EIN(1)/AKT)
      EN=-ESTEP/2.0D0
      DO 1000 I=1,NSTEP
      EN=EN+ESTEP
C USE LOG INTERPOLATION FOR ELASTIC
      IF(EN.LE.XEN(2)) THEN
       QELA=26.7D-16
       QMOM=26.7D-16
       GO TO 30
      ENDIF
      DO 3 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 4
   3  CONTINUE
      J=NDATA
   4  YXJ=LOG(YELAT(J))
      YXJ1=LOG(YELAT(J-1))
      XNJ=LOG(XEN(J))
      XNJ1=LOG(XEN(J-1))
      A=(YXJ-YXJ1)/(XNJ-XNJ1)
      B=(XNJ1*YXJ-XNJ*YXJ1)/(XNJ1-XNJ)
      QELA=EXP(A*LOG(EN)+B)*1.D-16
      YXJ=LOG(YXSEC(J))
      YXJ1=LOG(YXSEC(J-1))
      A=(YXJ-YXJ1)/(XNJ-XNJ1)
      B=(XNJ1*YXJ-XNJ*YXJ1)/(XNJ1-XNJ)
      QMOM=EXP(A*LOG(EN)+B)*1.D-16
   30 CONTINUE
      PEQEL(2,I)=0.5+(QELA-QMOM)/QELA
      Q(2,I)=QELA
C
      Q(3,I)=0.0D0
      PEQEL(3,I)=0.5D0
      IF(EN.LT.E(3)) GO TO 200
      DO 110 J=2,NION
      IF(EN.LE.XION(J)) GO TO 120
  110 CONTINUE
      J=NION
  120 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
C   USE ANISOTROPIC SCATTERING FOR PRIMARY IONISATION ELECTRON FOR
C ENERGIES ABOVE 2 * IONISATION ENERGY
C  ANISOTROPIC DISTRIBUTION SAME AS ELASTIC AT ENERGY OFFSET BY
C  IONISATION ENERGY
      IF(EN.LE.(2.0*E(3))) GO TO 200
      PEQEL(3,I)=PEQEL(2,(I-IOFF))
C
  200 Q(4,I)=0.0D0
      IF(EN.LT.XATT(1)) GO TO 300
      IF(EN.GT.XATT(14)) GO TO 300
      DO 210 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 220
  210 CONTINUE
      J=NATT
  220 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=(A*EN+B)*1.D-19
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C V4 + V2 SUPERELASTIC
      QIN(1,I)=0.0D0
      IF(EN.LE.0.0) GO TO 350
      DO 310 J=2,NVIB1
      IF((EN+EIN(2)).LE.XVIB1(J)) GO TO 320
  310 CONTINUE
      J=NVIB1
  320 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      EFAC=SQRT(1.0-(EIN(1)/EN))
      QIN(1,I)=0.091*LOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(1,I)=QIN(1,I)+(EN+EIN(2))*(A*(EN+EIN(2))+B)/EN
      QIN(1,I)=QIN(1,I)*APOP/(1.0+APOP)*1.D-16
  350 CONTINUE
C V4 + V2
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 400
      DO 360 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 370
  360 CONTINUE
      J=NVIB1
  370 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      EFAC=SQRT(1.0-(EIN(2)/EN))
      QIN(2,I)=0.091*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      ELF=EN-EIN(2)
      FWD=LOG((EN+ELF)/(EN+ELF-2.0*SQRT(EN*ELF)))
      BCK=LOG((EN+ELF+2.0*SQRT(EN*ELF))/(EN+ELF))
C RATIO OF MT TO TOTAL X-SECT FOR RESONANCE PART =RAT
      XMT=((1.5-FWD/(FWD+BCK))*QIN(2,I)+RAT*(A*EN+B))*1.D-16
      QIN(2,I)=((A*EN+B)+QIN(2,I))*1.D-16
      PEQIN(2,I)=0.5+(QIN(2,I)-XMT)/QIN(2,I)
      QIN(2,I)=QIN(2,I)/(1.0+APOP)
  400 CONTINUE
C V1 + V3
      QIN(3,I)=0.0D0
      IF(EN.LE.EIN(3)) GO TO 500
      DO 410 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      EFAC=SQRT(1.0-(EIN(3)/EN))
      QIN(3,I)=0.082*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      ELF=EN-EIN(3)
      FWD=LOG((EN+ELF)/(EN+ELF-2.0*SQRT(EN*ELF)))
      BCK=LOG((EN+ELF+2.0*SQRT(EN*ELF))/(EN+ELF))
C RATIO OF MT TO TOTAL X-SECT FOR RESONANCE PART =RAT
      XMT=((1.5-FWD/(FWD+BCK))*QIN(3,I)+RAT*(A*EN+B))*1.D-16
      QIN(3,I)=((A*EN+B)+QIN(3,I))*1.D-16
      PEQIN(3,I)=0.5+(QIN(3,I)-XMT)/QIN(3,I)
  500 CONTINUE
C  VIBRATION HARMONICS 1
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
C  VIBRATION HARMONICS 2
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
C  DISOCIATIVE EXCITATION
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 850
      DO 810 J=2,NDIS1
      IF(EN.LE.XDIS1(J)) GO TO 820
  810 CONTINUE
      J=NDIS1
  820 A=(YDIS1(J)-YDIS1(J-1))/(XDIS1(J)-XDIS1(J-1))
      B=(XDIS1(J-1)*YDIS1(J)-XDIS1(J)*YDIS1(J-1))/(XDIS1(J-1)-XDIS1(J))
      QIN(6,I)=(A*EN+B)*1.D-16
  850 CONTINUE
C  DISOCIATIVE EXCITATION
      QIN(7,I)=0.0D0
      IF(EN.LE.EIN(7)) GO TO 900
      DO 860 J=2,NDIS2
      IF(EN.LE.XDIS2(J)) GO TO 870
  860 CONTINUE
      J=NDIS2
  870 A=(YDIS2(J)-YDIS2(J-1))/(XDIS2(J)-XDIS2(J-1))
      B=(XDIS2(J-1)*YDIS2(J)-XDIS2(J)*YDIS2(J-1))/(XDIS2(J-1)-XDIS2(J))
      QIN(7,I)=(A*EN+B)*1.D-16
  900 CONTINUE
C    DISOCIATIVE EXCITATION
      QIN(8,I)=0.0D0
      IF(EN.LE.EIN(8)) GO TO 950
      DO 910 J=2,NDIS3
      IF(EN.LE.XDIS3(J)) GO TO 920
  910 CONTINUE
      J=NDIS3
  920 A=(YDIS3(J)-YDIS3(J-1))/(XDIS3(J)-XDIS3(J-1))
      B=(XDIS3(J-1)*YDIS3(J)-XDIS3(J)*YDIS3(J-1))/(XDIS3(J-1)-XDIS3(J))
      QIN(8,I)=(A*EN+B)*1.D-16
  950 CONTINUE
C    DISOCIATIVE EXCITATION
      QIN(9,I)=0.0D0
      IF(EN.LE.EIN(9)) GO TO 990
      DO 960 J=2,NDIS4
      IF(EN.LE.XDIS4(J)) GO TO 970
  960 CONTINUE
      J=NDIS4
  970 A=(YDIS4(J)-YDIS4(J-1))/(XDIS4(J)-XDIS4(J-1))
      B=(XDIS4(J-1)*YDIS4(J)-XDIS4(J)*YDIS4(J-1))/(XDIS4(J-1)-XDIS4(J))
      QIN(9,I)=(A*EN+B)*1.D-16
  990 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)+QIN(7,I)+QIN(8,I)+QIN(9,I)
C
 1000 CONTINUE
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
