CDECK  ID>, GAS59.
      SUBROUTINE GAS59(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XELM(69),YELM(69),XVIBR(21),YVIBR(21),
     /XION(106),YION(106),XATT(10),YATT(10),XDIS1(28),YDIS1(28),
     /XEL(11),YEL(11)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XELM/0.00,.001,0.01,.012,.014,.017,0.02,.025,0.03,.035,
     /0.04,0.05,0.06,0.07,0.08,0.09,0.10,0.12,0.14,0.17,
     /0.20,0.25,0.30,0.35,0.40,0.45,0.50,0.60,0.70,0.80,
     /1.00,1.20,1.40,1.70,2.00,2.50,3.00,3.50,4.00,5.00,
     /6.00,7.00,8.00,9.00,10.0,12.0,15.0,20.0,30.0,40.0,
     /50.0,60.0,80.0,100.,150.,200.,300.,400.,500.,600.,
     /800.,1000.,2000.,4000.,6000.,8000.,10000.,20000.,100000./
C ELASTIC MOMENTUM TRANSFER X-SECTION
      DATA YELM/106.,106.,106.,105.,104.,102.,100.,95.0,90.0,86.0,
     /83.0,76.5,70.5,65.5,60.5,55.5,51.0,43.0,32.5,20.5,
     /13.0,6.00,4.00,3.20,3.20,3.30,3.60,4.30,5.00,6.50,
     /9.50,12.5,16.5,21.0,26.0,28.5,30.0,30.5,30.0,28.5,
     /26.5,24.5,22.5,20.5,18.5,16.0,13.0,8.50,4.70,3.40,
     /2.50,2.10,1.55,1.20,0.85,0.65,0.55,0.40,0.35,0.30,
     /0.25,0.20,0.11,0.07,0.04,0.03,.025,.013,.002/
C  ELASTIC X-SECTION (ONLY KNOWN IN LIMITED RANGE)
      DATA XEL/1.00,2.00,2.50,3.00,5.00,7.50,10.0,15.0,20.0,60.0,
     /100./
      DATA YEL/11.5,28.5,33.0,40.5,51.5,48.0,41.0,30.5,24.0,8.00,
     /6.40/
C VIBRATION RESONANCE SHAPE FUNCTION
C GAUSSIAN SHAPE FUNCTION AT 2.5 EV RESONANCE FWHM = 1.6 EV
C PLUS A HIGH ENERGY TAIL
      DATA XVIBR/0.35,0.70,1.00,1.30,1.60,1.90,2.20,2.50,3.00,3.50,
     /4.00,4.50,5.00,5.50,6.00,6.50,10.0,100.,1000.,10000.,
     /100000./
      DATA YVIBR/0.00,0.16,0.47,1.14,2.25,3.65,4.90,5.40,4.90,3.65,
     /2.25,1.14,0.47,0.16,0.04,0.02,.001,.0001,.00001,.000001,
     /.0000001/
C USE BEB VALUES FOR IONISATION
      DATA XION/11.33,11.5,12.0,12.5,13.0,13.5,14.0,14.5,15.0,15.5,
     /16.0,16.5,17.0,17.5,18.0,18.5,19.0,19.5,20.0,20.5,
     /21.0,21.5,22.0,22.5,23.0,23.5,24.0,26.0,28.0,30.0,
     /32.0,34.0,36.0,38.0,40.0,45.0,50.0,55.0,60.0,65.0,
     /70.0,75.0,80.0,85.0,90.0,95.0,100.,105.,110.,115.,
     /120.,125.,130.,135.,140.,145.,150.,160.,170.,180.,
     /190.,200.,210.,220.,230.,240.,250.,300.,350.,400.,
     /450.,500.,550.,600.,650.,700.,750.,800.,850.,900.,
     /950.,1000.,1100.,1200.,1300.,1400.,1500.,1600.,1700.,1800.,
     /1900.,2000.,2200.,2400.,2700.,3000.,4000.,5000.,6000.,8000.,
     /10000.,15000.,20000.,40000.,70000.,100000./
      DATA YION/0.00,.050,.202,.359,.518,.676,.832,.986,1.14,1.28,
     /1.42,1.56,1.69,1.82,1.94,2.06,2.17,2.28,2.38,2.49,
     /2.60,2.70,2.80,2.90,2.99,3.08,3.16,3.47,3.74,3.96,
     /4.16,4.32,4.46,4.58,4.68,4.88,5.04,5.15,5.22,5.26,
     /5.27,5.27,5.26,5.24,5.20,5.16,5.12,5.08,5.03,4.98,
     /4.92,4.87,4.82,4.76,4.71,4.66,4.60,4.50,4.40,4.31,
     /4.21,4.12,4.04,3.95,3.87,3.80,3.72,3.39,3.12,2.88,
     /2.69,2.52,2.37,2.23,2.12,2.01,1.92,1.84,1.76,1.69,
     /1.62,1.56,1.46,1.37,1.29,1.22,1.15,1.10,1.05,1.00,
     /.960,.922,.854,.797,.724,.665,.524,.435,.372,.291,
     /.240,.169,.131,.071,.043,.031/
C   ATTACHMENT X-SECTION ( NO DATA)
      DATA XATT/7.00,7.50,8.00,8.50,9.00,9.50,10.0,10.5,11.0,11.5/
      DATA YATT/10*0.0/
C  EXCITATION AND DISSOCIATION
      DATA XDIS1/7.50,8.50,10.0,12.5,15.0,20.0,25.0,30.0,40.0,60.0,
     /80.0,100.,150.,200.,300.,400.,500.,750.,1000.,1500.,
     /2000.,3000.,4000.,6000.,8000.,10000.,20000.,100000./
      DATA YDIS1/0.00,1.80,4.50,5.70,6.50,7.00,7.25,7.25,7.00,6.50,
     /6.00,5.50,4.80,4.00,3.00,2.50,2.00,1.40,1.00,0.80,
     /0.60,0.40,0.30,0.20,0.18,0.16,0.08,0.02/
C
C  ******************************************************************
C  ISOTROPIC FIT TO DRIFT DIFFUSION DATA IN ARGON / GEH4 MIXTURES OF
C   SOEJIMA AND NAKAMURA   J VAC SCI TECHNOL A 11 (1993) 1161-1164
C   OTHER ELECTRON SCATTERING REFERENCES :
C    DILLON ET AL    J.PHYS B 26(1993)3147
C    KARWASZ         J.PHYS B 28(1995)1301
C    MOZEJKO ET AL   J.PHYS.B 29(1996)L571
C  IONISATION X-SECTION FROM BEB THEORY OF KIM ET AL NIST WEB PAGE
C
C
C COMBINED EXCITATION AND DISSOCIATION X-SECTION FROM CONSISTENT
C SUM OF ELASTIC, VIBRATION ,IONISATION AND EXCITATION/DISSOCIATION
C TO GIVE EXPERIMENTAL VALUES OF THE TOTAL X-SECTION.
C
C  ANALYSIS SUMMARY :  DRIFT AND DIFFUSION DATA CONSTRAIN VIBRATIONAL
C  X-SECTION BUT ARE ONLY SENSITIVE TO THE ELASTIC X-SECTION BELOW
C  0.2EV .  VALUES OF THE ELASTIC X-SECTION ABOVE 1 EV ARE CONSTRAINED
C  BY THE ELECTRON SCATTERING MEASUREMENTS OF DILLON ET AL.
C THE ELASTIC X-SECTION IN THE RAMSAUER DIP REGION BETWEEN 0.2 AND
C 1.0 EV IS NOT CONSTRAINED BY THE DATA AND ONLY MEASUREMENTS OF DRIFT
C AND DIFFUSION IN PURE GERMANE WILL ALLOW A COMPLETE ANALYSIS
C
C --------------------------------------------------------------------
C**********************************************************************
      NAME='Germane (2005)'
C**********************************************************************
      NIN=6
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
C
      NDATA=69
      NVIBR=21
      NION=106
      NATT=10
      NDIS1=28
      E(1)=0.0
      E(2)=2.0*EMASS/(76.6418*AMU)
      E(3)=11.33
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
C OPAL AND BEATY ENERGY SPLITTING FACTOR
      EOBY=E(3)
      EIN(1)=-0.1016
      EIN(2)=0.1016
      EIN(3)=0.2611
      EIN(4)=0.35
      EIN(5)=0.50
      EIN(6)=7.5
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC        GERMANE       '
      SCRPT(3)=' IONISATION    ELOSS= 11.33   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB V2+V4     ELOSS= -0.1016 '
      SCRPT(8)=' VIB V2+V4     ELOSS=  0.1016 '
      SCRPT(9)=' VIB V1+V3     ELOSS=  0.2611 '
      SCRPT(10)=' VIB  HAR      ELOSS=  0.350  '
      SCRPT(11)=' VIB  HAR      ELOSS=  0.500  '
      SCRPT(12)=' EXC+DISSOC    ELOSS=  7.5    '
      APOP=DEXP(EIN(1)/AKT)
C
      EN=-ESTEP/2.0D0
      DO 1000 I=1,NSTEP
      EN=EN+ESTEP
C USE LOG INTERPOLATION FOR ELASTIC
      IF(EN.LE.XELM(2)) THEN
       QMOM=YELM(2)*1.D-16
       GO TO 30
      ENDIF
      DO 3 J=2,NDATA
      IF(EN.LE.XELM(J)) GO TO 4
   3  CONTINUE
      J=NDATA
   4  XNJ=DLOG(XELM(J))
      XNJ1=DLOG(XELM(J-1))
      YXJ=DLOG(YELM(J))
      YXJ1=DLOG(YELM(J-1))
      A=(YXJ-YXJ1)/(XNJ-XNJ1)
      B=(XNJ1*YXJ-XNJ*YXJ1)/(XNJ1-XNJ)
      QMOM=DEXP(A*DLOG(EN)+B)*1.D-16
   30 CONTINUE
      PEQEL(2,I)=0.5D0
      Q(2,I)=QMOM
C
C IONISATION
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
C
C ATTACHMENT
C
  200 Q(4,I)=0.0D0
      IF(EN.LT.XATT(1)) GO TO 300
      IF(EN.GT.XATT(10)) GO TO 300
      DO 210 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 220
  210 CONTINUE
      J=NATT
  220 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=(A*EN+B)*1.D-19
C
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
C V4 + V2 SUPERELASTIC
      QIN(1,I)=0.0D0
      IF(EN.LE.0.0) GO TO 350
      IF((EN+EIN(2)).LE.XVIBR(1)) GO TO 330
      DO 310 J=2,NVIBR
      IF((EN+EIN(2)).LE.XVIBR(J)) GO TO 320
  310 CONTINUE
      J=NVIBR
  320 A=(YVIBR(J)-YVIBR(J-1))/(XVIBR(J)-XVIBR(J-1))
      B=(XVIBR(J-1)*YVIBR(J)-XVIBR(J)*YVIBR(J-1))/(XVIBR(J-1)-XVIBR(J))
      QIN(1,I)=1.50*((EN+EIN(2))*(A*(EN+EIN(2))+B)/EN)
  330 CONTINUE
      EFAC=DSQRT(1.0-(EIN(1)/EN))
      QDIPOL=0.825*DLOG((EFAC+1.0)/(EFAC-1.0))/EN
      QIN(1,I)=QIN(1,I)+QDIPOL
      QIN(1,I)=QIN(1,I)*APOP/(1.0+APOP)*1.D-16
  350 CONTINUE
C
C V4 + V2
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 400
      IF(EN.LE.XVIBR(1)) GO TO 380
      DO 360 J=2,NVIBR
      IF(EN.LE.XVIBR(J)) GO TO 370
  360 CONTINUE
      J=NVIBR
  370 A=(YVIBR(J)-YVIBR(J-1))/(XVIBR(J)-XVIBR(J-1))
      B=(XVIBR(J-1)*YVIBR(J)-XVIBR(J)*YVIBR(J-1))/(XVIBR(J-1)-XVIBR(J))
      QIN(2,I)=1.50*(A*EN+B)
  380 CONTINUE
      EFAC=DSQRT(1.0-(EIN(2)/EN))
      QDIPOL=0.825*DLOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(2,I)=QIN(2,I)+QDIPOL
      QIN(2,I)=QIN(2,I)/(1.0+APOP)*1.D-16
  400 CONTINUE
C
C V1 + V3
      QIN(3,I)=0.0D0
      IF(EN.LE.EIN(3)) GO TO 500
      IF(EN.LE.XVIBR(1)) GO TO 430
      DO 410 J=2,NVIBR
      IF(EN.LE.XVIBR(J)) GO TO 420
  410 CONTINUE
      J=NVIBR
  420 A=(YVIBR(J)-YVIBR(J-1))/(XVIBR(J)-XVIBR(J-1))
      B=(XVIBR(J-1)*YVIBR(J)-XVIBR(J)*YVIBR(J-1))/(XVIBR(J-1)-XVIBR(J))
      QIN(3,I)=0.84*(A*EN+B)
  430 CONTINUE
      EFAC=DSQRT(1.0-(EIN(3)/EN))
      QDIPOL=0.530*DLOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(3,I)=(QDIPOL+QIN(3,I))*1.D-16
  500 CONTINUE
C
C  VIBRATION HARMONICS 1
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 600
      DO 510 J=2,NVIBR
      IF(EN.LE.XVIBR(J)) GO TO 520
  510 CONTINUE
      J=NVIBR
  520 A=(YVIBR(J)-YVIBR(J-1))/(XVIBR(J)-XVIBR(J-1))
      B=(XVIBR(J-1)*YVIBR(J)-XVIBR(J)*YVIBR(J-1))/(XVIBR(J-1)-XVIBR(J))
      QIN(4,I)=0.113*(A*EN+B)*1.D-16
  600 CONTINUE
C
C  VIBRATION HARMONICS 2
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 700
      DO 610 J=2,NVIBR
      IF(EN.LE.XVIBR(J)) GO TO 620
  610 CONTINUE
      J=NVIBR
  620 A=(YVIBR(J)-YVIBR(J-1))/(XVIBR(J)-XVIBR(J-1))
      B=(XVIBR(J-1)*YVIBR(J)-XVIBR(J)*YVIBR(J-1))/(XVIBR(J-1)-XVIBR(J))
      QIN(5,I)=0.074*(A*EN+B)*1.D-16
  700 CONTINUE
C
C  EXCITATION + DISSOCIATION
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
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)
C
 1000 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(6)) NIN=5
      IF(EFINAL.LE.EIN(5)) NIN=4
      IF(EFINAL.LE.EIN(4)) NIN=3
      IF(EFINAL.LE.EIN(3)) NIN=2
      IF(EFINAL.LE.EIN(2)) NIN=1
C
      RETURN
      END