CDECK  ID>, GAS39.
      SUBROUTINE GAS39(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(30),YXSEC(30),XVIB1(39),YVIB1(39),
     /XVIB2(34),YVIB2(34),XEXC(18),YEXC(18),XION(69),YION(69),
     /XATT(30),YATT(30),XAT1(9),YAT1(9)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C  ELASTIC
      DATA XEN/0.00,0.01,0.02,0.04,0.07,0.10,0.15,0.20,0.30,0.50,
     /0.80,1.00,1.20,1.50,1.80,2.20,3.00,3.50,5.00,8.00,
     /10.0,12.0,15.0,20.0,30.0,40.0,100.,1000.,10000.,100000./
      DATA YXSEC/99.0,90.0,80.0,58.0,45.0,36.5,28.5,23.0,16.0,9.82,
     /7.62,8.61,11.6,14.3,20.0,23.8,27.6,28.1,26.4,28.1,
     /29.2,29.2,26.4,17.1,9.90,6.50,2.70,0.27,.027,.0027/
C  VIBRATION V2 (010) BENDING
      DATA XVIB1/.0490,0.05,.055,0.06,0.07,0.08,0.09,0.10,0.12,0.14,
     /0.17,0.20,0.25,0.30,0.35,0.40,0.50,0.60,0.80,1.00,
     /1.20,1.40,1.70,2.00,2.50,3.00,3.50,4.00,5.00,6.00,
     /8.00,10.0,15.0,20.0,40.0,100.,1000.,10000.,100000./
      DATA YVIB1/0.00,2.40,10.0,18.5,21.0,22.0,21.0,19.5,14.0,10.0,
     /7.00,5.00,3.20,2.10,1.50,1.20,0.90,0.78,0.60,0.50,
     /0.43,0.39,0.33,0.29,0.25,0.22,0.25,0.32,0.40,0.45,
     /0.20,0.10,.075,.052,.032,.013,.0013,.00013,.000013/
C  VIBRATION V1  (100) SYMMETRIC STRETCH
      DATA XVIB2/.0810,0.09,0.10,0.11,0.12,0.13,0.15,0.17,0.20,0.25,
     /0.30,0.35,0.40,0.50,0.60,0.80,1.00,1.20,1.40,1.70,
     /2.00,2.50,3.00,3.50,4.00,5.00,6.00,8.00,10.0,20.0,
     /100.,1000.,10000.,100000./
      DATA YVIB2/0.00,2.50,6.00,7.50,8.00,7.50,5.80,4.20,2.50,1.50,
     /1.05,0.74,0.58,0.40,0.29,0.16,0.12,0.10,0.10,.125,
     /.165,0.27,0.43,0.51,0.49,0.20,0.12,0.07,.057,.033,
     /.008,.0008,.00008,.000008/
C VIBRATION V3  (001) ASYMMETRIC STRETCH : USED DIPOLE EXCITATION FUNC.
C
C IONISATION
      DATA XION/10.07,10.5,11.0,11.5,12.0,12.5,13.0,13.5,14.0,14.5,
     /15.0,15.5,16.0,16.5,17.0,18.0,19.0,20.0,21.0,22.0,
     /23.0,24.0,26.0,28.0,30.0,32.0,34.0,36.0,38.0,40.0,
     /45.0,50.0,55.0,60.0,65.0,70.0,75.0,80.0,90.0,100.,
     /120.,140.,160.,180.,200.,220.,240.,250.,300.,350.,
     /400.,450.,500.,550.,600.,650.,700.,800.,900.,1000.,
     /1500.,2000.,2500.,3000.,5000.,10000.,20000.,40000.,100000./
      DATA YION/0.00,.192,.421,.652,.880,1.10,1.32,1.53,1.72,1.92,
     /2.20,2.48,2.74,3.03,3.31,3.84,4.34,4.83,5.28,5.69,
     /6.06,6.40,6.99,7.48,7.90,8.26,8.58,8.84,9.05,9.23,
     /9.53,9.69,9.75,9.74,9.68,9.59,9.47,9.34,9.06,8.76,
     /8.17,7.63,7.14,6.71,6.33,5.99,5.69,5.55,4.96,4.48,
     /4.10,3.79,3.52,3.29,3.09,2.92,2.76,2.50,2.29,2.12,
     /1.56,1.28,1.10,0.94,0.66,0.42,0.24,.134,.069/
C ATTACHMENT CS2 -  (PROBABLY 3 BODY MORMALISED AT 40 TORR)
      DATA XAT1/.0001,.001,0.01,.017,.025,0.03,.035,0.04,10.0/
      DATA YAT1/35.0,35.0,28.0,20.0,10.0,5.00,1.50,.00001,.0000001/
C DISOCIATIVE ATTACHMENT    UNITS OF 10**-19
      DATA XATT/2.41,2.50,2.60,2.70,2.80,3.00,3.20,3.35,3.60,3.70,
     /3.80,4.00,4.20,4.40,5.40,5.50,5.75,6.00,6.25,6.50,
     /6.75,7.00,7.50,7.75,8.00,8.25,8.50,10.0,100.,100000./
      DATA YATT/0.00,0.01,0.02,0.04,0.08,0.40,2.00,3.70,3.00,3.10,
     /2.70,1.50,0.40,0.01,0.01,0.10,0.50,1.45,1.80,0.90,
     /0.30,0.20,0.30,0.90,0.50,0.10,0.01,0.01,.001,.0001/
C  EXCITATION
      DATA XEXC/6.20,7.00,8.00,9.00,10.0,11.0,12.0,14.0,17.0,20.0,
     /30.0,40.0,60.0,80.0,100.,1000.,10000.,100000./
      DATA YEXC/0.00,0.60,1.50,3.30,5.20,7.00,8.00,8.80,9.20,8.90,
     /8.00,7.40,6.30,5.50,5.00,0.50,0.05,.005/
C ----------------------------------------------------------------
C LACK OF ELECTRON DRIFT DATA .
C USED SOHNS ELECTRON SCATTERING DATA AND UNPUBLISHED DATA BY ALLEN.
C  THE ATTACHMENT IS PROBABLY 3 BODY EXCEPT FOR THE DISOCIATIVE
C   ATTACHMENT.
C  THE 3-BODY X-SECTION  CORRESPONDS TO 40 TORR PRESSURE
C ---------------------------------------------------------------
      NAME='CS2 (2001)'
C
      NIN=6
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=30
      NVIB1=39
      NVIB2=34
      NION=69
      NATT=30
      NAT1=9
      NEXC=18
      E(1)=0.0
      E(2)=2.0*EMASS/(76.1427*AMU)
      E(3)=10.07
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=10.07
      EIN(1)=-0.049
      EIN(2)=0.049
      EIN(3)=-0.081
      EIN(4)=0.081
      EIN(5)=0.190
      EIN(6)=6.20
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       CS2            '
      SCRPT(3)=' IONISATION    ELOSS= 10.07   '
      SCRPT(4)=' ATTACHMENT  (ASSUMED 2 BODY) '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB V2        ELOSS= -0.049  '
      SCRPT(8)=' VIB V2        ELOSS=  0.049  '
      SCRPT(9)=' VIB V1        ELOSS= -0.081  '
      SCRPT(10)=' VIB V1        ELOSS=  0.081  '
      SCRPT(11)=' VIB V3        ELOSS=  0.190  '
      SCRPT(12)=' EXC           ELOSS=  6.20   '
      APOPV2=EXP(EIN(1)/AKT)
      APOPV1=EXP(EIN(3)/AKT)
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
   30 Q(3,I)=0.0D0
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
      IF(EN.LT.XAT1(1)) GO TO 250
      IF(EN.GT.XAT1(NAT1)) GO TO 250
      DO 210 J=2,NAT1
      IF(EN.LE.XAT1(J)) GO TO 220
  210 CONTINUE
      J=NAT1
  220 A=(YAT1(J)-YAT1(J-1))/(XAT1(J)-XAT1(J-1))
      B=(XAT1(J-1)*YAT1(J)-XAT1(J)*YAT1(J-1))/(XAT1(J-1)-XAT1(J))
      Q(4,I)=(A*EN+B)*1.D-16*1.3
  250 CONTINUE
      IF(EN.LT.XATT(1)) GO TO 300
      IF(EN.GT.XATT(NATT)) GO TO 300
      DO 260 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 270
  260 CONTINUE
      J=NATT
  270 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=Q(4,I)+(A*EN+B)*1.D-19
  300 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C			
C SUPERELASTIC V2 BENDING MODE
C
      QIN(1,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 340
      DO 310 J=2,NVIB1
      IF((EN+EIN(2)).LE.XVIB1(J)) GO TO 320
  310 CONTINUE
      J=NVIB1
  320 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(1,I)=(EN+EIN(2))*(A*(EN+EIN(2))+B)*1.D-16/EN
      QIN(1,I)=QIN(1,I)*APOPV2/(1.0+APOPV2)
  340 CONTINUE
C
C VIBRATION V2 BENDING MODE
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 400
      DO 350 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 360
  350 CONTINUE
      J=NVIB1
  360 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(2,I)=(A*EN+B)*1.D-16
      QIN(2,I)=QIN(2,I)/(1.0+APOPV2)
  400 CONTINUE
C
C SUPERELASTIC OF V1 SYMMETRIC STRETCH VIBRATION
C
      QIN(3,I)=0.0D0
      IF(EN.EQ.0.0) GO TO 440
      DO 410 J=2,NVIB2
      IF((EN+EIN(4)).LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(3,I)=(EN+EIN(4))*(A*(EN+EIN(4))+B)*1.D-16/EN
      QIN(3,I)=QIN(3,I)*APOPV1/(1.0+APOPV1)
  440 CONTINUE
C
C VIBRATION V3 SYMMETRIC STRETCH
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 500
      DO 450 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 460
  450 CONTINUE
      J=NVIB2
  460 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(4,I)=(A*EN+B)*1.D-16
      QIN(4,I)=QIN(4,I)/(1.0+APOPV1)
  500 CONTINUE
C
C  VIBRATION V3 ASYMMETRIC STRETCH
      QIN(5,I)=0.0D0
      IF(EN.LE.EIN(5)) GO TO 600
      EFAC=SQRT(1.0-(EIN(5)/EN))
      QIN(5,I)=0.710*LOG((1.0+EFAC)/(1.0-EFAC))/EN
      QIN(5,I)=QIN(5,I)*1.D-16
  600 CONTINUE
C
C EXCITATION (DISOCIATION)
      QIN(6,I)=0.0D0
      IF(EN.LE.EIN(6)) GO TO 900
      DO 810 J=2,NEXC
      IF(EN.LE.XEXC(J)) GO TO 820
  810 CONTINUE
      J=NEXC
  820 A=(YEXC(J)-YEXC(J-1))/(XEXC(J)-XEXC(J-1))
      B=(XEXC(J-1)*YEXC(J)-XEXC(J)*YEXC(J-1))/(XEXC(J-1)-XEXC(J))
      QIN(6,I)=(A*EN+B)*1.D-16
  900 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)+
     /QIN(5,I)+QIN(6,I)
 9000 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(6)) NIN=5
      IF(EFINAL.LE.EIN(5)) NIN=4
      END
