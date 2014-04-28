CDECK  ID>, GAS30.
      SUBROUTINE GAS30(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C ----------------------------------------------------------------
C   SF6 FILE FROM ITOH ET AL J.PHYS.D.  26 (1993) 1975-1979
C ---------------------------------------------------------------
      NAME='SF6 (93 Itoh)'
C
      NIN=2
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      E(1)=0.0
      E(2)=2.0D0*EMASS/(146.05642*AMU)
      E(3)=15.8
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=15.8
      EIN(1)=0.095
      EIN(2)=9.80
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       SF6            '
      SCRPT(3)=' IONISATION    ELOSS= 15.8    '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' VIB           ELOSS=  0.095  '
      SCRPT(8)=' EXC           ELOSS=  9.80   '
      EN=-ESTEP/2.0D0
      DO 9000 I=1,NSTEP
      EN=EN+ESTEP
      IF(EN.EQ.0.0D0) THEN
       BTA=-5.0
       GO TO 3
      ENDIF
      BTA=LOG10(EN)
   3  BTA2=BTA*BTA
      BTA3=BTA2*BTA
      EN2=EN*EN
      EN3=EN2*EN
      Q(2,I)=0.0D0
      IF(EN.EQ.0.0) THEN
       Q(2,I)=20.0
       GO TO 10
      ENDIF
      IF(EN.LE.0.255) THEN
       Q(2,I)=10.0**(1.055-1.033*BTA-0.1632*BTA2+0.0126*BTA3)
       GO TO 10
      ENDIF
      IF(EN.LE.0.92) THEN
       Q(2,I)=10.0**(1.041-0.189*BTA+2.091*BTA2+1.348*BTA3)
       GO TO 10
      ENDIF
      IF(EN.LE.1.90) THEN
       Q(2,I)=10.0**(1.037-0.3741*BTA+1.193*BTA2+0.5179*BTA3)
       GO TO 10
      ENDIF
      IF(EN.LE.6.20) THEN
       Q(2,I)=1.917+6.463*EN-1.027*EN2+0.05562*EN3
       GO TO 10
      ENDIF
      IF(EN.LE.28.2) THEN
       Q(2,I)=12.53+0.7762*EN-0.0457*EN2+0.0006344*EN3
       GO TO 10
      ENDIF
      IF(EN.LE.51.0) THEN
       Q(2,I)=20.44-0.3373*EN+0.002436*EN2-0.000006189*EN3
       GO TO 10
      ENDIF
      IF(EN.LE.80.0) THEN
       Q(2,I)=29.09-0.7115*EN+0.007397*EN2-0.00002485*EN3
       GO TO 10
      ENDIF
      IF(EN.LE.188.0) THEN
       Q(2,I)=10.51*EXP(-0.00558*EN)
       GO TO 10
      ENDIF
      IF(EN.LE.364.0) THEN
       Q(2,I)=1289.0*EN**(-1.118)
       GO TO 10
      ENDIF
      Q(2,I)=4.881*EXP(-0.002807*EN)
  10  Q(2,I)=Q(2,I)*1.D-16
      Q(3,I)=0.0D0
      IF(EN.LE.15.8) GO TO 20
      IF(EN.LE.38.9) THEN
       Q(3,I)=4.715-0.693*EN+0.0306*EN2-0.0003508*EN3
       GO TO 20
      ENDIF
      IF(EN.LE.122.0) THEN
       Q(3,I)=6.986-EXP(2.07-0.0145*EN-0.00014*EN2)
       GO TO 20
      ENDIF
      IF(EN.LE.201.0) THEN
       Q(3,I)=4.364+0.0323*EN-0.00009987*EN2
       GO TO 20
      ENDIF
      Q(3,I)=EXP(2.151-0.00115*EN)
  20  Q(3,I)=Q(3,I)*1.D-16
      Q(4,I)=0.0D0
      QA1=0.0D0
      IF(EN.EQ.0.0) THEN
       QA1=4000.0
       GO TO 30
      ENDIF
      IF(EN.GT.25.0) THEN
       QA5=0.0D0
       GO TO 70
      ENDIF
      IF(EN.LE.0.14) THEN
       QA1=436.0*(0.0617*SQRT(1.0/EN)*EXP(-1.0*(EN/0.0045)**2)+
     /EXP(-EN/0.0559))
       GO TO 30
      ENDIF
      IF(EN.LE.0.9746) THEN
       QA1=EXP(6.477-20.91*EN+1.183*EN2)
      ENDIF
  30  Q(4,I)=QA1*1.D-16
      QA2=0.0D0
      IF(EN.LE.0.312) THEN
       QA2=2.85*EN+5.419*EN2+30.49*EN3
       GO TO 40
      ENDIF
      IF(EN.LE.0.425) THEN
       QA2=468.0*EN3-624.3*EN2+268.1*EN-34.75
       GO TO 40
      ENDIF
      IF(EN.LE.1.05) THEN
       QA2=8.751-22.15*EN+19.08*EN2-5.592*EN3
       GO TO 40
      ENDIF
      QA2=EXP(8.054-10.42*EN)
  40  Q(4,I)=Q(4,I)+QA2*1.D-16
      QA3=0.0D0
      IF(EN.LT.2.19) GO TO 50
      IF(EN.LE.2.90) THEN
       QA3=-0.1069+0.08552*EN-0.01676*EN2
       GO TO 50
      ENDIF
      IF(EN.LT.3.32) GO TO 50
      IF(EN.LE.4.27) THEN
       QA3=-0.2016+0.2133*EN-0.07421*EN2+0.00851*EN3
       GO TO 50
      ENDIF
      IF(EN.LE.5.59) THEN
       QA3=0.7777-0.6913*EN+0.1856*EN2-0.0153*EN3
       GO TO 50
      ENDIF
      IF(EN.LE.7.95) THEN
       QA3=0.9885-0.3216*EN+0.03252*EN2-0.0009533*EN3
       GO TO 50
      ENDIF
      IF(EN.LE.9.73) THEN
       QA3=-0.3504+0.08087*EN-0.0045*EN2
       GO TO 50
      ENDIF
      IF(EN.LE.11.1) THEN
       QA3=1.397-0.2724*EN+0.01335*EN2
       GO TO 50
      ENDIF
      IF(EN.LE.11.8) THEN
       QA3=-3.30+0.5801*EN-0.02533*EN2
       GO TO 50
      ENDIF
      QA3=EXP(10.91-1.264*EN)
  50  Q(4,I)=Q(4,I)+QA3*1.D-16
      QA4=0.0D0
      IF(EN.LT.3.92) GO TO 60
      IF(EN.LE.8.25) THEN
       QA4=EXP(-466.8+296.4*EN-71.09*EN2+7.573*EN3-0.3033*EN*EN3)
      ENDIF
  60  Q(4,I)=Q(4,I)+QA4*1.D-16
      QA5=0.0D0
      IF(EN.LE.1.50) GO TO 70
      IF(EN.LE.3.27) THEN
       QA5=EXP(2.932*EN3-22.91*EN2+56.52*EN-53.37)
       GO TO 70
      ENDIF
      IF(EN.LE.7.45) THEN
       QA5=EXP(0.5554*EN3-9.613*EN2+52.832*EN-100.3)
       GO TO 70
      ENDIF
      IF(EN.LE.10.6) THEN
       QA5=EXP(0.1216*EN2-1.035*EN-9.723)
       GO TO 70
      ENDIF
      IF(EN.LE.11.7) THEN
       QA5=EXP(-1.114*EN2+25.12*EN-148.0)-0.00012
       GO TO 70
      ENDIF
      QA5=EXP(-0.9386*EN2+21.0*EN-123.9)
  70  Q(4,I)=Q(4,I)+QA5*1.D-16
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C VIBRATIONAL SUM
      QIN(1,I)=0.0D0
      IF(EN.LE.EIN(1).OR.EN.GT.50.0) GO TO 400
      IF(EN.LE.0.247) THEN
       QIN(1,I)=(14.06+4.425/EN-0.5472/EN2)*1.D-16
       GO TO 400
      ENDIF
      IF(EN.LE.0.505) THEN
       QIN(1,I)=(EXP(11.19*EN3-13.91*EN2+4.663*EN+2.664))*1.D-16
       GO TO 400
      ENDIF
      IF(EN.LE.1.03) THEN
       QIN(1,I)=(EXP(0.3166*EN2-1.341*EN+3.509))*1.D-16
       GO TO 400
      ENDIF
      QIN(1,I)=(22.0*10.0**(-0.2645*EN))*1.D-16
C EXCITATION
  400 QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 500
      IF(EN.LE.26.66) THEN
       QIN(2,I)=(4.811*BTA-4.769)*1.D-16
       GO TO 500
      ENDIF
      IF(EN.LE.29.3) THEN
       QIN(2,I)=(3.643-0.204*EN+0.005477*EN2)*1.D-16
       GO TO 500
      ENDIF
      IF(EN.LE.56.6) THEN
       QIN(2,I)=(0.01382*EN**(1.522))*1.D-16
       GO TO 500
      ENDIF
      IF(EN.LE.65.2) THEN
       QIN(2,I)=(-25.26+0.9902*EN-0.007593*EN2)*1.D-16
       GO TO 500
      ENDIF
      IF(EN.LE.100.0) THEN
       QIN(2,I)=(2.197+0.1479*EN-0.001123*EN2)*1.D-16
       GO TO 500
      ENDIF
      IF(EN.LE.250.0) THEN
       QIN(2,I)=(17.11*EXP(-0.0109*EN))*1.D-16
       GO TO 500
      ENDIF
      QIN(2,I)=(6566000.0*EN**(-2.821))*1.D-16
  500 CONTINUE
      IF(QIN(2,I).LE.0.0) QIN(2,I)=0.0
C
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+QIN(1,I)+QIN(2,I)
 9000 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(2)) NIN=1
      IF(EFINAL.LE.EIN(1)) NIN=0
      END
