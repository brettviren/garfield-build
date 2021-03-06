CDECK  ID>, GAS44.
      SUBROUTINE GAS44(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(67),YXSEC(67),XION(48),YION(48),XEXC(25),YEXC(25),
     /XEXS(34),YEXS(34),XEL(69),YEL(69)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C ELASTIC MOMENTUM TRANSFER
      DATA XEN/0.00,0.008,0.009,0.01,0.013,0.017,0.020,0.025,0.03,0.04,
     /0.05,0.06,0.07,0.08,0.09,0.10,0.12,0.15,0.18,0.20,
     /0.25,0.30,0.40,0.50,0.60,0.70,0.80,0.90,1.00,1.20,
     /1.50,1.80,2.00,2.50,3.00,4.00,5.00,6.00,7.00,8.00,
     /9.00,10.0,11.0,12.0,13.6,16.5,18.0,20.0,25.0,30.0,
     /40.0,50.0,60.0,70.0,75.0,80.0,90.0,100.,150.,200.,
     /400.,600.,1000.,2000.,10000.,20000.,100000./
      DATA YXSEC/4.90,5.18,5.19,5.21,5.26,5.31,5.35,5.41,5.46,5.54,
     /5.62,5.68,5.74,5.79,5.83,5.86,5.94,6.04,6.12,6.16,
     /6.27,6.35,6.49,6.59,6.66,6.73,6.77,6.82,6.85,6.91,
     /6.96,6.98,6.99,6.96,6.89,6.62,6.31,6.00,5.68,5.35,
     /5.03,4.72,4.44,4.15,3.83,3.25,2.99,2.58,2.00,1.60,
     /1.06,0.77,0.57,0.46,0.40,0.37,0.30,0.26,.132,.081,
     /.024,.012,.0048,.0014,.00008,.00002,.0000012/
C ELASTIC TOTAL
      DATA XEL/0.00,0.05,0.10,0.20,0.30,0.40,0.50,0.60,0.80,1.00,
     /1.22,1.50,1.70,2.00,2.40,2.50,3.00,3.40,4.00,4.40,
     /5.00,5.40,6.00,6.40,7.00,7.40,8.00,8.40,9.00,9.40,
     /10.0,10.4,11.4,12.0,12.4,13.4,14.0,14.4,15.4,16.0,
     /16.4,17.4,18.0,20.0,25.0,30.0,40.0,50.0,60.0,70.0,
     /80.0,90.0,100.,125.,150.,200.,250.,300.,400.,500.,
     /600.,800.,1000.,1500.,2000.,4000.,10000.,20000.,100000./
      DATA YEL/4.90,5.48,5.67,5.86,5.98,6.03,6.08,6.09,6.11,6.12,
     /6.10,6.08,6.07,6.03,5.95,5.93,5.82,5.73,5.60,5.51,
     /5.38,5.29,5.15,5.07,4.94,4.86,4.75,4.68,4.57,4.49,
     /4.39,4.32,4.15,4.06,4.00,3.85,3.76,3.71,3.58,3.51,
     /3.46,3.34,3.26,3.00,2.52,2.15,1.63,1.29,1.06,.884,
     /.734,.653,.572,.445,.351,.239,.181,.152,.107,.080,
     /.065,.040,.030,.020,.015,.0073,.0033,.0015,.0004/
C  IONISATION
      DATA XION/24.587,25.0,25.5,26.0,26.5,27.0,28.0,29.0,30.0,32.0,
     /34.0,36.0,38.0,40.0,45.0,50.0,55.0,60.0,70.0,80.0,
     /100.,120.,150.,175.,200.,250.,300.,400.,500.,600.,
     /700.,800.,900.,1000.,1200.,1400.,1600.,1800.,2000.,2500.,
     /3000.,4000.,5000.,6000.,8000.,10000.,20000.,100000./
      DATA YION/0.0,.0052,.0113,.0175,.0236,.030,.043,.055,.066,.089,
     /.111,.132,.150,.166,.203,.235,.260,.281,.312,.335,
     /.364,.369,.364,.354,.342,.315,.287,.249,.215,.191,
     /.173,.159,.145,.133,.117,.103,.093,.086,.080,.065,
     /.055,.045,.036,.031,.025,.020,.0117,.0040/
C TRIPLET EXCITATION
      DATA XEXC/19.82,20.0,20.2,20.5,20.6,20.8,21.0,21.3,22.0,25.0,
     /30.0,40.0,50.0,60.0,70.0,80.0,90.0,100.,150.,200.,
     /400.,1000.,10000.,20000.,100000./
      DATA YEXC/0.00,.047,.053,.035,.029,.043,.042,.041,.046,.075,
     /.071,.054,.038,.026,.017,.013,.0094,.0075,.0022,.00094,
     /.00012,.000008,.000000008,.000000001,.0000000003/
C  SINGLET EXCITATION
      DATA XEXS/20.61,20.9,21.0,21.5,22.0,22.5,25.0,28.0,30.0,35.0,
     /40.0,45.0,50.0,60.0,70.0,80.0,90.0,100.,150.,200.,
     /300.,400.,500.,600.,800.,1000.,1500.,2000.,3000.,4000.,
     /6000.,10000.,20000.,100000./
      DATA YEXS/0.00,.025,.022,.0265,.0315,.036,.065,.082,.092,.115,
     /.133,.148,.155,.175,.177,.178,.178,.177,.163,.148,
     /.121,.099,.086,.075,.061,.051,.038,.030,.022,.017,
     /.013,.0088,.0052,.0018/
      NAME='He3 (2002)'
C --------------------------------------------------------------------
C  HELIUM 4 BEST KNOWN GAS USED AS STANDARD ACCURACY BETTER THAN 0.2%
C  AT ALL FIELDS ONLY DIFFERENCE FROM HE3 IS ATOMIC MASS.
C  UPDATED 1997 DATA FILE TO INCLUDE ANISOTROPIC ELASTIC SCATTTERING
C --------------------------------------------------------------------
      NIN=2
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
C ANISOTROPIC ELASTIC SCATTERING COPY (OFFSET) TO IONISATION
      KEL(2)=1
      KEL(3)=1
C
      NEL=69
      NDATA=67
      NION=48
      NEXC=25
      NEXS=34
      E(1)=0.0
      E(2)=2.0*EMASS/(3.01600*AMU)
      E(3)=24.587
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=15.8
      IOFF=INT(0.5+E(3)/ESTEP)
      EIN(1)=19.82
      EIN(2)=20.61
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC (ANIS) HELIUM 3      '
      SCRPT(3)=' IONISATION    ELOSS= 24.587  '
      SCRPT(4)='                              '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' EXC TRPLT     ELOSS= 19.82   '
      SCRPT(8)=' EXC SNGLT     ELOSS= 20.61   '
      EN=-ESTEP/2.0D0
      DO 900 I=1,NSTEP
      EN=EN+ESTEP
      DO 10 J=2,NEL
      IF(EN.LE.XEL(J)) GO TO 20
   10 CONTINUE
      J=NEL
   20 A=(YEL(J)-YEL(J-1))/(XEL(J)-XEL(J-1))
      B=(XEL(J-1)*YEL(J)-XEL(J)*YEL(J-1))/(XEL(J-1)-XEL(J))
      QELA=(A*EN+B)*1.0D-16
      DO 50 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 60
   50 CONTINUE
      J=NDATA
   60 A=(YXSEC(J)-YXSEC(J-1))/(XEN(J)-XEN(J-1))
      B=(XEN(J-1)*YXSEC(J)-XEN(J)*YXSEC(J-1))/(XEN(J-1)-XEN(J))
      QMOM=(A*EN+B)*1.0D-16
      PEQEL(2,I)=0.5+(QELA-QMOM)/QELA
      Q(2,I)=QELA
C
      Q(3,I)=0.0D0
      PEQEL(3,I)=0.5D0
      IF(EN.LE.E(3)) GO TO 200
      DO 110 J=2,NION
      IF(EN.LE.XION(J)) GO TO 120
  110 CONTINUE
      J=NION
  120 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
C USE ANISOTROPIC SCATTERING FOR PRIMARY IONISATION ELECTRON FOR
C ENERGIES ABOVE 2 * IONISATION ENERGIES
C ANISOTROPIC ANGULAR DISTRIBUTION SAME AS ELASTIC AT ENERGY OFFSET BY
C IONISATION ENERGY
      IF(EN.LE.2.0*E(3)) GO TO 200
      PEQEL(3,I)=PEQEL(2,(I-IOFF))
C
  200 CONTINUE
      Q(4,I)=0.0D0
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
      QIN(1,I)=0.0D0
      IF(EN.LE.EIN(1)) GO TO 600
      DO 510 J=2,NEXC
      IF(EN.LE.XEXC(J)) GO TO 520
  510 CONTINUE
      J=NEXC
  520 A=(YEXC(J)-YEXC(J-1))/(XEXC(J)-XEXC(J-1))
      B=(XEXC(J-1)*YEXC(J)-XEXC(J)*YEXC(J-1))/(XEXC(J-1)-XEXC(J))
      QIN(1,I)=(A*EN+B)*1.D-16
  600 CONTINUE
C
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 700
      DO 610 J=2,NEXS
      IF(EN.LE.XEXS(J)) GO TO 620
  610 CONTINUE
      J=NEXS
  620 A=(YEXS(J)-YEXS(J-1))/(XEXS(J)-XEXS(J-1))
      B=(XEXS(J-1)*YEXS(J)-XEXS(J)*YEXS(J-1))/(XEXS(J-1)-XEXS(J))
      QIN(2,I)=(A*EN+B)*1.D-16
  700 CONTINUE
C
      Q(1,I)=Q(2,I)+Q(3,I)+QIN(1,I)+QIN(2,I)
  900 CONTINUE
C  SAVE COMPUTE TIME
      IF(EFINAL.LE.EIN(2)) NIN=1
      IF(EFINAL.LE.EIN(1)) NIN=0
C
      END
