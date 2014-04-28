CDECK  ID>, GAS7.
      SUBROUTINE GAS7(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION XEN(116),YXSEC(116),XION(77),YION(77)
      DIMENSION XEXC1(98),YEXC1(98),XEXC2(76),YEXC2(76),XEXC3(67)
      DIMENSION YEXC3(67),XEXC4(62),YEXC4(62)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C
      DATA XEN/0.00,.001,.005,.007,0.01,.015,0.02,.025,0.03,0.04,
     /0.05,0.06,0.07,0.08,0.10,0.12,0.14,0.17,0.20,0.25,
     /0.27,0.30,0.32,0.35,0.37,0.40,0.42,0.44,0.46,0.48,
     /0.50,0.51,0.52,0.53,0.54,0.55,0.56,0.57,0.58,0.59,
     /0.60,0.61,0.62,0.63,0.64,0.65,0.66,0.67,0.68,0.69,
     /0.70,.715,0.73,0.75,0.77,0.80,0.83,0.85,0.87,0.90,
     /1.00,1.08,1.14,1.20,1.30,1.40,1.50,1.70,2.00,2.50,
     /3.00,3.50,4.00,4.50,5.00,5.50,6.00,6.50,7.00,8.00,
     /9.00,10.0,12.0,15.0,18.0,20.0,25.0,30.0,40.0,50.0,
     /60.0,70.0,80.0,90.0,100.,125.,150.,200.,250.,300.,
     /400.,500.,600.,700.,800.,1000.,1500.,2000.,3000.,4000.,
     /5000.,6000.,8000.,10000.,20000.,200000./
      DATA YXSEC/131.,115.,97.0,91.1,83.9,74.6,67.3,61.2,56.1,47.9,
     /41.4,36.2,31.8,28.2,22.5,18.1,14.8,11.1,8.36,5.33,
     /4.47,3.43,2.88,2.22,1.86,1.43,1.20,1.01,.844,.708,
     /.596,.548,.504,.465,.430,.399,.372,.348,.328,.310,
     /.296,.285,.276,.270,.266,.265,.266,.270,.276,.287,
     /.306,.341,.377,.427,.479,.562,.651,.713,.778,.880,
     /1.26,1.62,1.92,2.25,2.85,3.51,4.22,5.73,7.97,11.8,
     /15.8,20.4,24.4,28.0,30.7,31.5,32.3,31.6,31.0,27.5,
     /22.8,18.5,14.0,9.71,7.73,6.72,5.35,4.43,3.42,2.81,
     /2.42,2.17,2.00,1.89,1.80,1.73,1.65,1.34,1.15,1.05,
     /0.95,0.86,0.75,0.69,0.63,0.56,0.37,0.29,0.21,0.16,
     /0.13,0.12,.087,.073,.037,.006/
C
      DATA XION/12.13,12.5,13.0,13.5,14.0,14.5,15.0,15.5,16.0,16.5,
     /17.0,18.0,19.0,20.0,21.0,22.0,23.0,24.0,26.0,28.0,
     /30.0,32.0,34.0,36.0,40.0,45.0,50.0,55.0,60.0,65.0,
     /70.0,80.0,90.0,100.,110.,120.,130.,140.,150.,160.,
     /180.,200.,250.,300.,350.,400.,450.,500.,550.,600.,
     /700.,800.,900.,1000.,1200.,1400.,1600.,1800.,2000.,2500.,
     /3000.,3500.,4000.,4500.,5000.,5500.,6000.,7000.,8000.,9000.,
     /10000.,12000.,14000.,16000.,18000.,20000.,200000./
      DATA YION/0.00,.090,.236,.394,.559,.730,.906,1.09,1.27,1.42,
     /1.58,1.85,2.11,2.35,2.56,2.82,3.02,3.21,3.49,3.71,
     /3.93,4.15,4.30,4.42,4.60,4.82,4.96,5.08,5.19,5.24,
     /5.28,5.32,5.41,5.53,5.61,5.59,5.53,5.41,5.31,5.16,
     /4.91,4.71,4.29,3.96,3.66,3.40,3.19,3.01,2.81,2.67,
     /2.44,2.22,2.05,1.92,1.68,1.48,1.37,1.23,1.14,.974,
     /.842,.742,.668,.607,.560,.519,.485,.426,.385,.349,
     /.321,.275,.244,.220,.202,.188,.0155/
      DATA XEXC1/8.315,8.35,8.40,8.45,8.50,8.55,8.60,8.65,8.70,8.75,
     /8.80,8.85,8.90,8.95,9.00,9.05,9.10,9.15,9.20,9.25,
     /9.30,9.35,9.40,9.45,9.50,9.55,9.60,9.65,9.70,9.75,
     /9.80,9.90,10.0,10.2,10.4,10.6,11.0,12.0,13.0,14.0,
     /15.0,16.0,18.0,20.0,22.0,24.0,26.0,28.0,30.0,35.0,
     /40.0,50.0,60.0,70.0,80.0,90.0,100.,120.,140.,160.,
     /180.,200.,240.,280.,320.,360.,400.,450.,500.,600.,
     /700.,800.,900.,1000.,1200.,1400.,1600.,2000.,2400.,2800.,
     /3200.,3600.,4000.,4500.,5000.,6000.,7000.,8000.,9000.,10000.,
     /15000.,20000.,30000.,40000.,60000.,80000.,100000.,200000./
      DATA YEXC1/0.00,.0116,.024,.027,.026,.029,.0346,.040,.0458,.0516,
     /.0584,.0670,.079,.093,.106,.115,.116,.112,.108,.106,
     /.107,.115,.125,.144,.206,.240,.214,.168,.146,.144,
     /.147,.156,.164,.178,.192,.206,.232,.288,.336,.387,
     /.425,.459,.512,.552,.581,.602,.616,.624,.629,.634,
     /.629,.590,.571,.538,.509,.480,.456,.413,.374,.350,
     /.326,.307,.269,.245,.221,.206,.192,.173,.158,.139,
     /.125,.110,.101,.0917,.080,.0706,.063,.053,.046,.040,
     /.0360,.033,.0302,.0274,.025,.0216,.019,.017,.015,.013,
     /.0101,.0082,.006,.005,.0035,.003,.0025,.0017/
      DATA XEXC2/9.447,9.45,9.50,9.55,9.60,9.65,9.70,9.75,9.80,9.90,
     /10.0,10.2,10.4,10.6,11.0,12.0,13.0,14.0,15.0,16.0,
     /18.0,20.0,22.0,24.0,26.0,28.0,30.0,35.0,40.0,50.0,
     /60.0,70.0,80.0,90.0,100.,120.,140.,160.,180.,200.,
     /240.,280.,320.,360.,400.,450.,500.,600.,700.,800.,
     /900.,1000.,1200.,1400.,1600.,2000.,2400.,2800.,3200.,3600.,
     /4000.,4500.,5000.,6000.,7000.,8000.,9000.,10000.,15000.,20000.,
     /30000.,40000.,60000.,80000.,100000.,200000./
      DATA YEXC2/0.00,.0035,.0108,.0158,.0157,.015,.0176,.027,.037,.063,
     /.0884,.134,.177,.214,.277,.392,.461,.504,.530,.544,
     /.557,.555,.547,.538,.526,.515,.503,.475,.450,.408,
     /.374,.346,.323,.302,.284,.255,.232,.214,.199,.185,
     /.164,.148,.134,.124,.114,.106,.097,.0851,.0756,.0684,
     /.0624,.0575,.0499,.0442,.0397,.0332,.0287,.0252,.0227,.0206,
     /.0189,.0172,.0157,.0135,.0119,.0107,.0097,.00884,.00632,.00499,
     /.00359,.00286,.00210,.00171,.00146,.00097/
      DATA XEXC3/9.917,10.0,10.2,10.4,10.6,11.0,12.0,13.0,14.0,15.0,
     /16.0,18.0,20.0,22.0,24.0,26.0,28.0,30.0,35.0,40.0,
     /50.0,60.0,70.0,80.0,90.0,100.,120.,140.,160.,180.,
     /200.,240.,280.,320.,360.,400.,450.,500.,600.,700.,
     /800.,900.,1000.,1200.,1400.,1600.,2000.,2400.,2800.,3200.,
     /3600.,4000.,4500.,5000.,6000.,7000.,8000.,9000.,10000.,15000.,
     /20000.,30000.,40000.,60000.,80000.,100000.,200000./
      DATA YEXC3/0.00,.000005,.0389,.118,.190,.315,.615,.810,.939,1.03,
     /1.08,1.14,1.17,1.17,1.16,1.15,1.13,1.11,1.07,1.02,
     /.930,.858,.800,.745,.701,.661,.596,.544,.501,.466,
     /.435,.386,.348,.318,.292,.271,.249,.230,.202,.180,
     /.162,.149,.137,.119,.106,.0947,.0792,.0684,.0604,.0541,
     /.0492,.0451,.0410,.0375,.0324,.0284,.0254,.0231,.0212,.0152,
     /.0120,.00861,.00686,.00504,.00409,.00351,.00233/
      DATA XEXC4/11.70,12.0,13.0,14.0,15.0,16.0,18.0,20.0,22.0,24.0,
     /26.0,28.0,30.0,35.0,40.0,50.0,60.0,70.0,80.0,90.0,
     /100.,120.,140.,160.,180.,200.,240.,280.,320.,360.,
     /400.,450.,500.,600.,700.,800.,900.,1000.,1200.,1400.,
     /1600.,2000.,2400.,2800.,3200.,3600.,4000.,4500.,5000.,6000.,
     /7000.,8000.,9000.,10000.,15000.,20000.,30000.,40000.,60000.,8E4,
     /100000.,200000./
      DATA YEXC4/0.00,.00194,.0299,.0776,.134,.194,.308,.410,.494,.564,
     /.622,.668,.705,.767,.801,.820,.807,.784,.756,.727,
     /.698,.645,.598,.558,.523,.492,.442,.400,.367,.340,
     /.316,.291,.271,.237,.212,.192,.176,.162,.141,.125,
     /.112,.0946,.0818,.0722,.0648,.0589,.0540,.0492,.0450,.0388,
     /.0342,.0306,.0277,.0254,.0182,.0144,.0104,.00828,.00608,.00494,
     /.00424,.00281/
C
      NAME='Xe (2003)'
C
C --------------------------------------------------------------------
C  DATA ON XENON NOT AS GOOD AS ARGON . USED MOMENTUM TRANSFER
C  X-SECTION FROM SCHMIDT. AND FIT TO TOWNSEND  COEFFICIENT OF
C  JACQUES ET AL J.PHYS D 19 (1986) 1731-1739 AND KRUITHOF TO OBTAIN
C  INELASTIC X-SECTIONS.
C --------------------------------------------------------------------
C
C
      NIN=4
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
      NDATA=116
      NION=77
      NEXC1=98
      NEXC2=76
      NEXC3=67
      NEXC4=62
      E(1)=0.0
      E(2)=2.0*EMASS/(131.30*AMU)
      E(3)=12.13
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=8.70
      EIN(1)=8.315
      EIN(2)=9.447
      EIN(3)=9.917
      EIN(4)=11.70
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       XENON          '
      SCRPT(3)=' IONISATION    ELOSS= 12.13   '
      SCRPT(4)='                              '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' EXC           ELOSS=  8.315  '
      SCRPT(8)=' EXC           ELOSS=  9.447  '
      SCRPT(9)=' EXC           ELOSS=  9.917  '
      SCRPT(10)=' EXC           ELOSS= 11.70   '
      EN=-ESTEP/2.0D0
      DO 900 I=1,NSTEP
      EN=EN+ESTEP
      IF(EN.LE.XEN(2)) THEN
       Q(2,I)=122.D-16
       GO TO 200
      ENDIF
      DO 150 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 160
  150 CONTINUE
      J=NDATA
  160 YXJ=LOG(YXSEC(J))
      YXJ1=LOG(YXSEC(J-1))
      XNJ=LOG(XEN(J))
      XNJ1=LOG(XEN(J-1))
      A=(YXJ-YXJ1)/(XNJ-XNJ1)
      B=(XNJ1*YXJ-XNJ*YXJ1)/(XNJ1-XNJ)
      Q(2,I)=EXP(A*LOG(EN)+B)*1.D-16
  200 CONTINUE
      Q(3,I)=0.0D0
      IF(EN.LE.E(3)) GO TO 230
      DO 210 J=2,NION
      IF(EN.LE.XION(J)) GO TO 220
  210 CONTINUE
      J=NION
  220 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.0D-16
  230 CONTINUE
      Q(4,I)=0.0D0
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
      QIN(1,I)=0.0D0
      IF(EN.LE.EIN(1)) GO TO 370
      DO 350 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 360
  350 CONTINUE
      J=NEXC1
  360 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(1,I)=(A*EN+B)*1.0D-16
  370 CONTINUE
      QIN(2,I)=0.0D0
      IF(EN.LE.EIN(2)) GO TO 470
      DO 450 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 460
  450 CONTINUE
      J=NEXC2
  460 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(2,I)=(A*EN+B)*1.0D-16
  470 CONTINUE
      QIN(3,I)=0.0D0
      IF(EN.LE.EIN(3)) GO TO 570
      DO 550 J=2,NEXC3
      IF(EN.LE.XEXC3(J)) GO TO 560
  550 CONTINUE
      J=NEXC3
  560 A=(YEXC3(J)-YEXC3(J-1))/(XEXC3(J)-XEXC3(J-1))
      B=(XEXC3(J-1)*YEXC3(J)-XEXC3(J)*YEXC3(J-1))/(XEXC3(J-1)-XEXC3(J))
      QIN(3,I)=(A*EN+B)*1.0D-16
  570 CONTINUE
      QIN(4,I)=0.0D0
      IF(EN.LE.EIN(4)) GO TO 670
      DO 650 J=2,NEXC4
      IF(EN.LE.XEXC4(J)) GO TO 660
  650 CONTINUE
      J=NEXC4
  660 A=(YEXC4(J)-YEXC4(J-1))/(XEXC4(J)-XEXC4(J-1))
      B=(XEXC4(J-1)*YEXC4(J)-XEXC4(J)*YEXC4(J-1))/(XEXC4(J-1)-XEXC4(J))
      QIN(4,I)=(A*EN+B)*1.0D-16
  670 CONTINUE
      Q(1,I)=Q(2,I)+Q(3,I)+QIN(1,I)+QIN(2,I)+QIN(3,I)+QIN(4,I)
  900 CONTINUE
      IF(EFINAL.LT.EIN(4)) NIN=3
      IF(EFINAL.LT.EIN(3)) NIN=2
      IF(EFINAL.LT.EIN(2)) NIN=1
      IF(EFINAL.LT.EIN(1)) NIN=0
      END
