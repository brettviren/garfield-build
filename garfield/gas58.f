CDECK  ID>, GAS58.
      SUBROUTINE GAS58(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
      DIMENSION Q(6,2048),QIN(220,2048),E(6),EIN(220),PJ(220)
      DIMENSION XMOM(65),YMOM(65),XELA(59),YELA(59),XVIB1(50),YVIB1(50),
     /XVIB2(24),YVIB2(24),XVIB3(20),YVIB3(20),XVIB4(18),YVIB4(18),
     /XVIB5(18),YVIB5(18),XVIB6(15),YVIB6(15),XVIB7(17),YVIB7(17),
     /XVIB8(15),YVIB8(15),
     /XTRP1(25),YTRP1(25),XTRP3(23),YTRP3(23),XTRP5(26),YTRP5(26),
     /XTRP7(29),YTRP7(29),XTRP8(19),YTRP8(19),
     /XSNG2(29),YSNG2(29),XSNG5(26),YSNG5(26),XION(43),YION(43)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
      DATA XELA/0.00,.010,.015,0.02,0.03,0.04,0.05,0.07,0.10,0.12,
     /0.15,0.17,0.20,0.25,0.30,0.35,0.40,0.50,0.70,1.00,
     /1.20,1.30,1.50,1.70,1.90,2.10,2.20,2.50,2.80,3.00,
     /3.30,3.60,4.00,4.50,5.00,6.00,7.00,8.00,10.0,12.0,
     /15.0,17.0,20.0,25.0,30.0,50.0,75.0,100.,150.,200.,
     /300.,500.,700.,1000.,2000.,4000.,10000.,20000.,100000./
      DATA YELA/1.10,1.92,2.23,2.49,2.95,3.35,3.78,4.48,5.26,5.72,
     /6.33,6.62,7.11,7.70,8.22,8.62,8.96,9.24,9.52,9.74,
     /10.26,10.91,11.99,13.78,16.98,17.62,18.94,19.11,22.7,18.74,
     /16.88,15.59,14.08,12.90,12.74,12.53,12.43,13.01,13.23,13.23,
     /12.80,12.56,12.10,11.41,10.67,8.30,6.66,5.38,4.18,3.50,
     /2.70,1.831,1.455,1.03,0.58,0.28,0.10,.052,.008/
      DATA XMOM/0.00,.001,.002,.003,.005,.007,.0085,.010,.015,0.02,
     /0.03,0.04,0.05,0.07,0.10,0.12,0.15,0.17,0.20,0.25,
     /0.30,0.35,0.40,0.50,0.70,1.00,1.20,1.30,1.50,1.70,
     /1.90,2.10,2.20,2.50,2.80,3.00,3.30,3.60,4.00,4.50,
     /5.00,6.00,7.00,8.00,10.0,12.0,15.0,17.0,20.0,25.0,
     /30.0,50.0,75.0,100.,150.,200.,300.,500.,700.,1000.,
     /2000.,4000.,10000.,20000.,100000./
      DATA YMOM/1.10,1.36,1.49,1.62,1.81,2.00,2.10,2.19,2.55,2.85,
     /3.38,3.82,4.30,5.08,5.92,6.42,7.08,7.38,7.88,8.48,
     /8.98,9.36,9.67,9.87,9.97,9.96,10.34,10.92,11.87,13.47,
     /16.41,16.85,18.02,17.92,21.0,17.20,15.3,13.96,12.42,11.19,
     /10.86,10.36,10.0,10.2,9.90,9.50,8.70,8.26,7.60,6.70,
     /5.90,3.80,2.56,1.80,1.13,0.80,0.48,0.23,.143,.077,
     /.038,.019,.008,.004,.001/
      DATA XVIB1/0.29,0.30,0.33,0.40,0.75,0.90,1.00,1.10,1.16,1.20,
     /1.22,1.40,1.50,1.60,1.65,1.70,1.80,1.90,2.00,2.10,
     /2.20,2.30,2.40,2.50,2.60,2.70,2.75,2.80,2.90,3.00,
     /3.10,3.20,3.30,3.40,3.50,3.60,4.00,5.00,15.0,18.0,
     /20.0,22.0,23.0,25.0,29.0,32.0,50.0,80.0,1000.,100000./
      DATA YVIB1/.00,.001,.0017,.0025,.0037,.0055,.0065,.009,.011,.0125,
     /.0135,.070,.100,.150,.270,.315,.540,1.485,4.80,2.565,
     /1.20,4.50,2.76,1.59,3.15,1.545,0.60,1.35,.525,0.870,
     /1.17,0.855,0.66,0.60,.585,0.57,.055,.035,.035,0.04,
     /.065,.085,.085,0.06,0.03,.015,.012,0.00,0.00,0.00/
      DATA XVIB2/0.59,1.70,1.80,1.90,2.00,2.10,2.20,2.30,2.40,2.50,
     /2.60,2.70,2.75,2.80,2.90,3.00,3.10,3.20,3.30,3.40,
     /3.50,3.60,1000.,100000./
      DATA YVIB2/0.00,0.00,.015,0.63,1.935,3.30,1.47,0.54,2.115,3.00,
     /0.54,1.05,1.725,1.275,0.33,0.90,0.645,0.375,0.345,0.30,
     /0.213,0.00,0.00,0.00/
      DATA XVIB3/0.88,1.90,2.00,2.10,2.20,2.30,2.40,2.50,2.60,2.70,
     /2.75,2.80,2.90,3.00,3.10,3.20,3.30,3.40,1000.,100000./
      DATA YVIB3/0.00,0.00,0.96,2.055,2.70,1.695,0.075,0.96,1.47,0.45,
     /0.96,0.54,0.855,0.405,0.282,0.291,0.0615,0.00,0.00,0.00/
      DATA XVIB4/1.17,2.00,2.10,2.20,2.30,2.40,2.50,2.60,2.70,2.75,
     /2.80,2.90,3.00,3.10,3.20,3.30,1000.,100000./
      DATA YVIB4/0.0,0.0,.2025,1.515,2.385,1.440,.555,.0825,1.2,1.095,
     /0.675,0.03,0.33,0.315,0.06,0.00,0.00,0.00/
      DATA XVIB5/1.47,2.10,2.20,2.30,2.40,2.50,2.60,2.70,2.75,2.80,
     /2.90,3.00,3.10,3.20,3.30,3.40,1000.,100000./
      DATA YVIB5/0.00,0.00,.825,1.23,1.53,1.44,0.345,.0225,.345,0.54,
     /0.66,.2175,.105,.315,.1035,0.00,0.00,0.00/
      DATA XVIB6/1.76,2.20,2.30,2.40,2.50,2.60,2.70,2.75,2.80,2.90,
     /3.00,3.10,3.20,1000.,100000./
      DATA YVIB6/0.00,0.00,.0063,1.125,1.74,1.38,0.78,0.45,.315,.246,
     /0.48,.1635,0.00,0.00,0.00/
      DATA XVIB7/2.06,2.30,2.40,2.50,2.60,2.70,2.75,2.80,2.90,3.00,
     /3.10,3.20,3.30,3.40,3.50,1000.,100000./
      DATA YVIB7/0.00,0.00,.0126,0.39,0.66,0.96,.795,0.60,0.18,.0063,
     /.192,.204,.078,.0189,0.00,0.00,0.00/
      DATA XVIB8/2.35,2.50,2.60,2.70,2.75,2.80,2.90,3.00,3.10,3.20,
     /3.30,3.40,3.50,1000.,100000./
      DATA YVIB8/0.00,0.00,.0189,0.36,0.36,0.33,.345,.264,.0375,.0063,
     /.1545,.0252,0.00,0.00,0.00/
      DATA XTRP1/6.17,7.00,7.80,8.50,9.00,10.0,11.0,12.0,13.0,14.0,
     /16.0,17.0,18.0,20.0,22.0,24.0,26.0,30.0,34.0,40.0,
     /50.0,70.0,150.,1000.,100000./
      DATA YTRP1/0.00,.0033,.0085,.0213,.0307,.0468,.059,.069,.075,.082,
     /.089,.089,.084,.072,.061,.052,.045,.034,.029,.023,
     /.019,.004,0.00,0.00,0.00/
      DATA XTRP3/7.35,8.00,9.00,10.0,11.0,12.0,13.0,14.0,15.0,16.0,
     /17.0,18.0,20.0,22.0,26.0,30.0,34.0,40.0,50.0,70.0,
     /150.0,1000.,100000./
      DATA YTRP3/.0,.0543,.1434,.2312,.2975,.343,.373,.387,.397,.399,
     /.383,.354,.289,.227,.165,.131,.106,.0777,.0469,.0168,
     /0.00,0.00,0.00/
      DATA XTRP5/7.80,8.10,8.50,8.70,9.00,10.0,11.0,12.0,13.0,14.0,
     /16.0,17.0,18.0,20.0,22.0,24.0,26.0,30.0,34.0,40.0,
     /50.0,70.0,150.0,500.0,1000.,100000./
      DATA YTRP5/0.0,.0015,.0097,.018,.029,.073,.115,.148,.180,.208,
     /.205,.178,.152,.122,.105,.091,.081,.066,.057,.047,
     /.041,.021,.007,0.00,0.00,0.00/
      DATA XSNG2/8.55,9.00,12.0,14.0,15.0,16.0,17.0,18.0,19.0,20.0,
     /24.0,26.0,30.0,40.0,50.0,70.0,100.,150.,200.,250.,
     /300.,500.,700.,1000.,2000.,4000.,10000.,20000.,100000./
      DATA YSNG2/.0,.0141,.163,.2276,.2412,.2481,.2483,.238,.2268,.2150,
     /.1860,.1734,.1527,.1160,.0900,.0642,.0425,.0268,.0201,.0161,
     /.0134,.0082,.0060,.0042,.0020,.0010,.0004,.0002,.00004/
      DATA XTRP7/11.03,11.5,12.0,12.5,13.0,13.5,13.8,14.0,14.2,14.5,
     /15.0,16.0,17.0,18.0,19.0,20.0,22.0,24.0,26.0,28.0,
     /30.0,36.0,40.0,50.0,70.0,100.0,150.0,1000.,100000./
      DATA YTRP7/.0,.0405,.093,.1965,.435,.735,.93,.975,.96,.945,
     /.825,.645,.525,.450,.405,.375,.315,.2655,.225,.2085,
     /.1665,.117,.0945,.0585,.0225,.0023,0.00,0.00,0.00/
      DATA XTRP8/11.87,11.92,12.7,17.0,19.0,20.0,22.0,24.0,26.0,28.0,
     /30.0,32.0,40.0,50.0,70.0,100.,150.0,1000.,100000./
      DATA YTRP8/.0,.0496,.0041,.0346,.0436,.0448,.0405,.0338,.0289,
     /.0241,.0193,.0172,.0122,.010,.007,.005,0.00,0.00,0.00/
      DATA XSNG5/13.0,14.0,15.0,16.0,17.0,18.0,20.0,22.0,25.0,30.0,
     /40.0,60.0,80.0,100.,150.,200.,250.,300.,500.,700.,
     /1000.,2000.,4000.,10000.,20000.,100000./
      DATA YSNG5/0.0,.081,0.19,0.25,0.42,0.52,0.75,0.96,1.19,1.48,
     /1.65,1.76,1.68,1.58,1.33,1.16,1.05,0.96,0.74,0.64,
     /0.53,0.28,0.14,0.06,0.03,.006/
      DATA XION/15.6,16.0,16.5,17.0,17.5,18.0,18.5,19.0,19.5,20.0,
     /21.0,22.0,23.0,24.0,25.0,26.0,30.0,34.0,38.0,45.0,
     /50.0,60.0,75.0,100.,125.,150.,200.,250.,300.,400.,
     /500.,700.,1000.,1500.,2000.,3000.,4000.,6000.,8000.,10000.,
     /20000.,40000.,100000./
      DATA YION/0.00,.021,.047,.071,.099,.129,.164,.199,.230,.270,
     /.344,.418,.492,.565,.640,.714,1.03,1.27,1.49,1.78,
     /1.94,2.18,2.39,2.52,2.52,2.45,2.27,2.08,1.92,1.66,
     /1.45,1.16,0.91,.654,.521,.375,.295,.209,.164,.135,
     /.073,.040,.017/
C
      NAME='N2 (2004 anis.)'
C    --------------------------------------------------------------
C    NITROGEN FROM PITCHFORD AND PHELPS . JILA REPORT NO.26 (1985)
C    MULTI TERM CROSS SECTIONS WITH MODIFICATION CF:PHELPS PRIVATE
C    COMMUNICATION . REDUCED 11.03 ENERGY LOSS X-SECTION BY 0.6666
C    IN CODE.
C       ACCURACY ABOUT 1% AT ALL FIELDS.
C    COMBINED SOME CLOSE LEVELS IN ORDER TO SAVE COMPUTING TIME
C    2004: INCLUDED FULL TREATMENT OF ROTATIONAL STATES
C    ANISOTROPIC ELASTIC SCATTERING
C    --------------------------------------------------------------
      NIN=71
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
    2 KIN(J)=0
C USE ANISTROPIC ELASTIC SCATTERING AND COPY (OFFSET) TO IONISATION
      KEL(2)=1
      KEL(3)=1
C
      NELA=59
      NMOM=65
      NVIB1=50
      NVIB2=24
      NVIB3=20
      NVIB4=18
      NVIB5=18
      NVIB6=15
      NVIB7=17
      NVIB8=15
      NTRP1=25
      NTRP3=23
      NTRP5=26
      NTRP7=29
      NTRP8=19
      NSNG2=29
      NSNG5=26
      NION=43
      E(1)=0.0
      E(2)=2.0*EMASS/(27.7940*AMU)
      E(3)=15.60
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=13.0
      IOFF=INT(0.5+E(3)/ESTEP)
C CALC FRACTIONAL POPULATION DENSITY FOR ROTATIONAL STATES
      B0=2.4668D-4
      A0=0.5291772083
      QBQA=1.06
      QBK=1.67552*(QBQA*A0)**2*1.D-16
      DO 3 K=1,29,2
    3 PJ(K)=3*(2*K+1)*EXP(-K*(K+1)*B0/AKT)
      DO 4 K=2,28,2
    4 PJ(K)=6*(2*K+1)*EXP(-K*(K+1)*B0/AKT)
      SUM=6.0D0
      DO 5 K=1,29
    5 SUM=SUM+PJ(K)
      FROT0=6.0D0/SUM
      DO 6 K=1,29
    6 PJ(K)=PJ(K)/SUM
C CALC ROTATIONAL TRANSITION ENERGIES
      DO 7 K=1,28
      J=K-1
      EIN(K+28)=B0*(4*J+6)
    7 EIN(K)=-EIN(K+28)
      EIN(57)=0.290
      EIN(58)=0.590
      EIN(59)=0.880
      EIN(60)=1.17
      EIN(61)=1.47
      EIN(62)=1.76
      EIN(63)=2.06
      EIN(64)=2.35
      EIN(65)=6.17
      EIN(66)=7.35
      EIN(67)=7.80
      EIN(68)=8.55
      EIN(69)=11.03
      EIN(70)=11.87
      EIN(71)=13.0
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC  ANISOTROPIC NITROGEN'
      SCRPT(3)=' IONISATION    ELOSS= 15.60   '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' ROT 2-0       ELOSS= -0.00148'
      SCRPT(8)=' ROT 3-1       ELOSS= -0.00247'
      SCRPT(9)=' ROT 4-2       ELOSS= -0.00345'
      SCRPT(10)=' ROT 5-3       ELOSS= -0.00444'
      SCRPT(11)=' ROT 6-4       ELOSS= -0.00543'
      SCRPT(12)=' ROT 7-5       ELOSS= -0.00641'
      SCRPT(13)=' ROT 8-6       ELOSS= -0.00740'
      SCRPT(14)=' ROT 9-7       ELOSS= -0.00839'
      SCRPT(15)=' ROT 10-8      ELOSS= -0.00937'
      SCRPT(16)=' ROT 11-9      ELOSS= -0.0104 '
      SCRPT(17)=' ROT 12-10     ELOSS= -0.0113 '
      SCRPT(18)=' ROT 13-11     ELOSS= -0.0123 '
      SCRPT(19)=' ROT 14-12     ELOSS= -0.0133 '
      SCRPT(20)=' ROT 15-13     ELOSS= -0.0143 '
      SCRPT(21)=' ROT 16-14     ELOSS= -0.0153 '
      SCRPT(22)=' ROT 17-15     ELOSS= -0.0163 '
      SCRPT(23)=' ROT 18-16     ELOSS= -0.0173 '
      SCRPT(24)=' ROT 19-17     ELOSS= -0.0183 '
      SCRPT(25)=' ROT 20-18     ELOSS= -0.0192 '
      SCRPT(26)=' ROT 21-19     ELOSS= -0.0202 '
      SCRPT(27)=' ROT 22-20     ELOSS= -0.0212 '
      SCRPT(28)=' ROT 23-21     ELOSS= -0.0222 '
      SCRPT(29)=' ROT 24-22     ELOSS= -0.0232 '
      SCRPT(30)=' ROT 25-23     ELOSS= -0.0242 '
      SCRPT(31)=' ROT 26-24     ELOSS= -0.0252 '
      SCRPT(32)=' ROT 27-25     ELOSS= -0.0261 '
      SCRPT(33)=' ROT 28-26     ELOSS= -0.0271 '
      SCRPT(34)=' ROT 29-27     ELOSS= -0.0281 '
      SCRPT(35)=' ROT 0-2       ELOSS=  0.00148'
      SCRPT(36)=' ROT 1-3       ELOSS=  0.00247'
      SCRPT(37)=' ROT 2-4       ELOSS=  0.00345'
      SCRPT(38)=' ROT 3-5       ELOSS=  0.00444'
      SCRPT(39)=' ROT 4-6       ELOSS=  0.00543'
      SCRPT(40)=' ROT 5-7       ELOSS=  0.00641'
      SCRPT(41)=' ROT 6-8       ELOSS=  0.00740'
      SCRPT(42)=' ROT 7-9       ELOSS=  0.00839'
      SCRPT(43)=' ROT 8-10      ELOSS=  0.00937'
      SCRPT(44)=' ROT 9-11      ELOSS=  0.0104 '
      SCRPT(45)=' ROT 10-12     ELOSS=  0.0113 '
      SCRPT(46)=' ROT 11-13     ELOSS=  0.0123 '
      SCRPT(47)=' ROT 12-14     ELOSS=  0.0133 '
      SCRPT(48)=' ROT 13-15     ELOSS=  0.0143 '
      SCRPT(49)=' ROT 14-16     ELOSS=  0.0153 '
      SCRPT(50)=' ROT 15-17     ELOSS=  0.0163 '
      SCRPT(51)=' ROT 16-18     ELOSS=  0.0173 '
      SCRPT(52)=' ROT 17-19     ELOSS=  0.0183 '
      SCRPT(53)=' ROT 18-20     ELOSS=  0.0192 '
      SCRPT(54)=' ROT 19-21     ELOSS=  0.0202 '
      SCRPT(55)=' ROT 20-22     ELOSS=  0.0212 '
      SCRPT(56)=' ROT 21-23     ELOSS=  0.0222 '
      SCRPT(57)=' ROT 22-24     ELOSS=  0.0232 '
      SCRPT(58)=' ROT 23-25     ELOSS=  0.0242 '
      SCRPT(59)=' ROT 24-26     ELOSS=  0.0252 '
      SCRPT(60)=' ROT 25-27     ELOSS=  0.0261 '
      SCRPT(61)=' ROT 26-28     ELOSS=  0.0271 '
      SCRPT(62)=' ROT 27-29     ELOSS=  0.0281 '
      SCRPT(63)=' VIB V1        ELOSS=  0.290  '
      SCRPT(64)=' VIB 2V1       ELOSS=  0.590  '
      SCRPT(65)=' VIB 3V1       ELOSS=  0.880  '
      SCRPT(66)=' VIB 4V1       ELOSS=  1.17   '
      SCRPT(67)=' VIB 5V1       ELOSS=  1.47   '
      SCRPT(68)=' VIB 6V1       ELOSS=  1.76   '
      SCRPT(69)=' VIB 7V1       ELOSS=  2.06   '
      SCRPT(70)=' VIB 8V1       ELOSS=  2.35   '
      SCRPT(71)=' EXC TRPLT1    ELOSS=  6.17   '
      SCRPT(72)=' EXC TRPLT3    ELOSS=  7.35   '
      SCRPT(73)=' EXC TRPLT5    ELOSS=  7.80   '
      SCRPT(74)=' EXC SNGLT2    ELOSS=  8.55   '
      SCRPT(75)=' EXC TRPLT7    ELOSS= 11.03   '
      SCRPT(76)=' EXC TRPLT8    ELOSS= 11.87   '
      SCRPT(77)=' EXC SNGLT5    ELOSS= 13.0    '
C
      EN=-ESTEP/2.0D0
      DO 900 I=1,NSTEP
      EN=EN+ESTEP
C
C ELASTIC (+ROTATIONAL)
      DO 10 J=2,NELA
      IF(EN.LE.XELA(J)) GO TO 15
   10 CONTINUE
      J=NELA
   15 A=(YELA(J)-YELA(J-1))/(XELA(J)-XELA(J-1))
      B=(XELA(J-1)*YELA(J)-XELA(J)*YELA(J-1))/(XELA(J-1)-XELA(J))
      QELA=(A*EN+B)*1.0D-16
C
C  MOMENTUM TRANSFER
      DO 20 J=2,NMOM
      IF(EN.LE.XMOM(J)) GO TO 25
   20 CONTINUE
      J=NMOM
   25 A=(YMOM(J)-YMOM(J-1))/(XMOM(J)-XMOM(J-1))
      B=(XMOM(J-1)*YMOM(J)-XMOM(J)*YMOM(J-1))/(XMOM(J-1)-XMOM(J))
      QMOM=(A*EN+B)*1.0D-16
C
      PEQEL(2,I)=0.5+(QELA-QMOM)/QELA
      Q(2,I)=QELA
C
      Q(3,I)=0.0D0
      PEQEL(3,I)=0.5D0
      IF(EN.LT.E(3)) GO TO 50
      DO 30 J=2,NION
      IF(EN.LE.XION(J)) GO TO 40
   30 CONTINUE
      J=NION
   40 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
C USE ANISOTROPIC SCATTERING FOR PRIMARY IONISATION ELECTRON FOR
C ENERGIES ABOVE 2 * IONISATION ENERGY
C ANISOTROPIC ANGULAR DISTRIBUTION SAME AS ELASTIC AT ENERGY OFFSET BY
C IONISATION ENERGY
      IF(EN.LE.(2.0*E(3))) GO TO 50
      PEQEL(3,I)=PEQEL(2,(I-IOFF))
C
   50 Q(4,I)=0.0D0
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C---------------------------------------------------------------------
C  QUADRUPOLE BORN ROTATIONAL STATES  ( GERJUOY AND STEIN)
C---------------------------------------------------------------------
C  SUPERELASTIC ROTATION
      DO 51 K=1,28
      AJ=DBLE(K+1)
   51 QIN(K,I)=PJ(K+1)*QBK*SQRT(1.0-EIN(K)/EN)*AJ*(AJ-1.0)/((2.0*AJ+1.0
     /)*(2.0*AJ-1.0))
C
      DO 52 K=29,56
   52 QIN(K,I)=0.0D0
C INELASTIC ROTATION
C   ROT 0-2
      IF(EN.LE.EIN(29)) GO TO 60
       QIN(29,I)=FROT0*QBK*SQRT(1.0-EIN(29)/EN)*2.0/3.0
C   ROT 1-3 AND HIGHER
      DO 53 K=30,56
      AJ=DBLE(K-29)
      IF(EN.LE.EIN(K)) GO TO 60
   53 QIN(K,I)=PJ(K-29)*QBK*SQRT(1.0-EIN(K)/EN)*(AJ+2.0)*(AJ+1.0)/((2.0
     /*AJ+3.0)*(2.0*AJ+1.0))
C BORN (1/E) FALL OFF IN ROTATIONAL X-SECS ABOVE 6.0 EV
   60 IF(EN.LT.6.0) GO TO 80
      DO 70 K=1,56
   70 QIN(K,I)=QIN(K,I)*6.0/EN
C
C---------------------------------------------------------------------
   80 CONTINUE
C---------------------------------------------------------------------
      QIN(57,I)=0.0D0
      IF(EN.LE.EIN(57)) GO TO 110
      DO 90 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 100
   90 CONTINUE
      J=NVIB1
  100 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(57,I)=(A*EN+B)*1.D-16
  110 CONTINUE
C
      QIN(58,I)=0.0D0
      IF(EN.LE.EIN(58)) GO TO 140
      DO 120 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 130
  120 CONTINUE
      J=NVIB2
  130 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(58,I)=(A*EN+B)*1.D-16
  140 CONTINUE
C
      QIN(59,I)=0.0D0
      IF(EN.LE.EIN(59)) GO TO 170
      DO 150 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 160
  150 CONTINUE
      J=NVIB3
  160 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(59,I)=(A*EN+B)*1.D-16
  170 CONTINUE
C
      QIN(60,I)=0.0D0
      IF(EN.LE.EIN(60)) GO TO 200
      DO 180 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 190
  180 CONTINUE
      J=NVIB4
  190 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(60,I)=(A*EN+B)*1.D-16
  200 CONTINUE
C
      QIN(61,I)=0.0D0
      IF(EN.LE.EIN(61)) GO TO 230
      DO 210 J=2,NVIB5
      IF(EN.LE.XVIB5(J)) GO TO 220
  210 CONTINUE
      J=NVIB5
  220 A=(YVIB5(J)-YVIB5(J-1))/(XVIB5(J)-XVIB5(J-1))
      B=(XVIB5(J-1)*YVIB5(J)-XVIB5(J)*YVIB5(J-1))/(XVIB5(J-1)-XVIB5(J))
      QIN(61,I)=(A*EN+B)*1.D-16
  230 CONTINUE
C
      QIN(62,I)=0.0D0
      IF(EN.LE.EIN(62)) GO TO 260
      DO 240 J=2,NVIB6
      IF(EN.LE.XVIB6(J)) GO TO 250
  240 CONTINUE
      J=NVIB6
  250 A=(YVIB6(J)-YVIB6(J-1))/(XVIB6(J)-XVIB6(J-1))
      B=(XVIB6(J-1)*YVIB6(J)-XVIB6(J)*YVIB6(J-1))/(XVIB6(J-1)-XVIB6(J))
      QIN(62,I)=(A*EN+B)*1.D-16
  260 CONTINUE
C
      QIN(63,I)=0.0D0
      IF(EN.LE.EIN(63)) GO TO 330
      DO 310 J=2,NVIB7
      IF(EN.LE.XVIB7(J)) GO TO 320
  310 CONTINUE
      J=NVIB7
  320 A=(YVIB7(J)-YVIB7(J-1))/(XVIB7(J)-XVIB7(J-1))
      B=(XVIB7(J-1)*YVIB7(J)-XVIB7(J)*YVIB7(J-1))/(XVIB7(J-1)-XVIB7(J))
      QIN(63,I)=(A*EN+B)*1.D-16
  330 CONTINUE
C
      QIN(64,I)=0.0D0
      IF(EN.LE.EIN(64)) GO TO 360
      DO 340 J=2,NVIB8
      IF(EN.LE.XVIB8(J)) GO TO 350
  340 CONTINUE
      J=NVIB8
  350 A=(YVIB8(J)-YVIB8(J-1))/(XVIB8(J)-XVIB8(J-1))
      B=(XVIB8(J-1)*YVIB8(J)-XVIB8(J)*YVIB8(J-1))/(XVIB8(J-1)-XVIB8(J))
      QIN(64,I)=(A*EN+B)*1.D-16
  360 CONTINUE
C
      QIN(65,I)=0.0D0
      IF(EN.LE.EIN(65)) GO TO 450
      DO 430 J=2,NTRP1
      IF(EN.LE.XTRP1(J)) GO TO 440
  430 CONTINUE
      J=NTRP1
  440 A=(YTRP1(J)-YTRP1(J-1))/(XTRP1(J)-XTRP1(J-1))
      B=(XTRP1(J-1)*YTRP1(J)-XTRP1(J)*YTRP1(J-1))/(XTRP1(J-1)-XTRP1(J))
      QIN(65,I)=(A*EN+B)*1.D-16
  450 CONTINUE
C
      QIN(66,I)=0.0D0
      IF(EN.LE.EIN(66)) GO TO 510
      DO 490 J=2,NTRP3
      IF(EN.LE.XTRP3(J)) GO TO 500
  490 CONTINUE
      J=NTRP3
  500 A=(YTRP3(J)-YTRP3(J-1))/(XTRP3(J)-XTRP3(J-1))
      B=(XTRP3(J-1)*YTRP3(J)-XTRP3(J)*YTRP3(J-1))/(XTRP3(J-1)-XTRP3(J))
      QIN(66,I)=(A*EN+B)*1.D-16
  510 CONTINUE
C
      QIN(67,I)=0.0D0
      IF(EN.LE.EIN(67)) GO TO 570
      DO 550 J=2,NTRP5
      IF(EN.LE.XTRP5(J)) GO TO 560
  550 CONTINUE
      J=NTRP5
  560 A=(YTRP5(J)-YTRP5(J-1))/(XTRP5(J)-XTRP5(J-1))
      B=(XTRP5(J-1)*YTRP5(J)-XTRP5(J)*YTRP5(J-1))/(XTRP5(J-1)-XTRP5(J))
      QIN(67,I)=(A*EN+B)*1.D-16
  570 CONTINUE
C
      QIN(68,I)=0.0D0
      IF(EN.LE.EIN(68)) GO TO 660
      DO 640 J=2,NSNG2
      IF(EN.LE.XSNG2(J)) GO TO 650
  640 CONTINUE
      J=NSNG2
  650 A=(YSNG2(J)-YSNG2(J-1))/(XSNG2(J)-XSNG2(J-1))
      B=(XSNG2(J-1)*YSNG2(J)-XSNG2(J)*YSNG2(J-1))/(XSNG2(J-1)-XSNG2(J))
      QIN(68,I)=(A*EN+B)*1.D-16
  660 CONTINUE
C
      QIN(69,I)=0.0D0
      IF(EN.LE.EIN(69)) GO TO 720
      DO 700 J=2,NTRP7
      IF(EN.LE.XTRP7(J)) GO TO 710
  700 CONTINUE
      J=NTRP7
  710 A=(YTRP7(J)-YTRP7(J-1))/(XTRP7(J)-XTRP7(J-1))
      B=(XTRP7(J-1)*YTRP7(J)-XTRP7(J)*YTRP7(J-1))/(XTRP7(J-1)-XTRP7(J))
      QIN(69,I)=0.6666*(A*EN+B)*1.D-16
  720 CONTINUE
C
      QIN(70,I)=0.0D0
      IF(EN.LE.EIN(70)) GO TO 750
      DO 730 J=2,NTRP8
      IF(EN.LE.XTRP8(J)) GO TO 740
  730 CONTINUE
      J=NTRP8
  740 A=(YTRP8(J)-YTRP8(J-1))/(XTRP8(J)-XTRP8(J-1))
      B=(XTRP8(J-1)*YTRP8(J)-XTRP8(J)*YTRP8(J-1))/(XTRP8(J-1)-XTRP8(J))
      QIN(70,I)=(A*EN+B)*1.D-16
  750 CONTINUE
C
      QIN(71,I)=0.0D0
      IF(EN.LE.EIN(71)) GO TO 810
      DO 790 J=2,NSNG5
      IF(EN.LE.XSNG5(J)) GO TO 800
  790 CONTINUE
      J=NSNG5
  800 A=(YSNG5(J)-YSNG5(J-1))/(XSNG5(J)-XSNG5(J-1))
      B=(XSNG5(J-1)*YSNG5(J)-XSNG5(J)*YSNG5(J-1))/(XSNG5(J-1)-XSNG5(J))
      QIN(71,I)=(A*EN+B)*1.D-16
  810 CONTINUE
C
C
      SUM=0.0D0
      DO 898 K=1,56
      SUM=SUM+QIN(K,I)
  898 CONTINUE
C GET CORRECT ELASTIC XSECTION BY SUBTRACTION OF ROTATION
      Q(2,I)=Q(2,I)-SUM
      SUM1=0.0D0
      DO 899 K=57,71
      SUM1=SUM1+QIN(K,I)
  899 CONTINUE
      Q(1,I)=Q(2,I)+Q(3,I)+SUM+SUM1
  900 CONTINUE
C SAVE COMPUTE TIME
      DO 1000 K=1,71
      J=72-K
      IF(EFINAL.LE.EIN(J)) NIN=J-1
 1000 CONTINUE
C
      END
