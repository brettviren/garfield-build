CDECK  ID>, GAS23.
      SUBROUTINE GAS23(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
      DIMENSION PEQEL(6,2048),PEQIN(220,2048),KIN(220),KEL(6)
      DIMENSION Q(6,2048),QIN(220,2048),E(6),EIN(220),PJ(40)
      DIMENSION XEN(53),YEN(53),XVIB1(52),YVIB1(52),XVIB2(27),YVIB2(27),
     /XVIB3(24),YVIB3(24),XVIB4(23),YVIB4(23),XVIB5(20),YVIB5(20),
     /XVIB6(19),YVIB6(19),XION(90),YION(90),XATT(52),YATT(52),
     /XEXC(30),YEXC(30),XEXC1(26),YEXC1(26),XEXC2(24),YEXC2(24),
     /XEXC3(22),YEXC3(22),XEXC4(21),YEXC4(21),XEXC5(19),YEXC5(19)
      CHARACTER*30 SCRPT(226)
      CHARACTER*15 NAME
C ELASTIC MOMENTUM TRANSFER
      DATA XEN/0.00,.001,.002,.003,.005,.007,.0085,0.01,.015,0.02,
     /0.03,0.04,0.05,0.07,0.10,0.12,0.14,0.16,0.18,0.20,
     /0.25,0.30,0.40,0.50,0.60,0.80,1.00,1.20,1.40,1.60,
     /1.80,2.00,2.50,3.00,4.00,5.00,6.00,7.00,8.00,10.0,
     /12.0,15.0,17.0,20.0,25.0,30.0,50.0,75.0,100.,200.,
     /1000.,10000.,100000./
      DATA YEN/0.51,0.90,1.08,1.24,1.50,1.72,1.85,2.00,2.42,2.77,
     /3.38,3.90,4.35,5.10,6.00,6.50,7.10,7.75,8.10,8.50,
     /10.7,12.4,14.7,15.6,16.1,16.4,17.5,22.4,30.1,36.2,
     /37.4,37.2,20.3,16.4,12.1,11.2,10.7,10.2,9.81,8.83,
     /8.48,8.38,8.08,7.58,6.59,5.79,3.59,2.29,1.70,1.00,
     /0.15,.015,.0015/
C
      DATA XVIB1/.266,.270,0.28,0.30,0.32,0.35,0.40,0.45,0.50,0.60,
     /0.70,0.80,0.85,0.90,0.95,1.00,1.05,1.10,1.22,1.31,
     /1.41,1.51,1.65,1.74,1.82,1.90,1.98,2.09,2.17,2.28,
     /2.32,2.40,2.51,2.69,2.87,3.07,3.29,3.53,3.82,4.00,
     /5.00,6.00,8.00,10.0,12.0,15.0,20.0,30.0,100.,1000.,
     /10000.,100000./
      DATA YVIB1/0.00,.045,.081,.117,.131,.153,.165,.168,.167,.155,
     /.135,.118,.112,.115,.120,.130,.196,.320,0.77,1.31,
     /2.30,3.44,3.23,3.80,4.20,3.74,3.34,3.64,3.18,2.67,
     /2.74,2.39,2.00,1.57,1.17,0.83,0.55,0.35,0.18,.051,
     /.043,.037,.030,.025,.022,.018,.014,.010,.0037,.00037,
     /.000037,.0000037/
      DATA XVIB2/.528,1.10,1.20,1.30,1.40,1.50,1.60,1.70,1.80,1.90,
     /2.00,2.10,2.20,2.30,2.40,2.50,2.60,2.70,2.80,2.90,
     /3.00,3.20,10.0,100.,1000.,10000.,100000./
      DATA YVIB2/0.00,.027,.055,.135,.495,1.11,1.66,1.43,1.22,1.66,
     /1.43,1.14,1.15,0.91,0.67,0.67,0.44,0.39,0.22,0.22,
     /0.11,.055,.005,.0005,.00005,.000005,.0000006/
      DATA XVIB3/.787,1.40,1.50,1.60,1.70,1.80,1.90,2.00,2.10,2.20,
     /2.30,2.40,2.50,2.60,2.70,2.80,2.90,3.00,3.20,10.0,
     /100.,1000.,10000.,100000./
      DATA YVIB3/0.00,.055,0.28,0.77,1.08,0.83,0.49,0.72,0.83,0.44,
     /0.39,0.44,0.22,0.25,0.17,0.11,0.12,.055,.022,.0022,
     /.00022,.000022,.0000022,.00000022/
      DATA XVIB4/1.043,1.40,1.50,1.60,1.70,1.80,1.90,2.00,2.10,2.20,
     /2.30,2.40,2.50,2.60,2.70,2.80,2.90,3.00,10.0,100.,
     /1000.,10000.,100000./
      DATA YVIB4/0.00,.013,0.11,0.25,0.61,0.77,0.61,0.20,0.32,0.41,
     /0.22,0.12,0.20,.045,.045,.012,.0032,.0027,.0003,.00003,
     /.000003,.0000003,.00000003/
      DATA XVIB5/1.295,1.60,1.70,1.80,1.90,2.00,2.10,2.20,2.30,2.40,
     /2.50,2.60,2.70,2.80,3.00,10.0,100.,1000.,10000.,100000./
      DATA YVIB5/0.00,.055,0.29,0.32,0.54,0.32,0.11,.049,0.20,.072,
     /.045,.045,.009,.004,.002,.0002,.00002,.000002,.0000002,.00000002/
      DATA XVIB6/1.544,1.70,1.80,1.90,2.00,2.10,2.20,2.30,2.40,2.50,
     /2.60,2.70,2.80,3.00,10.0,100.,1000.,10000.,100000./
      DATA YVIB6/0.00,.049,0.13,0.22,0.61,0.61,0.45,0.34,0.20,0.14,
     /0.13,.042,.014,.0045,.0005,.00005,.000005,.0000005,.00000005/
C
      DATA XION/14.013,14.5,15.0,15.5,16.0,16.5,17.0,17.5,18.0,18.5,
     /19.0,19.5,20.0,20.5,21.0,21.5,22.0,22.5,23.0,23.5,
     /24.0,26.0,28.0,30.0,32.0,34.0,36.0,38.0,40.0,45.0,
     /50.0,55.0,60.0,65.0,70.0,75.0,80.0,85.0,90.0,95.0,
     /100.,105.,110.,115.,120.,125.,130.,135.,140.,145.,
     /150.,160.,180.,200.,250.,300.,350.,400.,450.,500.,
     /550.,600.,650.,700.,750.,800.,850.,900.,950.,1000.,
     /1100.,1200.,1300.,1400.,1500.,1600.,1800.,2000.,2500.,3000.,
     /4000.,6000.,8000.,10000.,15000.,20000.,30000.,40000.,60000.,
     /100000./
      DATA YION/0.00,.0273,.051,.077,.106,.139,.177,.214,.254,.297,
     /.340,.386,.428,.472,.516,.560,.601,.643,.684,.724,
     /.766,.933,1.09,1.24,1.38,1.50,1.60,1.70,1.79,1.97,
     /2.12,2.24,2.34,2.43,2.50,2.53,2.59,2.60,2.63,2.64,
     /2.65,2.66,2.66,2.65,2.64,2.63,2.62,2.60,2.59,2.58,
     /2.57,2.52,2.45,2.37,2.16,1.99,1.85,1.72,1.59,1.50,
     /1.43,1.35,1.27,1.21,1.15,1.11,1.06,1.03,.994,.959,
     /.864,.810,.762,.721,.683,.650,.592,.545,.456,.392,
     /.309,.219,.172,.141,.099,.077,.054,.042,.029,
     /.019/
      DATA XATT/9.00,9.20,9.30,9.35,9.40,9.45,9.60,9.65,9.70,9.75,
     /9.80,9.85,9.90,10.0,10.1,10.2,10.3,10.4,10.5,10.6,
     /10.7,10.8,10.9,11.0,11.1,11.2,11.3,11.4,11.5,11.6,
     /11.7,11.8,11.9,12.0,12.1,12.2,12.3,12.4,12.5,12.6,
     /12.8,13.0,19.0,25.0,30.0,35.0,40.0,60.0,100.,1000.,
     /10000.,100000./
      DATA YATT/0.00,.00009,.00018,.00026,.00034,.00073,.0011,.0017,
     /.0018,.0019,.0020,.0020,.0020,.0020,.0020,.0019,.0018,.0017,.0015,
     /.0014,.0012,.0011,.0010,.00088,.00077,.00065,.00055,.00047,.00040,
     /.00033,.00028,.00024,.00019,.00017,.00014,.00011,.00010,.00009,
     /.00008,.00007,.00006,.00006,.00006,.00006,.0001,.0001,.0001,.0001,
     /.0001,.0001,.0000001,.00000001/
C  EXCITATION A3 PI
      DATA XEXC/6.04,6.20,6.40,6.60,7.00,7.15,8.00,9.00,10.0,11.0,
     /12.0,13.0,14.0,15.0,17.0,20.0,22.0,24.0,27.0,30.0,
     /35.0,40.0,50.0,60.0,70.0,80.0,100.,1000.,10000.,100000./
      DATA YEXC/0.00,2.04,2.09,2.04,0.55,0.29,0.53,0.94,1.06,1.08,
     /1.02,0.92,0.81,0.71,0.55,0.39,0.34,0.29,.245,0.22,
     /0.21,0.20,0.18,0.17,0.15,0.14,.127,.028,.0028,.00028/
C EXCITATION  A3 SIGMA
      DATA XEXC1/6.82,7.00,8.00,9.00,10.0,11.0,12.0,13.0,14.0,15.0,
     /17.0,20.0,22.0,24.0,27.0,30.0,35.0,40.0,50.0,60.0,
     /70.0,80.0,100.0,1000.,10000.,100000./
      DATA YEXC1/0.00,.013,0.07,0.34,0.46,0.50,0.49,0.46,0.42,0.38,
     /0.32,0.25,0.21,0.18,0.15,.118,.084,.056,.031,.018,
     /.0118,.007,.003,.00014,.000014,.0000014/
C EXCITATION  A1 PI
      DATA XEXC2/8.07,9.00,10.0,11.0,12.0,13.0,14.0,15.0,17.0,20.0,
     /22.0,24.0,27.0,30.0,35.0,40.0,50.0,60.0,70.0,80.0,
     /100.,1000.,10000.,100000./
      DATA YEXC2/0.00,.108,0.18,0.24,0.27,0.29,0.32,0.35,0.38,0.39,
     /0.40,0.42,0.42,0.41,0.40,0.39,0.38,0.36,0.35,0.34,
     /0.31,.084,.0084,.00084/
C  EXCITATION B3 SIGMA
      DATA XEXC3/10.39,11.0,12.0,13.0,14.0,15.0,17.0,20.0,22.0,24.0,
     /27.0,30.0,35.0,40.0,50.0,60.0,70.0,80.0,100.,1000.,
     /10000.,100000./
      DATA YEXC3/0.00,.025,.035,.055,.066,.074,.077,.060,.042,.028,
     /.018,.015,.0137,.0127,.0118,.0118,.0108,.0108,.0099,.0014,
     /.00014,.000014/
C  EXCITATION C1 SIGMA +E1 PI
      DATA XEXC4/11.3,12.0,13.0,14.0,15.0,17.0,20.0,22.0,24.0,27.0,
     /30.0,35.0,40.0,50.0,60.0,70.0,80.0,100.,1000.,10000.,
     /100000./
      DATA YEXC4/0.00,.056,.087,0.12,0.14,.175,0.22,0.24,0.25,0.27,
     /0.28,0.28,0.28,0.27,0.25,.245,0.24,0.22,.063,.0063,
     /.00063/
C   EXCITATION   SUM OF HIGHER LEVELS
      DATA XEXC5/13.5,14.0,15.0,17.0,20.0,22.0,24.0,27.0,30.0,35.0,
     /40.0,50.0,60.0,70.0,80.0,100.,1000.,10000.,100000./
      DATA YEXC5/0.00,0.07,0.14,0.29,0.39,0.42,0.45,0.48,0.49,0.50,
     /0.52,0.52,0.50,0.49,0.48,0.46,0.13,.013,.0013/
C -------------------------------------------------------------------
C  FIT TO DATA OF :
C             HADDAD AND MILLOY      AUST J. PHYS 36(1983)473
C             PETROVIC AND CROMPTON  AUST J. PHYS 42(1989)609
C             NAKAMURA               J.PHYS D 20(1987) 933
C             SAELEE AND LUCAS       J.PHYS D 10(1977) 343
C     AND LOW TEMPERTURE PACK AND PHELPS DATA
C    REPLACES 1998 ROUTINE
C  USES ANISTROPIC ANGULAR DISTRIBUTION FOR DIPOLE ROTATIONAL STATES
C -------------------------------------------------------------------
C
      NAME='CO (2003)'
C
      NIN=64
      DO 1 J=1,6
    1 KEL(J)=0
      DO 2 J=1,NIN
      KIN(J)=0
    2 IF(J.LE.52) KIN(J)=1
      NDATA=53
      NVIB1=52
      NVIB2=27
      NVIB3=24
      NVIB4=23
      NVIB5=20
      NVIB6=19
      NION=90
      NATT=52
      NEXC=30
      NEXC1=26
      NEXC2=24
      NEXC3=22
      NEXC4=21
      NEXC5=19
      E(1)=0.0
      E(2)=2.0*EMASS/(28.0104*AMU)
      E(3)=14.013
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=14.013
C   B0 IS ROTATIONAL CONSTANT AND DBA IS DIPOLE MOMENT
C   DRAT IS RATIO OF MOMENTUM TRANSFER TO TOTAL X-SECTION FOR DIPOLE
C ------------------------------------------------------
      B0=2.384D-4
      DBA=0.0432
      DRAT=0.25
      AVIB=1.0
C -------------------------------------------------------
      A0=0.5291772083D-8
      RY=13.60569172
      DBK=8.37758*RY*(DBA*A0)**2
C CALCULATE ROTATIONAL STATE POPULATION AT TEMPERATURE
      DO 3 K=1,26
    3 PJ(K)=(2*K+1)*EXP(-K*(K+1)*B0/AKT)
      SUM=1.0
      DO 4 K=1,26
    4 SUM=SUM+PJ(K)
      FROT0=1.0D0/SUM
      DO 5 K=1,26
    5 PJ(K)=PJ(K)/SUM
C CALC ROTATIONAL TRANSITION ENERGIES
      DO 6 K=1,26
      J=K-1
      EIN(K+26)=B0*2*(J+1)
    6 EIN(K)=-EIN(K+26)
      EIN(53)=0.266
      EIN(54)=0.528
      EIN(55)=0.787
      EIN(56)=1.043
      EIN(57)=1.295
      EIN(58)=1.544
      EIN(59)=6.04
      EIN(60)=6.82
      EIN(61)=8.07
      EIN(62)=10.39
      EIN(63)=11.3
      EIN(64)=13.5
C     IF(LBMCPR)WRITE(LUNOUT,99) FROT0,(PJ(J),J=1,30)
C  99 FORMAT(2X,'POP OF STATES=',/,11(2X,D10.3))
C     IF(LBMCPR)WRITE(LUNOUT,98) (EIN(J),J=1,64)
C  98 FORMAT(2X,'TRANS ENERGY=',/,10(2X,D10.3))
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       CARBON MONOXIDE'
      SCRPT(3)=' IONISATION    ELOSS= 14.013  '
      SCRPT(4)=' ATTACHMENT                   '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      SCRPT(7)=' ROT 1-0       ELOSS= -0.00048'
      SCRPT(8)=' ROT 2-1       ELOSS= -0.00095'
      SCRPT(9)=' ROT 3-2       ELOSS= -0.00143'
      SCRPT(10)=' ROT 4-3       ELOSS= -0.00191'
      SCRPT(11)=' ROT 5-4       ELOSS= -0.00238'
      SCRPT(12)=' ROT 6-5       ELOSS= -0.00286'
      SCRPT(13)=' ROT 7-6       ELOSS= -0.00334'
      SCRPT(14)=' ROT 8-7       ELOSS= -0.00381'
      SCRPT(15)=' ROT 9-8       ELOSS= -0.00429'
      SCRPT(16)=' ROT 10-9      ELOSS= -0.00477'
      SCRPT(17)=' ROT 11-10     ELOSS= -0.00524'
      SCRPT(18)=' ROT 12-11     ELOSS= -0.00572'
      SCRPT(19)=' ROT 13-12     ELOSS= -0.00620'
      SCRPT(20)=' ROT 14-13     ELOSS= -0.00668'
      SCRPT(21)=' ROT 15-14     ELOSS= -0.00715'
      SCRPT(22)=' ROT 16-15     ELOSS= -0.00763'
      SCRPT(23)=' ROT 17-16     ELOSS= -0.00811'
      SCRPT(24)=' ROT 18-17     ELOSS= -0.00858'
      SCRPT(25)=' ROT 19-18     ELOSS= -0.00906'
      SCRPT(26)=' ROT 20-19     ELOSS= -0.00954'
      SCRPT(27)=' ROT 21-20     ELOSS= -0.0100 '
      SCRPT(28)=' ROT 22-21     ELOSS= -0.0105 '
      SCRPT(29)=' ROT 23-22     ELOSS= -0.0110 '
      SCRPT(30)=' ROT 24-23     ELOSS= -0.0114 '
      SCRPT(31)=' ROT 25-24     ELOSS= -0.0119 '
      SCRPT(32)=' ROT 26-25     ELOSS= -0.0124 '
      SCRPT(33)=' ROT 0-1       ELOSS=  0.00048'
      SCRPT(34)=' ROT 1-2       ELOSS=  0.00095'
      SCRPT(35)=' ROT 2-3       ELOSS=  0.00143'
      SCRPT(36)=' ROT 3-4       ELOSS=  0.00191'
      SCRPT(37)=' ROT 4-5       ELOSS=  0.00238'
      SCRPT(38)=' ROT 5-6       ELOSS=  0.00286'
      SCRPT(39)=' ROT 6-7       ELOSS=  0.00334'
      SCRPT(40)=' ROT 7-8       ELOSS=  0.00381'
      SCRPT(41)=' ROT 8-9       ELOSS=  0.00429'
      SCRPT(42)=' ROT 9-10      ELOSS=  0.00477'
      SCRPT(43)=' ROT 10-11     ELOSS=  0.00524'
      SCRPT(44)=' ROT 11-12     ELOSS=  0.00572'
      SCRPT(45)=' ROT 12-13     ELOSS=  0.00620'
      SCRPT(46)=' ROT 13-14     ELOSS=  0.00668'
      SCRPT(47)=' ROT 14-15     ELOSS=  0.00715'
      SCRPT(48)=' ROT 15-16     ELOSS=  0.00763'
      SCRPT(49)=' ROT 16-17     ELOSS=  0.00811'
      SCRPT(50)=' ROT 17-18     ELOSS=  0.00858'
      SCRPT(51)=' ROT 18-19     ELOSS=  0.00906'
      SCRPT(52)=' ROT 19-20     ELOSS=  0.00954'
      SCRPT(53)=' ROT 20-21     ELOSS=  0.0100 '
      SCRPT(54)=' ROT 21-22     ELOSS=  0.0105 '
      SCRPT(55)=' ROT 22-23     ELOSS=  0.0110 '
      SCRPT(56)=' ROT 23-24     ELOSS=  0.0114 '
      SCRPT(57)=' ROT 24-25     ELOSS=  0.0119 '
      SCRPT(58)=' ROT 25-26     ELOSS=  0.0124 '
      SCRPT(59)=' VIB V1        ELOSS=  0.266  '
      SCRPT(60)=' VIB 2V1       ELOSS=  0.528  '
      SCRPT(61)=' VIB 3V1       ELOSS=  0.787  '
      SCRPT(62)=' VIB 4V1       ELOSS=  1.043  '
      SCRPT(63)=' VIB 5V1       ELOSS=  1.295  '
      SCRPT(64)=' VIB 6V1       ELOSS=  1.544  '
      SCRPT(65)=' EXC A3 PI     ELOSS=  6.04   '
      SCRPT(66)=' EXC A3 SIGMA  ELOSS=  6.82   '
      SCRPT(67)=' EXC A1 PI     ELOSS=  8.07   '
      SCRPT(68)=' EXC B3 SIGMA  ELOSS= 10.39   '
      SCRPT(69)=' EXC C1 + E1   ELOSS= 11.3    '
      SCRPT(70)=' EXC           ELOSS= 13.5    '
      EN=-ESTEP/2.0D0
      DO 9000 I=1,NSTEP
      EN=EN+ESTEP
      DO 10 J=2,NDATA
      IF(EN.LE.XEN(J)) GO TO 20
   10 CONTINUE
      J=NDATA
   20 A=(YEN(J)-YEN(J-1))/(XEN(J)-XEN(J-1))
      B=(XEN(J-1)*YEN(J)-XEN(J)*YEN(J-1))/(XEN(J-1)-XEN(J))
      Q(2,I)=(A*EN+B)*1.0D-16
C
      Q(3,I)=0.0D0
      IF(EN.LT.E(3)) GO TO 50
      DO 30 J=2,NION
      IF(EN.LE.XION(J)) GO TO 40
   30 CONTINUE
      J=NION
   40 A=(YION(J)-YION(J-1))/(XION(J)-XION(J-1))
      B=(XION(J-1)*YION(J)-XION(J)*YION(J-1))/(XION(J-1)-XION(J))
      Q(3,I)=(A*EN+B)*1.D-16
C
   50 Q(4,I)=0.0D0
      IF(EN.LT.XATT(1)) GO TO 55
      IF(EN.GT.XATT(NATT)) GO TO 55
      DO 51 J=2,NATT
      IF(EN.LE.XATT(J)) GO TO 52
   51 CONTINUE
      J=NATT
   52 A=(YATT(J)-YATT(J-1))/(XATT(J)-XATT(J-1))
      B=(XATT(J-1)*YATT(J)-XATT(J)*YATT(J-1))/(XATT(J-1)-XATT(J))
      Q(4,I)=(A*EN+B)*1.D-16
C
   55 Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C----------------------------------------------------------------------
C DIPOLE BORN ROTATIONAL STATES
C----------------------------------------------------------------------
      ENRT=SQRT(EN)
C SUPER ELASTIC ROTATIONAL COLLISIONS
      DO 150 L=1,26
      AL=DBLE(L)
      QIN(L,I)=PJ(L)*DBK*LOG((ENRT+SQRT(EN-EIN(L)))/(SQRT(EN-EIN(L))-
     /ENRT))*AL/((2.0*AL+1.0)*EN)
  150 PEQIN(L,I)=0.5+(QIN(L,I)-DRAT*QIN(L,I))/QIN(L,I)
      DO 155 L=27,52
  155 QIN(L,I)=0.0D0
C ROT 0-1
      IF(EN.LE.EIN(27)) GO TO 200
      QIN(27,I)=FROT0*DBK*LOG((ENRT+SQRT(EN-EIN(27)))/(ENRT-SQRT(EN-
     /EIN(27))))/EN
      PEQIN(27,I)=0.5+(QIN(27,I)-DRAT*QIN(27,I))/QIN(27,I)
C ROT 1-2 AND HIGHER
      DO 160 L=28,52
      IF(EN.LE.EIN(L)) GO TO 200
      AL=DBLE(L-27)
      QIN(L,I)=PJ(L-27)*DBK*LOG((ENRT+SQRT(EN-EIN(L)))/(ENRT-SQRT(EN-
     /EIN(L))))*(AL+1.0)/((2.0*AL+1.0)*EN)
  160 PEQIN(L,I)=0.5+(QIN(L,I)-DRAT*QIN(L,I))/QIN(L,I)
C
  200 CONTINUE
C
      QIN(53,I)=0.0D0
      IF(EN.LE.EIN(53)) GO TO 400
      DO 310 J=2,NVIB1
      IF(EN.LE.XVIB1(J)) GO TO 320
  310 CONTINUE
      J=NVIB1
  320 A=(YVIB1(J)-YVIB1(J-1))/(XVIB1(J)-XVIB1(J-1))
      B=(XVIB1(J-1)*YVIB1(J)-XVIB1(J)*YVIB1(J-1))/(XVIB1(J-1)-XVIB1(J))
      QIN(53,I)=(A*EN+B)*1.D-16*AVIB
  400 CONTINUE
C
      QIN(54,I)=0.0D0
      IF(EN.LE.EIN(54)) GO TO 500
      DO 410 J=2,NVIB2
      IF(EN.LE.XVIB2(J)) GO TO 420
  410 CONTINUE
      J=NVIB2
  420 A=(YVIB2(J)-YVIB2(J-1))/(XVIB2(J)-XVIB2(J-1))
      B=(XVIB2(J-1)*YVIB2(J)-XVIB2(J)*YVIB2(J-1))/(XVIB2(J-1)-XVIB2(J))
      QIN(54,I)=(A*EN+B)*1.D-16
  500 CONTINUE
C
      QIN(55,I)=0.0D0
      IF(EN.LE.EIN(55)) GO TO 600
      DO 510 J=2,NVIB3
      IF(EN.LE.XVIB3(J)) GO TO 520
  510 CONTINUE
      J=NVIB3
  520 A=(YVIB3(J)-YVIB3(J-1))/(XVIB3(J)-XVIB3(J-1))
      B=(XVIB3(J-1)*YVIB3(J)-XVIB3(J)*YVIB3(J-1))/(XVIB3(J-1)-XVIB3(J))
      QIN(55,I)=(A*EN+B)*1.D-16
  600 CONTINUE
C
      QIN(56,I)=0.0D0
      IF(EN.LE.EIN(56)) GO TO 700
      DO 610 J=2,NVIB4
      IF(EN.LE.XVIB4(J)) GO TO 620
  610 CONTINUE
      J=NVIB4
  620 A=(YVIB4(J)-YVIB4(J-1))/(XVIB4(J)-XVIB4(J-1))
      B=(XVIB4(J-1)*YVIB4(J)-XVIB4(J)*YVIB4(J-1))/(XVIB4(J-1)-XVIB4(J))
      QIN(56,I)=(A*EN+B)*1.D-16
  700 CONTINUE
C
      QIN(57,I)=0.0D0
      IF(EN.LE.EIN(57)) GO TO 800
      DO 710 J=2,NVIB5
      IF(EN.LE.XVIB5(J)) GO TO 720
  710 CONTINUE
      J=NVIB5
  720 A=(YVIB5(J)-YVIB5(J-1))/(XVIB5(J)-XVIB5(J-1))
      B=(XVIB5(J-1)*YVIB5(J)-XVIB5(J)*YVIB5(J-1))/(XVIB5(J-1)-XVIB5(J))
      QIN(57,I)=(A*EN+B)*1.D-16
  800 CONTINUE
C
      QIN(58,I)=0.0D0
      IF(EN.LE.EIN(58)) GO TO 900
      DO 810 J=2,NVIB6
      IF(EN.LE.XVIB6(J)) GO TO 820
  810 CONTINUE
      J=NVIB6
  820 A=(YVIB6(J)-YVIB6(J-1))/(XVIB6(J)-XVIB6(J-1))
      B=(XVIB6(J-1)*YVIB6(J)-XVIB6(J)*YVIB6(J-1))/(XVIB6(J-1)-XVIB6(J))
      QIN(58,I)=(A*EN+B)*1.D-16
  900 CONTINUE
C
      QIN(59,I)=0.0D0
      IF(EN.LE.EIN(59)) GO TO 1000
      DO 910 J=2,NEXC
      IF(EN.LE.XEXC(J)) GO TO 920
  910 CONTINUE
      J=NEXC
  920 A=(YEXC(J)-YEXC(J-1))/(XEXC(J)-XEXC(J-1))
      B=(XEXC(J-1)*YEXC(J)-XEXC(J)*YEXC(J-1))/(XEXC(J-1)-XEXC(J))
      QIN(59,I)=(A*EN+B)*1.D-16
 1000 CONTINUE
C
      QIN(60,I)=0.0D0
      IF(EN.LE.EIN(60)) GO TO 1100
      DO 1010 J=2,NEXC1
      IF(EN.LE.XEXC1(J)) GO TO 1020
 1010 CONTINUE
      J=NEXC1
 1020 A=(YEXC1(J)-YEXC1(J-1))/(XEXC1(J)-XEXC1(J-1))
      B=(XEXC1(J-1)*YEXC1(J)-XEXC1(J)*YEXC1(J-1))/(XEXC1(J-1)-XEXC1(J))
      QIN(60,I)=(A*EN+B)*1.D-16
 1100 CONTINUE
C
      QIN(61,I)=0.0D0
      IF(EN.LE.EIN(61)) GO TO 1200
      DO 1110 J=2,NEXC2
      IF(EN.LE.XEXC2(J)) GO TO 1120
 1110 CONTINUE
      J=NEXC2
 1120 A=(YEXC2(J)-YEXC2(J-1))/(XEXC2(J)-XEXC2(J-1))
      B=(XEXC2(J-1)*YEXC2(J)-XEXC2(J)*YEXC2(J-1))/(XEXC2(J-1)-XEXC2(J))
      QIN(61,I)=(A*EN+B)*1.D-16
 1200 CONTINUE
C
      QIN(62,I)=0.0D0
      IF(EN.LE.EIN(62)) GO TO 1300
      DO 1210 J=2,NEXC3
      IF(EN.LE.XEXC3(J)) GO TO 1220
 1210 CONTINUE
      J=NEXC3
 1220 A=(YEXC3(J)-YEXC3(J-1))/(XEXC3(J)-XEXC3(J-1))
      B=(XEXC3(J-1)*YEXC3(J)-XEXC3(J)*YEXC3(J-1))/(XEXC3(J-1)-XEXC3(J))
      QIN(62,I)=(A*EN+B)*1.D-16
 1300 CONTINUE
C
      QIN(63,I)=0.0D0
      IF(EN.LE.EIN(63)) GO TO 1400
      DO 1310 J=2,NEXC4
      IF(EN.LE.XEXC4(J)) GO TO 1320
 1310 CONTINUE
      J=NEXC4
 1320 A=(YEXC4(J)-YEXC4(J-1))/(XEXC4(J)-XEXC4(J-1))
      B=(XEXC4(J-1)*YEXC4(J)-XEXC4(J)*YEXC4(J-1))/(XEXC4(J-1)-XEXC4(J))
      QIN(63,I)=(A*EN+B)*1.D-16
 1400 CONTINUE
C
      QIN(64,I)=0.0D0
      IF(EN.LE.EIN(64)) GO TO 1500
      DO 1410 J=2,NEXC5
      IF(EN.LE.XEXC5(J)) GO TO 1420
 1410 CONTINUE
      J=NEXC5
 1420 A=(YEXC5(J)-YEXC5(J-1))/(XEXC5(J)-XEXC5(J-1))
      B=(XEXC5(J-1)*YEXC5(J)-XEXC5(J)*YEXC5(J-1))/(XEXC5(J-1)-XEXC5(J))
      QIN(64,I)=(A*EN+B)*1.D-16
 1500 CONTINUE
C
      SUM=0.0D0
      DO 2000 K=1,64
      SUM=SUM+QIN(K,I)
 2000 CONTINUE
      Q(1,I)=Q(2,I)+Q(3,I)+Q(4,I)+SUM
 9000 CONTINUE
C  SAVE COMPUTE TIME
      DO 9900 K=1,64
      J=65-K
      IF(EFINAL.LE.EIN(J))  NIN=J-1
 9900 CONTINUE
C
      END
