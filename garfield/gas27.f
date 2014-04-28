CDECK  ID>, GAS27.
      SUBROUTINE GAS27(Q,QIN,NIN,E,EIN,NAME,VIRIAL,EOBY
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
C    MAXWELL MODEL DECEMBER 1994
C ---------------------------------------------------------------
      NAME='Maxwell (1994)'
C
      NIN=0
      DO 1 J=1,6
    1 KEL(J)=0
C     DO 2 J=1,NIN
C   2 KIN(J)=0
      SIGC=6.0D-16
      E(1)=0.0
      E(2)=2.0*EMASS/(4.0*AMU)
      E(3)=99.
      E(4)=0.0
      E(5)=0.0
      E(6)=0.0
      EOBY=99.
      SCRPT(1)='                              '
      SCRPT(2)=' ELASTIC       MAXWELL        '
      SCRPT(3)='                              '
      SCRPT(4)='                              '
      SCRPT(5)='                              '
      SCRPT(6)='                              '
      EN=-ESTEP/2.0D0
      DO 9000 I=1,NSTEP
      EN=EN+ESTEP
      IF(EN.EQ.0.0) THEN
      Q(2,I)=100000.D-16
      GO TO 10
      ENDIF
      Q(2,I)=SIGC/SQRT(EN)
  10  Q(3,I)=0.0D0
      Q(4,I)=0.0D0
      Q(5,I)=0.0D0
      Q(6,I)=0.0D0
C
      Q(1,I)=Q(2,I)
 9000 CONTINUE
      END
