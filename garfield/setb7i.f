CDECK  ID>, SETB7I.
      SUBROUTINE SETB7I
*-----------------------------------------------------------------------
*   SETB7I - Oiginally part of SETUP.
*   Author: Originally from Steve Biagi, extensively modified.
*   (Last changed on 21/12/05.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
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
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
       DOUBLE PRECISION ALPHA,ATT
       COMMON /CTOWNS/ ALPHA,ATT
       DOUBLE PRECISION ALPER,ATTER
       COMMON /CTWNER/ ALPER,ATTER
       DOUBLE PRECISION DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
       COMMON /DIFERB/ DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
       DOUBLE PRECISION DFLER,DFTER
       COMMON /DIFERL/ DFLER,DFTER
       DOUBLE PRECISION DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
       COMMON /DIFLAB/ DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
       DOUBLE PRECISION DIFLN,DIFTR
       COMMON /DIFVEL/ DIFLN,DIFTR
       DOUBLE PRECISION WX,WY,WZ
       COMMON /VEL/ WX,WY,WZ
       DOUBLE PRECISION DWX,DWY,DWZ
       COMMON /VELERR/ DWX,DWY,DWZ
       DOUBLE PRECISION ZTOT,TTOT,ZTOTS,TTOTS
       COMMON/TTRM/ZTOT,TTOT,ZTOTS,TTOTS
       DOUBLE PRECISION CON
       INTEGER ITHRM
       COMMON /THRM/ CON,ITHRM
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       DOUBLE PRECISION EOVB,WB,BTHETA,BMAG
       COMMON/BFLD/EOVB,WB,BTHETA,BMAG
      INTEGER J,K
*** Angles.
      THETA=0.785D0
      PHI=0.1D0
*** Zero common blocks of output results
      WX=0.0D0
      WY=0.0D0
      WZ=0.0D0
      DWX=0.0D0
      DWY=0.0D0
      DWZ=0.0D0
      TTOTS=0.0D0
      ALPHA=0.0D0
      ATT=0.0D0
      ALPER=0.0D0
      ATTER=0.0D0
      DIFLN=0.0D0
      DIFTR=0.0D0
      DFLER=0.0D0
      DFTER=0.0D0
      DIFXX=0.0D0
      DIFYY=0.0D0
      DIFZZ=0.0D0
      DIFYZ=0.0D0
      DIFXY=0.0D0
      DIFXZ=0.0D0
      DXXER=0.0D0
      DYYER=0.0D0
      DZZER=0.0D0
      DYZER=0.0D0
      DXYER=0.0D0
      DXZER=0.0D0
      DO 65 J=1,300
   65 TIME(J)=0.0D0
      DO 70 K=1,5*mxngas
   70 ICOLL(K)=0
      DO 80 K=1,512
   80 ICOLN(K)=0
      DO 100 K=1,2048
  100 SPEC(K)=0.0D0
      DO 101 K=1,8
  101 TCFMAX(K)=0.0D0
*** Can set random number seed to seed value here
      RSTART=0.666D0
C     RANDOM NUMBER SEED FUNCTION (RSTART)
*** Final energy
      ESTART=EFINAL/50.0D0
      ITHRM=0
*** Calculate thermal velocity distribution integrals
C     CON=1.0D-13/SQRT(AMU/(2.0D0*BOLTZJ*(TEMPC+ABZERO)))
C N.B. Loaded error function integrals in data array .
*** Radians per picosecond
      WB=AWB*BMAG*1.0D-12
*** Metres per picosecond
      IF(BMAG.NE.0.0D0)EOVB=EMAG*1.D-9/BMAG
      END
