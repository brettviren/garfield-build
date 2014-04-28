CDECK  ID>, OUTPUT.
      SUBROUTINE OUTPUT
      IMPLICIT REAL*8 (A-H,O-Z)
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
*   Grouped QIN1 ... QIN6 in QINN
       DOUBLE PRECISION QELM,QSUM,QION,QINN,QSATT
       COMMON /MIX1/ QELM(2048),QSUM(2048),QION(mxngas,2048),
     -      QINn(220,2048,mxngas),QSATT(2048)
*   EVECT is originally called E or ES depending on the routine.
       DOUBLE PRECISION Evect,EROOT,QTOT,QREL,QINEL,QEL
       COMMON /MIX2/ Evect(2048),EROOT(2048),QTOT(2048),QREL(2048),
     -      QINEL(2048),QEL(2048)
*   Extensively reduced.
       INTEGER NINn
*           ,LION,LIN1,LIN2,LIN3,LIN4,LIN5,LIN6
*           DOUBLE PRECISION ALION,ALIN1,ALIN2,ALIN3,ALIN4,ALIN5,ALIN6
       COMMON /MIX3/ NINn(mxngas)
*           ,LION(6),LIN1(220),
*     -      LIN2(220),LIN3(220),LIN4(220),LIN5(220),LIN6(220),ALION(6),
*     -      ALIN1(220),ALIN2(220),ALIN3(220),ALIN4(220),ALIN5(220),
*     -      ALIN6(220)
*   Grouped AN1 ... AN6 in ANn
       DOUBLE PRECISION ANn,AN,FRAC
       COMMON /RATIO/ ANn(mxngas),AN,FRAC(mxngas)
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
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
       DOUBLE PRECISION EOVB,WB,BTHETA,BMAG
       COMMON/BFLD/EOVB,WB,BTHETA,BMAG
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       DOUBLE PRECISION SIMF
       COMMON/SINT/SIMF(2048)
*   Changed name of common from /NAMES/ to /MBGNAM/ for Mac OS X
       CHARACTER*15 NAMEG
       COMMON /MBGNAM/ NAMEG(mxngas)
       CHARACTER*30 DSCRPT
       COMMON/SCRIP/DSCRPT(512)
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
      if(lbmcpr)WRITE(lunout,15)
      IF(LBMCPR)WRITE(LUNOUT,15)
   15 FORMAT('----------------------------------------------------------
     /-------------------')
      NREAL=NMAX
      IF(LBMCPR)WRITE(LUNOUT,109) TMAX1,NNULL,NREAL
  109 FORMAT(/,2X,'CALCULATED MAX. COLLISION TIME =',F7.2,' PICOSECONDS.
     /',2(/),2X,'NUMBER OF NULL COLLISIONS =',I11,/,2X,'NUMBER OF REAL C
     /OLLISIONS =',I11)
      WMNZ=WZ*1.0D-05
      WMNY=WY*1.0D-05
      WMNX=WX*1.0D-05
      IF(LBMCPR)WRITE(LUNOUT,940) WMNZ,DWZ,WMNY,DWY,WMNX,DWX
  940 FORMAT(/,2X,'Z DRIFT VELOCITY =',E11.4,' MICRONS/NANOSECOND  +-',F
     /8.2,'% ',/,2X,'Y DRIFT VELOCITY =',E11.4,' MICRONS/NANOSECOND  +-'
     /,F8.2,'%',/,2X,'X DRIFT VELOCITY =',E11.4,' MICRONS/NANOSECOND  +-
     /',F8.2,'%',/)
      IF(BMAG.GT.0.0D0.AND.(BTHETA.GT.0.0D0.AND.BTHETA.LT.180.0D0))
     /GO TO 800
      DTOVMB=DIFTR*EMAG/WZ
      DTMN=SQRT(2.0D0*DIFTR/WZ)*10000.0D0
      DFTER1=SQRT(DFTER**2+DWZ**2)
      DFTER2=DFTER1/2.0
      IF(LBMCPR)WRITE(LUNOUT,954)
      IF(LBMCPR)WRITE(LUNOUT,950) DIFTR,DFTER,DTOVMB,DFTER1,DTMN,DFTER2
  950 FORMAT(/,2X,'TRANSVERSE DIFFUSION   =',D11.4,' +-',F8.2,'%',/,10X,
     /'=',F9.4,' EV. +-',F8.2,'%',/,10X,'=',F9.3,' MICRONS/CENTIMETER**0
     /.5  +-',F8.2,'%',/)
      DLOVMB=DIFLN*EMAG/WZ
      DLMN=SQRT(2.0D0*DIFLN/WZ)*10000.0D0
      DFLER1=SQRT(DFLER**2+DWZ**2)
      DFLER2=DFLER1/2.0
      IF(LBMCPR)WRITE(LUNOUT,992) DIFLN,DFLER,DLOVMB,DFLER1,DLMN,DFLER2
  992 FORMAT(/,2X,'LONGITUDINAL DIFFUSION =',D11.4,' +-',F8.1,'%',/,10X,
     /'=',F9.4,' EV. +-',F8.2,'%',/,10X,'=',F9.3,' MICRONS/CENTIMETER**0
     /.5  +-',F8.2,'%',/)
      GO TO 900
  800 IF(LBMCPR)WRITE(LUNOUT,954)
  954 FORMAT(/,10X,' DIFFUSION IN CM**2/SEC.',/)
      IF(LBMCPR)WRITE(LUNOUT,955) DIFXX,DIFYY,DIFZZ,DIFYZ,DIFXY,DIFXZ
  955 FORMAT(/,2X,'DIFFUSION TENSOR :',/,6X,' DIFXX =',D11.4,' DIFYY =',
     /D11.4,' DIFZZ =',D11.4,/,6X,' DIFYZ =',D11.4,' DIFXY =',D11.4,' DI
     /FXZ =',D11.4,/)
      IF(LBMCPR)WRITE(LUNOUT,956) DXXER,DYYER,DZZER,DYZER,DXYER,DXZER
  956 FORMAT(/,2X,'ERROR ON DIFFUSION TENSOR :',/,6X,' DIFXX =',F8.2,'%
     / DIFYY =',F8.2,'%  DIFZZ =',F8.2,'%',/,6X,' DIFYZ =',F8.2,'%  DIFX
     /Y =',F8.2,'%  DIFXZ =',F8.2,'%',/)
       IF(BTHETA.EQ.90. .and. lbmcpr)WRITE(LUNOUT,957)
     -      DIFLN,DFLER,DIFTR,DFTER,DIFXX,DXXER
  957 FORMAT(/,8X,' LONGITUDINAL DIFFUSION =',D11.4,' +-',F8.2,'%',/,10X
     /,' TRANSVERSE DIFFUSION =',D11.4,' +-',F8.2,'%',/,2X,'TRANSVERSE D
     /IFFUSION (PARALLEL TO B-FIELD) DIFXX=',D11.4,' +-',F8.2,'%',/)
  900 IF(LBMCPR)WRITE(LUNOUT,333) ALPHA,ALPER,ATT,ATTER
  333 FORMAT(2(/),'  IONISATION RATE /CM.=',E11.4,' +/-',F6.2,' PERCENT.
     /',/,'  ATTACHMENT RATE /CM.=',E11.4,' +/-',F6.2,' PERCENT.',2(/))
      IF(LBMCPR)WRITE(LUNOUT,960) AVE,DEN
  960 FORMAT(/,2X,'MEAN ELECTRON ENERGY =',F9.4,' EV. ERROR =  +-',F8.2,
     /'%',/)
      END