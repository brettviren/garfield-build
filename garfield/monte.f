CDECK  ID>, MONTE.
      SUBROUTINE MONTE
      IMPLICIT REAL*8 (A-H,O-Z)
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
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
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
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
       DOUBLE PRECISION CON
       INTEGER ITHRM
       COMMON /THRM/ CON,ITHRM
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
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
      DIMENSION XST(100000),YST(100000),ZST(100000),STO(100000)
      DIMENSION WZST(10),AVEST(10)
      DIMENSION DFZZST(10),DFYYST(10),DFXXST(10)
C -------------------------------------------------------------------
C   CALCULATES COLLISION EVENTS AND UPDATES DIFFUSION AND VELOCITY.
C   USED WITH MAGNETIC FIELD B =0.0   ELECTRIC FIELD IN Z DIRECTION.
C -------------------------------------------------------------------
      WX=0.0D0
      WY=0.0D0
      DWX=0.0D0
      DWY=0.0D0
      DIFYZ=0.0D0
      DIFXY=0.0D0
      DIFXZ=0.0D0
      DYZER=0.0D0
      DXYER=0.0D0
      DXZER=0.0D0
      X=0.0D0
      Y=0.0D0
      Z=0.0D0
      ST=0.0D0
      ST1=0.0D0
      ST2=0.0D0
      SUME2=0.0D0
      SUMXX=0.0D0
      SUMYY=0.0D0
      SUMZZ=0.0D0
      SUMVX=0.0D0
      SUMVY=0.0D0
      ZOLD=0.0D0
      STOLD=0.0D0
      ST1OLD=0.0D0
      ST2OLD=0.0D0
      SZZOLD=0.0D0
      SXXOLD=0.0D0
      SYYOLD=0.0D0
      SVXOLD=0.0D0
      SVYOLD=0.0D0
      SME2OLD=0.0D0
      SMALL=1.0D-20
      TMAX1=0.0D0
      RDUM=RSTART
      E1=ESTART
      CONST9=CONST3*0.01D0
      ARAT=EMASS/AMU
      INTEM=8
      ITMAX=10
      ID=0
      NCOL=0
      NNULL=0
C  NUMBER OF COLLISIONS FOR DE-CORRELATION
      NCOLM=100000
      TDASH=0.0D0
C
C     INITIAL DIRECTION COSINES
C
      DCZ1=COS(THETA)
      DCX1=SIN(THETA)*COS(PHI)
      DCY1=SIN(THETA)*SIN(PHI)
C
      BP=EMAG*EMAG*CONST1
      F1=EMAG*CONST2
      F2=EMAG*CONST3
      F4=2.0D0*ACOS(-1.0D0)
      DELTAE=EFINAL/DBLE(INTEM)
      J2M=NMAX/ITMAX
C MAIN LOOP
      DO 210 J1=1,ITMAX
      DO 133 J2=1,J2M
    1 R1=drand48(RDUM)
      I=INT(E1/DELTAE)+1
      I=MIN(I,INTEM)
      TLIM=TCFMAX(I)
      T=-LOG(R1)/TLIM+TDASH
      TDASH=T
      AP=DCZ1*F2*SQRT(E1)
      E=E1+(AP+BP*T)*T
      IE=INT(E/ESTEP)+1
      IE=MIN(IE,2048)
      IF(TCF(IE).GT.TLIM) THEN
       TDASH=TDASH+LOG(R1)/TLIM
       TCFMAX(I)=1.05D0*TCFMAX(I)
C      if(lbmcpr)WRITE(lunout,996)
C996   FORMAT(/,5X,' WARNING NULL COLLISION TIME INCREASED',/)
       GO TO 1
      ENDIF
C
C     TEST FOR REAL OR NULL COLLISION
C
      R5=drand48(RDUM)
      TLIM=TCF(IE)/TLIM
      IF(R5.GT.TLIM) THEN
       NNULL=NNULL+1
       GO TO 1
      ENDIF
C
C  CALCULATE DIRECTION COSINES AND POSITIONS AT INSTANT BEFORE COLLISION
C    ALSO UPDATE DIFFUSION  AND ENERGY CALCULATIONS.
      T2=T*T
      IF(T.GE.TMAX1) TMAX1=T
      TDASH=0.0D0
      CONST6=SQRT(E1/E)
      DCX2=DCX1*CONST6
      DCY2=DCY1*CONST6
      DCZ2=DCZ1*CONST6+EMAG*T*CONST5/SQRT(E)
      A=AP*T
      B=BP*T2
      SUME2=SUME2+T*(E1+A/2.0D0+B/3.0D0)
      CONST7=CONST9*SQRT(E1)
      A=T*CONST7
      NCOL=NCOL+1
      CX1=DCX1*CONST7
      CY1=DCY1*CONST7
      CZ1=DCZ1*CONST7
      X=X+DCX1*A
      Y=Y+DCY1*A
      Z=Z+DCZ1*A+T2*F1
      ST=ST+T
      IT=INT(T+1.0)
      IT=MIN(IT,300)
      TIME(IT)=TIME(IT)+1.0D0
      SPEC(IE)=SPEC(IE)+1.0D0
      WZ=Z/ST
      SUMVX=SUMVX+CX1*CX1*T2
      SUMVY=SUMVY+CY1*CY1*T2
      IF(ID.EQ.0) GO TO 121
      KDUM=0
      DO 120 JDUM=1,4
      ST2=ST2+T
      NCOLDM=NCOL+KDUM
      IF(NCOLDM.GT.NCOLM) NCOLDM=NCOLDM-NCOLM
      SDIF=ST-STO(NCOLDM)
      SUMXX=SUMXX+((X-XST(NCOLDM))**2)*T/SDIF
      SUMYY=SUMYY+((Y-YST(NCOLDM))**2)*T/SDIF
      IF(J1.LT.3) GO TO 120
      ST1=ST1+T
      SUMZZ=SUMZZ+((Z-ZST(NCOLDM)-WZ*SDIF)**2)*T/SDIF
  120 KDUM=KDUM+12500
  121 XST(NCOL)=X
      YST(NCOL)=Y
      ZST(NCOL)=Z
      STO(NCOL)=ST
      IF(NCOL.GE.NCOLM) THEN
       ID=ID+1
       XID=DBLE(ID)
       NCOL=0
      ENDIF
C ---------------------------------------------------------------------
C     DETERMINATION OF REAL COLLISION TYPE
C ---------------------------------------------------------------------
      R2=drand48(RDUM)
C FIND LOCATION WITHIN 4 UNITS IN COLLISION ARRAY
      CALL SORT(I,R2,IE)
  140 I=I+1
      IF(CF(IE,I).LT.R2) GO TO 140
      S1=RGAS(I)
      EI=EIN(I)
      IF(IPN(I).LE.0) GO TO 666
C  USE FLAT DISTRIBUTION OF  ELECTRON ENERGY BETWEEN E-EION AND 0.0 EV
C  SAME AS IN BOLTZMANN
      R9=drand48(RDUM)
      EXTRA=R9*(E-EI)
      EI=EXTRA+EI
C
C  GENERATE SCATTERING ANGLES AND UPDATE  LABORATORY COSINES AFTER
C   COLLISION ALSO UPDATE ENERGY OF ELECTRON.
C
  666 IPT=IARRY(I)
      ICOLL(IPT)=ICOLL(IPT)+1
      ICOLN(I)=ICOLN(I)+1
      IF(E.LT.EI) THEN
C      if(lbmcpr)WRITE(lunout,994) E,EI,J2
C994  FORMAT(2X,' WARNING ENERGY =',F8.3,' LESS THAN ENERGY LOSS EI=',F8
C    /.3,' AT ITER=',I12,' DUE TO BINNING ERROR')
C  FIX ENERGY LOSS SMALLER THAN INCIDENT ENERGY IF ERROR OCCURS
       EI=E-0.0001D0
      ENDIF
      S2=(S1*S1)/(S1-1.0D0)
C  ANISOTROPIC SCATTERING
      IF(INDEX(I).NE.0) THEN
       R31=drand48(RDUM)
       R3=drand48(RDUM)
       F3=1.0D0-R3*ANGCT(IE,I)
       IF(R31.GT.PSCT(IE,I)) F3=-F3
      ELSE
C ISOTROPIC  SCATTERING
       R3=drand48(RDUM)
       F3=1.0D0-2.0D0*R3
      ENDIF
      THETA0=ACOS(F3)
      R4=drand48(RDUM)
      PHI0=F4*R4
      F8=SIN(PHI0)
      F9=COS(PHI0)
      ARG1=1.0D0-S1*EI/E
      ARG1=MAX(ARG1,SMALL)
      D=1.0D0-F3*SQRT(ARG1)
      E1=E*(1.0D0-EI/(S1*E)-2.0D0*D/S2)
      E1=MAX(E1,SMALL)
      Q=SQRT((E/E1)*ARG1)/S1
      Q=MIN(Q,1.0D0)
      THETA=ASIN(Q*SIN(THETA0))
      F6=COS(THETA)
      U=(S1-1.0D0)*(S1-1.0D0)/ARG1
      CSQD=F3*F3
      IF(F3.LT.0.0D0.AND.CSQD.GT.U) F6=-1.0D0*F6
      F5=SIN(THETA)
      DCZ2=MIN(DCZ2,1.0D0)
      ARGZ=SQRT(DCX2*DCX2+DCY2*DCY2)
      IF(ARGZ.EQ.0.0D0) THEN
C      if(lbmcpr)WRITE(lunout,9232) ITER,ID,E1
C9232  FORMAT(3X,'WARNING ARGZ= 0.0 AT ITER =',I10,' ID =',I10,' E1=',E1
C    /2.3)
       DCZ1=F6
       DCX1=F9*F5
       DCY1=F8*F5
       GO TO 130
      ENDIF
      DCZ1=DCZ2*F6+ARGZ*F5*F8
      DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
      DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
  130 CONTINUE
  133 CONTINUE
C   ------------------------------------------
      IF(J1.EQ.1.and.lbmcpr) WRITE(lunout,201)
  201 FORMAT(/,'    VEL      POS        TIME      ENERGY   COUNT   DIFXX
     /     DIFYY     DIFZZ',/)
      WZ=WZ*1.0D+09
      AVE=SUME2/ST
      DIFLN=0.0D0
      IF(NISO.EQ.0) THEN
       DIFXX=5.0D+15*SUMVX/ST
       DIFYY=5.0D+15*SUMVY/ST
       DFXXST(J1)=5.0D+15*(SUMVX-SVXOLD)/(ST-STOLD)
       DFYYST(J1)=5.0D+15*(SUMVY-SVYOLD)/(ST-STOLD)
      ELSE
       IF(ST2.NE.0.0D0) THEN
        DIFYY=5.0D+15*SUMYY/ST2
        DIFXX=5.0D+15*SUMXX/ST2
        DFXXST(J1)=5.0D+15*(SUMXX-SXXOLD)/(ST2-ST2OLD)
        DFYYST(J1)=5.0D+15*(SUMYY-SYYOLD)/(ST2-ST2OLD)
       ELSE
        DFXXST(J1)=0.0D0
        DFYYST(J1)=0.0D0
       ENDIF
      ENDIF
      IF(ST1.NE.0.0D0) THEN
       DIFZZ=5.0D+15*SUMZZ/ST1
       DFZZST(J1)=5.0D+15*(SUMZZ-SZZOLD)/(ST1-ST1OLD)
      ELSE
       DFZZST(J1)=0.0D0
      ENDIF
      WZST(J1)=(Z-ZOLD)/(ST-STOLD)*1.0D+09
      AVEST(J1)=(SUME2-SME2OLD)/(ST-STOLD)
      ZOLD=Z
      STOLD=ST
      ST1OLD=ST1
      ST2OLD=ST2
      SVXOLD=SUMVX
      SVYOLD=SUMVY
      SZZOLD=SUMZZ
      SXXOLD=SUMXX
      SYYOLD=SUMYY
      SME2OLD=SUME2
      if(lbmcpr)WRITE(lunout,202) WZ,Z,ST,AVE,ID,DIFXX,DIFYY,DIFZZ
  202 FORMAT(1X,F8.2,2(1X,D10.3),F9.4,1X,I5,1X,3(2X,F8.1))
C LOOP
  210 CONTINUE
C CALCULATE ERRORS AND CHECK AVERAGES
      TWZST=0.0D0
      TAVE=0.0D0
      T2WZST=0.0D0
      T2AVE=0.0D0
      TZZST=0.0D0
      TYYST=0.0D0
      TXXST=0.0D0
      T2ZZST=0.0D0
      T2YYST=0.0D0
      T2XXST=0.0D0
      DO 768 K=1,10
      TWZST=TWZST+WZST(K)
      TAVE=TAVE+AVEST(K)
      T2WZST=T2WZST+WZST(K)*WZST(K)
      T2AVE=T2AVE+AVEST(K)*AVEST(K)
      TXXST=TXXST+DFXXST(K)
      TYYST=TYYST+DFYYST(K)
      T2YYST=T2YYST+DFYYST(K)*DFYYST(K)
      T2XXST=T2XXST+DFXXST(K)*DFXXST(K)
      IF(K.LT.3) GO TO 768
      TZZST=TZZST+DFZZST(K)
      T2ZZST=T2ZZST+DFZZST(K)*DFZZST(K)
  768 CONTINUE
      DWZ=100.0D0*SQRT((T2WZST-TWZST*TWZST/10.0D0)/9.0D0)/WZ
      DEN=100.0D0*SQRT((T2AVE-TAVE*TAVE/10.0D0)/9.0D0)/AVE
      DXXER=100.0D0*SQRT((T2XXST-TXXST*TXXST/10.0D0)/9.0D0)/DIFXX
      DYYER=100.0D0*SQRT((T2YYST-TYYST*TYYST/10.0D0)/9.0D0)/DIFYY
      DZZER=100.0D0*SQRT((T2ZZST-TZZST*TZZST/8.0D0)/7.0D0)/DIFZZ
      DIFLN=DIFZZ
      DIFTR=(DIFXX+DIFYY)/2.0D0
C  CONVERT CM/SEC
      WZ=WZ*1.0D05
      DFLER=DZZER
      DFTER=(DXXER+DYYER)/2.0D0
C CALCULATE TOWNSEND COEFICIENTS AND ERRORS
      ANCATT=0.0D0
      ANCION=0.0D0
      DO 800 I=1,NGAS
      ANCATT=ANCATT+ICOLL((5*I)-2)
  800 ANCION=ANCION+ICOLL((5*I)-3)
      ATTER=0.0D0
      IF(ANCATT.EQ.0.0D0) GO TO 810
      ATTER=100.0D0*SQRT(ANCATT)/ANCATT
  810 ATT=ANCATT/(ST*WZ)*1.0D12
      ALPER=0.0D0
      IF(ANCION.EQ.0.0D0) GO TO 820
      ALPER=100.0D0*SQRT(ANCION)/ANCION
  820 ALPHA=ANCION/(ST*WZ)*1.0D12
      END