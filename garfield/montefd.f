CDECK  ID>, MONTEFD.
      SUBROUTINE MONTEFD(IFAIL)
*-----------------------------------------------------------------------
*   MONTEFD - Calculates collision events and updates diffusion and
*             velocity. this routine handles terminations at fixed
*             drift distances.
*   Author: Steve Biagi, with modifications.
*   (Last changed on 16/ 3/08.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
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
       DOUBLE PRECISION ALPHAST,VDST,TSTEP,ZSTEP,TFINAL,ZFINAL
       INTEGER ITFINAL,IPRIM
       COMMON/CION/ALPHAST,VDST,TSTEP,ZSTEP,TFINAL,ZFINAL,ITFINAL,IPRIM
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       DOUBLE PRECISION ZTOT,TTOT,ZTOTS,TTOTS
       COMMON/TTRM/ZTOT,TTOT,ZTOTS,TTOTS
       DOUBLE PRECISION XSS,YSS,ZSS,TSS,ESS,DCXS,DCYS,DCZS
       INTEGER IPLS
       COMMON/IPS/XSS(200),YSS(200),ZSS(200),TSS(200),ESS(200),
     -      DCXS(200),DCYS(200),DCZS(200),IPLS(200)
       DOUBLE PRECISION ESPL,XSPL,YSPL,ZSPL,TSPL,XXSPL,YYSPL,ZZSPL,
     -      VZSPL,TSSUM,TSSUM2,ATTOION,ATTIOER,ATTATER
       INTEGER NESST
       COMMON/SPLOUT/ESPL(8),XSPL(8),YSPL(8),ZSPL(8),TSPL(8),XXSPL(8),
     -      YYSPL(8),ZZSPL(8),VZSPL(8),TSSUM(8),TSSUM2(8),ATTOION,
     -      ATTIOER,ATTATER,NESST(9)
       DOUBLE PRECISION TMSPL,TTMSPL,RSPL,RRSPL,RRSPM
       COMMON/SPL1/TMSPL(8),TTMSPL(8),RSPL(8),RRSPL(8),RRSPM(8)
       DOUBLE PRECISION ZPLANE1,ZPLANE2,ZPLANE3,ZPLANE4,ZPLANE5,ZPLANE6,
     -      ZPLANE7,ZPLANE8
       INTEGER IZFINAL
       COMMON/CTCALC/ZPLANE1,ZPLANE2,ZPLANE3,ZPLANE4,ZPLANE5,ZPLANE6,
     -      ZPLANE7,ZPLANE8,IZFINAL
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
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
       DOUBLE PRECISION EPRM(4000000),DRAND48,S,RDUM,E1,AP,BP,E,EI,
     -      CONST6,CONST7,CONST9,CX1,CY1,CZ1,THETA0,PHI0,
     -      TLIM,TDASH,T,T2,TOLD,TLFT,TTEM,EPOT,TZSTOP,TZSTOP1,ESEC,
     -      F1,F2,F3,F4,F5,F6,F8,F9,R1,R2,R3,R4,R5,R9,R31,S1,S2,A,B,
     -      DCX1,DCY1,DCZ1,DCX100,DCY100,DCZ100,DCX2,DCY2,DCZ2,E100,
     -      ZSTRT,TSSTRT,ARG1,D,Q,U,CSQD,ARGZ,W,ANEION,ANBT,EPRMBAR,
     -      E2PRM,EBAR,EERR
       INTEGER IESPECP(100),I,ID,INTEM,I100,NCOL,NELEC,NEION,NMXADD,
     -      NPONT,NCLUS,J1,JPRINT,IPRINT,IDUM,ISOL,IE,IT,IPT,IDM1,
     -      ITER,IZPLANE,JCT,IFAIL
       EXTERNAL DRAND48
       logical lab18,lab20
*** Identify the routine if desired.
       IF(LIDENT)PRINT *,' /// ROUTINE MONTEFD ///'
*** This will usually work.
       IFAIL=0
*** Initialise
      S=0.0D0
      ST=0.0D0
      X=0.0D0
      Y=0.0D0
      Z=0.0D0
      ZTOT=0.0D0
      ZTOTS=0.0D0
      TTOT=0.0D0
      TTOTS=0.0D0
      SMALL=1.0D-20
      TMAX1=0.0D0
      RDUM=RSTART
      E1=ESTART
      CONST9=CONST3*0.01D0
      API=ACOS(-1.0D0)
      DO 25 I=1,300
   25 TIME(I)=0.0D0
      DO 26 I=1,5*mxngas
   26 ICOLL(I)=0
      DO 27 I=1,512
   27 ICOLN(I)=0
      DO 28 I=1,2048
   28 SPEC(I)=0.0D0
      DO 33 I=1,100
   33 IESPECP(I)=0
      DO 34 I=1,8
      ESPL(I)=0.0D0
      XSPL(I)=0.0D0
      YSPL(I)=0.0D0
      ZSPL(I)=0.0D0
      TSPL(I)=0.0D0
      XXSPL(I)=0.0D0
      YYSPL(I)=0.0D0
      ZZSPL(I)=0.0D0
      VZSPL(I)=0.0D0
      TSSUM(I)=0.0D0
      TSSUM2(I)=0.0D0
      TMSPL(I)=0.0D0
      TTMSPL(I)=0.0D0
      RSPL(I)=0.0D0
      RRSPL(I)=0.0D0
      RRSPM(I)=0.0D0
   34 NESST(I)=0
      NESST(9)=0
      ID=0
      INTEM=8
      I100=0
      NCOL=0
      NNULL=0
      NELEC=0
      NEION=0
      NMXADD=0
      NPONT=0
      NCLUS=0
      J1=1
      ZSTRT=0.0D0
      TSSTRT=0.0D0
*** Initial direction cosines
      DCZ1=COS(THETA)
      DCX1=SIN(THETA)*COS(PHI)
      DCY1=SIN(THETA)*SIN(PHI)
      DCX100=DCX1
      DCY100=DCY1
      DCZ100=DCZ1
      E100=E1
      BP=EMAG*EMAG*CONST1
      F1=EMAG*CONST2
      F2=EMAG*CONST3
      F4=2.0D0*API
*** Set to maximum possible collision freq.
      TLIM=TCFMAX(1)
      DO 35 I=2,INTEM
   35 IF(TLIM.LT.TCFMAX(I)) TLIM=TCFMAX(I)
      JPRINT=NMAX/10
      IPRINT=0
      ITER=0
      IPRIM=0
*** Loop for new starting electrons
 544  IPRIM=IPRIM+1
      if(ldebug)print *,' ++++++ MONTEFD DEBUG : New primary: ',IPRIM
      IZPLANE=0
      TZSTOP=1000.0D0
      IF(IPRIM.GT.1) THEN
*** Check if program will exceed maximum number of iterations in this
*   cycle , if so output current results.
           IF(ITER.GT.NMAX) THEN
                IPRIM=IPRIM-1
                GO TO 700
           ENDIF
           X=0.0D0
           Y=0.0D0
           Z=0.0D0
           DCZ1=DCZ100
           DCX1=DCX100
           DCY1=DCY100
           NCLUS=NCLUS+1
           E1=E100
           ST=0.0D0
           ZSTRT=0.0D0
           TSSTRT=0.0D0
      ENDIF
      IF(IPRIM.GT.4000000) THEN
          IF(LBMCPR)WRITE(LUNOUT,944) IPRIM
  944     FORMAT(2X,' PROGRAM STOPPED TOO MANY PRIMARIES IPRIM=',I7)
          IFAIL=1
           PRINT *,' !!!!!! MONTEFD WARNING: Attachment too high;'//
     -          ' unable to compute transport parameters.'
          RETURN
C          STOP
      ENDIF
      EPRM(IPRIM)=E1
      IDUM=INT(E1)+1
      IDUM=MIN(IDUM,100)
      IESPECP(IDUM)=IESPECP(IDUM)+1
*** Start of loop for newly created electrons .
 555  TDASH=0.0D0
      NELEC=NELEC+1
*** Main loop
    1 CONTINUE
C      if(100000*(iter/100000).eq.iter)print *,' Iter = ',iter
C     IF(ITER.GT.NMAX)  GO TO 315
      R1=drand48(RDUM)
      T=-LOG(R1)/TLIM+TDASH
      TOLD=TDASH
      TDASH=T
      AP=DCZ1*F2*SQRT(E1)
      lab18=.false.
      lab20=.false.
   15 continue
      IF((T.GE.TZSTOP.AND.TOLD.LT.TZSTOP) .or. lab18 .or. lab20) THEN
           if((.not.lab18) .and. (.not.lab20))then
                TLFT=TZSTOP
*** Store position and energy at z plane = IZPLANE.
                CALL SPLANE(T,E1,DCX1,DCY1,DCZ1,AP,BP,EMAG,TLFT,
     -               IZPLANE)
           endif
*   Change  IF statement from (IZFINAL+1) to (IZFINAL-1)
*   for anode termination .
           IF(IZPLANE.GE.(IZFINAL+1) .or. lab18 .or. lab20) THEN
  18            continue
                lab18=.false.
                if(.not.lab20)then
                     ZTOT=ZTOT+Z
                     TTOT=TTOT+ST
                     ZTOTS=ZTOTS+Z-ZSTRT
                     TTOTS=TTOTS+ST-TSSTRT
*   No more electrons in cascade return to main.
                     IF(NELEC.EQ.(NCLUS+1)) GO TO 544
                endif
*   Take electrons from store
C  20           continue
                lab20=.false.
                X=XSS(NPONT)
                Y=YSS(NPONT)
                Z=ZSS(NPONT)
                ST=TSS(NPONT)
                E1=ESS(NPONT)
                DCX1=DCXS(NPONT)
                DCY1=DCYS(NPONT)
                DCZ1=DCZS(NPONT)
                IZPLANE=IPLS(NPONT)
                NPONT=NPONT-1
                ZSTRT=Z
                TSSTRT=ST
                IF(Z.GT.ZFINAL) THEN
*   Check if electron has enough energy to go back to final plane
                     EPOT=EMAG*(Z-ZFINAL)*100.0D0
                     IF(E1.LT.EPOT) THEN
                          NELEC=NELEC+1
                          ISOL=1
                          GO TO 18
                     ENDIF
                ENDIF
                CALL TCALC(Z,DCZ1,E1,EMAG,TZSTOP,TZSTOP1,ISOL,
     -               IZPLANE)
                IF(TZSTOP.EQ.-99.0D0) THEN
*   Catch runaway electrons at high field
                     NELEC=NELEC+1
                     ISOL=1
                     GO TO 18
                ENDIF
                GO TO 555
           ENDIF
*   If two solutions repeat entry for second solution.
           IF(ISOL.EQ.2) THEN
                TZSTOP=TZSTOP1
                ISOL=1
                GO TO 15
           ENDIF
      ENDIF
      E=E1+(AP+BP*T)*T
      IF(E.LT.0.0D0) THEN
            IF(LBMCPR)WRITE(LUNOUT,999) E,E1,AP,BP,T,DCZ1,ITER
 999        FORMAT(2X,' WARNING ENERGY LT.0. E=',D12.3,' E1=',D12.3,
     -           ' AP=',D12.3,' BP=',D12.3,' T=',D12.3,/,' DCZ1=',D12.3,
     -           ' ITER=',I10)
            E=0.001D0
      ENDIF
      IE=INT(E/ESTEP)+1
      IE=MIN(IE,2048)
*** Test for real or null collision
      R5=drand48(RDUM)
      TTEM=TCF(IE)/TLIM
      IF(R5.LE.TTEM)GO TO 137
      NNULL=NNULL+1
      GO TO 1
*** Calculate direction cosines and positions at instant before
*   collision also update diffusion  and energy calculations.
  137 T2=T*T
      IF(T.GE.TMAX1) TMAX1=T
      TDASH=0.0D0
      CONST6=SQRT(E1/E)
      DCX2=DCX1*CONST6
      DCY2=DCY1*CONST6
      DCZ2=DCZ1*CONST6+EMAG*T*CONST5/SQRT(E)
      A=AP*T
      B=BP*T2
      CONST7=CONST9*SQRT(E1)
      A=T*CONST7
      NCOL=NCOL+1
      CZ1=DCZ1*CONST7
      X=X+DCX1*A
      Y=Y+DCY1*A
      Z=Z+DCZ1*A+T2*F1
      ST=ST+T
      IT=INT(T+1.0D0)
      IT=MIN(IT,300)
      TIME(IT)=TIME(IT)+1.0D0
      CX1=DCX1*CONST7
      CY1=DCY1*CONST7
      SPEC(IE)=SPEC(IE)+1.0D0
*** Determination of real collision type
      R2=drand48(RDUM)
*   Find location within 4 units in collision array
      CALL SORT(I,R2,IE)
  140 I=I+1
      IF(CF(IE,I).LT.R2) GO TO 140
      S1=RGAS(I)
      EI=EIN(I)
      IF(E.LT.EI) THEN
*** Fix energy loss smaller than incident energy if a bining error
*   occurs.
C          IF(LBMCPR)WRITE(LUNOUT,994) E,EI,ITER
C994       FORMAT(3X,' WARNING BINNING ERROR ENERGY =',F8.3,' EI=',
C    -          F8.3,' ITER =',I12)
           EI=E-0.0001D0
      ENDIF
      IF(IPN(I).EQ.0) GO TO 666
*   Attachment
      IF(IPN(I).EQ.-1) THEN
           NEION=NEION+1
           IPT=IARRY(I)
           ID=ID+1
           ITER=ITER+1
           IF(ITER.LT.0)GOTO 315
           IPRINT=IPRINT+1
           ICOLL(IPT)=ICOLL(IPT)+1
           ICOLN(I)=ICOLN(I)+1
           IT=INT(T+1.0D0)
           IT=MIN(IT,300)
           TIME(IT)=TIME(IT)+1.0D0
           ZTOT=ZTOT+Z
           TTOT=TTOT+ST
           ZTOTS=ZTOTS+Z-ZSTRT
           TTOTS=TTOTS+ST-TSSTRT
           IDM1=1+INT(Z/ZSTEP)
           IF(IDM1.GT.9) IDM1=9
*   Next line added to avoid IDM1=0 (RV 6/11/2007).
           IF(IDM1.LT.1) IDM1=1
*   End of modification.
           NESST(IDM1)=NESST(IDM1)-1
*   Electron captured start new primary
           IF(NELEC.EQ.(NCLUS+1)) GO TO 544
*   Electron captured take next electron from store
C           GO TO 20
           lab20=.true.
           goto 15
      ENDIF
      R9=drand48(RDUM)
C     ESEC=R9*(E-EI)
*** Use OPAL Peterson and Beaty splitting factor.
      ESEC=WPL(I)*TAN(R9*ATAN((E-EI)/(2.0D0*WPL(I))))
      EI=ESEC+EI
*** Store position ,energy, direction cosines and time of generation
*   of ionisation electron
      NCLUS=NCLUS+1
      NPONT=NPONT+1
      NMXADD=MAX(NPONT,NMXADD)
      IF(NPONT.GT.200) THEN
           IF(LBMCPR)WRITE(LUNOUT,546) NPONT,ITER
 546       FORMAT(2X,' PROGRAM STOPPED NPONT=',I3,' ITER=',I10)
           IFAIL=1
           RETURN
C           STOP
      ENDIF
      XSS(NPONT)=X
      YSS(NPONT)=Y
      ZSS(NPONT)=Z
      TSS(NPONT)=ST
      ESS(NPONT)=ESEC
*   Randomise secondary electron direction
      R3=drand48(RDUM)
      F3=1.0D0-2.0D0*R3
      THETA0=ACOS(F3)
      F6=COS(THETA0)
      F5=SIN(THETA0)
      R4=drand48(RDUM)
      PHI0=F4*R4
      F8=SIN(PHI0)
      F9=COS(PHI0)
      DCXS(NPONT)=F9*F5
      DCYS(NPONT)=F8*F5
      DCZS(NPONT)=F6
      IDM1=1+INT(Z/ZSTEP)
      IF(IDM1.GT.9) IDM1=9
*   Next line added to avoid IDM1=0 (RV 23/11/2007).
      IF(IDM1.LT.1) IDM1=1
*   End of modification.
      IPLS(NPONT)=IDM1
      NESST(IPLS(NPONT))=NESST(IPLS(NPONT))+1
*** Generate scattering angles and update  laboratory cosines after
*   collision also update energy of electron.
  666 IPT=IARRY(I)
      ID=ID+1
      ITER=ITER+1
      IF(ITER.LT.0)GOTO 315
      IPRINT=IPRINT+1
      ICOLL(IPT)=ICOLL(IPT)+1
      ICOLN(I)=ICOLN(I)+1
      S2=(S1*S1)/(S1-1.0D0)
*   Anisotropic scattering
      IF(INDEX(I).NE.0) THEN
           R31=drand48(RDUM)
           R3=drand48(RDUM)
           F3=1.0D0-R3*ANGCT(IE,I)
           IF(R31.GT.PSCT(IE,I)) F3=-F3
      ELSE
*  Isotropic scattering
           R3=drand48(RDUM)
           F3=1.0D0-2.0D0*R3
      ENDIF
      THETA0=ACOS(F3)
      R4=drand48(RDUM)
      PHI0=F4*R4
      F8=SIN(PHI0)
      F9=COS(PHI0)
      IF(E.LT.EI) EI=0.0D0
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
C          IF(LBMCPR)WRITE(LUNOUT,9232) ITER,ID,E1
C9232      FORMAT(3X,' WARNING ARGZ= 0.0  AT ITER =',I10,' ID=',I10,
C    -         ' E1=',E12.3)
           DCZ1=F6
           DCX1=F9*F5
           DCY1=F8*F5
           GO TO 190
      ENDIF
      DCZ1=DCZ2*F6+ARGZ*F5*F8
      DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
      DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
 190  CONTINUE
*** Store direction cosines and energy after N collisions
*   for later reuse in primary generation.
      I100=I100+1
      IF(I100.EQ.200) THEN
           DCZ100=DCZ1
           DCX100=DCX1
           DCY100=DCY1
           E100=E1
           I100=0
      ENDIF
      IF(Z.GT.ZFINAL) THEN
*   Check if electron has enough energy to go back to final plane.
           EPOT=EMAG*(Z-ZFINAL)*100.0D0
           IF(E1.LT.EPOT)then
C               GO TO 18
                lab18=.true.
                goto 15
           endif
      ENDIF
*   Calculate time tzstop to arrive at next z plane IZPLANE.
      CALL TCALC(Z,DCZ1,E1,EMAG,TZSTOP,TZSTOP1,ISOL,IZPLANE)
*   Catch runaway electrons at high field
      IF(TZSTOP.EQ.-99.0D0)then
C          GO TO 18
           lab18=.true.
           goto 15
      endif
      IF(IPRINT.GT.JPRINT) GO TO 200
      GO TO 1
*   Intermediate printout
 200  IPRINT=0
      W=ZTOTS/TTOTS
      W=W*1.0D+09
      XID=DBLE(ID)
      JCT=ID/100000
C     IF(J1.EQ.1 .and. LBMCPR)WRITE(LUNOUT,201)
C 201 FORMAT(/,7X,'INTERMEDIATE OUTPUT',/,'    VEL       POS       TIME
C    /     COUNT')
C     IF(LBMCPR)WRITE(LUNOUT,202) W,ZTOTS,TTOTS,JCT
C 202 FORMAT(1X,F8.3,2(1X,D10.3),1X,I6)
      J1=J1+1
      GO TO 1
*** Main loop end
  700 XID=DBLE(ID)
      IF(NELEC.GT.IPRIM) THEN
           ANEION=DBLE(NEION)
           ANBT=DBLE(NELEC-IPRIM)
           ATTOION=ANEION/ANBT
           ATTATER=SQRT(ANEION)/ANEION
           ATTIOER=SQRT(ANBT)/ANBT
      ELSE
           ATTOION=-1.0D0
           ANEION=DBLE(NEION)
           ATTATER=SQRT(ANEION)/ANEION
      ENDIF
      JCT=ID/10000
      IF(J1.EQ.1) THEN
          IF(LBMCPR)WRITE(LUNOUT,940) NCLUS,ITER,NELEC,NEION
  940     FORMAT(2(/),' PROGRAM STOPPED (TOO FEW COLLISIONS),',
     -         ' DECREASE THE ESTIMATED ALPHA.  NCLUS=',I7,
     -         '  ITER =',I9,' NELEC=',I9,' NEION =',I6)
      ENDIF
      IF(LBMCPR)WRITE(LUNOUT,878) NELEC,NEION,IPRIM
 878  FORMAT(/,' TOTAL NO OF ELECTRONS=',I8,/,' TOTAL NO OF NEG. IONS=',
     /I8,/,' TOTAL NO OF PRIMARIES=',I8)
      EPRMBAR=0.0D0
      E2PRM=0.0D0
      IF(IPRIM.EQ.1) RETURN
      DO 310 I=1,IPRIM
      E2PRM=E2PRM+EPRM(I)*EPRM(I)
  310 EPRMBAR=EPRMBAR+EPRM(I)
      EBAR=EPRMBAR/IPRIM
      EERR=SQRT(E2PRM/IPRIM-EBAR**2)
      IF(LBMCPR)WRITE(LUNOUT,836) EBAR,EERR
  836 FORMAT(/,2X,'AVERAGE ENERGY OF PRIMARY ELECTRON =',F10.3,' EV.',/,
     /'   ENERGY SPREAD OF PRIMARY ELECTRON =',F10.3,' EV.')
C     IF(LBMCPR)WRITE(LUNOUT,837) (IESPECP(J),J=1,100)
C 837 FORMAT(/,2X,'ENERGY SPECTRUM OF PRIMARIES IN 1EV BINS',/,10(2X,10I
C    /5,/))
      RETURN
  315 IF(ITER.GT.NMAX) THEN
           IF(LBMCPR)WRITE(LUNOUT,991)
     -          ITER,NMAX,NPONT,NELEC,IPRIM,NMXADD
 991       FORMAT(2(/),' PROGRAM STOPPED.  ITER =',I10,'    NMAX =',I10/
     -          ' NPONT=',I4,' NELEC=',I8,' IPRIM=',I6,' NMXADD=',I3)
           IFAIL=1
           RETURN
C           STOP
      ENDIF
      END
