CDECK  ID>, MONTEFTA.
      SUBROUTINE MONTEFTA(IFAIL)
*-----------------------------------------------------------------------
*   MONTEFTA - Calculates collision events and updates diffusion and
*              velocity. This routine handles terminations at fixed
*              drift times. Bfield parallel to Efield.
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
       DOUBLE PRECISION EOVB,WB,BTHETA,BMAG
       COMMON/BFLD/EOVB,WB,BTHETA,BMAG
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
       DOUBLE PRECISION XS,YS,ZS,TS,ES,DCX,DCY,DCZ
       INTEGER IPL
       COMMON/IPT/XS(200),YS(200),ZS(200),TS(200),ES(200),
     -      DCX(200),DCY(200),DCZ(200),IPL(200)
*   Combined /TPLOUT/, /TPLOUTG/, /TPLOUTH/ in a single common.
       DOUBLE PRECISION ETPL,XTPL,YTPL,ZTPL,TTPL,XXTPL,YYTPL,ZZTPL,
     -      YZTPL,XZTPL,XYTPL,VZTPL,VYTPL,VXTPL,ATTOINT,ATTERT,AIOERT
       INTEGER NETPL
       COMMON /TPLOUT/ ETPL(8),XTPL(8),YTPL(8),ZTPL(8),TTPL(8),XXTPL(8),
     -      YYTPL(8),ZZTPL(8),YZTPL(8),XZTPL(8),XYTPL(8),
     -      VZTPL(8),VYTPL(8),VXTPL(8),ATTOINT,ATTERT,AIOERT,NETPL(8)
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
      DOUBLE PRECISION EPRM(4000000),DRAND48,S,RDUM,E1,EI,ESEC,
     -     CONST6,CONST7,CONST9,A,DX,DY,
     -     ZSTRT,TSSTRT,DCX1,DCY1,DCZ1,VTOT,CX1,CY1,CZ1,CX2,CY2,
     -     E100,DCX100,DCY100,DCZ100,DCX2,DCY2,DCZ2,
     -     AP,BP,TLIM,TDASH,TSTOP,T,E,TTEM,T2,PHI0,THETA0,
     -     F1,F2,F3,F4,F5,F6,F8,F9,R1,R2,R3,R4,R5,R9,R31,S1,S2,
     -     WBT,COSWT,SINWT,ARG1,D,Q,U,CSQD,ARGZ,W,ANEION,ANBT,EPRMBAR,
     -     E2PRM,EBAR,EERR
      INTEGER IESPECP(100),I,ID,INTEM,I100,NCOL,NELEC,NEION,NMXADD,IT,
     -     NPONT,NCLUS,J1,IPRINT,JPRINT,ITER,IPLANE,IDUM,IE,IPT,JCT,
     -     IFAIL
      EXTERNAL DRAND48
      logical lab20
*** Identify the routine if requested.
      IF(LIDENT)PRINT *,' /// ROUTINE MONTEFTA ///'
*** Assume this will work.
      IFAIL=0
*** Initialise.
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
      ETPL(I)=0.0D0
      XTPL(I)=0.0D0
      YTPL(I)=0.0D0
      ZTPL(I)=0.0D0
      TTPL(I)=0.0D0
      XXTPL(I)=0.0D0
      YYTPL(I)=0.0D0
      ZZTPL(I)=0.0D0
      VZTPL(I)=0.0D0
   34 NETPL(I)=0
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
*   Initial direction cosines
      DCZ1=COS(THETA)
      DCX1=SIN(THETA)*COS(PHI)
      DCY1=SIN(THETA)*SIN(PHI)
*   Initial velocity
      VTOT=CONST9*SQRT(E1)
      CX1=DCX1*VTOT
      CY1=DCY1*VTOT
      CZ1=DCZ1*VTOT
      E100=E1
      DCZ100=DCZ1
      DCX100=DCX1
      DCY100=DCY1
      BP=EMAG*EMAG*CONST1
      F1=EMAG*CONST2
      F2=EMAG*CONST3
      API=ACOS(-1.0D0)
      F4=2.0D0*API
*   Set to maximum possible collision freq.
      TLIM=TCFMAX(1)
      DO 35 I=2,INTEM
   35 IF(TLIM.LT.TCFMAX(I)) TLIM=TCFMAX(I)
      JPRINT=NMAX/10
      IPRINT=0
      ITER=0
      IPLANE=0
      IPRIM=0
*** Loop for new starting electrons
  544 IPRIM=IPRIM+1
      IF(IPRIM.GT.1) THEN
*   Check if program will exceed maximum number of iterations
*   in this cycle if so output current results.
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
           E1=E100
           VTOT=CONST9*SQRT(E1)
           CX1=DCX1*VTOT
           CY1=DCY1*VTOT
           CZ1=DCZ1*VTOT
           NCLUS=NCLUS+1
           ST=0.0D0
           TSSTRT=0.0D0
           ZSTRT=0.0D0
           IPLANE=0
      ENDIF
      IF(IPRIM.GT.4000000) THEN
      IF(LBMCPR)WRITE(LUNOUT,944) IPRIM
 944  FORMAT(/,2X,'PROGRAM STOPPED TOO MANY PRIMARIES IPRIM =',I7)
       PRINT *,' !!!!!! MONTEFTA WARNING: Attachment too high;'//
     -      ' unable to compute transport parameters.'
      GO TO 700
      ENDIF
      EPRM(IPRIM)=E1
      IDUM=INT(E1)+1
      IDUM=MIN(IDUM,100)
      IESPECP(IDUM)=IESPECP(IDUM)+1
*** Start of loop for newly created electrons
  555 TDASH=0.0D0
      NELEC=NELEC+1
      TSTOP=TSTEP+IPLANE*TSTEP
*** Main loop
    1 CONTINUE
C     IF(ITER.GT.NMAX)  GO TO 315
      R1=drand48(RDUM)
      T=-LOG(R1)/TLIM+TDASH
      TDASH=T
      AP=DCZ1*F2*SQRT(E1)
      lab20=.false.
  15  continue
      IF((T+ST).GE.TSTOP .or. lab20) THEN
           if(.not.lab20)then
                IPLANE=IPLANE+1
                TSTOP=TSTOP+TSTEP
*   Store position and energy at time plane = IPLANE.
                CALL TPLANEA(T,E1,CX1,CY1,DCZ1,AP,BP,EMAG,IPLANE)
*   Check if passed through more than one plane in this step
                IF((T+ST).GE.TSTOP.AND.TSTOP.LE.TFINAL) GO TO 15
           endif
           IF((T+ST).GE.TFINAL .or. lab20) THEN
                if(.not.lab20)then
                     ZTOT=ZTOT+Z
                     TTOT=TTOT+ST
                     ZTOTS=ZTOTS+Z-ZSTRT
                     TTOTS=TTOTS+ST-TSSTRT
                     TSTOP=TSTEP
*   No more electrons in cascade try new primary electron
                     IF(NELEC.EQ.(NCLUS+1)) GO TO 544
                endif
*   Take electrons from store
C   20          continue
                lab20=.false.
                X=XS(NPONT)
                Y=YS(NPONT)
                Z=ZS(NPONT)
                ST=TS(NPONT)
                E1=ES(NPONT)
                DCX1=DCX(NPONT)
                DCY1=DCY(NPONT)
                DCZ1=DCZ(NPONT)
                VTOT=CONST9*SQRT(E1)
                CX1=DCX1*VTOT
                CY1=DCY1*VTOT
                CZ1=DCZ1*VTOT
                IPLANE=IPL(NPONT)
                NPONT=NPONT-1
                ZSTRT=Z
                TSSTRT=ST
                GO TO 555
           ENDIF
      ENDIF
 913  FORMAT(3X,' AFTER STORE ITER=',I10,' E1=',D12.3,' T=',D12.3,
     -     ' AP=',D12.3,' BP=',D12.3,' DCZ1=',D12.3)
      E=E1+(AP+BP*T)*T
      IF(E.LT.0.0D0) THEN
           IF(LBMCPR)WRITE(LUNOUT,913)ITER,E,E1,AP,BP,DCZ1
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
      WBT=WB*T
      COSWT=COS(WBT)
      SINWT=SIN(WBT)
      CONST6=SQRT(E1/E)
      CX2=CX1*COSWT-CY1*SINWT
      CY2=CY1*COSWT+CX1*SINWT
      VTOT=CONST9*SQRT(E)
      DCX2=CX2/VTOT
      DCY2=CY2/VTOT
      DCZ2=DCZ1*CONST6+EMAG*T*CONST5/SQRT(E)
      CONST7=CONST9*SQRT(E1)
      A=T*CONST7
      NCOL=NCOL+1
      DX=(CX1*SINWT-CY1*(1.0D0-COSWT))/WB
      X=X+DX
      DY=(CY1*SINWT+CX1*(1.0D0-COSWT))/WB
      Y=Y+DY
      Z=Z+DCZ1*A+T2*F1
      ST=ST+T
      IT=INT(T+1.0D0)
      IT=MIN(IT,300)
      TIME(IT)=TIME(IT)+1.0D0
      SPEC(IE)=SPEC(IE)+1.0D0
*** Determination of real collision type
C     R2=RNDM2(RDUM)
      R2=drand48(RDUM)
*   Find location within 4 units in collision array
      CALL SORT(I,R2,IE)
  140 I=I+1
      IF(CF(IE,I).LT.R2) GO TO 140
      S1=RGAS(I)
      EI=EIN(I)
      IF(E.LT.EI) THEN
C          IF(LBMCPR)WRITE(LUNOUT,994) E,EI,ITER
C994       FORMAT(2X,' WARNING ENERGY =',F8.3,' LESS THAN ENERGY',
C    -          ' LOSS EI=',F8.3,' AT ITER=',I12,' DUE TO BINNING',
C    -          ' ERROR')
*** Fix energy loss smaller than incident energy if a bining error
*   occurs.
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
*   Electron captured start new primary
           IF(NELEC.EQ.(NCLUS+1)) GO TO 544
*   Electron captured take next electron from store
C          GO TO 20
           lab20=.true.
           goto 15
      ENDIF
      R9=drand48(RDUM)
C     ESEC=R9*(E-EI)
*   Use OPAL Peterson and Beaty splitting factor.
      ESEC=WPL(I)*TAN(R9*ATAN((E-EI)/(2.0D0*WPL(I))))
      EI=ESEC+EI
*** Store position ,energy, direction cosines and time of generation
*   of ionisation electron
      NCLUS=NCLUS+1
      NPONT=NPONT+1
      NMXADD=MAX(NPONT,NMXADD)
      IF(NPONT.GT.200) THEN
           IF(LBMCPR)WRITE(LUNOUT,546) NPONT,ITER
 546       FORMAT(2X,' PROGRAM STOPPED . NPONT=',I4,' ITER=',I10)
           IFAIL=1
           RETURN
C           STOP
      ENDIF
      XS(NPONT)=X
      YS(NPONT)=Y
      ZS(NPONT)=Z
      TS(NPONT)=ST
      ES(NPONT)=ESEC
*** Randomise secondary electron direction
      R3=drand48(RDUM)
      F3=1.0D0-2.0D0*R3
      THETA0=ACOS(F3)
      F6=COS(THETA0)
      F5=SIN(THETA0)
      R4=drand48(rdum)
      PHI0=F4*R4
      F8=SIN(PHI0)
      F9=COS(PHI0)
      DCX(NPONT)=F9*F5
      DCY(NPONT)=F8*F5
      DCZ(NPONT)=F6
      IPL(NPONT)=IPLANE
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
*   Isotropic scattering
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
C9232      FORMAT(3X,'WARNING ARGZ= 0.0 AT ITER =',I10,' ID =',I10,
C    -          ' E1=',E12.3)
           DCZ1=F6
           DCX1=F9*F5
           DCY1=F8*F5
           GO TO 190
      ENDIF
      DCZ1=DCZ2*F6+ARGZ*F5*F8
      DCY1=DCY2*F6+(F5/ARGZ)*(DCX2*F9-DCY2*DCZ2*F8)
      DCX1=DCX2*F6-(F5/ARGZ)*(DCY2*F9+DCX2*DCZ2*F8)
  190 CONTINUE
      VTOT=CONST9*SQRT(E1)
      CX1=DCX1*VTOT
      CY1=DCY1*VTOT
      CZ1=DCZ1*VTOT
*** Store direction cosines and energy after N collisions
*   for later reuse in primary generation
      I100=I100+1
      IF(I100.EQ.200) THEN
           DCZ100=DCZ1
           DCX100=DCX1
           DCY100=DCY1
           E100=E1
           I100=0
      ENDIF
      IF(IPRINT.GT.JPRINT) GO TO 200
      GO TO 1
*** Intermediate printout
 200  IPRINT=0
      W=ZTOTS/TTOTS
      W=W*1.0D+09
      JCT=ID/100000
C     IF(J1.EQ.1 .and. LBMCPR)WRITE(LUNOUT,201)
C 201 FORMAT(/,7X,'INTERMEDIATE OUTPUT',/,'    VEL      POS        TIME
C    /       COUNT    ')
C     IF(LBMCPR)WRITE(LUNOUT,202) W,ZTOTS,TTOTS,JCT
C 202 FORMAT(1X,F8.3,2(1X,D10.3),4X,I6)
      J1=J1+1
      GO TO 1
*** Main loop end
  700 XID=DBLE(ID)
      IF(NELEC.GT.IPRIM) THEN
           ANEION=DBLE(NEION)
           ANBT=DBLE(NELEC-IPRIM)
           ATTOINT=ANEION/ANBT
           ATTERT=SQRT(ANEION)/ANEION
           AIOERT=SQRT(ANBT)/ANBT
      ELSE
           ANEION=DBLE(NEION)
           ATTOINT=-1.0D0
           ATTERT=SQRT(ANEION)/ANEION
      ENDIF
      JCT=ID/100000
      IF(J1.EQ.1) THEN
          IF(LBMCPR)WRITE(LUNOUT,940) NCLUS,ITER
  940     FORMAT(2(/),' PROGRAM STOPPED  (TOO FEW COLLISIONS),',
     -         ' DECREASE THE ESTIMATED ALPHA.  NCLUS = ',I7,
     -         '  ITER =',I9)
          IFAIL=1
          RETURN
C          STOP
      ENDIF
      IF(LBMCPR)WRITE(LUNOUT,878) NELEC,NEION,IPRIM
 878  FORMAT(/,' TOTAL NO OF ELECTRONS=',I8,/,' TOTAL NO OF NEG. IONS=',
     /I8,/,' TOTAL NO OF PRIMARIES=',I8)
      EPRMBAR=0.0D0
      E2PRM=0.0D0
      IF(IPRIM.EQ.1) RETURN
      DO 310 I=1,IPRIM
      E2PRM=E2PRM+EPRM(I)*EPRM(I)
 310  EPRMBAR=EPRMBAR+EPRM(I)
      EBAR=EPRMBAR/IPRIM
      EERR=SQRT(E2PRM/IPRIM-EBAR**2)
      IF(LBMCPR)WRITE(LUNOUT,836) EBAR,EERR
 836  FORMAT(/,2X,'AVERAGE ENERGY OF PRIMARY ELECTRON =',F10.3,' EV.',/,
     /'   ENERGY SPREAD OF PRIMARY ELECTRON =',F10.3,' EV.')
C     IF(LBMCPR)WRITE(LUNOUT,835) (IESPECP(J),J=1,100)
C835  FORMAT(/,2X,'ENERGY SPECTRUM OF PRIMARY ELECTRONS IN 1 EV. BINS',/
C    /,10(2X,10I5,/))
      RETURN
  315 IF(ITER.GT.NMAX) THEN
           IF(LBMCPR)WRITE(LUNOUT,991)
     -          ITER,NMAX,NPONT,NELEC,IPRIM,NMXADD
 991       FORMAT(2(/),' PROGRAM STOPPED.  ITER =',I10,'    NMAX =',I10/
     -          ' NPONT=',I4,' NELEC=',I8,' IPRIM=',I4,' NMXADD=',I3)
           IFAIL=1
           RETURN
C           STOP
      ENDIF
      END
