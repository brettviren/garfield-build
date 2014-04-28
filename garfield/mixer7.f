CDECK  ID>, MIXER7.
       SUBROUTINE MIXER7(IFAIL)
*-----------------------------------------------------------------------
*   MIXER7 - Fills arrays of collision frequency
*            can have a mixture of up to 6 gases
*   Author: Steve Biagi, with modifications.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
*   Grouped AN1 ... AN6 in ANn
       DOUBLE PRECISION ANn,AN,FRAC
       COMMON /RATIO/ ANn(mxngas),AN,FRAC(mxngas)
       INTEGER NGASN
       COMMON /GASN/ NGASN(mxngas)
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
*   Is in effect the old ANCT common.
       DOUBLE PRECISION PSCT,ANGCT
       INTEGER INDEX,NISO
       COMMON/ANIS/PSCT(2048,512),ANGCT(2048,512),INDEX(512),NISO
       DOUBLE PRECISION FCION,FCATT
       COMMON/FRED/FCION(2048),FCATT(2048)
*   Grouped VAN1 ... VAN6 in VANn
       DOUBLE PRECISION VANn,VAN
       COMMON /MRATIO/ VANn(mxngas),VAN
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
       CHARACTER*15 NAMEn(mxngas)
       CHARACTER*30 SCRPn(226,mxngas)
       DOUBLE PRECISION Qn(6,2048,mxngas),En(6,mxngas),EInn(220,mxngas),
     -      QATT(mxngas,2048),
     -      PEQELn(6,2048,mxngas),PEQINn(220,2048,mxngas),
     -      EHALF,AJ,PSCT1,ANGC,PSCT2,BP,F2,ELOW,EHI,
     -      VIRIAL(mxngas),EBn(mxngas),RGASn(mxngas)
       INTEGER KINn(220,mxngas),KELn(6,mxngas),igas,ilev,
     -      I,J,K,IE,NP,IF,KELSUM,JLOW,JHI,L,IFAIL
*** Identify if requested.
      IF(LIDENT)PRINT *,' /// ROUTINE MIXER7 ///'
      NISO=0
*** This in principle will work.
      IFAIL=0
*** Initialise
      do 100 igas=1,mxngas
      NINn(igas)=0
      NAMEG(igas)='---------------'
      do 110 j=1,6
      KELn(J,igas)=0
      En(j,igas)=0
      do 115 k=1,2048
      Qn(j,k,igas)=0
115   continue
110   continue
      DO 120 j=1,220
      KINn(J,igas)=0
      EInn(j,igas)=0
120   continue
100   continue
*** Energy vector
      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MIXER7 DEBUG   : Mixing'',
     -     '' for Efinal = '',E12.5,'' eV.'')') EFINAL
      ESTEP=EFINAL/DBLE(NSTEP)
      EHALF=ESTEP/2.0D0
      Evect(1)=EHALF
      DO 3 I=2,2048
      AJ=DBLE(I-1)
      Evect(I)=EHALF+ESTEP*AJ
    3 EROOT(I)=SQRT(Evect(I))
      EROOT(1)=SQRT(EHALF)
      DO 6 I=1,512
    6 INDEX(I)=0
*** Call gas cross-sections
      do 10 igas=1,ngas
      CALL GETGAS(NGASN(igas),Qn(1,1,igas),QINn(1,1,igas),NINn(igas),
     -     En(1,igas),EInn(1,igas),NAMEn(igas),
     -     VIRIAL(igas),EBn(igas),PEQELn(1,1,igas),PEQINn(1,1,igas),
     -     KELn(1,igas),KINn(1,igas),SCRPn(1,igas))
      do ie=1,2048
      do ilev=1,6
      if (qn(ilev,ie,igas).lt.0.0d0)then
           print *,' !!!!!! MIXER7 WARNING : Negative elastic'//
     -          ' cross section for gas=',igas,' type=',ilev,
     -          ' energy=',evect(ie),' cs=',qn(ilev,ie,igas),
     -          '; set to 0.'
           qn(ilev,ie,igas)=0
      endif
      enddo
      do ilev=1,ninn(igas)
      if (qinn(ilev,ie,igas).lt.0.0d0)then
           print *,' !!!!!! MIXER7 WARNING : Negative inelastic'//
     -          ' cross section for gas=',igas,' type=',ilev,
     -          ' energy=',evect(ie),' cs=',qinn(ilev,ie,igas),
     -          '; set to 0.'
           qinn(ilev,ie,igas)=0
      endif
      enddo
      enddo
10    continue
C ---------------------------------------------------------------
C  CORRECTION OF NUMBER DENSITY DUE TO VIRIAL COEFFICIENT
C  CAN BE PROGRAMMED HERE NOT YET IMPLEMENTED.
C-----------------------------------------------------------------
C-----------------------------------------------------------------
C     CALCULATION OF COLLISION FREQUENCIES FOR AN ARRAY OF
C     ELECTRON ENERGIES IN THE RANGE ZERO TO EFINAL
C
C     L=5*N-4    ELASTIC NTH GAS
C     L=5*N-3    IONISATION NTH GAS
C     L=5*N-2    ATTACHMENT NTH GAS
C     L=5*N-1    INELASTIC NTH GAS
C     L=5*N      SUPERELASTIC NTH GAS
C---------------------------------------------------------------
      DO 700 IE=1,2048
      FCION(IE)=0.0D0
      FCATT(IE)=0.0D0
*** Initial NP
      NP=0
*** Loop over the gases
      do 710 igas=1,ngas
      NP=NP+1
      CF(IE,NP)=Qn(2,IE,igas)*VANn(igas)
      PSCT(IE,NP)=0.5
      ANGCT(IE,NP)=1.0
      INDEX(NP)=0
      IF(KELn(2,igas).EQ.1) THEN
           PSCT1=PEQELn(2,IE,igas)
           CALL ANGCUT(PSCT1,ANGC,PSCT2)
           ANGCT(IE,NP)=ANGC
           PSCT(IE,NP)=PSCT2
           INDEX(NP)=1
      ENDIF
      IF(IE.eq.1)then
           RGASN(igas)=1.0D0+En(2,igas)/2.0D0
           RGAS(NP)=RGASN(igas)
           EIN(NP)=0.0D0
           IPN(NP)=0
           L=1+5*(igas-1)
           IARRY(NP)=L
           DSCRPT(NP)=SCRPN(2,igas)
           NAMEG(igas)=NAMEn(igas)
      endif
      IF(EFINAL.ge.En(3,igas))then
           NP=NP+1
           CF(IE,NP)=Qn(3,IE,igas)*VANn(igas)
           FCION(IE)=FCION(IE)+CF(IE,NP)
           PSCT(IE,NP)=0.5
           ANGCT(IE,NP)=1.0
           INDEX(NP)=0
           IF(KELn(3,igas).EQ.1) THEN
                PSCT1=PEQELn(3,IE,igas)
                CALL ANGCUT(PSCT1,ANGC,PSCT2)
                ANGCT(IE,NP)=ANGC
                PSCT(IE,NP)=PSCT2
                INDEX(NP)=1
           ENDIF
           IF(IE.EQ.1)then
                RGAS(NP)=RGASN(igas)
                EIN(NP)=En(3,igas)/RGASN(igas)
                WPL(NP)=EBn(igas)
                IPN(NP)=1
                L=2+5*(igas-1)
                IARRY(NP)=L
                DSCRPT(NP)=SCRPN(3,igas)
           endif
      endif
      IF(EFINAL.ge.En(4,igas))then
           NP=NP+1
           CF(IE,NP)=Qn(4,IE,igas)*VANn(igas)
           FCATT(IE)=FCATT(IE)+CF(IE,NP)
           PSCT(IE,NP)=0.5
           ANGCT(IE,NP)=1.0
           IF(IE.eq.1)then
                INDEX(NP)=0
                RGAS(NP)=RGASN(igas)
                EIN(NP)=0.0D0
                IPN(NP)=-1
                L=3+5*(igas-1)
               IARRY(NP)=L
               DSCRPT(NP)=SCRPN(4,igas)
           endif
      endif
      DO 50 J=1,NINN(igas)
      NP=NP+1
      CF(IE,NP)=QINn(J,IE,igas)*VANn(igas)
      PSCT(IE,NP)=0.5
      ANGCT(IE,NP)=1.0
      INDEX(NP)=0
      IF(KINn(J,igas).NE.0) THEN
           PSCT1=PEQINn(J,IE,igas)
           CALL ANGCUT(PSCT1,ANGC,PSCT2)
           ANGCT(IE,NP)=ANGC
           PSCT(IE,NP)=PSCT2
           INDEX(NP)=1
      ENDIF
      IF(IE.eq.1)then
           RGAS(NP)=RGASN(igas)
           EIN(NP)=EInn(J,igas)/RGASN(igas)
           L=4+5*(igas-1)
           IF(EInn(J,igas).LT.0.0D0) L=5+5*(igas-1)
           IPN(NP)=0
           IARRY(NP)=L
           DSCRPT(NP)=SCRPN(6+J,igas)
      endif
   50 CONTINUE
710   continue
*** End of loop updates
      IPLAST=NP
      ISIZE=1
      IF(IPLAST.GE.2)   ISIZE=2
      IF(IPLAST.GE.4)   ISIZE=4
      IF(IPLAST.GE.8)   ISIZE=8
      IF(IPLAST.GE.16)  ISIZE=16
      IF(IPLAST.GE.32)  ISIZE=32
      IF(IPLAST.GE.64)  ISIZE=64
      IF(IPLAST.GE.128) ISIZE=128
      IF(IPLAST.GE.256) ISIZE=256
      IF(IPLAST.GE.512) ISIZE=512
      IF(IPLAST.GE.1024)ISIZE=1024
*** Can increase array size up to 1356 if more complex mixtures used.
*   1356 = 6 * 226 ( 6 = max no of gases. 226 = max no of levels )
      IF(IPLAST.GT.512)then
           print *,' ###### MIXER7 ERROR   : Too many levels in',
     -          ' calculation. Can increase the array sizes from',
     -          ' 512 up to 1356 maximum.'
           IFAIL=1
           RETURN
      ENDIF
*** Calculation of total collision frequency
      TCF(IE)=0.0D0
      DO 610 IF=1,IPLAST
      TCF(IE)=TCF(IE)+CF(IE,IF)
      IF(CF(IE,IF).LT.0.0D0.and.lbmcpr) WRITE(lunout,776)
     -     CF(IE,IF),IE,IF,IARRY(IF),EIN(IF)
  776 FORMAT('  WARNING NEGATIVE COLLISION FEQUENCY =',D12.3,' IE =',I6,
     /' IF =',I3,' IARRY=',I5,' EIN=',F7.4)
 610  CONTINUE
      DO 620 IF=1,IPLAST
      IF(TCF(IE).EQ.0.0D0) GO TO 615
      CF(IE,IF)=CF(IE,IF)/TCF(IE)
      GO TO 620
 615  CF(IE,IF)=0.0D0
 620  CONTINUE
      DO 630 IF=2,IPLAST
      CF(IE,IF)=CF(IE,IF)+CF(IE,IF-1)
 630  CONTINUE
      FCATT(IE)=FCATT(IE)*EROOT(IE)
      FCION(IE)=FCION(IE)*EROOT(IE)
      TCF(IE)=TCF(IE)*EROOT(IE)
 700  CONTINUE
C     if(lbmcpr)WRITE(lunout,841) (INDEX(J),J, J=1,IPLAST)
C 841 FORMAT(2X,' INDEX=',I3,' J=',I3)
*** Set anisotropic flag if anisotropic scattering data is detected
      KELSUM=0
      do 703 igas=1,ngas
      DO 701 J=1,6
      KELSUM=KELSUM+KELn(J,igas)
701   continue
      DO 702 J=1,220
      KELSUM=KELSUM+KINn(J,igas)
702   continue
703   continue
      IF(KELSUM.GT.0) NISO=1
C     IF(NISO.EQ.1.and.lbmcpr) WRITE(lunout,7765) NISO
C7765 FORMAT(3X,' ANISOTROPIC SCATTERING DETECTED NISO=',I5)
*** Calculate null collision frequency
      BP=EMAG*EMAG*CONST1
      F2=EMAG*CONST3
      ELOW=TMAX*(TMAX*BP-F2*SQRT(0.5D0*EFINAL))/ESTEP-1.0D0
      ELOW=MIN(ELOW,SMALL)
      EHI=TMAX*(TMAX*BP+F2*SQRT(0.5D0*EFINAL))/ESTEP+1.0D0
      IF(EHI.GT.10000.) EHI=10000.
      DO 810 I=1,8
      JLOW=2048-256*(9-I)+1+INT(ELOW)
      JHI=2048-256*(8-I)+INT(EHI)
      JLOW=MAX(JLOW,1)
      JHI=MIN(JHI,2048)
      DO 800 J=JLOW,JHI
      IF(TCF(J).GE.TCFMAX(I)) TCFMAX(I)=TCF(J)
  800 CONTINUE
  810 CONTINUE
*** Cross section data for integrals in  output
      DO 900 I=1,NSTEP
      qtot(i)=0
      qel(i)=0
      do 901 igas=1,ngas
*   Total and elastic cross sections
      QTOT(I)=qtot(i)+ANn(igas)*Qn(1,I,igas)
      QEL(I)=qel(i)+ANn(igas)*Qn(2,I,igas)
*   Ionisation and attachment for individual gases
      QION(igas,I)=Qn(3,I,igas)*ANn(igas)
      QATT(igas,I)=Qn(4,I,igas)*ANn(igas)
901   continue
*   Totals
      QREL(I)=0.0D0
      QSATT(I)=0.0D0
      QSUM(I)=0.0D0
      DO 855 igas=1,NGAS
      QSUM(I)=QSUM(I)+QION(igas,I)+QATT(igas,I)
      QSATT(I)=QSATT(I)+QATT(igas,I)
      QREL(I)=QREL(I)+QION(igas,I)-QATT(igas,I)
      DO 860 J=1,NINN(igas)
      QSUM(I)=QSUM(I)+QINn(J,I,igas)*ANn(igas)
860   continue
855   continue
*
900   CONTINUE
      END
