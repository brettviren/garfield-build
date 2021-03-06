CDECK  ID>, SST.
      SUBROUTINE SST
      IMPLICIT REAL*8 (A-H,O-Z)
       DOUBLE PRECISION ALPHAST,VDST,TSTEP,ZSTEP,TFINAL,ZFINAL
       INTEGER ITFINAL,IPRIM
       COMMON/CION/ALPHAST,VDST,TSTEP,ZSTEP,TFINAL,ZFINAL,ITFINAL,IPRIM
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
       DOUBLE PRECISION VDOUT,VDERR,WSOUT,WSERR,DLOUT,DLERR,DTOUT,DTERR,
     -      ALPHSST,ALPHERR,ATTSST,ATTERR
       COMMON/SSTOUT/VDOUT,VDERR,WSOUT,WSERR,DLOUT,DLERR,DTOUT,DTERR,
     -      ALPHSST,ALPHERR,ATTSST,ATTERR
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
      DIMENSION ESST(8),VDSST(8),WSSST(8),DXSST(8),DYSST(8),WTEMP(8)
      DIMENSION DRSST(8)
      DIMENSION ALFNE(8),ALFNJ(8),ALFN(8),ZSST(8),DLSST(8)
      DIMENSION NEPL(8)
C----------------------------------------------------------------------
C CALCULATES STEADY STATE TOWNSEND COEFFICIENTS.
C LOADS REULTS AND ERRORS INTO COMMON BLOCKS /SSTOUT/
C -------------------------------------------------------------------
      VDOUT=0.0D0
      VDERR=0.0D0
      WSOUT=0.0D0
      WSERR=0.0D0
      DLOUT=0.0D0
      DLERR=0.0D0
      DTOUT=0.0D0
      DTERR=0.0D0
      ALPHSST=0.0D0
      ALPHERR=0.0D0
      ATTSST=0.0D0
      ATTERR=0.0D0
      JPRINT=IZFINAL
C CALCULATE NUMBER OF ELECTRONS AT EACH PLANE
      NEPL(1)=IPRIM+NESST(1)
      DO 21 K=2,JPRINT
      NEPL(K)=NEPL(K-1)+NESST(K)
  21  CONTINUE
C  SUBSTITUTE NEPL FOR NEEST
      DO 22 K=1,JPRINT
  22  NESST(K)=NEPL(K)
      DO 23 I=1,JPRINT
      IF(NESST(I).EQ.0) THEN
       JPRINT=I-1
       GO TO 24
      ENDIF
  23  CONTINUE
  24  ESST(1)=ESPL(1)/TSSUM(1)
      ZSST(1)=ZSPL(1)/TSSUM(1)
      VDSST(1)=VZSPL(1)/TSSUM(1)
      WTEMP(1)=ZSTEP*TSSUM(1)/TMSPL(1)
      WSSST(1)=WTEMP(1)
      DXSST(1)=((XXSPL(1)/TSSUM(1))-(XSPL(1)/TSSUM(1))**2)*WSSST(1)/
     /(2.0D0*ZSTEP)
      DYSST(1)=((YYSPL(1)/TSSUM(1))-(YSPL(1)/TSSUM(1))**2)*WSSST(1)/
     /(2.0D0*ZSTEP)
      DLSST(1)=((TTMSPL(1)/TSSUM(1))-(TMSPL(1)/TSSUM(1))**2)*WSSST(1)**3
     //(2.0D0*ZSTEP)
      IF(NESST(1).EQ.0) GO TO 1
      ALFNE(1)=(LOG(DBLE(NESST(1)))-LOG(DBLE(IPRIM)))/ZSTEP
    1 ALFNJ(1)=0.0D0
      ALFN(1)=0.0D0
      DO 10 I=2,JPRINT
      ESST(I)=ESPL(I)/TSSUM(I)
      ZSST(I)=ZSPL(I)/TSSUM(I)
      VDSST(I)=VZSPL(I)/TSSUM(I)
      WTEMP(I)=ZSTEP*DBLE(I)*TSSUM(I)/TMSPL(I)
      WSSST(I)=(WTEMP(I)*WTEMP(I-1))/(I*WTEMP(I-1)-(I-1)*WTEMP(I))
      DXSST(I)=((XXSPL(I)/TSSUM(I))-(XSPL(I)/TSSUM(I))**2-(XXSPL(I-1)/
     /TSSUM(I-1))+(XSPL(I-1)/TSSUM(I-1))**2)*WSSST(I)/(2.0D0*ZSTEP)
      DYSST(I)=((YYSPL(I)/TSSUM(I))-(YSPL(I)/TSSUM(I))**2-(YYSPL(I-1)/
     /TSSUM(I-1))+(YSPL(I-1)/TSSUM(I-1))**2)*WSSST(I)/(2.0D0*ZSTEP)
      DLSST(I)=((TTMSPL(I)/TSSUM(I))-(TMSPL(I)/TSSUM(I))**2-(TTMSPL(I-1)
     //TSSUM(I-1))+(TMSPL(I-1)/TSSUM(I-1))**2)*WSSST(I)**3/(2.0D0*ZSTEP)
      ALFN(I)=(LOG(TSSUM(I))-LOG(TSSUM(I-1)))/ZSTEP
      ALFNJ(I)=(LOG(TSSUM(I)*VDSST(I))-LOG(TSSUM(I-1)*VDSST(I-1)))/ZST
     /EP
      IF(NESST(I).EQ.0.OR.NESST(I-1).EQ.0) GO TO 10
   10 ALFNE(I)=(LOG(DBLE(NESST(I)))-LOG(DBLE(NESST(I-1))))/ZSTEP
      DXFIN=((XXSPL(JPRINT)/TSSUM(JPRINT))-(XSPL(JPRINT)/TSSUM(JPRINT))
     /**2)*WSSST(JPRINT)/(JPRINT*2.0D0*ZSTEP)
      DXFIN=DXFIN*1.0D+16
      DYFIN=((YYSPL(JPRINT)/TSSUM(JPRINT))-(YSPL(JPRINT)/TSSUM(JPRINT))
     /**2)*WSSST(JPRINT)/(JPRINT*2.0D0*ZSTEP)
      DYFIN=DYFIN*1.0D+16
      DLFIN=((TTMSPL(JPRINT)/TSSUM(JPRINT))-(TMSPL(JPRINT)/TSSUM(JPRINT)
     /)**2)*WSSST(JPRINT)**3/(JPRINT*2.0D0*ZSTEP)
      DLFIN=DLFIN*1.0D+16
      ALNGTH=ZSTEP*DBLE(JPRINT)
      ALFIN=LOG(DBLE(NESST(JPRINT))/DBLE(IPRIM))/ALNGTH
      ALFIN=ALFIN*0.01D0
      DO 15 J=1,JPRINT
      VDSST(J)=VDSST(J)*1.0D+09
      WSSST(J)=WSSST(J)*1.0D+09
      DXSST(J)=DXSST(J)*1.0D+16
      DYSST(J)=DYSST(J)*1.0D+16
      DLSST(J)=DLSST(J)*1.0D+16
      ALFN(J)=ALFN(J)*0.01D0
      ALFNJ(J)=ALFNJ(J)*0.01D0
      ALFNE(J)=ALFNE(J)*0.01D0
  15  CONTINUE
      IF(LBMCPR)WRITE(LUNOUT,800) JPRINT
 800  FORMAT(2(/),' STEADY STATE TOWNSEND RESULTS FOR',I2,' SEQUENTIAL S
     /PACE PLANES',2(/),'PLANE   NEL     VD      WS     DL       DT
     /EBAR   ALFN    ALFNJ   ALFNE',/)
      DO 20 IPL=1,JPRINT
      DRSST(IPL)=(DXSST(IPL)+DYSST(IPL))/2.0
  20   IF(LBMCPR)WRITE(LUNOUT,810)
     -      IPL,NESST(IPL),VDSST(IPL),WSSST(IPL),DLSST(IPL),DRSST(IPL),
     -      ESST(IPL),ALFN(IPL),ALFNJ(IPL),ALFNE(IPL)
 810  FORMAT(1X,I2,2X,I7,2(1X,F6.1),2F9.1,F6.1,3F8.1)
      IF(NESST(1).GT.NESST(5)) THEN
C NET ATTACHMENT THEREFORE TAKE RESULTS FROM PLANE 2
       VDOUT=VDSST(2)
       VDERR=100.0D0*ABS((VDSST(2)-VDSST(3))/(2.0D0*VDSST(2)))
       WSOUT=WSSST(2)
       WSERR=100.0D0*ABS((WSSST(2)-WSSST(3))/(2.0D0*WSSST(2)))
       DLOUT=DLSST(2)
       DLERR=100.0D0*ABS((DLSST(2)-DLSST(3))/(2.0D0*DLSST(2)))
       DTOUT=DRSST(2)
       DTERR=100.0D0*ABS((DRSST(2)-DRSST(3))/(2.0D0*DRSST(2)))
       IF(ATTOION.EQ.-1.0D0) THEN
C NO IONISATION
        ALPHSST=0.0
        ALPHERR=0.0
        ANST2=DBLE(NESST(2))
        ANST3=DBLE(NESST(3))
        ANST4=ANST3-SQRT(ANST3)
        ANST5=LOG(ANST2/ANST3)
        ANST6=LOG(ANST2/ANST4)
        ANST7=ANST6/ANST5
        ANST8=ANST7-1.0D0
        ATTSST=-(ALFN(2)+ALFNJ(2)+ALFNE(2))/3.0D0
        ATTERR=100.0D0*SQRT(ANST8**2+ATTATER**2)
       ELSE
        ANST2=DBLE(NESST(2))
        ANST3=DBLE(NESST(3))
        ANST4=ANST3-SQRT(ANST3)
        ANST5=LOG(ANST2/ANST3)
        ANST6=LOG(ANST2/ANST4)
        ANST7=ANST6/ANST5
        ANST8=ANST7-1.0D0
        ATMP=(ALFN(2)+ALFNJ(2)+ALFNE(2))/3.0D0
        ALPHSST=ATMP/(1.0D0-ATTOION)
        ALPHERR=100.0D0*SQRT(ANST8**2+ATTIOER**2)
        ATTSST=ATTOION*ATMP/(1.0D0-ATTOION)
        ATTERR=100.0D0*SQRT(ANST8**2+ATTATER**2)
       ENDIF
      ELSE
C NET IONISATION THEREFORE TAKE RESULTS FROM PLANE 8
       VDOUT=VDSST(8)
       VDERR=100.0D0*ABS((VDSST(8)-VDSST(7))/(2.0D0*VDSST(8)))
       WSOUT=WSSST(8)
       WSERR=100.0D0*ABS((WSSST(8)-WSSST(7))/(2.0D0*WSSST(8)))
       DLOUT=DLFIN
       DLERR=100.0D0*ABS((DLOUT-DLSST(8))/(2.0D0*DLOUT))
       DTOUT=(DXFIN+DYFIN)/2.0D0
       DTERR=100.0D0*ABS((DTOUT-DRSST(8))/(2.0D0*DTOUT))
       ATMP=(ALFN(8)+ALFNJ(8)+ALFNE(8))/3.0D0
       ATMP2=(ALFN(7)+ALFNJ(7)+ALFNE(7))/3.0D0
       ATER=ABS((ATMP-ATMP2)/(2.0D0*ATMP))
       ALPHSST=ATMP/(1.0D0-ATTOION)
       ALPHERR=100.0D0*SQRT(ATER**2+ATTIOER**2)
       ATTSST=ATTOION*ATMP/(1.0D0-ATTOION)
       IF(ATTOION.NE.0.0D0) THEN
        ATTERR=100.0D0*SQRT(ATER**2+ATTATER**2)
       ELSE
        ATTERR=0.0D0
       ENDIF
      ENDIF
      END
