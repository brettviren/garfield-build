CDECK  ID>, PTG.
      SUBROUTINE PTG
      IMPLICIT REAL*8 (A-H,O-Z)
       DOUBLE PRECISION ALPHAST,VDST,TSTEP,ZSTEP,TFINAL,ZFINAL
       INTEGER ITFINAL,IPRIM
       COMMON/CION/ALPHAST,VDST,TSTEP,ZSTEP,TFINAL,ZFINAL,ITFINAL,IPRIM
*   Combined /TPLOUT/, /TPLOUTG/, /TPLOUTH/ in a single common.
       DOUBLE PRECISION ETPL,XTPL,YTPL,ZTPL,TTPL,XXTPL,YYTPL,ZZTPL,
     -      YZTPL,XZTPL,XYTPL,VZTPL,VYTPL,VXTPL,ATTOINT,ATTERT,AIOERT
       INTEGER NETPL
       COMMON /TPLOUT/ ETPL(8),XTPL(8),YTPL(8),ZTPL(8),TTPL(8),XXTPL(8),
     -      YYTPL(8),ZZTPL(8),YZTPL(8),XZTPL(8),XYTPL(8),
     -      VZTPL(8),VYTPL(8),VXTPL(8),ATTOINT,ATTERT,AIOERT,NETPL(8)
*   Combined /PTTOF/, /PTTOFG/ and /PTTOFH/ in a single common.
       DOUBLE PRECISION RI,EPT,VZPT,VYPT,VXPT,TTEST
       COMMON /PTTOF/ RI(8),EPT(8),VZPT(8),VYPT(8),VXPT(8),TTEST(8)
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
      DIMENSION ANTPL(8)
C ------------------------------------------------
C CALCULATES PULSED TOWNSEND COEFFICIENTS
C-------------------------------------------------
      ANTPL(1)=DBLE(NETPL(1))
      RI(1)=(LOG(ANTPL(1))-LOG(DBLE(IPRIM)))/TSTEP
      EPT(1)=ETPL(1)/ANTPL(1)
      TTEST(1)=TTPL(1)/ANTPL(1)
      VZPT(1)=1.0D+09*VZTPL(1)/ANTPL(1)
      VYPT(1)=1.0D+09*VYTPL(1)/ANTPL(1)
      DO 10 I=2,ITFINAL
      IF(NETPL(I).EQ.0) THEN
       ITFINAL=I-1
       GO TO 11
      ENDIF
      ANTPL(I)=DBLE(NETPL(I))
      RI(I)=(LOG(ANTPL(I))-LOG(ANTPL(I-1)))/TSTEP
      EPT(I)=ETPL(I)/ANTPL(I)
      TTEST(I)=TTPL(I)/ANTPL(I)
      VZPT(I)=1.0D+09*VZTPL(I)/ANTPL(I)
      VYPT(I)=1.0D+09*VYTPL(I)/ANTPL(I)
  10  CONTINUE
  11  IF(LBMCPR)WRITE(LUNOUT,900) ITFINAL
 900  FORMAT(2(/),' PULSED TOWNSEND RESULTS AT',I2,' SEQUENTIAL TIME PLA
     /NES',/,'PLANE   (ION-ATT)FRQ.    ENERGY      WVZ       WVY   NO.OF
     / ELECTRONS',/)
      DO 20 IPL=1,ITFINAL
       IF(LBMCPR)WRITE(LUNOUT,910)
     -      IPL,RI(IPL),EPT(IPL),VZPT(IPL),VYPT(IPL),NETPL(IPL)
 910  FORMAT(1X,I2,4X,D12.4,4X,F7.2,4X,F6.1,4X,F6.1,4X,I8)
  20  CONTINUE
      END
