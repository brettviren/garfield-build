CDECK  ID>, SPLANE.
      SUBROUTINE SPLANE(T,E1,DCX1,DCY1,DCZ1,AP,BP,EFLD,TIMLFT,IZPLANE)
      IMPLICIT REAL*8 (A-H,O-Z)
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
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
C--------------------------------------------------------
C STORES POSITION, TIME AND ENERGY AND SUMS REQUIRED
C TO CALCULATE DEVIATIONS AND MEANS AT PLANE =IZPLANE
C----------------------------------------------------
      IF(IZPLANE.GT.8) RETURN
      T2LFT=TIMLFT*TIMLFT
      A=AP*TIMLFT
      B=BP*T2LFT
      EPLANE=E1+A+B
      CONST6=SQRT(E1/EPLANE)
C     DCX2=DCX1*CONST6
C     DCY2=DCY1*CONST6
      DCZ2=DCZ1*CONST6+EFLD*TIMLFT*CONST5/SQRT(EPLANE)
      XPLANE=X+DCX1*TIMLFT*SQRT(E1)*CONST3*0.01D0
      YPLANE=Y+DCY1*TIMLFT*SQRT(E1)*CONST3*0.01D0
      ZPLANE=Z+DCZ1*TIMLFT*SQRT(E1)*CONST3*0.01D0+T2LFT*EFLD*CONST2
      VZPLANE=DCZ2*SQRT(EPLANE)*CONST3*0.01D0
      WGHT=ABS(1.0D0/VZPLANE)
      RPLANE=SQRT(XPLANE**2+YPLANE**2)
      XSPL(IZPLANE)=XSPL(IZPLANE)+XPLANE*WGHT
      YSPL(IZPLANE)=YSPL(IZPLANE)+YPLANE*WGHT
      RSPL(IZPLANE)=RSPL(IZPLANE)+RPLANE*WGHT
      ZSPL(IZPLANE)=ZSPL(IZPLANE)+ZPLANE*WGHT
      TMSPL(IZPLANE)=TMSPL(IZPLANE)+(ST+TIMLFT)*WGHT
      TTMSPL(IZPLANE)=TTMSPL(IZPLANE)+(ST+TIMLFT)*(ST+TIMLFT)*WGHT
      XXSPL(IZPLANE)=XXSPL(IZPLANE)+XPLANE*XPLANE*WGHT
      YYSPL(IZPLANE)=YYSPL(IZPLANE)+YPLANE*YPLANE*WGHT
      RRSPM(IZPLANE)=RRSPM(IZPLANE)+RPLANE*RPLANE*WGHT
      ZZSPL(IZPLANE)=ZZSPL(IZPLANE)+ZPLANE*ZPLANE*WGHT
      ESPL(IZPLANE)=ESPL(IZPLANE)+EPLANE*WGHT
      TSPL(IZPLANE)=TSPL(IZPLANE)+WGHT/(ST+TIMLFT)
      VZSPL(IZPLANE)=VZSPL(IZPLANE)+VZPLANE*WGHT
      TSSUM(IZPLANE)=TSSUM(IZPLANE)+WGHT
      TSSUM2(IZPLANE)=TSSUM2(IZPLANE)+WGHT*WGHT
      END
