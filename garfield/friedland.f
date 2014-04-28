CDECK  ID>, FRIEDLAND.
      SUBROUTINE FRIEDLAND
*-----------------------------------------------------------------------
*   FRIEDL - Calculate distribution function using Friedland
*            technique. Use distribution function to calculate average
*            energy ionisation rate and attachment rate.
*   Reference: J. Friedland, Physics of Fluids 20(1461)1977
*   Author: Steve Biagi
*   (Last changed on 23/ 5/05.)
*-----------------------------------------------------------------------
      implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       DOUBLE PRECISION FCION,FCATT
       COMMON/FRED/FCION(2048),FCATT(2048)
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       DOUBLE PRECISION CONST1,CONST2,CONST3,CONST4,CONST5
       COMMON/CNSTS1/CONST1,CONST2,CONST3,CONST4,CONST5
       DOUBLE PRECISION ZTOT,TTOT,ZTOTS,TTOTS
       COMMON/TTRM/ZTOT,TTOT,ZTOTS,TTOTS
*   Sometimes IPLAST is called LAST
       DOUBLE PRECISION CF,EIN,TCF,RGAS,WPL
       INTEGER IARRY,IPN,IPLAST,ISIZE
       COMMON/LARGE/CF(2048,512),EIN(512),TCF(2048),IARRY(512),
     -      RGAS(512),IPN(512),WPL(512),IPLAST,ISIZE
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
      DOUBLE PRECISION FR(2048),ALFBAR,ATTBAR,EBAR,FSUM
      INTEGER I
*** Initialise.
      ALFBAR=0.0D0
      ATTBAR=0.0D0
      EBAR=0.0D0
      FSUM=0.0D0
      DO 100 I=1,2048
      FR(I)=SPEC(I)/TCF(I)
*   Changed E to EVECT (RV, 20/4/2005)
      EBAR=EBAR+Evect(I)*SPEC(I)/TCF(I)
      ALFBAR=ALFBAR+FCION(I)*SPEC(I)/TCF(I)
      ATTBAR=ATTBAR+FCATT(I)*SPEC(I)/TCF(I)
 100  FSUM=FSUM+FR(I)
*** Normalise
      DO 200 I=1,2048
 200  FR(I)=FR(I)/FSUM
      EBAR=EBAR/TTOTS
      ALFBAR=ALFBAR/TTOTS
      ATTBAR=ATTBAR/TTOTS
*** Output result.
      IF(LBMCPR)WRITE(LUNOUT,900) EBAR,ALFBAR,ATTBAR
 900  FORMAT(2(/),' ESTIMATE USING FRIEDLAND :',/,' AVERAGE ENERGY =',
     -     F8.3,'EV.',/,' AVERAGE IONISATION =',E11.4,' *10**12/SEC'/
     -     ' AVERAGE ATTACHMENT =',E11.4,' *10**12/SEC')
      END
