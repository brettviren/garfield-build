CDECK  ID>, GASLEX.
      SUBROUTINE GASLEX(IFAIL)
*-----------------------------------------------------------------------
*   GASLEX - Extracts all excitations and ionisations.
*   (Last changed on 29/ 6/09.)
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
*   Changed name of common from /NAMES/ to /MBGNAM/ for Mac OS X
       CHARACTER*15 NAMEG
       COMMON /MBGNAM/ NAMEG(mxngas)
*   Grouped AN1 ... AN6 in ANn
       DOUBLE PRECISION ANn,AN,FRAC
       COMMON /RATIO/ ANn(mxngas),AN,FRAC(mxngas)
       INTEGER NGASN
       COMMON /GASN/ NGASN(mxngas)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*15 NAME
       CHARACTER*30 SCRPT(226)
       CHARACTER*80 GASID
       DOUBLE PRECISION Q(6,2048),QIN(220,2048),E(6),EI(220),
     -      PEQEL(6,2048),PEQIN(220,2048),VIRIAL,EB
       REAL PGAS,TGAS,GASFRM(MXGNAM),GASSUM
       INTEGER KIN(220),KEL(6),NIN,I,J,IFAIL,IREF,IFAIL1,IFAIL2,
     -      IFAIL3,IFAIL4,IFAIL5,IFAIL6
*** Retrieve pressure and temperature.
       CALL GASINF(PGAS,TGAS,GASID,GASFRM)
*** Loop over all gases
       GASSUM=0
       DO 10 I=1,MXGNAM
       IF(GASFRM(I).LE.0)GOTO 10
       GASSUM=GASSUM+GASFRM(I)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   : Gas '',I3,
     -      '' has fraction '',F10.3)') I,GASFRM(I)
*   Pressure and temperature.
       TEMPC=20.0
       TORR=760.0
*   Set a final energy such that all excitations are present.
       EFINAL=1000.0
*   Density correction factors.
       AKT=(ABZERO+TEMPC)*BOLTZ
*   Store the gas mixing data.
       NGAS=1
       FRAC(1)=100.0
       NGASN(1)=I
*   Retrieve the gas data (1 is not a specific cross section).
       CALL GETGAS(I,Q,QIN,NIN,E,EI,NAME,VIRIAL,EB,PEQEL,PEQIN,
     -     KEL,KIN,SCRPT)
*   The elastic term is entry 2.
       CALL GASIDO(IREF,NAME//SCRPT(2),1,REAL(E(2)),IFAIL1)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   :'',
     -      '' Elastic:       '',A,'' (Eloss = '',E12.5,'' eV)'')')
     -      SCRPT(2),E(2)
*   The ionisation is at entry 3.
       CALL GASIDO(IREF,NAME//SCRPT(3),2,REAL(E(3)),IFAIL2)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   :'',
     -      '' Ionisation:    '',A,'' (Eloss = '',E12.5,'' eV)'')')
     -      SCRPT(3),E(3)
*   The attachment is at entry 4.
       CALL GASIDO(IREF,NAME//SCRPT(4),3,REAL(E(4)),IFAIL3)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   :'',
     -      '' Attachment:    '',A,'' (Eloss = '',E12.5,'' eV)'')')
     -      SCRPT(4),E(4)
*   The inelastic, super-elastic + excitations are in 6+I, I=1,NIN.
       DO 20 J=1,NIN
       IF(SCRPT(6+J)(1:4).NE.' EXC'.AND.EI(J).GT.0)THEN
            CALL GASIDO(IREF,NAME//SCRPT(6+J),4,REAL(EI(J)),IFAIL4)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   :'',
     -           '' Inelastic:     '',A,'' (Eloss = '',E12.5,'' eV)'')')
     -           SCRPT(6+J),EI(J)
       ELSEIF(SCRPT(6+J)(1:4).NE.' EXC')THEN
            CALL GASIDO(IREF,NAME//SCRPT(6+J),5,REAL(EI(J)),IFAIL5)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   :'',
     -           '' Super-elastic: '',A,'' (Eloss = '',E12.5,'' eV)'')')
     -           SCRPT(6+J),EI(J)
       ELSE
            CALL GASIDO(IREF,NAME//SCRPT(6+J),6,REAL(EI(J)),IFAIL6)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASLEX DEBUG   :'',
     -           '' Excitation:    '',A,'' (Eloss = '',E12.5,'' eV)'')')
     -           SCRPT(6+J),EI(J)
       ENDIF
20     CONTINUE
*   Check error flags.
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.IFAIL4.NE.0.OR.
     -      IFAIL5.NE.0.OR.IFAIL6.NE.0)THEN
            PRINT *,' !!!!!! GASLEX WARNING : Error storing a cross'//
     -           ' section term; abandoning.'
            IFAIL=1
            RETURN
       ENDIF
*   Next gas
10     CONTINUE
*** Check that there is a gas.
       IF(GASSUM.LE.0)THEN
            PRINT *,' !!!!!! GASLEX WARNING : Gas composition does'//
     -           ' not contain a single element; table empty.'
            IFAIL=1
       ELSE
            IFAIL=0
       ENDIF
       END
