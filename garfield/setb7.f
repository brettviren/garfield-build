CDECK  ID>, SETB7.
       SUBROUTINE SETB7(EE,BB,BTH,TT,PP,NNMAX,IFAIL)
*-----------------------------------------------------------------------
*   SETB7  - Sets parameters for Magboltz 7, originally called SETUP.
*   Author: Originally from Steve Biagi, extensively modified.
*   (Last changed on 21/12/05.)
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
*   Grouped AN1 ... AN6 in ANn
       DOUBLE PRECISION ANn,AN,FRAC
       COMMON /RATIO/ ANn(mxngas),AN,FRAC(mxngas)
       INTEGER NGASN
       COMMON /GASN/ NGASN(mxngas)
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
       DOUBLE PRECISION CON
       INTEGER ITHRM
       COMMON /THRM/ CON,ITHRM
       DOUBLE PRECISION EOVB,WB,BTHETA,BMAG
       COMMON/BFLD/EOVB,WB,BTHETA,BMAG
*   Grouped VAN1 ... VAN6 in VANn
       DOUBLE PRECISION VANn,VAN
       COMMON /MRATIO/ VANn(mxngas),VAN
       DOUBLE PRECISION ZTOT,TTOT,ZTOTS,TTOTS
       COMMON/TTRM/ZTOT,TTOT,ZTOTS,TTOTS
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,I,J,NSCALE,NNMAX
       DOUBLE PRECISION EE,BB,BTH,TT,PP,EOVM,TOTFRAC,CORR
*** Identify the procedure.
       IF(LIDENT)PRINT *,' /// ROUTINE SETB7 ///'
*** New update of constants 1998
       API=ACOS(-1.0D0)
       EOVM=SQRT(2.0D0*ECHARG/EMASS)*100.0D0
       CONST1=AWB/2.0D0*1.0D-19
       CONST2=CONST1*1.0D-02
       CONST3=SQRT(0.2D0*AWB)*1.0D-09
       CONST4=CONST3*ALOSCH*1.0D-15
       CONST5=CONST3/2.0D0
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SETB7  DEBUG   : E='',E15.2,
     -      '' V/cm, B='',E15.2,'' T, angle='',F10.3,'' degrees''/26X,
     -      ''T='',F10.3,'' K, p='',F10.3,'' Torr, n='',I10)')
     -      EE,BB,BTH,TT,PP,NNMAX
*** Assume things will work.
       IFAIL=0
*** Number of collisions.
       NMAX=NNMAX
*** Request automatic calculation of EFINAL.
       EFINAL=0.0
*** Establish the gas mixture.
       NGAS=0
       TOTFRAC=0.0
       DO 10 I=1,mxgnam
       IF(FRAMIX(I).GT.0)THEN
*   Ensure the limit on gas components is not exceeded.
            IF(NGAS.GE.mxngas)THEN
                  PRINT *,' !!!!!! SETB7  WARNING : The mixture'//
     -                 ' consists of more than MXNGAS components.'
                  PRINT *,'                         Adjust this'//
     -                 ' parameter and recompile the program.'
                  IFAIL=1
                  RETURN
            ENDIF
*   Add a new gas to the list.
            NGAS=NGAS+1
            FRAC(NGAS)=FRAMIX(I)
            TOTFRAC=TOTFRAC+FRAC(NGAS)
            NGASN(NGAS)=I
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SETB7  DEBUG   : Gas '',
     -           I3,'' id='',I3,'' frac='',E12.5)')
     -           NGAS,NGASN(NGAS),FRAC(NGAS)
       ENDIF
10     CONTINUE
*   Check that there is some gas to be scaled.
       IF(TOTFRAC.LE.0.OR.NGAS.LE.0)THEN
            PRINT *,' !!!!!! SETB7  WARNING : No gas present in the'//
     -           ' mixture; setup aborted.'
            IFAIL=1
            RETURN
       ENDIF
*** Temperature and pressure.
       TEMPC=TT-273.15D0
       TORR=PP
*   Density correction factors.
       CORR=ABZERO*TORR/(ATMOS*(ABZERO+TEMPC))
       AKT=(ABZERO+TEMPC)*BOLTZ
*   Scale the fractions.
       DO 20 J=1,NGAS
       ANn(J)=FRAC(J)*CORR*ALOSCH/TOTFRAC
       VANn(J)=FRAC(J)*CORR*CONST4*1.0D15/TOTFRAC
20     CONTINUE
*** Field values
       EMAG=EE
       BMAG=BB
       BTHETA=BTH
*** Set remaining parameters.
      TMAX=100.0D0
      NSCALE=10000000
      NMAX=NMAX*NSCALE
      IF(NMAX.LT.0) THEN
           PRINT *,' !!!!!! SETB7  WARNING : NMAX is too large and'//
     -          ' has caused overflow; no setup phase performed.'
           IFAIL=1
           RETURN
      ENDIF
      NSTEP=2048
*** Initialisations that may need to be redone.
      CALL SETB7I
      END
