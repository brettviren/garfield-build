CDECK  ID>, GASB7.
       SUBROUTINE GASB7(EE,BB,BTH,TT,PP,SSTTHR,NNMAX,IFAIL)
*-----------------------------------------------------------------------
*   GASB7  - Interface to Magboltz 7, originally the main program.
*   Author: Steve Biagi, extensively modified.
*   (Last changed on 17/ 3/09.)
*-----------------------------------------------------------------------
       implicit none
*   Array dimensions.
       integer mxngas
       parameter(mxngas=6)
       DOUBLE PRECISION EOVB,WB,BTHETA,BMAG
       COMMON/BFLD/EOVB,WB,BTHETA,BMAG
       INTEGER NGAS,NSTEP,IDBG
       DOUBLE PRECISION EFINAL,ESTEP,AKT,ARY,TEMPC,TORR
       PARAMETER(ARY=13.60569172)
       COMMON/INPT/NGAS,NSTEP,EFINAL,ESTEP,AKT,TEMPC,TORR,IDBG
       DOUBLE PRECISION TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX,RSTART,
     -      EMAG
       INTEGER NMAX
       COMMON/SETP/TMAX,SMALL,API,ESTART,THETA,PHI,TCFMAX(8),RSTART,
     -      EMAG,NMAX
*   Adjusted size of ICOLL
       DOUBLE PRECISION TIME,SPEC,TMAX1,AVE,DEN,XID,X,Y,Z,ST
       INTEGER ICOLL,NNULL,ICOLN
       COMMON/OUTPT/TIME(300),ICOLL(5*mxngas),SPEC(2048),TMAX1,
     -      AVE,DEN,XID,X,Y,Z,ST,NNULL,ICOLN(512)
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
       INTEGER NRETRY,IELOW,IFAIL,IFAIL1,NNMAX
       DOUBLE PRECISION EOP,ALPP,ATTP,SSTMIN,TGAS,EE,BB,BTH,TT,PP,
     -      SSTTHR,EFMAX
       PARAMETER(EFMAX=100000.0)
*** Identify the procedure.
       IF(LIDENT)PRINT *,' /// ROUTINE GASB7 ///'
*** Assume this will work.
       IFAIL=0
*** Set parameters
       CALL SETB7(EE,BB,BTH,TT,PP,NNMAX,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GASB7  WARNING : Setup phase of Magboltz'//
     -           ' failed at E=',EE,' B=',BB,' angle=',BTH,
     -           '; not computing transport parameters.'
            IFAIL=1
            RETURN
       ENDIF
*** Calculate EFINAL if set to 0.
       IF(EFINAL.le.0.0D0)THEN
*   Start at 0.5 eV. If E/p > 15 start at 8.0 eV.
            EFINAL=0.5D0
            EOP=EMAG*(TEMPC+273.15D0)/(TORR*293.15D0)
            IF(EOP.GT.15.0D0) EFINAL=8.0D0
            ESTART=EFINAL/50.0D0
*   Keep track of the number of retries
            NRETRY=0
10          CONTINUE
            NRETRY=NRETRY+1
            CALL MIXER7(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASB7  WARNING : Gas mixing phase'//
     -                ' of Magboltz failed; not computing transport'//
     -                ' parameters.'
                 IFAIL=1
                 RETURN
            ENDIF
*   Loop to calculate EFINAL
            IF(BMAG.EQ.0.0D0.OR.BTHETA.EQ.0.0D0.OR.
     -           ABS(BTHETA).EQ.180.0D0)THEN
                 CALL ELIMIT(IELOW)
            ELSEIF(BTHETA.EQ.90.0D0) THEN
                 CALL ELIMITB(IELOW)
            ELSE
                 CALL ELIMITC(IELOW)
            ENDIF
            IF(IELOW.EQ.1) THEN
                 EFINAL=EFINAL*SQRT(2.0D0)
                 ESTART=EFINAL/50.0D0
                 IF(EFINAL.GT.EFMAX)THEN
                      EFINAL=EFMAX
                      ESTART=EFINAL/50.0
                      CALL MIXER7(IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                            PRINT *,' !!!!!! GASB7  WARNING : Mixing'//
     -                           ' for Efinal = Emax failed; not'//
     -                           '  computing transport parameters.'
                            IFAIL=1
                            RETURN
                      ELSE
                            PRINT *,' ------ GASB7  WARNING : Reached'//
     -                           ' maximum parametrised e- energy;'//
     -                           ' please check energy distributions.'
                            LF0PLT=.TRUE.
                      ENDIF
                 ELSEIF(NRETRY.LT.50)THEN
                      GO TO 10
                 ELSE
                      PRINT *,' !!!!!! GASB7  WARNING : Calculation'//
     -                     ' of EFINAL did not converge; reduced'//
     -                     ' precision.'
                 ENDIF
            ENDIF
**  Otherwise simply mix the gases using the set value.
       ELSE
            CALL MIXER7(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASB7  WARNING : Gas mixing phase'//
     -                ' of Magboltz failed; not computing transport'//
     -                ' parameters.'
                 IFAIL=1
                 RETURN
            ENDIF
       ENDIF
*** Start printing.
       CALL PRNTER
*** Monte Carlo loops, again keep track of repetitions
       NRETRY=0
20     CONTINUE
       NRETRY=NRETRY+1
*   Call the appropriate routine
       IF(BMAG.EQ.0.0D0)THEN
            CALL MONTE
       ELSEIF(BTHETA.EQ.0.0D0.OR.BTHETA.EQ.180.0D0) THEN
            CALL MONTEA
       ELSEIF(BTHETA.EQ.90.0D0) THEN
            CALL MONTEB
       ELSE
            CALL MONTEC
       ENDIF
*   Check for convergence.
       IF(SPEC(2048).GT.500.0D0) THEN
            IF(EFINAL*2.GT.EFMAX)THEN
                 EFINAL=EFMAX
                 ESTART=EFINAL/50.0
                 CALL MIXER7(IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                       PRINT *,' !!!!!! GASB7  WARNING : Mixing'//
     -                      ' for Efinal = Emax failed; not'//
     -                      '  computing transport parameters.'
                       IFAIL=1
                       RETURN
                 ELSE
                       PRINT *,' ------ GASB7  WARNING : Reached'//
     -                      ' maximum parametrised e- energy;'//
     -                      ' please check energy distributions.'
                       LF0PLT=.TRUE.
                 ENDIF
            ELSEIF(NRETRY.LT.5)THEN
                 EFINAL=EFINAL*2
                 ESTART=EFINAL/50.0D0
                 PRINT *,' ------ GASB7  MESSAGE : Energy range is'//
     -                ' insufficient; increasing to ',EFINAL,' eV'//
     -                ' and trying again.'
                 IF(LDEBUG)CALL F0PLT7('Insufficient E range')
                 CALL SETB7I
                 CALL MIXER7(IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! GASB7  WARNING : Gas mixing'//
     -                     ' phase of Magboltz failed; not computing'//
     -                     ' transport parameters.'
                      IFAIL=1
                      RETURN
                 ENDIF
                 GO TO 20
            ELSE
                 PRINT *,' !!!!!! GASB7  WARNING : Reached maximum'//
     -                ' number of range increases; reduced precision.'
            ENDIF
       ENDIF
*** Output from initial MC loops
       CALL OUTPUT
*** If attachment or ionisation is greater than SSTMIN, include spatial
*   gradients in the solution .
       TGAS=273.15D0+TEMPC
       ALPP=ALPHA*760.0D0*TGAS/(TORR*293.15D0)
       ATTP=ATT*760.0D0*TGAS/(TORR*293.15D0)
*   Set the value of SSTMIN
       SSTMIN=SSTTHR
*   Check whether the threshold is passed
C       IF(ALPP.GT.SSTMIN.OR.ATTP.GT.SSTMIN)then
*   Modification at the request of Steve Biagi (RV 6/9/2007).
       IF(ABS(ALPP-ATTP).GT.SSTMIN)then
            IF(BMAG.EQ.0.0D0) THEN
                 CALL ALPCALC(IFAIL1)
            ELSE IF(BTHETA.EQ.0.0D0.OR.BTHETA.EQ.180.0D0) THEN
                 CALL ALPCLCA(IFAIL1)
            ELSE IF(BTHETA.EQ.90.0D0) THEN
                 CALL ALPCLCB(IFAIL1)
            ELSE
                 CALL ALPCLCC(IFAIL1)
            ENDIF
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASB7  WARNING : Failure performing'//
     -                ' high-field calculations.'
                 IFAIL=1
                 RETURN
            ENDIF
      ENDIF
*   Output after the full calculations
      CALL OUTPUT2
      END
