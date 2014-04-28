CDECK  ID>, DLCDIC.
       SUBROUTINE DLCDIC(X1,Y1,Z1,FDIV1,FDIV2,FDIV3,Q,ITYPE,IFAIL)
*-----------------------------------------------------------------------
*   DLCDIC - Procedure call interface for DLCDIV.
*   (Last changed on 11/ 6/02.)
*-----------------------------------------------------------------------
       implicit none
       INTEGER MXWIRE,MXSW,MXLIST,MXCHA,MXGRID,MXMATT,MXPOLE,MX3D,
     -         MXPSTR,
     -         MXPAIR,MXPART,MXFOUR,MXCLUS,
     -         MXLINE,MXEQUT,
     -         MXRECL,MXINCH,MXWORD,MXCHAR,MXNAME,MXLUN,
     -         MXINS,MXREG,MXARG,MXCONS,MXVAR,MXALGE,
     -         MXZERO,MXSTCK,MXFPNT,MXFPAR,MXWKLS,
     -         MXHLEV,MXHLRL,MXSUBT,
     -         MXDLVL,MXILVL,MXDLIN,
     -         MXHIST,MXFRAC,MXBANG,MXBTAB,
     -         MXEXG,MXIOG,MXCSG,
     -         MXORIA,
     -         MXMAT,MXEMAT,MXMDIM,
     -         MXSHOT,MXZPAR,
     -         MXMAP,MXEPS,MXWMAP,MXSOLI,MXSBUF,
     -         MXPLAN,MXPOIN,MXEDGE,
     -         MXMCA
       PARAMETER (MXWIRE=  2000,MXSW  =  200)
       PARAMETER (MXMATT=    10)
       PARAMETER (MX3D  =   100)
       PARAMETER (MXPOLE=    10)
       PARAMETER (MXPSTR=   100)
       PARAMETER (MXLIST=  1000)
       PARAMETER (MXHIST=   200, MXCHA = MXLIST/2)
       PARAMETER (MXGRID=    50)
       PARAMETER (MXNAME=   200, MXLUN =    30)
       PARAMETER (MXCLUS=   500, MXPAIR=  2000, MXPART= 10000)
       PARAMETER (MXLINE=   150, MXEQUT=    50)
       PARAMETER (MXFOUR=    16)
       PARAMETER (MXRECL= 10000)
       PARAMETER (MXINCH=  2000, MXWORD=   200, MXCHAR=MXINCH)
       PARAMETER (MXINS =  1000, MXREG =   500, MXCONS=  -500,
     -            MXVAR =   500, MXALGE=   500, MXARG =   100)
       PARAMETER (MXMAT =   500, MXEMAT=200000, MXMDIM=   10)
       PARAMETER (MXZERO=MXWIRE)
       PARAMETER (MXSTCK=     5)
       PARAMETER (MXFPNT= 20000, MXFPAR=    10)
       PARAMETER (MXWKLS=    10)
       PARAMETER (MXHLEV=     9, MXSUBT=   200, MXHLRL=  860)
       PARAMETER (MXDLVL=    10, MXILVL=    20, MXDLIN= 2500)
       PARAMETER (MXFRAC=    13)
       PARAMETER (MXBANG=    20, MXBTAB=    25)
       PARAMETER (MXEXG =    50, MXIOG =    10, MXCSG =  200)
       PARAMETER (MXORIA=  1000)
       PARAMETER (MXSHOT=    10, MXZPAR=4*MXSHOT+2)
       PARAMETER (MXMAP =350000,MXEPS =   10)
       PARAMETER (MXWMAP=     5)
       PARAMETER (MXSOLI=  1000)
       PARAMETER (MXPLAN= 50000, MXPOIN=100000,MXEDGE=100)
       PARAMETER (MXSBUF= 20000)
       PARAMETER (MXMCA = 50000)
*   The parameter MXNBMC must equal MXGNAM (sequence MAGBPARM) !
       INTEGER MXNBMC
       PARAMETER(MXNBMC=60)
       DOUBLE PRECISION XU,YU,ZU,TU,XTARG,YTARG,TMC,DMC
       REAL DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,DTARG,EPSDFI,EPSTWI,
     -      EPSATI,RDF2,DSCMIN,DSCMAX,DTFACT,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,EPSDIF,RTRAP,
     -      STMAX,EQTTHR,EQTASP,EQTCLS,QPCHAR
       INTEGER NU,ISTAT,ITARG,MXDIFS,MXTWNS,MXATTS,MDF2,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,
     -      IPTYPE,IPTECH
       LOGICAL LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       COMMON /DRFDAT/ XU(MXLIST),YU(MXLIST),ZU(MXLIST),TU(MXLIST),
     -      XTARG,YTARG,TMC,DMC,DTARG,
     -      DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,
     -      DDXMIN,DDXMAX,DDYMIN,DDYMAX,DDZMIN,DDZMAX,
     -      EQTTHR,EQTASP,EQTCLS,QPCHAR,
     -      RTRAP,STMAX,EPSDIF,EPSDFI,EPSTWI,EPSATI,RDF2,DSCMIN,DSCMAX,
     -      DTFACT,MDF2,
     -      MXDIFS,MXTWNS,MXATTS,
     -      NU,ISTAT,ITARG,
     -      ISTAT1,ISTAT2,ISTAT3,ISTAT4,ISTAT5,ISTAT6,NMC,MCMETH,IPTYPE,
     -      IPTECH,LREPSK,LKINK,LSTMAX,LEQSRT,LEQCRS,LEQMRK,LAVPRO
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       COMPLEX ICONS
       REAL PI,CLOG2,EPS0,ECHARG,EMASS,CLIGHT,MEV2KG,BOLTZ,GRAV
       PARAMETER (PI=3.141592653589793238,
     -      CLOG2=0.693147180559945309417,
     -      ICONS=(0.0,1.0),
     -      EPS0=8.854187817E-14,
     -      ECHARG=1.60217733E-19,
     -      EMASS=9.1093897E-31,
     -      GRAV=9.80665,
     -      CLIGHT=2.99792458E4,
     -      MEV2KG = 1.782661845E-30,
     -      BOLTZ=1.380658E-23)
       REAL X1,Y1,Z1,FDIV1,FDIV2,FDIV3,Q,TCOLL,EX,EY,EZ,ETOT,VOLT
       INTEGER ITYPE,IFAIL,IFAIL1,ILOC,ILOC0,ILOC1,ILOC2
       DOUBLE PRECISION RVC(3,3),X0,Y0,Z0,F0(3),F1(3),F2(3),VC(3),
     -      CI0,CI1,CI2,BETA10,BETA20,BETA21,TSTEP,DSTEP,RNORM,
     -      DIV(3)
      logical ll
*      parameter(ll=.true.)
      parameter(ll=.false.)
*** Initialise the constants appearing in the RKF formulas.
       PARAMETER(CI0   =214.0D0/ 891.0D0,CI1   =   1.0D0/  33.0D0,
     -           CI2   =650.0D0/ 891.0D0,
     -           BETA10=  1.0D0/   4.0D0,BETA20=-189.0D0/ 800.0D0,
     -           BETA21=729.0D0/ 800.0D0)
*** Set failure flag.
       IFAIL=1
*** Set initial values.
       FDIV1=1
       FDIV2=1
       FDIV3=1
*** Create double precision copies of the input location.
       X0=DBLE(X1)
       Y0=DBLE(Y1)
       Z0=DBLE(Z1)
*** Compute the drift velocity ("c" frame).
       CALL DLCVEL(X0,Y0,Z0,F0,Q,ITYPE,ILOC0)
*   Ensure the norm is not zero.
       IF(SQRT(F0(1)**2+F0(2)**2+F0(3)**2).LE.0.OR.ILOC0.NE.0)THEN
            PRINT *,' !!!!!! DLCDIC WARNING : Initial velocity is'//
     -           ' zero or location is not free; abandoned.'
            RETURN
       ENDIF
*** Set the step size, either fixed time steps ...
       IF(MCMETH.EQ.0)THEN
            TSTEP=TMC
*   or fixed distance steps ...
       ELSEIF(MCMETH.EQ.1)THEN
            TSTEP=DMC/SQRT(F0(1)**2+F0(2)**2+F0(3)**2)
*   or steps based on collision time ...
       ELSE
            CALL EFIELD(REAL(X0),REAL(Y0),REAL(Z0),EX,EY,EZ,ETOT,
     -           VOLT,0,ILOC)
            TCOLL=1E8*EMASS*SQRT(F0(1)**2+F0(2)**2+F0(3)**2)/
     -           (ECHARG*SQRT(EX**2+EY**2+EZ**2))
            TSTEP=NMC*TCOLL
      if(ll)print *,' Collision time=',TCOLL*1000000,' psec'
       ENDIF
*   Make a rough estimate of the length of this step.
       DSTEP=TSTEP*SQRT(F0(1)**2+F0(2)**2+F0(3)**2)
      if(ll)print *,'Time step: ',tstep,' Distance: ',dstep
*** Compute velocity at probe points needed for the RKF formula.
       CALL DLCVEL(X0+TSTEP*BETA10*F0(1),
     -      Y0+TSTEP*BETA10*F0(2),
     -      Z0+TSTEP*BETA10*F0(3),
     -      F1,Q,ITYPE,ILOC1)
       CALL DLCVEL(
     -      X0+TSTEP*(BETA20*F0(1)+BETA21*F1(1)),
     -      Y0+TSTEP*(BETA20*F0(2)+BETA21*F1(2)),
     -      Z0+TSTEP*(BETA20*F0(3)+BETA21*F1(3)),
     -      F2,Q,ITYPE,ILOC2)
*   Check only location codes.
       IF(ILOC0.NE.0.OR.ILOC1.NE.0.OR.ILOC2.NE.0)THEN
            PRINT *,' !!!!!! DLCDIC WARNING : Non-free location code'//
     -           ' found while tracking a particle; abandoned.'
            RETURN
       ENDIF
*   If everything is OK, compute effective velocity for this step.
       VC(1)=CI0*F0(1)+CI1*F1(1)+CI2*F2(1)
       VC(2)=CI0*F0(2)+CI1*F1(2)+CI2*F2(2)
       VC(3)=CI0*F0(3)+CI1*F1(3)+CI2*F2(3)
      if(ll)print *,' Step to: ',x0+tstep*vc(1),y0+tstep*vc(2),
     -     z0+tstep*vc(2)
*** Rotation matrix from "c" to "v" frame, axis 1: effective velocity.
       RVC(1,1)=VC(1)
       RVC(1,2)=VC(2)
       RVC(1,3)=VC(3)
       RNORM=SQRT(RVC(1,1)**2+RVC(1,2)**2+RVC(1,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCDIC WARNING : Zero norm found for'//
     -           ' rotation matrix axis 1; abandoned.'
            RETURN
       ENDIF
       RVC(1,1)=RVC(1,1)/RNORM
       RVC(1,2)=RVC(1,2)/RNORM
       RVC(1,3)=RVC(1,3)/RNORM
*   Axis 2: orthogonal in the 2 largest components.
       IF(ABS(VC(1)).GE.ABS(VC(3)).AND.ABS(VC(2)).GE.ABS(VC(3)))THEN
            RVC(2,1)=-VC(2)
            RVC(2,2)=VC(1)
            RVC(2,3)=0
       ELSEIF(ABS(VC(1)).GE.ABS(VC(2)).AND.ABS(VC(3)).GE.ABS(VC(2)))THEN
            RVC(2,1)=-VC(3)
            RVC(2,2)=0
            RVC(2,3)=VC(1)
       ELSE
            RVC(2,1)=0
            RVC(2,2)=VC(3)
            RVC(2,3)=-VC(2)
       ENDIF
       RNORM=SQRT(RVC(2,1)**2+RVC(2,2)**2+RVC(2,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCDIC WARNING : Zero norm found for'//
     -           ' rotation matrix axis 2; abandoned.'
            RETURN
       ENDIF
       RVC(2,1)=RVC(2,1)/RNORM
       RVC(2,2)=RVC(2,2)/RNORM
       RVC(2,3)=RVC(2,3)/RNORM
*   Axis 3: vectorial product of axes 1 and 2.
       RVC(3,1)=RVC(1,2)*RVC(2,3)-RVC(1,3)*RVC(2,2)
       RVC(3,2)=RVC(1,3)*RVC(2,1)-RVC(1,1)*RVC(2,3)
       RVC(3,3)=RVC(1,1)*RVC(2,2)-RVC(1,2)*RVC(2,1)
       RNORM=SQRT(RVC(3,1)**2+RVC(3,2)**2+RVC(3,3)**2)
       IF(RNORM.LE.0)THEN
            PRINT *,' !!!!!! DLCDIC WARNING : Zero norm found for'//
     -           ' rotation matrix axis 3; abandoned.'
            RETURN
       ENDIF
       RVC(3,1)=RVC(3,1)/RNORM
       RVC(3,2)=RVC(3,2)/RNORM
       RVC(3,3)=RVC(3,3)/RNORM
*** Call the divergence routine.
       CALL DLCDIV(X0,Y0,Z0,VC,RVC,DIV,DSTEP,TSTEP,Q,ITYPE,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DLCDIC WARNING : Divergence calculation'//
     -           ' failed; abandoned.'
            RETURN
       ENDIF
*** Return the results.
       FDIV1=REAL(DIV(1))
       FDIV2=REAL(DIV(2))
       FDIV3=REAL(DIV(3))
*** Seems to have worked.
       IFAIL=0
       END
