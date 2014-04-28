CDECK  ID>, DLCDIV.
       SUBROUTINE DLCDIV(X0,Y0,Z0,VC,RVC,DIV,DSTEP,TSTEP,Q,ITYPE,IFAIL)
*-----------------------------------------------------------------------
*   DLCDIV - Computes the divergence of the drift field.
*   (Last changed on 26/10/07.)
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
       DOUBLE PRECISION X0,Y0,Z0,XS,YS,ZS,VC(3),
     -      RVC(3,3),DIV(3),F0(3),F1(3),F2(3),PHIC(3),PHIV(3,6),
     -      CI0,CI1,CI2,BETA10,BETA20,BETA21,TSTEP,DSTEP,
     -      FACT1,FACT2,FACT3
       REAL Q
       INTEGER ILOC0,ILOC1,ILOC2,ITYPE,I,J,K,IFAIL
      logical ll
C      parameter(ll=.true.)
      parameter(ll=.false.)
*** Initialise the constants appearing in the RKF formulas.
       PARAMETER(CI0   =214.0D0/ 891.0D0,CI1   =   1.0D0/  33.0D0,
     -           CI2   =650.0D0/ 891.0D0,
     -           BETA10=  1.0D0/   4.0D0,BETA20=-189.0D0/ 800.0D0,
     -           BETA21=729.0D0/ 800.0D0)
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCDIV ///'
*** Set failure flag.
       IFAIL=1
*** Trace 6 lines to compute the F factors using the "c" frame.
       DO 10 I=1,6
*   Parameters of starting point.
       IF(I.LE.2)THEN
            FACT1=2*(REAL(I)-1.5)
            FACT2=0
            FACT3=0
       ELSEIF(I.LE.4)THEN
            FACT1=0
            FACT2=2*(REAL(I)-3.5)
            FACT3=0
       ELSE
            FACT1=0
            FACT2=0
            FACT3=2*(REAL(I)-5.5)
       ENDIF
*   Set starting point.
       XS=X0+DSTEP*(FACT1*RVC(1,1)+FACT2*RVC(2,1)+FACT3*RVC(3,1))
       YS=Y0+DSTEP*(FACT1*RVC(1,2)+FACT2*RVC(2,2)+FACT3*RVC(3,2))
       ZS=Z0+DSTEP*(FACT1*RVC(1,3)+FACT2*RVC(2,3)+FACT3*RVC(3,3))
      if(ll)print *,' Start ',i,':',xs,ys,zs
*   Compute velocities at probe points.
       CALL DLCVEL(XS,
     -      YS,
     -      ZS,
     -      F0,Q,ITYPE,ILOC0)
       CALL DLCVEL(XS+TSTEP*BETA10*F0(1),
     -      YS+TSTEP*BETA10*F0(2),
     -      ZS+TSTEP*BETA10*F0(3),
     -      F1,Q,ITYPE,ILOC1)
       CALL DLCVEL(
     -      XS+TSTEP*(BETA20*F0(1)+BETA21*F1(1)),
     -      YS+TSTEP*(BETA20*F0(2)+BETA21*F1(2)),
     -      ZS+TSTEP*(BETA20*F0(3)+BETA21*F1(3)),
     -      F2,Q,ITYPE,ILOC2)
*   If hitting non-free zone, suppress scaling.
       IF(ILOC0.NE.0.OR.ILOC1.NE.0.OR.ILOC2.NE.0)THEN
            DO 20 J=1,3
            PHIC(J)=VC(J)
20          CONTINUE
      if(ll)print *,' Hit non-free point, set to nominal'
       ELSE
*   Compute mean velocity vector for this path.
            PHIC(1)=CI0*F0(1)+CI1*F1(1)+CI2*F2(1)
            PHIC(2)=CI0*F0(2)+CI1*F1(2)+CI2*F2(2)
            PHIC(3)=CI0*F0(3)+CI1*F1(3)+CI2*F2(3)
       ENDIF
      if(ll)print *,' Vel_c ',i,': ',(phic(j),j=1,3)
      if(ll)print *,' End   ',i,': ',xs+tstep*phic(1),
     -     ys+tstep*phic(2),zs+tstep*phic(3)
*   Store the mean velocities in the "v" frame.
       DO 30 J=1,3
       PHIV(J,I)=0
       DO 40 K=1,3
       PHIV(J,I)=PHIV(J,I)+RVC(J,K)*PHIC(K)
40     CONTINUE
30     CONTINUE
      if(ll)print *,' Vel_v ',I,': ',(phiv(j,i),j=1,3)
*   Verify that the neighbouring lines also go forward.
       IF(PHIV(1,I).LE.0)THEN
            PRINT *,' !!!!!! DLCDIV WARNING : A neighbouring drift'//
     -           ' line is going in reverse; terminating.'
            RETURN
       ENDIF
*   Next drift line.
10     CONTINUE
*** Compute scalings
       DIV(1)=1+TSTEP*(PHIV(1,2)-PHIV(1,1))/(2*DSTEP)
       DIV(2)=1+TSTEP*(PHIV(2,4)-PHIV(2,3))/(2*DSTEP)
       DIV(3)=1+TSTEP*(PHIV(3,6)-PHIV(3,5))/(2*DSTEP)
      if(ll)print *,'Scaling: ',(div(j),j=1,3)
*** Ensure that the scalings are positive.
       IF(DIV(1).LE.0.OR.DIV(2).LE.0.OR.DIV(3).LE.0)THEN
            PRINT *,' !!!!!! DLCDIV WARNING : Found negative'//
     -           ' convergence factors; scaling set to 1.'
            DIV(1)=1
            DIV(2)=1
            DIV(3)=1
       ENDIF
*** Things seem to have worked.
       IFAIL=0
       END
