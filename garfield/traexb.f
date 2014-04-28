CDECK  ID>, TRAEXB.
       SUBROUTINE TRAEXB(XIN,VIN,XOUT,VOUT,ENERGY,STEP,IFAIL)
*-----------------------------------------------------------------------
*   TRAEXB - Traces an electron through an E and B field.
*   (Last changed on 19/ 1/09.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION WGT,FPRMAT,
     -      FPROJ,FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX,GYBOX,GZBOX
       REAL PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM
       INTEGER NLINED,NGRIDX,NGRIDY,ITRTYP,NTRLIN,NTRSAM,INDPOS,NCTRW,
     -      NTRFLX,NINORD,
     -      NCPNAM,NCXLAB,NCYLAB,NCFPRO,IPRMAT,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,ITFSRM,NTRERR
       LOGICAL LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG,LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       COMMON /PARMS / WGT(MXLIST),FPRMAT(3,3),
     -      FPROJ(3,3),FPROJA,FPROJB,FPROJC,FPROJD,FPROJN,
     -      EPSGX,EPSGY,EPSGZ,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,
     -      GXBOX(12),GYBOX(12),GZBOX(12),
     -      PXMIN,PYMIN,PZMIN,PXMAX,PYMAX,PZMAX,
     -      PRTHL,PRPHIL,PRAL,PRBL,PRCL,PROROT,
     -      PRFABS,PRFREF,PRFMIN,PRFMAX,PRFCAL,WLMIN,WLMAX,
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      TRMASS,TRENER,TRCHAR,TRXDIR,TRYDIR,TRZDIR,TRTH,TRPHI,TRDIST,
     -      TRFLUX,TRELEC,TRNSRM,
     -      INDPOS(11000),IPRMAT(3),NCTRW,NCPNAM,
     -      ITRTYP,NTRLIN,NTRSAM,NTRFLX,ITFSRM,NTRERR(10),
     -      NLINED,NINORD,NGRIDX,NGRIDY,NCXLAB,NCYLAB,NCFPRO,
     -      NPRCOL,ICOL0,ICOLBX,ICOLPL,ICOLST,ICOLW1,ICOLW2,ICOLW3,
     -      ICOLD1,ICOLD2,ICOLD3,ICOLRB,NGBOX,
     -      LTRMS,LTRDEL,LTRINT,LTREXB,LTRCUT,TRFLAG(10),LINCAL,
     -      LFULLB,LFULLP,LFULLT,LSPLIT,LSORT,LOUTL,LEPSG,LGSTEP,
     -      LDLSRM,LDTSRM,LTRVVL
       CHARACTER*80 PARTID,PXLAB,PYLAB,PROLAB
       CHARACTER*10 PNAME
       CHARACTER*5  PRVIEW
       CHARACTER*(MXCHAR) FCNTRW
       COMMON /PARCHR/ PARTID,FCNTRW,PNAME,PXLAB,PYLAB,PROLAB,PRVIEW
       DOUBLE PRECISION XIN(3),XOUT(3),VEL(3),
     -      DT,T,WORK(18),SPEED,VNORM,STEP,RADIUS,GAMMA
       REAL BX,BY,BZ,BTOT,XPOS,YPOS,ZPOS,VIN(3),VOUT(3),ENERGY
       INTEGER I,IFAIL,NSTEP
       EXTERNAL TRASUB
       COMMON /EXBCOM/ GAMMA
*** For now, assume that the routine will fail.
       IFAIL=1
*** Ensure that the energy is larger than 0.
       IF(ENERGY.LE.0)THEN
            PRINT *,' !!!!!! TRAEXB WARNING : Energy is not > 0;'//
     -           ' not traced.'
            RETURN
       ENDIF
*** Compute particle's speed (eV gives m/sec, need MeV to cm/microsec)
       SPEED=CLIGHT*SQRT(1-1/(1+(ECHARG*ENERGY)/
     -      (100*EMASS*CLIGHT**2))**2)
*** Compute gamma factor which we'll need for the trajectory.
       GAMMA=1/SQRT(1-(SPEED/CLIGHT)**2)
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRAEXB DEBUG   : Energy: '',
     -      E15.8,'' MeV''/26X,''Speed:  '',E15.8,'' cm/microsec''/
     -      26X,''Gamma:  '',E15.8/
     -      26X,''Starting position: '',3E12.5/
     -      26X,''Starting velocity: '',3E12.5)') ENERGY,SPEED,GAMMA,
     -      (XIN(I),I=1,3),(VIN(I),I=1,3)
*** Establish the speed vector.
       VNORM=SQRT(VIN(1)**2+VIN(2)**2+VIN(3)**2)
       IF(VNORM.LE.0)THEN
            PRINT *,' !!!!!! TRAEXB WARNING : Speed vector has norm'//
     -           ' 0; not traced.'
            RETURN
       ENDIF
       VEL(1)=SPEED*VIN(1)/VNORM
       VEL(2)=SPEED*VIN(2)/VNORM
       VEL(3)=SPEED*VIN(3)/VNORM
*** First estimate of the step size to be taken.
       NSTEP=10
       DT=STEP/(10*SPEED)
*** Estimate bending radius so as to get the scale for integration.
       XPOS=XT0+COS(TRPHI)*XIN(1)-
     -      SIN(TRPHI)*SIN(TRTH)*XIN(2)+
     -      SIN(TRPHI)*COS(TRTH)*XIN(3)
       YPOS=YT0+COS(TRTH)*XIN(2)+
     -      SIN(TRTH)*XIN(3)
       ZPOS=ZT0-SIN(TRPHI)*XIN(1)-
     -      COS(TRPHI)*SIN(TRTH)*XIN(2)+
     -      COS(TRPHI)*COS(TRTH)*XIN(3)
       CALL BFIELD(XPOS,YPOS,ZPOS,BX,BY,BZ,BTOT)
       IF(BTOT.GT.0)THEN
            RADIUS=1.0D8*(EMASS*SPEED)/(ECHARG*BTOT)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRAEXB DEBUG   :'',
     -           '' Bending radius: '',E15.8,'' cm.'')') RADIUS
            IF(RADIUS.LT.STEP)THEN
                 NSTEP=NSTEP*2*NINT(STEP/RADIUS)
                 DT=DT/(2*NINT(STEP/RADIUS))
            ENDIF
       ENDIF
*** Starting conditions.
       T=0
*** Make steps.
       XOUT(1)=XIN(1)
       XOUT(2)=XIN(2)
       XOUT(3)=XIN(3)
       DO 10 I=1,NSTEP
       CALL DRKNYS(3,DT,T,XOUT,VEL,TRASUB,WORK)
10     CONTINUE
*** At the end, return the new velocity vector.
       VNORM=SQRT(VEL(1)**2+VEL(2)**2+VEL(3)**2)
       VOUT(1)=VEL(1)/VNORM
       VOUT(2)=VEL(2)/VNORM
       VOUT(3)=VEL(3)/VNORM
*** Things seem to have worked properly.
       IFAIL=0
       END
