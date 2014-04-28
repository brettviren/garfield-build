CDECK  ID>, SRMMST.
       SUBROUTINE SRMMST(EKIN,DE,STEP,MODEL,STPMIN,IFAIL)
*-----------------------------------------------------------------------
*   SRMMST - Determines the smallest step size for which there is little
*            or no risk of finding negative energy fluctuations.
*   VARIABLES : EKIN       : Kinetic energy [MeV]
*               DE         : Mean em energy loss over the step [MeV]
*               STEP       : Step length as guessed [cm]
*               STPMIN     : Minimum step length needed [cm]
*               BETA2      : Velocity-squared
*               GAMMA      : Projectile gamma
*               EMAX       : Maximum energy transfer per collision [MeV]
*               XI         : Rutherford term [MeV]
*               FCONST     : Proportionality constant
*               EMASS      : Electron mass [MeV]
*   (Last changed on 25/ 3/07.)
*-----------------------------------------------------------------------
       implicit none
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       DOUBLE PRECISION CLSDIS,CLSAVE
       REAL EGAS,VGAS,XGAS,YGAS,DGAS,AGAS,BGAS,HGAS,MGAS,WGAS,OGAS,SGAS,
     -      EXGAS,IOGAS,
     -      CVGAS,CXGAS,CYGAS,CDGAS,CAGAS,CBGAS,CHGAS,CMGAS,CWGAS,COGAS,
     -      CSGAS,CEXGAS,CIOGAS,
     -      VGAS2,XGAS2,YGAS2,DGAS2,AGAS2,BGAS2,HGAS2,MGAS2,WGAS2,OGAS2,
     -      SGAS2,EXGAS2,IOGAS2,
     -      AORIG,AORIG2,PENPRB,PENRMS,PENDT,ENIOG,ENEXG,
     -      BANG,BTAB,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1,SEXTR2,SEXTR3,SEXTR4,
     -      EEXTR1,EEXTR2,EEXTR3,EEXTR4,
     -      ZEXTR1,ZEXTR2,ZEXTR3,ZEXTR4,
     -      GASRNG,
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,GASFRM,ELOSCS
       LOGICAL GASOK,TAB2D,GASOPT,HEEDOK,SRIMOK,TRIMOK,GASSET
       INTEGER NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP
       CHARACTER*80 GASID
       CHARACTER*(MXCHAR) FCNTAB,FCNCLS
       CHARACTER*10 CLSTYP
       CHARACTER*45 DSCEXG(MXEXG),DSCIOG(MXIOG),DSCCSG(MXCSG)
       COMMON /GASDAT/ CLSDIS(MXPAIR),CLSAVE,
     -      EGAS(MXLIST),
     -      VGAS(MXLIST),XGAS(MXLIST),YGAS(MXLIST),WGAS(MXLIST),
     -      DGAS(MXLIST),OGAS(MXLIST),AGAS(MXLIST),BGAS(MXLIST),
     -      HGAS(MXLIST),MGAS(MXLIST),SGAS(MXLIST,6),
     -      EXGAS(MXLIST,MXEXG),IOGAS(MXLIST,MXIOG),
     -      CVGAS(MXLIST),CXGAS(MXLIST),CYGAS(MXLIST),CWGAS(MXLIST),
     -      CDGAS(MXLIST),COGAS(MXLIST),CAGAS(MXLIST),CBGAS(MXLIST),
     -      CHGAS(MXLIST),CMGAS(MXLIST),CSGAS(MXLIST,6),
     -      CEXGAS(MXLIST,MXEXG),CIOGAS(MXLIST,MXIOG),
     -      VGAS2(MXLIST,MXBANG,MXBTAB),WGAS2(MXLIST,MXBANG,MXBTAB),
     -      XGAS2(MXLIST,MXBANG,MXBTAB),YGAS2(MXLIST,MXBANG,MXBTAB),
     -      AGAS2(MXLIST,MXBANG,MXBTAB),BGAS2(MXLIST,MXBANG,MXBTAB),
     -      DGAS2(MXLIST,MXBANG,MXBTAB),OGAS2(MXLIST,MXBANG,MXBTAB),
     -      HGAS2(MXLIST,MXBANG,MXBTAB),MGAS2(MXLIST,MXBANG,MXBTAB),
     -      SGAS2(MXLIST,MXBANG,MXBTAB,6),
     -      EXGAS2(MXLIST,MXBANG,MXBTAB,MXEXG),
     -      IOGAS2(MXLIST,MXBANG,MXBTAB,MXIOG),
     -      AORIG(MXLIST),AORIG2(MXLIST,MXBANG,MXBTAB),
     -      PENPRB(MXEXG),PENRMS(MXEXG),PENDT(MXEXG),
     -      ENIOG(MXIOG),ENEXG(MXEXG),
     -      BANG(MXBANG),BTAB(MXBTAB),
     -      GASRNG(20,2),GASFRM(MXNBMC),ELOSCS(MXCSG),
     -      Z,A,RHO,CMEAN,EMPROB,EPAIR,PGAS,TGAS,GASDEN,
     -      DTION,DLION,
     -      VEXTR1,VEXTR2,VEXTR3,VEXTR4,
     -      XEXTR1,XEXTR2,XEXTR3,XEXTR4,
     -      YEXTR1,YEXTR2,YEXTR3,YEXTR4,
     -      DEXTR1,DEXTR2,DEXTR3,DEXTR4,
     -      AEXTR1,AEXTR2,AEXTR3,AEXTR4,
     -      BEXTR1,BEXTR2,BEXTR3,BEXTR4,
     -      HEXTR1,HEXTR2,HEXTR3,HEXTR4,
     -      MEXTR1,MEXTR2,MEXTR3,MEXTR4,
     -      WEXTR1,WEXTR2,WEXTR3,WEXTR4,
     -      OEXTR1,OEXTR2,OEXTR3,OEXTR4,
     -      SEXTR1(6),SEXTR2(6),SEXTR3(6),SEXTR4(6),
     -      EEXTR1(MXEXG),EEXTR2(MXEXG),EEXTR3(MXEXG),EEXTR4(MXEXG),
     -      ZEXTR1(MXIOG),ZEXTR2(MXIOG),ZEXTR3(MXIOG),ZEXTR4(MXIOG),
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IHMETH,IMMETH,
     -      IWMETH,IOMETH,ISMETH,IEMETH,IZMETH,
     -      IVEXTR,IXEXTR,IYEXTR,IDEXTR,IAEXTR,IBEXTR,IHEXTR,IMEXTR,
     -      IWEXTR,IOEXTR,ISEXTR,IEEXTR,IZEXTR,
     -      JVEXTR,JXEXTR,JYEXTR,JDEXTR,JAEXTR,JBEXTR,JHEXTR,JMEXTR,
     -      JWEXTR,JOEXTR,JSEXTR,JEEXTR,JZEXTR,
     -      NGAS,NCLS,NBANG,NBTAB,NFTAB,NFCLS,
     -      IATHR,IBTHR,IHTHR,
     -      NEXGAS,NIOGAS,NCSGAS,ICSTYP(MXCSG),
     -      GASOK(20),GASOPT(20,4),
     -      TAB2D,HEEDOK,SRIMOK,TRIMOK,GASSET
       COMMON /GASCHR/ FCNTAB,FCNCLS,CLSTYP,GASID,DSCEXG,DSCIOG,DSCCSG
       REAL EKIN,DE,STEP,STPMIN,
     -      FCONST,EMASS,GAMMA,BETA2,EMAX,XLMIN,RKLIM,
     -      XI,XINEW,RKAPPA,RKNEW,STPNOW,DENOW,EXPMAX
       INTEGER MODEL,IFAIL,ITER
       LOGICAL RETRY
*** Fixed values.
       PARAMETER(FCONST=0.1534, EMASS=0.510999,EXPMAX=30)
*** By default, assume the step is right.
       STPMIN=STEP
*** Count interations
       ITER=0
*** By default, this works.
       IFAIL=0
*** Check correctness.
       IF(EKIN.LE.0.OR.DE.LE.0.OR.STEP.LE.0)THEN
            PRINT *,' !!!!!! SRMMST WARNING : Input parameters not'//
     -           ' valid: Ekin = ',EKIN,' MeV, dE = ',DE,
     -           ' MeV, step length = ',STEP,' cm.'
            IFAIL=1
            RETURN
       ELSEIF(TRMASS.LE.0.OR.ABS(TRCHAR).LE.0)THEN
            PRINT *,' !!!!!! SRMMST WARNING : Track parameters not'//
     -           ' valid: mass = ',TRMASS,' MeV, charge = ',TRCHAR
            IFAIL=1
            RETURN
       ELSEIF(A.LE.0.OR.Z.LE.0.OR.RHO.LE.0)THEN
            PRINT *,' !!!!!! SRMMST WARNING : Gas parameters not'//
     -           ' valid: A = ',A,', Z = ',Z,' density = ',RHO,' g/cm3.'
            IFAIL=1
            RETURN
       ENDIF
*** Basic kinematic parameters
       IF(EKIN.GT.1E-5*TRMASS)THEN
            BETA2 = 1-1/(1+EKIN/TRMASS)**2
       ELSE
            BETA2 = 2*EKIN/TRMASS
       ENDIF
       GAMMA = 1+EKIN/TRMASS
*** Compute maximum energy transfer
       EMAX = 2*EMASS*BETA2*GAMMA**2/
     -      (1+2*GAMMA*EMASS/TRMASS+(EMASS/TRMASS)**2)
*** Compute the Rutherford term
       XI = FCONST * TRCHAR**2 * Z * RHO * STEP / (A*BETA2)
*** Compute the scaling parameter
       RKAPPA = XI/EMAX
*** Step size and energy loss
       DENOW=DE
       STPNOW=STEP
*** Debugging output.
10     CONTINUE
       RETRY = .FALSE.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMMST DEBUG   : Settings''/
     -      26X,''dE = '',E10.3,'' MeV, step = '',E10.3,'' cm,''/
     -      26X,''Ekin = '',E10.3,'' MeV, beta2 = '',E10.3,
     -          '', gamma = '',F10.3,'',''/
     -      26X,''Agas = '',F10.3,'', Zgas = '',F10.3,
     -          '', rho = '',E10.3,'' g/cm3,''/
     -      26X,''Qpart = '',F10.3,'', mpart = '',F10.3,'' MeV,''/
     -      26X,''Emax = '',E10.3,'' MeV, xi = '',E10.3,'' MeV'',
     -          '', kappa = '',E10.3,''.'')')
     -      DENOW,STPNOW,EKIN,BETA2,GAMMA,A,Z,RHO,TRCHAR,TRMASS,EMAX,
     -      XI,RKAPPA
*** No fluctuations: any step is permitted
       IF(MODEL.LE.0.OR.MODEL.GT.4)THEN
            STPMIN=STPNOW
*   Landau distribution
       ELSEIF(MODEL.EQ.1)THEN
            XLMIN = -3
            IF(-XLMIN-1.0+0.577215-BETA2-DENOW/XI.LT.-EXPMAX)THEN
                 RKLIM=0
            ELSE
                 RKLIM=EXP(-XLMIN-1.0+0.577215-BETA2-DENOW/XI)
            ENDIF
            STPMIN=STPNOW*(RKLIM/RKAPPA)
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Landau distribution'',
     -           '' is imposed.''/26X,''kappa_min = '',E12.5,
     -           '', d_min = '',E12.5,'' cm'')') RKLIM,STPMIN
*   Vavilov distribution, ensure we're in range.
       ELSEIF(MODEL.EQ.2)THEN
            IF(RKAPPA.LT.0.1)THEN
                 XLMIN = -2.7
            ELSEIF(RKAPPA.LT.1)THEN
                 XLMIN = -2.9
            ELSEIF(RKAPPA.LT.2)THEN
                 XLMIN = -3.0
            ELSEIF(RKAPPA.LT.3)THEN
                 XLMIN = -3.1
            ELSEIF(RKAPPA.LT.4)THEN
                 XLMIN = -3.2
            ELSEIF(RKAPPA.LT.5)THEN
                 XLMIN = -3.3
            ELSEIF(RKAPPA.LT.6)THEN
                 XLMIN = -3.4
            ELSEIF(RKAPPA.LT.7)THEN
                 XLMIN = -3.5
            ELSEIF(RKAPPA.LT.8)THEN
                 XLMIN = -3.6
            ELSE
                 XLMIN = -3.7
            ENDIF
            IF(-XLMIN-1.0+0.577215-BETA2-DENOW/XI.LT.-EXPMAX)THEN
                 RKLIM=0
            ELSE
                 RKLIM=EXP(-XLMIN-1.0+0.577215-BETA2-DENOW/XI)
            ENDIF
            STPMIN=STPNOW*(RKLIM/RKAPPA)
            XINEW=FCONST * TRCHAR**2 * Z * RHO * STPMIN / (A*BETA2)
            RKNEW=XINEW/EMAX
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Vavilov distribution'',
     -           '' is imposed.''/26X,''kappa_min = '',E12.5,
     -           '', d_min = '',E12.5,'' cm''/26X,''kappa_new = '',
     -           E12.5,'', xi_new = '',E12.5,'' MeV'')')
     -           RKLIM,STPMIN,RKNEW,XINEW
            IF(STPMIN.GT.STPNOW*1.1)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''New pass needed'',
     -                '' because of step size increase.'')')
                 RETRY=.TRUE.
            ENDIF
*   Gaussian model
       ELSEIF(MODEL.EQ.3)THEN
            STPMIN=STPNOW*16*XI*EMAX*(1-BETA2/2)/DENOW**2
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Gaussian distribution'',
     -           '' is imposed.''/26X,''d_min = '',E12.5,'' cm.''/26X,
     -           ''sigma/mu_old = '',E12.5,'', sigma/mu_min = '',
     -           E12.5)') STPMIN,SQRT(XI*EMAX*(1-BETA2/2))/DE,
     -           SQRT((FCONST * TRCHAR**2 *
     -           Z * RHO * STPMIN / (A*BETA2))*EMAX*(1-BETA2/2))/
     -           (STPMIN*DENOW/STPNOW)
*   Combined model: for low kappa, use the Landau distribution.
       ELSEIF(RKAPPA.LT.0.05)THEN
            XLMIN = -3
            IF(-XLMIN-1.0+0.577215-BETA2-DENOW/XI.LT.-EXPMAX)THEN
                 RKLIM=0
            ELSE
                 RKLIM=EXP(-XLMIN-1.0+0.577215-BETA2-DENOW/XI)
            ENDIF
            STPMIN=STPNOW*(RKLIM/RKAPPA)
            XINEW=FCONST * TRCHAR**2 * Z * RHO * STPMIN / (A*BETA2)
            RKNEW=XINEW/EMAX
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Landau distribution'',
     -           '' automatic.''/26X,''kappa_min = '',E12.5,
     -           '', d_min = '',E12.5,'' cm'')') RKLIM,STPMIN
            IF(RKNEW.GT.0.05.OR.STPMIN.GT.STPNOW*1.1)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''New pass needed'',
     -                '' because of model change or'',
     -                '' step size increase.'')')
                 RETRY=.TRUE.
            ENDIF
*   For medium kappa, use the Vavilov distribution
       ELSEIF(RKAPPA.LT.5)THEN
            IF(RKAPPA.LT.0.1)THEN
                 XLMIN = -2.7
            ELSEIF(RKAPPA.LT.1)THEN
                 XLMIN = -2.9
            ELSEIF(RKAPPA.LT.2)THEN
                 XLMIN = -3.0
            ELSEIF(RKAPPA.LT.3)THEN
                 XLMIN = -3.1
            ELSEIF(RKAPPA.LT.4)THEN
                 XLMIN = -3.2
            ELSEIF(RKAPPA.LT.5)THEN
                 XLMIN = -3.3
            ELSEIF(RKAPPA.LT.6)THEN
                 XLMIN = -3.4
            ELSEIF(RKAPPA.LT.7)THEN
                 XLMIN = -3.5
            ELSEIF(RKAPPA.LT.8)THEN
                 XLMIN = -3.6
            ELSE
                 XLMIN = -3.7
            ENDIF
            IF(-XLMIN-1.0+0.577215-BETA2-DENOW/XI.LT.-EXPMAX)THEN
                 RKLIM=0
            ELSE
                 RKLIM=EXP(-XLMIN-1.0+0.577215-BETA2-DENOW/XI)
            ENDIF
            STPMIN=STPNOW*(RKLIM/RKAPPA)
            XINEW=FCONST * TRCHAR**2 * Z * RHO * STPMIN / (A*BETA2)
            RKNEW=XINEW/EMAX
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Vavilov distribution'',
     -           '' automatic.''/26X,''kappa_min = '',E12.5,
     -           '', d_min = '',E12.5,'' cm''/26X,''kappa_new = '',
     -           E12.5,'', xi_new = '',E12.5,'' MeV'')')
     -           RKLIM,STPMIN,RKNEW,XINEW
            IF(RKNEW.GT.5.OR.STPMIN.GT.STPNOW*1.1)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''New pass needed'',
     -                '' because of model change or'',
     -                '' step size increase.'')')
                 RETRY=.TRUE.
            ENDIF
*   And for large kappa, use the Gaussian values.
       ELSE
            STPMIN=STPNOW*16*XI*EMAX*(1-BETA2/2)/DENOW**2
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Gaussian distribution'',
     -           '' automatic.''/26X,''d_min = '',E12.5,'' cm.''/26X,
     -           ''sigma/mu_old = '',E12.5,'', sigma/mu_min = '',
     -           E12.5)') STPMIN,SQRT(XI*EMAX*(1-BETA2/2))/DE,
     -           SQRT((FCONST * TRCHAR**2 *
     -           Z * RHO * STPMIN / (A*BETA2))*EMAX*(1-BETA2/2))/
     -           (STPMIN*DENOW/STPNOW)
       ENDIF
*** See whether we should do another pass
       IF(STPNOW.LT.STPMIN)THEN
            IF(RETRY.AND.ITER.LT.10)THEN
                 ITER=ITER+1
                 RKAPPA=RKNEW
                 XI=XINEW
                 DENOW=DENOW*STPMIN/STPNOW
                 STPNOW=STPMIN
                 GOTO 10
            ELSEIF(RETRY)THEN
                 PRINT *,' !!!!!! SRMMST WARNING : No convergence'//
     -                ' reached on step size.'
                 NTRERR(7)=NTRERR(7)+1
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Step size must be'',
     -                '' increased to '',E12.5,'' cm'')') STPMIN
            ENDIF
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Step size OK, minimum: '',
     -           E12.5,'' cm'')') STPMIN
       ENDIF
       END
