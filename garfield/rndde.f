CDECK  ID>, RNDDE.
       REAL FUNCTION RNDDE(EKIN,DE,STEP,MODEL)
*-----------------------------------------------------------------------
*   RNDDE  - Generates a random energy loss.
*   VARIABLES : EKIN       : Kinetic energy [MeV]
*               DE         : Mean energy loss over the step [MeV]
*               STEP       : Step length [cm]
*               BETA2      : Velocity-squared
*               GAMMA      : Projectile gamma
*               EMAX       : Maximum energy transfer per collision [MeV]
*               XI         : Rutherford term [MeV]
*               FCONST     : Proportionality constant
*               EMASS      : Electron mass [MeV]
*   (Last changed on 26/10/07.)
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
       REAL EKIN,DE,STEP,
     -      FCONST,EMASS,GAMMA,BETA2,EMAX,
     -      XI,RKAPPA,
     -      XLAN,XLMEAN,XLMAX,
     -      RANLAN,RNDUNI,RNDVAV,RNDVVL,RNDNOR,PAR(7)
       INTEGER MODEL
       EXTERNAL RANLAN,RNDUNI,RNDVAV,RNDVVL,RNDNOR
*** Fixed values.
       PARAMETER(FCONST=0.1534, EMASS=0.510999)
*   Initial values
       DATA PAR /0.50884, 1.26116, 0.0346688, 1.46314, 0.15088E-2,
     -       1.00324, -0.13049E-3/
*** Check correctness.
       IF(EKIN.LE.0.OR.DE.LE.0.OR.STEP.LE.0)THEN
            PRINT *,' !!!!!! RNDDE  WARNING : Input parameters not'//
     -           ' valid: Ekin = ',EKIN,' MeV, dE = ',DE,
     -           ' MeV, step length = ',STEP,' cm.'
            RNDDE=0
            RETURN
       ELSEIF(TRMASS.LE.0.OR.ABS(TRCHAR).LE.0)THEN
            PRINT *,' !!!!!! RNDDE  WARNING : Track parameters not'//
     -           ' valid: mass = ',TRMASS,' MeV, charge = ',TRCHAR
            RNDDE=0
            RETURN
       ELSEIF(A.LE.0.OR.Z.LE.0.OR.RHO.LE.0)THEN
            PRINT *,' !!!!!! RNDDE  WARNING : Gas parameters not'//
     -           ' valid: A = ',A,', Z = ',Z,' density = ',RHO,' g/cm3.'
            RNDDE=0
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
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ RNDDE  DEBUG   : Settings''/
     -      26X,''dE = '',E10.3,'' MeV, step = '',E10.3,'' cm,''/
     -      26X,''Ekin = '',E10.3,'' MeV, beta2 = '',E10.3,
     -          '', gamma = '',F10.3,'',''/
     -      26X,''Agas = '',F10.3,'', Zgas = '',F10.3,
     -          '', rho = '',E10.3,'' g/cm3,''/
     -      26X,''Qpart = '',F10.3,'', mpart = '',F10.3,'' MeV,''/
     -      26X,''Emax = '',E10.3,'' MeV, xi = '',E10.3,'' MeV'',
     -          '', kappa = '',E10.3,''.'')')
     -      DE,STEP,EKIN,BETA2,GAMMA,A,Z,RHO,TRCHAR,TRMASS,EMAX,
     -      XI,RKAPPA
*** No fluctuations.
       IF(MODEL.LE.0.OR.MODEL.GT.4)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Fixed energy loss.'')')
            RNDDE=DE
*   Landau distribution
       ELSEIF(MODEL.EQ.1)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Landau imposed.'')')
            XLMEAN=-(LOG(XI/EMAX)+BETA2+(1-0.577215))
            XLAN=RANLAN(RNDUNI(1.0))
            RNDDE=DE+XI*(XLAN-XLMEAN)
*   Vavilov distribution, ensure we're in range.
       ELSEIF(MODEL.EQ.2)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Vavilov imposed.'')')
            IF(LTRVVL.AND.RKAPPA.GT.0.01.AND.RKAPPA.LE.10)THEN
                 RNDDE=DE+XI*(RNDVVL(RKAPPA,BETA2)+
     -                LOG(XI/EMAX)+BETA2+(1-0.577215))
            ELSEIF((.NOT.LTRVVL).AND.
     -           RKAPPA.GT.0.01.AND.RKAPPA.LE.12)THEN
                 RNDDE=DE+XI*(RNDVAV(RKAPPA,BETA2)+
     -                LOG(XI/EMAX)+BETA2+(1-0.577215))
            ELSE
                 NTRERR(2)=NTRERR(2)+1
                 RNDDE=DE
            ENDIF
*   Gaussian model
       ELSEIF(MODEL.EQ.3)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Gaussian imposed.'')')
            RNDDE=RNDNOR(DE,SQRT(XI*EMAX*(1-BETA2/2)))
*   Combined model: for low kappa, use the Landau distribution.
       ELSEIF(RKAPPA.LT.0.05)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Landau automatic.'')')
            XLMEAN=-(LOG(XI/EMAX)+BETA2+(1-0.577215))
            XLMAX=(PAR(1)+PAR(2)*XLMEAN+PAR(3)*XLMEAN**2+
     -           PAR(7)*XLMEAN**3)+
     -           (PAR(4)+XLMEAN*PAR(5))*EXP(PAR(6)*XLMEAN)
10          CONTINUE
            XLAN=RANLAN(RNDUNI(1.0))
            IF(XLAN.GT.XLMAX)GOTO 10
            RNDDE=DE+XI*(XLAN-XLMEAN)
*   For medium kappa, use the Vavilov distribution, precise
       ELSEIF(LTRVVL.AND.RKAPPA.LT.5)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Vavilov slow automatic.'')')
            RNDDE=DE+XI*(RNDVVL(RKAPPA,BETA2)+
     -           LOG(XI/EMAX)+BETA2+(1-0.577215))
*   ... or fast.
       ELSEIF(RKAPPA.LT.5)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Vavilov fast automatic.'')')
            RNDDE=DE+XI*(RNDVAV(RKAPPA,BETA2)+
     -           LOG(XI/EMAX)+BETA2+(1-0.577215))
*   And for large kappa, use the Gaussian values.
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Gaussian automatic.'')')
            RNDDE=RNDNOR(DE,SQRT(XI*EMAX*(1-BETA2/2)))
       ENDIF
*** Debugging output
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ RNDDE  DEBUG   : Energy'',
     -      '' loss generated = '',E12.5,'' MeV.'')') RNDDE
       END
