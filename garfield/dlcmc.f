CDECK  ID>, DLCMC.
       SUBROUTINE DLCMC(X1,Y1,Z1,Q,ITYPE)
*-----------------------------------------------------------------------
*   DLCMC  - Subroutine that computes a drift line using a Monte-Carlo
*            technique to take account of diffusion.
*   VARIABLES :
*   REFERENCE :
*   (Last changed on  4/ 4/10.)
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
       CHARACTER*80 CELLID
       CHARACTER*3 TYPE
       CHARACTER WIRTYP(MXWIRE),PLATYP(5),
     -      PSLAB1(5,MXPSTR),PSLAB2(5,MXPSTR)
       LOGICAL YNPLAN(4),PERX,PERY,PERZ,YNPLAX,YNPLAY,YNMATX,YNMATY,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,
     -      PERRX,PERRY,PERRZ,CNALSO(MXWIRE),LBGFMP,CELSET,LDIPOL,
     -      BEMSET
       INTEGER INDSW(MXWIRE),NWIRE,NSW,ICTYPE,MODE,NTUBE,MTUBE,
     -      NXMATT,NYMATT,N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA(5),NPSTR1(5),NPSTR2(5),
     -      INDST1(5,MXPSTR),INDST2(5,MXPSTR)
       REAL X(MXWIRE),Y(MXWIRE),V(MXWIRE),E(MXWIRE),D(MXWIRE),W(MXWIRE),
     -      U(MXWIRE),DENS(MXWIRE),
     -      COSPH2(MXWIRE),SINPH2(MXWIRE),AMP2(MXWIRE),
     -      COPLAN(4),VTPLAN(4),XMATT(MXMATT,5),YMATT(MXMATT,5),
     -      X3D(MX3D),Y3D(MX3D),Z3D(MX3D),E3D(MX3D),
     -      DOWN(3),PLSTR1(5,MXPSTR,3),PLSTR2(5,MXPSTR,3),
     -      COTUBE,VTTUBE,B2SIN(MXWIRE),P1,P2,C1,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,
     -      KAPPA
       COMPLEX ZMULT,WMAP(MXWIRE)
       COMMON /CELDAT/ ZMULT,WMAP,X,Y,V,E,D,W,U,DENS,
     -      COSPH2,SINPH2,AMP2,
     -      B2SIN,COPLAN,VTPLAN,XMATT,YMATT,X3D,Y3D,Z3D,E3D,DOWN,
     -      PLSTR1,PLSTR2,
     -      XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,VMIN,VMAX,
     -      COPLAX,COPLAY,COMATX,COMATY,COTUBE,VTTUBE,
     -      CORVTA,CORVTB,CORVTC,V0,SX,SY,SZ,P1,P2,C1,KAPPA,
     -      INDSW,NWIRE,NSW,ICTYPE,MODE,NXMATT,NYMATT,NTUBE,MTUBE,
     -      N3D,NTERMB,NTERMP,IENBGF,
     -      INDPLA,NPSTR1,NPSTR2,INDST1,INDST2,
     -      YNPLAN,YNPLAX,YNPLAY,YNMATX,YNMATY,PERX,PERY,PERZ,
     -      POLAR,TUBE,PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,CNALSO,
     -      PERRX,PERRY,PERRZ,LBGFMP,CELSET,LDIPOL,BEMSET
       COMMON /CELCHR/ CELLID,WIRTYP,PLATYP,TYPE,PSLAB1,PSLAB2
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
       DOUBLE PRECISION F0(3),X0,Y0,Z0,THETA,PHI,DIST,X0NEW,Y0NEW,Z0NEW,
     -      TSTEP,DVEC(3)
       REAL Q,X1,Y1,Z1,EX,EY,EZ,ETOT,VOLT,GASDFT,GASDFL,DT,DL,RNDNOR,
     -      RNDEXP,TCOLL,BX,BY,BZ,BTOT
       INTEGER IFLAG,ILOC,ITYPE,IPLANE,IOUT
       EXTERNAL GASDFT,GASDFL,RNDNOR,RNDEXP
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCMC ///'
*** Initial debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMC  DEBUG   : MC drift'',
     -      '' from ('',E15.8,'','',E15.8,'','',E15.8,''), Q='',E8.1,
     -      '' type='',I2,''.'')') X1,Y1,Z1,Q,ITYPE
*** Initialise the output position and time vectors.
       NU=1
       XU(1)=DBLE(X1)
       YU(1)=DBLE(Y1)
       ZU(1)=DBLE(Z1)
       TU(1)=0.0D0
       ISTAT=0
       IPTYPE=ITYPE
       IPTECH=2
       QPCHAR=Q
*** Check the initial position, setting a status code if appropriate.
       CALL EFIELD(X1,Y1,Z1,EX,EY,EZ,ETOT,VOLT,0,ILOC)
       CALL BFIELD(X1,Y1,Z1,BX,BY,BZ,BTOT)
*   In a wire.
       IF(ILOC.GT.0.AND.ILOC.LE.MXWIRE)THEN
            IF((X(ILOC)-X1)**2+(Y(ILOC)-Y1)**2.LE.0.25*D(ILOC)**2)THEN
                 ISTAT=ILOC
            ELSE
                 ISTAT=ILOC+MXWIRE
            ENDIF
*   Outside the planes.
       ELSEIF(ILOC.EQ.-1.OR.ILOC.EQ.-4)THEN
            IF(YNPLAN(1).AND.X1.LE.COPLAN(1))THEN
                 ISTAT=-11
            ELSEIF(YNPLAN(2).AND.X1.GE.COPLAN(2))THEN
                 ISTAT=-12
            ELSEIF(YNPLAN(3).AND.Y1.LE.COPLAN(3))THEN
                 ISTAT=-13
            ELSEIF(YNPLAN(4).AND.Y1.GE.COPLAN(4))THEN
                 ISTAT=-14
            ELSEIF(TUBE)THEN
                 CALL INTUBE(X1,Y1,COTUBE,NTUBE,IOUT)
                 IF(IOUT.EQ.1)THEN
                      ISTAT=-15
                 ELSEIF(IOUT.NE.0)THEN
                      ISTAT=-3
                 ENDIF
            ENDIF
            IF(ISTAT.EQ.0)THEN
                 PRINT *,' !!!!!! DLCMC  WARNING : Field location'//
     -                ' code does not match geometry; please report.'
                 ISTAT=-4
            ENDIF
*   In a material.
       ELSEIF(ILOC.EQ.-5)THEN
            ISTAT=-5
*   In a solid.
       ELSEIF(ILOC.GT.2*MXWIRE.AND.ILOC.LE.2*MXWIRE+MXSOLI)THEN
            ISTAT=ILOC
*   Outside the mesh.
       ELSEIF(ILOC.EQ.-6)THEN
            ISTAT=-6
*   Other bizarre codes.
       ELSEIF(ILOC.NE.0)THEN
            PRINT *,' ###### DLCMC  ERROR   : Unexpected ILOC=',ILOC,
     -           ' received from EFIELD ; program bug, please report.'
            ISTAT=-3
       ENDIF
*   Always return if location code is non-zero.
       IF(ILOC.NE.0)RETURN
*** Check the initial status, establishing eg the target wire.
       CALL DLCSTA(Q,ITYPE)
       IF(ISTAT.NE.0)RETURN
*** Store the initial point locally in scalar double precision.
       X0=DBLE(X1)
       Y0=DBLE(Y1)
       Z0=DBLE(Z1)
*** Start making steps.
30     CONTINUE
*   Compute drift velocity.
       CALL DLCVEL(X0,Y0,Z0,F0,Q,ITYPE,ILOC)
*   Ensure the norm is not zero.
       IF(SQRT(F0(1)**2+F0(2)**2+F0(3)**2).LE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCMC   DEBUG   : Velocity'//
     -           ' is zero at NU=',NU,'; returning with ISTAT=-3.'
            ISTAT=-3
            RETURN
       ENDIF
*   Compute the diffusion terms.
       IF(ITYPE.EQ.1)THEN
            IF(GASOK(8))THEN
                 DT=GASDFT(EX,EY,EZ,BX,BY,BZ)
            ELSE
                 DT=0
            ENDIF
            IF(GASOK(3))THEN
                 DL=GASDFL(EX,EY,EZ,BX,BY,BZ)
            ELSE
                 DL=0
            ENDIF
       ELSE
            IF(DTION.LT.0)THEN
                 DT=SQRT((2.0*BOLTZ*TGAS)/(ECHARG*ETOT))
            ELSE
                 DT=DTION
            ENDIF
            IF(DLION.LT.0)THEN
                 DL=SQRT((2.0*BOLTZ*TGAS)/(ECHARG*ETOT))
            ELSE
                 DL=DLION
            ENDIF
       ENDIF
*   If making fixed size time steps ...
       IF(MCMETH.EQ.0)THEN
            TSTEP=TMC
*   If making fixed distance steps ...
       ELSEIF(MCMETH.EQ.1)THEN
            TSTEP=DMC/SQRT(F0(1)**2+F0(2)**2+F0(3)**2)
*   If making steps based on collision time ...
       ELSE
            TCOLL=1E8*EMASS*SQRT(F0(1)**2+F0(2)**2+F0(3)**2)/
     -           (ECHARG*SQRT(EX**2+EY**2+EZ**2))
C       PRINT *,' Collision time=',TCOLL*1000000,' psec'
            TSTEP=NMC*RNDEXP(TCOLL)
       ENDIF
*   Draw a random diffusion direction in the particle frame.
       DVEC(1)=SQRT(SQRT(F0(1)**2+F0(2)**2+F0(3)**2)*TSTEP)*
     -      RNDNOR(0.0,DL)
       DVEC(2)=SQRT(SQRT(F0(1)**2+F0(2)**2+F0(3)**2)*TSTEP)*
     -      RNDNOR(0.0,DT)
       DVEC(3)=SQRT(SQRT(F0(1)**2+F0(2)**2+F0(3)**2)*TSTEP)*
     -      RNDNOR(0.0,DT)
*   Compute rotation to align (1,0,0) with the drift velocity vector.
       IF(F0(1)**2+F0(2)**2.LE.0)THEN
            IF(F0(3).LT.0)THEN
                 THETA=-PI/2
            ELSEIF(F0(3).GT.0)THEN
                 THETA=+PI/2
            ELSE
                 IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : Velocity',
     -                ' vector of length = 0 seen; returning ISTAT=-3.'
                 ISTAT=-3
                 RETURN
            ENDIF
            PHI=0
       ELSE
            PHI=ATAN2(F0(2),F0(1))
            THETA=ATAN2(F0(3),SQRT(F0(1)**2+F0(2)**2))
       ENDIF
*   Compute the proposed end-point of this step.
       X0NEW=X0+TSTEP*F0(1)+COS(PHI)*COS(THETA)*DVEC(1)-
     -      SIN(PHI)*DVEC(2)-COS(PHI)*SIN(THETA)*DVEC(3)
       Y0NEW=Y0+TSTEP*F0(2)+SIN(PHI)*COS(THETA)*DVEC(1)+
     -      COS(PHI)*DVEC(2)-SIN(PHI)*SIN(THETA)*DVEC(3)
       Z0NEW=Z0+TSTEP*F0(3)+SIN(THETA)*DVEC(1)+COS(THETA)*DVEC(3)
*** Check that the target wire is not crossed while exploring the field.
       IF(ITARG.GT.0)THEN
            CALL DLCMIN(XTARG,YTARG,X0,Y0,X0NEW,Y0NEW,DIST,IFLAG)
*   If it is, quit at this point after terminating via DLCWIR.
            IF(DIST.LT.0.25*DTARG**2)THEN
                 IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : DLCWIR',
     -                ' entered from DLCMC.'
                 CALL DLCWIR(1,Q,ITYPE)
                 RETURN
            ENDIF
       ENDIF
*** Check that none of the planes was crossed during this computation.
       IF(YNPLAN(1).OR.YNPLAN(2).OR.YNPLAN(3).OR.YNPLAN(4))THEN
            IPLANE=0
            IF(YNPLAN(1).AND.X0NEW.LE.COPLAN(1))IPLANE=1
            IF(YNPLAN(2).AND.X0NEW.GE.COPLAN(2))IPLANE=2
            IF(YNPLAN(3).AND.Y0NEW.LE.COPLAN(3))IPLANE=3
            IF(YNPLAN(4).AND.Y0NEW.GE.COPLAN(4))IPLANE=4
            IF(IPLANE.NE.0)THEN
                 IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : Plane ',
     -                IPLANE,' was crossed during the last step.'
                 CALL DLCPLM(X0NEW,Y0NEW,Z0NEW,TU(NU)+TSTEP,IPLANE)
                 RETURN
            ENDIF
       ENDIF
*** Check that the tube was not left.
       IF(TUBE)THEN
            CALL INTUBE(REAL(X0NEW),REAL(Y0NEW),COTUBE,NTUBE,IOUT)
            IF(IOUT.NE.0)THEN
                 IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : The tube'//
     -                ' was left during the last step.'
                 CALL DLCTUB(Q,ITYPE)
                 RETURN
            ENDIF
       ENDIF
*** Compute the electric field for the next step.
       CALL EFIELD(REAL(X0NEW),REAL(Y0NEW),REAL(Z0NEW),EX,EY,EZ,ETOT,
     -      VOLT,0,ILOC)
       CALL BFIELD(REAL(X0NEW),REAL(Y0NEW),REAL(Z0NEW),BX,BY,BZ,BTOT)
*** Check that no dielectric was entered nor that the mesh was left.
       IF(ICTYPE.EQ.0.AND.ILOC.NE.0)THEN
            CALL DLCFMP(X0,Y0,Z0,X0NEW,Y0NEW,Z0NEW,TSTEP,ILOC,Q,ITYPE)
            IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : Drift',
     -           ' medium or mesh left at NU=',NU,' ILOC=',ILOC
            RETURN
       ELSEIF(ICTYPE.EQ.-1.AND.ILOC.NE.0)THEN
            CALL DLCSOL(X0,Y0,Z0,X0NEW,Y0NEW,Z0NEW,TSTEP,ILOC,Q,ITYPE)
            IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : Solid',
     -           ' entered at NU=',NU,' ILOC=',ILOC
            RETURN
       ENDIF
*** Redefine X0, Y0 and Z0.
       X0=X0NEW
       Y0=Y0NEW
       Z0=Z0NEW
*** Copy new X0 and Y0 to XU and YU, add new TU.
       NU=NU+1
       XU(NU)=X0
       YU(NU)=Y0
       ZU(NU)=Z0
       TU(NU)=TU(NU-1)+TSTEP
*** Check particle position.
       CALL DLCSTA(Q,ITYPE)
       IF(ISTAT.NE.0)RETURN
*** Make sure all exceptions have been caught.
       IF(ILOC.NE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : Received ILOC=',
     -           ILOC,' from EFIELD, NU=',NU,'; returning ISTAT=-3.'
            ISTAT=-3
            RETURN
*   Make sure the field is not zero.
       ELSEIF(SQRT(EX**2+EY**2+EZ**2).LE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DLCMC  DEBUG   : Electric field',
     -           ' zero at NU=',NU,'; returning with ISTAT=-3.'
            ISTAT=-3
            RETURN
*   Make sure we haven't got more than MXLIST points already.
       ELSEIF(NU.EQ.MXLIST)THEN
            ISTAT=-2
            RETURN
       ENDIF
*** And go for the next step.
       GOTO 30
       END
