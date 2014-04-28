CDECK  ID>, DRFXTP.
       SUBROUTINE DRFXTP
*-----------------------------------------------------------------------
*   DRFXTP - Routine plotting and printing x(t) correlation plots for
*            all selected wires in the drift area.
*   VARIABLES : STRING      : (Part of) the command; the header record.
*               INFILE      : Used for producing comment strings.
*               LXTWRT      : TRUE if the x(t) data are to be written.
*               FILE        : Name of the x(t) data set, length NCFILE.
*               MEMBER      : Member name, length NCMEMB.
*               REMARK      : Remark field of the header, length NCREM.
*               DATE, TIME  : Clock date and time (header record).
*               XSTEP       : Sampling step size.
*               ANGLE       : x(t) angle with the y-axis.
*               JUMP        : Number of points to be interpolated.
*               ITERMX      : Max number of minimisation loops.
*               A.L/R.MIN/MAX : Search angle limits.
*               P, Q, R(REF): Lines are represented as Px+Qy=R.
*               PRECIS      : Tells whether an XT entry is enterpolated.
*               IXM, IXP    : Sampling is done at X(I)+I*XSTEP I=IXM,IXP
*               XT( . , )   : 1-3: time, 4-6: C coordinate, 7: diffusion
*               TPARA, CPARA: Minimum of the fitted parabola.
*   (Last changed on 26/ 1/03.)
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
       REAL XT(7,MXLIST),XPL(MXLIST),YPL(MXLIST),ZPL(MXLIST),
     -      ANG,QCHARG,P,Q,REF,
     -      GRSMIN,GRSMAX,GMINR,GMAXR,XSTEP,XSTEPR,XXSTEP,XTXMIN,XTXMAX,
     -      ANGLE,ANGLER,EPS,EPSR,CXTMIN,CXTMAX,RDIST,CPARA,TPARA,
     -      CDRIFT,TDRIFT,ALMIN,ALMAX,ALMINR,ALMAXR,ARMIN,ARMAX,
     -      ARMINR,ARMAXR,TMIN,TMAX,XTAUX,XTXMIR,XTXMAR,CSTEP
       INTEGER NXT(MXLIST),IXTFLG(MXLIST),NCFILE,NCMEMB,NCREM,KX,NWORD,
     -      I,J,INEXT,ITERMX,ITERMR,JUMPR,JUMP,IFAIL,IFAIL1,IFAIL2,
     -      IFAIL3,NDLC,
     -      IXM,IXP,IIX,IX,JJX,JX,ISET,JSET,IANG,ITERSK,IMIN,ITAB,JTAB,
     -      IOS,NPLOT,ITER,IFLAG,IIMIN,II,INPCMP,INXT,JOVER,ISIZ(1),
     -      IDIM(1),NC1,NC2
       LOGICAL XTSET(MXLIST),PRECIS(3),FLAG(MXWORD+3),LXTWRT,PRAUX,
     -      LXTPRT,LXTPLT,EXMEMB,LXTKP
       CHARACTER*132 OUTSTR
       CHARACTER*80 STRING
       CHARACTER*30 INFILE
       CHARACTER*29 REMARK
       CHARACTER*20 STR1,STR2
       CHARACTER*(MXNAME) FILE
       CHARACTER*8 DATE,TIME,MEMBER
       EXTERNAL INPCMP
       SAVE ANGLE,JUMP,ITERMX,EPS,ALMIN,ALMAX,ARMIN,ARMAX,JOVER
*** Initialise the parameters to be remembered via DATA statements.
       DATA ANGLE /0.0/
       DATA JUMP,ITERMX /1,5/
       DATA EPS /1.0E-3/
       DATA ALMIN,ALMAX,ARMIN,ARMAX /-90.0,90.0,-90.0,90.0/
       DATA LXTPRT,LXTPLT,LXTKP /.TRUE.,.TRUE.,.FALSE./
       DATA JOVER /0/
*** Define some formats.
1080   FORMAT('Angle to y =',F8.2,' degrees  ')
1090   FORMAT('Wire no    =',I3,'    (type ',A1,')   ')
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE DRFXTP ///'
*** Preset some of the arguments.
       FILE=' '
       MEMBER='< none >'
       REMARK='None'
       NCFILE=1
       NCMEMB=8
       NCREM=4
       LXTWRT=.FALSE.
       GRSMIN=+1.0
       GRSMAX=-1.0
*   And obtain a rounded default value for the x-step.
       XSTEP=(DXMAX-DXMIN)/20.0
       KX=NINT(LOG10(XSTEP))
       IF(KX.GE.0)THEN
            XSTEP=XSTEP/10.0**KX
       ELSE
            XSTEP=XSTEP*10.0**(-KX)
       ENDIF
       IF(XSTEP.GE.0.1.AND.XSTEP.LT.0.2)THEN
            XXSTEP=0.1
       ELSEIF(XSTEP.GE.0.2.AND.XSTEP.LT.0.5)THEN
            XXSTEP=0.2
       ELSEIF(XSTEP.GE.0.5.AND.XSTEP.LT.1.0)THEN
            XXSTEP=0.5
       ELSEIF(XSTEP.GE.1.0.AND.XSTEP.LT.2.0)THEN
            XXSTEP=1.0
       ELSEIF(XSTEP.GE.2.0.AND.XSTEP.LT.5.0)THEN
            XXSTEP=2.0
       ELSEIF(XSTEP.GE.5.0.AND.XSTEP.LT.10.0)THEN
            XXSTEP=5.0
       ELSE
            PRINT *,' !!!!!! DRFXTP WARNING : Unable to find a default',
     -           ' x-step; set to 1.'
            XXSTEP=1.0
       ENDIF
       IF(KX.GE.0)THEN
            XSTEP=XXSTEP*10.0**KX
       ELSE
            XSTEP=XXSTEP/10.0**(-KX)
       ENDIF
*   Finally also set the range in x to the full range.
       XTXMIN=DXMIN
       XTXMAX=DXMAX
*** Extract the parameters from the input.
       CALL INPNUM(NWORD)
*   Initialise the FLAG array.
       DO 10 I=1,MXWORD+3
       IF(I.LE.NWORD)THEN
            FLAG(I)=.FALSE.
       ELSE
            FLAG(I)=.TRUE.
       ENDIF
10     CONTINUE
*   Mark the keywords.
       DO 20 I=1,NWORD
       IF(INPCMP(I,'ANG#LE')+INPCMP(I,'D#ATASET')+INPCMP(I,'J#UMP')+
     -      INPCMP(I,'L#EFT-#ANGLE-#RANGE')+INPCMP(I,'NO#NE')+
     -      INPCMP(I,'IT#ERATIONS')+INPCMP(I,'OFF')+
     -      INPCMP(I,'PREC#ISION')+INPCMP(I,'RAN#GE')+
     -      INPCMP(I,'REM#ARK')+INPCMP(I,'RI#GHT-#ANGLE-#RANGE')+
     -      INPCMP(I,'ST#EP')+INPCMP(I,'SC#ALE')+
     -      INPCMP(I,'PR#INT-#XT-#RELATION')+
     -      INPCMP(I,'NOPR#INT-#XT-#RELATION')+
     -      INPCMP(I,'PL#OT-#XT-#RELATION')+
     -      INPCMP(I,'NOPL#OT-#XT-#RELATION')+
     -      INPCMP(I,'X-ST#EP')+
     -      INPCMP(I,'X-R#ANGE')+
     -      INPCMP(I,'KEEP-#RESULTS')+INPCMP(I,'KEEP-#XT-#RELATION')+
     -      INPCMP(I,'NOKEEP-#RESULTS')+
     -      INPCMP(I,'NOKEEP-#XT-#RELATION').NE.0)FLAG(I)=.TRUE.
20     CONTINUE
**  Next interpret the words.
       INEXT=2
       DO 30 I=2,NWORD
       IF(I.LT.INEXT)GOTO 30
*   Look for the ANGLE at which the tracks are going to be.
       IF(INPCMP(I,'ANG#LE').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'the argument is missing.      ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,ANGLER,0.0)
                 IF(ABS(ANGLER).GT.90.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'Too large an angle (Max = 90).')
                 ELSEIF(IFAIL.EQ.0)THEN
                      ANGLE=ANGLER
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Look for a DATASET (and perhaps a member) receiving the x(t)'s.
       ELSEIF(INPCMP(I,'D#ATASET').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'the dataset name is missing.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCFILE)
                 FILE=STRING
                 IF(.NOT.FLAG(I+2))THEN
                      CALL INPSTR(I+2,I+2,STRING,NCMEMB)
                      MEMBER=STRING
                      INEXT=I+3
                 ELSE
                      INEXT=I+2
                 ENDIF
                 LXTWRT=.TRUE.
            ENDIF
*   Look for the number of ITERATION cycles.
       ELSEIF(INPCMP(I,'IT#ERATIONS').NE.0)THEN
            IF(INPCMP(I+1,'OFF')+INPCMP(I+1,'NO#NE').NE.0)THEN
                 ITERMX=0
                 INEXT=I+2
            ELSEIF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Argument (n or OFF) missing.  ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,ITERMR,5)
                 IF(IFAIL.EQ.0.AND.ITERMR.LT.0)THEN
                      CALL INPMSG(I+1,'Should be a positive integer. ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      ITERMX=ITERMR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Look for the number of intermediate points to be JUMPed.
       ELSEIF(INPCMP(I,'J#UMP').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'the argument is missing.      ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,JUMPR,10)
                 IF(JUMPR.LE.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'should be a positive number.  ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      JUMP=JUMPR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Look for the LEFT-ANGLE-RANGE.
       ELSEIF(INPCMP(I,'L#EFT-#ANGLE-#RANGE').NE.0)THEN
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'The angle-range is incomplete.')
                 IF(.NOT.FLAG(I+1).AND.FLAG(I+2))INEXT=I+2
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,ALMINR,ALMIN)
                 CALL INPRDR(I+2,ALMAXR,ALMAX)
                 IF(ABS(ALMINR).GT.90.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(I,'See the next message.         ')
                      CALL INPMSG(I+1,'Not between -90 and +90 degr. ')
                 ELSEIF(ABS(ALMAXR).GT.90.0.AND.IFAIL2.EQ.0)THEN
                      CALL INPMSG(I,'See the next message.         ')
                      CALL INPMSG(I+2,'Not between -90 and +90 degr. ')
                 ELSEIF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      ALMIN=MIN(ALMINR,ALMAXR)
                      ALMAX=MAX(ALMINR,ALMAXR)
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Look for the PRECISION (convergence parameter).
       ELSEIF(INPCMP(I,'PREC#ISION').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'The argument is missing.      ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,EPSR,EPS)
                 IF(EPSR.LE.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'Should be a positive number.  ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      EPS=EPSR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Look for a REMARK replacing the default remark in the header,
       ELSEIF(INPCMP(I,'REM#ARK').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No remark has been found.     ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCREM)
                 REMARK=STRING(1:NCREM)
                 INEXT=I+2
            ENDIF
*   Look for the RIGHT-ANGLE-RANGE.
       ELSEIF(INPCMP(I,'RI#GHT-#ANGLE-#RANGE').NE.0)THEN
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'The angle-range is incomplete.')
                 IF(.NOT.FLAG(I+1).AND.FLAG(I+2))INEXT=I+2
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,ARMINR,ARMIN)
                 CALL INPRDR(I+2,ARMAXR,ARMAX)
                 IF(ABS(ARMINR).GT.90.0.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(I,'See the next message.         ')
                      CALL INPMSG(I+1,'Not between -90 and +90 degr. ')
                 ELSEIF(ABS(ARMAXR).GT.90.0.AND.IFAIL2.EQ.0)THEN
                      CALL INPMSG(I,'See the next message.         ')
                      CALL INPMSG(I+2,'Not between -90 and +90 degr. ')
                 ELSEIF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      ARMIN=MIN(ARMINR,ARMAXR)
                      ARMAX=MAX(ARMINR,ARMAXR)
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Look for the plotting scale.
       ELSEIF(INPCMP(I,'SC#ALE').NE.0)THEN
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'the arguments are missing.    ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,GMINR,+1.0)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+2,GMAXR,-1.0)
                 IF(GMINR.EQ.GMAXR)THEN
                      CALL INPMSG(I+1,'zero range in the')
                      CALL INPMSG(I+2,'scale not permitted')
                 ELSE
                      GRSMIN=MIN(GMINR,GMAXR)
                      GRSMAX=MAX(GMINR,GMAXR)
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Look for a X-STEP size, if the default is not suitable.
       ELSEIF(INPCMP(I,'X-ST#EP')+INPCMP(I,'ST#EP').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'The argument is missing.      ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,XSTEPR,XSTEP)
                 IF(XSTEPR.LE.0.0.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'Should be a positive number.  ')
                 ELSEIF(XSTEPR.LT.(DXMAX-DXMIN)/MXLIST.AND.
     -                IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'Too small, increase MXLIST.   ')
                 ELSEIF(IFAIL.EQ.0)THEN
                      XSTEP=XSTEPR
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Look for an X-RANGE keyword.
       ELSEIF(INPCMP(I,'X-R#ANGE')+INPCMP(I,'RAN#GE').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
            ELSEIF(FLAG(I+2))THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
                 CALL INPMSG(I+1,'Argument can not be used.     ')
                 INEXT=I+2
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,XTXMIR,DXMIN)
                 CALL INPRDR(I+2,XTXMAR,DXMAX)
                 IF(XTXMIR.EQ.XTXMAR)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.     ')
                      CALL INPMSG(I+2,'See the preceding message.    ')
                 ELSEIF((XTXMIR.LT.DXMIN.AND.XTXMAR.LT.DXMIN).OR.
     -                (XTXMIR.GT.DXMAX.AND.XTXMAR.GT.DXMAX))THEN
                      CALL INPMSG(I+1,'Range falls outside the area. ')
                      CALL INPMSG(I+2,'See the preceding message.    ')
                 ELSE
                      XTXMIN=MAX(MIN(XTXMIR,XTXMAR),DXMIN)
                      XTXMAX=MIN(MAX(XTXMIR,XTXMAR),DXMAX)
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Printing options.
       ELSEIF(INPCMP(I,'PR#INT-#XT-#RELATION').NE.0)THEN
            LXTPRT=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-#XT-#RELATION').NE.0)THEN
            LXTPRT=.FALSE.
*   Plotting options.
       ELSEIF(INPCMP(I,'PL#OT-#XT-#RELATION').NE.0)THEN
            LXTPLT=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-#XT-#RELATION').NE.0)THEN
            LXTPLT=.FALSE.
*  Keep results.
       ELSEIF(INPCMP(I,'KEEP-#RESULTS')+
     -      INPCMP(I,'KEEP-#XT-#RELATION').NE.0)THEN
            LXTKP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-#RESULTS')+
     -      INPCMP(I,'NOKEEP-#XT-#RELATION').NE.0)THEN
            LXTKP=.FALSE.
*   OFF and NONE keywords out of sequence.
       ELSEIF(INPCMP(I,'OFF')+INPCMP(I,'NO#NE').NE.0)THEN
            CALL INPMSG(I,'Valid keyword out of sequence.')
*   Unknown keywords.
       ELSE
            CALL INPMSG(I,'The option is not known.      ')
       ENDIF
30     CONTINUE
*   Print error messages.
       CALL INPERR
*   And check the length of the various identifiers.
       IF(NCFILE.GT.MXNAME)PRINT *,' !!!!!! DRFXTP WARNING : The file'//
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       IF(NCMEMB.GT.8)PRINT *,' !!!!!! DRFXTP WARNING : The member'//
     -      ' name is shortened to '//MEMBER//', first 8 characters.'
       IF(NCREM.GT.29)PRINT *,' !!!!!! DRFXTP WARNING : The remark'//
     -      ' shortened to '//REMARK//', first 29 characters.'
       NCFILE=MIN(NCFILE,MXNAME)
       NCMEMB=MIN(NCMEMB,8)
       NCREM=MIN(NCREM,29)
*   Check whether the member already exists.
       IF(LXTWRT)THEN
            CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'XTPLOT',EXMEMB)
            IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
                 PRINT *,' ------ DRFXTP MESSAGE : A copy of the'//
     -                ' member exists; new member will be appended.'
            ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
                 PRINT *,' !!!!!! DRFXTP WARNING : A copy of the'//
     -                ' member exists already; member will not be'//
     -                ' written.'
                 LXTWRT=.FALSE.
            ENDIF
       ENDIF
*   Define the line parameters P and Q such that P*X + Q*Y = R.
       P=COS(-PI*ANGLE/180.0)
       Q=SIN(-PI*ANGLE/180.0)
*   Set the drift-line counter to 0 initially.
       NDLC=0
*** Print some preliminary debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFXTP DEBUG   : Start of'',
     -      '' debugging output''//26X,''Current driftline parameters''/
     -      26X,''RTRAP ='',F10.3,'' EPSDIF= '',E10.3,'' NLINED= '',I10/
     -      26X,''AREA  = ('',E10.3,'','',E10.3,'') ('',E10.3,'','',
     -      E10.3,'')''/)') RTRAP,EPSDIF,NLINED,DXMIN,DYMIN,DXMAX,DYMAX
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Current x(t) specific settings''/
     -      26X,''ANGLE = '',F10.3,'' P = '',E10.3,'' Q = '',E10.3/
     -      26X,''XSTEP = '',E10.3,'' ITERMX = '',I6,'' JUMP = '',I6/
     -      26X,''XTXMIN = '',E10.3,'' XTXMAX = '',E10.3)')
     -      ANGLE,P,Q,XSTEP,ITERMX,JUMP,XTXMIN,XTXMAX
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Angles left: '',2E10.3,/
     -      26X,''Angles right: '',2E10.3,/,
     -      26X,''EPS = '',E10.3)') ALMIN,ALMAX,ARMIN,ARMAX,EPS
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''LXTWRT='',L1,'', FILE='',A,'','',
     -      /26X,''MEMBER='',A,'', REMARK='',A)')
     -      LXTWRT,FILE(1:NCFILE),MEMBER(1:NCMEMB),REMARK(1:NCREM)
*** Pick the wires located inside the drift area.
       DO 100 I=1,NWIRE
       IF(X(I).LT.DXMIN.OR.X(I).GT.DXMAX.OR.
     -      Y(I).LT.DYMIN.OR.Y(I).GT.DYMAX.OR.INDSW(I).EQ.0)GOTO 100
       IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Wire '',I3,'' (type '',A1,
     -      '') at '',2F10.3,'' selected.'')') I,WIRTYP(I),X(I),Y(I)
*   Set the IXM and IXP parameters to ensure that the area is not left.
       IXM=JUMP*ANINT((XTXMIN-X(I))/(JUMP*XSTEP))
       IXP=JUMP*ANINT((XTXMAX-X(I))/(JUMP*XSTEP))
       IF(X(I)+IXM*XSTEP.LT.XTXMIN)IXM=IXM+JUMP
       IF(X(I)+IXP*XSTEP.GT.XTXMAX)IXP=IXP-JUMP
*   Check we remain in the storage allocated for the list.
       IF(IXP-IXM+1.GT.MXLIST)THEN
            PRINT *,' !!!!!! DRFXTP WARNING : No x(t) for wire ',I,
     -           ' because MXLIST is too small.'
            PRINT *,'                         Consider making X-STEP'//
     -           ' larger or choose a smaller AREA.'
            GOTO 100
       ENDIF
*   Draw a set of axis if LDRPLT is on.
       IF(LDRPLT)THEN
            CALL GRAXIS(DXMIN,DYMIN,DXMAX,DYMAX,
     -            'DRIFT LINES USED FOR THE X(T) PLOT      ')
            CALL CELLAY(DXMIN,DYMIN,DXMAX,DYMAX)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRATTS('E-DRIFT-LINE','POLYLINE')
       ENDIF
*** Initialise the arrays for this wire.
       DO 110 ISET=1,MXLIST
       DO 105 JSET=1,7
       XT(JSET,ISET)=0.0
105    CONTINUE
       NXT(ISET)=0
       XTSET(ISET)=.FALSE.
       IXTFLG(ISET)=-11
110    CONTINUE
*** Initialise the maximum and minimum of the C range.
       CXTMIN=Q*X(I)-P*Y(I)
       CXTMAX=Q*X(I)-P*Y(I)
*** Loop around the wire to get a rough picture of the x(t) relation.
       QCHARG=1.0
       DO 120 IANG=-NLINED/2,NLINED/2
*   Translate into an angle taking the limits into account.
       IF(IANG.LT.0)THEN
            IF(NLINED.GT.2)THEN
                 ANG=180.0+ALMIN+REAL(-IANG-1)*(ALMAX-ALMIN)/
     -                REAL(NLINED/2-1)
            ELSE
                 ANG=180.0+0.5*(ALMIN+ALMAX)
            ENDIF
       ELSEIF(IANG.GT.0)THEN
            IF(NLINED.GT.2)THEN
                 ANG=ARMIN+REAL(IANG-1)*(ARMAX-ARMIN)/REAL(NLINED/2-1)
            ELSE
                 ANG=0.5*(ARMIN+ARMAX)
            ENDIF
       ELSE
            GOTO 120
       ENDIF
*   Convert to radians.
       ANG=ANG*PI/180.0
*   Calculate a radial drift-line.
       RDIST=D(I)*(0.5+1.0E-4*(1.0+MAX(ABS(X(I)),ABS(Y(I)))))
       IF(LDEBUG)PRINT *,' ++++++ DRFXTP DEBUG   : RDIST/D=',RDIST/D(I)
       CALL DLCALC(X(I)+RDIST*COS(ANG),Y(I)+RDIST*SIN(ANG),0.0,QCHARG,1)
*   Plot and print data id requested.
       IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
       NDLC=NDLC+1
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Angle='',F10.3,'' ISTAT='',I3,
     -      '' NU='',I3)') 180.0*ANG/PI,ISTAT,NU
*** Loop over the points for which a t is to be found.
       DO 130 IIX=IXM,IXP,JUMP
       IX=IIX-IXM+1
       REF=P*(X(I)+IIX*XSTEP)+Q*Y(I)
*   Find the lowest t intersection for each x.
       CALL DRFXT1(P,Q,REF,CDRIFT,TDRIFT,IFAIL)
       IF(IFAIL.NE.0)GOTO 130
*   Keep track of the extrema of C.
       IF(CXTMIN.GT.CDRIFT)CXTMIN=CDRIFT
       IF(CXTMAX.LT.CDRIFT)CXTMAX=CDRIFT
*   And store it in its proper place in XT(,IX).
       IF(NXT(IX).EQ.0)THEN
            XT(1,IX)=TDRIFT
            XT(4,IX)=CDRIFT
            NXT(IX)=1
       ELSE
            DO 150 ITAB=1,NXT(IX)
            IF(ABS(TDRIFT-XT(ITAB,IX)).LE.1E-4*ABS(TDRIFT).AND.
     -           ABS(CDRIFT-XT(ITAB+3,IX)).LE.1E-4*ABS(CDRIFT))THEN
                 IF(LDEBUG)PRINT *,' ++++++ DRFXTP DEBUG   : Not'//
     -                ' storing this point (coincides).'
                 GOTO 130
            ENDIF
            IF(TDRIFT.LT.XT(ITAB,IX))THEN
                 DO 160 JTAB=3,ITAB+1,-1
                 XT(JTAB,IX)=XT(JTAB-1,IX)
                 XT(JTAB+3,IX)=XT(JTAB+2,IX)
160              CONTINUE
                 XT(ITAB,IX)=TDRIFT
                 XT(ITAB+3,IX)=CDRIFT
                 GOTO 170
            ENDIF
150         CONTINUE
            IF(NXT(IX).LT.3)THEN
                 XT(NXT(IX)+1,IX)=TDRIFT
                 XT(NXT(IX)+4,IX)=CDRIFT
            ENDIF
170         CONTINUE
            NXT(IX)=MIN(3,NXT(IX)+1)
       ENDIF
       XTSET(IX)=.TRUE.
130    CONTINUE
120    CONTINUE
*   Output the C extrema for debugging purposes, if requested.
       IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Extrema for C are: '',2E15.8)')
     -      CXTMIN,CXTMAX
*   And set the C-stepping size.
       CSTEP=ABS(CXTMAX-CXTMIN)/2.0
       IF(CSTEP.LT.ABS(DYMAX-DYMIN)/(100.0*P))THEN
            CSTEP=(DYMAX-DYMIN)/(10.0*P)
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''CSTEP too small, replaced'',
     -           '' by'',E15.8,''.'')') CSTEP
       ENDIF
*** Find more accurate values for all data points.
       QCHARG=-1.0
       DO 200 IIX=IXM,IXP,JUMP
*   IX is a shorthand for the array indices corresponding with IIX.
       IX=IIX-IXM+1
*   Set the track parameter.
       REF=P*(X(I)+IIX*XSTEP)+Q*Y(I)
*   Take correct action in case this point coincides with the wire.
       IF(IIX.EQ.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Point '',I3,'' coincides'',
     -           '' with the wire, no minimisation.'')') IX
            XTSET(IX)=.TRUE.
            XT(1,IX)=0.0
            XT(4,IX)=P*X(I)+Q*Y(I)
            XT(7,IX)=0.0
            IXTFLG(IX)=-1
            GOTO 200
       ENDIF
*   Print the crude infomation we have so far, if debugging is on.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(/26X,''Start of minimisation for point '',I3,
     -           '' XTSET='',L1,'' NXT='',I3,/)') IX,XTSET(IX),NXT(IX)
            DO 205 INXT=1,NXT(IX)
            WRITE(LUNOUT,'(26X,''   c'',I1,''='',E15.8,'', t'',I1,''='',
     -           E15.8)') INXT,XT(INXT+3,IX),INXT,XT(INXT,IX)
205         CONTINUE
            WRITE(*,'('' '')')
       ENDIF
**  Next try to find 3 points forming a parabola. Case 1: no points.
       IF(NXT(IX).EQ.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Crude information absent'',
     -           '' for this data-point, it is skipped.'')')
            IXTFLG(IX)=-2
            GOTO 200
       ENDIF
**  Suppose we already have 3 points, make sure iteration makes sense.
       ITERSK=0
       IF(NXT(IX).EQ.3)THEN
            CALL DRFXT2(XT(4,IX),XT(1,IX),XT(5,IX),XT(2,IX),
     -           XT(6,IX),XT(3,IX),CPARA,TPARA,IFAIL,IFLAG)
            IF(IFLAG.EQ.+1.AND.IFAIL.EQ.0.AND.
     -           ABS(TPARA-XT(1,IX)).LT.EPS*(TPARA+XT(1,IX)).AND.
     -           (CPARA-MAX(XT(4,IX),XT(5,IX),XT(6,IX)))*
     -           (CPARA-MIN(XT(4,IX),XT(5,IX),XT(6,IX))).LE.0)THEN
                 ITERSK=1
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Minimisation is not'',
     -                '' meaningful: TPARA='',E15.8)') TPARA
            ENDIF
       ENDIF
**  If no iteration has been requested, simply recalculate.
       IF(ITERMX.EQ.0.OR.ITERSK.EQ.1)THEN
            CALL DLCALC(P*REF+Q*XT(4,IX),Q*REF-P*XT(4,IX),0.0,QCHARG,1)
            IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
            NDLC=NDLC+1
            IF(ISTAT.EQ.I)THEN
                 XT(1,IX)=TU(NU)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Precise value of t='',
     -                E15.8,''.'')') XT(1,IX)
                 IXTFLG(IX)=0
                 GOTO 310
            ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Recalculation of'',
     -                ''crude drift-line returns ISTAT='',I3)') ISTAT
                 XTSET(IX)=.FALSE.
                 IXTFLG(IX)=-3
                 GOTO 200
            ENDIF
       ENDIF
*   Also initialise the PRECIS list.
       PRECIS(1)=.FALSE.
       PRECIS(2)=.FALSE.
       PRECIS(3)=.FALSE.
**  In case there is a single crossing, add one new point.
       IF(NXT(IX).EQ.1)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''One data point, search'',
     -           '' for one new point towards the C range middle.'')')
            ITER=0
            IF(XT(5,IX).GT.(CXTMAX-CXTMIN)/2.0)THEN
                 XT(5,IX)=XT(4,IX)-CSTEP
            ELSE
                 XT(5,IX)=XT(4,IX)+CSTEP
            ENDIF
210         CONTINUE
            CALL DLCALC(P*REF+Q*XT(5,IX),Q*REF-P*XT(5,IX),0.0,QCHARG,1)
            IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
            NDLC=NDLC+1
            XT(2,IX)=TU(NU)
            PRECIS(2)=.TRUE.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''ITER='',I2,'', C='',E15.8,
     -           '', T='',E15.8,'', ISTAT='',I3)')
     -           ITER,XT(5,IX),XT(2,IX),ISTAT
            IF(ISTAT.NE.I)THEN
                 XT(5,IX)=0.5*(XT(4,IX)+XT(5,IX))
                 ITER=ITER+1
                 IF(ITER.LE.ITERMX)GOTO 210
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''2nd point not found'')')
                 IXTFLG(IX)=-4
                 XTSET(IX)=.FALSE.
                 GOTO 200
            ELSEIF(XT(2,IX).LT.XT(1,IX))THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Second data point'',
     -                '' has a T < T0, data points swapped.'')')
                 XTAUX=XT(1,IX)
                 XT(1,IX)=XT(2,IX)
                 XT(2,IX)=XTAUX
                 XTAUX=XT(4,IX)
                 XT(4,IX)=XT(5,IX)
                 XT(5,IX)=XTAUX
                 PRAUX=PRECIS(1)
                 PRECIS(1)=PRECIS(2)
                 PRECIS(2)=PRAUX
            ENDIF
            NXT(IX)=2
       ENDIF
*   Add a third point in the event there are two data points.
       IF(NXT(IX).EQ.2)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Two data points so far,'',
     -           '' adding a point at a mirrored C.'')')
            ITER=0
            XT(6,IX)=2*XT(4,IX)-XT(5,IX)
220         CONTINUE
            CALL DLCALC(P*REF+Q*XT(6,IX),Q*REF-P*XT(6,IX),0.0,QCHARG,1)
            IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
            NDLC=NDLC+1
            XT(3,IX)=TU(NU)
            PRECIS(3)=.TRUE.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''ITER='',I2,'', C='',E15.8,
     -           '', T='',E15.8,'', ISTAT='',I3)')
     -           ITER,XT(6,IX),XT(3,IX),ISTAT
            IF(ISTAT.NE.I)THEN
                 XT(6,IX)=0.5*(XT(4,IX)+XT(6,IX))
                 ITER=ITER+1
                 IF(ITER.LE.ITERMX)GOTO 220
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''3rd point not found'')')
                 XTSET(IX)=.FALSE.
                 IXTFLG(IX)=-5
                 GOTO 200
            ELSEIF(XT(3,IX).LT.XT(1,IX))THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Third data point'',
     -                '' has a T < T0, data points swapped.'')')
                 XTAUX=XT(1,IX)
                 XT(1,IX)=XT(3,IX)
                 XT(3,IX)=XTAUX
                 XTAUX=XT(4,IX)
                 XT(4,IX)=XT(6,IX)
                 XT(6,IX)=XTAUX
                 PRAUX=PRECIS(1)
                 PRECIS(1)=PRECIS(3)
                 PRECIS(3)=PRAUX
            ENDIF
            NXT(IX)=3
       ENDIF
**  Parabolic minimisation itself, first sort the XT array.
       IF(XT(4,IX).GT.XT(5,IX))THEN
            XTAUX=XT(2,IX)
            XT(2,IX)=XT(1,IX)
            XT(1,IX)=XTAUX
            XTAUX=XT(5,IX)
            XT(5,IX)=XT(4,IX)
            XT(4,IX)=XTAUX
            PRAUX=PRECIS(2)
            PRECIS(2)=PRECIS(1)
            PRECIS(1)=PRAUX
       ENDIF
       IF(XT(5,IX).GT.XT(6,IX))THEN
            XTAUX=XT(3,IX)
            XT(3,IX)=XT(2,IX)
            XT(2,IX)=XTAUX
            XTAUX=XT(6,IX)
            XT(6,IX)=XT(5,IX)
            XT(5,IX)=XTAUX
            PRAUX=PRECIS(3)
            PRECIS(3)=PRECIS(2)
            PRECIS(2)=PRAUX
       ENDIF
       IF(XT(4,IX).GT.XT(5,IX))THEN
            XTAUX=XT(2,IX)
            XT(2,IX)=XT(1,IX)
            XT(1,IX)=XTAUX
            XTAUX=XT(5,IX)
            XT(5,IX)=XT(4,IX)
            XT(4,IX)=XTAUX
            PRAUX=PRECIS(2)
            PRECIS(2)=PRECIS(1)
            PRECIS(1)=PRAUX
       ENDIF
*   Calculate exact drift time for one of the side points.
       IF(.NOT.PRECIS(1))THEN
            CALL DLCALC(P*REF+Q*XT(4,IX),Q*REF-P*XT(4,IX),0.0,QCHARG,1)
            IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
            NDLC=NDLC+1
            XT(1,IX)=TU(NU)
            PRECIS(1)=.TRUE.
       ENDIF
*   Calculate exact drift time for the middle point, if not yet done.
       IF(.NOT.PRECIS(2))THEN
            CALL DLCALC(P*REF+Q*XT(5,IX),Q*REF-P*XT(5,IX),0.0,QCHARG,1)
            IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
            NDLC=NDLC+1
            XT(2,IX)=TU(NU)
            PRECIS(2)=.TRUE.
       ENDIF
*   Calculate exact drift time for the other side point.
       IF(.NOT.PRECIS(3))THEN
            CALL DLCALC(P*REF+Q*XT(6,IX),Q*REF-P*XT(6,IX),0.0,QCHARG,1)
            IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
            NDLC=NDLC+1
            XT(3,IX)=TU(NU)
            PRECIS(3)=.TRUE.
       ENDIF
**  Starting point found, now proceed with parabolic minimisation.
       DO 300 J=1,ITERMX
       IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Parabolic search loop '',I2,/,
     -      29X,''C,T low   ='',E15.8,'', '',E15.8,'', Prec='',L1,/,
     -      29X,''C,T middle='',E15.8,'', '',E15.8,'', Prec='',L1,/,
     -      29X,''C,T high  ='',E15.8,'', '',E15.8,'', Prec='',L1,/)')
     -      J,(XT(II+3,IX),XT(II,IX),PRECIS(II),II=1,3)
*   Fit a parabola to the three points.
       CALL DRFXT2(XT(4,IX),XT(1,IX),XT(5,IX),XT(2,IX),
     -      XT(6,IX),XT(3,IX),CPARA,TPARA,IFAIL,IFLAG)
       IF(IFLAG.NE.+1.OR.IFAIL.NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''DRFXT2 returns on IFAIL='',
     -           I2,'', IFLAG='',I2)') IFAIL,IFLAG
            XTSET(IX)=.FALSE.
            IXTFLG(IX)=-6
            GOTO 200
       ENDIF
*   Check whether the fit is at all acceptable.
       IF(XT(1,IX).GT.XT(2,IX).AND.XT(3,IX).GT.XT(2,IX).AND.
     -      (CPARA-XT(4,IX))*(CPARA-XT(6,IX)).GE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Inadequate fit.'')')
            IXTFLG(IX)=-10
            XTSET(IX)=.FALSE.
            GOTO 200
       ENDIF
*   Calculate a drift-line from the presumed minimum.
       CALL DLCALC(P*REF+Q*CPARA,Q*REF-P*CPARA,0.0,QCHARG,1)
       IF(LDRPLT.AND.NU.GT.1)CALL GPL2(NU,XU,YU)
       NDLC=NDLC+1
       IF(ISTAT.NE.I)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Wire lost ISTAT='',I3)')ISTAT
            XTSET(IX)=.FALSE.
            IXTFLG(IX)=-3
            GOTO 200
       ENDIF
*   Print some debugging output if requested about the minimum.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''CPARA='',E15.8,'', TPARA='',
     -      E15.8,'', TU(NU)='',E15.8)') CPARA,TPARA,TU(NU)
*   Stop if the change is very small, CPARA is internal.
       IF((CPARA-XT(4,IX))*(CPARA-XT(6,IX)).LT.0.0.AND.
     -      ABS(TU(NU)-MIN(XT(1,IX),XT(2,IX),XT(3,IX))).LE.
     -      EPS*(ABS(TU(NU)+MIN(XT(1,IX),XT(2,IX),XT(3,IX)))))THEN
            IF(TU(NU).LT.XT(2,IX))THEN
                 XT(2,IX)=TU(NU)
                 XT(5,IX)=CPARA
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Convergence :'',
     -           '' C='',E15.8,'', T='',E15.8)') XT(5,IX),XT(2,IX)
            IXTFLG(IX)=J
            GOTO 310
*   New point is worse but inside, replace outer point on same side.
       ELSEIF((CPARA-XT(4,IX))*(CPARA-XT(6,IX)).LT.0.0.AND.
     -      TU(NU).GT.MIN(XT(1,IX),XT(2,IX),XT(3,IX)))THEN
            IF(CPARA.LT.XT(5,IX))THEN
                 XT(1,IX)=TU(NU)
                 XT(4,IX)=CPARA
                 PRECIS(1)=.TRUE.
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                ''Lower C point is replaced by new minimum.'')')
            ELSE
                 XT(3,IX)=TU(NU)
                 XT(6,IX)=CPARA
                 PRECIS(3)=.TRUE.
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                ''Higher C point is replaced by new minimum.'')')
            ENDIF
*   The new point is better and inside, remove opposite outer point.
       ELSEIF((CPARA-XT(4,IX))*(CPARA-XT(5,IX)).LT.0.0)THEN
            XT(6,IX)=XT(5,IX)
            XT(5,IX)=CPARA
            XT(3,IX)=XT(2,IX)
            XT(2,IX)=TU(NU)
            PRECIS(3)=PRECIS(2)
            PRECIS(2)=.TRUE.
       ELSEIF((CPARA-XT(5,IX))*(CPARA-XT(6,IX)).LT.0.0)THEN
            XT(4,IX)=XT(5,IX)
            XT(5,IX)=CPARA
            XT(1,IX)=XT(2,IX)
            XT(2,IX)=TU(NU)
            PRECIS(1)=PRECIS(2)
            PRECIS(2)=.TRUE.
*   New point outside, non-parabolic, shift to add new point.
       ELSEIF(CPARA.LE.XT(4,IX))THEN
            XT(6,IX)=XT(5,IX)
            XT(5,IX)=XT(4,IX)
            XT(4,IX)=CPARA
            XT(3,IX)=XT(2,IX)
            XT(2,IX)=XT(1,IX)
            XT(1,IX)=TU(NU)
            PRECIS(3)=PRECIS(2)
            PRECIS(2)=PRECIS(1)
            PRECIS(1)=.TRUE.
       ELSEIF(CPARA.GE.XT(6,IX))THEN
            XT(4,IX)=XT(5,IX)
            XT(5,IX)=XT(6,IX)
            XT(6,IX)=CPARA
            XT(1,IX)=XT(2,IX)
            XT(2,IX)=XT(3,IX)
            XT(3,IX)=TU(NU)
            PRECIS(1)=PRECIS(2)
            PRECIS(2)=PRECIS(3)
            PRECIS(3)=.TRUE.
*   Position not recognised (in view of some logic modifications ...).
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Unrecognised pos, quit'')')
            XTSET(IX)=.FALSE.
            IXTFLG(IX)=-8
            GOTO 200
       ENDIF
*   Warn if the process did not converge.
300    CONTINUE
       XTSET(IX)=.FALSE.
       IF(LDEBUG)WRITE(LUNOUT,'(/26X,''Not converged.'')')
       IXTFLG(IX)=-9
       GOTO 200
**  End of the minimisation process.
310    CONTINUE
*   Calculate the integrated diffusion coefficient.
       IF(GASOK(3))CALL DLCDIF(XT(7,IX))
*   Find the intersections of the drift-line with the R=const lines.
       DO 430 JJX=IIX-JUMP+1,IIX+JUMP-1
       JX=JJX-IXM+1
       IF(JJX.LT.IXM.OR.JJX.GT.IXP.OR.JX.EQ.IX)GOTO 430
       REF=P*(X(I)+JJX*XSTEP)+Q*Y(I)
       CALL DRFXT1(P,Q,REF,CDRIFT,TDRIFT,IFAIL)
       IF(IFAIL.EQ.0)THEN
            XTSET(JX)=.TRUE.
            NXT(JX)=1
            XT(1,JX)=TU(NU)-TDRIFT
            XT(4,JX)=CDRIFT
            IXTFLG(JX)=0
       ENDIF
430    CONTINUE
*   Proceed with next point.
200    CONTINUE
*** Close the plotframe for drift lines (if the plot is made).
       IF(LDRPLT)THEN
            CALL GRNEXT
            CALL GRALOG('Drift lines for an x(t)-plot            ')
       ENDIF
*** Store minimum in position 2.
       DO 540 IIX=IXM,IXP
       IX=IIX-IXM+1
       IF(NXT(IX).EQ.0)GOTO 540
       IMIN=1
       DO 530 IIMIN=2,NXT(IX)
       IF(XT(IIMIN,IX).LT.XT(IMIN,IX))IMIN=IIMIN
530    CONTINUE
       XT(2,IX)=XT(IMIN,IX)
       XT(5,IX)=XT(IMIN+3,IX)
540    CONTINUE
*** Plot the obtained x(t)-relation.
       IF(LXTPLT)THEN
*   Datermine maximum and minimum.
            IF(GRSMIN.GT.GRSMAX)THEN
                 TMIN=0.0
                 TMAX=0.0
                 DO 500 IIX=IXM,IXP
                 IF(XTSET(IIX-IXM+1).AND.NXT(IIX-IXM+1).GT.0)THEN
                      TMIN=MIN(TMIN,XT(2,IIX-IXM+1))
                      TMAX=MAX(TMAX,XT(2,IIX-IXM+1))
                 ENDIF
500              CONTINUE
                 TMIN=0.9*TMIN
                 TMAX=1.1*TMAX
            ELSE
                 TMIN=GRSMIN
                 TMAX=GRSMAX
            ENDIF
*   Open a frame to plot the curves in.
            CALL GRCART(XTXMIN-X(I),TMIN,XTXMAX-X(I),TMAX,
     -           '           x-Distance from the Wire [cm]',
     -           '           Minimum Drift Time [microsec]',
     -           'x(t)-Correlation plot                   ')
*   Add some comments to the plot.
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            WRITE(INFILE,1080) ANGLE
            CALL GRCOMM(3,INFILE)
            WRITE(INFILE,1090) I,WIRTYP(I)
            CALL GRCOMM(4,INFILE)
*   And plot the curves, first the minimum drift time itself.
            NPLOT=0
            CALL GRATTS('FUNCTION-1','POLYLINE')
            CALL GRATTS('FUNCTION-1','POLYMARKER')
            DO 510 IIX=IXM,IXP
            IX=IIX-IXM+1
            IF(XTSET(IX))THEN
                 NPLOT=NPLOT+1
                 XPL(NPLOT)=IIX*XSTEP
                 YPL(NPLOT)=XT(2,IX)
            ELSE
                 IF(NPLOT.GT.1)THEN
                      CALL GPL(NPLOT,XPL,YPL)
                 ELSEIF(NPLOT.EQ.1)THEN
                      CALL GPM(1,XPL,YPL)
                 ENDIF
                 NPLOT=0
            ENDIF
510         CONTINUE
            IF(NPLOT.GT.1)THEN
                 CALL GPL(NPLOT,XPL,YPL)
            ELSEIF(NPLOT.EQ.1)THEN
                 CALL GPM(1,XPL,YPL)
            ENDIF
*   next the diffusion coefficient, provided the data is present.
            IF(GASOK(3))THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 CALL GRATTS('FUNCTION-2','POLYMARKER')
                 NPLOT=0
                 DO 520 IIX=IXM,IXP
                 IX=IIX-IXM+1
                 IF(XTSET(IX).AND.IIX.EQ.JUMP*(IIX/JUMP))THEN
                      NPLOT=NPLOT+1
                      XPL(NPLOT)=IIX*XSTEP
                      YPL(NPLOT)=XT(7,IX)
                 ELSE
                      IF(NPLOT.GT.1)THEN
                           CALL GPL(NPLOT,XPL,YPL)
                      ELSEIF(NPLOT.EQ.1)THEN
                           CALL GPM(1,XPL,YPL)
                      ENDIF
                      NPLOT=0
                 ENDIF
520              CONTINUE
                 IF(NPLOT.GT.1)THEN
                      CALL GPL(NPLOT,XPL,YPL)
                 ELSEIF(NPLOT.EQ.1)THEN
                      CALL GPM(1,XPL,YPL)
                 ENDIF
                 CALL GSLN(1)
            ENDIF
*   Close the plotframe and register the plot.
            CALL GRNEXT
            WRITE(INFILE,'(''x(t) plot for wire '',I3,'', type '',A1)')
     -           I,WIRTYP(I)
            CALL GRALOG(INFILE//'          ')
       ENDIF
*** Store the obtained x(t)-relation: store minimum in position 2.
       IF(LXTKP)THEN
*   Select the points where a minimum was found.
            NPLOT=0
            DO 560 IIX=IXM,IXP
            IX=IIX-IXM+1
            IF(XTSET(IX))THEN
                 NPLOT=NPLOT+1
                 XPL(NPLOT)=IIX*XSTEP
                 YPL(NPLOT)=XT(2,IX)
                 ZPL(NPLOT)=XT(7,IX)
            ENDIF
560         CONTINUE
*   Save the vectors.
            JOVER=JOVER+1
            CALL OUTFMT(REAL(I),2,STR2,NC2,'LEFT')
            CALL OUTFMT(REAL(JOVER),2,STR1,NC1,'LEFT')
*   Save the selected electron x(t) relations.
            ISIZ(1)=NPLOT
            IDIM(1)=MXLIST
            CALL MATSAV(XPL,1,IDIM,ISIZ,'X_'//STR1(1:NC1),IFAIL1)
            CALL MATSAV(YPL,1,IDIM,ISIZ,'T_'//STR1(1:NC1),IFAIL2)
            CALL MATSAV(ZPL,1,IDIM,ISIZ,'S_'//STR1(1:NC1),IFAIL3)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0)THEN
                 PRINT *,' !!!!!! DRFXTP WARNING : Error saving x(t)'//
     -                ' for wire '//STR2(1:NC2)//'.'
            ELSE
                 PRINT *,' ------ DRFXTP MESSAGE : x(t) Relation for'//
     -                ' wire '//STR2(1:NC2)//' saved as'//
     -                ' X_'//STR1(1:NC1)//','//
     -                ' T_'//STR1(1:NC1)//' and'//
     -                ' S_'//STR1(1:NC1)//'.'
            ENDIF
       ENDIF
*** Open a dataset for the x(t) if LXTWRT is .TRUE.
       IF(LXTWRT)THEN
*   Open the file and inform DSNLOG.
            CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! DRFXTP WARNING : Opening the file'//
     -                FILE(1:NCFILE)//' failed ; write flag cancelled'
                 LXTWRT=.FALSE.
            ENDIF
            CALL DSNLOG(FILE,'x(t)-plot ','Sequential','Write     ')
*   Now write a heading record to the file.
            CALL DATTIM(DATE,TIME)
            WRITE(STRING,'(''% Created '',A8,'' At '',A8,1X,A8,
     -           '' XTPLOT   "Wire '',I3,'' type '',A1,'' angle '',
     -           F7.1,''"'')') DATE,TIME,MEMBER,I,WIRTYP(I),ANGLE
            IF(REMARK.NE.'None')STRING(51:79)=REMARK
            WRITE(12,'(A80)') STRING
*   Specify the number of records to be written.
            WRITE(12,'('' This member contains '',I3,'' records.'')')
     -           IXP-IXM+1
       ENDIF
**  Print a heading for the x(t) table.
       IF(LXTPRT)THEN
            WRITE(LUNOUT,'(''  x(t)-CORRELATION FOR WIRE '',I3,
     -           '' (TYPE '',A1,'')'',/,
     -           ''  ======================================'')')
     -           I,WIRTYP(I)
            WRITE(LUNOUT,'(''  Wire location: ('',E15.8,'','',
     -           E15.8,'')'',/,''  Convergence at: '',E15.8)')
     -           X(I),Y(I),EPS
            IF(ITERMX.EQ.0)THEN
                 WRITE(LUNOUT,'(''  Minimisation has been disabled.'')')
            ELSE
                 WRITE(LUNOUT,'(''  Minimisation has been enabled.'')')
            ENDIF
            WRITE(LUNOUT,'(/''          x-value  corresponding t'',
     -           ''  corresponding y        diffusion     Remarks''/
     -           ''             [cm]       [microsec]'',
     -           ''             [cm]       [microsec]''//)')
       ENDIF
**  Write the data itself, interpreting the various flags.
       DO 620 JJX=IXM,IXP
       J=JJX-IXM+1
       REF=P*(X(I)+JJX*XSTEP)+Q*Y(I)
*   Prepare a string containing roughly the data.
       IF(IXTFLG(J).GT.0)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A,1X,I2,1X,A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           '   Minimisation converged in',IXTFLG(J),'steps.'
       ELSEIF(IXTFLG(J).EQ.0)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           '   Minimisation not requested or not meaningful.'
       ELSEIF(IXTFLG(J).EQ.-1)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           '   The wire is located at this x-value.'
       ELSEIF(IXTFLG(J).EQ.-2)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' ! Drift-lines starting at this x, do not'//
     -           ' reach the wire.'
       ELSEIF(IXTFLG(J).EQ.-3)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' # Recalculation of the optimum drift-line'//
     -           ' failed.'
       ELSEIF(IXTFLG(J).EQ.-4)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' # Failure to add a 2nd data-point,'//
     -           ' increase LINES.'
       ELSEIF(IXTFLG(J).EQ.-5)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' # Failure to add a third data-point,'//
     -           ' increase LINES.'
       ELSEIF(IXTFLG(J).EQ.-6)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' ! The minimisation process diverged.'
       ELSEIF(IXTFLG(J).EQ.-7)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' ! The minimum does not look parabolic.'
       ELSEIF(IXTFLG(J).EQ.-8)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' # Internal error ; program bug - please report.'
       ELSEIF(IXTFLG(J).EQ.-9)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' ! Minimisation attempted, but no convergence.'
       ELSEIF(IXTFLG(J).EQ.-10)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' # Inadequate parabolic fit; program bug.'
       ELSEIF(IXTFLG(J).EQ.-11)THEN
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' ! No drift-line found which could'//
     -           ' be interpolated.'
       ELSE
            WRITE(OUTSTR,'(2X,4(E15.8,2X),A)')
     -           JJX*XSTEP,XT(2,J),Q*REF-P*XT(5,J),XT(7,J),
     -           ' # x(t) flag not recognised; program bug.'
       ENDIF
*   Remove irrelevant fields.
       IF(.NOT.GASOK(3).OR..NOT.XTSET(J))OUTSTR(54:68)=
     -      '  Not available'
       IF(NXT(J).EQ.0.AND.JJX.NE.0)OUTSTR(20:51)=
     -      '  Not available    Not available'
       IF(IXTFLG(J).EQ.0.AND.JJX.NE.JUMP*INT(REAL(JJX)/REAL(JUMP)))
     -      OUTSTR(54:)=
     -      '  Not available     Interpolated data-point.'
*   And output the string to the relevant units.
       IF(LXTPRT)WRITE(LUNOUT,'(A)',IOSTAT=IOS,ERR=2010) OUTSTR
       IF(LXTWRT)WRITE(12,'(L1,A)',IOSTAT=IOS,ERR=2010)
     -      XTSET(J),OUTSTR(2:)
*   Next data point.
620    CONTINUE
*   Close the file, if openend.
       IF(LXTWRT)CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
*** Proceed with next wire.
100    CONTINUE
*** Normal end of this routine.
       CALL TIMLOG('Calculating x(t) relations:             ')
       IF(LDEBUG)PRINT *,' ++++++ DRFXTP DEBUG   : End of debug output.'
       RETURN
*** Handle I/O problems.
2010   CONTINUE
       PRINT *,' ###### DRFXTP ERROR   : Error while'//
     -      ' writing the x(t) data set ; attempt to close.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### DRFXTP ERROR   : Unable to close the data set'//
     -      ' of the x(t) relations ; results not predictable.'
       CALL INPIOS(IOS)
       END
