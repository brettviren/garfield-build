CDECK  ID>, DRFINP.
       SUBROUTINE DRFINP
*-----------------------------------------------------------------------
*   DRFINP - Routine reading instructions with regard to the drift line
*            and equal time contours and calling the appropriate routine
*   VARIABLES : STRING     : (parts of) the instruction read by INPWRD
*               VTEST      : Used for drift velocity printing.
*   (Last changed on 12/ 4/08.)
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
       LOGICAL MAGOK
       REAL ALFA,B0X,B0Y,B0Z,SUSWIR,SUSGAS,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX
       INTEGER MAGSRC,
     -      IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z
       CHARACTER*(MXCHAR) FUNB0X,FUNB0Y,FUNB0Z
       COMMON /MAGDAT/ ALFA,SUSWIR,SUSGAS,
     -      B0X,B0Y,B0Z,BSCALE,BFMIN,BFMAX,
     -      BFXMIN,BFYMIN,BFZMIN,BFXMAX,BFYMAX,BFZMAX,
     -      MAGSRC,IBXTYP,IBYTYP,IBZTYP,
     -      IRB0X,IRB0Y,IRB0Z,IRV0X,IRV0Y,IRV0Z,
     -      IENB0X,IENB0Y,IENB0Z,IBXDIR,IBYDIR,IBZDIR,
     -      NCB0X,NCB0Y,NCB0Z,
     -      MAGOK
       COMMON /MAGCHR/ FUNB0X,FUNB0Y,FUNB0Z
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
       CHARACTER*(MXCHAR) STRING
       DOUBLE PRECISION VTEST(3)
       INTEGER INPCMP,INPTYP,NWORD,IFAIL,IFAIL1,IFAIL2,IFAIL3,NLINER,NC,
     -      NGRIDR,NGRDXR,NGRDYR,ILOC,ILOC1,ILOC2,I,INEXT,NLTR,NLTRR,
     -      ITEST,NTEST
       REAL XTEST,YTEST,ZTEST,EXTEST,EYTEST,EZTEST,ETEST,VOLT,QTEST,
     -      TMIN,TMAX,SMIN,SMAX,AMIN,AMAX,BMIN,BMAX,CPU,RNDM
       LOGICAL FLAG(MXWORD+3),LDIFF,LTOWN,LATTA
       EXTERNAL INPCMP,INPTYP,RNDM
*** Define some formats
1020   FORMAT('  Current number of grid points is ',I3,' by ',I3,'.')
1040   FORMAT('  Distance between equal time contours ',F10.3,
     -        ' [microsec]')
*** Identify the routine, if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DRFINP ///'
*** Print a heading for the drift section.
       WRITE(LUNOUT,'(''1'')')
       PRINT *,' ================================================'
       PRINT *,' ==========  Start of drift section    =========='
       PRINT *,' ================================================'
       PRINT *,' '
*** Check that valid gas data are present.
       IF(.NOT.GASOK(1))THEN
            PRINT *,' ###### DRFINP ERROR   : The drift velocity data'//
     -           ' are missing; this section can not be executed.'
            CALL SKIP
            RETURN
       ENDIF
*** Set default area.
       CALL GRASET(DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX)
*** Start an input loop.
       CALL INPPRM('Drift','NEW-PRINT')
10     CONTINUE
       CALL INPWRD(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Skip if the line is blank.
       IF(NWORD.EQ.0)GOTO 10
*** Return to main program if & is the first character.
       IF(STRING(1:1).EQ.'&')THEN
            RETURN
*** Look for the AREA instruction.
       ELSEIF(INPCMP(1,'AREA').NE.0)THEN
            CALL CELVIE(DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX)
            CALL INPERR
*** ARRIVAL-TIME-DISTRIBUTION.
       ELSEIF(INPCMP(1,'ARR#IVAL-#TIME-#DISTRIBUTION').NE.0)THEN
            CALL DRFARR
*** Cluster study.
       ELSEIF(INPCMP(1,'CL#USTERING-#HISTOGRAMS').NE.0)THEN
            CALL DRFCLS
*** Look for the keyword DRIFT.
       ELSEIF(INPCMP(1,'DR#IFT').NE.0)THEN
            CALL DRFDRF
*** Look for the EPSILON keyword.
       ELSEIF(INPCMP(1,'EPS#ILON').NE.0)THEN
            PRINT *,' !!!!!! DRFINP WARNING : This parameter should'//
     -           ' be changed from INTEGRATION-PARAMETERS.'
*** Look for the keyword GRAPHICS.
       ELSEIF(INPCMP(1,'GRA#PHICS-#INPUT').NE.0)THEN
            CALL DRFGRA
*** Look for the keyword GRID.
       ELSEIF(INPCMP(1,'GRI#D').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,1020) NGRIDX,NGRIDY
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NGRIDR,25)
                 IF(NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)
     -                CALL INPMSG(2,'GRID out of range 2 -> MXGRID.')
                 IF(IFAIL1.NE.0.OR.NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! DRFINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRIDR
                      NGRIDY=NGRIDR
                 ENDIF
            ELSEIF(NWORD.EQ.3)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPCHK(3,1,IFAIL2)
                 CALL INPRDI(2,NGRDXR,25)
                 CALL INPRDI(3,NGRDYR,25)
                 IF(NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID)
     -                CALL INPMSG(2,'out of the range 2 -> MXGRID. ')
                 IF(NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)
     -                CALL INPMSG(2,'out of the range 2 -> MXGRID. ')
                 IF(IFAIL1.NE.0.OR.NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID.OR.
     -                NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! DRFINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRDXR
                      NGRIDY=NGRDYR
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! DRFINP WARNING : GRID requires 1'//
     -                ' or 2 arguments ; the instruction is ignored.'
            ENDIF
            CALL INPERR
*** Integration parameters.
       ELSEIF(INPCMP(1,'INT#EGRATION-#PARAMETERS').NE.0)THEN
            CALL DLCPAR
*** Look for the keyword LINE and find NLINED.
       ELSEIF(INPCMP(1,'L#INES').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Number of drift lines is '',I4,
     -                ''.'')') NLINED
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NLINER,0)
                 IF(IFAIL1.EQ.0.AND.NLINER.LE.0)CALL INPMSG(2,
     -                'number of drift lines not > 0 ')
                 IF(IFAIL1.NE.0.OR.NLINER.LE.0)THEN
                      PRINT *,' !!!!!! DRFINP WARNING : LINES is'//
     -                     ' ignored because of (syntax) errors.'
                 ELSE
                      NLINED=NLINER
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! DRFINP WARNING : LINES needs one'//
     -                ' argument ; instruction is ignored.'
            ENDIF
            CALL INPERR
*** Print the Lorentz angle.
       ELSEIF(INPCMP(1,'LO#RENTZ-#ANGLE').NE.0)THEN
            IF(.NOT.MAGOK)THEN
                 PRINT *,' ------ DRFINP MESSAGE : The magnetic field'//
     -                ' is off; Lorentz angle by definition zero.'
            ELSEIF(NWORD.LT.3.OR.NWORD.GT.4)THEN
                 PRINT *,' !!!!!! DRFINP WARNING : The LORENT-ANGLE'//
     -                ' instruction takes 2 arguments; ignored.'
            ELSE
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPCHK(3,2,IFAIL2)
                 CALL INPCHK(4,2,IFAIL3)
                 CALL INPRDR(2,XTEST,0.0)
                 CALL INPRDR(3,YTEST,0.0)
                 CALL INPRDR(4,ZTEST,0.0)
                 IF(IFAIL1+IFAIL2+IFAIL3.NE.0)THEN
                      PRINT *,' !!!!!! DRFINP WARNING : LORENTZ-ANGLE'//
     -                     ' ignored because of syntax errors.'
                      CALL INPERR
                      GOTO 10
                 ENDIF
                 IF(POLAR)THEN
                      CALL CFMPTR(XTEST,YTEST,XTEST,YTEST,1,IFAIL)
                      IF(IFAIL.NE.0)THEN
                           PRINT *,' ++++++ DRFINP DEBUG   : Illegal'//
     -                          ' coordinates; no output.'
                           CALL INPERR
                           GOTO 10
                      ENDIF
                 ENDIF
                 CALL EFIELD(XTEST,YTEST,ZTEST,
     -                EXTEST,EYTEST,EZTEST,ETEST,VOLT,
     -                0,ILOC1)
                 CALL DLCVEL(DBLE(XTEST),DBLE(YTEST),DBLE(ZTEST),
     -                VTEST,-1.0,1,ILOC2)
                 IF(ILOC1.NE.0.OR.ILOC2.NE.0.OR.ETEST.EQ.0.OR.
     -                VTEST(1)**2+VTEST(2)**2.EQ.0)THEN
                      PRINT *,' !!!!!! DRFINP WARNING : Lorentz angle'//
     -                     ' not computed (e.g. in a wire).'
                 ELSE
                      WRITE(LUNOUT,'(/''  Lorentz angle is: '',E15.8,
     -                     '' degrees.'')') 180.0*ACOS(MAX(-1.0,MIN(1.0,
     -                     REAL((EXTEST*VTEST(1)+EYTEST*VTEST(2)+
     -                     EZTEST*VTEST(3))/(ETEST*(SQRT(VTEST(1)**2+
     -                     VTEST(2)**2+VTEST(3)**2)))))))/PI
                 ENDIF
            ENDIF
            CALL INPERR
*** Minimisation.
       ELSEIF(INPCMP(1,'MIN#IMISE').NE.0)THEN
            CALL DRFMIN
*** Look for the keyword OPTIONS:
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(/
     -                ''  LOCAL OPTIONS CURRENTLY IN EFFECT:''//
     -                ''  Plotting of the drift lines'',
     -                '' (DRIFT-PLOT):     '',L1/
     -                ''  Printing of drift line details'',
     -                '' (DRIFT-PRINT): '',L1/
     -                ''  Plotting of a table of contour'',
     -                '' heights (KEY): '',L1/
     -                ''  Contour all media (T) or drift'',
     -                '' medium (F):    '',L1/
     -                ''  Plot wires by markers'',
     -                '' (WIRE-MARKERS):         '',L1/
     -                ''  Check for multiple field map indices:'',
     -                ''         '',L1/)')
     -                LDRPLT,LDRPRT,LKEYPL,LCNTAM,LWRMRK,LMAPCH
            ELSE
                 DO 40 I=2,NWORD
*   search for plotting of drift lines option,
                 IF(INPCMP(I,'NOD#RIFT-PL#OT').NE.0)THEN
                      LDRPLT=.FALSE.
                 ELSEIF(INPCMP(I,'D#RIFT-PL#OT').NE.0)THEN
                      LDRPLT=.TRUE.
*   search for printing-of-drift lines option
                 ELSEIF(INPCMP(I,'NOD#RIFT-PR#INT').NE.0)THEN
                      LDRPRT=.FALSE.
                 ELSEIF(INPCMP(I,'D#RIFT-PR#INT').NE.0)THEN
                      LDRPRT=.TRUE.
*   Look for the contour key plotting option.
                 ELSEIF(INPCMP(I,'NOK#EY-#PLOT').NE.0)THEN
                      LKEYPL=.FALSE.
                 ELSEIF(INPCMP(I,'K#EY-#PLOT').NE.0)THEN
                      LKEYPL=.TRUE.
*   Contour drawing options.
                 ELSEIF(INPCMP(I,'CONT#OUR-ALL-#MEDIA').NE.0)THEN
                      LCNTAM=.TRUE.
                 ELSEIF(INPCMP(I,'CONT#OUR-DR#IFT-#MEDIUM')+
     -                INPCMP(I,'CONT#OUR-DR#IFT-#MEDIA').NE.0)THEN
                      LCNTAM=.FALSE.
*   Wires drawn as markers.
                 ELSEIF(INPCMP(I,'NOW#IRE-M#ARKERS').NE.0)THEN
                      LWRMRK=.FALSE.
                 ELSEIF(INPCMP(I,'W#IRE-M#ARKERS').NE.0)THEN
                      LWRMRK=.TRUE.
*   Detect multiple map indices.
                 ELSEIF(INPCMP(I,'CH#ECK-MAP-#INDICES')+
     -                INPCMP(I,'CH#ECK-MAP-#INDEXING').NE.0)THEN
                      LMAPCH=.TRUE.
                 ELSEIF(INPCMP(I,'NOCH#ECK-MAP-#INDICES')+
     -                INPCMP(I,'NOCH#ECK-MAP-#INDEXING').NE.0)THEN
                      LMAPCH=.FALSE.
*   Invalid option if not yet recognised.
                 ELSE
                      CALL INPMSG(I,'the option is not known.      ')
                 ENDIF
40               CONTINUE
            ENDIF
            CALL INPERR
*** PLOT: plot various drift related items.
       ELSEIF(INPCMP(1,'PL#OT-#FIELD').NE.0)THEN
            CALL DRFPLT
*** PREPARE-TRACK: Prepare a drifting information table.
       ELSEIF(INPCMP(1,'PR#EPARE-#TRACK').NE.0.AND..NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! DRFINP WARNING : Track preparation'//
     -           ' must be done after track definition.'
*   Track has indeed been defined.
       ELSEIF(INPCMP(1,'PR#EPARE-#TRACK').NE.0)THEN
*   Initial option values.
            LDIFF=GASOK(3)
            LTOWN=GASOK(4)
            LATTA=GASOK(6)
            NLTR=NLINED
*   Flag recognised keywords.
            DO 30 I=1,NWORD+3
            FLAG(I)=.FALSE.
            IF(INPCMP(I,'A#TTACHMENT-#COEFFICIENT')+
     -           INPCMP(I,'D#IFFUSION-#COEFFICIENT')+
     -           INPCMP(I,'L#INES')+
     -           INPCMP(I,'NOA#TTACHMENT-#COEFFICIENT')+
     -           INPCMP(I,'NOD#IFFUSION-#COEFFICIENT')+
     -           INPCMP(I,'NOT#OWNSEND-#COEFFICIENT')+
     -           INPCMP(I,'T#OWNSEND-#COEFFICIENT').NE.0)FLAG(I)=.TRUE.
30          CONTINUE
*   Loop over the parameter string.
            INEXT=2
            DO 20 I=2,NWORD
            IF(I.LT.INEXT)GOTO 20
*   Check for the number of drift-lines to be used.
            IF(INPCMP(I,'L#INES').NE.0)THEN
                 IF(I+1.GT.NWORD.OR.FLAG(I+1))THEN
                      CALL INPMSG(I,'The argument is missing.      ')
                 ELSEIF(INPTYP(I+1).LE.0)THEN
                      CALL INPMSG(I+1,'The argument is not numeric.  ')
                      INEXT=I+2
                 ELSE
                      CALL INPCHK(I+1,1,IFAIL)
                      CALL INPRDI(I+1,NLTRR,NLTR)
                      IF(IFAIL.EQ.0.AND.NLTRR.LT.4)THEN
                           CALL INPMSG(I+1,
     -                          'At least 4 lines are needed.  ')
                      ELSEIF(IFAIL.EQ.0.AND.NLTRR.GT.MXLIST-4)THEN
                           CALL INPMSG(I+1,
     -                          'Not more than MXLIST-4 lines. ')
                      ELSEIF(IFAIL.EQ.0)THEN
                           NLTR=NLTRR
                      ENDIF
                      INEXT=I+2
                 ENDIF
*   Check for the diffusion options.
            ELSEIF(INPCMP(I,'D#IFFUSION-#COEFFICIENT').NE.0)THEN
                 IF(.NOT.GASOK(3))THEN
                      CALL INPMSG(I,'No diffusion data are present.')
                 ELSE
                      LDIFF=.TRUE.
                 ENDIF
            ELSEIF(INPCMP(I,'NOD#IFFUSION-#COEFFICIENT').NE.0)THEN
                 LDIFF=.FALSE.
*   Check for the Townsend options.
            ELSEIF(INPCMP(I,'T#OWNSEND-#COEFFICIENT').NE.0)THEN
                 IF(.NOT.GASOK(4))THEN
                      CALL INPMSG(I,'No Townsend data are present. ')
                 ELSE
                      LTOWN=.TRUE.
                 ENDIF
            ELSEIF(INPCMP(I,'NOT#OWNSEND-#COEFFICIENT').NE.0)THEN
                 LTOWN=.FALSE.
*   Check for the attachment options.
            ELSEIF(INPCMP(I,'A#TTACHMENT-#COEFFICIENT').NE.0)THEN
                 IF(.NOT.GASOK(6))THEN
                      CALL INPMSG(I,'No attachment data are present')
                 ELSE
                      LATTA=.TRUE.
                 ENDIF
            ELSEIF(INPCMP(I,'NOA#TTACHMENT-#COEFFICIENT').NE.0)THEN
                 LATTA=.FALSE.
*   Unrecognised option.
            ELSE
                 CALL INPMSG(I,'Invalid option, ignored.      ')
            ENDIF
20          CONTINUE
*   Dump error messages.
            CALL INPERR
*   Call the preparation routine with proper arguments.
            CALL DLCTRP(XT0,YT0,ZT0,XT1,YT1,ZT1,LDIFF,LTOWN,LATTA,NLTR,
     -           TMIN,TMAX,SMIN,SMAX,AMIN,AMAX,BMIN,BMAX,IFAIL)
*** Look for the SELECT instruction.
       ELSEIF(INPCMP(1,'SEL#ECT').NE.0)THEN
            CALL CELSEL(' ')
*** Test drift line calculation.
       ELSEIF(INPCMP(1,'SIN#GLE').NE.0)THEN
            CALL DRFSIN
*** Test drift speed calculation.
       ELSEIF(INPCMP(1,'SP#EED').NE.0)THEN
            PRINT *,' ++++++ DRFINP DEBUG   : SPEED start.'
            QTEST=-1.0
            ITEST=1
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPRDR(2,XTEST,0.0)
            CALL INPRDR(3,YTEST,0.0)
            CALL INPRDR(4,ZTEST,0.0)
            DO 60 I=5,NWORD
            IF(INPCMP(I,'E#LECTRON').NE.0)THEN
                 ITEST=1
            ELSEIF(INPCMP(I,'I#ON').NE.0)THEN
                 IF(GASOK(2))THEN
                      ITEST=2
                 ELSE
                      CALL INPMSG(I,'ion mobility data missing.    ')
                 ENDIF
            ELSEIF(INPCMP(I,'POS#ITIVE').NE.0)THEN
                 QTEST=+1.0
            ELSEIF(INPCMP(I,'NEG#ATIVE').NE.0)THEN
                 QTEST=-1.0
            ELSE
                 CALL INPMSG(I,'The option is not known.      ')
            ENDIF
60          CONTINUE
            CALL INPERR
            IF(IFAIL1+IFAIL2+IFAIL3.NE.0)GOTO 10
            IF(POLAR)THEN
                 CALL CFMPTR(XTEST,YTEST,XTEST,YTEST,1,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' ++++++ DRFINP DEBUG   : Illegal'//
     -                     ' coordinates; no output.'
                      GOTO 10
                 ENDIF
            ENDIF
            CALL DLCVEL(DBLE(XTEST),DBLE(YTEST),DBLE(ZTEST),
     -           VTEST,QTEST,ITEST,ILOC)
            PRINT 3030,(VTEST(I),I=1,3),PGAS
3030        FORMAT(' Vx=',E15.8,'   Vy=',E15.8,'   Vz=',E15.8,
     -           '  PGAS=',F10.1)
            IF(POLAR)PRINT *,' (These are internal velocity components)'
            PRINT *,' ++++++ DRFINP DEBUG   : SPEED end.'
*** Drift time table printing.
       ELSEIF(INPCMP(1,'TAB#LE').NE.0)THEN
            CALL DRFTAB
*** Time drift line calculation.
       ELSEIF(INPCMP(1,'TIME').NE.0)THEN
            PRINT *,' ++++++ DRFINP DEBUG   : TIME start.'
            CALL INPCHK(2,1,IFAIL1)
            CALL INPRDI(2,NTEST,10)
            CALL INPERR
            QTEST=-1.0
            PRINT *,' Drift line table:'
            PRINT *,' '
            CALL TIMED(CPU)
            DO 3050 I=1,NTEST
            XTEST=DXMIN+RNDM(I)  *(DXMAX-DXMIN)
            YTEST=DYMIN+RNDM(I+1)*(DYMAX-DYMIN)
            CALL DLCALC(XTEST,YTEST,0.0,QTEST,1)
3060        FORMAT(' Line ',I3,' steps=',I3,' start at (x,y)=',2F15.8,
     -           ' drift time=',F15.8,' microsec')
3065        FORMAT(' Line ',I3,' steps=',I3,' start at (r,phi)=',2F15.8,
     -           ' drift time=',F15.8,' microsec')
            IF(POLAR)THEN
                 CALL CFMRTP(XTEST,YTEST,XTEST,YTEST,1)
                 PRINT 3065,I,NU,XTEST,YTEST,TU(NU)
            ELSE
                 PRINT 3060,I,NU,XTEST,YTEST,TU(NU)
            ENDIF
3050        CONTINUE
            CALL TIMED(CPU)
            PRINT *,' '
            PRINT *,' Total CPU time used=',CPU,' seconds'
            CALL TIMLOG('< TIME: drift lines >                   ')
            PRINT *,' ++++++ DRFINP DEBUG   : TIME end.'
*** Timing distributions.
       ELSEIF(INPCMP(1,'TIMING-#HISTOGRAMS').NE.0)THEN
            CALL DRFTIM
*** Look for the instruction TRACK.
       ELSEIF(INPCMP(1,'TR#ACK').NE.0)THEN
            CALL TRAREA
*** Look for the TRAP instruction.
       ELSEIF(INPCMP(1,'TRAP').NE.0)THEN
            PRINT *,' !!!!!! DRFINP WARNING : This parameter should'//
     -           ' changed from INTEGRATION-PARAMETERS.'
*** Read track information from a dataset if GET is the command.
       ELSEIF(INPCMP(1,'GET-TR#ACK').NE.0)THEN
            CALL DLCTRG(IFAIL)
*** Write the track data if WRITE-TRACK is a keyword.
       ELSEIF(INPCMP(1,'WR#ITE-T#RACK').NE.0)THEN
            CALL DLCTRW
*** Search for the XTPLOT instruction.
       ELSEIF(INPCMP(1,'XT-#PLOT').NE.0)THEN
            IF(POLAR)THEN
                 PRINT *,' !!!!!! DRFINP WARNING : This instruction'//
     -                ' is not valid for polar cells; not executed.'
            ELSE
                 CALL DRFXTP
            ENDIF
*** Writing out of equal time contours.
       ELSEIF(INPCMP(1,'WR#ITE-EQ#UAL-TIM#E-#CONTOURS')+
     -      INPCMP(1,'WR#ITE-ISO#CHRONS')+
     -      INPCMP(1,'WR#ITE-ISO#CHRONES').NE.0)THEN
            CALL DRFEQW
*** It is not possible to get here if the keyword is recognised.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! DRFINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction ; it is ignored.'
       ENDIF
*** End of the loop; go for another iteration.
       GOTO 10
       END
