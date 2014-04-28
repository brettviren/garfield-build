CDECK  ID>, DLCMCA.
       SUBROUTINE DLCMCA(X1,Y1,Z1,NETOT,NITOT,STAT,
     -      NHIST,IHIST,ITYPE,IENTRY,OPTION)
*-----------------------------------------------------------------------
*   DLCMCA - Subroutine that computes a drift line using a Monte-Carlo
*            technique to take account of diffusion and of avalanche
*            formation.
*   VARIABLES :
*   REFERENCE :
*   (Last changed on 21/ 5/08.)
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
       REAL XLIST(MXMCA),YLIST(MXMCA),ZLIST(MXMCA),TLIST(MXMCA),
     -      ELIST(MXMCA),
     -      XELIST(MXMCA),YELIST(MXMCA),ZELIST(MXMCA),TELIST(MXMCA)
       INTEGER NLIST(MXMCA),ISLIST(MXMCA),NMCA
       COMMON /MCAMAT/ XLIST,YLIST,ZLIST,TLIST,ELIST,
     -      XELIST,YELIST,ZELIST,TELIST,NLIST,ISLIST,NMCA
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
       INTEGER MXVEC
       PARAMETER(MXVEC=10000)
       REAL Q,X1,Y1,Z1,GASTWN,GASATT,PROBTH,PALPHA,PETA,TOFF,
     -      ALPHA(MXLIST),ETA(MXLIST),RVECU(MXVEC),RVECN(MXVEC)
       INTEGER IFAIL,IPART,I,J,K,L,IMCA,NINTER,
     -      NELEC,NION,NETOT,NITOT,NHIST,IHIST(*),IENTRY(*),ITYPE(2,*),
     -      IVECU,IVECN,NEW,NMAX
       LOGICAL LELEPL,LIONPL,LTOWN,LATTA,STAT(4)
       CHARACTER*(*) OPTION
       EXTERNAL GASTWN,GASATT
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCMCA ///'
*** Initial debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DLCMCA DEBUG   : MC drift'',
     -      '' from ('',E15.8,'','',E15.8,'','',E15.8,'')'')') X1,Y1,Z1
*** Make sure that electron drift velocities are available.
       IF(.NOT.GASOK(1))THEN
            PRINT *,' !!!!!! DLCMCA WARNING : Electron drift velocity'//
     -           ' data missing; no avalanche.'
            RETURN
       ENDIF
*** Obtain the matrix to store the avalanche development.
       CALL BOOK('BOOK','MCAMAT','MCA',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DLCMCA WARNING : Unable to obtain'//
     -           ' storage for the avalanche; avalanche not computed.'
            RETURN
       ENDIF
*** Default options.
       LELEPL=.FALSE.
       LIONPL=.FALSE.
       LTOWN=GASOK(4)
       LATTA=GASOK(6)
       NMAX=0
*** Default settings of parameters.
       PROBTH=0.01
*** Decode the options.
       IF(INDEX(OPTION,'NOPLOT-ELECTRON').NE.0)THEN
            LELEPL=.FALSE.
       ELSEIF(INDEX(OPTION,'PLOT-ELECTRON').NE.0)THEN
            LELEPL=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'NOPLOT-ION').NE.0)THEN
            LIONPL=.FALSE.
       ELSEIF(INDEX(OPTION,'PLOT-ION').NE.0)THEN
            IF(.NOT.GASOK(2))THEN
                 PRINT *,' !!!!!! DLCMCA WARNING : Ion mobilities are'//
     -                ' absent; can not compute ion drift lines.'
            ELSE
                 LIONPL=.TRUE.
            ENDIF
       ENDIF
       IF(INDEX(OPTION,'NOTOWNSEND').NE.0)THEN
            LTOWN=.FALSE.
       ELSEIF(INDEX(OPTION,'TOWNSEND').NE.0.AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! DLCMCA WARNING : Townsend data is not'//
     -           ' present; TOWNSEND option not valid.'
       ELSEIF(INDEX(OPTION,'TOWNSEND').NE.0)THEN
            LTOWN=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'NOATTACHMENT').NE.0)THEN
            LATTA=.FALSE.
       ELSEIF(INDEX(OPTION,'ATTACHMENT').NE.0.AND..NOT.GASOK(6))THEN
            PRINT *,' !!!!!! DLCMCA WARNING : Attachment data is not'//
     -           ' present; ATTACHMENT option not valid.'
       ELSEIF(INDEX(OPTION,'ATTACHMENT').NE.0)THEN
            LATTA=.TRUE.
       ENDIF
       IF(INDEX(OPTION,'ABORT-100000').NE.0)THEN
            NMAX=100000
       ELSEIF(INDEX(OPTION,'ABORT-10000').NE.0)THEN
            NMAX=10000
       ELSEIF(INDEX(OPTION,'ABORT-1000').NE.0)THEN
            NMAX=1000
       ELSEIF(INDEX(OPTION,'ABORT-100').NE.0)THEN
            NMAX=100
       ENDIF
*** Make sure that some kind of output has been requested.
       IF(.NOT.(LATTA.OR.LTOWN))THEN
            PRINT *,' !!!!!! DLCMCA WARNING : Neither attachment not'//
     -           ' multiplication to be included; no avalanche.'
            CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL)
            RETURN
       ENDIF
*** Initialise the avalanche table.
       NMCA=1
       XLIST(1)=X1
       YLIST(1)=Y1
       ZLIST(1)=Z1
       TLIST(1)=0
       NLIST(1)=1
       NETOT=1
       NITOT=0
*** Loop over the table.
       IMCA=0
100    CONTINUE
*   Check we are still in the table.
       IMCA=IMCA+1
       IF(IMCA.GT.NMCA)THEN
            CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL)
            RETURN
       ENDIF
*** Loop over the electrons at this location.
       DO 40 J=1,NLIST(IMCA)
*   Compute an electron drift line.
       Q=-1
       IPART=1
       CALL DLCMC(XLIST(IMCA),YLIST(IMCA),ZLIST(IMCA),Q,IPART)
*   Compute alpha and eta vectors.
       CALL DLCEQU(ALPHA,ETA,IFAIL)
*   Offset the time of the electrons by the starting time.
       DO 10 I=1,NU
       TU(I)=TU(I)+TLIST(IMCA)
10     CONTINUE
*** Follow the avalanche development
       DO 20 I=1,NU-1
*   Set initial number of electrons and ions.
       NELEC=1
       NION=0
*   Compute the number of subdivisions.
       NINTER=(ALPHA(I)+ETA(I))/PROBTH
       IF(NINTER.LT.1)NINTER=1
*** Loop over the subdivisions.
       DO 50 K=1,NINTER
*   Probabilities for gain and loss.
       PALPHA=ALPHA(I)/REAL(NINTER)
       PETA=ETA(I)/REAL(NINTER)
*   Gaussian approximation.
       IF(NELEC.GT.100)THEN
            DATA IVECN/0/
            IF(IVECN.EQ.0.OR.IVECN+2.GT.MXVEC)THEN
                 CALL RNORML(RVECN,MXVEC)
                 IVECN=1
            ENDIF
            IF(LTOWN)THEN
                 NELEC=NELEC+NINT(REAL(NELEC)*PALPHA+RVECN(IVECN)*
     -                SQRT(REAL(NELEC)*PALPHA*(1-PALPHA)))
                 NION=NION+NINT(REAL(NELEC)*PALPHA+RVECN(IVECN)*
     -                SQRT(REAL(NELEC)*PALPHA*(1-PALPHA)))
                 IVECN=IVECN+1
            ENDIF
            IF(LATTA)THEN
                 NELEC=NELEC-NINT(REAL(NELEC)*PETA+RVECN(IVECN)*
     -                SQRT(REAL(NELEC)*PETA*(1-PETA)))
                 IVECN=IVECN+1
            ENDIF
*   Binomial approximation.
       ELSE
            NEW=0
            DO 80 L=1,NELEC
            DATA IVECU/0/
            IF(IVECU.EQ.0.OR.IVECU+2.GT.MXVEC)THEN
                 CALL RANLUX(RVECU,MXVEC)
                 IVECU=1
            ENDIF
            IF(LTOWN)THEN
                 IF(RVECU(IVECU).LT.PALPHA)THEN
                      NEW=NEW+1
                      NION=NION+1
                 ENDIF
                 IVECU=IVECU+1
            ENDIF
            IF(LATTA)THEN
                 IF(RVECU(IVECU).LT.PETA)NEW=NEW-1
                 IVECU=IVECU+1
            ENDIF
80          CONTINUE
            NELEC=NELEC+NEW
       ENDIF
*   Verify that there still is an electron.
       IF(NELEC.LE.0)THEN
            NETOT=NETOT-1
            IF(STAT(2))CALL DLCMCF(REAL(XU(I)+XU(I+1))/2,
     -           REAL(YU(I)+YU(I+1))/2,REAL(ZU(I)+ZU(I+1))/2,
     -           REAL(TU(I)+TU(I+1))/2,1,NHIST,IHIST,
     -           ITYPE,IENTRY,2)
            GOTO 60
       ENDIF
*   Next subdivision.
50     CONTINUE
60     CONTINUE
*** If at least 1 new electron has been created, add to the table.
       IF(NELEC.GT.1)THEN
*   Ensure we do not pass the maximum permitted avalanche size.
            IF(NMCA+1.GT.NMAX.AND.NMAX.GT.0)THEN
                 PRINT *,' !!!!!! DLCMCA WARNING : Avalanche exceeds'//
     -                ' maximum permitted size; avalanche ended.'
                 CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL)
                 RETURN
*   Ensure there is still space in the table.
            ELSEIF(NMCA+1.GT.MXMCA)THEN
                 PRINT *,' !!!!!! DLCMCA WARNING : Overflow of'//
     -                ' secondary electron table; avalanche ended.'
                 CALL BOOK('RELEASE','MCAMAT','MCA',IFAIL)
                 RETURN
            ENDIF
*   Add the point to the table,
            NMCA=NMCA+1
            XLIST(NMCA)=XU(I+1)
            YLIST(NMCA)=YU(I+1)
            ZLIST(NMCA)=ZU(I+1)
            TLIST(NMCA)=TU(I+1)
            NLIST(NMCA)=NELEC-1
*   And also enter in the overall statistics.
            NETOT=NETOT+NELEC-1
*   And enter the newly created electrons in the histograms.
            IF(STAT(1))CALL DLCMCF(REAL(XU(I)+XU(I+1))/2,
     -           REAL(YU(I)+YU(I+1))/2,REAL(ZU(I)+ZU(I+1))/2,
     -           REAL(TU(I)+TU(I+1))/2,NELEC-1,NHIST,IHIST,
     -           ITYPE,IENTRY,1)
       ENDIF
*** Also compute the newly produced ions if requested.
       IF(NION.GE.1.AND.(LIONPL.OR.STAT(4)))THEN
*   Store offset time.
            TOFF=TU(I+1)
*   Make a backup of the electron drift line.
            CALL DLCBCK('SAVE')
            DO 30 K=1,NION
*   Compute the ion drift lines.
            Q=+1
            IPART=2
            CALL DLCMC(XLIST(NMCA),YLIST(NMCA),ZLIST(NMCA),Q,IPART)
*   Offset the time of the ions by the starting time.
            DO 90 L=1,NU
            TU(L)=TU(L)+TOFF
90          CONTINUE
*   Enter the ion end point in the histograms if requested.
            IF(STAT(4))CALL DLCMCF(REAL(XU(NU)),REAL(YU(NU)),
     -           REAL(ZU(NU)),REAL(TU(NU)),1,
     -           NHIST,IHIST,ITYPE,IENTRY,4)
*   Plot the ion drift line.
            IF(LIONPL)CALL DLCPLT
30          CONTINUE
*   Restore electron drift line.
            CALL DLCBCK('RESTORE')
       ENDIF
*** Keep track of ion statistics.
       NITOT=NITOT+NION
*** Make sure the electron is still alive.
       IF(NELEC.LE.0)THEN
            NU=I
            GOTO 70
       ENDIF
20     CONTINUE
*   If electron survived, register its end point.
       IF(STAT(3))CALL DLCMCF(REAL(XU(NU)),
     -      REAL(YU(NU)),REAL(ZU(NU)),REAL(TU(NU)),1,
     -      NHIST,IHIST,ITYPE,IENTRY,3)
*   Plot the electron if requested.
70     CONTINUE
       IF(LELEPL)CALL DLCPLT
*   Proceed with next electron.
40     CONTINUE
*** And proceed with the next table entry.
       GOTO 100
       END
