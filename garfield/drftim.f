CDECK  ID>, DRFTIM.
       SUBROUTINE DRFTIM
*-----------------------------------------------------------------------
*   DRFTIM - Computes the arrival time distribution of some selected
*            electrons from random tracks.
*   VARIABLES :
*   (Last changed on 17/ 6/07.)
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
       INTEGER MXELEC
       PARAMETER(MXELEC=10)
*** Declarations, start setting the max number of histogram channels.
       CHARACTER*(MXCHAR) FCNWGT
       CHARACTER*80 TITLE
       CHARACTER*15 STR1,STR2,STR3
       CHARACTER*10 VARLIS(MXVAR)
       REAL ARRTIM(1,MXPART),VAR(MXVAR),ANGMIN,ANGMAX,ANGMIR,ANGMAR,
     -      XARMIN,XARMAX,YARMIN,YARMAX,XARMIR,XARMAR,YARMIR,YARMAR,
     -      TFORC1,TFORC2,RES(1),XRNDM,ARNDM,RMAX,WEIGHT,XCL,YCL,ZCL,
     -      ECL,BCL,SCL,TCL,RNDM,RNDNOR,EXTRA1
       INTEGER IRFTEL(MXELEC),IRFTGL,MELEC(MXELEC),MODVAR(MXVAR),
     -      JSEL,JALL,IPAIR,NPAIR,NPART,NGLOB,NC1,NC2,NC3,NC,MR,NRES,
     -      IRNDM,NRNDM,NRNDMR,NCHA,NCHAR,KELEC,NCWGT,I,J,IFAIL1,IFAIL2,
     -      MODRES(1),IENWGT,IW,ISWCNT,INEXT,NWORD,NVAR,NREXP,IPRT,
     -      INPTYP,INPCMP
       LOGICAL FLAG(MXWORD+3),LGLBPL,LELEPL,USE(MXVAR),LATTA,
     -      LGLBPR,LELEPR,WFORCE,LKEEP,LDRMC,DONE,OK
       EXTERNAL RNDM,RNDNOR,DIVDIF,INPCMP,INPTYP
       SAVE NRNDM,NCHA,KELEC,MELEC,LGLBPL,LELEPL,LDRMC,LATTA,
     -      LGLBPR,LELEPR,ANGMIN,ANGMAX,VARLIS
*** Initialise those variables that are kept across calls.
       DATA NRNDM  /1000/,    NCHA   /100/
       DATA KELEC  /1/,       MELEC  /MXELEC*5/
       DATA LGLBPL /.TRUE./,  LELEPL /.TRUE./
       DATA LGLBPR /.FALSE./, LELEPR /.FALSE./
       DATA LKEEP  /.FALSE./
       DATA LATTA  /.FALSE./, LDRMC  /.FALSE./
       DATA (VARLIS(I),I=1,2) /'X         ','ANGLE     '/
*** Check the presence of sufficient gas data.
       IF(.NOT.(GASOK(1).AND.GASOK(3).AND.
     -      (GASOK(5).OR.HEEDOK.OR.SRIMOK)))THEN
            PRINT *,' ###### DRFTIM ERROR   : Insufficient gas data'//
     -           ' to perform the calculations.'
            PRINT *,'                         Required are velocity,'//
     -           ' diffusion and cluster data.'
            RETURN
       ENDIF
*** Make sure the cell is not in polar coordinates.
       IF(POLAR)THEN
            PRINT *,' ###### DRFTIM ERROR   : The TIMING function'//
     -           ' can not be applied to polar geometries.'
            RETURN
       ENDIF
*** Initialise various variables being reset at each call.
       XARMIN=DXMIN
       XARMAX=DXMAX
       YARMIN=DYMIN
       YARMAX=DYMAX
       ANGMIN=0
       ANGMAX=0
       WFORCE=.FALSE.
       TFORC1=-1.0
       TFORC2=-1.0
       JSEL=0
       JALL=0
       FCNWGT='1'
       NCWGT=1
*** Examine the input line, flag the known words.
       CALL INPNUM(NWORD)
       DO 10 I=2,NWORD
       IF(INPCMP(I,'X-R#ANGE')+INPCMP(I,'WEIGHT#ING-#FUNCTION')+
     -      INPCMP(I,'Y-R#ANGE')+INPCMP(I,'T#IME-WIN#DOW')+
     -      INPCMP(I,'BIN#S')+INPCMP(I,'ANG#LE-#RANGE')+
     -      INPCMP(I,'ATT#ACHMENT')+INPCMP(I,'NOATT#ACHMENT')+
     -      INPCMP(I,'EL#ECTRONS')+INPCMP(I,'ITER#ATIONS')+
     -      INPCMP(I,'ITER#ATE')+
     -      INPCMP(I,'M#ONTE-C#ARLO-DR#IFT')+
     -      INPCMP(I,'NOM#ONTE-C#ARLO-DR#IFT')+
     -      INPCMP(I,'PL#OT-O#VERALL')+INPCMP(I,'NOPL#OT-O#VERALL')+
     -      INPCMP(I,'PL#OT-SEL#ECTED-#ELECTRONS')+
     -      INPCMP(I,'NOPL#OT-SEL#ECTED-#ELECTRONS')+
     -      INPCMP(I,'PR#INT-O#VERALL')+INPCMP(I,'NOPR#INT-O#VERALL')+
     -      INPCMP(I,'PR#INT-SEL#ECTED-#ELECTRONS')+
     -      INPCMP(I,'NOPR#INT-SEL#ECTED-#ELECTRONS')+
     -      INPCMP(I,'KEEP-#HISTOGRAMS')+INPCMP(I,'NOKEEP-#HISTOGRAMS')+
     -      INPCMP(I,'SIN#GLE-CL#USTER')+
     -      INPCMP(I,'NOSIN#GLE-CL#USTER').NE.0)THEN
            FLAG(I)=.TRUE.
       ELSE
            FLAG(I)=.FALSE.
       ENDIF
10     CONTINUE
       FLAG(NWORD+1)=.TRUE.
       FLAG(NWORD+2)=.TRUE.
       FLAG(NWORD+3)=.TRUE.
       INEXT=2
**  Read in detail.
       OK=.TRUE.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*  Read the angle range.
       IF(INPCMP(I,'ANG#LE-#RANGE').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
                 OK=.FALSE.
            ELSEIF(FLAG(I+2))THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 IF(IFAIL1.NE.0)OK=.FALSE.
                 CALL INPRDR(I+1,ANGMIR,180*ANGMIN/PI)
                 IF(IFAIL1.EQ.0.AND.
     -                (ANGMIR.LT.-90.OR.ANGMIR.GT.+90))THEN
                      CALL INPMSG(I+1,'Not within the range [-90,90].')
                      OK=.FALSE.
                 ELSEIF(ANGMIR.GE.-90.AND.ANGMIR.LE.+90.AND.
     -                IFAIL1.EQ.0)THEN
                      ANGMIN=PI*ANGMIR/180
                      ANGMAX=PI*ANGMIR/180
                 ENDIF
                 INEXT=I+3
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,ANGMIR,180*ANGMIN/PI)
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)OK=.FALSE.
                 IF(IFAIL1.EQ.0.AND.
     -                (ANGMIR.LT.-90.OR.ANGMIR.GT.+90))THEN
                      CALL INPMSG(I+1,'Not within the range [-90,90].')
                      OK=.FALSE.
                 ENDIF
                 CALL INPRDR(I+2,ANGMAR,180*ANGMAX/PI)
                 IF(IFAIL2.EQ.0.AND.
     -                (ANGMAR.LT.-90.OR.ANGMAR.GT.+90))THEN
                      CALL INPMSG(I+2,'Not within the range [-90,90].')
                      OK=.FALSE.
                 ELSEIF(ANGMIR.GE.-90.AND.ANGMIR.LE.+90.AND.
     -                ANGMAR.GE.-90.AND.ANGMAR.LE.+90.AND.
     -                IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      ANGMIN=PI*MIN(ANGMIR,ANGMAR)/180
                      ANGMAX=PI*MAX(ANGMIR,ANGMAR)/180
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Explicit time scale.
       ELSEIF(INPCMP(I,'T#IME-WIN#DOW').NE.0)THEN
            IF(I+2.GT.NWORD.OR.FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'This keyword has 2 arguments. ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2)THEN
                 CALL INPMSG(I+1,'This should be a real argument')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2)THEN
                 CALL INPMSG(I+2,'This should be a real argument')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,TFORC1,-1.0)
                 CALL INPRDR(I+2,TFORC2,-1.0)
                 IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.
     -                TFORC1.LT.0.0.OR.TFORC2.LT.0.0.OR.
     -                TFORC1.EQ.TFORC2)THEN
                      CALL INPMSG(I+1,'Window incorrectly specified. ')
                      CALL INPMSG(I+2,'(See preceding message.)      ')
                      OK=.FALSE.
                 ELSE
                      WFORCE=.TRUE.
                 ENDIF
            ENDIF
            INEXT=I+3
*   The BINS keyword.
       ELSEIF(INPCMP(I,'BIN#S').NE.0)THEN
            IF(I+1.GT.NWORD.OR.FLAG(I+1))THEN
                 CALL INPMSG(I,'This keyword has one argument.')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I+1,'This is an integer argument.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NCHAR,MXCHA)
                 IF(NCHAR.LE.1.OR.NCHAR.GT.MXCHA)THEN
                      CALL INPMSG(I+1,'Inacceptable number of bins.  ')
                      OK=.FALSE.
                 ELSE
                      NCHA=NCHAR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Histogram keeping option.
       ELSEIF(INPCMP(I,'KEEP-#HISTOGRAMS').NE.0)THEN
            LKEEP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-#HISTOGRAMS').NE.0)THEN
            LKEEP=.FALSE.
*   Read the first and last particle to be considered.
       ELSEIF(INPCMP(I,'EL#ECTRONS').NE.0)THEN
            KELEC=0
            DO 21 J=I+1,NWORD
            IF(FLAG(J))THEN
                 GOTO 22
            ELSEIF(KELEC.GE.MXELEC)THEN
                 CALL INPMSG(J,'No room to store this electron')
                 OK=.FALSE.
                 GOTO 21
            ELSE
                 KELEC=KELEC+1
            ENDIF
            IF(INPCMP(J,'L#AST').NE.0)THEN
                 MELEC(KELEC)=0
                 INEXT=J+1
            ELSEIF(INPCMP(J,'ONE-B#UT-#LAST').NE.0)THEN
                 MELEC(KELEC)=-1
                 INEXT=J+1
            ELSEIF(INPCMP(J,'TW#O-B#UT-#LAST').NE.0)THEN
                 MELEC(KELEC)=-2
                 INEXT=J+1
            ELSEIF(INPCMP(J,'TH#REE-B#UT-#LAST').NE.0)THEN
                 MELEC(KELEC)=-3
                 INEXT=J+1
            ELSEIF(INPTYP(J).NE.1)THEN
                 CALL INPMSG(J,'This argument is an integer.  ')
                 OK=.FALSE.
                 INEXT=J
                 KELEC=KELEC-1
            ELSE
                 CALL INPCHK(J,1,IFAIL1)
                 CALL INPRDI(J,MR,5)
                 IF(MR.LT.1-MXPART.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(J,'Smaller than 1-MXPART. ')
                      OK=.FALSE.
                      KELEC=KELEC-1
                 ELSEIF(MR.GT.MXPART.AND.IFAIL1.EQ.0)THEN
                      CALL INPMSG(J,'Larger than MXPART.    ')
                      OK=.FALSE.
                      KELEC=KELEC-1
                 ELSEIF(IFAIL1.EQ.0)THEN
                      MELEC(KELEC)=MR
                 ENDIF
                 INEXT=J+1
            ENDIF
21          CONTINUE
22          CONTINUE
            IF(KELEC.LE.0)THEN
                 CALL INPMSG(I,'Should have an argument.      ')
                 OK=.FALSE.
                 KELEC=1
                 MELEC(1)=5
            ENDIF
*   The ITERATIONS keyword.
       ELSEIF(INPCMP(I,'ITER#ATIONS')+INPCMP(I,'ITER#ATE').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'This keyword has one argument.')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'This is an integer argument.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NRNDMR,NRNDM)
                 IF(NRNDMR.LT.1)THEN
                      CALL INPMSG(I+1,'At least 1 iteration needed.  ')
                      OK=.FALSE.
                 ELSE
                      NRNDM=NRNDMR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Monte Carlo drifting.
       ELSEIF(INPCMP(I,'M#ONTE-C#ARLO-DR#IFT-#LINES')+
     -      INPCMP(I,'MC-DR#IFT-#LINES').NE.0)THEN
            LDRMC=.TRUE.
       ELSEIF(INPCMP(I,'NOM#ONTE-C#ARLO-DR#IFT-#LINES')+
     -      INPCMP(I,'NOMC-DR#IFT-#LINES')+
     -      INPCMP(I,'RUN#GE-K#UTTA-DR#IFT-#LINES').NE.0)THEN
            LDRMC=.FALSE.
*   Take attachment into account.
       ELSEIF(INPCMP(I,'ATT#ACHMENT').NE.0)THEN
            LATTA=.TRUE.
       ELSEIF(INPCMP(I,'NOATT#ACHMENT').NE.0)THEN
            LATTA=.FALSE.
*   Plot options.
       ELSEIF(INPCMP(I,'PL#OT-O#VERALL').NE.0)THEN
            LGLBPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-O#VERALL').NE.0)THEN
            LGLBPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-SEL#ECTED-#ELECTRON').NE.0)THEN
            LELEPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-SEL#ECTED-#ELECTRON').NE.0)THEN
            LELEPL=.FALSE.
*   Print options.
       ELSEIF(INPCMP(I,'PR#INT-O#VERALL').NE.0)THEN
            LGLBPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-O#VERALL').NE.0)THEN
            LGLBPR=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT-SEL#ECTED-#ELECTRON').NE.0)THEN
            LELEPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-SEL#ECTED-#ELECTRON').NE.0)THEN
            LELEPR=.FALSE.
*   Find the x-coordinate range on which this routine will work.
       ELSEIF(INPCMP(I,'X-R#ANGE').NE.0)THEN
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,XARMIR,DXMIN)
                 CALL INPRDR(I+2,XARMAR,DXMAX)
                 IF(XARMIR.EQ.XARMAR)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.     ')
                      CALL INPMSG(I+2,'See preceding message.        ')
                      OK=.FALSE.
                 ELSEIF(MIN(XARMIR,XARMAR).LT.DXMIN.OR.
     -                MAX(XARMIR,XARMAR).GT.DXMAX)THEN
                      CALL INPMSG(I+1,'x-Range not inside the area.  ')
                      CALL INPMSG(I+2,'See preceding message.        ')
                      OK=.FALSE.
                 ELSE
                      XARMIN=MAX(DXMIN,MIN(XARMIR,XARMAR))
                      XARMAX=MIN(DXMAX,MAX(XARMIR,XARMAR))
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Find the y-coordinate range on which this routine will work.
       ELSEIF(INPCMP(I,'Y-R#ANGE').NE.0)THEN
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'Should have two arguments.    ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,YARMIR,DYMIN)
                 CALL INPRDR(I+2,YARMAR,DYMAX)
                 IF(YARMIR.EQ.YARMAR)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.     ')
                      CALL INPMSG(I+2,'See preceding message.        ')
                      OK=.FALSE.
                 ELSEIF(MIN(YARMIR,YARMAR).LT.DYMIN.OR.
     -                MAX(YARMIR,YARMAR).GT.DYMAX)THEN
                      CALL INPMSG(I+1,'y-Range not inside the area.  ')
                      CALL INPMSG(I+2,'See preceding message.        ')
                      OK=.FALSE.
                 ELSE
                      YARMIN=MAX(DYMIN,MIN(YARMIR,YARMAR))
                      YARMAX=MIN(DYMAX,MAX(YARMIR,YARMAR))
                 ENDIF
                 INEXT=I+3
            ENDIF
*   Weighting function.
       ELSEIF(INPCMP(I,'WEIGHT#ING-#FUNCTION').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have one argument.     ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,FCNWGT,NCWGT)
            ENDIF
*   The option is not known to the program.
       ELSE
            CALL INPMSG(I,'The option is not known.      ')
            OK=.FALSE.
       ENDIF
20     CONTINUE
*   Display error messages.
       CALL INPERR
**  Print some debugging output, to check correct input handling.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DRFTIM DEBUG   :'',
     -           '' x-range=('',E12.5,'','',E12.5,''),''/25X,
     -           '' y-range=('',E12.5,'','',E12.5,''),''/25X,
     -           '' angles =('',E12.5,'','',E12.5,''),''/25X,
     -           '' weight = '',A/25X,
     -           '' MC drift = '',L1,'', attachment '',L1/25X,
     -           '' iterations='',I5,'', bins='',I3)')
     -           XARMIN,XARMAX,YARMIN,YARMAX,ANGMIN,ANGMAX,
     -           FCNWGT(1:NCWGT),LDRMC,LATTA,NRNDM,NCHA
            WRITE(LUNOUT,'(26X,''Selected electrons: '',100(I3:))')
     -           (MELEC(I),I=1,KELEC)
       ENDIF
*** Quit now if OK is no longer true and if JFAIL is set.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### DRFTIM ERROR   : Instruction is not'//
     -           ' carried out because of the above errors.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### DRFTIM ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
       ENDIF
*** Translate the weighting function.
       CALL ALGPRE(FCNWGT,NCWGT,VARLIS,2,NRES,USE,IENWGT,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DRFTIM WARNING : Unable to translate'//
     -           ' the weighting function ; no timing histograms.'
            RETURN
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! DRFTIM WARNING : Timing histogram does'//
     -           ' not return 1 result ; no timing histograms.'
            RETURN
       ENDIF
*** Initialise progress printing.
       CALL PROINT('TIMING',2,6)
*** Loop over the selected, attracting wires inside the AREA.
       CALL PROFLD(1,'Wires',REAL(NSW))
       ISWCNT=0
       DO 100 IW=1,NWIRE
*   Check sense wire status.
       IF(INDSW(IW).NE.0)ISWCNT=ISWCNT+1
       CALL PROSTA(1,REAL(ISWCNT))
       IF(INDSW(IW).EQ.0.OR.X(IW).LT.DXMIN.OR.X(IW).GT.DXMAX.OR.
     -      Y(IW).LT.DYMIN.OR.Y(IW).GT.DYMAX.OR.E(IW).LT.0.0)GOTO 100
       IF(LDEBUG)PRINT *,' ++++++ DRFTIM DEBUG   : Wire ',IW,' selected'
*   Inform what is going on.
       CALL PROFLD(2,'Histogram allocation',-1.0)
       CALL PROSTA(2,0.0)
*** Open a plot frame of DRIFT-PLOT is on.
       IF(LDRPLT)THEN
            CALL GRAXIS(DXMIN,DYMIN,DXMAX,DYMAX,
     -           'DRIFT LINES FOR THE TIMING PLOT')
            CALL CELLAY(DXMIN,DYMIN,DXMAX,DYMAX)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
       ENDIF
*   Allocate histogram storage and reset the various counters.
       IF(WFORCE)THEN
            CALL HISADM('ALLOCATE',IRFTGL,NCHA,
     -           TFORC1,TFORC2,.FALSE.,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' ###### DRFTIM ERROR   : Unable to obtain'//
     -                ' histogram space (all, t) ; end of calculations.'
                 RETURN
            ENDIF
            DO 112 I=1,KELEC
            CALL HISADM('ALLOCATE',IRFTEL(I),NCHA,
     -           TFORC1,TFORC2,.FALSE.,IFAIL2)
            IF(IFAIL2.NE.0)THEN
                 PRINT *,' ###### DRFTIM ERROR   : Unable to obtain'//
     -                ' histogram space (sel, t) ; end of calculations.'
                 RETURN
            ENDIF
112         CONTINUE
       ELSE
            CALL HISADM('ALLOCATE',IRFTGL,NCHA,
     -           0.0,1.0,.TRUE.,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' ###### DRFTIM ERROR   : Unable to obtain'//
     -                ' histogram space (all, t) ; end of calculations.'
                 RETURN
            ENDIF
            DO 113 I=1,KELEC
            CALL HISADM('ALLOCATE',IRFTEL(I),NCHA,
     -           0.0,1.0,.TRUE.,IFAIL2)
            IF(IFAIL2.NE.0)THEN
                 PRINT *,' ###### DRFTIM ERROR   : Unable to obtain'//
     -                ' histogram space (sel, t) ; end of calculations.'
                 RETURN
            ENDIF
113         CONTINUE
       ENDIF
*** Initialise counter of all electrons.
       NGLOB=0
*   Carry out NRNDM global random cycles, resetting the counters.
       CALL PROFLD(2,'Tracks',REAL(NRNDM))
       CALL PROSTA(2,0.0)
       IF(NRNDM.LE.10)THEN
            IPRT=1
       ELSE
            IPRT=10**(INT(LOG10(REAL(2*NRNDM)))-1)
       ENDIF
*** Start of MC loop.
       DO 140 IRNDM=1,NRNDM
       IF(IRNDM.EQ.IPRT*(IRNDM/IPRT))CALL PROSTA(2,REAL(IRNDM))
*** Draw a track location.
       XRNDM=XARMIN+RNDM(+IRNDM)*(XARMAX-XARMIN)
       ARNDM=ANGMIN+RNDM(-IRNDM)*(ANGMAX-ANGMIN)
       RMAX=ABS(DXMAX-DXMIN)+ABS(DYMAX-DYMIN)
       XT0=XRNDM-RMAX*SIN(ARNDM)
       YT0=Y(IW)-RMAX*COS(ARNDM)
       ZT0=0
       XT1=XRNDM+RMAX*SIN(ARNDM)
       YT1=Y(IW)+RMAX*COS(ARNDM)
       ZT1=0
       TRFLAG(1)=.TRUE.
*   Compute weight for this track.
       VAR(1)=XRNDM
       VAR(2)=180*ARNDM/PI
       MODVAR(1)=2
       MODVAR(2)=2
       NVAR=2
       NREXP=1
       CALL ALGEXE(IENWGT,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL1)
       IF(IFAIL1.NE.0.OR.MODRES(1).NE.2)THEN
            WRITE(LUNOUT,'(''  ++++++ DRFTIM DEBUG   : Weight not'',
     -           '' used, mode='',I1,'' IFAIL='',I2)')
     -           MODRES(1),IFAIL1
            WEIGHT=1
       ELSE
            WEIGHT=RES(1)
       ENDIF
*   Clip the track to make sure it fits in the AREA.
       CALL CLIP(XT0,YT0,XT1,YT1,DXMIN,MAX(DYMIN,YARMIN),
     -      DXMAX,MIN(DYMAX,YARMAX),IFAIL1)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFTIM DEBUG   : Track: '',
     -      ''('',E15.8,'','',E15.8,'') to ('',E15.8,'','',E15.8,
     -      '').'')') XT0,YT0,XT1,YT1
*   Be sure that at least part of the track is located inside the area.
       IF(IFAIL1.NE.0)THEN
            IF(LDEBUG)PRINT *,' ++++++ DRFTIM DEBUG   : Track not'//
     -           ' in area or zero length; no further computations.'
            GOTO 140
       ENDIF
*** Start clustering on this track.
       CALL TRACLI
*   Reset number of electrons accumulated.
       NPART=0
*** Get a new cluster.
150    CONTINUE
       CALL TRACLS(XCL,YCL,ZCL,ECL,NPAIR,EXTRA1,DONE,IFAIL1)
*   Check whether there was a mistake.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DRFTIM WARNING : Clustering error;'//
     -           ' no histograms.'
            CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL1)
            DO 155 I=1,KELEC
            CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL1)
155         CONTINUE
            IF(LDRPLT)THEN
                 CALL TRAPLT
                 CALL GRALOG('Aborted drift lines for TIMING plot:')
                 CALL GRNEXT
            ENDIF
            RETURN
*   Check whether this was beyond the last cluster.
       ELSEIF(DONE)THEN
            GOTO 170
       ENDIF
*** Monte Carlo variant.
       IF(LDRMC)THEN
*   Loop over the pairs in the cluster.
            DO 200 IPAIR=1,NPAIR
*   Compute the drift line.
            CALL DLCMC(XCL,YCL,ZCL,-1.0,1)
*   Make sure it ends on the wire.
            IF(ISTAT.NE.IW.OR.NU.LT.2)GOTO 200
*   See whether it was lost by attachment.
            IF(GASOK(6).AND.LATTA)THEN
                 CALL DLCATT(BCL)
                 IF(BCL.LT.RNDM(I))GOTO 200
            ENDIF
*   Add the time to the table.
            IF(NPART.GE.MXPART)THEN
                 PRINT *,' !!!!!! DRFTIM WARNING : Too many'//
     -                ' electrons on the track; increase MXPART.'
                 GOTO 140
            ENDIF
            NPART=NPART+1
            ARRTIM(1,NPART)=REAL(TU(NU))
*   And to the overall timing histogram.
            CALL HISENT(IRFTGL,ARRTIM(1,NPART),WEIGHT)
*   Keep track of number of entries.
            NGLOB=NGLOB+1
*   Plot the drift line if required.
            IF(NU.GT.2.AND.LDRPLT)THEN
                 IF(POLAR)CALL CF2RTC(XU,YU,XU,YU,NU)
                 CALL GRATTS('E-DRIFT-LINE','POLYLINE')
                 CALL GPL2(NU,XU,YU)
            ENDIF
200         CONTINUE
*** Analytic variant.
       ELSE
            CALL DLCALC(XCL,YCL,ZCL,-1.0,1)
*   Make sure it ends on the wire.
            IF(ISTAT.NE.IW.OR.NU.LT.2)GOTO 150
*   Adjust number of pairs for attachment.
            IF(GASOK(6).AND.LATTA)THEN
                 CALL DLCATT(BCL)
                 NPAIR=BCL*NPAIR
            ENDIF
*   Compute diffusion and store time.
            TCL=REAL(TU(NU))
            CALL DLCDIF(SCL)
*   And generate the correponding number of arrival times.
            DO 160 IPAIR=1,NPAIR
            IF(NPART.GE.MXPART)THEN
                 PRINT *,' !!!!!! DRFTIM WARNING : Too many'//
     -                ' electrons on the track; increase MXPART.'
                 GOTO 140
            ENDIF
            NPART=NPART+1
            ARRTIM(1,NPART)=RNDNOR(TCL,SCL)
*   And to the overall timing histogram.
            CALL HISENT(IRFTGL,ARRTIM(1,NPART),WEIGHT)
*   Keep track of number of entries.
            NGLOB=NGLOB+1
160         CONTINUE
*   Plot the drift line if required.
            IF(NU.GT.2.AND.LDRPLT)THEN
                 IF(POLAR)CALL CF2RTC(XU,YU,XU,YU,NU)
                 CALL GRATTS('E-DRIFT-LINE','POLYLINE')
                 CALL GPL2(NU,XU,YU)
            ENDIF
       ENDIF
*   Next cluster.
       GOTO 150
*** All clusters done.
170    CONTINUE
       IF(LDRPLT)CALL TRAPLT
*** Find the M'th particle to arrive and enter in a histogram.
       IF(NPART.GE.1)THEN
            CALL SORTRQ(ARRTIM,1,NPART,1)
            DO 180 I=1,KELEC
            IF(MELEC(I).GT.0.AND.MELEC(I).LE.NPART.AND.NPART.GT.0)THEN
                 CALL HISENT(IRFTEL(I),ARRTIM(1,MELEC(I)),WEIGHT)
            ELSEIF(MELEC(I).LE.0.AND.MELEC(I)+NPART.GE.1)THEN
                 CALL HISENT(IRFTEL(I),ARRTIM(1,NPART+MELEC(I)),WEIGHT)
            ENDIF
180         CONTINUE
       ENDIF
*** Proceed with the next random cycle.
140    CONTINUE
*** Check we did indeed collect something.
       IF(NGLOB.LE.0)THEN
            CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL1)
            DO 210 I=1,KELEC
            CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL1)
210         CONTINUE
            IF(LKEEP)PRINT *,' !!!!!! DRFTIM WARNING : Histograms'//
     -           ' not kept - no entries.'
            GOTO 100
       ENDIF
*** Close the plot, if open.
       IF(LDRPLT)THEN
            CALL GRALOG('Drift lines for a timing plot:')
            CALL GRNEXT
       ENDIF
*** Plot the curves.
       IF(LELEPL)THEN
*   Inform about progress.
            CALL PROFLD(2,'Plot selected e-',-1.0)
            CALL PROSTA(2,0.0)
*   Plot each of the electrons.
            DO 190 I=1,KELEC
            CALL OUTFMT(REAL(MELEC(I)),2,STR1,NC1,'LEFT')
            CALL OUTFMT(REAL(IW),2,STR2,NC2,'LEFT')
            IF(MELEC(I).GT.0)THEN
                 TITLE='Arrival time of electron '//STR1(1:NC1)//
     -                ' on wire '//STR2(1:NC2)
                 NC=34+NC1+NC2
            ELSEIF(MELEC(I).EQ.0)THEN
                 TITLE='Arrival time last electron'//
     -                ' on wire '//STR2(1:NC2)
                 NC=35+NC2
            ELSE
                 TITLE='Arrival time last'//STR1(1:NC1)//' electron'//
     -                ' on wire '//STR2(1:NC2)
                 NC=35+NC1+NC2
            ENDIF
            CALL HISPLT(IRFTEL(I),'Arrival time [microsec]',
     -           TITLE(1:NC),.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRNEXT
            CALL GRALOG(TITLE(1:NC))
190         CONTINUE
       ENDIF
       IF(LELEPR)THEN
*   Inform about progress.
            CALL PROFLD(2,'Print selected e-',-1.0)
            CALL PROSTA(2,0.0)
*   Print each of the electrons.
            DO 144 I=1,KELEC
            CALL OUTFMT(REAL(MELEC(I)),2,STR1,NC1,'LEFT')
            CALL OUTFMT(REAL(IW),2,STR2,NC2,'LEFT')
            IF(MELEC(I).GT.0)THEN
                 TITLE='Arrival time of electron '//STR1(1:NC1)//
     -                ' on wire '//STR2(1:NC2)
                 NC=34+NC1+NC2
            ELSEIF(MELEC(I).EQ.0)THEN
                 TITLE='Arrival time last electron'//
     -                ' on wire '//STR2(1:NC2)
                 NC=35+NC2
            ELSE
                 TITLE='Arrival time last'//STR1(1:NC1)//' electron'//
     -                ' on wire '//STR2(1:NC2)
                 NC=35+NC1+NC2
            ENDIF
            CALL HISPRT(IRFTEL(I),'Arrival time [microsec]',TITLE(1:NC))
144         CONTINUE
       ENDIF
*   Global plot.
       IF(LGLBPL)THEN
*   Inform about progress.
            CALL PROFLD(2,'Plot all e-',-1.0)
            CALL PROSTA(2,0.0)
*   Global plot.
            CALL OUTFMT(REAL(IW),2,STR2,NC2,'LEFT')
            CALL HISPLT(IRFTGL,'Arrival time [microsec]',
     -           'Arrival time of all electrons on wire '//
     -           STR2(1:NC2),.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG('Overall arrival time distribution.      ')
            CALL GRNEXT
       ENDIF
       IF(LGLBPR)THEN
*   Inform about progress.
            CALL PROFLD(2,'Print all e-',-1.0)
            CALL PROSTA(2,0.0)
*   Global printout.
            CALL OUTFMT(REAL(IW),2,STR2,NC2,'LEFT')
            CALL HISPRT(IRFTGL,'Arrival time [microsec]',
     -           'Arrival time of all electrons on wire '//
     -           STR2(1:NC2))
       ENDIF
*** Get rid of the histograms, unless KEEP has been specified.
       IF(LKEEP)THEN
*   Inform about progress.
            CALL PROFLD(2,'Saving histograms',-1.0)
            CALL PROSTA(2,0.0)
*   Find names for the histograms and save them.
            JALL=JALL+1
            CALL OUTFMT(REAL(JALL),2,STR1,NC1,'LEFT')
            CALL HISSAV(IRFTGL,'ALL_'//STR1(1:NC1),IFAIL1)
            CALL OUTFMT(REAL(IW),2,STR2,NC2,'LEFT')
            IF(IFAIL1.EQ.0)THEN
                 PRINT *,' ------ DRFTIM MESSAGE : Arrival time'//
     -                ' histogram of all electrons for wire '//
     -                STR2(1:NC2)//' is kept as ALL_'//
     -                STR1(1:NC1)//'.'
            ELSE
                 PRINT *,' !!!!!! DRFTIM WARNING : Arrival time'//
     -                ' histogram of all electrons for wire '//
     -                STR2(1:NC2)//' has not been saved.'
                 CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL1)
            ENDIF
            DO 401 I=1,KELEC
            JSEL=JSEL+1
            CALL OUTFMT(REAL(JSEL),2,STR1,NC1,'LEFT')
            CALL HISSAV(IRFTEL(I),'SEL_'//STR1(1:NC1),IFAIL1)
            CALL OUTFMT(REAL(MELEC(I)),2,STR2,NC2,'LEFT')
            CALL OUTFMT(REAL(IW),2,STR3,NC3,'LEFT')
            IF(IFAIL1.EQ.0)THEN
                 PRINT *,' ------ DRFTIM MESSAGE : Arrival time'//
     -                ' histogram of electron '//STR2(1:NC2)//
     -                ' for wire '//STR3(1:NC3)//' is kept as SEL_'//
     -                STR1(1:NC1)//'.'
            ELSE
                 PRINT *,' !!!!!! DRFTIM WARNING : Arrival time'//
     -                ' histogram of electron '//STR2(1:NC2)//
     -                ' for wire '//STR3(1:NC3)//' has not been saved.'
                 CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL1)
            ENDIF
401         CONTINUE
       ELSE
            CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL1)
            DO 403 I=1,KELEC
            CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL1)
403         CONTINUE
       ENDIF
*** Proceed with the next wire.
100    CONTINUE
*** End of progress printing.
       CALL PROEND
*** Register the amount of CPU time used by this routine.
       CALL TIMLOG('Calculating arrival times:              ')
       END
