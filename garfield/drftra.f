CDECK  ID>, DRFTRA.
       SUBROUTINE DRFTRA(Q,ITYPE,TSTEP,LDRMC,LEQTPL,LEQREV,
     -      LLINPL,LLINPR,
     -      LTIMPL,LVELPL,LDIFPL,LAVAPL,LFUNPL,FUNCT,NCF,MARKER)
*-----------------------------------------------------------------------
*   DRFTRA - Subroutine calculating and plotting drift lines given an
*            electric field, it optionally plots some isochronous lines
*            and graphs of the drift-time, velocity, diffusion and the
*            Townsend coefficient. Calculation starts from the track.
*   VARIABLES : ISTVEC      : Vector of status codes.
*               TIMVEC      : Vector of drift times.
*               VELVEC      : Vector of the average drift velocity.
*               DIFVEC      : Vector of the integrated diffusion.
*               AVAVEC      : Vector of multiplication factor.
*               DRLENG      : The length of the current drift line.
*               logicals    : inherited from DRFDRF, see there.
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       DOUBLE PRECISION TRANSF,DRLENG,XAUX1,XAUX2,YAUX1,YAUX2
       CHARACTER*133 STRING
       CHARACTER*80 FUNCT,STASTR
       CHARACTER*30 AUXSTR
       CHARACTER*10 VARLIS(MXVAR)
       LOGICAL LEQTPL,LLINPL,LLINPR,LTIMPL,LVELPL,LDIFPL,LAVAPL,LFUNPL,
     -      MARKER,USE(MXVAR),DONE,LDRMC,LEQREV
       REAL TIMVEC(MXLIST),VELVEC(MXLIST),VAR(MXVAR),
     -      DIFVEC(MXLIST),FUNVEC(MXLIST),AVAVEC(MXLIST),ATTVEC(MXLIST),
     -      POSVEC(MXLIST),
     -      RES(1),XSTART,YSTART,ZSTART,ESTART,XT0P,YT0P,XT1P,YT1P,
     -      TSTEP,Q,XR0,YR0,XR1,YR1,VXMIN,VYMIN,VXMAX,VYMAX,
     -      XT0AUX,YT0AUX,ZT0AUX,XT1AUX,YT1AUX,ZT1AUX,EXTRA1
       INTEGER ISTVEC(MXLIST),MODVAR(MXVAR),MODRES(1),I,J,IPAIR,NPAIR,
     -      NCSTAT,NRES,IENTRY,NCF,ITYPE,IFAIL,IU,NCAUX,NMAX
       SAVE VARLIS
*** Initialise the VARLIS list of function variables.
       DATA (VARLIS(I),I=1,11)/
     -      'LENGTH    ','TIME      ','DIFFUSION ','AVALANCHE ',
     -      'LOSS      ','X_END     ','Y_END     ','Z_END     ',
     -      'X_START   ','Y_START   ','Z_START   '/
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DRFTRA ///'
*** Perform preliminary checks, make sure the track has been set.
       IF(.NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! DRFTRA WARNING : The track has not been'//
     -           ' set ; the plot is not made.'
            RETURN
       ENDIF
*   The track should be in the drift area.
       IF(POLAR)THEN
            IFAIL=0
            CALL CFMCTR(XT0,YT0,XR0,YR0,1)
            CALL CFMCTR(XT1,YT1,XR1,YR1,1)
            IF(XR0.LT.DXMIN.OR.XR0.GT.DXMAX.OR.
     -           XR1.LT.DXMIN.OR.XR1.GT.DXMAX.OR.
     -           YR0.LT.DYMIN.OR.YR0.GT.DYMAX.OR.
     -           YR1.LT.DYMIN.OR.YR1.GT.DYMAX)IFAIL=1
       ELSE
            XT0AUX=XT0
            YT0AUX=YT0
            ZT0AUX=ZT0
            XT1AUX=XT1
            YT1AUX=YT1
            ZT1AUX=ZT1
            CALL CLIP3(XT0AUX,YT0AUX,ZT0AUX,XT1AUX,YT1AUX,ZT1AUX,
     -           DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,IFAIL)
       ENDIF
       IF(IFAIL.NE.0.AND.POLAR)THEN
            PRINT *,' !!!!!! DRFTRA WARNING : The track lies at',
     -           ' least partialy outside the drift area ;',
     -           ' no drift lines.'
            RETURN
       ELSEIF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DRFTRA WARNING : The track is not',
     -           ' lying at least partially in the drift',
     -           ' area or has length 0 ; no drift lines.'
            RETURN
       ENDIF
*** Initialise the output vectores.
       DO 10 I=1,MXLIST
       ISTVEC(I)=0
       TIMVEC(I)=0.0
       VELVEC(I)=0.0
       DIFVEC(I)=0.0
       AVAVEC(I)=0.0
       ATTVEC(I)=0.0
       FUNVEC(I)=0.0
       POSVEC(I)=0.0
10     CONTINUE
*** Translate the function if requested.
       IF(LFUNPL)THEN
            IF(POLAR)THEN
                 VARLIS(6)='R_END'
                 VARLIS(7)='PHI_END'
                 VARLIS(9)='R_START'
                 VARLIS(10)='PHI_START'
            ELSE
                 VARLIS(6)='X_END'
                 VARLIS(7)='Y_END'
                 VARLIS(9)='X_START'
                 VARLIS(10)='Y_START'
            ENDIF
            IF(INDEX(FUNCT(1:NCF),'@').NE.0)THEN
                 NRES=1
                 CALL ALGEDT(VARLIS,11,IENTRY,USE,NRES)
            ELSE
                 CALL ALGPRE(FUNCT,NCF,VARLIS,11,NRES,USE,IENTRY,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! DRFTRA WARNING : The function '//
     -                     FUNCT(1:NCF)//' can not be used because'//
     -                     ' of the syntax errors (see above).'
                      CALL ALGCLR(IENTRY)
                      RETURN
                 ELSEIF(((USE(1).OR.USE(2)).AND..NOT.GASOK(1)).OR.
     -                (USE(3).AND..NOT.GASOK(3)).OR.
     -                (USE(4).AND..NOT.GASOK(4)).OR.
     -                (USE(5).AND..NOT.GASOK(6)))THEN
                      PRINT *,' !!!!!! DRFTRA WARNING : The amount of'//
     -                     ' gas data is insufficient to calculate'//
     -                     ' the function '//FUNCT(1:NCF)
                      CALL ALGCLR(IENTRY)
                      RETURN
                 ENDIF
            ENDIF
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! DRFTRA WARNING : The function '//
     -                FUNCT(1:NCF)//' does not return a single'//
     -                ' result ; rejected.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
       ENDIF
*** Initialise clustering.
       CALL TRACLI
*** Prepare for output: print a heading if printing is requested.
       IF(LLINPR)THEN
            XT0P=XT0
            YT0P=YT0
            XT1P=XT1
            YT1P=YT1
            IF(POLAR)CALL CFMCTP(XT0P,YT0P,XT0P,YT0P,1)
            IF(POLAR)CALL CFMCTP(XT1P,YT1P,XT1P,YT1P,1)
            WRITE(LUNOUT,'(''1 Track drift line plot :'',/,
     -           ''  ======================='',//)')
            IF(ITYPE.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Drifting'',
     -                '' electrons with charge '',F4.1)') Q
            ELSE
                 WRITE(LUNOUT,'(''  Drifting'',
     -                '' ions with charge '',F4.1)') Q
            ENDIF
            WRITE(LUNOUT,'(''  The particle begins at ('',F10.3,'','',
     -           F10.3,'','',F10.3,'')''/
     -           ''  and goes towards       ('',F10.3,'','',F10.3,'','',
     -           F10.3,'')''//)') XT0P,YT0P,ZT0,XT1P,YT1P,ZT1
            IF(.NOT.POLAR)THEN
                 WRITE(LUNOUT,'(''     x-start     y-start'',
     -                ''     z-start  Drift time  Mean speed'',
     -                ''   Diffusion   Avalanche'',
     -                ''  Status information'')')
                 WRITE(LUNOUT,'(''        [cm]        [cm]'',
     -                ''        [cm]     [musec]  [cm/musec]'',
     -                ''     [musec]   [numeric]'',//)')
            ELSE
                 WRITE(LUNOUT,'(''     r-start   phi-start'',
     -                ''     z-start  Drift time  Mean speed'',
     -                ''   Diffusion   Avalanche'',
     -                ''  Status information'')')
                 WRITE(LUNOUT,'(''        [cm]   [degrees]'',
     -                ''        [cm]     [musec]  [cm/musec]'',
     -                ''     [musec]   [numeric]'',//)')
            ENDIF
        ENDIF
*   Open a plot frame for the drift-lines if plotting is requested.
       IF(LEQTPL.OR.LLINPL)THEN
            IF(ITYPE.EQ.1.AND.Q.LT.0)THEN
                 CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -                'Electron drift lines from a track')
            ELSEIF(ITYPE.EQ.1)THEN
                 CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -                'Positron drift lines from a track')
            ELSEIF(Q.LT.0)THEN
                 CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -                'Drift lines of negative ions from a track')
            ELSE
                 CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -                'Drift lines of positive ions from a track')
            ENDIF
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            IF(PARTID.NE.'Unknown')CALL GRCOMM(3,'Particle: '//PARTID)
            IF(LEQTPL)THEN
                 CALL OUTFMT(TSTEP,2,AUXSTR,NCAUX,'LEFT')
                 CALL GRCOMM(4,'Isochron interval: '//AUXSTR(1:NCAUX)//
     -                ' [microsec]')
                 CALL DRFEQR
            ENDIF
            CALL GRALOG('Track drift line plot.                  ')
       ENDIF
*** Start drift lines from the track.
       I=0
20     CONTINUE
*   Generate a cluster.
       CALL TRACLS(XSTART,YSTART,ZSTART,ESTART,NPAIR,EXTRA1,DONE,IFAIL)
       IF(DONE)GOTO 40
*   Loop over the electrons.
       IF(LDRMC)THEN
            NMAX=NPAIR
       ELSE
            NMAX=1
       ENDIF
       DO 50 IPAIR=1,NMAX
*   Convert position to internal in polar cells.
       IF(POLAR)CALL CFMCTR(XSTART,YSTART,XSTART,YSTART,1)
*   Calculate the drift line starting at (XSTART,YSTART)
       IF(LDRMC)THEN
            CALL DLCMC(XSTART,YSTART,ZSTART,Q,ITYPE)
       ELSE
            CALL DLCALC(XSTART,YSTART,ZSTART,Q,ITYPE)
       ENDIF
*   Skip if the line has no steps.
       IF(NU.LE.0.OR.NU.GT.MXLIST)THEN
            PRINT *,' !!!!!! DRFTRA WARNING : Drift line has no'//
     -           ' steps or more than MXLIST steps; skipped.'
            GOTO 20
       ENDIF
*   Increment track counter.
       IF(I.GE.MXLIST)THEN
            PRINT *,' !!!!!! DRFTRA WARNING : Maximum number of'//
     -           ' electrons reached ; rest is skipped.'
            GOTO 40
       ELSE
            I=I+1
       ENDIF
*   Convert position to cartesian in polar cells.
       IF(POLAR)CALL CFMRTC(XSTART,YSTART,XSTART,YSTART,1)
*   To be able to store the coordinate.
       POSVEC(I)=SQRT((XSTART-XT0)**2+(YSTART-YT0)**2+(ZSTART-ZT0)**2)
*   And convert position to polar in polar cells for printing.
       IF(POLAR)CALL CFMCTP(XSTART,YSTART,XSTART,YSTART,1)
**  Calculate and store the derived information for the graphs.
       ISTVEC(I)=ISTAT
       TIMVEC(I)=TU(NU)
       IF(LVELPL.OR.(LFUNPL.AND.USE(1)))THEN
            DRLENG=0.0
            DO 30 IU=2,NU
            IF(POLAR)THEN
                 CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX1,YAUX1,1)
                 CALL CF2RTC(XU(IU),YU(IU),XAUX2,YAUX2,1)
                 DRLENG=DRLENG+SQRT((XAUX2-XAUX1)**2+
     -                (YAUX2-YAUX1)**2+(ZU(IU)-ZU(IU-1))**2)
            ELSE
                 DRLENG=DRLENG+SQRT((XU(IU)-XU(IU-1))**2+
     -                (YU(IU)-YU(IU-1))**2+(ZU(IU)-ZU(IU-1))**2)
            ENDIF
30          CONTINUE
            IF(TU(NU).GT.0.0)THEN
                 VELVEC(I)=REAL(DRLENG/TU(NU))
            ELSE
                 VELVEC(I)=0.0
            ENDIF
       ENDIF
       IF(GASOK(3).AND.(LDIFPL.OR.(LFUNPL.AND.USE(3))))
     -      CALL DLCDIF(DIFVEC(I))
       IF(GASOK(4).AND.(LAVAPL.OR.(LFUNPL.AND.USE(4))))
     -      CALL DLCTWN(AVAVEC(I))
       IF(GASOK(6).AND.LFUNPL.AND.USE(5))CALL DLCATT(ATTVEC(I))
       IF(LFUNPL)THEN
            VAR(1)=DRLENG
            VAR(2)=REAL(TU(NU))
            VAR(3)=DIFVEC(I)
            VAR(4)=AVAVEC(I)
            VAR(5)=ATTVEC(I)
            IF(POLAR)THEN
                 CALL CF2RTP(XU(1),YU(1),XAUX1,YAUX1,1)
                 VAR(9)=REAL(XAUX1)
                 VAR(10)=REAL(YAUX1)
                 CALL CF2RTP(XU(NU),YU(NU),XAUX1,YAUX1,1)
                 VAR(6)=REAL(XAUX1)
                 VAR(7)=REAL(YAUX1)
            ELSE
                 VAR(9)=REAL(XU(1))
                 VAR(10)=REAL(YU(1))
                 VAR(6)=REAL(XU(NU))
                 VAR(7)=REAL(YU(NU))
            ENDIF
            VAR(11)=REAL(ZU(1))
            VAR(8)=REAL(ZU(NU))
            MODVAR(1)=2
            MODVAR(2)=2
            MODVAR(3)=2
            MODVAR(4)=2
            MODVAR(5)=2
            MODVAR(6)=2
            MODVAR(7)=2
            MODVAR(8)=2
            MODVAR(9)=2
            MODVAR(10)=2
            MODVAR(11)=2
            CALL ALGEXE(IENTRY,VAR,MODVAR,11,RES,MODRES,1,IFAIL)
            IF(MODRES(1).NE.2)THEN
                 PRINT *,' !!!!!! DRFTRA WARNING : Function does not'//
     -                ' return a number; set to 0.'
                 FUNVEC(I)=0
            ELSE
                 FUNVEC(I)=RES(1)
            ENDIF
       ENDIF
*   Print information on this drift line if requested.
       IF(LLINPR)THEN
            CALL DLCSTF(ISTAT,STASTR,NCSTAT)
            WRITE(STRING,'(2X,7(E10.3,2X),A)')
     -           XSTART,YSTART,ZSTART,
     -           TIMVEC(I),VELVEC(I),DIFVEC(I),AVAVEC(I),
     -           STASTR(1:MIN(45,NCSTAT))
            IF(.NOT.LVELPL)STRING(49:60)=' unavailable'
            IF(.NOT.GASOK(3).OR..NOT.LDIFPL)
     -           STRING(61:72)=' unavailable'
            IF(.NOT.GASOK(4).OR..NOT.LAVAPL)
     -           STRING(73:84)=' unavailable'
            WRITE(LUNOUT,'(A133)') STRING
       ENDIF
*   Plot the drift line obtained - if this is requested.
       IF(LLINPL)CALL DLCPLT
*** Invert TU in order to obtain the time-distance from the sense wire.
       IF(LEQREV.AND.LEQTPL)THEN
            DO 80 J=1,NU
            TU(J)=TU(NU)-TU(J)
80          CONTINUE
*   Reverse XU, YU and TU so that they can be treated as plot vectors.
            DO 90 J=1,INT(NU/2.0)
            TRANSF=TU(J)
            TU(J)=TU(NU-J+1)
            TU(NU-J+1)=TRANSF
            TRANSF=XU(J)
            XU(J)=XU(NU-J+1)
            XU(NU-J+1)=TRANSF
            TRANSF=YU(J)
            YU(J)=YU(NU-J+1)
            YU(NU-J+1)=TRANSF
            TRANSF=ZU(J)
            ZU(J)=ZU(NU-J+1)
            ZU(NU-J+1)=TRANSF
90          CONTINUE
*   Add to the equal time contour table, select appropriate drift-lines.
            IF((ISTAT.GE.-15.AND.ISTAT.LE.-11).OR.
     -           (ISTAT.GE.1.AND.ISTAT.LE.NWIRE).OR.
     -           (ISTAT.GE.2*MXWIRE+1.AND.ISTAT.LE.2*MXWIRE+NSOLID))
     -           CALL DRFEQT(TSTEP,ISTAT)
       ELSEIF(LEQTPL)THEN
            CALL DRFEQT(TSTEP,-20)
       ENDIF
*   Next electron in the cluster.
50     CONTINUE
*   Next cluster.
       GOTO 20
*   Last cluster processed.
40     CONTINUE
*   Plot the track.
       IF(LEQTPL.OR.LLINPL)CALL TRAPLT
*** Register the amount of CPU time used for calculating drift lines.
       CALL TIMLOG('Making a track drift-line plot:         ')
*** Algebra stuff.
       IF(LFUNPL)THEN
             CALL ALGERR
             CALL ALGCLR(IENTRY)
       ENDIF
*** Plot the equal time contours, if requested.
       IF(LEQTPL)CALL DRFEQP
*   Clear the screen if at least a plot has been made.
       IF(LEQTPL.OR.LLINPL)CALL GRNEXT
*   Print any error messages accumulated by DRFEQT.
       IF(LEQTPL)CALL DRFEQE
*** Plot the various graphs as requested, first the drift time plot.
       IF(LTIMPL)CALL DRFTR2(TIMVEC,POSVEC,ISTVEC,I,MARKER,
     -      'Drift time [microsec]','Drift time')
       IF(LTIMPL)
     -      CALL GRALOG('Graph of the drift-time                 ')
*   Next the average drift velocity plot.
       IF(LVELPL)CALL DRFTR2(VELVEC,POSVEC,ISTVEC,I,MARKER,
     -      'Mean drift speed [cm/microsec]',
     -      'Average drift speed')
       IF(LVELPL)
     -      CALL GRALOG('Graph of the average drift-velocity     ')
*   diffusion plot ...
       IF(LDIFPL)CALL DRFTR2(DIFVEC,POSVEC,ISTVEC,I,MARKER,
     -      'Integrated diffusion [microsec]',
     -      'Integrated diffusion')
       IF(LDIFPL)
     -      CALL GRALOG('Graph of the integrated diffusion       ')
*   the multiplication plot ...
       IF(LAVAPL)CALL DRFTR2(AVAVEC,POSVEC,ISTVEC,I,MARKER,
     -      'Multiplication Factor [numeric]',
     -      'Multiplication factor')
       IF(LAVAPL)
     -      CALL GRALOG('Graph of the multiplication factor      ')
*   and the function plot.
       IF(LFUNPL)THEN
            STRING=' '
            STRING(1:40)='Graph of '//FUNCT(1:MIN(31,NCF))
            STRING(81-MIN(40,NCF):80)=FUNCT(1:MIN(40,NCF))
            CALL DRFTR2(FUNVEC,POSVEC,ISTVEC,I,MARKER,
     -           STRING(41:80),STRING(1:40))
            STRING=' '
            STRING(1:40)='Graph of '//FUNCT(1:MIN(31,NCF))
            CALL GRALOG(STRING(1:40))
       ENDIF
**  Register the amount of CPU time used for plotting these curves.
       CALL TIMLOG('Plotting various drift related graphs:  ')
       END
