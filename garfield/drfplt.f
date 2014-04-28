CDECK  ID>, DRFPLT.
       SUBROUTINE DRFPLT
*-----------------------------------------------------------------------
*   DRFPLT - Subroutine plotting the electric field, the magnetic field
*            and the potential in a variety of ways: histograms, contour
*            plots, vector plots and surface plots.
*   Variables : XPL,YPL    : Used for plotting lines
*               FUNCT.     : Stores the function text the plots
*               VAR        : Array of input values for ALGEXE
*               GRID       : Array of 'hights' for surface plots
*               COORD      : Contains the ordinate of the graph data
*               VALUE      : Contains the function values of the graph
*               HIST       : Stores the histogram
*               CMIN,CMAX  : Range of contour heights
*               HMIN,HMAX  : Range in the histogram
*               NCHA       : Number of bins in the histogram.
*               FLAG       : Logicals used for parsing the command
*               LHIST ...  : Determines whether the plot will be made
*               PHI,THETA  : Viewing angle for 3-dimensional plots.
*   (Last changed on 12/ 2/99.)
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
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       DOUBLE PRECISION F0(3),XPOS1,YPOS1,XPOS2,YPOS2
       REAL COORD(MXLIST),VALUE(MXLIST),RES(5),VAR(MXVAR),
     -      HMIN,HMAX,GRSMIN,GRSMAX,RT0,RT1,PT0,PT1,XPOS,YPOS,ZPOS,
     -      FACNRM,VOLT,CMIN,CMAX,QPLT,THETA,PHI,GMINR,GMAXR,
     -      HMINR,HMAXR,CMINR,CMAXR,XXPOS,YYPOS,VXMIN,VYMIN,VXMAX,VYMAX
       INTEGER NCHA,NCONT,NGRPNT,MODVAR(MXVAR),MODRES(5),NCTOT,ILOC,
     -      ISURF,IVECT1,IVECT2,IVECT3,IHIST,IFLAT,ICHK,JCHK,IHISRF,
     -      NREXP,I,J,
     -      INEXT,NWORD,IFAIL1,IFAIL2,NPNTR,NC1,NC2,NC3,NC4,NC5,II,
     -      INPCMP,NCFTRA,ITYPE,IFAIL,IENTRA,ICOORD,NCHAR,NRES,NCAUX,
     -      NCONTR,NCONTP,IENTRY,NCAUX1,NCAUX2,NCAUX3,NCAUX4
       CHARACTER*(MXCHAR) STRING,FUNCT1,FUNCT2,FUNCT3,FUNCT4,FUNCT5,
     -      FUNTRA
       CHARACTER*20 AUX1,AUX2,AUX3,AUX4
       CHARACTER*10 VARLIS(MXVAR)
       LOGICAL USE(MXVAR),FLAG(MXWORD+5),
     -      EVALE,EVALB,EVALV,EVALT,EVALD,EVALA,EVALL,EVALP,
     -      LHIST,LVECT,LGRAPH,LCONT,LSURF,CAUTO,HAUTO,CLAB,LGRPRT,
     -      LMCDR
       EXTERNAL INPCMP,DCONT
       COMMON /CN2DAT/ IENTRY,EVALE,EVALB,EVALV,EVALT,EVALD,EVALA,EVALL,
     -      EVALP,QPLT,ITYPE,LMCDR
       REAL WS,PAR,DUM,SMIN,SMAX
       COMMON /MATRIX/ WS(MXWIRE,MXWIRE),PAR(37),
     -      DUM(MXWIRE**2+8*MXWIRE-31)
       SAVE VARLIS,HMIN,HMAX,NCHA,NCONT,NGRPNT,PHI,THETA,LGRPRT
       DATA (VARLIS(I),I=1,24) /'X         ','Y         ','EX        ',
     -             'EY        ','EZ        ','E         ','BX        ',
     -             'BY        ','BZ        ','B         ','VDX       ',
     -             'VDY       ','VDZ       ','VD        ','LORENTZ   ',
     -             'TIME      ','PATH      ','DIFFUSION ','AVALANCHE ',
     -             'LOSS      ','STATUS    ','P         ','Z         ',
     -             'T         '/
       DATA HMIN,HMAX /0.0,10000.0/
       DATA NCONT/21/
       DATA NGRPNT/MXLIST/,LGRPRT/.FALSE./
       DATA NCHA/100/
       DATA PHI,THETA/30.0,60.0/
*** Define an output format.
1010   FORMAT(26X,A10,L2,3X,A20,2X,I2,2(2X,E10.3),2(2X,I6),2(2X,E10.3))
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE DRFPLT ///'
*** Preset the options, function strings etc,
       FUNCT1=' '
       FUNCT2=' '
       FUNCT3=' '
       FUNCT4=' '
       FUNCT5=' '
       LGRAPH=.FALSE.
       LSURF=.FALSE.
       LVECT=.FALSE.
       LHIST=.FALSE.
       LCONT=.FALSE.
       FUNTRA='?'
       NCFTRA=1
       CMIN=0.0
       CMAX=10000.0
       CAUTO=.TRUE.
       CLAB=.TRUE.
       HAUTO=.TRUE.
       GRSMIN=1
       GRSMAX=-1
       LMCDR=.FALSE.
*** Drift line options.
       QPLT=-1.0
       ITYPE=1
*** Make sure the variables have appropriate names
       IF(POLAR)THEN
            VARLIS(1)='R         '
            VARLIS(2)='PHI       '
            VARLIS(3)='ER        '
            VARLIS(4)='EPHI      '
            VARLIS(7)='BR        '
            VARLIS(8)='BPHI      '
            VARLIS(11)='VDR       '
            VARLIS(12)='VDPHI     '
       ELSE
            VARLIS(1)='X         '
            VARLIS(2)='Y         '
            VARLIS(3)='EX        '
            VARLIS(4)='EY        '
            VARLIS(7)='BX        '
            VARLIS(8)='BY        '
            VARLIS(11)='VDX       '
            VARLIS(12)='VDY       '
       ENDIF
*** Examine the input, first step is finding out where the keywords are.
       CALL INPNUM(NWORD)
       DO 10 I=1,MXWORD+5
       IF(I.EQ.1.OR.I.GT.NWORD)THEN
            FLAG(I)=.TRUE.
       ELSEIF(INPCMP(I,'A#NGLES')+INPCMP(I,'AUTO#MATIC')+
     -      INPCMP(I,'BI#NS')+INPCMP(I,'SC#ALE')+
     -      INPCMP(I,'PR#INT')+INPCMP(I,'NOPR#INT')+
     -      INPCMP(I,'C#ONTOUR')+INPCMP(I,'G#RAPH')+
     -      INPCMP(I,'H#ISTOGRAM')+INPCMP(I,'N')+
     -      INPCMP(I,'LAB#ELS')+INPCMP(I,'NOLAB#ELS')+
     -      INPCMP(I,'RA#NGE')+INPCMP(I,'S#URFACE')+
     -      INPCMP(I,'VE#CTOR')+INPCMP(I,'ON')+
     -      INPCMP(I,'EL#ECTRON')+INPCMP(I,'ION')+
     -      INPCMP(I,'POS#ITIVE')+INPCMP(I,'NEG#ATIVE')+
     -      INPCMP(I,'M#ONTE-C#ARLO-#DRIFT-#LINES')+
     -      INPCMP(I,'MC-#DRIFT-#LINES')+
     -      INPCMP(I,'NOM#ONTE-C#ARLO-#DRIFT-#LINES')+
     -      INPCMP(I,'NOMC-#DRIFT-#LINES')+
     -      INPCMP(I,'RUN#GE-K#UTTA-#DRIFT-#LINES')+
     -      INPCMP(I,'RKF-#DRIFT-#LINES').NE.0)THEN
            FLAG(I)=.TRUE.
       ELSE
            FLAG(I)=.FALSE.
       ENDIF
10     CONTINUE
*** Start a loop over the list,
       INEXT=1
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*   warn if the user uses a sub-keyword out of context.
       IF(INPCMP(I,'RA#NGE')+INPCMP(I,'N')+INPCMP(I,'BI#NS')+
     -      INPCMP(I,'LAB#ELS')+INPCMP(I,'NOLAB#ELS')+
     -      INPCMP(I,'A#NGLES')+INPCMP(I,'AUTO#MATIC')+
     -      INPCMP(I,'PR#INT')+INPCMP(I,'NOPR#INT')+
     -      INPCMP(I,'ON')+INPCMP(I,'SC#ALE').NE.0)THEN
            CALL INPMSG(I,'Valid option out of context.  ')
            IF(.NOT.FLAG(I+1))THEN
                 CALL INPMSG(I+1,'See the previous message.     ')
                 INEXT=I+2
                 IF(.NOT.FLAG(I+2))THEN
                      CALL INPMSG(I+2,'See the previous messages.    ')
                      INEXT=I+3
                 ENDIF
            ENDIF
*   warn if an unknown keywords appear,
       ELSEIF(.NOT.FLAG(I))THEN
            CALL INPMSG(I,'Item is not a known keyword.  ')
**  Find out whether a GRAPH is requested next,
       ELSEIF(INPCMP(I,'G#RAPH').NE.0)THEN
*   Plot already requested ?
            IF(LGRAPH)PRINT *,' !!!!!! DRFPLT WARNING : Only one'//
     -           ' graph per PLOT statement can be processed.'
            LGRAPH=.TRUE.
*   Store the function string.
            IF(FLAG(I+1))THEN
                 FUNCT1(1:2)='VD'
                 NC1=2
                 INEXT=I+1
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NC1)
                 FUNCT1(1:NC1)=STRING(1:NC1)
                 INEXT=I+2
            ENDIF
*   Look for sub-keywords with GRAPH.
            DO 230 II=I,NWORD
            IF(II.LT.INEXT)GOTO 230
*   Look for the subkeyword ON.
            IF(INPCMP(II,'ON').NE.0)THEN
                 IF(FLAG(II+1))THEN
                      CALL INPMSG(II,'The curve function is absent. ')
                 ELSE
                      CALL INPSTR(II+1,II+1,FUNTRA,NCFTRA)
                      INEXT=II+2
                 ENDIF
*   Look for the subkeyword N.
            ELSEIF(INPCMP(II,'N').NE.0)THEN
                 IF(FLAG(II+1))THEN
                      CALL INPMSG(II,'number of points is missing.  ')
                 ELSE
                      CALL INPCHK(II+1,1,IFAIL1)
                      CALL INPRDI(II+1,NPNTR,NGRPNT)
                      IF(NPNTR.LT.2.AND.IFAIL1.EQ.0)CALL INPMSG(II+1,
     -                     'number of point less than 2.  ')
                      IF(NPNTR.GT.MXLIST.AND.IFAIL1.EQ.0)CALL INPMSG
     -                     (II+1,'number of points > MXLIST.    ')
                      IF(NPNTR.GE.2.AND.NPNTR.LE.MXLIST)NGRPNT=NPNTR
                      INEXT=II+2
                 ENDIF
*   Look for print options.
            ELSEIF(INPCMP(II,'PR#INT').NE.0)THEN
                 LGRPRT=.TRUE.
                 INEXT=II+1
            ELSEIF(INPCMP(II,'NOPR#INT').NE.0)THEN
                 LGRPRT=.FALSE.
                 INEXT=II+1
*   Scale of the graph.
            ELSEIF(INPCMP(II,'SC#ALE')+INPCMP(II,'RA#NGE').NE.0)THEN
                 IF(FLAG(II+1).OR.FLAG(II+2))THEN
                      CALL INPMSG(II,'the arguments are missing.    ')
                 ELSE
                      CALL INPCHK(II+1,2,IFAIL1)
                      CALL INPRDR(II+1,GMINR,+1.0)
                      CALL INPCHK(II+2,2,IFAIL2)
                      CALL INPRDR(II+2,GMAXR,-1.0)
                      IF(GMINR.EQ.GMAXR)THEN
                           CALL INPMSG(II+1,'zero range in the')
                           CALL INPMSG(II+2,'scale not permitted')
                      ELSE
                           GRSMIN=MIN(GMINR,GMAXR)
                           GRSMAX=MAX(GMINR,GMAXR)
                      ENDIF
                      INEXT=II+3
                 ENDIF
*   Otherwise skip to the next keyword.
            ELSE
                 GOTO 20
            ENDIF
230         CONTINUE
**  Find out whether a CONTOUR plot is requested next,
       ELSEIF(INPCMP(I,'C#ONTOUR').NE.0)THEN
            IF(LCONT)PRINT *,' !!!!!! DRFPLT WARNING : Only one'//
     -           ' contour plot per PLOT statement can be processed.'
            LCONT=.TRUE.
*   Store the function string, using the default if absent.
            IF(FLAG(I+1))THEN
                 FUNCT2(1:2)='VD'
                 NC2=2
                 INEXT=I+1
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NC2)
                 FUNCT2(1:NC2)=STRING(1:NC2)
                 INEXT=I+2
            ENDIF
*   Set default values for the range, depending on the function.
            CMIN=0.0
            CMAX=10000.0
*   Look for sub-keywords with CONTOUR.
            DO 210 II=I+1,NWORD
            IF(II.LT.INEXT)GOTO 210
*   LABELing of the contours.
            IF(INPCMP(II,'LAB#ELS').NE.0)THEN
                 CLAB=.TRUE.
                 INEXT=II+1
            ELSEIF(INPCMP(II,'NOLAB#ELS').NE.0)THEN
                 CLAB=.FALSE.
                 INEXT=II+1
*   The RANGE subkeyword.
            ELSEIF(INPCMP(II,'RA#NGE').NE.0)THEN
                 IF(INPCMP(II+1,'AUTO#MATIC').NE.0)THEN
                      CMIN=0.0
                      CMAX=0.0
                      CAUTO=.TRUE.
                      INEXT=II+2
                 ELSEIF((.NOT.FLAG(II+1)).AND.FLAG(II+2))THEN
                      CALL INPCHK(II+1,2,IFAIL1)
                      CALL INPRDR(II+1,CMINR,CMIN)
                      CMIN=CMINR
                      CMAX=CMINR
                      CAUTO=.FALSE.
                      INEXT=II+2
                 ELSEIF((.NOT.FLAG(II+1)).AND.(.NOT.FLAG(II+2)))THEN
                      CALL INPCHK(II+1,2,IFAIL1)
                      CALL INPCHK(II+2,2,IFAIL2)
                      CALL INPRDR(II+1,CMINR,CMIN)
                      CALL INPRDR(II+2,CMAXR,CMAX)
                      CMIN=MIN(CMINR,CMAXR)
                      CMAX=MAX(CMINR,CMAXR)
                      CAUTO=.FALSE.
                      INEXT=II+3
                 ELSE
                      CALL INPMSG(II,'RANGE takes two arguments.    ')
                      IF(FLAG(II+1))THEN
                           INEXT=II+1
                      ELSE
                           CALL INPMSG(II+1,
     -                          'Ignored, see previous message.')
                           INEXT=II+2
                      ENDIF
                 ENDIF
*   Sub keyword N.
            ELSEIF(INPCMP(II,'N').NE.0)THEN
                 IF(FLAG(II+1))THEN
                      CALL INPMSG(II,'N must have an argument.      ')
                      INEXT=II+1
                 ELSE
                      CALL INPCHK(II+1,1,IFAIL1)
                      CALL INPRDI(II+1,NCONTR,NCONT)
                      IF(NCONTR.LT.0.AND.IFAIL1.EQ.0)CALL INPMSG(II+1,
     -                     'number of contour steps is < 0')
                      IF(NCONTR.GT.MXWIRE.AND.IFAIL1.EQ.0)CALL INPMSG
     -                     (II+1,'may not exceed MXWIRE.        ')
                      IF(NCONTR.GE.0.AND.NCONTR.LE.MXWIRE)NCONT=NCONTR
                      INEXT=II+2
                 ENDIF
*   Otherwise skip to the next keyword.
            ELSE
                 GOTO 20
            ENDIF
210         CONTINUE
**  A SURFACE (3 dimensional plot) has perhaps been requested,
       ELSEIF(INPCMP(I,'S#URFACE').NE.0)THEN
            IF(LSURF)PRINT *,' !!!!!! DRFPLT WARNING : Only one'//
     -           ' surface per PLOT statement can be processed.'
            LSURF=.TRUE.
            IF(FLAG(I+1))THEN
                 FUNCT3(1:2)='VD'
                 NC3=2
                 INEXT=I+1
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NC3)
                 FUNCT3(1:NC3)=STRING(1:NC3)
                 INEXT=I+2
            ENDIF
*   Look for sub-keywords with SURFACE.
            DO 220 II=I,NWORD
            IF(II.LT.INEXT)GOTO 220
*   Look for the subkeyword ANGLE.
            IF(INPCMP(II,'A#NGLES').NE.0)THEN
                 IF(.NOT.FLAG(II+1).AND.FLAG(II+2))THEN
                      CALL INPMSG(II,'ANGLE has 2 args: theta, phi. ')
                      CALL INPMSG(II+1,'See the previous message.     ')
                      INEXT=II+2
                 ELSEIF(FLAG(II+1))THEN
                      CALL INPMSG(II,'ANGLE has 2 args: theta, phi. ')
                      INEXT=II+1
                 ELSE
                      CALL INPCHK(II+1,2,IFAIL1)
                      CALL INPRDR(II+1,PHI,30.0)
                      CALL INPCHK(II+2,2,IFAIL1)
                      CALL INPRDR(II+2,THETA,60.0)
                      INEXT=II+3
                 ENDIF
*   Otherwise skip to the next keyword.
            ELSE
                 GOTO 20
            ENDIF
220         CONTINUE
**  A vector plot.
       ELSEIF(INPCMP(I,'VE#CTOR').NE.0)THEN
            IF(LVECT)PRINT *,' !!!!!! DRFPLT WARNING : Only one'//
     -           ' vector plot per PLOT statement can be processed.'
            LVECT=.TRUE.
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 IF(.NOT.POLAR)THEN
                      FUNCT4(1:11)='VDX,VDY,VDZ'
                      NC4=11
                 ELSE
                      FUNCT4(1:13)='VDR,VDPHI,VDZ'
                      NC4=13
                 ENDIF
                 IF(.NOT.FLAG(I+1).AND.FLAG(I+2))THEN
                      CALL INPSTR(I+1,I+1,STRING,NCAUX)
                      IF(INDEX(STRING(1:NCAUX),'@').NE.0)THEN
                           FUNCT4(1:1)='@'
                           NC4=1
                      ELSE
                           CALL INPMSG(I+1,
     -                          'Has 2 or 3 args, default used.')
                      ENDIF
                      INEXT=I+2
                 ELSE
                      INEXT=I+1
                 ENDIF
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NC4)
                 FUNCT4(1:NC4+1)=STRING(1:NC4)//','
                 CALL INPSTR(I+2,I+2,STRING,NCAUX)
                 FUNCT4(NC4+2:NC4+NCAUX+2)=STRING(1:NCAUX)//','
                 NC4=NC4+NCAUX+2
                 IF(.NOT.FLAG(I+3))THEN
                      CALL INPSTR(I+3,I+3,STRING,NCAUX)
                      FUNCT4(NC4+1:NC4+NCAUX)=STRING(1:NCAUX)
                      NC4=NC4+NCAUX
                      INEXT=I+4
                 ELSE
                      FUNCT4(NC4+1:NC4+1)='0'
                      NC4=NC4+1
                      INEXT=I+3
                 ENDIF
            ENDIF
**  Finally, find out whether the next plot is a HISTOGRAM.
       ELSEIF(INPCMP(I,'H#ISTOGRAM').NE.0)THEN
            IF(LHIST)PRINT *,' !!!!!! DRFPLT WARNING : Only one'//
     -           ' histogram per PLOT statement can be processed.'
            LHIST=.TRUE.
            IF(FLAG(I+1))THEN
                 FUNCT5(1:2)='VD'
                 NC5=2
                 HMIN=0.0
                 HMAX=10000.0
                 INEXT=I+1
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NC5)
                 FUNCT5(1:NC5)=STRING(1:NC5)
                 INEXT=I+2
            ENDIF
*   Look for subkeywords associated with HISTOGRAM.
            DO 200 II=I,NWORD
            IF(II.LT.INEXT)GOTO 200
*   The RANGE subkeyword.
            IF(INPCMP(II,'RA#NGE').NE.0)THEN
                 IF(INPCMP(II+1,'AUTO#MATIC').NE.0)THEN
                      HMIN=0.0
                      HMAX=0.0
                      HAUTO=.TRUE.
                      INEXT=II+2
                 ELSEIF(.NOT.FLAG(II+1).AND..NOT.FLAG(II+2))THEN
                      CALL INPCHK(II+1,2,IFAIL1)
                      CALL INPCHK(II+2,2,IFAIL2)
                      CALL INPRDR(II+1,HMINR,HMIN)
                      CALL INPRDR(II+2,HMAXR,HMAX)
                      HAUTO=.FALSE.
                      IF(HMINR.EQ.HMAXR)THEN
                           CALL INPMSG(II+1,
     -                          'Zero range not permitted.     ')
                           CALL INPMSG(II+2,
     -                          'See the previous message.     ')
                      ELSE
                           HMIN=MIN(HMINR,HMAXR)
                           HMAX=MAX(HMINR,HMAXR)
                      ENDIF
                      INEXT=II+3
                 ELSE
                      CALL INPMSG(II,'RANGE takes two arguments.    ')
                      IF(FLAG(II+1))THEN
                           INEXT=II+1
                      ELSE
                           CALL INPMSG(II+1,
     -                          'Ignored, see previous message.')
                           INEXT=II+2
                      ENDIF
                 ENDIF
*   The BINS subkeyword.
            ELSEIF(INPCMP(II,'BI#NS').NE.0)THEN
                 IF(FLAG(II+1))THEN
                      CALL INPMSG(II,'This keyword has one argument.')
                      INEXT=II+1
                 ELSE
                      CALL INPCHK(II+1,1,IFAIL)
                      CALL INPRDI(II+1,NCHAR,MXCHA)
                      IF(NCHAR.LE.1.OR.NCHAR.GT.MXCHA)THEN
                           CALL INPMSG(II+1,
     -                          'Inacceptable number of bins.  ')
                      ELSE
                           NCHA=NCHAR
                      ENDIF
                      INEXT=II+2
                 ENDIF
*   Otherwise quit this loop.
            ELSE
                 GOTO 20
            ENDIF
200         CONTINUE
**  Drift parameters.
       ELSEIF(INPCMP(I,'EL#ECTRON').NE.0)THEN
            ITYPE=1
       ELSEIF(INPCMP(I,'ION').NE.0)THEN
            ITYPE=2
       ELSEIF(INPCMP(I,'POS#ITIVE').NE.0)THEN
            QPLT=+1
       ELSEIF(INPCMP(I,'NEG#ATIVE').NE.0)THEN
            QPLT=+1
       ELSEIF(INPCMP(I,'M#ONTE-C#ARLO-#DRIFT-#LINES')+
     -      INPCMP(I,'MC-#DRIFT-#LINES').NE.0)THEN
            LMCDR=.TRUE.
       ELSEIF(INPCMP(I,'NOM#ONTE-C#ARLO-#DRIFT-#LINES')+
     -      INPCMP(I,'NOMC-#DRIFT-#LINES')+
     -      INPCMP(I,'RUN#GE-K#UTTA-#DRIFT-#LINES')+
     -      INPCMP(I,'RKF-#DRIFT-#LINES').NE.0)THEN
            LMCDR=.FALSE.
**  Warn if the user aks for an unknown plot type or makes an error,
       ELSE
            CALL INPMSG(I,'Should have been a plot type. ')
       ENDIF
20     CONTINUE
**  Print error messages.
       CALL INPERR
*** Next print the list of plots if the DEBUG option is on.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(
     -           ''  ++++++ DRFPLT DEBUG   : List of requested plots:''/
     -           ''                          Type      Y/N  '',
     -           ''Function (1:20)       NC  <--------Range------->  '',
     -           ''# cont  # bins  <-------Angle-------->'')')
            IF(LGRAPH)THEN
                 WRITE(LUNOUT,'(26X,A10,L2,3X,A20,1X,I3,34X,I6)')
     -                'Graph     ',LGRAPH,FUNCT1(1:20),NC1,NGRPNT
            ELSE
                 WRITE(LUNOUT,'(26X,A10,L2)') 'Graph     ',LGRAPH
            ENDIF
            IF(LCONT.AND..NOT.CAUTO)THEN
                 WRITE(LUNOUT,
     -                '(26X,A7,3X,L2,3X,A20,1X,I3,2(2X,E10.3),2X,I6)')
     -                'Contour',LCONT,FUNCT2(1:20),NC2,CMIN,CMAX,NCONT
            ELSEIF(LCONT.AND.CAUTO)THEN
                 WRITE(LUNOUT,'(26X,A7,3X,L2,3X,A20,1X,I3,
     -                ''       Automatic scaling'',2X,I6)')
     -                'Contour',LCONT,FUNCT2(1:20),NC2,NCONT
            ELSE
                 WRITE(LUNOUT,'(26X,A10,L2)') 'Contour   ',LCONT
            ENDIF
            IF(LSURF)THEN
                 WRITE(LUNOUT,
     -                '(26X,A10,L2,3X,A20,1X,I3,40X,2(2X,E10.3))')
     -                'Surface   ',LSURF,FUNCT3(1:20),NC3,PHI,THETA
            ELSE
                 WRITE(LUNOUT,'(26X,A10,L2)') 'Surface   ',LSURF
            ENDIF
            IF(LVECT)THEN
                 WRITE(LUNOUT,'(26X,A10,L2,3X,A20,1X,I3)')
     -                'Vector    ',LVECT ,FUNCT4(1:20),NC4
            ELSE
                 PRINT '(26X,A10,L2)','Vector    ',LVECT
            ENDIF
            IF(LHIST.AND..NOT.HAUTO)THEN
                 WRITE(LUNOUT,
     -                '(26X,A10,L2,3X,A20,1X,I3,2(2X,E10.3),10X,I6)')
     -                'Histogram ',LHIST ,FUNCT5(1:20),NC5,
     -                HMIN,HMAX,NCHA
            ELSEIF(LHIST)THEN
                 WRITE(LUNOUT,'(26X,A10,L2,3X,A20,1X,I3,
     -                ''       Automatic scaling'',10X,I6)')
     -                'Histogram ',LHIST ,FUNCT5(1:20),NC5,NCHA
            ELSE
                 WRITE(LUNOUT,'(26X,A10,L2)') 'Histogram   ',LHIST
            ENDIF
            WRITE(LUNOUT,'('' '')')
       ENDIF
*** Take care of the 'GRAPH' type plots, translate curve function.
       IF(LGRAPH.AND.FUNTRA(1:NCFTRA).NE.'?')THEN
            CALL ALGPRE(FUNTRA,NCFTRA,VARLIS(24),1,NRES,USE(24),
     -           IENTRA,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : Graph not made'//
     -                ' because of an error in the track function.'
                 CALL ALGCLR(IENTRA)
                 GOTO 101
            ELSEIF(NRES.NE.3)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : Graph not made'//
     -                ' because the curve does not give 3 results.'
                 CALL ALGCLR(IENTRA)
                 GOTO 101
            ELSEIF(.NOT.USE(24))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : Graph not made'//
     -                ' because the track does not depend on T.'
                 CALL ALGCLR(IENTRA)
                 GOTO 101
            ENDIF
*   If no curve is defined, the track must be.
       ELSEIF(LGRAPH.AND..NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! DRFPLT WARNING : Neither a track nor'//
     -           ' a curve has been defined ; graph not made.'
            GOTO 101
       ENDIF
*   Parameters look a priori acceptable.
       IF(LGRAPH)THEN
*   Transform the function into an instruction list,
            IF(INDEX(FUNCT1(1:NC1),'@').NE.0)THEN
                 NRES=1
                 CALL ALGEDT(VARLIS,23,IENTRY,USE,NRES)
                 FUNCT1='Edited function'
                 NC1=15
            ELSE
                 CALL ALGPRE(FUNCT1,NC1,VARLIS,23,NRES,USE,IENTRY,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! DRFPLT WARNING : Graph not'//
     -                     ' produced because of syntax errors.'
                      GOTO 100
                 ENDIF
            ENDIF
*   Figure out which quatities are effectively used.
            EVALE=.FALSE.
            EVALB=.FALSE.
            EVALV=.FALSE.
            EVALT=.FALSE.
            EVALD=.FALSE.
            EVALA=.FALSE.
            EVALL=.FALSE.
            EVALP=.FALSE.
            IF(USE(3).OR.USE(4).OR.USE(5).OR.USE(6).OR.USE(15))
     -           EVALE=.TRUE.
            IF(USE(7).OR.USE(8).OR.USE(9).OR.USE(10))EVALB=.TRUE.
            IF(USE(11).OR.USE(12).OR.USE(13).OR.USE(14).OR.
     -           USE(15))EVALV=.TRUE.
            IF(USE(16).OR.USE(17).OR.USE(18).OR.USE(19).OR.
     -           USE(20).OR.USE(21))EVALT=.TRUE.
            IF(USE(17))EVALP=.TRUE.
            IF(USE(18))EVALD=.TRUE.
            IF(USE(19))EVALA=.TRUE.
            IF(USE(20))EVALL=.TRUE.
*   Be sure only one result is returned.
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The function'//
     -                ' does not return precisely 1 result; no graph.'
                 GOTO 100
            ENDIF
*   check the use of magnetic field quantities,
            IF(EVALB.AND..NOT.MAGOK)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The function to be'//
     -                ' plotted uses magnetic field quantities,'
                 PRINT *,'                         no such field has'//
     -                ' been defined however ; plot not made.'
                 GOTO 100
            ENDIF
            IF((USE(7).OR.USE(8)).AND.POLAR)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : Bx and By should'//
     -                ' not be used with polar cells ; plot not made.'
                 GOTO 100
            ENDIF
*   Check use of absent gas data.
            IF(EVALD.AND..NOT.GASOK(3))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The graph tries'//
     -                ' to use absent diffusion data ; plot not made.'
                 GOTO 100
            ENDIF
            IF(EVALA.AND..NOT.GASOK(4))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The graph tries'//
     -                ' to use absent Townsend data ; plot not made.'
                 GOTO 100
            ENDIF
            IF(EVALL.AND..NOT.GASOK(6))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The graph tries'//
     -                ' to use absent attachment data ; plot not made.'
                 GOTO 100
            ENDIF
*   Select the axis with the largest range for ordinate.
            IF(FUNTRA(1:NCFTRA).NE.'?')THEN
                 ICOORD=3
            ELSEIF(POLAR)THEN
                 CALL CFMCTP(XT0,YT0,RT0,PT0,1)
                 CALL CFMCTP(XT1,YT1,RT1,PT1,1)
                 IF((ZT1-ZT0)**2.GT.(XT1-XT0)**2+(YT1-YT0)**2)THEN
                      ICOORD=11
                 ELSEIF(ABS(RT0-RT1).GT.ABS(PT0-PT1))THEN
                      ICOORD=1
                 ELSE
                      ICOORD=2
                 ENDIF
            ELSE
                 IF((ZT1-ZT0)**2.GT.(XT1-XT0)**2+(YT1-YT0)**2)THEN
                      ICOORD=11
                 ELSEIF(ABS(XT0-XT1).GT.ABS(YT0-YT1))THEN
                      ICOORD=1
                 ELSE
                      ICOORD=2
                 ENDIF
            ENDIF
*   Print a heading for the numbers.
            IF(FUNTRA(1:NCFTRA).EQ.'?')THEN
                 IF(LGRPRT)WRITE(LUNOUT,'(/''  GRAPH OF '',A,
     -                '' ON '',A//2X,''Coordinate'',48X,''Function'')')
     -                FUNCT1(1:NC1),'THE TRACK'
            ELSE
                 IF(LGRPRT)WRITE(LUNOUT,'(/''  GRAPH OF '',A,
     -                '' ON '',A//2X,''Coordinate'',48X,''Function'')')
     -                FUNCT1(1:NC1),FUNTRA(1:NCFTRA)
            ENDIF
*   Fill the vectors,
            DO 30 I=1,NGRPNT
            IF(ICOORD.NE.3)THEN
                 XPOS=XT0+REAL(I-1)*(XT1-XT0)/REAL(NGRPNT-1)
                 YPOS=YT0+REAL(I-1)*(YT1-YT0)/REAL(NGRPNT-1)
                 ZPOS=ZT0+REAL(I-1)*(ZT1-ZT0)/REAL(NGRPNT-1)
                 IF(POLAR)CALL CFMCTR(XPOS,YPOS,XPOS,YPOS,1)
            ELSE
                 VAR(1)=REAL(I-1)/REAL(NGRPNT-1)
                 MODVAR(1)=2
                 CALL ALGEXE(IENTRA,VAR,MODVAR,1,RES,MODRES,3,IFAIL)
                 XPOS=RES(1)
                 YPOS=RES(2)
                 ZPOS=RES(3)
                 IF(POLAR)CALL CFMPTR(XPOS,YPOS,XPOS,YPOS,1,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      XPOS=1.0
                      YPOS=0.0
                      ZPOS=0.0
                      PRINT *,' !!!!!! DRFPLT WARNING : The curve'//
     -                     ' function returns invalid coordinates.'
                 ENDIF
            ENDIF
            CALL DCONT2(XPOS,YPOS,ZPOS,RES,ILOC)
            IF(ICOORD.EQ.3)THEN
                 COORD(I)=REAL(I-1)/REAL(NGRPNT-1)
            ELSEIF(ICOORD.EQ.2)THEN
                 COORD(I)=YPOS
            ELSEIF(ICOORD.EQ.11)THEN
                 COORD(I)=ZPOS
            ELSE
                 COORD(I)=XPOS
            ENDIF
            VALUE(I)=RES(1)
*   Print the point if this has been requested.
            IF(LGRPRT)WRITE(LUNOUT,'(4(2X,E15.8))')
     -           XPOS,YPOS,ZPOS,VALUE(I)
30          CONTINUE
*   Plot the graph.
            IF(GRSMIN.LT.GRSMAX)CALL GRGRSC(GRSMIN,GRSMAX)
            IF(ICOORD.EQ.3)THEN
                 CALL GRGRPH(COORD,VALUE,NGRPNT,'Curve parameter',
     -                FUNCT1(NC1+1:40)//FUNCT1(1:NC1),
     -                'GRAPH OF '//FUNCT1(1:31))
            ELSEIF(POLAR.AND.ICOORD.EQ.1)THEN
                 CALL GRGRPH(COORD,VALUE,NGRPNT,'Radius [cm]',
     -                FUNCT1(NC1+1:40)//FUNCT1(1:NC1),
     -                'GRAPH OF '//FUNCT1(1:31))
            ELSEIF(POLAR.AND.ICOORD.EQ.2)THEN
                 CALL GRGRPH(COORD,VALUE,NGRPNT,'Angle [degrees]',
     -                FUNCT1(NC1+1:40)//FUNCT1(1:NC1),
     -                'GRAPH OF '//FUNCT1(1:31))
            ELSEIF((.NOT.POLAR).AND.ICOORD.EQ.1)THEN
                 CALL GRGRPH(COORD,VALUE,NGRPNT,'x-Axis [cm]',
     -                FUNCT1(NC1+1:40)//FUNCT1(1:NC1),
     -                'GRAPH OF '//FUNCT1(1:31))
            ELSEIF((.NOT.POLAR).AND.ICOORD.EQ.2)THEN
                 CALL GRGRPH(COORD,VALUE,NGRPNT,'y-Axis [cm]',
     -                FUNCT1(NC1+1:40)//FUNCT1(1:NC1),
     -                'GRAPH OF '//FUNCT1(1:31))
            ELSEIF((.NOT.POLAR).AND.ICOORD.EQ.11)THEN
                 CALL GRGRPH(COORD,VALUE,NGRPNT,'z-Axis [cm]',
     -                FUNCT1(NC1+1:40)//FUNCT1(1:NC1),
     -                'GRAPH OF '//FUNCT1(1:31))
            ELSE
                 PRINT *,' ###### DRFPLT ERROR   : Inconsistent axis'//
     -                ' selection ; program bug - please report.'
            ENDIF
*   Log this frame and prepare for the next plot.
            CALL GRNEXT
            CALL GRALOG('Graph of '//FUNCT1(1:31))
            CALL TIMLOG('Plotting the graph of '//FUNCT1(1:18))
*   print the number of arithmetic errors.
            CALL ALGERR
100         CONTINUE
*   Release the entry points.
            CALL ALGCLR(IENTRY)
            IF(FUNTRA(1:NCFTRA).NE.'?')CALL ALGCLR(IENTRA)
       ENDIF
*   Continue here if the parameters were not acceptable.
101    CONTINUE
*** Take care of the contours.
       IF(LCONT)THEN
*   Convert to an instruction list,
            IF(INDEX(FUNCT2(1:NC2),'@').NE.0)THEN
                 NRES=1
                 CALL ALGEDT(VARLIS,23,IENTRY,USE,NRES)
                 FUNCT2='Edited function'
                 NC2=15
            ELSE
                 CALL ALGPRE(FUNCT2,NC2,VARLIS,23,NRES,USE,IENTRY,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! DRFPLT WARNING : No contour'//
     -                     ' plot because of function syntax errors.'
                      GOTO 110
                 ENDIF
            ENDIF
*   Be sure only one result is returned.
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The function does'//
     -                ' not return precisely 1 result; no contour.'
                 GOTO 110
            ENDIF
*   Figure out which quantities are effectively used.
            EVALE=.FALSE.
            EVALB=.FALSE.
            EVALV=.FALSE.
            EVALT=.FALSE.
            EVALD=.FALSE.
            EVALA=.FALSE.
            EVALL=.FALSE.
            EVALP=.FALSE.
            IF(USE(3).OR.USE(4).OR.USE(5).OR.USE(6).OR.USE(15))
     -           EVALE=.TRUE.
            IF(USE(7).OR.USE(8).OR.USE(9).OR.USE(10))EVALB=.TRUE.
            IF(USE(11).OR.USE(12).OR.USE(13).OR.USE(14).OR.
     -           USE(15))EVALV=.TRUE.
            IF(USE(16).OR.USE(17).OR.USE(18).OR.USE(19).OR.
     -           USE(20).OR.USE(21))EVALT=.TRUE.
            IF(USE(17))EVALP=.TRUE.
            IF(USE(18))EVALD=.TRUE.
            IF(USE(19))EVALA=.TRUE.
            IF(USE(20))EVALL=.TRUE.
*   Check the use of magnetic field quantities.
            IF(EVALB.AND..NOT.MAGOK)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The function to be'//
     -                ' plotted uses magnetic field quantities,'
                 PRINT *,'                         no such field has'//
     -                ' been defined however ; plot not made.'
                 GOTO 110
            ENDIF
            IF((USE(7).OR.USE(8)).AND.POLAR)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : Bx and By should'//
     -                ' not be used with polar cells ; plot not made.'
                 GOTO 110
            ENDIF
*   Check use of absent gas data.
            IF(EVALD.AND..NOT.GASOK(3))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The contour tries'//
     -                ' to use absent diffusion data ; plot not made.'
                 GOTO 110
            ENDIF
            IF(EVALA.AND..NOT.GASOK(4))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The contour tries'//
     -                ' to use absent Townsend data ; plot not made.'
                 GOTO 110
            ENDIF
            IF(EVALL.AND..NOT.GASOK(6))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The contour tries'//
     -                ' to use absent attachment data ; plot not made.'
                 GOTO 110
            ENDIF
*   Plot the contours.
            CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -           'Contours of '//FUNCT2(1:NC2))
            NCONTP=NCONT
            CALL GRCONT(DCONT,CMIN,CMAX,VXMIN,VYMIN,VXMAX,VYMAX,
     -           NCONTP,CAUTO,POLAR,CLAB)
            CALL GRNEXT
*   Print the table of contour heights.
            CALL OUTFMT(CMIN,2,AUX1,NCAUX1,'LEFT')
            CALL OUTFMT(CMAX,2,AUX2,NCAUX2,'LEFT')
            CALL OUTFMT(REAL(NCONTP),2,AUX3,NCAUX3,'LEFT')
            CALL OUTFMT((CMAX-CMIN)/REAL(MAX(1,NCONTP)),2,
     -           AUX4,NCAUX4,'LEFT')
            IF(NCONTP.GE.1)WRITE(LUNOUT,'(/''  The contours'',
     -           '' correspond to '',A,'' = '',A,'' to '',A,
     -           '' in '',A,'' steps.''/''  The interval between 2'',
     -           '' contours is '',A,''.'')')
     -           FUNCT2(1:NC2),AUX1(1:NCAUX1),AUX2(1:NCAUX2),
     -           AUX3(1:NCAUX3),AUX4(1:NCAUX4)
            IF(NCONTP.EQ.0)WRITE(LUNOUT,'(/''  The contour'',
     -           '' corresponds to '',A,'' = '',A,''.'')')
     -           FUNCT2(1:NC2),AUX1(1:NCAUX1)
*   Keep track of the plots being made.
            CALL GRALOG('Contours of '//FUNCT2(1:NC2)//':')
            CALL TIMLOG('Plotting contours of '//FUNCT2(1:NC2)//':')
*   Print the number of arithmetic errors.
            CALL ALGERR
110         CONTINUE
            CALL ALGCLR(IENTRY)
       ENDIF
*** If one of the other plots is asked for, prepare the function string.
       IF(LHIST.OR.LSURF.OR.LVECT)THEN
            NCTOT=0
            IF(LSURF)THEN
                 ISURF=1
                 FUNCT1(1:NC3)=FUNCT3(1:NC3)
                 NCTOT=NC3
            ENDIF
            IF(LVECT)THEN
                 IF(LSURF)THEN
                      IVECT1=2
                      IVECT2=3
                      IVECT3=4
                      FUNCT1(NCTOT+1:NCTOT+NC4+1)=','//FUNCT4(1:NC4)
                      NCTOT=NCTOT+NC4+1
                 ELSE
                      IVECT1=1
                      IVECT2=2
                      IVECT3=3
                      FUNCT1(1:NC4)=FUNCT4(1:NC4)
                      NCTOT=NC4
                 ENDIF
            ENDIF
            IF(LHIST)THEN
                 IF(LSURF.OR.LVECT)THEN
                      IF(LSURF.AND..NOT.LVECT)IHIST=2
                      IF(LVECT.AND..NOT.LSURF)IHIST=4
                      IF(LSURF.AND.     LVECT)IHIST=5
                      FUNCT1(NCTOT+1:NCTOT+NC5+1)=','//FUNCT5(1:NC5)
                      NCTOT=NCTOT+NC5+1
                 ELSE
                      IHIST=1
                      FUNCT1(1:NC5)=FUNCT5(1:NC5)
                      NCTOT=NC5
                 ENDIF
            ENDIF
*   Turn it into an instruction list,
            NREXP=0
            IF(LHIST)NREXP=NREXP+1
            IF(LSURF)NREXP=NREXP+1
            IF(LVECT)NREXP=NREXP+3
            IF(INDEX(FUNCT1(1:NCTOT),'@').NE.0)THEN
                 NRES=NREXP
                 CALL ALGEDT(VARLIS,23,IENTRY,USE,NRES)
                 FUNCT1='Edited function'
                 NCTOT=15
            ELSE
                 CALL ALGPRE(FUNCT1,NCTOT,VARLIS,23,NRES,USE,IENTRY,
     -                IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! DRFPLT WARNING : Plots not'//
     -                     ' produced because of syntax errors.'
                      GOTO 120
                 ENDIF
            ENDIF
*   Be sure only one result is returned.
            IF(NRES.NE.NREXP)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The function does'//
     -                ' not return the correct number of results;'//
     -                ' histogram, surface and vector plot skipped.'
                 GOTO 120
            ENDIF
*   Figure out which quantities are effectively used.
            EVALE=.FALSE.
            EVALB=.FALSE.
            EVALV=.FALSE.
            EVALT=.FALSE.
            EVALD=.FALSE.
            EVALA=.FALSE.
            EVALL=.FALSE.
            EVALP=.FALSE.
            IF(USE(3).OR.USE(4).OR.USE(5).OR.USE(6).OR.USE(15))
     -           EVALE=.TRUE.
            IF(USE(7).OR.USE(8).OR.USE(9).OR.USE(10))EVALB=.TRUE.
            IF(USE(11).OR.USE(12).OR.USE(13).OR.USE(14).OR.
     -           USE(15))EVALV=.TRUE.
            IF(USE(16).OR.USE(17).OR.USE(18).OR.USE(19).OR.
     -           USE(20).OR.USE(21))EVALT=.TRUE.
            IF(USE(17))EVALP=.TRUE.
            IF(USE(18))EVALD=.TRUE.
            IF(USE(19))EVALA=.TRUE.
            IF(USE(20))EVALL=.TRUE.
*   check the use of magnetic field quantities,
            IF(EVALB.AND..NOT.MAGOK)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The function to be'//
     -                ' plotted uses magnetic field quantities,'
                 PRINT *,'                         no such field has'//
     -                ' been defined however ; plot not made.'
                 GOTO 120
            ENDIF
            IF((USE(7).OR.USE(8)).AND.POLAR)THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : Bx and By should'//
     -                ' not be used with polar cells ; plot not made.'
                 GOTO 120
            ENDIF
*   Check use of absent gas data.
            IF(EVALD.AND..NOT.GASOK(3))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The plot tries'//
     -                ' to use absent diffusion data ; plot not made.'
                 GOTO 120
            ENDIF
            IF(EVALA.AND..NOT.GASOK(4))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The plot tries'//
     -                ' to use absent Townsend data ; plot not made.'
                 GOTO 120
            ENDIF
            IF(EVALL.AND..NOT.GASOK(6))THEN
                 PRINT *,' !!!!!! DRFPLT WARNING : The plot tries'//
     -                ' to use absent attachment data ; plot not made.'
                 GOTO 120
            ENDIF
*   Obtain the matrix for surface plotting.
            IF(LSURF)THEN
                 CALL BOOK('BOOK','MATRIX','SURFACE',IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! DRFPLT WARNING : Unable to'//
     -                     ' obtain storage for the surface plot.'
                      PRINT *,'                         The plot'//
     -                     ' will not be made.'
                      LSURF=.FALSE.
                 ENDIF
            ENDIF
*   Open a plotting frame for a VECTOR plot, if requested.
            IF(LVECT)THEN
                 CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -                'Vector plot of '//FUNCT4(1:NC4))
                 CALL GRALOG('Vector plot of '//FUNCT4(1:NC4)//':')
*   Otherwise, merely request the viewing area.
            ELSE
                 CALL GRVIEW(VXMIN,VYMIN,VXMAX,VYMAX)
            ENDIF
*   Allocate an histogram, if needed.
            IF(LHIST)THEN
                 CALL HISADM('ALLOCATE',IHISRF,NCHA,HMIN,HMAX,
     -                HAUTO,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! DRFPLT WARNING : Unable to'//
     -                     ' allocate histogram storage; histogram'//
     -                     ' cancelled.'
                      LHIST=.FALSE.
                 ENDIF
            ENDIF
*   Fill all the arrays and matrices required for these plots.
            CALL GRATTS('FUNCTION-1','POLYLINE')
            DO 50 I=1,NGRIDX
            IF(.NOT.POLAR)THEN
                 XXPOS=VXMIN+REAL(I-1)*(VXMAX-VXMIN)/REAL(NGRIDX-1)
            ELSE
                 XXPOS=LOG(EXP(VXMIN)+REAL(I-1)*(EXP(VXMAX)-EXP(VXMIN))/
     -                REAL(NGRIDX-1))
            ENDIF
*   set a normalisation factor, to get the arrows more or less right
            IF(.NOT.POLAR)THEN
                 FACNRM=MIN(VYMAX-VYMIN,VXMAX-VXMIN)/REAL(NGRIDX)
            ELSE
                 FACNRM=LOG((EXP(VXMIN)+REAL(I-1)*(EXP(VXMAX)-
     -                EXP(VXMIN))/REAL(NGRIDX))/(EXP(VXMIN)+REAL(I)*
     -                (EXP(VXMAX)-EXP(VXMIN))/REAL(NGRIDX)))
            ENDIF
            DO 60 J=1,NGRIDY
            YYPOS=VYMIN+REAL(J-1)*(VYMAX-VYMIN)/REAL(NGRIDY-1)
*   Coordinate transformation to the viewing plane.
            XPOS=FPROJ(1,1)*XXPOS+FPROJ(2,1)*YYPOS+FPROJ(3,1)
            YPOS=FPROJ(1,2)*XXPOS+FPROJ(2,2)*YYPOS+FPROJ(3,2)
            ZPOS=FPROJ(1,3)*XXPOS+FPROJ(2,3)*YYPOS+FPROJ(3,3)
            IF(XPOS.LT.DXMIN.OR.XPOS.GT.DXMAX.OR.
     -           YPOS.LT.DYMIN.OR.YPOS.GT.DYMAX.OR.
     -           ZPOS.LT.DZMIN.OR.ZPOS.GT.DZMAX)THEN
                 IF(LSURF)WS(I,J)=0.0
                 GOTO 60
            ENDIF
*   Evaluate the function.
            VAR(1)=XPOS
            VAR(2)=YPOS
            VAR(23)=ZPOS
*   Calculate the fields needed for the rest.
            IF(EVALE)CALL EFIELD(VAR(1),VAR(2),VAR(23),
     -           VAR(3),VAR(4),VAR(5),VAR(6),VOLT,0,ILOC)
            IF(EVALB)CALL BFIELD(VAR(1),VAR(2),VAR(23),
     -           VAR(7),VAR(8),VAR(9),VAR(10))
*   Get the local drift velocity.
            IF(EVALV)THEN
                 CALL DLCVEL(DBLE(VAR(1)),DBLE(VAR(2)),DBLE(VAR(23)),
     -                F0,QPLT,ITYPE,ILOC)
                 VAR(11)=REAL(F0(1))
                 VAR(12)=REAL(F0(2))
                 VAR(13)=REAL(F0(3))
                 VAR(14)=SQRT(VAR(11)**2+VAR(12)**2+VAR(13)**2)
            ENDIF
*   Lorentz angle.
            IF(EVALE.AND.EVALV)VAR(15)=ACOS(MAX(-1.0,MIN(1.0,
     -           REAL((VAR(3)*VAR(11)+VAR(4)*VAR(12)+VAR(5)*VAR(13))/
     -           (VAR(6)*VAR(14))))))
*   Store drift line related quantities.
            IF(EVALT)THEN
                 CALL DLCALC(VAR(1),VAR(2),VAR(23),QPLT,ITYPE)
                 VAR(16)=TU(NU)
                 IF(EVALP)THEN
                      VAR(17)=0.0
                      DO 64 II=2,NU
                      IF(POLAR)THEN
                           CALL CF2RTC(XU(II-1),YU(II-1),XPOS1,YPOS1,1)
                           CALL CF2RTC(XU(II)  ,YU(II)  ,XPOS2,YPOS2,1)
                           VAR(17)=VAR(17)+SQRT((XPOS2-XPOS1)**2+
     -                          (YPOS2-YPOS1)**2+(ZU(II)-ZU(II-1))**2)
                      ELSE
                           VAR(17)=VAR(17)+SQRT((XU(II)-XU(II-1))**2+
     -                          (YU(II)-YU(II-1))**2+
     -                          (ZU(II)-ZU(II-1))**2)
                      ENDIF
64                    CONTINUE
                 ENDIF
                 IF(EVALD)CALL DLCDIF(VAR(18))
                 IF(EVALA)CALL DLCTWN(VAR(19))
                 IF(EVALL)CALL DLCATT(VAR(20))
                 VAR(21)=ISTAT
            ENDIF
*   Store gas pressure.
            VAR(22)=PGAS
*   Transform vectors and covectors to polar coordinates if needed.
            IF(POLAR)THEN
                 CALL CFMRTP(VAR(1),VAR(2),VAR(1),VAR(2),1)
                 VAR(3)=VAR(3)/VAR(1)
                 VAR(4)=VAR(4)/VAR(1)
                 VAR(6)=SQRT(VAR(3)**2+VAR(4)**2+VAR(5)**2)
                 VAR(11)=VAR(11)*VAR(1)
                 VAR(12)=VAR(12)*VAR(1)
                 VAR(14)=SQRT(VAR(11)**2+VAR(12)**2+VAR(13)**2)
            ENDIF
            DO 65 II=1,23
            MODVAR(II)=2
65          CONTINUE
            CALL ALGEXE(IENTRY,VAR,MODVAR,23,RES,MODRES,5,IFAIL)
*   Vector plot plotting.
            IF(LVECT)THEN
                 IF(RES(IVECT1)**2+RES(IVECT2)**2+RES(IVECT3)**2.GT.0)
     -                CALL PLAARR(XPOS,YPOS,ZPOS,
     -                0.5*FACNRM*RES(IVECT1)/SQRT(RES(IVECT1)**2+
     -                RES(IVECT2)**2+RES(IVECT3)**2),
     -                0.5*FACNRM*RES(IVECT2)/SQRT(RES(IVECT1)**2+
     -                RES(IVECT2)**2+RES(IVECT3)**2),
     -                0.5*FACNRM*RES(IVECT3)/SQRT(RES(IVECT1)**2+
     -                RES(IVECT2)**2+RES(IVECT3)**2))
            ENDIF
            IF(LSURF)WS(I,J)=RES(ISURF)
*   fill the histogram, if requested,
            IF(LHIST)CALL HISENT(IHISRF,RES(IHIST),1.0)
60          CONTINUE
50          CONTINUE
            CALL TIMLOG('Accumulating plot data on the grid:     ')
            IF(LVECT)CALL GRNEXT
*   plot the 3-dimensional picture if requested
            IF(LSURF)THEN
*   Check that the surface is not flat.
                 IFLAT=1
                 SMIN=WS(1,1)
                 SMAX=WS(1,1)
                 DO 80 ICHK=1,NGRIDX
                 DO 70 JCHK=1,NGRIDY
                 IF(ABS(WS(ICHK,JCHK)-WS(1,1)).GT.1.0E-5*
     -                 (1.0+ABS(WS(ICHK,JCHK))+ABS(WS(1,1))))IFLAT=0
                 SMIN=MIN(SMIN,WS(1,1))
                 SMAX=MAX(SMAX,WS(1,1))
70               CONTINUE
80               CONTINUE
                 IF(IFLAT.NE.0)THEN
                      PRINT *,' !!!!!! DRFPLT WARNING : The surface is',
     -                     ' not plotted because it is entirely flat.'
                      CALL BOOK('RELEASE','MATRIX','SURFACE',IFAIL)
                      GOTO 90
                 ENDIF
*   Switch the screen to graphics mode.
                 CALL GRGRAF(.TRUE.)
*   Fill the PAR vector.
                 PAR(1)=THETA
                 PAR(2)=PHI
                 PAR(3)=VXMIN
                 PAR(4)=VXMAX
                 PAR(5)=VYMIN
                 PAR(6)=VYMAX
                 PAR(7)=SMIN
                 PAR(8)=SMAX
                 PAR(9)=1000+NGRIDX
                 PAR(10)=1000+NGRIDY
                 PAR(11)=510
                 PAR(12)=510
                 PAR(13)=510
                 PAR(14)=1
                 PAR(15)=1
                 PAR(16)=1
                 PAR(17)=0.02
                 PAR(18)=0.02
                 PAR(19)=0.02
                 PAR(20)=0.03
                 PAR(21)=2
                 PAR(22)=0.03
                 PAR(23)=0.03
                 PAR(24)=0.03
                 PAR(25)=7
                 PAR(26)=8
                 PAR(27)=9
                 PAR(28)=10
                 PAR(29)=11
                 PAR(30)=12
                 PAR(31)=13
                 PAR(32)=14
                 PAR(33)=15
                 PAR(34)=16
                 PAR(35)=17
                 PAR(36)=18
                 PAR(37)=19
*   Plot the surface.
                 CALL ISVP(1,DISPX0+0.1,DISPX1-0.1,
     -                DISPY0+0.1,DISPY1-0.1)
                 CALL ISWN(1,0.0,1.0,0.0,1.0)
                 CALL ISELNT(1)
                 CALL IGTABL(MXWIRE,MXWIRE,WS,37,PAR,'S1')
*   Close the plot.
                 CALL GRNEXT
*   Record what happened.
                 CALL TIMLOG('Making a 3-dimensional plot:            ')
                 CALL GRALOG('3-D plot of '//FUNCT3(1:28))
*   Release the matrix.
                 CALL BOOK('RELEASE','MATRIX','SURFACE',IFAIL)
90               CONTINUE
            ENDIF
*   plot the histogram if requested, delete after use.
            IF(LHIST)THEN
                 CALL HISPLT(IHISRF,FUNCT5(1:NC5),
     -                'Histogram of '//FUNCT5(1:NC5),.TRUE.)
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 CALL GRNEXT
                 CALL GRALOG('Histogram of '//FUNCT5(1:NC5)//':')
                 CALL TIMLOG('Plotting an histogram of '//
     -                FUNCT5(1:NC5))
                 CALL HISADM('DELETE',IHISRF,0,0.0,0.0,.FALSE.,IFAIL)
            ENDIF
*   print the number of arithmetic errors.
            CALL ALGERR
120         CONTINUE
*   release the algebra storage.
            CALL ALGCLR(IENTRY)
       ENDIF
       END
