CDECK  ID>, DRFARR.
       SUBROUTINE DRFARR
*-----------------------------------------------------------------------
*   DRFARR - Computes the arrival time distribution of the M'th electron
*            from a given track.
*   VARIABLES :
*   (Last changed on 11/10/11.)
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
       CHARACTER*(MXCHAR) STRING
       CHARACTER*(MXNAME) FILE
       CHARACTER*80 TITLE
       CHARACTER*29 REMARK
       CHARACTER*15 STR1,STR2,STR3,STR4,STRID
       CHARACTER*8 TIME,DATE,MEMBER
       CHARACTER*1 STEP,SCAN
       REAL ARRTIM(2,MXPART),XPL(MXLIST),YPL(MXLIST),THRESH,THRR,
     -      ARRLIS(MXLIST,4+3*MXELEC),TSTEP,TEMIN,TEMAX,TGMIN,TGMAX,
     -      UARMIN,UARMAX,VARMIN,VARMAX,UARMIR,UARMAR,VARMIR,VARMAR,
     -      UAROFF,WAROFF,XW,YW,ZW,
     -      USTEP,USTEPR,TANPHI,ANGLER,TFORC1,TFORC2,XCL,YCL,ZCL,ECL,
     -      ACL,BCL,FCL,SCL,TCL,RNDNOR,HGMIN,HGMAX,HEMIN,HEMAX,
     -      SMIN,SMAX,AMIN,AMAX,BMIN,BMAX,TMAX,EXTRA1
       INTEGER ARRFLG(MXLIST),IRFTEL(MXELEC),IRFXEL(MXELEC),
     -      IRFXGL,IRFTGL,IRFNCL,IRFNEL,NCLUS,NCHA,NCHAR,NPLOT,
     -      MELEC(MXELEC),NELEC(MXELEC),ISIZ(1),IDIM(1),NCFILE,NCMEMB,
     -      NCREM,NLTR,NLTRR,JSEL,JALL,JOVER,
     -      INEXT,NWORD,NRNDM,NRNDMR,IORDER,
     -      IORDR,KELEC,MR,IW,ISWCNT,IXM,IXP,IX,IFAIL,IFAIL1,IFAIL2,
     -      IFAIL3,IFAIL4,IFAIL5,IFAIL6,NGLOB,NPART,IPRT,IRNDM,NPAIR,
     -      ICL,ICLS,INPCMP,INPTYP,NGBIN,I,J,K,II,
     -      NGENT,NEBIN,NEENT,NC1,NC2,NC3,NC4,NCID,NC,IOS,IPLANE,ISOLID
       LOGICAL FLAG(MXWORD+3),LARRWR,LGLBPL,LELEPL,LARRPL,TEAUTO,TGAUTO,
     -      LGLBPR,LELEPR,LARRPR,WFORCE,LHISKP,LEXIST,LSET,LDIFF,
     -      LATTAC,LARRKP,DONE,OK,EXMEMB
       EXTERNAL RNDNOR,INPCMP,INPTYP
       SAVE NRNDM,NCHA,KELEC,MELEC,TANPHI,LGLBPL,LELEPL,LARRPL,THRESH,
     -      IORDER,LGLBPR,LELEPR,LARRPR,TEAUTO,TGAUTO,LHISKP,
     -      LARRKP
*** Initialise those variables that are kept across calls.
       DATA NRNDM /1000/, NCHA /100/, IORDER /2/
       DATA KELEC /1/, MELEC /MXELEC*25/
       DATA TANPHI /0.0/, THRESH /0.5  /
       DATA LGLBPL /.FALSE./, LELEPL /.FALSE./, LARRPL /.TRUE./
       DATA LGLBPR /.FALSE./, LELEPR /.FALSE./, LARRPR /.FALSE./
       DATA TEAUTO /.TRUE. /, TGAUTO /.FALSE./, LATTAC /.FALSE./
       DATA LHISKP /.FALSE./, LARRKP /.FALSE./
*** Make sure the cell is not in polar coordinates.
       IF(POLAR)THEN
            PRINT *,' ###### DRFARR ERROR   : The ARRIVAL function'//
     -           ' can not be applied to polar geometries.'
            RETURN
       ENDIF
*** Initialise various variables being reset at each call.
       FILE=' '
       MEMBER='< none >'
       REMARK='None'
       NCFILE=1
       NCMEMB=8
       NCREM=4
       LARRWR=.FALSE.
       STEP='X'
       SCAN='Y'
       UARMIN=DXMIN
       UARMAX=DXMAX
       VARMIN=DYMIN
       VARMAX=DYMAX
       WAROFF=0
       CALL ROUND(UARMIN,UARMAX,20,'SMALLER,COARSER',USTEP)
       NLTR=NLINED
       WFORCE=.FALSE.
       TFORC1=-1.0
       TFORC2=-1.0
       JSEL=0
       JALL=0
       JOVER=0
       LDIFF=GASOK(3)
       OK=.TRUE.
*** Examine the input line, flag the known words.
       CALL INPNUM(NWORD)
       DO 10 I=2,NWORD
       IF(  INPCMP(I,'ATT#ACHMENT')+INPCMP(I,'NOATT#ACHMENT')+
     -      INPCMP(I,'BIN#S')+
     -      INPCMP(I,'DA#TASET')+INPCMP(I,'REM#ARK')+
     -      INPCMP(I,'DIFF#USION')+INPCMP(I,'NODIFF#USION')+
     -      INPCMP(I,'EL#ECTRONS')+
     -      INPCMP(I,'ITER#ATE')+INPCMP(I,'ITER#ATIONS')+
     -      INPCMP(I,'KEEP-HIST#OGRAMS')+INPCMP(I,'NOKEEP-HIST#OGRAMS')+
     -      INPCMP(I,'KEEP-RES#ULTS')+INPCMP(I,'NOKEEP-RES#ULTS')+
     -      INPCMP(I,'LIN#ES')+
     -      INPCMP(I,'PL#OT-ALL-#ELECTRONS')+
     -           INPCMP(I,'NOPL#OT-ALL-#ELECTRONS')+
     -      INPCMP(I,'PL#OT-SEL#ECTED-#ELECTRONS')+
     -           INPCMP(I,'NOPL#OT-SEL#ECTED-#ELECTRONS')+
     -      INPCMP(I,'OFF#SET')+
     -      INPCMP(I,'PL#OT-O#VERVIEW')+INPCMP(I,'NOPL#OT-O#VERVIEW')+
     -      INPCMP(I,'POL#YNOMIAL-ORD#ER')+
     -      INPCMP(I,'PR#INT-ALL-#ELECTRONS')+
     -           INPCMP(I,'NOPR#INT-ALL-#ELECTRONS')+
     -      INPCMP(I,'PR#INT-SEL#ECTED-#ELECTRONS')+
     -           INPCMP(I,'NOPR#INT-SEL#ECTED-#ELECTRONS')+
     -      INPCMP(I,'PR#INT-O#VERVIEW')+INPCMP(I,'NOPR#INT-O#VERVIEW')+
     -      INPCMP(I,'SC#AN')+INPCMP(I,'ST#EP')+
     -      INPCMP(I,'THR#ESHOLD')+INPCMP(I,'T#IME-WIN#DOW').NE.0)THEN
            FLAG(I)=.TRUE.
       ELSE
            FLAG(I)=.FALSE.
       ENDIF
10     CONTINUE
       FLAG(NWORD+1)=.TRUE.
       FLAG(NWORD+2)=.TRUE.
       FLAG(NWORD+3)=.TRUE.
       INEXT=2
*** Read in detail.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
**  Time window.
       IF(INPCMP(I,'T#IME-WIN#DOW').NE.0)THEN
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 TEAUTO=.TRUE.
                 WFORCE=.FALSE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'FULL-#RANGE').NE.0)THEN
                 TEAUTO=.FALSE.
                 WFORCE=.FALSE.
                 INEXT=I+2
            ELSEIF(I+2.GT.NWORD.OR.FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'This keyword has 2 arguments. ')
                 OK=.FALSE.
                 INEXT=I+3
            ELSEIF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2)THEN
                 CALL INPMSG(I+1,'This should be a real argument')
                 OK=.FALSE.
                 INEXT=I+3
            ELSEIF(INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2)THEN
                 CALL INPMSG(I+2,'This should be a real argument')
                 OK=.FALSE.
                 INEXT=I+3
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
                      TEAUTO=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
**  The BINS keyword.
       ELSEIF(INPCMP(I,'BIN#S').NE.0)THEN
            IF(I+1.GT.NWORD.OR.FLAG(I+1))THEN
                 CALL INPMSG(I,'This keyword has one argument.')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I+1,'This is an integer argument.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NCHAR,MXCHA)
                 IF(NCHAR.LE.1.OR.NCHAR.GT.MXCHA)THEN
                      CALL INPMSG(I+1,'Inacceptable number of bins.  ')
                      OK=.FALSE.
                 ELSE
                      NCHA=NCHAR
                 ENDIF
            ENDIF
            INEXT=I+2
**  Histogram keeping option.
       ELSEIF(INPCMP(I,'KEEP-HIST#OGRAMS').NE.0)THEN
            LHISKP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-HIST#OGRAMS').NE.0)THEN
            LHISKP=.FALSE.
**  Results keeping option.
       ELSEIF(INPCMP(I,'KEEP-RES#ULTS').NE.0)THEN
            LARRKP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-RES#ULTS').NE.0)THEN
            LARRKP=.FALSE.
**  Read the output data set name.
       ELSEIF(INPCMP(I,'DA#TASET').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have an argument.      ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCFILE)
                 FILE=STRING
                 INEXT=I+2
                 IF(.NOT.FLAG(I+2))THEN
                      CALL INPSTR(I+2,I+2,STRING,NCMEMB)
                      MEMBER=STRING
                      INEXT=I+3
                 ENDIF
                 LARRWR=.TRUE.
            ENDIF
**  Read the first and last particle to be considered.
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
                 CALL INPCHK(J,1,IFAIL)
                 CALL INPRDI(J,MR,5)
                 IF(MR.LT.1-MXPART.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(J,'Smaller than 1-MXPART. ')
                      OK=.FALSE.
                      KELEC=KELEC-1
                 ELSEIF(MR.GT.MXPART.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(J,'Larger than MXPART.    ')
                      OK=.FALSE.
                      KELEC=KELEC-1
                 ELSEIF(IFAIL.EQ.0)THEN
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
**  Number of lines to be used for track preparation.
       ELSEIF(INPCMP(I,'LINE#S').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Specify number of drift-lines.')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'This is an integer argument.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NLTRR,NLTR)
                 IF(NLTRR.LT.4.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'At least 4 lines are needed.  ')
                      OK=.FALSE.
                 ELSEIF(NLTRR.GT.MXLIST.AND.IFAIL.EQ.0)THEN
                      CALL INPMSG(I+1,'Not more than MXLIST lines.   ')
                      OK=.FALSE.
                 ELSEIF(IFAIL.EQ.0)THEN
                      NLTR=NLTRR
                 ENDIF
                 INEXT=I+2
            ENDIF
**  The ITERATIONS keyword.
       ELSEIF(INPCMP(I,'ITER#ATIONS')+INPCMP(I,'ITER#ATE').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'This keyword has one argument.')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'This is an integer argument.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NRNDMR,NRNDM)
                 IF(NRNDMR.LT.1)THEN
                      CALL INPMSG(I+1,'At least 1 iteration needed.  ')
                      OK=.FALSE.
                 ELSE
                      NRNDM=NRNDMR
                 ENDIF
            ENDIF
            INEXT=I+2
**  Include diffusion and attachment, if required.
       ELSEIF(INPCMP(I,'DIFF#USION').NE.0)THEN
            IF(GASOK(3))THEN
                 LDIFF=.TRUE.
            ELSE
                 CALL INPMSG(I,'No diffusion data available.')
                 OK=.FALSE.
            ENDIF
       ELSEIF(INPCMP(I,'NODIFF#USION').NE.0)THEN
            LDIFF=.FALSE.
       ELSEIF(INPCMP(I,'ATT#ACHMENT').NE.0)THEN
            IF(GASOK(6))THEN
                 LATTAC=.TRUE.
            ELSE
                 CALL INPMSG(I,'No attachment data available.')
                 OK=.FALSE.
            ENDIF
       ELSEIF(INPCMP(I,'NOATT#ACHMENT').NE.0)THEN
            LATTAC=.FALSE.
**  Plot options.
       ELSEIF(INPCMP(I,'PL#OT-ALL-#ELECTRONS').NE.0)THEN
            LGLBPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-ALL-#ELECTRONS').NE.0)THEN
            LGLBPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-SEL#ECTED-#ELECTRONS').NE.0)THEN
            LELEPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-SEL#ECTED-#ELECTRONS').NE.0)THEN
            LELEPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-O#VERVIEW').NE.0)THEN
            LARRPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-O#VERVIEW').NE.0)THEN
            LARRPL=.FALSE.
**  Print options.
       ELSEIF(INPCMP(I,'PR#INT-ALL-#ELECTRONS').NE.0)THEN
            LGLBPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-ALL-#ELECTRONS').NE.0)THEN
            LGLBPR=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT-SEL#ECTED-#ELECTRONS').NE.0)THEN
            LELEPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-SEL#ECTED-#ELECTRONS').NE.0)THEN
            LELEPR=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT-O#VERVIEW').NE.0)THEN
            LARRPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-O#VERVIEW').NE.0)THEN
            LARRPR=.FALSE.
**  The POLYNOMIAL-ORDER keyword.
       ELSEIF(INPCMP(I,'POL#YNOMIAL-ORD#ER').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'This keyword has one argument.')
                 OK=.FALSE.
            ELSEIF(INPCMP(I+1,'LIN#EAR').NE.0)THEN
                 IORDER=1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'QUAD#RATIC')+
     -           INPCMP(I+1,'PARA#BOLIC').NE.0)THEN
                 IORDER=2
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'CUB#IC').NE.0)THEN
                 IORDER=3
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I,'This is an integer argument.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,IORDR,IORDER)
                 IF(IORDR.LT.1.OR.IORDR.GT.10)THEN
                      CALL INPMSG(I+1,'Not in the range [1,10].      ')
                      OK=.FALSE.
                 ELSE
                      IORDER=IORDR
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Read the remark to be added to the dataset.
       ELSEIF(INPCMP(I,'REM#ARK').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have an argument.      ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCREM)
                 REMARK=STRING
                 INEXT=I+2
            ENDIF
**  Read the threshold value.
       ELSEIF(INPCMP(I,'THR#ESHOLD').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have an argument.      ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,THRR,THRESH)
                 IF(IFAIL1.EQ.0.AND.(THRR.LE.0.0.OR.THRR.GE.1.0))THEN
                      CALL INPMSG(I+1,'The threshold range is <0,1>. ')
                      OK=.FALSE.
                 ELSEIF(IFAIL1.EQ.0)THEN
                      THRESH=THRR
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Stepping direction.
       ELSEIF(INPCMP(I,'ST#EP').NE.0)THEN
*   Find out direction.
            IF(INPCMP(I+1,'X').NE.0)THEN
                 STEP='X'
                 UARMIN=DXMIN
                 UARMAX=DXMAX
                 CALL ROUND(UARMIN,UARMAX,20,'SMALLER,COARSER',USTEP)
            ELSEIF(INPCMP(I+1,'Y').NE.0)THEN
                 STEP='Y'
                 UARMIN=DYMIN
                 UARMAX=DYMAX
                 CALL ROUND(UARMIN,UARMAX,20,'SMALLER,COARSER',USTEP)
            ELSEIF(INPCMP(I+1,'Z').NE.0)THEN
                 STEP='Z'
                 UARMIN=DZMIN
                 UARMAX=DZMAX
                 CALL ROUND(UARMIN,UARMAX,20,'SMALLER,COARSER',USTEP)
            ELSE
                 CALL INPMSG(I,'Not followed by a direction.')
                 OK=.FALSE.
                 INEXT=I+1
                 GOTO 20
            ENDIF
            INEXT=I+2
*   Scan the sub-keywords.
            DO 30 J=I+2,NWORD
            IF(J.LT.INEXT)GOTO 30
            IF(FLAG(J))THEN
                 INEXT=J
                 GOTO 20
*   Range.
            ELSEIF(INPCMP(J,'RAN#GE').NE.0)THEN
                 IF(FLAG(J+1).OR.FLAG(J+2).OR.
     -                (INPTYP(J+1).NE.1.AND.INPTYP(J+1).NE.2).OR.
     -                (INPTYP(J+2).NE.1.AND.INPTYP(J+2).NE.2))THEN
                      CALL INPMSG(J,'Should have 2 real arguments.')
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(J+1,2,IFAIL1)
                      CALL INPCHK(J+2,2,IFAIL2)
                      IF(STEP.EQ.'X')THEN
                           CALL INPRDR(J+1,UARMIR,DXMIN)
                           CALL INPRDR(J+2,UARMAR,DXMAX)
                      ELSEIF(STEP.EQ.'Y')THEN
                           CALL INPRDR(J+1,UARMIR,DYMIN)
                           CALL INPRDR(J+2,UARMAR,DYMAX)
                      ELSEIF(STEP.EQ.'Z')THEN
                           CALL INPRDR(J+1,UARMIR,DZMIN)
                           CALL INPRDR(J+2,UARMAR,DZMAX)
                      ENDIF
                      IF(UARMIR.EQ.UARMAR)THEN
                           CALL INPMSG(J+1,'Zero range not permitted.')
                           CALL INPMSG(J+2,'See preceding message.')
                           OK=.FALSE.
                      ELSEIF((STEP.EQ.'X'.AND.(
     -                     MAX(UARMIR,UARMAR).LT.DXMIN.OR.
     -                     MIN(UARMIR,UARMAR).GT.DXMAX)).OR.
     -                     (STEP.EQ.'Y'.AND.(
     -                     MAX(UARMIR,UARMAR).LT.DYMIN.OR.
     -                     MIN(UARMIR,UARMAR).GT.DYMAX)).OR.
     -                     (STEP.EQ.'Z'.AND.(
     -                     MAX(UARMIR,UARMAR).LT.DZMIN.OR.
     -                     MIN(UARMIR,UARMAR).GT.DZMAX)))THEN
                           CALL INPMSG(J+1,'Range not inside the area.')
                           CALL INPMSG(J+2,'See preceding message.')
                           OK=.FALSE.
                      ELSE
                           IF(STEP.EQ.'X')THEN
                                UARMIN=MAX(DXMIN,MIN(UARMIR,UARMAR))
                                UARMAX=MIN(DXMAX,MAX(UARMIR,UARMAR))
                           ELSEIF(STEP.EQ.'Y')THEN
                                UARMIN=MAX(DYMIN,MIN(UARMIR,UARMAR))
                                UARMAX=MIN(DYMAX,MAX(UARMIR,UARMAR))
                           ELSEIF(STEP.EQ.'Z')THEN
                                UARMIN=MAX(DZMIN,MIN(UARMIR,UARMAR))
                                UARMAX=MIN(DZMAX,MAX(UARMIR,UARMAR))
                           ENDIF
                      ENDIF
                      INEXT=J+3
                 ENDIF
*   Step size.
            ELSEIF(INPCMP(J,'INCR#EMENT').NE.0)THEN
                 IF(FLAG(J+1).OR.
     -                (INPTYP(J+1).NE.1.AND.INPTYP(J+1).NE.2))THEN
                      CALL INPMSG(J,'Should have 1 real argument.')
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(J+1,2,IFAIL)
                      CALL INPRDR(J+1,USTEPR,USTEP)
                      IF(USTEPR.LE.0)THEN
                           CALL INPMSG(J+1,
     -                          'The step size must be positive')
                           OK=.FALSE.
                      ELSE
                           USTEP=USTEPR
                      ENDIF
                      INEXT=J+2
                 ENDIF
*   Unknown sub-keyword.
            ELSE
                 CALL INPMSG(J,'Not a sub-keyword of STEP.')
            ENDIF
30          CONTINUE
**  Scanning direction.
       ELSEIF(INPCMP(I,'SC#AN').NE.0)THEN
*   Find out direction.
            IF(INPCMP(I+1,'X').NE.0)THEN
                 SCAN='X'
                 VARMIN=DXMIN
                 VARMAX=DXMAX
            ELSEIF(INPCMP(I+1,'Y').NE.0)THEN
                 SCAN='Y'
                 VARMIN=DYMIN
                 VARMAX=DYMAX
            ELSEIF(INPCMP(I+1,'Z').NE.0)THEN
                 SCAN='Z'
                 VARMIN=DZMIN
                 VARMAX=DZMAX
            ELSE
                 CALL INPMSG(I,'Not followed by a direction.')
                 OK=.FALSE.
                 INEXT=I+1
                 GOTO 20
            ENDIF
            INEXT=I+2
*  Scan for sub-keywords.
            DO 40 J=I+2,NWORD
            IF(J.LT.INEXT)GOTO 40
            IF(FLAG(J))THEN
                 INEXT=J
                 GOTO 20
*   Range.
            ELSEIF(INPCMP(J,'RAN#GE').NE.0)THEN
                 IF(FLAG(J+1).OR.FLAG(J+2).OR.
     -                (INPTYP(J+1).NE.1.AND.INPTYP(J+1).NE.2).OR.
     -                (INPTYP(J+2).NE.1.AND.INPTYP(J+2).NE.2))THEN
                      CALL INPMSG(J,'Should have 2 real arguments.')
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(J+1,2,IFAIL1)
                      CALL INPCHK(J+2,2,IFAIL2)
                      IF(STEP.EQ.'X')THEN
                           CALL INPRDR(J+1,VARMIR,DXMIN)
                           CALL INPRDR(J+2,VARMAR,DXMAX)
                      ELSEIF(STEP.EQ.'Y')THEN
                           CALL INPRDR(J+1,VARMIR,DYMIN)
                           CALL INPRDR(J+2,VARMAR,DYMAX)
                      ELSEIF(STEP.EQ.'Z')THEN
                           CALL INPRDR(J+1,VARMIR,DZMIN)
                           CALL INPRDR(J+2,VARMAR,DZMAX)
                      ENDIF
                      IF(VARMIR.EQ.VARMAR)THEN
                           CALL INPMSG(J+1,'Zero range not permitted.')
                           CALL INPMSG(J+2,'See preceding message.')
                           OK=.FALSE.
                      ELSEIF((SCAN.EQ.'X'.AND.(
     -                     MAX(VARMIR,VARMAR).LT.DXMIN.OR.
     -                     MIN(VARMIR,VARMAR).GT.DXMAX)).OR.
     -                     (SCAN.EQ.'Y'.AND.(
     -                     MAX(VARMIR,VARMAR).LT.DYMIN.OR.
     -                     MIN(VARMIR,VARMAR).GT.DYMAX)).OR.
     -                     (SCAN.EQ.'Z'.AND.(
     -                     MAX(VARMIR,VARMAR).LT.DZMIN.OR.
     -                     MIN(VARMIR,VARMAR).GT.DZMAX)))THEN
                           CALL INPMSG(J+1,'Range not inside the area.')
                           CALL INPMSG(J+2,'See preceding message.')
                           OK=.FALSE.
                      ELSE
                           IF(SCAN.EQ.'X')THEN
                                VARMIN=MAX(DXMIN,MIN(VARMIR,VARMAR))
                                VARMAX=MIN(DXMAX,MAX(VARMIR,VARMAR))
                           ELSEIF(SCAN.EQ.'Y')THEN
                                VARMIN=MAX(DYMIN,MIN(VARMIR,VARMAR))
                                VARMAX=MIN(DYMAX,MAX(VARMIR,VARMAR))
                           ELSEIF(SCAN.EQ.'Z')THEN
                                VARMIN=MAX(DZMIN,MIN(VARMIR,VARMAR))
                                VARMAX=MIN(DZMAX,MAX(VARMIR,VARMAR))
                           ENDIF
                      ENDIF
                      INEXT=J+3
                 ENDIF
*   Angle.
            ELSEIF(INPCMP(J,'ANG#LE').NE.0)THEN
                 IF(FLAG(J+1).OR.
     -                (INPTYP(J+1).NE.1.AND.INPTYP(J+1).NE.2))THEN
                      CALL INPMSG(J,'Should have 1 real argument.')
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(J+1,2,IFAIL)
                      CALL INPRDR(J+1,ANGLER,180.0*ATAN(TANPHI)/PI)
                      IF(ABS(ANGLER).GE.80)THEN
                           CALL INPMSG(J+1,
     -                          'Not within the range [-80,80].')
                           OK=.FALSE.
                      ELSE
                           TANPHI=TAN(PI*ANGLER/180.0)
                      ENDIF
                      INEXT=J+2
                 ENDIF
*   Unknown sub-keywords.
            ELSE
                 CALL INPMSG(J,'Not a sub-keyword of SCAN.')
            ENDIF
40          CONTINUE
**  Offset of the plane.
       ELSEIF(INPCMP(I,'OFF#SET').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have an argument.      ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,WAROFF,0.0)
                 INEXT=I+2
            ENDIF
**  The option is not known to the program.
       ELSE
            CALL INPMSG(I,'The option is not known.      ')
            OK=.FALSE.
       ENDIF
20     CONTINUE
*   Display error messages.
       CALL INPERR
*** Check the presence of sufficient gas data.
       IF((.NOT.GASOK(1)).OR.(.NOT.(GASOK(5).OR.HEEDOK.OR.SRIMOK)).OR.
     -      (LDIFF.AND..NOT.GASOK(3)).OR.
     -      (LATTAC.AND..NOT.GASOK(6)))THEN
            PRINT *,' ###### DRFARR ERROR   : Insufficient gas data'//
     -           ' to perform the calculations; ARRIVAL not executed.'
            RETURN
       ENDIF
*** Tell if diffusion is not taken into account.
       IF(.NOT.LDIFF)PRINT *,' ------ DRFARR MESSAGE : Diffusion will'//
     -      ' not be taken into account.'
*** Check the length of the various strings.
       IF(NCFILE.GT.MXNAME)THEN
            PRINT *,' !!!!!! DRFARR WARNING : The dataset name is too'//
     -           ' long and is truncated to '//FILE//'.'
            OK=.FALSE.
            NCFILE=MXNAME
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! DRFARR WARNING : The member name is too'//
     -           ' long and is truncated to '//MEMBER//'.'
            OK=.FALSE.
            NCMEMB=8
       ENDIF
       IF(NCREM.GT.29)THEN
            PRINT *,' !!!!!! DRFARR WARNING : The remark is too'//
     -           ' long and is truncated to '//REMARK//'.'
            OK=.FALSE.
            NCREM=29
       ENDIF
*   Check whether the member already exists.
       IF(LARRWR)THEN
            CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'ARRIVAL',
     -           EXMEMB)
            IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
                 PRINT *,' ------ DRFARR MESSAGE : A copy of the'//
     -                ' member exists; new member will be appended.'
            ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
                 PRINT *,' !!!!!! DRFARR WARNING : A copy of the'//
     -                ' member exists already; member will not be'//
     -                ' written.'
                 LARRWR=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Print some debugging output, to check correct input handling.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DRFARR DEBUG   : '',
     -           ''Step in '',A,'' range: '',2E12.5,'' increment:  '',
     -           E12.5/26X,
     -           ''Scan in '',A,'' range: '',2E12.5,'' tan(angle): '',
     -           E12.5/26X,''Offset '',E12.5/
     -           26X,''bins='',I3,'', lines='',I3,'', order='',I3/
     -           26X,''threshold='',E12.5)')
     -           STEP,UARMIN,UARMAX,USTEP,
     -           SCAN,VARMIN,VARMAX,TANPHI,
     -           NCHA,NLTR,IORDER,THRESH
            WRITE(LUNOUT,'(26X,''Selected electrons: '',100(I3:))')
     -           (MELEC(I),I=1,KELEC)
            IF(LARRWR)THEN
                 WRITE(LUNOUT,'(/26X,''Output dataset="'',A,
     -                 ''", member="'',A,''"''/26X,''Remark="'',A,
     -                 ''"'')') FILE(1:NCFILE),MEMBER(1:NCMEMB),
     -                 REMARK(1:NCREM)
            ELSE
                 WRITE(LUNOUT,'(/26X,''No dataset output.'')')
            ENDIF
       ENDIF
*** Quit now if OK is no longer true and if JFAIL is set.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### DRFARR ERROR   : Instruction is not'//
     -           ' carried out because of the above errors.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### DRFARR ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
       ENDIF
*** Check the parameters, first orthogonality.
       IF(SCAN.EQ.STEP)THEN
            PRINT *,' !!!!!! DRFARR WARNING : The scanning and the'//
     -           ' stepping direction coincide; not executed.'
            RETURN
       ELSEIF(
     -      (((SCAN.EQ.'X'.AND.STEP.EQ.'Y').OR.
     -        (SCAN.EQ.'Y'.AND.STEP.EQ.'X')).AND.
     -       (DZMIN-WAROFF)*(WAROFF-DZMAX).LT.0).OR.
     -      (((SCAN.EQ.'X'.AND.STEP.EQ.'Z').OR.
     -        (SCAN.EQ.'Z'.AND.STEP.EQ.'X')).AND.
     -       (DYMIN-WAROFF)*(WAROFF-DYMAX).LT.0).OR.
     -      (((SCAN.EQ.'Y'.AND.STEP.EQ.'Y').OR.
     -        (SCAN.EQ.'Z'.AND.STEP.EQ.'Z')).AND.
     -       (DXMIN-WAROFF)*(WAROFF-DXMAX).LT.0))THEN
            PRINT *,' !!!!!! DRFARR WARNING : The plane offset'//
     -           ' is located outside the area; not executed.'
            RETURN
       ENDIF
*** Initialise progress printing.
       CALL PROINT('ARRIVAL',3,6)
*** Loop over the electrodes by requested drift status code.
       CALL PROFLD(1,'Electrodes',REAL(5+NWIRE+NSOLID))
       ISWCNT=0
       DO 100 IW=-15,2*MXWIRE+MXSOLI
*   References.
       IPLANE=0
       ISOLID=0
*   Defaults
       XW=0
       YW=0
       ZW=0
       STRID='?'
       NCID=1
*   Skip the tube if non-existing / non-selected / out of area.
       IF(IW.EQ.-15)THEN
            ISWCNT=ISWCNT+1
            CALL PROSTA(1,REAL(ISWCNT))
            IF((.NOT.TUBE).OR.INDPLA(5).EQ.0)GOTO 100
            IF((DXMIN-COTUBE)*(COTUBE-DXMAX).LT.0.OR.
     -           (DYMIN-COTUBE)*(COTUBE-DYMAX).LT.0)GOTO 100
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFARR DEBUG   :'',
     -           '' Tube selected.'')')
            XW=0
            YW=0
            ZW=0
            STRID='the tube'
            NCID=8
*   Skip non-existing / non-selected / out of range planes.
       ELSEIF(IW.GE.-14.AND.IW.LE.-11)THEN
            ISWCNT=ISWCNT+1
            CALL PROSTA(1,REAL(ISWCNT))
            IPLANE=-(IW+10)
            IF((.NOT.YNPLAN(IPLANE)).OR.INDPLA(IPLANE).EQ.0)GOTO 100
            IF(IPLANE.LE.2.AND.
     -           (DXMIN-COPLAN(IPLANE))*(COPLAN(IPLANE)-DXMAX).LT.0)
     -           GOTO 100
            IF(IPLANE.GT.2.AND.
     -           (DYMIN-COPLAN(IPLANE))*(COPLAN(IPLANE)-DYMAX).LT.0)
     -           GOTO 100
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFARR DEBUG   :'',
     -           '' Plane '',I1,'' selected.'')') IPLANE
            IF(IPLANE.LE.2)THEN
                 XW=COPLAN(IPLANE)
                 YW=0
            ELSE
                 XW=0
                 YW=COPLAN(IPLANE)
            ENDIF
            ZW=0
            CALL OUTFMT(REAL(IPLANE),2,STR1,NC1,'LEFT')
            STRID='plane '//STR1(1:NC1)
            NCID=6+NC1
*   Status codes between -10 and 0 are not of interest.
       ELSEIF(IW.GE.-10.AND.IW.LE.0)THEN
            GOTO 100
*   Skip non-existing / non-selected / out of range wires.
       ELSEIF(IW.GE.1.AND.IW.LE.NWIRE)THEN
            ISWCNT=ISWCNT+1
            CALL PROSTA(1,REAL(ISWCNT))
            IF(INDSW(IW).EQ.0)GOTO 100
            IF((DXMIN-X(IW))*(X(IW)-DXMAX).LT.0.OR.
     -           (DYMIN-Y(IW))*(Y(IW)-DYMAX).LT.0)GOTO 100
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFARR DEBUG   :'',
     -           '' Wire '',I1,'' selected.'')') IW
            XW=X(IW)
            YW=Y(IW)
            ZW=0
            CALL OUTFMT(REAL(IW),2,STR1,NC1,'LEFT')
            STRID='wire '//STR1(1:NC1)
            NCID=5+NC1
*   Non-existent wires and replicas are of no interest.
       ELSEIF(IW.GE.NWIRE+1.AND.IW.LE.2*MXWIRE)THEN
            GOTO 100
*   Skip solids that were not selected.
       ELSEIF(IW.GE.2*MXWIRE+1.AND.IW.LE.2*MXWIRE+NSOLID)THEN
            ISWCNT=ISWCNT+1
            CALL PROSTA(1,REAL(ISWCNT))
            ISOLID=IW-2*MXWIRE
            IF(INDSOL(ISOLID).EQ.0)GOTO 100
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFARR DEBUG   :'',
     -           '' Solid '',I1,'' selected.'')') ISOLID
            IF(ISOLTP(ISOLID).EQ.1)THEN
                 XW=REAL(CBUF(ISTART(J)+3))
                 YW=REAL(CBUF(ISTART(J)+4))
                 ZW=REAL(CBUF(ISTART(J)+5))
            ELSEIF(ISOLTP(ISOLID).EQ.2)THEN
                 XW=REAL(CBUF(ISTART(J)+6))
                 YW=REAL(CBUF(ISTART(J)+7))
                 ZW=REAL(CBUF(ISTART(J)+8))
            ELSEIF(ISOLTP(ISOLID).EQ.3)THEN
                 XW=REAL(CBUF(ISTART(J)+4))
                 YW=REAL(CBUF(ISTART(J)+5))
                 ZW=REAL(CBUF(ISTART(J)+6))
            ELSEIF(ISOLTP(ISOLID).EQ.4)THEN
                 XW=REAL(CBUF(ISTART(J)+2))
                 YW=REAL(CBUF(ISTART(J)+3))
                 ZW=REAL(CBUF(ISTART(J)+4))
            ELSEIF(ISOLTP(ISOLID).EQ.5)THEN
                 XW=REAL(CBUF(ISTART(J)+4))
                 YW=REAL(CBUF(ISTART(J)+5))
                 ZW=REAL(CBUF(ISTART(J)+6))
            ELSEIF(ISOLTP(ISOLID).EQ.6)THEN
                 XW=REAL(CBUF(ISTART(J)+3))
                 YW=REAL(CBUF(ISTART(J)+4))
                 ZW=REAL(CBUF(ISTART(J)+5))
            ELSE
                 PRINT *,' !!!!!! DLCARR WARNING : Found a solid of'//
     -                ' unknown type; skipped.'
                 GOTO 100
            ENDIF
            CALL OUTFMT(REAL(ISOLID),2,STR1,NC1,'LEFT')
            STRID='solid '//STR1(1:NC1)
            NCID=6+NC1
*   Non-existent solids are to be skipped.
       ELSEIF(IW.GT.2*MXWIRE+NSOLID)THEN
            GOTO 100
       ENDIF
*** Compute a reasonable range, first set the reference.
       UAROFF=0
       IF(STEP.EQ.'X')THEN
            UAROFF=XW
       ELSEIF(STEP.EQ.'Y')THEN
            UAROFF=YW
       ELSEIF(STEP.EQ.'Z')THEN
            UAROFF=ZW
       ENDIF
*   Compute a range of increments.
       IXM=NINT((UARMIN-UAROFF)/USTEP)-1
       IXP=NINT((UARMAX-UAROFF)/USTEP)+1
*   Fix for the case one is very near an edge.
       IF(UARMIN-USTEP*0.001.GT.UAROFF+IXM*USTEP)IXM=IXM+1
       IF(UARMAX+USTEP*0.001.LT.UAROFF+IXP*USTEP)IXP=IXP-1
       IF(UARMIN-USTEP*0.001.GT.UAROFF+IXM*USTEP)IXM=IXM+1
       IF(UARMAX+USTEP*0.001.LT.UAROFF+IXP*USTEP)IXP=IXP-1
*   Make sure that the number of steps doesn't exceed MXLIST.
       IF(IXP-IXM+1.GT.MXLIST)THEN
            PRINT *,' !!!!!! DRFARR WARNING : No arrival time plot'//
     -           ' for electrode ',IW,' because MXLIST is too small.'
            PRINT *,'                         Consider making X-STEP'//
     -           ' larger or choose a smaller X-RANGE.'
            GOTO 100
       ENDIF
*** Loop over the x points.
       CALL PROFLD(2,'Steps',REAL(IXP-IXM+1))
       DO 110 IX=IXM,IXP
       CALL PROSTA(2,REAL(IX-IXM+1))
*   Initial values for the table.
       ARRLIS(IX-IXM+1,1)=UAROFF+IX*USTEP
       DO 111 I=2,4+3*MXELEC
       ARRLIS(IX-IXM+1,I)=0.0
111    CONTINUE
       ARRFLG(IX-IXM+1)=0
*** Establish track begin and end points for this coordinate.
       IF(STEP.EQ.'X'.AND.SCAN.EQ.'Y')THEN
            XT0=UAROFF+IX*USTEP+TANPHI*(VARMIN-YW)
            XT1=UAROFF+IX*USTEP+TANPHI*(VARMAX-YW)
            YT0=VARMIN
            YT1=VARMAX
            ZT0=WAROFF
            ZT1=WAROFF
            CALL CLIP(XT0,YT0,XT1,YT1,DXMIN,MAX(DYMIN,VARMIN),
     -           DXMAX,MIN(DYMAX,VARMAX),IFAIL)
       ELSEIF(STEP.EQ.'X'.AND.SCAN.EQ.'Z')THEN
            XT0=UAROFF+IX*USTEP+TANPHI*(VARMIN-ZW)
            XT1=UAROFF+IX*USTEP+TANPHI*(VARMAX-ZW)
            YT0=WAROFF
            YT1=WAROFF
            ZT0=VARMIN
            ZT1=VARMAX
            CALL CLIP(XT0,ZT0,XT1,ZT1,DXMIN,MAX(DZMIN,VARMIN),
     -           DXMAX,MIN(DZMAX,VARMAX),IFAIL)
       ELSEIF(STEP.EQ.'Y'.AND.SCAN.EQ.'X')THEN
            XT0=VARMIN
            XT1=VARMAX
            YT0=UAROFF+IX*USTEP+TANPHI*(VARMIN-XW)
            YT1=UAROFF+IX*USTEP+TANPHI*(VARMAX-XW)
            ZT0=WAROFF
            ZT1=WAROFF
            CALL CLIP(YT0,XT0,YT1,XT1,DYMIN,MAX(DXMIN,VARMIN),
     -           DYMAX,MIN(DXMAX,VARMAX),IFAIL)
       ELSEIF(STEP.EQ.'Y'.AND.SCAN.EQ.'Z')THEN
            XT0=WAROFF
            XT1=WAROFF
            YT0=UAROFF+IX*USTEP+TANPHI*(VARMIN-ZW)
            YT1=UAROFF+IX*USTEP+TANPHI*(VARMAX-ZW)
            ZT0=VARMIN
            ZT1=VARMAX
            CALL CLIP(YT0,ZT0,YT1,ZT1,DYMIN,MAX(DZMIN,VARMIN),
     -           DYMAX,MIN(DZMAX,VARMAX),IFAIL)
       ELSEIF(STEP.EQ.'Z'.AND.SCAN.EQ.'X')THEN
            XT0=VARMIN
            XT1=VARMAX
            YT0=WAROFF
            YT1=WAROFF
            ZT0=UAROFF+IX*USTEP+TANPHI*(VARMIN-XW)
            ZT1=UAROFF+IX*USTEP+TANPHI*(VARMAX-XW)
            CALL CLIP(ZT0,XT0,ZT1,XT1,DZMIN,MAX(DXMIN,VARMIN),
     -           DZMAX,MIN(DXMAX,VARMAX),IFAIL)
       ELSEIF(STEP.EQ.'Z'.AND.SCAN.EQ.'Y')THEN
            XT0=WAROFF
            XT1=WAROFF
            YT0=VARMIN
            YT1=VARMAX
            ZT0=UAROFF+IX*USTEP+TANPHI*(VARMIN-YW)
            ZT1=UAROFF+IX*USTEP+TANPHI*(VARMAX-YW)
            CALL CLIP(ZT0,YT0,ZT1,YT1,DZMIN,MAX(DYMIN,VARMIN),
     -           DZMAX,MIN(DYMAX,VARMAX),IFAIL)
       ELSE
            PRINT *,' !!!!!! DRFARR WARNING : Unknown pair of'//
     -           ' stepping and scanning directions; skipped.'
            GOTO 100
       ENDIF
*   Display the track.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFARR DEBUG   : From: '',
     -      ''('',E15.8,'','',E15.8,'','',E15.8,'')''/26X,''To:   '',
     -      ''('',E15.8,'','',E15.8,'','',E15.8,'').'')')
     -      XT0,YT0,ZT0,XT1,YT1,ZT1
*   Be sure the at least part of the track is located inside the area.
       IF(IFAIL.NE.0)THEN
            ARRFLG(IX-IXM+1)=-1
            PRINT *,' !!!!!! DRFARR WARNING : The track is located'//
     -           ' outside the drift-area; no further computations.'
            GOTO 110
       ENDIF
*   Declare the track as set.
       TRFLAG(1)=.TRUE.
*** Prepare the track for interpolation, prepare progress print.
       CALL PRORED(3)
       CALL PROFLD(3,'Track preparation',-1.0)
       CALL PROSTA(3,0.0)
*   Prepare the track, drift-time and diffusion needed only.
       CALL DLCTRP(XT0,YT0,ZT0,XT1,YT1,ZT1,
     -      LDIFF,.FALSE.,LATTAC,NLTR,TGMIN,TGMAX,
     -      SMIN,SMAX,AMIN,AMAX,BMIN,BMAX,IFAIL)
       IF(IFAIL.NE.0)THEN
            ARRFLG(IX-IXM+1)=-2
            PRINT *,' !!!!!! DRFARR WARNING : Track preparation has'//
     -           ' failed; no further arrival time computations.'
            GOTO 110
       ELSEIF(TGMIN.GE.TGMAX)THEN
            ARRFLG(IX-IXM+1)=-3
            PRINT *,' !!!!!! DRFARR WARNING : The range of arrival'//
     -           ' time for the track is nill; no further computations.'
            GOTO 110
       ENDIF
*   Round these values to obtain a sensible time scale.
       IF(LDIFF)THEN
            TGMIN=TGMIN-5*SMAX
            TGMAX=TGMAX+5*SMAX
       ENDIF
       CALL ROUND(TGMIN,TGMAX,NCHA,'LARGER',TSTEP)
       IF(TGMIN.LT.0.0)TGMIN=TGMIN+TSTEP*(1+INT(ABS(TGMIN/TSTEP)))
       TEMIN=TGMIN
       TEMAX=TGMAX
*** Allocate histogram storage, tell that we do this.
       CALL PROFLD(3,'Histogram allocation',-1.0)
       CALL PROSTA(3,0.0)
*   Timing histograms with forced time window.
       IF(WFORCE)THEN
            CALL HISADM('ALLOCATE',IRFTGL,NCHA,
     -           TFORC1,TFORC2,.FALSE.,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' ###### DRFARR ERROR   : Unable to obtain'//
     -                ' histogram space (all, t) ; end of calculations.'
                 RETURN
            ENDIF
            DO 112 I=1,KELEC
            CALL HISADM('ALLOCATE',IRFTEL(I),NCHA,
     -           TFORC1,TFORC2,.FALSE.,IFAIL2)
            IF(IFAIL2.NE.0)THEN
                 PRINT *,' ###### DRFARR ERROR   : Unable to obtain'//
     -                ' histogram space (sel, t) ; end of calculations.'
                 RETURN
            ENDIF
112         CONTINUE
*   Timing histograms with automatic time window.
       ELSE
            CALL HISADM('ALLOCATE',IRFTGL,NCHA,
     -           TGMIN,TGMAX,TGAUTO,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' ###### DRFARR ERROR   : Unable to obtain'//
     -                ' histogram space (all, t) ; end of calculations.'
                 RETURN
            ENDIF
            DO 113 I=1,KELEC
            CALL HISADM('ALLOCATE',IRFTEL(I),NCHA,
     -           TEMIN,TEMAX,TEAUTO,IFAIL2)
            IF(IFAIL2.NE.0)THEN
                 PRINT *,' ###### DRFARR ERROR   : Unable to obtain'//
     -                ' histogram space (sel, t) ; end of calculations.'
                 RETURN
            ENDIF
113         CONTINUE
       ENDIF
*   Origin histograms, also reset the counter.
       IF(SCAN.EQ.'X')THEN
            CALL HISADM('ALLOCATE',IRFXGL,NCHA,XT0,XT1,.FALSE.,IFAIL3)
       ELSEIF(SCAN.EQ.'Y')THEN
            CALL HISADM('ALLOCATE',IRFXGL,NCHA,YT0,YT1,.FALSE.,IFAIL3)
       ELSEIF(SCAN.EQ.'Z')THEN
            CALL HISADM('ALLOCATE',IRFXGL,NCHA,ZT0,ZT1,.FALSE.,IFAIL3)
       ENDIF
       IF(IFAIL3.NE.0)THEN
            PRINT *,' ###### DRFARR ERROR   : Unable to allocate'//
     -           ' histogram (origin all) ; end of calculations.'
            RETURN
       ENDIF
       DO 114 I=1,KELEC
       IF(SCAN.EQ.'X')THEN
            CALL HISADM('ALLOCATE',IRFXEL(I),NCHA,XT0,XT1,.FALSE.,
     -           IFAIL4)
       ELSEIF(SCAN.EQ.'Y')THEN
            CALL HISADM('ALLOCATE',IRFXEL(I),NCHA,YT0,YT1,.FALSE.,
     -           IFAIL4)
       ELSEIF(SCAN.EQ.'Z')THEN
            CALL HISADM('ALLOCATE',IRFXEL(I),NCHA,ZT0,ZT1,.FALSE.,
     -           IFAIL4)
       ENDIF
       IF(IFAIL4.NE.0)THEN
            PRINT *,' ###### DRFARR ERROR   : Unable to allocate'//
     -           ' histogram (origin selected) ; end of calculations.'
            RETURN
       ENDIF
       NELEC(I)=0
114    CONTINUE
*   Cluster count histograms and counter.
       CALL HISADM('INTEGER',IRFNCL,NCHA,0.0,0.0,.TRUE.,IFAIL5)
       CALL HISADM('INTEGER',IRFNEL,NCHA,0.0,0.0,.TRUE.,IFAIL6)
       IF(IFAIL5.NE.0.OR.IFAIL6.NE.0)THEN
            PRINT *,' ###### DRFARR ERROR   : Unable to obtain'//
     -           ' histogram space (cluster) ; end of calculations.'
            RETURN
       ENDIF
       NGLOB=0
*   Debugging output.
       IF(LDEBUG)PRINT *,' ++++++ DRFARR DEBUG   : Time range: Tmin=',
     -      TGMIN,', Tmax=',TGMAX
       IF(LDEBUG)PRINT *,'                         Autoscaling '//
     -      ' global=',TGAUTO,', selected=',TEAUTO
       IF(LDEBUG)PRINT *,'                         Forced window: ',
     -      WFORCE,' Range: ',TFORC1,TFORC2
*** Loop over the tracks, start progress printing.
       CALL PROFLD(3,'Tracks',REAL(NRNDM))
       CALL PROSTA(3,0.0)
       IF(NRNDM.LE.10)THEN
            IPRT=1
       ELSE
            IPRT=10**(INT(LOG10(REAL(2*NRNDM)))-1)
       ENDIF
*   Loop over the tracks.
       DO 140 IRNDM=1,NRNDM
       IF(IRNDM.EQ.IPRT*(IRNDM/IPRT))CALL PROSTA(3,REAL(IRNDM))
*   Initialise clustering.
       CALL TRACLI
*   Reset number of electrons accumulated.
       NPART=0
       NCLUS=0
**  Return to this point for a new cluster.
150    CONTINUE
*   Generate a new point on the track.
       CALL TRACLS(XCL,YCL,ZCL,ECL,NPAIR,EXTRA1,DONE,IFAIL1)
*   Check whether there was a mistake.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! DRFARR WARNING : Clustering error;'//
     -           ' point skipped.'
            ARRFLG(IX-IXM+1)=-6
            CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFXGL,0,0.0,0.0,.TRUE.,IFAIL)
            DO 155 I=1,KELEC
            CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFXEL(I),0,0.0,0.0,.TRUE.,IFAIL)
155         CONTINUE
            CALL HISADM('DELETE',IRFNCL,0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFNEL,0,0.0,0.0,.TRUE.,IFAIL)
            IF(LHISKP)PRINT *,' !!!!!! DRFARR WARNING : Histograms'//
     -           ' not kept - no entries.'
            GOTO 110
*   Check whether this was beyond the last cluster.
       ELSEIF(DONE)THEN
            GOTO 170
       ENDIF
*   Increment cluster count.
       NCLUS=NCLUS+1
*   Find the drift time and the diffusion coefficient for this point.
       CALL DLCTRI(XCL,YCL,ZCL,TCL,ICL,SCL,ACL,BCL,FCL,
     -      LDIFF,.FALSE.,LATTAC,IFAIL)
*   Skip the rest if the status code doesn't match.
       IF(ICL.NE.IW.OR.IFAIL.NE.0)GOTO 150
*   Apply the attachment coefficient if available.
       IF(LATTAC.AND.GASOK(6))NPAIR=NINT(REAL(NPAIR)*BCL)
**  Generate the individual arrival times within the cluster.
       DO 160 ICLS=1,NPAIR
*   Increment counter.
       IF(NPART.GE.MXPART)THEN
            PRINT *,' !!!!!! DRFARR WARNING : Too many particles'//
     -           ' generated on the track; increase MXPART.'
            GOTO 140
       ENDIF
       NPART=NPART+1
*   Register the time.
       IF(LDIFF)THEN
            ARRTIM(1,NPART)=RNDNOR(TCL,SCL)
       ELSE
            ARRTIM(1,NPART)=TCL
       ENDIF
       CALL HISENT(IRFTGL,ARRTIM(1,NPART),1.0)
*   Register the origin.
       IF(SCAN.EQ.'X')THEN
            ARRTIM(2,NPART)=XCL
       ELSEIF(SCAN.EQ.'Y')THEN
            ARRTIM(2,NPART)=YCL
       ELSEIF(SCAN.EQ.'Z')THEN
            ARRTIM(2,NPART)=ZCL
       ENDIF
       CALL HISENT(IRFXGL,ARRTIM(2,NPART),1.0)
*   Increment overall electron counter.
       NGLOB=NGLOB+1
160    CONTINUE
*   Next cluster.
       GOTO 150
*   Last cluster done.
170    CONTINUE
**  Enter the electron count.
       CALL HISENT(IRFNCL,REAL(NCLUS),1.0)
       CALL HISENT(IRFNEL,REAL(NPART),1.0)
*   Find the M'th particle to arrive and enter in a histogram.
       IF(NPART.GE.1)THEN
            CALL SORTRQ(ARRTIM,2,NPART,1)
            DO 161 I=1,KELEC
            IF(MELEC(I).GT.0.AND.MELEC(I).LE.NPART.AND.NPART.GT.0)THEN
                 CALL HISENT(IRFTEL(I),ARRTIM(1,MELEC(I)),1.0)
                 CALL HISENT(IRFXEL(I),ARRTIM(2,MELEC(I)),1.0)
                 NELEC(I)=NELEC(I)+1
            ELSEIF(MELEC(I).LE.0.AND.MELEC(I)+NPART.GE.1)THEN
                 CALL HISENT(IRFTEL(I),ARRTIM(1,NPART+MELEC(I)),1.0)
                 CALL HISENT(IRFXEL(I),ARRTIM(2,NPART+MELEC(I)),1.0)
                 NELEC(I)=NELEC(I)+1
            ENDIF
161         CONTINUE
       ENDIF
*   Proceed with the next random cycle.
140    CONTINUE
*** Check we did indeed collect something.
       IF(NGLOB.LE.0)THEN
            ARRFLG(IX-IXM+1)=-4
            CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFXGL,0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFNCL,0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFNEL,0,0.0,0.0,.TRUE.,IFAIL)
            DO 142 I=1,KELEC
            CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL)
            CALL HISADM('DELETE',IRFXEL(I),0,0.0,0.0,.TRUE.,IFAIL)
142         CONTINUE
            IF(LHISKP)PRINT *,' !!!!!! DRFARR WARNING : Histograms'//
     -           ' not kept - no entries.'
            GOTO 110
       ENDIF
*   Inform about progress.
       CALL PROFLD(3,'Extracting data',-1.0)
       CALL PROSTA(3,0.0)
*** Obtain average, median and spread; first all electrons.
       CALL HISINQ(IRFTGL,LEXIST,LSET,NGBIN,HGMIN,HGMAX,NGENT,
     -      ARRLIS(IX-IXM+1,2),ARRLIS(IX-IXM+1,4))
       IF(LEXIST.AND.NGENT*(HGMAX-HGMIN).GT.0)THEN
            CALL HISSCL(IRFTGL,REAL(NGBIN)/REAL(NGENT*(HGMAX-HGMIN)))
            CALL HISINV(IRFTGL,THRESH,ARRLIS(IX-IXM+1,3),IORDER,IFAIL1)
            CALL HISSCL(IRFTGL,REAL(NGENT*(HGMAX-HGMIN))/REAL(NGBIN))
       ELSE
            IFAIL1=1
       ENDIF
*   Same for selected electron.
       IFAIL2=0
       DO 141 I=1,KELEC
       CALL HISINQ(IRFTEL(I),LEXIST,LSET,NEBIN,HEMIN,HEMAX,NEENT,
     -      ARRLIS(IX-IXM+1,2+3*I),ARRLIS(IX-IXM+1,4+3*I))
       IF(LEXIST.AND.NEENT*(HEMAX-HEMIN).GT.0)THEN
            CALL HISSCL(IRFTEL(I),REAL(NEBIN)/REAL(NEENT*(HEMAX-HEMIN)))
            CALL HISINV(IRFTEL(I),THRESH,ARRLIS(IX-IXM+1,3+3*I),
     -           IORDER,IFAIL2)
            CALL HISSCL(IRFTEL(I),REAL(NEENT*(HEMAX-HEMIN))/REAL(NEBIN))
       ELSE
            IFAIL2=1
       ENDIF
141    CONTINUE
*   Keep track of error conditions.
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)ARRFLG(IX-IXM+1)=-5
*   Plot the curves.
       IF(LELEPL)THEN
*   Inform about progress.
            CALL PROFLD(3,'Plot selected e-',-1.0)
            CALL PROSTA(3,0.0)
            CALL OUTFMT(UAROFF+IX*USTEP,2,STR1,NC1,'LEFT')
            DO 143 I=1,KELEC
            CALL OUTFMT(REAL(MELEC(I)),2,STR2,NC2,'LEFT')
            IF(MELEC(I).GT.0)THEN
                 TITLE='Time electron '//STR2(1:NC2)//' to '//
     -                STRID(1:NCID)//' from '//STR1(1:NC1)//' cm'
                 NC=27+NC1+NC2+NCID
            ELSEIF(MELEC(I).EQ.0)THEN
                 TITLE='Time last electron to '//STRID(1:NCID)//
     -                ' from '//STR1(1:NC1)//' cm'
                 NC=31+NC1+NCID
            ELSE
                 TITLE='Time last'//STR2(1:NC2)//' electron'//
     -                ' to '//STRID(1:NCID)//' from '//STR1(1:NC1)//
     -                ' cm'
                 NC=31+NC1+NC2+NCID
            ENDIF
            CALL HISPLT(IRFTEL(I),'Arrival time [microsec]',
     -           TITLE(1:NC),.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG(TITLE(1:NC))
            CALL GRNEXT
            IF(MELEC(I).GT.0)THEN
                 TITLE='Origin electron '//STR2(1:NC2)//' to '//
     -                STRID(1:NCID)//' from '//STR1(1:NC1)//' cm'
                 NC=29+NC1+NC2+NCID
            ELSEIF(MELEC(I).EQ.0)THEN
                 TITLE='Origin last electron to '//STRID(1:NCID)//
     -                ' from '//STR1(1:NC1)//' cm'
                 NC=33+NC1+NCID
            ELSE
                 TITLE='Origin last'//STR2(1:NC2)//' electron'//
     -                ' to '//STRID(1:NCID)//' from '//STR1(1:NC1)//
     -                ' cm'
                 NC=33+NC1+NC2+NCID
            ENDIF
            CALL HISPLT(IRFXEL(I),'Origin [track coordinate]',
     -           TITLE(1:NC),.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG(TITLE(1:NC))
            CALL GRNEXT
143         CONTINUE
       ENDIF
       IF(LELEPR)THEN
*   Inform about progress.
            CALL PROFLD(3,'Print selected e-',-1.0)
            CALL PROSTA(3,0.0)
            CALL OUTFMT(UAROFF+IX*USTEP,2,STR1,NC1,'LEFT')
            DO 144 I=1,KELEC
            CALL OUTFMT(REAL(MELEC(I)),2,STR2,NC2,'LEFT')
            IF(MELEC(I).GT.0)THEN
                 TITLE='Time electron '//STR2(1:NC2)//' to '//
     -                STRID(1:NCID)//' from '//STR1(1:NC1)//' cm'
                 NC=27+NC1+NC2+NCID
            ELSEIF(MELEC(I).EQ.0)THEN
                 TITLE='Time last electron to '//STRID(1:NCID)//
     -                ' from '//STR1(1:NC1)//' cm'
                 NC=31+NC1+NCID
            ELSE
                 TITLE='Time last'//STR2(1:NC2)//' electron'//
     -                ' to '//STRID(1:NCID)//' from '//STR1(1:NC1)//
     -                ' cm'
                 NC=31+NC1+NC2+NCID
            ENDIF
            CALL HISPRT(IRFTEL(I),'Arrival time [microsec]',
     -           TITLE(1:NC))
            IF(MELEC(I).GT.0)THEN
                 TITLE='Origin electron '//STR2(1:NC2)//' to '//
     -                STRID(1:NCID)//' from '//STR1(1:NC1)//' cm'
                 NC=29+NC1+NC2+NCID
            ELSEIF(MELEC(I).EQ.0)THEN
                 TITLE='Origin last electron to '//STRID(1:NCID)//
     -                ' from '//STR1(1:NC1)//' cm'
                 NC=33+NC1+NCID
            ELSE
                 TITLE='Origin last'//STR2(1:NC2)//' electron'//
     -                ' to '//STRID(1:NCID)//' from '//STR1(1:NC1)//
     -                ' cm'
                 NC=33+NC1+NC2+NCID
            ENDIF
            CALL HISPRT(IRFXEL(I),'Origin [track coordinate]',
     -           TITLE(1:NC))
144         CONTINUE
       ENDIF
*   Global plot.
       IF(LGLBPL)THEN
*   Inform about progress.
            CALL PROFLD(3,'Plot all e-',-1.0)
            CALL PROSTA(3,0.0)
            CALL OUTFMT(UAROFF+IX*USTEP,2,STR1,NC1,'LEFT')
            CALL HISPLT(IRFTGL,'Arrival time [microsec]',
     -           'Time all electrons to '//STRID(1:NCID)//' from '//
     -           STR1(1:NC1)//' cm',.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG('Overall arrival time distribution.      ')
            CALL GRNEXT
            CALL HISPLT(IRFXGL,'Origin [cm]',
     -           'Origin all electrons to '//STRID(1:NCID)//' from '//
     -           STR1(1:NC1)//' cm',.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG('Origin of the electrons.                ')
            CALL GRNEXT
            CALL HISPLT(IRFNCL,'Number of clusters',
     -           'Clusters per track at '//STR1(1:NC1)//' cm',
     -           .TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG('Clusters per track.                     ')
            CALL GRNEXT
            CALL HISPLT(IRFNEL,'Number of electrons',
     -           'Accepted electrons at '//STR1(1:NC1)//' cm',
     -           .TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRALOG('Electrons per track.                    ')
            CALL GRNEXT
       ENDIF
       IF(LGLBPR)THEN
*   Inform about progress.
            CALL PROFLD(3,'Print all e-',-1.0)
            CALL PROSTA(3,0.0)
            CALL OUTFMT(UAROFF+IX*USTEP,2,STR1,NC1,'LEFT')
            CALL HISPRT(IRFTGL,'Arrival time [microsec]',
     -           'Time all electrons to '//STRID(1:NCID)//' from '//
     -           STR1(1:NC1)//' cm')
            CALL HISPRT(IRFXGL,'y-Origin [cm]',
     -           'Origin all electrons to '//STRID(1:NCID)//' from '//
     -           STR1(1:NC1)//' cm')
       ENDIF
*** Get rid of the histograms, unless KEEP has been specified.
       IF(LHISKP)THEN
*   Inform about progress.
            CALL PROFLD(3,'Saving histograms',-1.0)
            CALL PROSTA(3,0.0)
            JALL=JALL+1
            CALL OUTFMT(REAL(JALL),2,STR1,NC1,'LEFT')
            CALL HISSAV(IRFTGL,'ALL_'//STR1(1:NC1),IFAIL1)
            CALL OUTFMT(UAROFF+IX*USTEP,2,STR3,NC3,'LEFT')
            IF(IFAIL1.EQ.0)THEN
                 PRINT *,' ------ DRFARR MESSAGE : Arrival time'//
     -                ' histogram of all electrons to '//
     -                STRID(1:NCID)//' from '//STR3(1:NC3)//
     -                ' cm is kept as ALL_'//STR1(1:NC1)//'.'
            ELSE
                 PRINT *,' !!!!!! DRFARR WARNING : Arrival time'//
     -                ' histogram of all electrons to '//
     -                STRID(1:NCID)//' from '//STR3(1:NC3)//
     -                ' cm has not been saved.'
                 CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL)
            ENDIF
            DO 401 II=1,KELEC
            JSEL=JSEL+1
            CALL OUTFMT(REAL(JSEL),2,STR4,NC4,'LEFT')
            CALL HISSAV(IRFTEL(II),'SEL_'//STR4(1:NC4),IFAIL1)
            CALL OUTFMT(REAL(MELEC(II)),2,STR1,NC1,'LEFT')
            CALL OUTFMT(UAROFF+IX*USTEP,2,STR3,NC3,'LEFT')
            IF(IFAIL1.EQ.0)THEN
                 PRINT *,' ------ DRFARR MESSAGE : Arrival time'//
     -                ' histogram of electron '//STR1(1:NC1)//
     -                ' to '//STRID(1:NCID)//' from '//
     -                STR3(1:NC3)//' cm is kept'//
     -                ' as SEL_'//STR4(1:NC4)//'.'
            ELSE
                 PRINT *,' !!!!!! DRFARR WARNING : Arrival time'//
     -                ' histogram of electron '//STR1(1:NC1)//
     -                ' to '//STRID(1:NCID)//' from '//
     -                STR3(1:NC3)//' cm has not been saved.'
                 CALL HISADM('DELETE',IRFTEL(II),0,0.0,0.0,.TRUE.,IFAIL)
            ENDIF
401         CONTINUE
       ELSE
            CALL HISADM('DELETE',IRFTGL,0,0.0,0.0,.TRUE.,IFAIL)
            DO 403 I=1,KELEC
            CALL HISADM('DELETE',IRFTEL(I),0,0.0,0.0,.TRUE.,IFAIL)
403         CONTINUE
       ENDIF
       CALL HISADM('DELETE',IRFXGL,0,0.0,0.0,.TRUE.,IFAIL)
       CALL HISADM('DELETE',IRFNCL,0,0.0,0.0,.TRUE.,IFAIL)
       CALL HISADM('DELETE',IRFNEL,0,0.0,0.0,.TRUE.,IFAIL)
       DO 404 I=1,KELEC
       CALL HISADM('DELETE',IRFXEL(I),0,0.0,0.0,.TRUE.,IFAIL)
404    CONTINUE
*** Next x-coordinate.
110    CONTINUE
*** End of full progress printing in 3 loops.
       CALL PRORED(2)
*** Plot an overview for this wire.
       IF(LARRPL)THEN
**  Inform about progress.
            CALL PROFLD(2,'Overview plots',-1.0)
            CALL PROSTA(2,0.0)
**  Plots of the average arrival time.
            TMAX=-1.0
            DO 210 I=1,IXP-IXM+1
            IF(ARRFLG(I).NE.0)GOTO 210
            DO 220 J=2,2+3*KELEC,3
            TMAX=MAX(TMAX,ARRLIS(I,J))
220         CONTINUE
210         CONTINUE
*   No valid data.
            IF(TMAX.LE.0.0)THEN
                 PRINT *,' !!!!!! DRFARR WARNING : Insufficient'//
     -                ' "average" data for '//STRID(1:NCID)//
     -                '; plot not made.'
            ELSE
*   Open frame.
                 CALL GRCART(ARRLIS(1,1),0.0,ARRLIS(IXP-IXM+1,1),TMAX,
     -                'Distance from electrode centre [cm]',
     -                'Drift time [microsec]',
     -                'Average arrival times for '//STRID(1:NCID))
*   Add some comments.
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
                 IF(PARTID.NE.'Unknown')
     -                CALL GRCOMM(3,'Particle: '//PARTID)
                 CALL OUTFMT(180*ATAN(TANPHI)/PI,2,STR2,NC2,'LEFT')
                 CALL GRCOMM(4,'Angle: '//STR2(1:NC2)//' degrees')
*   Plot each of the curves in turn.
                 DO 230 I=2,2+3*KELEC,3
                 IF(I.EQ.2)THEN
                      CALL GRATTS('FUNCTION-1','POLYLINE')
                      CALL GRATTS('FUNCTION-1','POLYMARKER')
                 ELSE
                      CALL GRATTS('FUNCTION-2','POLYLINE')
                      CALL GRATTS('FUNCTION-2','POLYMARKER')
                 ENDIF
                 NPLOT=0
                 DO 240 IX=1,IXP-IXM+1
                 IF(ARRFLG(IX).EQ.0)THEN
                      NPLOT=NPLOT+1
                      XPL(NPLOT)=ARRLIS(IX,1)
                      YPL(NPLOT)=ARRLIS(IX,I)
                 ELSE
                      IF(NPLOT.GT.1)THEN
                           CALL GPL(NPLOT,XPL,YPL)
                      ELSEIF(NPLOT.EQ.1)THEN
                           CALL GPM(1,XPL,YPL)
                      ENDIF
                      NPLOT=0
                 ENDIF
240              CONTINUE
                 IF(NPLOT.GT.1)THEN
                      CALL GPL(NPLOT,XPL,YPL)
                 ELSEIF(NPLOT.EQ.1)THEN
                      CALL GPM(1,XPL,YPL)
                 ENDIF
230              CONTINUE
*   Close plot, record.
                 CALL GRALOG('Overview of average arrival times.')
                 CALL GRNEXT
            ENDIF
**  Plots of the median arrival time.
            TMAX=-1.0
            DO 211 I=1,IXP-IXM+1
            IF(ARRFLG(I).NE.0)GOTO 211
            DO 221 J=3,3+3*KELEC,3
            TMAX=MAX(TMAX,ARRLIS(I,J))
221         CONTINUE
211         CONTINUE
*   No valid data.
            IF(TMAX.LE.0.0)THEN
                 PRINT *,' !!!!!! DRFARR WARNING : Insufficient'//
     -                ' "median" data for '//STRID(1:NCID)//
     -                '; plot not made.'
            ELSE
*   Open frame.
                 CALL GRCART(ARRLIS(1,1),0.0,ARRLIS(IXP-IXM+1,1),TMAX,
     -                'Distance from electrode centre [cm]',
     -                'Drift time [microsec]',
     -                'Median arrival times for '//STRID(1:NCID))
*   Add some comments.
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
                 IF(PARTID.NE.'Unknown')
     -                CALL GRCOMM(3,'Particle: '//PARTID)
                 CALL OUTFMT(180*ATAN(TANPHI)/PI,2,STR2,NC2,'LEFT')
                 CALL GRCOMM(4,'Angle: '//STR2(1:NC2)//' degrees')
*   Plot each of the curves in turn.
                 DO 231 I=3,3+3*KELEC,3
                 IF(I.EQ.3)THEN
                      CALL GRATTS('FUNCTION-1','POLYLINE')
                      CALL GRATTS('FUNCTION-1','POLYMARKER')
                 ELSE
                      CALL GRATTS('FUNCTION-2','POLYLINE')
                      CALL GRATTS('FUNCTION-2','POLYMARKER')
                 ENDIF
                 NPLOT=0
                 DO 241 IX=1,IXP-IXM+1
                 IF(ARRFLG(IX).EQ.0)THEN
                      NPLOT=NPLOT+1
                      XPL(NPLOT)=ARRLIS(IX,1)
                      YPL(NPLOT)=ARRLIS(IX,I)
                 ELSE
                      IF(NPLOT.GT.1)THEN
                           CALL GPL(NPLOT,XPL,YPL)
                      ELSEIF(NPLOT.EQ.1)THEN
                           CALL GPM(1,XPL,YPL)
                      ENDIF
                      NPLOT=0
                 ENDIF
241              CONTINUE
                 IF(NPLOT.GT.1)THEN
                      CALL GPL(NPLOT,XPL,YPL)
                 ELSEIF(NPLOT.EQ.1)THEN
                      CALL GPM(1,XPL,YPL)
                 ENDIF
231              CONTINUE
*   Close plot, record.
                 CALL GRALOG('Overview of median arrival times.')
                 CALL GRNEXT
            ENDIF
**  Plots of the arrival time spreads.
            TMAX=-1.0
            DO 212 I=1,IXP-IXM+1
            IF(ARRFLG(I).NE.0)GOTO 212
            DO 222 J=4,4+3*KELEC,3
            TMAX=MAX(TMAX,ARRLIS(I,J))
222         CONTINUE
212         CONTINUE
*   No valid data.
            IF(TMAX.LE.0.0)THEN
                 PRINT *,' !!!!!! DRFARR WARNING : Insufficient'//
     -                ' "spread" data for '//STRID(1:NCID)//
     -                '; plot not made.'
            ELSE
*   Open frame.
                 CALL GRCART(ARRLIS(1,1),0.0,ARRLIS(IXP-IXM+1,1),TMAX,
     -                'Distance from electrode centre [cm]',
     -                'RMS of arrival time [microsec]',
     -                'Arrival time spread for '//STRID(1:NCID))
*   Add some comments.
                 IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
                 IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
                 IF(PARTID.NE.'Unknown')
     -                CALL GRCOMM(3,'Particle: '//PARTID)
                 CALL OUTFMT(180*ATAN(TANPHI)/PI,2,STR2,NC2,'LEFT')
                 CALL GRCOMM(4,'Angle: '//STR2(1:NC2)//' degrees')
*   Plot each of the curves in turn.
                 DO 232 I=4,4+3*KELEC,3
                 IF(I.EQ.4)THEN
                      CALL GRATTS('FUNCTION-1','POLYLINE')
                      CALL GRATTS('FUNCTION-1','POLYMARKER')
                 ELSE
                      CALL GRATTS('FUNCTION-2','POLYLINE')
                      CALL GRATTS('FUNCTION-2','POLYMARKER')
                 ENDIF
                 NPLOT=0
                 DO 242 IX=1,IXP-IXM+1
                 IF(ARRFLG(IX).EQ.0)THEN
                      NPLOT=NPLOT+1
                      XPL(NPLOT)=ARRLIS(IX,1)
                      YPL(NPLOT)=ARRLIS(IX,I)
                 ELSE
                      IF(NPLOT.GT.1)THEN
                           CALL GPL(NPLOT,XPL,YPL)
                      ELSEIF(NPLOT.EQ.1)THEN
                           CALL GPM(1,XPL,YPL)
                      ENDIF
                      NPLOT=0
                 ENDIF
242              CONTINUE
                 IF(NPLOT.GT.1)THEN
                      CALL GPL(NPLOT,XPL,YPL)
                 ELSEIF(NPLOT.EQ.1)THEN
                      CALL GPM(1,XPL,YPL)
                 ENDIF
232              CONTINUE
*   Close plot, record.
                 CALL GRALOG('Overview of arrival times spreads.')
                 CALL GRNEXT
            ENDIF
       ENDIF
*** Output the data to a dataset if requested.
       IF(LARRWR)THEN
**  Inform about progress.
            CALL PROFLD(2,'Dataset output',-1.0)
            CALL PROSTA(2,0.0)
**  Open the dataset.
            CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! DRFARR WARNING : Opening the file'//
     -                FILE(1:NCFILE)//' failed ; write flag cancelled.'
                 LARRWR=.FALSE.
            ENDIF
            CALL DSNLOG(FILE,'Arrival   ','Sequential','Write     ')
*   Now write a heading record to the file.
            CALL DATTIM(DATE,TIME)
            IF(REMARK.NE.'None')THEN
                 WRITE(12,'(''% Created '',A8,'' At '',A8,1X,A8,
     -                '' ARRIVAL  "'',A29,''"'')',ERR=2010,IOSTAT=IOS)
     -                DATE,TIME,MEMBER,REMARK
            ELSE
                 WRITE(12,'(''% Created '',A8,'' At '',A8,1X,A8,
     -                '' ARRIVAL  "'',A15,'' phi '',F9.2,''"'')',
     -                ERR=2010,IOSTAT=IOS)
     -                DATE,TIME,MEMBER,STRID,180*ATAN(TANPHI)/PI
            ENDIF
*   Specify the number of records to be written.
            WRITE(12,'('' Threshold setting: '',E15.8/
     -           '' Angle to vertical: '',E15.8/
     -           '' Random cycles:     '',I10/
     -           '' Selected electrons:'',I10)',ERR=2010,IOSTAT=IOS)
     -           THRESH,180*ATAN(TANPHI)/PI,NRNDM,KELEC
*   Indicate the columns.
            WRITE(12,'(''        Distance Electron    Average time'',
     -           ''  Threshold time     Time spread Notes''/
     -           ''            [cm]               [microsec]'',
     -           ''      [microsec]      [microsec]'')',
     -           ERR=2010,IOSTAT=IOS)
**  Write the data itself, interpreting the various flags.
            DO 300 IX=1,IXP-IXM+1
*   Prepare a string containing roughly the data.
            IF(ARRFLG(IX).EQ.0)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',3(1X,E15.8),
     -                1X,''No problem'')',ERR=2010,IOSTAT=IOS)
     -                (ARRLIS(IX,I),I=1,4)
                 DO 301 K=1,KELEC
                 WRITE(12,'(17X,I8,3(1X,E15.8))',ERR=2010,IOSTAT=IOS)
     -                MELEC(K),(ARRLIS(IX,1+3*K+I),I=1,3)
301              CONTINUE
            ELSEIF(ARRFLG(IX).EQ.-1)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',
     -                3(''   Not available''),
     -                1X,''! Track located outside the area.'')',
     -                ERR=2010,IOSTAT=IOS) ARRLIS(IX,1)
            ELSEIF(ARRFLG(IX).EQ.-2)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',
     -                3(''   Not available''),
     -                1X,''! Track preparation failed.'')',
     -                ERR=2010,IOSTAT=IOS) ARRLIS(IX,1)
            ELSEIF(ARRFLG(IX).EQ.-3)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',
     -                3(''   Not available''),
     -                1X,''! Track has zero time range.'')',
     -                ERR=2010,IOSTAT=IOS) ARRLIS(IX,1)
            ELSEIF(ARRFLG(IX).EQ.-4)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',
     -                3(''   Not available''),
     -                1X,''! No track data collected.'')',
     -                ERR=2010,IOSTAT=IOS) ARRLIS(IX,1)
            ELSEIF(ARRFLG(IX).EQ.-5)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',3(1X,E15.8),
     -                1X,''! Poor statistics or data.'')',
     -                ERR=2010,IOSTAT=IOS) (ARRLIS(IX,I),I=1,4)
                 DO 302 K=1,KELEC
                 WRITE(12,'(17X,I8,3(1X,E15.8))',ERR=2010,IOSTAT=IOS)
     -                MELEC(K),(ARRLIS(IX,1+3*K+I),I=1,3)
302              CONTINUE
            ELSEIF(ARRFLG(IX).EQ.-6)THEN
                 WRITE(12,'(1X,E15.8,1X,''     all'',
     -                3(''   Not available''),
     -                1X,''! Clustering error.'')',
     -                ERR=2010,IOSTAT=IOS) ARRLIS(IX,1)
            ELSE
                 WRITE(12,'(1X,E15.8,1X,''     all'',3(1X,E15.8),
     -                1X,''# Unknown status flag.'')',
     -                ERR=2010,IOSTAT=IOS) (ARRLIS(IX,I),I=1,4)
                 DO 303 K=1,KELEC
                 WRITE(12,'(17X,I8,3(1X,E15.8))',ERR=2010,IOSTAT=IOS)
     -                MELEC(K),(ARRLIS(IX,1+3*K+I),I=1,3)
303              CONTINUE
            ENDIF
300         CONTINUE
*   Close the file, if openend.
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       ENDIF
*** Save the results, if desired.
       IF(LARRKP)THEN
*   Store the matrix dimensions for all matrix saves.
            ISIZ(1)=IXP-IXM+1
            IDIM(1)=MXLIST
*   Format the output sequence number.
            JOVER=JOVER+1
            CALL OUTFMT(REAL(JOVER),2,STR1,NC1,'LEFT')
*   Save the x-coordinates.
            IF(STEP.EQ.'X')THEN
                 CALL MATSAV(ARRLIS(1,1),1,IDIM,ISIZ,
     -                'X_'//STR1(1:NC1),IFAIL1)
            ELSEIF(STEP.EQ.'Y')THEN
                 CALL MATSAV(ARRLIS(1,1),1,IDIM,ISIZ,
     -                'Y_'//STR1(1:NC1),IFAIL1)
            ELSEIF(STEP.EQ.'Z')THEN
                 CALL MATSAV(ARRLIS(1,1),1,IDIM,ISIZ,
     -                'Z_'//STR1(1:NC1),IFAIL1)
            ENDIF
*   Save the all-electron x(t) relation.
            CALL MATSAV(ARRLIS(1,2),1,IDIM,ISIZ,
     -           'MEAN_'//STR1(1:NC1),IFAIL2)
            CALL MATSAV(ARRLIS(1,3),1,IDIM,ISIZ,
     -           'MEDIAN_'//STR1(1:NC1),IFAIL3)
            CALL MATSAV(ARRLIS(1,4),1,IDIM,ISIZ,
     -           'RMS_'//STR1(1:NC1),IFAIL4)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           IFAIL4.NE.0)PRINT *,' !!!!!! DRFARR WARNING :'//
     -           ' Error saving all-electron x(t) relation.'
*   Save the selected electron sequence numbers.
            DO 400 K=1,KELEC
            CALL OUTFMT(REAL(K),2,STR2,NC2,'LEFT')
            CALL NUMSAV(REAL(MELEC(K)),'E_'//STR2(1:NC2),IFAIL1)
400         CONTINUE
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! DRFARR WARNING : Unable'//
     -           ' to save the electron sequence numbers.'
*   Save the selected electron x(t) relations.
            DO 410 K=1,KELEC
            CALL OUTFMT(REAL(K),2,STR2,NC2,'LEFT')
            CALL MATSAV(ARRLIS(1,2+3*K),1,IDIM,ISIZ,
     -           'MEAN'//STR2(1:NC2)//'_'//STR1(1:NC1),IFAIL2)
            CALL MATSAV(ARRLIS(1,3+3*K),1,IDIM,ISIZ,
     -           'MEDIAN'//STR2(1:NC2)//'_'//STR1(1:NC1),IFAIL3)
            CALL MATSAV(ARRLIS(1,4+3*K),1,IDIM,ISIZ,
     -           'RMS'//STR2(1:NC2)//'_'//STR1(1:NC1),IFAIL4)
            IF(IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.IFAIL4.NE.0)
     -           PRINT *,' !!!!!! DRFARR WARNING : Error saving x(t)'//
     -           ' for selected electron '//STR2(1:NC2)//'.'
410         CONTINUE
       ENDIF
*** Proceed with the next wire.
100    CONTINUE
*** End of progress printing.
       CALL PROEND
*** Register the amount of CPU time used by this routine.
       CALL TIMLOG('Calculating arrival times:              ')
       RETURN
*** Handle I/O problems.
2010   CONTINUE
       PRINT *,' ###### DRFARR ERROR   : Error while'//
     -      ' writing the arrival data set ; attempt to close.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### DRFARR ERROR   : Unable to close the data set'//
     -      ' of the arrival times ; results not predictable.'
       CALL INPIOS(IOS)
       END
