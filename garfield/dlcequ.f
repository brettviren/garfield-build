CDECK  ID>, DLCEQU.
       SUBROUTINE DLCEQU(ALPHA,ETA,IFAIL)
*-----------------------------------------------------------------------
*   DLCEQU - Computes equilibrated alpha's and eta's over the current
*            drift line.
*   VARIABLES :
*   (Last changed on 26/10/07.)
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
       INTEGER ILOC,I,J,IFAIL,IRES
       REAL GASTWN,GASATT,XPOS1,YPOS1,XPOS2,YPOS2,EX,EY,EZ,ETOT,
     -      VOLT,ALPHA(MXLIST),ETA(MXLIST),STEP,SCALE,SUB1,SUB2,
     -      BX,BY,BZ,BTOT,DRES
       DOUBLE PRECISION VD(3),VTERM(3),WG6(6),TG6(6)
       LOGICAL TRY1,TRY2,DONE
       LOGICAL LTOWN,LATTA
       EXTERNAL GASTWN,GASATT
*** Locations and weights for 6-point Gaussian integration.
       DATA (TG6(I),WG6(I),I=1,6) /
     -      -0.93246 95142 03152 028D0, 0.17132 44923 79170 345D0,
     -      -0.66120 93864 66264 514D0, 0.36076 15730 48138 608D0,
     -      -0.23861 91860 83196 909D0, 0.46791 39345 72691 047D0,
     -       0.23861 91860 83196 909D0, 0.46791 39345 72691 047D0,
     -       0.66120 93864 66264 514D0, 0.36076 15730 48138 608D0,
     -       0.93246 95142 03152 028D0, 0.17132 44923 79170 345D0/
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE DLCEQU ///'
*** Assume this will fail.
       IFAIL=1
*** Make sure that electron drift velocities are available.
       IF(.NOT.(GASOK(1).AND.GASOK(4)))THEN
            PRINT *,' !!!!!! DLCEQU WARNING : Electron drift velocity'//
     -           ' or avalanche data missing; avalanche not treated.'
            RETURN
       ENDIF
*   Establish the flags.
       LTOWN=.FALSE.
       LATTA=.FALSE.
       IF(GASOK(4))LTOWN=.TRUE.
       IF(GASOK(6))LATTA=.TRUE.
*** Check that a drift line exists.
       IF(IPTYPE.NE.1)THEN
            PRINT *,' !!!!!! DLCEQU WARNING : Current drift line is'//
     -           ' not for an electron; avalanche not processed.'
            RETURN
       ELSEIF(NU.LT.2)THEN
            RETURN
       ENDIF
*** See whether the drift line ends in a wire.
       IF(ISTAT.GT.0)THEN
            IRES=MOD(ISTAT,MXWIRE)
            DRES=D(IRES)
            D(IRES)=DRES/2
       ELSE
            DRES=-1
            IRES=0
       ENDIF
*** Loop a first time over the drift line to check for returns.
       DO 100 I=1,NU-1
*   Scaling factor for projected length.
       IF(LAVPRO)THEN
            VD(1)=0
            VD(2)=0
            VD(3)=0
            DO 330 J=1,6
            CALL DLCVEL(
     -           XU(I)+(1+TG6(J))/2*(XU(I+1)-XU(I)),
     -           YU(I)+(1+TG6(J))/2*(YU(I+1)-YU(I)),
     -           ZU(I)+(1+TG6(J))/2*(ZU(I+1)-ZU(I)),
     -           VTERM,QPCHAR,IPTYPE,ILOC)
            VD(1)=VD(1)+WG6(J)*VTERM(1)
            VD(2)=VD(2)+WG6(J)*VTERM(2)
            VD(3)=VD(3)+WG6(J)*VTERM(3)
330         CONTINUE
            IF(((XU(I+1)-XU(I))**2+
     -           (YU(I+1)-YU(I))**2+
     -           (ZU(I+1)-ZU(I))**2)*
     -           (VD(1)**2+VD(2)**2+VD(3)**2).LE.0)THEN
                 SCALE=0
            ELSE
                 SCALE=((XU(I+1)-XU(I))*VD(1)+
     -                (YU(I+1)-YU(I))*VD(2)+
     -                (ZU(I+1)-ZU(I))*VD(3))/
     -                SQRT(((XU(I+1)-XU(I))**2+
     -                (YU(I+1)-YU(I))**2+
     -                (ZU(I+1)-ZU(I))**2)*
     -                (VD(1)**2+VD(2)**2+VD(3)**2))
            ENDIF
       ELSE
            SCALE=1
       ENDIF
*   Length of the step.
       XPOS1=REAL(XU(I))
       YPOS1=REAL(YU(I))
       IF(POLAR)CALL CFMRTC(XPOS1,YPOS1,XPOS1,YPOS1,1)
       XPOS2=REAL(XU(I+1))
       YPOS2=REAL(YU(I+1))
       IF(POLAR)CALL CFMRTC(XPOS2,YPOS2,XPOS2,YPOS2,1)
       STEP=SQRT((XPOS1-XPOS2)**2+(YPOS1-YPOS2)**2+(ZU(I+1)-ZU(I))**2)
*   Compute the mean Townsend and attachment coefficients.
       ALPHA(I)=0
       ETA(I)=0
       DO 320 J=1,6
       CALL EFIELD(
     -      REAL(XU(I)+(1+TG6(J))/2*(XU(I+1)-XU(I))),
     -      REAL(YU(I)+(1+TG6(J))/2*(YU(I+1)-YU(I))),
     -      REAL(ZU(I)+(1+TG6(J))/2*(ZU(I+1)-ZU(I))),
     -      EX,EY,EZ,ETOT,VOLT,0,ILOC)
       CALL BFIELD(
     -      REAL(XU(I)+(1+TG6(J))/2*(XU(I+1)-XU(I))),
     -      REAL(YU(I)+(1+TG6(J))/2*(YU(I+1)-YU(I))),
     -      REAL(ZU(I)+(1+TG6(J))/2*(ZU(I+1)-ZU(I))),
     -      BX,BY,BZ,BTOT)
       IF(LTOWN)ALPHA(I)=ALPHA(I)+WG6(J)*GASTWN(EX,EY,EZ,BX,BY,BZ)
       IF(LATTA)ETA(I)=ETA(I)+WG6(J)*GASATT(EX,EY,EZ,BX,BY,BZ)
320    CONTINUE
       ALPHA(I)=ALPHA(I)*STEP*SCALE/2
       ETA(I)=ETA(I)*STEP*SCALE/2
*   Next point on the drift line.
100    CONTINUE
*** Skip equilibration if there projection hasn't been requested.
       IF(.NOT.LAVPRO)THEN
            IFAIL=0
            IF(IRES.GT.0)D(IRES)=DRES
            RETURN
       ENDIF
*** Try to alpha-equilibrate the returning parts.
       DO 110 I=1,NU-1
       IF(ALPHA(I).LT.0)THEN
*   Targets for subtracting.
            SUB1=-ALPHA(I)/2
            SUB2=-ALPHA(I)/2
            TRY1=.TRUE.
            TRY2=.TRUE.
*   Try to subtract half in earlier points.
            DO 120 J=1,I-1
            IF(ALPHA(I-J).GT.SUB1)THEN
                 ALPHA(I-J)=ALPHA(I-J)-SUB1
                 ALPHA(I)=ALPHA(I)+SUB1
                 SUB1=0
                 GOTO 130
            ELSEIF(ALPHA(I-J).GT.0)THEN
                 ALPHA(I)=ALPHA(I)+ALPHA(I-J)
                 SUB1=SUB1-ALPHA(I-J)
                 ALPHA(I-J)=0
            ENDIF
120         CONTINUE
            TRY1=.FALSE.
130         CONTINUE
*   Try to subtract the other half in later points.
            DO 140 J=1,NU-I-1
            IF(ALPHA(I+J).GT.SUB2)THEN
                 ALPHA(I+J)=ALPHA(I+J)-SUB2
                 ALPHA(I)=ALPHA(I)+SUB2
                 SUB2=0
                 GOTO 150
            ELSEIF(ALPHA(I+J).GT.0)THEN
                 ALPHA(I)=ALPHA(I)+ALPHA(I+J)
                 SUB2=SUB2-ALPHA(I+J)
                 ALPHA(I+J)=0
            ENDIF
140         CONTINUE
            TRY2=.FALSE.
150         CONTINUE
*   Done if both sides have margin left.
            DONE=.FALSE.
            IF(TRY1.AND.TRY2)THEN
                 DONE=.TRUE.
*   Try lower side again.
            ELSEIF(TRY1)THEN
                 SUB1=-ALPHA(I)
                 DO 160 J=1,I-1
                 IF(ALPHA(I-J).GT.SUB1)THEN
                      ALPHA(I-J)=ALPHA(I-J)-SUB1
                      ALPHA(I)=ALPHA(I)+SUB1
                      SUB1=0
                      DONE=.TRUE.
                      GOTO 170
                 ELSEIF(ALPHA(I-J).GT.0)THEN
                      ALPHA(I)=ALPHA(I)+ALPHA(I-J)
                      SUB1=SUB1-ALPHA(I-J)
                      ALPHA(I-J)=0
                 ENDIF
160              CONTINUE
170              CONTINUE
*   Try upper side again.
            ELSEIF(TRY2)THEN
                 SUB2=-ALPHA(I)
                 DO 180 J=1,NU-I-1
                 IF(ALPHA(I+J).GT.SUB2)THEN
                      ALPHA(I+J)=ALPHA(I+J)-SUB2
                      ALPHA(I)=ALPHA(I)+SUB2
                      SUB2=0
                      DONE=.TRUE.
                      GOTO 190
                 ELSEIF(ALPHA(I+J).GT.0)THEN
                      ALPHA(I)=ALPHA(I)+ALPHA(I+J)
                      SUB2=SUB2-ALPHA(I+J)
                      ALPHA(I+J)=0
                 ENDIF
180              CONTINUE
190              CONTINUE
            ENDIF
*   See whether we succeeded.
            IF(.NOT.DONE)THEN
                 PRINT *,' !!!!!! DLCEQU WARNING : Unable to even out'//
     -                ' backwards alpha steps; inaccurate avalanche.'
                 IF(IRES.GT.0)D(IRES)=DRES
                 RETURN
            ENDIF
       ENDIF
110    CONTINUE
*** Try to eta-equilibrate the returning parts.
       DO 210 I=1,NU-1
       IF(ETA(I).LT.0)THEN
*   Targets for subtracting.
            SUB1=-ETA(I)/2
            SUB2=-ETA(I)/2
            TRY1=.TRUE.
            TRY2=.TRUE.
*   Try to subtract half in earlier points.
            DO 220 J=1,I-1
            IF(ETA(I-J).GT.SUB1)THEN
                 ETA(I-J)=ETA(I-J)-SUB1
                 ETA(I)=ETA(I)+SUB1
                 SUB1=0
                 GOTO 230
            ELSEIF(ETA(I-J).GT.0)THEN
                 ETA(I)=ETA(I)+ETA(I-J)
                 SUB1=SUB1-ETA(I-J)
                 ETA(I-J)=0
            ENDIF
220         CONTINUE
            TRY1=.FALSE.
230         CONTINUE
*   Try to subtract the other half in later points.
            DO 240 J=1,NU-I-1
            IF(ETA(I+J).GT.SUB2)THEN
                 ETA(I+J)=ETA(I+J)-SUB2
                 ETA(I)=ETA(I)+SUB2
                 SUB2=0
                 GOTO 250
            ELSEIF(ETA(I+J).GT.0)THEN
                 ETA(I)=ETA(I)+ETA(I+J)
                 SUB2=SUB2-ETA(I+J)
                 ETA(I+J)=0
            ENDIF
240         CONTINUE
            TRY2=.FALSE.
250         CONTINUE
*   Done if both sides have margin left.
            DONE=.FALSE.
            IF(TRY1.AND.TRY2)THEN
                 DONE=.TRUE.
*   Try lower side again.
            ELSEIF(TRY1)THEN
                 SUB1=-ETA(I)
                 DO 260 J=1,I-1
                 IF(ETA(I-J).GT.SUB1)THEN
                      ETA(I-J)=ETA(I-J)-SUB1
                      ETA(I)=ETA(I)+SUB1
                      SUB1=0
                      DONE=.TRUE.
                      GOTO 270
                 ELSEIF(ETA(I-J).GT.0)THEN
                      ETA(I)=ETA(I)+ETA(I-J)
                      SUB1=SUB1-ETA(I-J)
                      ETA(I-J)=0
                 ENDIF
260              CONTINUE
270              CONTINUE
*   Try upper side again.
            ELSEIF(TRY2)THEN
                 SUB2=-ETA(I)
                 DO 280 J=1,NU-I-1
                 IF(ETA(I+J).GT.SUB2)THEN
                      ETA(I+J)=ETA(I+J)-SUB2
                      ETA(I)=ETA(I)+SUB2
                      SUB2=0
                      DONE=.TRUE.
                      GOTO 290
                 ELSEIF(ETA(I+J).GT.0)THEN
                      ETA(I)=ETA(I)+ETA(I+J)
                      SUB2=SUB2-ETA(I+J)
                      ETA(I+J)=0
                 ENDIF
280              CONTINUE
290              CONTINUE
            ENDIF
*   See whether we succeeded.
            IF(.NOT.DONE)THEN
                 PRINT *,' !!!!!! DLCEQU WARNING : Unable to even out'//
     -                ' backwards eta steps; inaccurate avalanche.'
                 IF(IRES.GT.0)D(IRES)=DRES
                 RETURN
            ENDIF
       ENDIF
210    CONTINUE
*** Seems to have worked.
       IFAIL=0
       IF(IRES.GT.0)D(IRES)=DRES
       END
