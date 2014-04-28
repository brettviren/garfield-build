CDECK  ID>, SIGINP.
       SUBROUTINE SIGINP
*-----------------------------------------------------------------------
*   SIGINP - Routine looking at the instructions in the signal section.
*            The actual calculations are performed by other routines.
*   VARIABLES : CHANGE      : .TRUE. when new ion tails have to be
*                             calculated (due to a change in parameters)
*               OPEN        : used for checking the the unit status
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       LOGICAL FPERX,FPERY,LCROSS,TRASET,TRAFLG,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       INTEGER NPAIR,ICLUST,NFOUR,MFEXP,MXMIN,MXMAX,
     -      MYMIN,MYMAX,NTRBNK,ITRMAJ,NTIME,NORIA,
     -      NASIMP,JIORD,NISIMP,NMQUAD,NCANG,IENANG
       REAL TIMSIG,SIGNAL,TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,
     -      AVALAN,TSTART,TDEV,PRSTHR,
     -      TRABNK,TRAVEC
       CHARACTER*(MXCHAR) FCNANG
       CHARACTER*12 AVATYP
       CHARACTER*3 FCELTP
       COMMON /SIGDAT/ TIMSIG(MXLIST),SIGNAL(MXLIST,MXSW,2),
     -      AVALAN(2),TRAVEC(MXLIST),
     -      TRABNK(MXLIST,9),TSTART,TDEV,PRSTHR,
     -      TCLUST,SCLUST,ACLUST,BCLUST,FCLUST,ICLUST,NPAIR,
     -      NFOUR,ITRMAJ,JIORD,NISIMP,NMQUAD,IENANG,NTIME,NORIA,
     -      MFEXP,MXMIN,MXMAX,MYMIN,MYMAX,NTRBNK,NASIMP,NCANG,
     -      TRASET,TRAFLG(9),FPERX,FPERY,LCROSS,LITAIL,LDTAIL,LRTAIL,
     -      LEPULS,LIPULS,SIGSET,RESSET
       COMMON /SIGCHR/ FCELTP,AVATYP,FCNANG
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
       CHARACTER*20 STR1,STR2,STR3,STR4
       LOGICAL CHANGE,OPEN,LDIFF,LTOWN,LATTA,FLAG(MXWORD+3)
       INTEGER NTIMER,IFAIL,IFAIL1,IFAIL2,IFAIL3,
     -      I,J,K,INEXT,NGRDXR,NGRDYR,NGRIDR,NWORD,NC,NFOURR,MFR,
     -      NLTR,NLTRR,IOS,INPTYP,INPCMP,NC1,NC2,NC3,NC4
       REAL TDEVR,TSTARR,FACTR,RELWID,THETAR,TMIN,TMAX,SMIN,SMAX,
     -      AMIN,AMAX,BMIN,BMAX
       DOUBLE PRECISION DUMMY(1)
       EXTERNAL INPCMP,INPTYP
*** Define some formats.
1110   FORMAT('  The Fourier series will have ',I10,' terms.')
*** Print a heading for the signal simulation pages.
       WRITE(*,'(''1'')')
       PRINT *,' ================================================'
       PRINT *,' ==========  Start of signal section   =========='
       PRINT *,' ================================================'
       PRINT *,' '
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE SIGINP ///'
*** Check that sufficient gas data have been read.
C       IF(.NOT.(GASOK(1).AND.GASOK(2).AND.
C     -      (GASOK(5).OR.HEEDOK.OR.SRIMOK)))THEN
C            PRINT *,' ###### SIGINP ERROR   : Insufficient gas data'//
C     -           ' (needed are the electron drift velocity,'
C            PRINT *,'                         the ion mobility and'//
C     -           ' cluster data); this section is skipped.'
       IF(.NOT.(GASOK(1).AND.GASOK(2)))THEN
            PRINT *,' ###### SIGINP ERROR   : Insufficient gas data.'//
     -           ' Needed are the electron drift velocity'
            PRINT *,'                         and the ion mobility;'//
     -           ' this section is skipped.'
            CALL SKIP
            RETURN
       ENDIF
*** Set default area.
       CALL GRASET(DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX)
*** Set SIGSET, CHANGE, RESSET and AVATYP to false.
       SIGSET=.FALSE.
       CHANGE=.TRUE.
       AVATYP='NOT SET'
       RESSET=.FALSE.
*** Start a loop over the input file, searching for keywords.
       CALL INPPRM('Signal','NEW-PRINT')
10     CONTINUE
       CALL INPWRD(NWORD)
       CALL INPSTR(1,1,STRING,NC)
*** Skip this line if it is blank.
       IF(NWORD.EQ.0)GOTO 10
*** Return to the main program if & is the first character.
       IF(STRING(1:1).EQ.'&')THEN
*   Close signal matrix scratch unit.
            INQUIRE(UNIT=13,OPENED=OPEN)
            IF(OPEN)CLOSE(UNIT=13,IOSTAT=IOS,ERR=2030)
*   Close ion tail storage.
            IF(SIGSET)CALL SIGIST('CLOSE',0,DUMMY,DUMMY,0,0,0,0,IFAIL1)
*   Release capacitance matrix memory.
            IF(TYPE.NE.'MAP')THEN
                 CALL BOOK('RELEASE','MATRIX','SIGNAL',IFAIL1)
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGINP WARNING :'//
     -                ' Unable to release signal matrix storage.'
            ENDIF
*   Reset initialisation flag.
            SIGSET=.FALSE.
*   Return to caller.
            RETURN
*** Add noise to the signals.
       ELSEIF(INPCMP(1,'ADD-N#OISE').NE.0)THEN
            CALL SIGNOI(IFAIL)
*** Look for the AREA instruction.
       ELSEIF(INPCMP(1,'AREA').NE.0)THEN
            CALL CELVIE(DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX)
            CALL INPERR
*** Read the avalanche parameters if AVALANCHE is a keyword.
       ELSEIF(INPCMP(1,'AV#ALANCHE').NE.0)THEN
*   Print the current setting, if entered without arguments.
            IF(NWORD.EQ.1)THEN
                 IF(AVATYP.EQ.'EXPONENTIAL')THEN
                      CALL OUTFMT(AVALAN(1),2,STR1,NC1,'LEFT')
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' is exponentially distributed with an'',
     -                     '' average of '',A,''.'')') STR1(1:NC1)
                 ELSEIF(AVATYP.EQ.'FIXED')THEN
                      CALL OUTFMT(AVALAN(1),2,STR1,NC1,'LEFT')
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' is '',A,'' irrespective of the'',
     -                     '' drift line.'')') STR1(1:NC1)
                 ELSEIF(AVATYP.EQ.'GAUSSIAN')THEN
                      CALL OUTFMT(AVALAN(1),2,STR1,NC1,'LEFT')
                      CALL OUTFMT(AVALAN(2),2,STR2,NC2,'LEFT')
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' distribution is Gaussian with''/
     -                     ''  mean '',A,'' and relative width '',A,
     -                     ''.'')') STR1(1:NC1),STR2(1:NC2)
                 ELSEIF(AVATYP.EQ.'NONE')THEN
                      WRITE(LUNOUT,'(''  There is no multiplication'',
     -                     '' along electron drift lines.'')')
                 ELSEIF(AVATYP.EQ.'NOT SET')THEN
                      WRITE(LUNOUT,'(''  No avalanche specification'',
     -                     '' has been entered in this section.'')')
                 ELSEIF(AVATYP.EQ.'POLYA-FIXED')THEN
                      CALL OUTFMT(AVALAN(1),2,STR1,NC1,'LEFT')
                      CALL OUTFMT(AVALAN(2),2,STR2,NC2,'LEFT')
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' is Polya distributed''/''  with fixed'',
     -                     '' mean '',A,'' and with a parameter '',
     -                     A,''.'')') STR1(1:NC1),STR2(1:NC2)
                 ELSEIF(AVATYP.EQ.'POLYA-TOWN')THEN
                      CALL OUTFMT(AVALAN(1),2,STR1,NC1,'LEFT')
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' is Polya distributed with a mean''/
     -                     ''  determined by the Townsend'',
     -                     '' coefficients and with a parameter '',
     -                     A,''.'')') STR1(1:NC1)
                 ELSEIF(AVATYP.EQ.'TOWNSEND')THEN
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' is determined by the Townsend'',
     -                     '' coefficients with exponential'',
     -                     '' fluctuations.'')')
                 ELSEIF(AVATYP.EQ.'TOWN-FIXED')THEN
                      WRITE(LUNOUT,'(''  The multiplication factor'',
     -                     '' is determined by the Townsend'',
     -                     '' coefficients without fluctuations.'')')
                 ELSE
                      PRINT *,' ###### SIGINP ERROR : Avalanche type ',
     -                     AVATYP,' not known.'
                 ENDIF
*   The avalanche type might be EXPONENTIAL.
            ELSEIF(NWORD.EQ.3.AND.INPCMP(2,'E#XPONENTIAL').NE.0)THEN
                 AVATYP='EXPONENTIAL'
                 CALL INPCHK(3,2,IFAIL1)
                 CALL INPRDR(3,FACTR,0.0)
                 IF(FACTR.GT.0.AND.IFAIL1.EQ.0)THEN
                      AVALAN(1)=FACTR
                 ELSE
                      CALL INPMSG(3,'Multiplication is not > 0.')
                      AVALAN(1)=1
                 ENDIF
*   The avalanche type might be FIXED.
            ELSEIF(NWORD.EQ.3.AND.INPCMP(2,'F#IXED').NE.0)THEN
                 AVATYP='FIXED'
                 CALL INPCHK(3,2,IFAIL1)
                 CALL INPRDR(3,FACTR,0.0)
                 IF(FACTR.GE.1.AND.IFAIL1.EQ.0)THEN
                      AVALAN(1)=FACTR
                 ELSE
                      CALL INPMSG(3,'Multiplication is < 1.')
                      AVALAN(1)=1
                 ENDIF
*   The avalanche type might be GAUSSIAN.
            ELSEIF(NWORD.EQ.4.AND.INPCMP(2,'G#AUSSIAN').NE.0)THEN
                 AVATYP='GAUSSIAN'
                 CALL INPCHK(3,2,IFAIL1)
                 CALL INPCHK(4,2,IFAIL2)
                 CALL INPRDR(3,FACTR,0.0)
                 CALL INPRDR(4,RELWID,0.0)
                 AVALAN(1)=FACTR
                 IF(RELWID.GT.0.0.AND.IFAIL2.EQ.0)THEN
                      AVALAN(2)=RELWID
                 ELSE
                      CALL INPMSG(4,'Relative width must be > 0.')
                      AVALAN(2)=0
                 ENDIF
*   The avalanche type might be NONE.
            ELSEIF(NWORD.EQ.4.AND.INPCMP(2,'NO#NE').NE.0)THEN
                 AVATYP='NONE'
*   The avalanche type might be POLYA-FIXED.
            ELSEIF(NWORD.GE.2.AND.NWORD.LE.4.AND.
     -           INPCMP(2,'P#OLYA-F#IXED')+
     -           INPCMP(2,'F#IXED-P#OLYA').NE.0)THEN
                 AVATYP='POLYA-FIXED'
                 IF(NWORD.GE.3)THEN
                      CALL INPCHK(3,2,IFAIL1)
                      CALL INPRDR(3,FACTR,0.0)
                 ELSE
                      IFAIL1=0
                      FACTR=1
                 ENDIF
                 IF(FACTR.GT.0.AND.IFAIL1.EQ.0)THEN
                      AVALAN(1)=FACTR
                 ELSE
                      CALL INPMSG(3,'Multiplication is not > 0.')
                      AVALAN(1)=1
                 ENDIF
                 IF(NWORD.GE.4)THEN
                      CALL INPCHK(4,2,IFAIL2)
                      CALL INPRDR(4,THETAR,0.5)
                 ELSE
                      IFAIL2=0
                      THETAR=0.5
                 ENDIF
                 IF(THETAR.GT.-1.AND.IFAIL2.EQ.0)THEN
                      AVALAN(2)=THETAR
                 ELSE
                      CALL INPMSG(4,'Polya parameter must be > -1.')
                      AVALAN(2)=0.5
                 ENDIF
*   The avalanche type might be POLYA-TOWNSEND.
            ELSEIF(NWORD.GE.2.AND.NWORD.LE.3.AND.
     -           INPCMP(2,'P#OLYA-T#OWNSEND')+
     -           INPCMP(2,'T#OWNSEND-P#OLYA').NE.0)THEN
                 AVATYP='POLYA-TOWN'
                 IF(NWORD.GE.3)THEN
                      CALL INPCHK(3,2,IFAIL1)
                      CALL INPRDR(3,THETAR,0.0)
                 ELSE
                      IFAIL1=0
                      THETAR=0.5
                 ENDIF
                 IF(THETAR.GT.-1.AND.IFAIL1.EQ.0)THEN
                      AVALAN(1)=THETAR
                 ELSE
                      CALL INPMSG(3,'Polya parameter must be > -1.')
                      AVALAN(1)=0.5
                 ENDIF
*   The avalanche type might be TOWNSEND.
            ELSEIF(NWORD.EQ.2.AND.INPCMP(2,'T#OWNSEND').NE.0)THEN
                 IF(GASOK(4))THEN
                      AVATYP='TOWNSEND'
                 ELSE
                      CALL INPMSG(2,'No Townsend data are present. ')
                 ENDIF
*   The avalanche type might be TOWNSEND-FIXED.
            ELSEIF(NWORD.EQ.2.AND.INPCMP(2,'T#OWNSEND-FIX#ED')+
     -                INPCMP(2,'FIX#ED-T#OWNSEND').NE.0)THEN
                 IF(GASOK(4))THEN
                      AVATYP='TOWN-FIXED'
                 ELSE
                      CALL INPMSG(2,'No Townsend data are present. ')
                 ENDIF
*   Apparently some incorrect format has been used.
            ELSE
                 PRINT *,' !!!!!! SIGINP WARNING : Incorrect format'//
     -                ' of an AVALANCHE statement; see the writeup.'
            ENDIF
            CALL INPERR
*** Look for the CHECK command.
       ELSEIF(INPCMP(1,'CH#ECK').NE.0)THEN
            CALL SIGCHK
*** Convolute signals with a transfer function.
       ELSEIF(INPCMP(1,'CON#VOLUTE-S#IGNALS').NE.0)THEN
            CALL SIGCNV(IFAIL)
*** Look for the FOURIER instruction.
       ELSEIF(INPCMP(1,'F#OURIER').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 PRINT 1110,NFOUR
            ELSEIF(NWORD.EQ.2.AND.INPCMP(2,'NONE').NE.0)THEN
                 IF(NFOUR.NE.0)THEN
                      CHANGE=.TRUE.
                      SIGSET=.FALSE.
                 ENDIF
                 NFOUR=0
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NFOURR,1)
*   check the new value, replace if acceptable.
                 MFR=NINT(LOG(REAL(NFOURR))/LOG(2.0))
                 IF(IFAIL1.EQ.0.AND.NFOURR.NE.2**MFR)THEN
                      CALL INPMSG(2,'Not an integral power of 2.   ')
                 ELSEIF(IFAIL1.EQ.0.AND.NFOURR.LE.0)THEN
                      CALL INPMSG(2,'Not larger than 0.            ')
                 ELSEIF(IFAIL1.EQ.0.AND.NFOURR.GT.MXFOUR)THEN
                      CALL INPMSG(2,'Larger than MXFOUR.           ')
                 ELSEIF(IFAIL1.EQ.0)THEN
                      IF(NFOUR.NE.NFOURR)THEN
                           CHANGE=.TRUE.
                           SIGSET=.FALSE.
                      ENDIF
                      NFOUR=NFOURR
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! SIGINP WARNING : FOURIER takes one'//
     -                ' argument ; instruction is ignored.'
            ENDIF
*   Print error messages.
            CALL INPERR
*** Read track information from a dataset if GET is the command.
       ELSEIF(INPCMP(1,'GET-TR#ACK').NE.0)THEN
            CALL DLCTRG(IFAIL)
*** Look for the keyword GRID.
       ELSEIF(INPCMP(1,'G#RID').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Current grid density: '',
     -                I3,'' by '',I3,'' points.'')') NGRIDX,NGRIDY
            ELSEIF(NWORD.EQ.2)THEN
                 CALL INPCHK(2,1,IFAIL1)
                 CALL INPRDI(2,NGRIDR,25)
                 IF(NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)
     -                CALL INPMSG(2,'GRID out of range 2 -> MXGRID.')
                 CALL INPERR
                 IF(IFAIL1.NE.0.OR.NGRIDR.LE.1.OR.NGRIDR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! SIGINP WARNING : GRID statement',
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
                 CALL INPERR
                 IF(IFAIL1.NE.0.OR.NGRDXR.LE.1.OR.NGRDXR.GT.MXGRID.OR.
     -                NGRDYR.LE.1.OR.NGRDYR.GT.MXGRID)THEN
                      PRINT *,' !!!!!! SIGINP WARNING : GRID statement',
     -                     ' ignored because of syntax or value errors.'
                 ELSE
                      NGRIDX=NGRDXR
                      NGRIDY=NGRDYR
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! SIGINP WARNING : GRID requires 1'//
     -                ' or 2 arguments ; the instruction is ignored.'
            ENDIF
*   Print error messages.
            CALL INPERR
*** Integration parameters.
       ELSEIF(INPCMP(1,'INT#EGRATION-#PARAMETERS').NE.0)THEN
            CALL DLCPAR
*** If OPTION is a keyword, try and identify them:
       ELSEIF(INPCMP(1,'OPT#IONS').NE.0)THEN
            IF(NWORD.EQ.1)WRITE(LUNOUT,'(
     -           ''  LOCAL OPTIONS CURRENTLY IN EFFECT:''//
     -           ''  Plotting of track, clusters and electrons:    '',
     -           L1/
     -           ''  Printing of track, clusters and electrons:    '',
     -           L1/
     -           ''  Contour all media (T) or drift medium (F):    '',
     -           L1/
     -           ''  Plotting of the drift lines (DRIFT-PLOT):     '',
     -           L1/
     -           ''  Printing of drift line details (DRIFT-PRINT): '',
     -           L1/
     -           ''  Plot wires by markers (WIRE-MARKERS):         '',
     -           L1/
     -           ''  Check for multiple field map indices:         '',
     -           L1/)') LCLPLT,LCLPRT,LCNTAM,LDRPLT,LDRPRT,LWRMRK,LMAPCH
            DO 11 I=2,NWORD
*   look for clusterplot option,
            IF(INPCMP(I,'NOC#LUSTER-PL#OT').NE.0)THEN
                 LCLPLT=.FALSE.
            ELSEIF(INPCMP(I,'C#LUSTER-PL#OT').NE.0)THEN
                 LCLPLT=.TRUE.
*   look for cluster-print option,
            ELSEIF(INPCMP(I,'NOC#LUSTER-PR#INT').NE.0)THEN
                 LCLPRT=.FALSE.
            ELSEIF(INPCMP(I,'C#LUSTER-PR#INT').NE.0)THEN
                 LCLPRT=.TRUE.
*   search for plotting-of-drift lines option,
            ELSEIF(INPCMP(I,'NOD#RIFT-PL#OT').NE.0)THEN
                 LDRPLT=.FALSE.
            ELSEIF(INPCMP(I,'D#RIFT-PL#OT').NE.0)THEN
                 LDRPLT=.TRUE.
*   search for printing-of-drift lines option,
            ELSEIF(INPCMP(I,'NOD#RIFT-PR#INT').NE.0)THEN
                 LDRPRT=.FALSE.
            ELSEIF(INPCMP(I,'DR#IFT-PR#INT').NE.0)THEN
                 LDRPRT=.TRUE.
*   Contour drawing options.
            ELSEIF(INPCMP(I,'CONT#OUR-ALL-#MEDIA').NE.0)THEN
                 LCNTAM=.TRUE.
            ELSEIF(INPCMP(I,'CONT#OUR-DR#IFT-#MEDIUM')+
     -           INPCMP(I,'CONT#OUR-DR#IFT-#MEDIA').NE.0)THEN
                 LCNTAM=.FALSE.
*   Wires drawn as markers.
            ELSEIF(INPCMP(I,'NOW#IRE-M#ARKERS').NE.0)THEN
                 LWRMRK=.FALSE.
            ELSEIF(INPCMP(I,'W#IRE-M#ARKERS').NE.0)THEN
                 LWRMRK=.TRUE.
*   Detect multiple map indices.
            ELSEIF(INPCMP(I,'CH#ECK-MAP-#INDICES')+
     -           INPCMP(I,'CH#ECK-MAP-#INDEXING').NE.0)THEN
                 LMAPCH=.TRUE.
            ELSEIF(INPCMP(I,'NOCH#ECK-MAP-#INDICES')+
     -           INPCMP(I,'NOCH#ECK-MAP-#INDEXING').NE.0)THEN
                 LMAPCH=.FALSE.
*   option not known.
            ELSE
                 CALL INPMSG(I,'The option is not known.      ')
            ENDIF
11          CONTINUE
*   Print error messages.
            CALL INPERR
*** Plot the signal field.
       ELSEIF(INPCMP(1,'PL#OT-F#IELD').NE.0)THEN
            CALL SIGWGT
*** Plot signals if PLOT-SIGNALS is a keyword.
       ELSEIF(INPCMP(1,'PL#OT-S#IGNALS').NE.0)THEN
            CALL SIGPLT
*** PREPARE-TRACK: Prepare a drifting information table.
       ELSEIF(INPCMP(1,'PR#EPARE-#TRACK').NE.0.AND..NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! SIGINP WARNING : Track preparation'//
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
*** Reset various things.
       ELSEIF(INPCMP(1,'RESET').NE.0)THEN
*   No keywords, reset everything.
            IF(NWORD.EQ.1)THEN
                 DO 110 K=1,MXSW
                 DO 120 J=1,MXLIST
                 SIGNAL(J,K,1)=0.0
                 SIGNAL(J,K,2)=0.0
120              CONTINUE
110              CONTINUE
                 RESSET=.FALSE.
                 AVATYP='NOT SET'
                 SIGSET=.FALSE.
                 CALL SIGIST('CLOSE',0,DUMMY,DUMMY,0,0,0,0,IFAIL1)
                 IF(TYPE.NE.'MAP')THEN
                      CALL BOOK('RELEASE','MATRIX','SIGNAL',IFAIL1)
                      IF(IFAIL1.NE.0)
     -                     PRINT *,' !!!!!! SIGINP WARNING : Unable '//
     -                     'to release signal matrix storage.'
                 ENDIF
            ELSE
*   Otherwise, loop over the keywords.
                 DO 70 I=2,NWORD
                 IF(INPCMP(I,'SIG#NALS').NE.0)THEN
                      DO 80 K=1,MXSW
                      DO 90 J=1,MXLIST
                      SIGNAL(J,K,1)=0.0
                      SIGNAL(J,K,2)=0.0
90                    CONTINUE
80                    CONTINUE
                 ELSEIF(INPCMP(I,'RES#OLUTION')+INPCMP(I,'WIN#DOW')+
     -                INPCMP(I,'TIME-WIN#DOW').NE.0)THEN
                      RESSET=.FALSE.
                 ELSEIF(INPCMP(I,'AVA#LANCHE-#MODEL').NE.0)THEN
                      AVATYP='NOT SET'
                 ELSEIF(INPCMP(I,'MAT#RICES').NE.0)THEN
                      SIGSET=.FALSE.
                      CALL SIGIST('CLOSE',0,DUMMY,DUMMY,0,0,0,0,IFAIL1)
                      IF(TYPE.NE.'MAP')THEN
                           CALL BOOK('RELEASE','MATRIX','SIGNAL',IFAIL1)
                           IF(IFAIL1.NE.0)
     -                          PRINT *,' !!!!!! SIGINP WARNING :'//
     -                          ' Unable to release signal matrix'//
     -                          ' storage.'
                      ENDIF
                 ELSE
                      CALL INPMSG(I,'Not a known option.')
                 ENDIF
70               CONTINUE
*   Print error messages.
                 CALL INPERR
            ENDIF
*** Look for the time window.
       ELSEIF(INPCMP(1,'RES#OLUTION')+INPCMP(1,'WIN#DOW')+
     -      INPCMP(1,'T#IME-WIN#DOW').NE.0)THEN
            IF(NWORD.EQ.1)THEN
                 IF(RESSET)THEN
                      CALL OUTFMT(TSTART,2,STR1,NC1,'LEFT')
                      CALL OUTFMT(TSTART+(NTIME-1)*TDEV,2,STR2,NC2,
     -                     'LEFT')
                      CALL OUTFMT(REAL(NTIME),2,STR3,NC3,'LEFT')
                      CALL OUTFMT(TDEV,2,STR4,NC4,'LEFT')
                      WRITE(LUNOUT,'(''  Time window: ['',A,
     -                     '', '',A,''] microsec, in '',A,
     -                     '' steps of '',A,'' microsec.'')')
     -                     STR1(1:NC1),STR2(1:NC2),STR3(1:NC3),
     -                     STR4(1:NC4)
                 ELSE
                      WRITE(LUNOUT,'(''  The time window has not'',
     -                     '' yet been set.'')')
                 ENDIF
            ELSEIF(NWORD.LE.4)THEN
                 CALL INPCHK(2,2,IFAIL1)
                 CALL INPRDR(2,TSTARR,TSTART)
                 CALL INPCHK(3,2,IFAIL2)
                 CALL INPRDR(3,TDEVR,TDEV)
                 IF(NWORD.GE.4)THEN
                      CALL INPCHK(4,1,IFAIL3)
                      CALL INPRDI(4,NTIMER,NTIME)
                 ELSE
                      NTIMER=NTIME
                      IFAIL3=0
                 ENDIF
                 IF(IFAIL1.EQ.0.AND.TSTARR.LT.0.0)THEN
                      CALL INPMSG(2,'The starting time is not > 0  ')
                      IFAIL1=1
                 ENDIF
                 IF(IFAIL2.EQ.0.AND.TDEVR.LE.0.0)THEN
                      CALL INPMSG(3,'The time resolution is not > 0')
                      IFAIL2=1
                 ENDIF
                 IF(IFAIL3.EQ.0.AND.
     -                (NTIMER.LE.1.OR.NTIMER.GT.MXLIST))THEN
                      CALL INPMSG(4,'Number of samples not in range')
                      IFAIL3=1
                 ENDIF
*   if the TSTART, TDEV read from input are > 0 transfer to TSTART, TDEV
                 IF(IFAIL1.EQ.0)THEN
                      IF(TSTART.NE.TSTARR)CHANGE=.TRUE.
                      TSTART=TSTARR
                 ELSE
                      PRINT *,' !!!!!! SIGINP WARNING : Start time in'//
     -                     ' RESOLUTION is ignored because of errors'
                 ENDIF
                 IF(IFAIL2.EQ.0)THEN
                      IF(TDEV.NE.TDEVR)CHANGE=.TRUE.
                      TDEV=TDEVR
                 ELSE
                      PRINT *,' !!!!!! SIGINP WARNING : Resolution in'//
     -                     ' RESOLUTION is ignored because of errors.'
                 ENDIF
                 IF(IFAIL3.EQ.0)THEN
                      IF(NTIMER.NE.NTIME)CHANGE=.TRUE.
                      NTIME=NTIMER
                 ELSE
                      PRINT *,' !!!!!! SIGINP WARNING : Number of'//
     -                     ' samples in RESOLUTION ignored because'//
     -                     ' of errors.'
                 ENDIF
*   Preset a vector of signal times.
                 DO 40 I=1,MXLIST
                 TIMSIG(I)=TSTART+(I-1)*TDEV
                 DO 100 J=1,MXSW
                 SIGNAL(I,J,1)=0.0
                 SIGNAL(I,J,2)=0.0
100              CONTINUE
40               CONTINUE
                 RESSET=.TRUE.
*   Incorrect number of arguments.
            ELSE
                 PRINT *,' !!!!!! SIGINP WARNING : RESOLUTION takes'//
     -                ' 1, 2 or 3 arguments ; instruction is ignored.'
            ENDIF
*   Print error messages.
            CALL INPERR
*** Search for the SELECT instruction.
       ELSEIF(INPCMP(1,'SEL#ECT').NE.0)THEN
            CALL CELSEL(' ')
            CHANGE=.TRUE.
            DO 130 K=1,MXSW
            DO 140 J=1,MXLIST
            SIGNAL(J,K,1)=0.0
            SIGNAL(J,K,2)=0.0
140         CONTINUE
130         CONTINUE
*** Start simulation if SIGNAL is a keyword.
       ELSEIF(INPCMP(1,'SIG#NAL').NE.0)THEN
            CALL SIGGEN(CHANGE)
*** Signal parameters
       ELSEIF(INPCMP(1,'SIG#NAL-PAR#ARAMETERS').NE.0)THEN
            CALL SIGPAR
*** Look for the instruction TRACK.
       ELSEIF(INPCMP(1,'TR#ACK').NE.0)THEN
            CALL TRAREA
*** Write signals if WRITE-SIGNALS is a keyword.
       ELSEIF(INPCMP(1,'WR#ITE-S#IGNALS').NE.0)THEN
            CALL SIGWRT
*** Write the track data if WRITE-TRACK is a keyword.
       ELSEIF(INPCMP(1,'WR#ITE-T#RACK').NE.0)THEN
            CALL DLCTRW
*** The instruction is not known.
       ELSE
            CALL INPSTR(1,1,STRING,NC)
            PRINT *,' !!!!!! SIGINP WARNING : '//STRING(1:NC)//' is'//
     -           ' not a valid instruction ; it is ignored.'
       ENDIF
*** End loop over input.
       GOTO 10
*** Handle error conditions.
2030   CONTINUE
       PRINT *,' !!!!!! SIGINP WARNING : Problems closing scratch'//
     -      ' data set on unit 13 or 14 (used for intermediate'//
     -      ' results);'
       PRINT *,'                         new simulations are'//
     -      ' probably not possible.'
       CALL INPIOS(IOS)
       END
