CDECK  ID>, SIGPLT.
       SUBROUTINE SIGPLT
*-----------------------------------------------------------------------
*   SIGPLT - Routine plotting the signal induced on the sense wires
*   VARIABLES :
*   (Last changed on 21/ 1/01.)
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
       CHARACTER*50 AUX
       REAL TPLMIN,TPLMAX,TMIN,TMAX,SPLMIN,SPLMAX,SIGMIN,SIGMAX,
     -      TMINR,TMAXR,SMINR,SMAXR
       INTEGER INPCMP,INPTYP,I,J,INEXT,ISW,JSW,JW,NC,NPLOT,NWORD,
     -      IFAIL1,IFAIL2,NFOUND,ITMIN,ITMAX
       LOGICAL FLAT,PLOT(MXSW),TAUTO,SAUTO,LPLCR,LPLDIR,FLAG(MXWORD)
       EXTERNAL INPCMP,INPTYP
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGPLT ///'
*** Initialise the time window.
       TMIN=TIMSIG(1)
       TMAX=TIMSIG(NTIME)
       TAUTO=.FALSE.
*   Preset the signal range.
       SAUTO=.TRUE.
*   The wire plot flags (sense wires with non-flat signal).
       DO 10 ISW=1,MXSW
       PLOT(ISW)=.FALSE.
       IF(ISW.LE.NSW)THEN
            DO 20 J=1,NTIME
            IF(SIGNAL(J,ISW,1).NE.0.OR.
     -           (LCROSS.AND.SIGNAL(J,ISW,2).NE.0))PLOT(ISW)=.TRUE.
20          CONTINUE
       ENDIF
10     CONTINUE
*   Plotting options.
       LPLCR=LCROSS
       LPLDIR=.TRUE.
*** Read the command line arguments.
       CALL INPNUM(NWORD)
*   Mark keyword.
       DO 25 I=1,MXWORD
       IF(I.GT.NWORD)THEN
            FLAG(I)=.TRUE.
       ELSEIF(INPCMP(I,'TIME-#WINDOW')+INPCMP(I,'WIN#DOW')+
     -      INPCMP(I,'RAN#GE')+INPCMP(I,'SC#ALE')+
     -      INPCMP(I+1,'AUTO#MATIC')+INPCMP(I,'WIRE#S')+
     -      INPCMP(I,'CR#OSS-#INDUCED-#SIGNALS')+
     -      INPCMP(I,'NOCR#OSS-#INDUCED-#SIGNALS')+
     -      INPCMP(I,'DIR#ECT-#SIGNALS')+INPCMP(I,'NODIR#ECT-#SIGNALS')+
     -      INPCMP(I,'ALL')+INPCMP(I,'ACT#IVE').NE.0)THEN
            FLAG(I)=.TRUE.
       ELSE
            FLAG(I)=.FALSE.
       ENDIF
25     CONTINUE
*   Loop over the words.
       INEXT=2
       DO 30 I=2,NWORD
       IF(I.LT.INEXT)GOTO 30
**  Time window.
       IF(INPCMP(I,'TIME-#WINDOW')+INPCMP(I,'WIN#DOW').NE.0)THEN
*   Automatic window.
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 TAUTO=.TRUE.
                 INEXT=I+2
*   No arguments.
            ELSEIF(NWORD.LT.I+2.OR.FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'Arguments missing')
*   Arguments, but not integer, real or *.
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.AND.
     -           INPTYP(I+1).NE.4).OR.(INPTYP(I+2).NE.1.AND.
     -           INPTYP(I+2).NE.2.AND.INPTYP(I+2).NE.4))THEN
                 CALL INPMSG(I,'Arguments of wrong type')
*   Two arguments: establish the range.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,TMINR,TMIN)
                 CALL INPRDR(I+2,TMAXR,TMAX)
                 IF(TMINR.EQ.TMAXR.OR.
     -                IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.
     -                MIN(TMINR,TMAXR).GT.TIMSIG(NTIME).OR.
     -                MAX(TMINR,TMAXR).LT.TIMSIG(1))THEN
                      CALL INPMSG(I+1,'Invalid range')
                      CALL INPMSG(I+2,'Invalid range')
                 ELSE
                      TMIN=MIN(TMINR,TMAXR)
                      TMAX=MAX(TMINR,TMAXR)
                      TAUTO=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
**  Signal range.
       ELSEIF(INPCMP(I,'RAN#GE')+INPCMP(I,'SC#ALE').NE.0)THEN
*   Automatic window.
            IF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 SAUTO=.TRUE.
                 INEXT=I+2
*   No arguments.
            ELSEIF(NWORD.LT.I+2.OR.FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'Arguments missing')
*   Arguments, but not integer, real or *.
            ELSEIF((INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2))THEN
                 CALL INPMSG(I,'Arguments of wrong type')
*   Two arguments: establish the range.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,SMINR,0.0)
                 CALL INPRDR(I+2,SMAXR,0.0)
                 IF(SMINR.EQ.SMAXR.OR.
     -                IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
                      CALL INPMSG(I+1,'Invalid range')
                      CALL INPMSG(I+2,'Invalid range')
                 ELSE
                      SPLMIN=MIN(SMINR,SMAXR)
                      SPLMAX=MAX(SMINR,SMAXR)
                      SAUTO=.FALSE.
                 ENDIF
                 INEXT=I+3
            ENDIF
**  Direct / Cross-induced / both.
       ELSEIF(INPCMP(I,'CR#OSS-#INDUCED-#SIGNALS').NE.0)THEN
            IF(.NOT.LCROSS)THEN
                 CALL INPMSG(I,'Option CROSS-INDUCED is off')
            ELSE
                 LPLCR=.TRUE.
            ENDIF
       ELSEIF(INPCMP(I,'NOCR#OSS-#INDUCED-#SIGNALS').NE.0)THEN
            LPLCR=.FALSE.
       ELSEIF(INPCMP(I,'DIR#ECT-#SIGNALS').NE.0)THEN
            LPLDIR=.TRUE.
       ELSEIF(INPCMP(I,'NODIR#ECT-#SIGNALS').NE.0)THEN
            LPLDIR=.FALSE.
**  Wire selections.
       ELSEIF(INPCMP(I,'WIRE#S').NE.0)THEN
*   All wires are selected.
            IF(INPCMP(I+1,'ALL').NE.0)THEN
                 DO 40 ISW=1,NSW
                 PLOT(ISW)=.TRUE.
40               CONTINUE
                 INEXT=I+2
*   All wires which have a signal.
            ELSEIF(INPCMP(I+1,'ACT#IVE').NE.0)THEN
                 DO 50 ISW=1,NSW
                 DO 60 J=1,NTIME
                 IF(SIGNAL(J,ISW,1).NE.0.OR.
     -                (LCROSS.AND.SIGNAL(J,ISW,2).NE.0))PLOT(ISW)=.TRUE.
60               CONTINUE
50               CONTINUE
                 INEXT=I+2
            ELSE
                 DO 70 J=I+1,NWORD
*   Leave when hitting a keyword.
                 IF(FLAG(J))THEN
                      INEXT=J
                      GOTO 90
*   Selection by wire number.
                 ELSEIF(INPTYP(J).EQ.1)THEN
                      CALL INPCHK(J,1,IFAIL1)
                      CALL INPRDI(J,JW,0)
                      IF(JW.GE.1.AND.JW.LE.NWIRE)THEN
                           JSW=INDSW(JW)
                           IF(JSW.NE.0)PLOT(JSW)=.TRUE.
                      ELSE
                           CALL INPMSG(J,'Not a valid wire number')
                      ENDIF
*   Selection by wire code.
                 ELSEIF(INPTYP(J).EQ.0)THEN
                      CALL INPSTR(J,J,AUX,NC)
                      NFOUND=0
                      DO 80 JW=1,NWIRE
                      IF(WIRTYP(JW).EQ.AUX(1:1))THEN
                           JSW=INDSW(JW)
                           IF(JSW.NE.0)THEN
                                PLOT(JSW)=.TRUE.
                                NFOUND=NFOUND+1
                           ENDIF
                      ENDIF
80                    CONTINUE
                      IF(NFOUND.EQ.0)CALL INPMSG(J,
     -                     'Not a known sense-wire code.')
                 ELSE
                      INEXT=J
                      GOTO 90
                 ENDIF
70               CONTINUE
                 INEXT=NWORD+1
90               CONTINUE
            ENDIF
*   Other keywords are not known.
       ELSEIF(FLAG(I))THEN
            CALL INPMSG(I,'Valid keyword out of context')
       ELSE
            CALL INPMSG(I,'Not a known keyword')
       ENDIF
30     CONTINUE
*** Print error messages.
       CALL INPERR
*** Make sure at least a bit of plotting is requested.
       IF(.NOT.(LPLCR.OR.LPLDIR))THEN
            PRINT *,' !!!!!! SIGPLT WARNING : No plot output has'//
     -           ' been requested ; no plot made.'
            RETURN
       ENDIF
*** Loop over all (groups of) sense wires, count the number of plots.
       NPLOT=0
       DO 100 ISW=1,NSW
       IF(.NOT.PLOT(ISW))GOTO 100
*   Find a proper time range.
       IF(TAUTO)THEN
            DO 120 J=1,NTIME
            IF((LPLDIR.AND.SIGNAL(J,ISW,1).NE.0).OR.
     -           (LCROSS.AND.LPLCR.AND.SIGNAL(J,ISW,2).NE.0))THEN
                 ITMIN=J
                 TMIN=TIMSIG(J)
                 GOTO 130
            ENDIF
120         CONTINUE
            PRINT *,' !!!!!! SIGPLT WARNING : Start time of signal'//
     -           ' not found ; program bug, please report.'
            GOTO 100
130         CONTINUE
            DO 140 J=NTIME,1,-1
            IF((LPLDIR.AND.SIGNAL(J,ISW,1).NE.0).OR.
     -           (LCROSS.AND.LPLCR.AND.SIGNAL(J,ISW,2).NE.0))THEN
                 ITMAX=J
                 TMAX=TIMSIG(J)
                 GOTO 150
            ENDIF
140         CONTINUE
            PRINT *,' !!!!!! SIGPLT WARNING : End time of signal'//
     -           ' not found ; program bug, please report.'
            GOTO 100
150         CONTINUE
            TPLMIN=TMIN-0.1*(TMAX-TMIN)
            TPLMAX=TMAX+0.1*(TMAX-TMIN)
       ELSE
            TPLMIN=TMIN
            TPLMAX=TMAX
            ITMIN=1
            ITMAX=NTIME
            DO 160 J=1,NTIME
            IF(TIMSIG(J).LT.TMIN)ITMIN=J
            IF(TIMSIG(NTIME-J+1).GT.TMAX)ITMAX=NTIME-J+1
160         CONTINUE
       ENDIF
*   Make sure the signal is not flat and find default signal range.
       FLAT=.TRUE.
       IF(LPLDIR)THEN
            SIGMIN=SIGNAL(ITMIN,ISW,1)
            SIGMAX=SIGNAL(ITMIN,ISW,1)
       ELSE
            SIGMIN=SIGNAL(ITMIN,ISW,2)
            SIGMAX=SIGNAL(ITMIN,ISW,2)
       ENDIF
       DO 110 J=ITMIN,ITMAX
       IF((LPLDIR.AND.SIGNAL(J,ISW,1).NE.0).OR.
     -      (LCROSS.AND.LPLCR.AND.SIGNAL(J,ISW,2).NE.0))FLAT=.FALSE.
       IF(LPLDIR)SIGMIN=MIN(SIGMIN,SIGNAL(J,ISW,1))
       IF(LPLCR.AND.LCROSS)SIGMIN=MIN(SIGMIN,SIGNAL(J,ISW,2))
       IF(LPLDIR)SIGMAX=MAX(SIGMAX,SIGNAL(J,ISW,1))
       IF(LPLCR.AND.LCROSS)SIGMAX=MAX(SIGMAX,SIGNAL(J,ISW,2))
110    CONTINUE
*   Print a warning if the signal is flat.
       IF(FLAT)THEN
            PRINT *,' !!!!!! SIGPLT WARNING : The signal on group ',
     -           ISW,' is zero within time window; not plotted.'
            GOTO 100
       ENDIF
*   Set the signal plot range.
       IF(SAUTO)THEN
            SPLMIN=SIGMIN-0.1*(SIGMAX-SIGMIN)
            SPLMAX=SIGMAX+0.1*(SIGMAX-SIGMIN)
       ENDIF
*   Open a frame for the plot.
       CALL OUTFMT(REAL(ISW),2,AUX,NC,'LEFT')
       CALL GRCART(TPLMIN,SPLMIN,TPLMAX,SPLMAX,
     -      'Time [microsec]','Current [microamp]',
     -      'Induced currents on group '//AUX(1:NC))
       IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
       IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
       IF(PARTID.NE.'Unknown')
     -      CALL GRCOMM(3,'Particle: '//PARTID)
       AUX(1:10)='Ion tail: '
       NC=10
       IF(LITAIL)THEN
            AUX(NC+1:NC+9)='present, '
            NC=NC+9
       ELSEIF(LRTAIL)THEN
            AUX(NC+1:NC+13)='not sampled, '
            NC=NC+13
       ELSEIF(LDTAIL)THEN
            AUX(NC+1:NC+10)='detailed, '
            NC=NC+10
       ELSE
            AUX(NC+1:NC+8)='absent, '
            NC=NC+8
       ENDIF
       AUX(NC+1:NC+16)='electron pulse: '
       NC=NC+16
       IF(LEPULS)THEN
             AUX(NC+1:NC+7)='present'
             NC=NC+7
       ELSE
             AUX(NC+1:NC+6)='absent'
             NC=NC+6
       ENDIF
       CALL GRCOMM(4,AUX(1:NC))
*   Plot the direct signal of the wire.
       IF(LPLDIR)THEN
            CALL GRATTS('FUNCTION-1','POLYLINE')
            CALL GRLINE(ITMAX-ITMIN+1,TIMSIG(ITMIN),SIGNAL(ITMIN,ISW,1))
       ENDIF
*   Plot the cross induced signal for the wire.
       IF(LCROSS.AND.LPLCR)THEN
            CALL GRATTS('FUNCTION-2','POLYLINE')
            CALL GRLINE(ITMAX-ITMIN+1,TIMSIG(ITMIN),SIGNAL(ITMIN,ISW,2))
       ENDIF
*   Remember that we plotted a signal.
       NPLOT=NPLOT+1
*   Close the plot.
       CALL GRNEXT
*   Log the plot,
       CALL OUTFMT(REAL(ISW),2,AUX,NC,'LEFT')
       CALL GRALOG('Signals on group '//AUX(1:NC)//'.')
100    CONTINUE
*** Print a warning if no plot was made.
       IF(NPLOT.EQ.0)PRINT *,' !!!!!! SIGPLT WARNING : No signal'//
     -      ' eligible for plotting found.'
*** Register the amount of CPU time used.
       CALL TIMLOG('Plotting the signals:                   ')
       END
