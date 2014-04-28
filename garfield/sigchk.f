CDECK  ID>, SIGCHK.
       SUBROUTINE SIGCHK
*-----------------------------------------------------------------------
*   SIGCHK - Performs some simple checks on the performance of the
*            signal routines.
*   VARIABLES : LAVACH      : if .TRUE.: check avalanche calculation.
*   (Last changed on 21/ 5/96.)
*-----------------------------------------------------------------------
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
       LOGICAL LAVACH,LDIFCH,LCLSCH,LKEEP,USE(MXVAR),EXIST,RSET
       INTEGER NRNDM,NCHA,MODVAR(MXVAR),MODRES(1)
       REAL XPL(MXLIST),YPL(MXLIST),AVER,SIGMA,SDIFF,XSTART,YSTART
       DOUBLE PRECISION XRAN
       CHARACTER*20 AUX,AUX2
       CHARACTER*10 VARCLS(MXVAR)
       SAVE LAVACH,LDIFCH,LCLSCH,NRNDM,XSTART,YSTART,FMIN,FMAX,NCHA,
     -      LKEEP
       DATA VARCLS(1)/'N         '/
       DATA LAVACH,LDIFCH,LCLSCH,LKEEP /.FALSE.,.FALSE.,.FALSE.,.FALSE./
       DATA NRNDM /100000/
       DATA XSTART,YSTART /0.0,0.0/
       DATA FMIN,FMAX /1.0,1.0E10/
       DATA NCHA /100/
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGCHK ///'
*** Decode the argument string.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   The AVALANCHE options.
       IF(INPCMP(I,'AVA#LANCHE').NE.0)THEN
            IF(AVATYP.EQ.'NOT SET')THEN
                 CALL INPMSG(I,'No avalanche type has been set')
            ELSE
                 LAVACH=.TRUE.
            ENDIF
       ELSEIF(INPCMP(I,'NOAVA#LANCHE').NE.0)THEN
            LAVACH=.FALSE.
*   The BINS keyword.
       ELSEIF(INPCMP(I,'BIN#S').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'This keyword has one argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NCHAR,MXCHA)
                 IF(NCHAR.LE.1.OR.NCHAR.GT.MXCHA)THEN
                      CALL INPMSG(I+1,'Inacceptable number of bins.  ')
                 ELSE
                      NCHA=NCHAR
                 ENDIF
            ENDIF
            INEXT=I+2
*   The CLUSTER options.
       ELSEIF(INPCMP(I,'CL#USTER').NE.0)THEN
            TRKLEN=SQRT((XT0-XT1)**2+(YT0-YT1)**2)
            IF(.NOT.GASOK(5))THEN
                 CALL INPMSG(I,'No cluster data available.    ')
            ELSEIF(TRKLEN*CMEAN.LT.0.1)THEN
                 CALL INPMSG(I,'The track is too short.       ')
            ELSE
                 LCLSCH=.TRUE.
            ENDIF
       ELSEIF(INPCMP(I,'NOCL#USTER').NE.0)THEN
            LCLSCH=.FALSE.
*   The diffusion options.
       ELSEIF(INPCMP(I,'DIFF#USION').NE.0)THEN
            IF(.NOT.GASOK(3))THEN
                 CALL INPMSG(I,'No diffusion data available.  ')
            ELSE
                 LDIFCH=.TRUE.
            ENDIF
       ELSEIF(INPCMP(I,'NODIFF#USION').NE.0)THEN
            LDIFCH=.FALSE.
*   Starting point of the drift line (if needed).
       ELSEIF(INPCMP(I,'FR#OM').NE.0)THEN
            IF(I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'This keyword has 2 arguments. ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 IF(POLAR)CALL CFMRTP(XSTART,YSTART,XSTART,YSTART,1)
                 CALL INPRDR(I+1,XSTART,XSTART)
                 CALL INPRDR(I+2,YSTART,XSTART)
                 IF(POLAR)THEN
                      CALL CFMPTR(XSTART,YSTART,XSTART,YSTART,1,IFAIL)
                      IF(IFAIL.NE.0)THEN
                           CALL INPMSG(I+1,
     -                          'Illegal polar coordinate.     ')
                           XSTART=0.0
                           YSTART=0.0
                      ENDIF
                 ENDIF
            ENDIF
            INEXT=I+3
*   Histogram keeping option.
       ELSEIF(INPCMP(I,'KEEP-#HISTOGRAMS').NE.0)THEN
            LKEEP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP-#HISTOGRAMS').NE.0)THEN
            LKEEP=.FALSE.
*   The RANGE for the avalanche multiplication histogram.
       ELSEIF(INPCMP(I,'R#ANGE').NE.0)THEN
            IF(I+2.GT.NWORD)THEN
                 CALL INPMSG(I,'This keyword has 2 arguments. ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,FMINR,FMIN)
                 CALL INPRDR(I+2,FMAXR,FMAX)
                 IF(FMINR.LE.0.OR.FMAXR.LE.0)THEN
                      CALL INPMSG(I,'Both arguments must be > 0.   ')
                 ELSEIF(FMINR.EQ.FMAXR)THEN
                      CALL INPMSG(I,'A zero range is not permitted.')
                 ELSE
                      FMIN=MIN(FMINR,FMAXR)
                      FMAX=MAX(FMINR,FMAXR)
                 ENDIF
            ENDIF
            INEXT=I+3
*   The repeat counter.
       ELSEIF(INPCMP(I,'REP#EAT')+INPCMP(I,'N').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'this keyword has one argument.')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NRNDMR,100000)
                 IF(NRNDMR.LE.0)THEN
                      CALL INPMSG(I+1,'The repeat counter is not > 0.')
                 ELSE
                      NRNDM=NRNDMR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Unknown option.
       ELSE
            CALL INPMSG(I,'The option is not known.      ')
       ENDIF
10     CONTINUE
       CALL INPERR
*** Carry out the AVALANCHE test, if requested.
       IF(LAVACH.AND.AVATYP.NE.'NOT SET')THEN
            CALL PROINT('Avalanche check',1,6)
*   Print some general information.
            WRITE(LUNOUT,'(''1 CHECK OF THE AVALANCHE CALCULATION''//
     -           ''  The avalanche type has been set to '',A)') AVATYP
            WRITE(LUNOUT,'(''  The avalanche will be simulated '',I7,
     -           '' times in this test.'')') NRNDM
*   For Townsend, print extra information and calculate a drift line.
            IF(AVATYP.EQ.'TOWNSEND'.OR.AVATYP.EQ.'POLYA-TOWN'.OR.
     -           AVATYP.EQ.'TOWN-FIXED')THEN
                 CALL PROFLD(1,'Computing drift line',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL DLCALC(XSTART,YSTART,0.0,-1.0,1)
                 CALL DLCTWN(ACLUST)
                 XPRT=XSTART
                 YPRT=YSTART
                 IF(POLAR)CALL CFMRTP(XSTART,YSTART,XPRT,YPRT,1)
                 WRITE(LUNOUT,'(/''  The drift-line over which the'',
     -                '' Townsend coefficient is integrated'')')
                 WRITE(LUNOUT,'(''  starts at ('',E15.8,'','',E15.8,
     -                ''). Its ISTAT code is '',I3,''.'')')
     -                XPRT,YPRT,ISTAT
                 WRITE(LUNOUT,'(''  The drift-time is expected to be '',
     -                E15.8,'' microsec. The calculation'')') TU(NU)
                 WRITE(LUNOUT,'(''  needed '',I3,'' steps. An average'',
     -                '' avalanche should create '',E15.8,'' pairs.'')')
     -                NU,ACLUST
                 IF(NU.LE.2.OR.ISTAT.EQ.-2.OR.ISTAT.EQ.-3)THEN
                      WRITE(LUNOUT,'(/''  There is no point in perfor'',
     -                     ''ming the test under these conditions.'')')
                      RETURN
                 ENDIF
            ELSE
                 ACLUST=1.0
            ENDIF
*   Initialize the histogram.
            CALL PROFLD(1,'Histogram allocation',-1.0)
            CALL PROSTA(1,0.0)
            CALL HISADM('ALLOCATE',IHIST,NCHA,FMIN,FMAX,.FALSE.,IFAIL1)
*   Generate entries.
            CALL PROFLD(1,'MC cycles',REAL(NRNDM))
            DO 30 I=1,NRNDM
            IF(I.EQ.(NRNDM/20)*(I/(NRNDM/20)))CALL PROSTA(1,REAL(I))
            CALL SIGAVA(FACTOR,ACLUST)
            CALL HISENT(IHIST,FACTOR,1.0)
30          CONTINUE
*   Plot the histogram.
            CALL PROFLD(1,'Histogram plotting',-1.0)
            CALL PROSTA(1,0.0)
            CALL HISPLT(IHIST,'Multiplication factor',
     -           'CHECK ON THE GENERATION OF AVALANCHES',.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
*   Get information about the histogram.
            CALL HISINQ(IHIST,EXIST,RSET,
     -           NNCHA,XXMIN,XXMAX,NNENT,AVER,SIGMA)
            CALL OUTFMT(AVER,2,AUX,NC,'LEFT')
            CALL OUTFMT(SIGMA,2,AUX2,NC2,'LEFT')
            CALL GRCOMM(4,'Mean: '//AUX(1:NC)//', width: '//AUX2(1:NC2))
*   Close the plot.
            CALL GRNEXT
            CALL GRALOG('Check: Avalanche multiplication.')
*   Keep the histogram if requested.
            IF(LKEEP)THEN
                 CALL PROFLD(1,'Histogram saving',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISSAV(IHIST,'AVALANCHE',IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      PRINT *,' ------ SIGCHK MESSAGE : Avalanche'//
     -                     ' histogram kept as AVALANCHE.'
                 ELSE
                      PRINT *,' !!!!!! SIGCHK WARNING : Avalanche'//
     -                     ' histogram not saved.'
                      CALL HISADM('DELETE',IHIST,0,0.0,0.0,.TRUE.,
     -                     IFAIL2)
                 ENDIF
*   Otherwise delete it.
            ELSE
                 CALL PROFLD(1,'Histogram deletion',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISADM('DELETE',IHIST,0,0.0,0.0,.TRUE.,IFAIL2)
            ENDIF
*   End of progress printing.
            CALL PROEND
       ENDIF
*** Carry out the DIFFUSION test, if requested.
       IF(LDIFCH.AND.GASOK(3))THEN
*   Print some general information.
            WRITE(LUNOUT,'(''1 CHECK OF THE DIFFUSION CALCULATION''//
     -           ''  The longitudinal diffusion process will be'',
     -           '' simulated '',I7/''  times in this test, by'',
     -           '' repeatedly drawing an arrival time.   '')') NRNDM
            CALL PROINT('Diffusion check',1,6)
            CALL PROFLD(1,'Computing drift line',-1.0)
            CALL PROSTA(1,0.0)
            XPRT=XSTART
            YPRT=YSTART
            IF(POLAR)CALL CFMRTP(XSTART,YSTART,XPRT,YPRT,1)
            CALL DLCALC(XSTART,YSTART,0.0,-1.0,1)
            CALL DLCDIF(SDIFF)
            WRITE(LUNOUT,'(/''  The drift-line over which the'',
     -           '' diffusion coefficient is to be integrated'')')
            WRITE(LUNOUT,'(''  starts at ('',E15.8,'','',E15.8,
     -           ''). Its ISTAT code is '',I3,''.'')') XPRT,YPRT,ISTAT
            WRITE(LUNOUT,'(''  The drift-time and the estimate'',
     -           '' for the average diffusion are '',E15.8)') TU(NU)
            WRITE(LUNOUT,'(''  and '',E15.5,''. The drift line was'',
     -            '' calculated in '',I3,'' steps.'')') SDIFF,NU
            IF(NU.LE.2.OR.ISTAT.EQ.-2.OR.ISTAT.EQ.-3)THEN
                 WRITE(LUNOUT,'(/''  There is no point in performing'',
     -                '' the test under these conditions.'')')
                 RETURN
            ENDIF
*   Initialize the histogram.
            CALL PROFLD(1,'Histogram allocation',-1.0)
            CALL PROSTA(1,0.0)
            CALL HISADM('ALLOCATE',IHIST,NCHA,0.0,0.0,.TRUE.,IFAIL1)
*   Generate entries.
            CALL PROFLD(1,'MC cycles',REAL(NRNDM))
            DO 50 I=1,NRNDM
            IF(I.EQ.(NRNDM/20)*(I/(NRNDM/20)))CALL PROSTA(1,REAL(I))
            DIFF=RNDNOR(REAL(TU(NU)),SDIFF)
            CALL HISENT(IHIST,DIFF,1.0)
50          CONTINUE
*   Plot the histogram.
            CALL HISPLT(IHIST,'Arrival time [microsec]',
     -           'CHECK ON THE DIFFUSION',.TRUE.)
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
*   Get information about the histogram.
            CALL HISINQ(IHIST,EXIST,RSET,
     -           NNCHA,XXMIN,XXMAX,NNENT,AVER,SIGMA)
            CALL OUTFMT(AVER,2,AUX,NC,'LEFT')
            CALL OUTFMT(SIGMA,2,AUX2,NC2,'LEFT')
            CALL GRCOMM(4,'Mean: '//AUX(1:NC)//', width: '//AUX2(1:NC2))
*   Close the plot.
            CALL GRNEXT
            CALL GRALOG('Check: spread due to diffusion.         ')
*   Keep the histogram if requested.
            IF(LKEEP)THEN
                 CALL PROFLD(1,'Histogram saving',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISSAV(IHIST,'DIFFUSION',IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      PRINT *,' ------ SIGCHK MESSAGE : Diffusion'//
     -                     ' histogram kept as DIFFUSION.'
                 ELSE
                      PRINT *,' !!!!!! SIGCHK WARNING : Diffusion'//
     -                     ' histogram not saved.'
                      CALL HISADM('DELETE',IHIST,0,0.0,0.0,.TRUE.,
     -                     IFAIL2)
                 ENDIF
*   Otherwise delete it.
            ELSE
                 CALL PROFLD(1,'Histogram deletion',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISADM('DELETE',IHIST,0,0.0,0.0,.TRUE.,IFAIL2)
            ENDIF
*   End of progress printing.
            CALL PROEND
       ENDIF
*** Carry out the CLUSTER test, if requested.
       IF(LCLSCH.AND.GASOK(5))THEN
*   Prepare for progress printing.
            CALL PROINT('Clustering check',1,6)
*   Obtain track length and set range.
            TRKLEN=SQRT((XT0-XT1)**2+(YT0-YT1)**2)
*   Print some general information.
            WRITE(LUNOUT,'(''1 CHECK OF THE CLUSTER FORMATION''//
     -           ''  Clusters will be generated '',I7,'' times on the'',
     -           '' current track, their number''/''  is counted each'',
     -           '' time and the distribution is plotted.'')') NRNDM
            WRITE(LUNOUT,'(''  The track has a length of '',E12.4,
     -           '' cm; the average number of clusters per''/
     -           ''  cm for this gas is '',E12.4,''.'')') TRKLEN,CMEAN
*   Initialize the histogram.
            CALL PROFLD(1,'Histogram allocation',-1.0)
            CALL PROSTA(1,0.0)
            NAUX=NINT(2*TRKLEN*CMEAN)+1
            IF(NAUX.GT.MXCHA)THEN
                 CALL HISADM('ALLOCATE',ICLUS,MXCHA,
     -                -0.5,-0.5+MXCHA*(((NAUX-1)/MXCHA)+1),
     -                .FALSE.,IFAIL1)
            ELSE
                 CALL HISADM('ALLOCATE',ICLUS,NAUX,-0.5,NAUX-0.5,
     -                .FALSE.,IFAIL1)
            ENDIF
            NAUX=NINT(2*TRKLEN*CMEAN*CLSAVE)+1
            IF(NAUX.GT.MXCHA)THEN
                 CALL HISADM('ALLOCATE',IELEC,MXCHA,
     -                -0.5,-0.5+MXCHA*(((NAUX-1)/MXCHA)+1),
     -                .FALSE.,IFAIL1)
            ELSE
                 CALL HISADM('ALLOCATE',IELEC,NAUX,-0.5,NAUX-0.5,
     -                .FALSE.,IFAIL1)
            ENDIF
*   Generate entries.
            CALL PROFLD(1,'Tracks',REAL(NRNDM))
            DO 90 I=1,NRNDM
            IF(I.EQ.(NRNDM/20)*(I/(NRNDM/20)))CALL PROSTA(1,REAL(I))
            NCLSCH=0
            NELECH=0
            DIST=0
70          CONTINUE
            DIST=DIST+RNDEXP(REAL(1)/CMEAN)
            IF(DIST.GT.TRKLEN)GOTO 80
            NCLSCH=NCLSCH+1
            CALL HISRAD(CLSDIS,NCLS,0.0D0,1.0D0,XRAN)
            NELECH=NELECH+INT(XRAN)
            GOTO 70
80          CONTINUE
            CALL HISENT(ICLUS,REAL(NCLSCH),1.0)
            CALL HISENT(IELEC,REAL(NELECH),1.0)
90          CONTINUE
*   Plot the histograms.
            CALL PROFLD(1,'Histogram plotting',-1.0)
            CALL PROSTA(1,0.0)
            CALL HISPLT(ICLUS,'Number of clusters on the track',
     -           'CHECK ON THE CLUSTER GENERATION',.TRUE.)
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            CALL HISINQ(ICLUS,EXIST,RSET,
     -           NNCHA,XXMIN,XXMAX,NNENT,AVER,SIGMA)
            CALL OUTFMT(AVER,2,AUX,NC,'LEFT')
            CALL OUTFMT(SIGMA,2,AUX2,NC2,'LEFT')
            CALL GRCOMM(4,'Mean: '//AUX(1:NC)//', width: '//AUX2(1:NC2))
            CALL GRNEXT
            CALL GRALOG('Check: cluster generation, cluster count')
            CALL HISPLT(IELEC,'Number of electrons on the track',
     -           'CHECK ON THE CLUSTER GENERATION',.TRUE.)
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            CALL HISINQ(IELEC,EXIST,RSET,
     -           NNCHA,XXMIN,XXMAX,NNENT,AVER,SIGMA)
            IF(AVER.GT.0)THEN
                 CALL OUTFMT(SIGMA**2/AVER,2,AUX,NC,'LEFT')
                 CALL GRCOMM(2,'Sigma**2/Mean: '//AUX(1:NC))
            ELSE
                 CALL GRCOMM(2,'Sigma**2/Mean: < mean zero>')
            ENDIF
            CALL OUTFMT(AVER,2,AUX,NC,'LEFT')
            CALL OUTFMT(SIGMA,2,AUX2,NC2,'LEFT')
            CALL GRCOMM(4,'Mean: '//AUX(1:NC)//', width: '//AUX2(1:NC2))
            CALL GRNEXT
            CALL GRALOG('Check: cluster generation, e- count     ')
*   Keep the histograms if requested.
            IF(LKEEP)THEN
                 CALL PROFLD(1,'Histogram saving',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISSAV(ICLUS,'CLUSTERS',IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      PRINT *,' ------ SIGCHK MESSAGE : Cluster'//
     -                     ' count histogram kept as CLUSTERS.'
                 ELSE
                      PRINT *,' !!!!!! SIGCHK WARNING : Cluster'//
     -                     ' count histogram not saved.'
                      CALL HISADM('DELETE',ICLUS,0,0.0,0.0,.TRUE.,
     -                     IFAIL2)
                 ENDIF
                 CALL HISSAV(IELEC,'ELECTRONS',IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      PRINT *,' ------ SIGCHK MESSAGE : Electron'//
     -                     ' count histogram kept as ELECTRONS.'
                 ELSE
                      PRINT *,' !!!!!! SIGCHK WARNING : Electron'//
     -                     ' count histogram not saved.'
                      CALL HISADM('DELETE',IELEC,0,0.0,0.0,.TRUE.,
     -                     IFAIL2)
                 ENDIF
*   Otherwise delete it.
            ELSE
                 CALL PROFLD(1,'Histogram deletion',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISADM('DELETE',ICLUS,0,0.0,0.0,.TRUE.,IFAIL2)
                 CALL HISADM('DELETE',IELEC,0,0.0,0.0,.TRUE.,IFAIL2)
            ENDIF
**  Sample the cluster size distribution and plot a histogram.
            WRITE(LUNOUT,'(/''  In the next test, a cluster size will'',
     -           '' be drawn '',I7,'' times.'')') NRNDM
            IF(CLSTYP.EQ.'FUNCTION'.OR.CLSTYP.EQ.'LANDAU')WRITE(LUNOUT,
     -           '('' The dotted curve represents the function'',
     -           '' the histogram is expected to follow.'')')
*   Initialise the histogram.
            CALL PROFLD(1,'Histogram allocation',-1.0)
            CALL PROSTA(1,0.0)
            CALL HISADM('ALLOCATE',IHIST,MIN(MXCHA,MXPAIR,NCLS),
     -           -0.5,-0.5+MIN(MXCHA,MXPAIR,NCLS),.FALSE.,IFAIL1)
*   Generate pairs.
            CALL PROFLD(1,'Clusters',REAL(NRNDM))
            DO 110 I=1,NRNDM
            IF(I.EQ.(NRNDM/20)*(I/(NRNDM/20)))CALL PROSTA(1,REAL(I))
            CALL HISRAD(CLSDIS,NCLS,0.0D0,1.0D0,XRAN)
            CALL HISENT(IHIST,REAL(XRAN),1.0)
110         CONTINUE
*   Plot the histogram with GRHIST,
            CALL PROFLD(1,'Histogram plotting',-1.0)
            CALL PROSTA(1,0.0)
            CALL GRAOPT('LIN-X, LOG-Y')
            CALL HISPLT(IHIST,'Cluster Size',
     -           'CHECK ON THE CLUSTER SIZE DISTRIBUTION',.TRUE.)
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            CALL HISINQ(IHIST,EXIST,RSET,
     -           NNCHA,XXMIN,XXMAX,NNENT,AVER,SIGMA)
            CALL OUTFMT(AVER,2,AUX,NC,'LEFT')
            CALL OUTFMT(SIGMA,2,AUX2,NC2,'LEFT')
            CALL GRCOMM(4,'Mean: '//AUX(1:NC)//', width: '//AUX2(1:NC2))
*   Get rid of the histogram.
            IF(LKEEP)THEN
                 CALL PROFLD(1,'Histogram saving',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISSAV(IHIST,'PAIRS',IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      PRINT *,' ------ SIGCHK MESSAGE : Pair'//
     -                     ' count histogram kept as PAIRS.'
                 ELSE
                      PRINT *,' !!!!!! SIGCHK WARNING : Pair'//
     -                     ' count histogram not saved.'
                      CALL HISADM('DELETE',IHIST,0,0.0,0.0,.TRUE.,
     -                     IFAIL1)
                 ENDIF
            ELSE
                 CALL PROFLD(1,'Histogram deletion',-1.0)
                 CALL PROSTA(1,0.0)
                 CALL HISADM('DELETE',IHIST,0,0.0,0.0,.TRUE.,IFAIL2)
            ENDIF
**  Skip plotting the function if of inappropriate type.
            IENTRY=0
            IF(CLSTYP.NE.'FUNCTION'.AND.CLSTYP.NE.'LANDAU')GOTO 150
*   Pass the cluster function through ALGPRE.
            IF(CLSTYP.EQ.'FUNCTION')THEN
                 CALL PROFLD(1,'Function plotting',-1.0)
                 CALL PROSTA(1,0.0)
                 IF(INDEX(FCNCLS(1:NFCLS),'@').NE.0)THEN
                      GOTO 150
                 ELSE
                      CALL ALGPRE(FCNCLS,NFCLS,VARCLS,1,NRES,USE,
     -                     IENTRY,IFAIL1)
                      IF(IFAIL1.NE.0.OR.NRES.NE.1)THEN
                           CALL ALGCLR(IENTRY)
                           GOTO 150
                      ENDIF
                 ENDIF
            ENDIF
*   And calculate it.
            SUM=0.0
            NTERM=MAX(3,2*INT(MXLIST/2)-1)
            DO 130 I=1,NTERM
            XPL(I)=(I-1)*REAL(NCLS)/REAL(NTERM-1)
            XVAL=(I-0.5)*REAL(NCLS)/REAL(NTERM-1)
            IF(CLSTYP.EQ.'FUNCTION')THEN
                 MODVAR(1)=2
                 CALL ALGEXE(IENTRY,XVAL,MODVAR,1,
     -                YPL(I),MODRES,1,IFAIL1)
            ELSEIF(CLSTYP.EQ.'LANDAU')THEN
                 IF((CMEAN*XVAL*EPAIR-EMPROB)/(1.54E5*(Z/A)*RHO)-
     -                LOG(CMEAN).LT.-5.0)THEN
                      YPL(I)=0.0
                 ELSE
                      YPL(I)=DENLAN((CMEAN*XVAL*EPAIR-EMPROB)/
     -                     (1.54E5*(Z/A)*RHO)-LOG(CMEAN))
                 ENDIF
            ENDIF
*   integrate the cluster function to be able to plot it to scale,
            IF(I.EQ.2*INT(I/2))SUM=SUM+4.0*YPL(I)
            IF(I.NE.2*INT(I/2))SUM=SUM+2.0*YPL(I)
130         CONTINUE
            SUM=NCLS*(SUM-YPL(1)-YPL(NTERM))/(3.0*(NTERM-1))
*   normalise the calculated distribution and clip above MAXHST,
            DO 140 I=1,NTERM
            YPL(I)=YPL(I)*REAL(NRNDM)/SUM
140         CONTINUE
*   and finally plot the cluster function on top of the histogram.
            CALL GRATTS('FUNCTION-2','POLYLINE')
            CALL GRLINE(NTERM,XPL,YPL)
*   Jump to this lable if no function is to be plotted.
150         CONTINUE
*   Print number of algebra error messages.
            CALL ALGERR
*   Clear the function used for plotting the cluster distribution.
            IF(CLSTYP.EQ.'FUNCTION'.AND.IENTRY.NE.0)CALL ALGCLR(IENTRY)
*   Close the graphics output.
            CALL GRNEXT
            CALL GRALOG('Check: cluster size distribution.       ')
            CALL GRAOPT('LIN-X, LIN-Y')
*   End of progress printing.
            CALL PROEND
       ENDIF
*** Logging of CPU time used for these checks.
       CALL TIMLOG('Various signal related checks:          ')
       END
