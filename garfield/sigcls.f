CDECK  ID>, SIGCLS.
       SUBROUTINE SIGCLS(LDIFF,LAVAL,LATTA,LTRACK,LTRMC,IFAIL)
*-----------------------------------------------------------------------
*   SIGCLS - Subroutine describing the cluster formation, it generates
*            clusters along the track and assigns a number of secondary
*            pairs to each cluster.
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
       REAL XCLUST,YCLUST,ZCLUST,ECLUST,XCLPR,YCLPR,Q,RNDEXP,
     -      VXMIN,VYMIN,VXMAX,VYMAX,EXTRA1
       INTEGER I,NCLUST,ISW,IFAIL,IFAIL1,NCSTAT,NNPAIR
       CHARACTER*50 STATUS
       LOGICAL LDIFF,LAVAL,LATTA,LTRACK,LTRMC,DONE
       EXTERNAL RNDEXP
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGCLS ///'
*** Set the charge.
       Q=-1.0
*** Usually this will work.
       IFAIL=0
*** Check option compatibility.
       IF(LEPULS.AND.LTRACK)THEN
            PRINT *,' !!!!!! SIGCLS WARNING : ELECTRON-PULSE'//
     -           ' is incompatible with INTERPOLATE-TRACK;'//
     -           ' INTERPOLATE-TRACK cancelled.'
            LTRACK=.FALSE.
       ENDIF
       IF(LTRMC.AND.LTRACK)THEN
            PRINT *,' !!!!!! SIGCLS WARNING : MONTE-CARLO-TRACKS'//
     -           ' is incompatible with INTERPOLATE-TRACK;'//
     -           ' INTERPOLATE-TRACK cancelled.'
            LTRACK=.FALSE.
       ENDIF
*** Initialise clustering.
       CALL TRACLI
       NCLUST=0
*** Start a plot of the clusters -if this is requested.
       IF(LCLPLT)THEN
            IF(LTRACK)PRINT *,' ------ SIGCLS MESSAGE : Due to the'//
     -           ' INTERPOLATE-TRACK, electrons will not be plotted.'
*   Layout.
            CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,
     -           'Track, clusters and drift lines         ')
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            IF(PARTID.NE.'Unknown')CALL GRCOMM(3,'Particle: '//PARTID)
       ENDIF
*** Print a table of the clusters not hitting a wire in the cell.
       IF(LCLPRT)THEN
            WRITE(LUNOUT,'(''  Table of the clusters''/
     -           ''  =====================''/)')
            IF(LTRACK)WRITE(LUNOUT,'(''  Note: the data contained in '',
     -           ''this table has been obtained via interpolation.''/)')
            IF(.NOT.POLAR)THEN
                 WRITE(LUNOUT,'(/''     No      x-start      y-start'',
     -                ''      z-start   Drift time    Diffusion Pairs'',
     -                ''  Pair Arrival time Total charge'')')
                 WRITE(LUNOUT,'( ''                [cm]         [cm]'',
     -                ''         [cm]   [microsec]   [microsec]      '',
     -                ''         [microsec]  [electrons]''/)')
            ELSE
                 WRITE(LUNOUT,'(''     No      r-start    phi-start'',
     -                ''      z-start   Drift time    Diffusion Pairs'',
     -                ''  Pair Arrival time Total charge'')')
                 WRITE(LUNOUT,'( ''                [cm]    [degrees]'',
     -                ''         [cm]   [microsec]   [microsec]      '',
     -                ''         [microsec]  [electrons]''/)')
            ENDIF
       ENDIF
*** Start drift lines from the track.
10     CONTINUE
*   Generate a cluster.
       CALL TRACLS(XCLUST,YCLUST,ZCLUST,ECLUST,NNPAIR,EXTRA1,DONE,
     -      IFAIL1)
       IF(DONE)THEN
            GOTO 20
       ELSEIF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! SIGCLS WARNING : Error generating a'//
     -           ' cluster; clustering ended.'
            IFAIL=1
            GOTO 20
       ENDIF
       NCLUST=NCLUST+1
*   Convert cluster position from polar if needed.
       IF(POLAR)THEN
            CALL CFMCTP(XCLUST,YCLUST,XCLPR,YCLPR,1)
            CALL CFMCTR(XCLUST,YCLUST,XCLUST,YCLUST,1)
       ELSE
            XCLPR=XCLUST
            YCLPR=YCLUST
       ENDIF
*** For MC calculation, calculate each electron separately.
       IF(LTRMC)THEN
*   Store the number of pairs as 1 (each is different).
            NPAIR=1
*   Print a line for this cluster.
            IF(LCLPRT)WRITE(LUNOUT,'(1X,I6,3(1X,E12.5),27X,I5)')
     -           NCLUST,XCLPR,YCLPR,ZCLUST,NNPAIR
*   Loop over the electrons and ions in this cluster.
            DO 30 I=1,NNPAIR
*   Compute the electron drift line.
            CALL DLCMC(XCLUST,YCLUST,ZCLUST,Q,1)
*   Store arrival time, ISTAT code for later use.
            TCLUST=TU(NU)
            IF(NFOUR.EQ.0)THEN
                 IF(ISTAT.GT.MXWIRE.AND.ISTAT.LE.2*MXWIRE)THEN
                      ICLUST=ISTAT-MXWIRE
                 ELSE
                      ICLUST=ISTAT
                 ENDIF
            ELSE
                 ICLUST=ISTAT
            ENDIF
*   Ensure the diffusion isn't counted twice.
            SCLUST=0
*   Compute Townsend and attachment coefficients if needed.
            IF(LAVAL.AND.
     -          (AVATYP.EQ.'TOWNSEND'.OR.
     -           AVATYP.EQ.'POLYA-TOWN'.OR.
     -           AVATYP.EQ.'TOWN-FIXED'))CALL DLCTWN(ACLUST)
            IF(LATTA)CALL DLCATT(BCLUST)
*   Store incidence angle.
            CALL DLCPHI(FCLUST)
*   Print the information for this cluster if requested (MC format).
            IF(LCLPRT)THEN
*   Format the status code.
                 CALL DLCSTF(ICLUST,STATUS,NCSTAT)
                 CALL DLCISW(ICLUST,ISW)
                 IF((ICLUST.GE.1.AND.ICLUST.LE.NWIRE).OR.
     -                (ICLUST.GT.2*MXWIRE.AND.
     -                ICLUST.LE.2*MXWIRE+MXSOLI).OR.
     -                (ICLUST.LE.-11.AND.ICLUST.GE.-15))THEN
                      IF(ISW.EQ.0)THEN
                           STATUS=STATUS(1:NCSTAT)//', not read out'
                           NCSTAT=MIN(LEN(STATUS),NCSTAT+14)
                      ELSE
                           STATUS=STATUS(1:NCSTAT)//', read out'
                           NCSTAT=MIN(LEN(STATUS),NCSTAT+10)
                      ENDIF
                 ENDIF
*   Print a line for this cluster.
                 WRITE(LUNOUT,'(84X,A)') STATUS(1:NCSTAT)
            ENDIF
*   Have the signal computed that stems from the electron.
            CALL SIGETR(LDIFF,LAVAL,LATTA,IFAIL1)
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCLS WARNING : Error'//
     -           ' tracing an electron; continuing.'
*   Plot this drift line, if requested - this may destroy (XU,YU).
            IF(LCLPLT)CALL DLCPLT
*   Compute the ion drift line from this point.
            IF(LIPULS)THEN
                 CALL DLCMC(XCLUST,YCLUST,ZCLUST,-Q,2)
*   Have the signal computed that stems from the ion.
                 CALL SIGITR(IFAIL1)
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCLS WARNING :'//
     -                ' Error tracing an ion; continuing.'
*   Plot this drift line, if requested - this may destroy (XU,YU).
                 IF(LCLPLT)CALL DLCPLT
            ENDIF
*   Next pair in the cluster.
30          CONTINUE
*** Analytic drifting with treatment of diffusion by Gauss propagation.
       ELSE
*   Store the number of pairs.
            NPAIR=NNPAIR
**  In case of interpolation, request data for this cluster position.
            IF(LTRACK)THEN
*   Interpolate (also in Cartesian coordinates).
                 CALL DLCTRI(XCLUST,YCLUST,ZCLUST,TCLUST,ICLUST,
     -                SCLUST,ACLUST,BCLUST,FCLUST,LDIFF,LAVAL,LATTA,
     -                IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGCLS WARNING : Interpolation'//
     -                     ' failure for a cluster; cluster skipped.'
                      GOTO 10
                 ENDIF
**  Otherwise compute the average drift line now.
            ELSE
*   Compute the electron drift line.
                 CALL DLCALC(XCLUST,YCLUST,ZCLUST,Q,1)
*   Store arrival time, ISTAT code and integrated diff. for later use.
                 TCLUST=TU(NU)
                 IF(NFOUR.EQ.0)THEN
                      IF(ISTAT.GT.MXWIRE.AND.ISTAT.LE.2*MXWIRE)THEN
                           ICLUST=ISTAT-MXWIRE
                      ELSE
                           ICLUST=ISTAT
                      ENDIF
                 ELSE
                      ICLUST=ISTAT
                 ENDIF
                 IF(LDIFF)CALL DLCDIF(SCLUST)
                 IF(LAVAL.AND.
     -               (AVATYP.EQ.'TOWNSEND'.OR.
     -                AVATYP.EQ.'POLYA-TOWN'.OR.
     -                AVATYP.EQ.'TOWN-FIXED'))CALL DLCTWN(ACLUST)
                 IF(LATTA)CALL DLCATT(BCLUST)
*   Store incidence angle.
                 CALL DLCPHI(FCLUST)
            ENDIF
*   Print the information for this cluster if requested (normal format).
            IF(LCLPRT)THEN
*   Format the status code.
                 CALL DLCSTF(ICLUST,STATUS,NCSTAT)
                 CALL DLCISW(ICLUST,ISW)
                 IF((ICLUST.GE.1.AND.ICLUST.LE.NWIRE).OR.
     -                (ICLUST.GT.2*MXWIRE.AND.
     -                ICLUST.LE.2*MXWIRE+MXSOLI).OR.
     -                (ICLUST.LE.-11.AND.ICLUST.GE.-15))THEN
                      IF(ISW.EQ.0)THEN
                           STATUS=STATUS(1:NCSTAT)//', not read out'
                           NCSTAT=MIN(LEN(STATUS),NCSTAT+14)
                      ELSE
                           STATUS=STATUS(1:NCSTAT)//', read out'
                           NCSTAT=MIN(LEN(STATUS),NCSTAT+14)
                      ENDIF
                 ENDIF
*   Print a line for this cluster.
                 WRITE(LUNOUT,'(1X,I6,5(1X,E12.5),1X,I5,2X,A)')
     -                NCLUST,XCLPR,YCLPR,ZCLUST,TCLUST,SCLUST,NPAIR,
     -                STATUS(1:NCSTAT)
            ENDIF
*   Trace the electron and compute signals.
            CALL SIGETR(LDIFF,LAVAL,LATTA,IFAIL1)
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCLS WARNING : Error'//
     -           ' tracing an electron; continuing.'
*   Plot this drift line, if requested - this may destroy (XU,YU).
            IF(LCLPLT.AND..NOT.LTRACK)CALL DLCPLT
*   Compute the ion drift line, if interested in the primary ion.
            IF(LIPULS)THEN
                 CALL DLCALC(XCLUST,YCLUST,ZCLUST,-Q,2)
*   Trace the electron and compute signals.
                 CALL SIGITR(IFAIL1)
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCLS WARNING :'//
     -                ' Error tracing an ion; continuing.'
*   Plot this drift line, if requested - this may destroy (XU,YU).
                 IF(LCLPLT)CALL DLCPLT
             ENDIF
       ENDIF
*** Go for a new cluster.
       GOTO 10
*** End of loop reached.
20     CONTINUE
*** Finish the plot of the track and the electron drift lines.
       IF(LCLPLT)THEN
*   Draw the track.
            CALL TRAPLT
*   Register the plot.
            CALL GRALOG('Track, cluster and electron drift lines.')
*   Close the plot.
            CALL GRNEXT
       ENDIF
*** Check that there is at least one cluster.
       IF(NCLUST.EQ.0)THEN
            PRINT *,' !!!!!! SIGCLS WARNING : No clusters have been'//
     -           ' generated.'
            IFAIL=1
       ENDIF
       END
