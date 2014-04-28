CDECK  ID>, SIGGEN.
       SUBROUTINE SIGGEN(CHANGE)
*-----------------------------------------------------------------------
*   SIGGEN - Routine computing a single signal.
*   VARIABLES : CHANGE      : see routine SIGINP
*   (Last changed on 13/ 3/05.)
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
       INTEGER NASIMR,NISIMR,NORIAR,JIORDR,IFAIL,IFAIL1,
     -      INEXT,I,J,NWORD,INPTYP,INPCMP
       REAL PRSR,XT0P,YT0P,XT1P,YT1P,XR0,XR1,YR0,YR1
       DOUBLE PRECISION DUMMY(1)
       LOGICAL CHANGE,LSIGAD,LDIFF,LAVAL,LATTA,LTRACK,LTRMC,OK
       EXTERNAL INPTYP,INPCMP
       SAVE LDIFF ,LAVAL ,LATTA ,LTRACK ,LTRMC
       DATA LDIFF ,LAVAL ,LATTA ,LTRACK ,LTRMC
     -     /.TRUE.,.TRUE.,.TRUE.,.FALSE.,.FALSE./
*** Identify the routine
       IF(LIDENT)PRINT *,' /// ROUTINE SIGGEN ///'
*** Reset the addition flag each time.
       LSIGAD=.FALSE.
*** Ensure that we can do this, assume for a start this is OK.
       OK=.TRUE.
*   Check location definition.
       IF(.NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The track location'//
     -           ' has not been set.'
            OK=.FALSE.
       ELSE
*   Check that the track lies at least partially in the drift area.
            IF(POLAR)THEN
                 IFAIL=0
                 CALL CFMCTR(XT0,YT0,XR0,YR0,1)
                 CALL CFMCTR(XT1,YT1,XR1,YR1,1)
                 IF(XR0.LT.DXMIN.OR.XR0.GT.DXMAX.OR.
     -                XR1.LT.DXMIN.OR.XR1.GT.DXMAX.OR.
     -                YR0.LT.DYMIN.OR.YR0.GT.DYMAX.OR.
     -                YR1.LT.DYMIN.OR.YR1.GT.DYMAX)THEN
                      PRINT *,' !!!!!! SIGGEN WARNING : The track'//
     -                     ' is located at least partialy outside'//
     -                     ' the drift area.'
                      OK=.FALSE.
                 ENDIF
            ELSE
                 CALL CLIP3(XT0,YT0,ZT0,XT1,YT1,ZT1,
     -                DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX,IFAIL)
                 IF(IFAIL.NE.0)THEN
                      PRINT *,' !!!!!! SIGGEN WARNING : The track is'//
     -                     ' not located at least partially in the'//
     -                     ' drift area.'
                      OK=.FALSE.
                 ENDIF
            ENDIF
       ENDIF
*   Clustering model.
       IF(ITRTYP.EQ.0)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The clustering model'//
     -           ' has yet not been set.'
            OK=.FALSE.
       ENDIF
*   Avalanche type.
       IF(AVATYP.EQ.'NOT SET')THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The avalanche type has'//
     -           ' not yet been set.'
            OK=.FALSE.
       ENDIF
*   Avalanche type.
       IF(.NOT.RESSET)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The time resolution has'//
     -           ' not yet been set.'
            OK=.FALSE.
            DO 60 I=1,NTIME
            TIMSIG(I)=TSTART+(I-1)*TDEV
60          CONTINUE
       ENDIF
*   Return if not OK.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### SIGGEN ERROR   : No signal simulation'//
     -           ' because of the above warnings.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### SIGGEN ERROR   : Program terminated'//
     -           ' because of the above warnings.'
            CALL QUIT
       ELSEIF(.NOT.OK)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : No signal simulation'//
     -           ' because of the above warnings.'
            RETURN
       ENDIF
*** Decode the argument string.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Look for the electron-pulse options.
       IF(INPCMP(I,'NOELE#CTRON-#PULSE').NE.0)THEN
            LEPULS=.FALSE.
       ELSEIF(INPCMP(I,'ELE#CTRON-#PULSE').NE.0)THEN
            LEPULS=.TRUE.
*   Look for the ion-pulse options.
       ELSEIF(INPCMP(I,'NOPRIM#ARY-ION-#PULSE')+
     -      INPCMP(I,'NOION-P#ULSE').NE.0)THEN
            LIPULS=.FALSE.
       ELSEIF(INPCMP(I,'PRIM#ARY-ION-#PULSE')+
     -      INPCMP(I,'ION-P#ULSE').NE.0)THEN
            LIPULS=.TRUE.
*   Look for the ion-tail options.
       ELSEIF(INPCMP(I,'NOION-T#AIL').NE.0)THEN
            LITAIL=.FALSE.
            LDTAIL=.FALSE.
            LRTAIL=.FALSE.
       ELSEIF(INPCMP(I,'ION-T#AIL')+
     -      INPCMP(I,'SIM#PLE-ION-T#AIL').NE.0)THEN
            LITAIL=.TRUE.
            LDTAIL=.FALSE.
            LRTAIL=.FALSE.
       ELSEIF(INPCMP(I,'DET#AILED-ION-T#AIL').NE.0)THEN
            LITAIL=.FALSE.
            LDTAIL=.TRUE.
            LRTAIL=.FALSE.
       ELSEIF(INPCMP(I,'NONSAMP#LED-ION-T#AIL').NE.0)THEN
            LITAIL=.FALSE.
            LDTAIL=.FALSE.
            LRTAIL=.TRUE.
*   Look for the cross-talk options.
       ELSEIF(INPCMP(I,'NOCR#OSS-#INDUCED-#SIGNAL').NE.0)THEN
            LCROSS=.FALSE.
       ELSEIF(INPCMP(I,'CR#OSS-#INDUCED-#SIGNAL').NE.0)THEN
            IF(.NOT.LCROSS)CHANGE=.TRUE.
            LCROSS=.TRUE.
*   Look for the diffusion options.
       ELSEIF(INPCMP(I,'NODIFF#USION').NE.0)THEN
            LDIFF=.FALSE.
       ELSEIF(INPCMP(I,'DIFF#USION').NE.0)THEN
            LDIFF=.TRUE.
*   Look for the track interpolation options.
       ELSEIF(INPCMP(I,'NOINT#ERPOLATE-TR#ACK').NE.0)THEN
            LTRACK=.FALSE.
       ELSEIF(INPCMP(I,'INT#ERPOLATE-TR#ACK').NE.0)THEN
            IF(.NOT.TRASET)THEN
                 CALL INPMSG(I,'The track is not prepared.')
            ELSE
                 LTRACK=.TRUE.
            ENDIF
*   Look for the Monte Carlo option.
       ELSEIF(INPCMP(I,'NOMC-DR#IFT-#LINES')+
     -      INPCMP(I,'NOM#ONTE-C#ARLO-DR#IFT-#LINES')+
     -      INPCMP(I,'RUN#GE-K#UTTA-DR#IFT-#LINES').NE.0)THEN
            LTRMC=.FALSE.
       ELSEIF(INPCMP(I,'MC-DR#IFT-#LINES')+
     -      INPCMP(I,'M#ONTE-C#ARLO-DR#IFT-#LINES').NE.0)THEN
            LTRMC=.TRUE.
*   Look for the avalanche options.
       ELSEIF(INPCMP(I,'NOAVAL#ANCHE').NE.0)THEN
            LAVAL=.FALSE.
       ELSEIF(INPCMP(I,'AVAL#ANCHE').NE.0)THEN
            IF(AVATYP.EQ.'NOT SET')PRINT *,' !!!!!! SIGGEN WARNING :',
     -           ' No avalanche specification seen so far; fixed',
     -           ' (perhaps default) factor used.'
            LAVAL=.TRUE.
*   Look for the attachment options.
       ELSEIF(INPCMP(I,'NOATT#ACHMENT').NE.0)THEN
            LATTA=.FALSE.
       ELSEIF(INPCMP(I,'ATT#ACHMENT').NE.0)THEN
            IF(.NOT.GASOK(6))THEN
                 CALL INPMSG(I,'No attachment data')
            ELSE
                 LATTA=.TRUE.
            ENDIF
*   Look for the ADD/NEW option.
       ELSEIF(INPCMP(I,'ADD').NE.0)THEN
            LSIGAD=.TRUE.
       ELSEIF(INPCMP(I,'NEW').NE.0)THEN
            LSIGAD=.FALSE.
*   Look for an angular spread function.
       ELSEIF(INPCMP(I,'ANG#ULAR-SP#READ').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'The function is missing.')
            ELSEIF(INPCMP(I+1,'FL#AT').NE.0)THEN
                 NCANG=1
                 FCNANG='1'
                 IENANG=0
                 LITAIL=.TRUE.
            ELSE
                 IENANG=0
                 LITAIL=.TRUE.
                 CALL INPSTR(I+1,I+1,FCNANG,NCANG)
            ENDIF
            INEXT=I+2
       ELSEIF(INPCMP(I,'NOANG#ULAR-SP#READ').NE.0)THEN
            IENANG=0
            NCANG=0
*   Look for angular integration options.
       ELSEIF(INPCMP(I,'ANG#ULAR-INT#EGRATION-P#OINTS').NE.0)THEN
            IF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NASIMR,2)
                 IF(NASIMR.GT.0)THEN
                      NASIMP=NASIMR
                      LITAIL=.TRUE.
                 ELSE
                      CALL INPMSG(I+1,'Number out of range.')
                 ENDIF
                 INEXT=I+2
             ELSEIF(INPTYP(I+1).EQ.4)THEN
                 NASIMP=2
                 INEXT=I+2
                 LITAIL=.TRUE.
             ENDIF
*   Look for number of ion angles.
       ELSEIF(INPCMP(I,'ION-ANG#LES').NE.0)THEN
            IF(INPCMP(I+1,'NOSAMP#LING')+INPCMP(I+1,'NOSAMP#LES')+
     -           INPCMP(I+1,'INF#INITE').NE.0)THEN
                 LRTAIL=.TRUE.
            ELSEIF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NORIAR,MXORIA)
                 IF(NORIAR.GE.1.AND.NORIAR.LE.MXORIA)THEN
                      IF(NORIAR.NE.NORIA)CHANGE=.TRUE.
                      LITAIL=.TRUE.
                      NORIA=NORIAR
                 ELSE
                      CALL INPMSG(I+1,'Number out of range.')
                 ENDIF
                 INEXT=I+2
             ELSEIF(INPTYP(I+1).EQ.4)THEN
                 LITAIL=.TRUE.
                 NORIA=MIN(50,MXORIA)
                 INEXT=I+2
             ENDIF
*   Look for signal averaging / sampling options.
       ELSEIF(INPCMP(I,'SAMP#LE-#SIGNAL').NE.0)THEN
            NISIMP=0
       ELSEIF(INPCMP(I,'SUM-#SIGNAL').NE.0)THEN
            NISIMP=-1
       ELSEIF(INPCMP(I,'AVER#AGE-#SIGNAL').NE.0)THEN
            IF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NISIMR,2)
                 IF(NISIMR.GT.0.AND.IFAIL1.EQ.0)THEN
                      NISIMP=NISIMR
                 ELSE
                      CALL INPMSG(I+1,'Not a valid number.')
                 ENDIF
                 INEXT=I+2
             ELSEIF(INPTYP(I+1).EQ.4)THEN
                 NISIMP=2
                 INEXT=I+2
             ENDIF
*   Signal interpolation order.
       ELSEIF(INPCMP(I,'INT#ERPOLATION-ORD#ER').NE.0)THEN
            IF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,JIORDR,2)
                 IF(JIORDR.GT.0.AND.IFAIL1.EQ.0)THEN
                      JIORD=JIORDR
                 ELSE
                      CALL INPMSG(I+1,'Not a valid number.')
                 ENDIF
                 INEXT=I+2
             ELSEIF(INPTYP(I+1).EQ.4)THEN
                 JIORD=1
                 INEXT=I+2
             ENDIF
*   Ion production threshold.
       ELSEIF(INPCMP(I,'ION-THR#ESHOLD').NE.0)THEN
            IF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,PRSR,PRSTHR)
                 IF(PRSR.GE.0.AND.PRSR.LT.1.AND.IFAIL1.EQ.0)THEN
                      PRSTHR=PRSR
                      LDTAIL=.TRUE.
                 ELSE
                      CALL INPMSG(I+1,'Not a valid number.')
                 ENDIF
                 INEXT=I+2
             ELSEIF(INPTYP(I+1).EQ.4)THEN
                 PRSTHR=1.0E-4
                 INEXT=I+2
                 LDTAIL=.TRUE.
             ENDIF
*   The option is not known.
       ELSE
            CALL INPMSG(I,'The option is not known.      ')
       ENDIF
10     CONTINUE
*** Print the error messages.
       CALL INPERR
*** Check consistency of options.
       IF(LTRACK.AND.LEPULS)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The INTERPOLATE-TRACK'//
     -           ' and ELECTRON-PULSE options'
            PRINT *,'                         are mutually exclusive'//
     -           ' ; INTERPOLATE-TRACK cancelled.'
            LTRACK=.FALSE.
       ENDIF
       IF(LTRACK.AND.LDTAIL)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The INTERPOLATE-TRACK'//
     -           ' and DETAILED-ION-TAIL options'
            PRINT *,'                         are mutually exclusive'//
     -           ' ; INTERPOLATE-TRACK cancelled.'
            LTRACK=.FALSE.
       ENDIF
       IF(LTRACK.AND.LTRMC)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : The INTERPOLATE-TRACK'//
     -           ' and MONTE-CARLO-TRACKS options'
            PRINT *,'                         are mutually exclusive'//
     -           ' ; INTERPOLATE-TRACK cancelled.'
            LTRACK=.FALSE.
       ENDIF
       IF(LEPULS.AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! SIGGEN WARNING : ELECTRON-PULSE has been'//
     -           ' requested, but the Townsend'
            PRINT *,'                         coefficients are'//
     -           ' missing ; ELECTRON-PULSE cancelled.'
            LEPULS=.FALSE.
       ENDIF
       IF(LTRMC.AND..NOT.LDIFF)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : DIFFUSION is implied'//
     -           ' by MC-DRIFT; diffusion included.'
       ENDIF
       IF(LIPULS.AND..NOT.LCROSS)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : ION-PULSE makes only'//
     -           ' sense with cross induced signals ;'
            PRINT *,'                         calculation of cross'//
     -           ' induced signals switched on.'
            LCROSS=.TRUE.
       ENDIF
*** Make sure we don't have LATTA on and the data absent.
       LATTA=LATTA.AND.GASOK(6)
*** Check the correct use of ADD and NEW.
       IF(LSIGAD.AND.CHANGE)PRINT *,' !!!!!! SIGGEN WARNING : New'//
     -      ' signal cannot be added to old one since you changed'//
     -      ' a parameter.'
       IF((.NOT.LSIGAD).OR.CHANGE)THEN
            DO 30 I=1,MXSW
            DO 20 J=1,MXLIST
            SIGNAL(J,I,1)=0.0
            SIGNAL(J,I,2)=0.0
20          CONTINUE
30          CONTINUE
       ENDIF
*** Make sure at least some signal output is requested.
       IF(.NOT.(LEPULS.OR.LITAIL.OR.LDTAIL.OR.LRTAIL))THEN
            PRINT *,' !!!!!! SIGGEN WARNING : Neither electron pulses'//
     -           ' nor ion tails are to be included ; no simulation.'
            RETURN
       ENDIF
       IF(NSW.EQ.0)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : No sense wires has been'//
     -           ' selected ; no signals calculated.'
            RETURN
       ENDIF
*** Initialise the matrices, cell type and signal storage.
       IF(.NOT.SIGSET)THEN
            CALL SIGINI(IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! SIGGEN WARNING : Initialisation of'//
     -                ' signal calculation failed; no signals.'
                 RETURN
            ENDIF
       ENDIF
*** Print a header, if cluster printing has been enabled.
       IF(LCLPRT)THEN
            IF(POLAR)THEN
                 CALL CFMCTP(XT0,YT0,XT0P,YT0P,1)
                 CALL CFMCTP(XT1,YT1,XT1P,YT1P,1)
            ELSE
                 XT0P=XT0
                 YT0P=YT0
                 XT1P=XT1
                 YT1P=YT1
            ENDIF
            WRITE(LUNOUT,'(''  Signal simulation:''/
     -           ''  ==================''//
     -           ''  The track begins at ('',
     -           E15.8,2('','',E15.8),'')''/
     -           ''  and ends at         ('',
     -           E15.8,2('','',E15.8),'')''/)')
     -           XT0P,YT0P,ZT0,XT1P,YT1P,ZT1
            CALL CELPRC(LUNOUT,0)
       ENDIF
*** Open/reset the storage file.
       IF(CHANGE)THEN
            CALL SIGIST('RESET',0,DUMMY,DUMMY,0,0,0,0,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! SIGGEN WARNING : Unable to'//
     -                ' reset a signal storage file; no signals.'
                 RETURN
            ENDIF
       ENDIF
*** Start simulation by generating clusters, also add signals.
       CALL SIGCLS(LDIFF,LAVAL,LATTA,LTRACK,LTRMC,IFAIL)
       IF(IENANG.GT.0)THEN
            CALL ALGCLR(IENANG)
            IENANG=0
       ENDIF
       IF(IFAIL.EQ.1)THEN
            PRINT *,' !!!!!! SIGGEN WARNING : Cluster generation'//
     -           ' failed ; no signal calculation.'
            RETURN
       ENDIF
*** Reset the CHANGE flag.
       CHANGE=.FALSE.
*** Register the amount of CPU time used.
       CALL TIMLOG('Generating a signal:                    ')
       END
