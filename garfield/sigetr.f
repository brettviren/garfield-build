CDECK  ID>, SIGETR.
       SUBROUTINE SIGETR(LDIFF,LAVAL,LATTA,IFAIL)
*-----------------------------------------------------------------------
*   SIGETR - Computes ion tails and electron pulses.
*   (Last changed on  8/ 5/07.)
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
       REAL EXMAP,EYMAP,EZMAP,VMAP,EWXMAP,EWYMAP,EWZMAP,VWMAP,
     -      BXMAP,BYMAP,BZMAP,
     -      XMAP,YMAP,ZMAP,XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      VMMIN,VMMAX,EPSMAT,EPSSUR,XFMOFF,YFMOFF,ZFMOFF
       INTEGER MATMAP,NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS,
     -      NWMAP
       LOGICAL MAPFLG,LMAPPL,SETAX,SETAY,SETAZ,ELMDGN,LSFDER
       CHARACTER EWSTYP
       CHARACTER*10 MATSRC
       COMMON /FLDMAP/ VMAP(MXMAP,10),VWMAP(MXMAP,10,MXWMAP),
     -      EXMAP(MXMAP,10),EYMAP(MXMAP,10),EZMAP(MXMAP,10),
     -      EWXMAP(MXMAP,10,MXWMAP),EWYMAP(MXMAP,10,MXWMAP),
     -      EWZMAP(MXMAP,10,MXWMAP),
     -      BXMAP(MXMAP,10),BYMAP(MXMAP,10),BZMAP(MXMAP,10),
     -      XMAP(MXMAP,10),YMAP(MXMAP,10),ZMAP(MXMAP,10),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,VMMIN,VMMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      EPSMAT(MXEPS),EPSSUR(MXEPS),MATMAP(MXMAP),
     -      NMAP,NEPS,MAPORD,MAPTYP,IDRMAT,INDEWS(MXWMAP),NWMAP,
     -      MAPFLG(10+4*MXWMAP),ELMDGN(MXMAP),
     -      LMAPPL,SETAX,SETAY,SETAZ,LSFDER
       COMMON /FLDCHR/ EWSTYP(MXWMAP),MATSRC
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
       CHARACTER*10 VARLIS(MXVAR)
       DOUBLE PRECISION TIME(MXLIST),SIG(MXLIST),
     -      VDRIFT(3,MXLIST),XAUX1,XAUX2,YAUX1,YAUX2,STEP,
     -      WG6(6),TG6(6),TWNSUM,ATTSUM
       REAL ORIION(MXLIST),DELION(MXLIST),ANGION(MXORIA),
     -      PRSELE(MXLIST),PRSION(MXLIST),QAVAL,
     -      VAR(MXVAR),RES(1),
     -      SUM,SCALE,TPAIR,QPAIR,ANGLE,
     -      GASTWN,GASATT,
     -      XORIG(MXLIST),YORIG(MXLIST),ZORIG(MXLIST),
     -      XSTART,YSTART,ZSTART,
     -      EX,EY,EZ,ETOT,VOLT,BX,BY,BZ,BTOT,
     -      DRES,EXPMAX
       PARAMETER(EXPMAX=30.0)
       INTEGER MODVAR(MXVAR),MODRES(1),IWION(MXLIST),IW,ISW,JSW,ISOLID,
     -      I,J,IU,IA,IPAIR,IFAIL,IFAIL1,ILOC,ILOCRS,NSIG,NRES,NERR
       LOGICAL USE(MXVAR),LDIFF,LAVAL,LATTA,START,OK,OVERFL
       EXTERNAL GASTWN,GASATT,GASVEL
       SAVE TG6,WG6
*** Locations and weights for 6-point Gaussian integration.
       DATA (TG6(I),WG6(I),I=1,6) /
     -      -0.93246 95142 03152 028D0, 0.17132 44923 79170 345D0,
     -      -0.66120 93864 66264 514D0, 0.36076 15730 48138 608D0,
     -      -0.23861 91860 83196 909D0, 0.46791 39345 72691 047D0,
     -       0.23861 91860 83196 909D0, 0.46791 39345 72691 047D0,
     -       0.66120 93864 66264 514D0, 0.36076 15730 48138 608D0,
     -       0.93246 95142 03152 028D0, 0.17132 44923 79170 345D0/
*** Indentify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE SIGETR ///'
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   : Called'',
     -      '' for ion simple('',L1,'')/detailed('',L1,
     -      '')/return('',L1,'')''/
     -      26X,''electron('',L1,''), diff/aval/att: '',3L1)')
     -      LITAIL,LDTAIL,LRTAIL,LEPULS,LDIFF,LAVAL,LATTA
*** Assume that the routine will fail.
       IFAIL=1
*** Compute wire and sense wire number if appropriate.
       IF((ICLUST.GE.1.AND.ICLUST.LE.NWIRE).OR.
     -      (ICLUST.GT.2*MXWIRE.AND.ICLUST.LE.2*MXWIRE+MXSOLI).OR.
     -      (ICLUST.LE.-11.AND.ICLUST.GE.-15))THEN
            IW=ICLUST
       ELSE
            IW=0
       ENDIF
       CALL DLCISW(ICLUST,ISW)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -      '' Cluster status code '',I4,'' (electrode '',
     -      I4,'')''/26X,''has '',I4,'' steps, total drift time '',
     -      E12.5,'' microsec.'')') ICLUST,ISW,NU,TCLUST
*** Verify that appropriate gas data is available.
       OK=.TRUE.
       IF(LEPULS.AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! SIGETR WARNING : No Townsend tables'//
     -           ' found; ELECTRON-PULSE cancelled.'
            LEPULS=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(LDTAIL.AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! SIGETR WARNING : No Townsend tables'//
     -           ' found; DETAILED-ION-TAIL cancelled.'
            LDTAIL=.FALSE.
            OK=.FALSE.
       ENDIF
*** Check that at least one of the flags is still on.
       IF(LITAIL.AND.LDTAIL)THEN
            PRINT *,' !!!!!! SIGETR WARNING : Both normal and'//
     -           ' detailed ion tail requested; detailed kept.'
            LITAIL=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(LITAIL.AND.LRTAIL)THEN
            PRINT *,' !!!!!! SIGETR WARNING : Both normal and'//
     -           ' nonsampled ion tail requested; nonsampled kept.'
            LITAIL=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(LDTAIL.AND.LRTAIL)THEN
            PRINT *,' !!!!!! SIGETR WARNING : Both detailed and'//
     -           ' nonsampled ion tail requested; detailed kept.'
            LRTAIL=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(.NOT.(LEPULS.OR.LDTAIL.OR.LITAIL.OR.LRTAIL))THEN
            PRINT *,' !!!!!! SIGETR WARNING : Neither ion'//
     -           ' tail, nor electron pulse remaining; no signal.'
            IFAIL=1
            RETURN
       ENDIF
*** See whether we should proceed.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### SIGETR ERROR   : Instruction is not'//
     -           ' carried out because of the above errors.'
            IFAIL=1
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### SIGETR ERROR   : Program terminated'//
     -           ' because of the above errors.'
            IFAIL=1
            CALL QUIT
       ENDIF
**** Don't proceed if the drift line has no steps or has zero length.
       IF((LEPULS.OR.LDTAIL).AND.
     -      (NU.LE.1.OR.TU(MAX(1,NU)).LE.0))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -           '' Drift line not processed: NU='',I4,'' TU(NU)='',
     -           E12.5,'' microsec.'')') NU,TU(MAX(1,NU))
            IFAIL=0
            RETURN
       ENDIF
*** Don't proceed if this is not a sense wire and CROSS-INDUCED is off.
       IF((IW.EQ.0.AND..NOT.(LCROSS.AND.(LEPULS.OR.LDTAIL))).OR.
     -      (IW.NE.0.AND.ISW.EQ.0.AND.(.NOT.LCROSS)))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -           '' Drift line not processed: LCROSS='',L1,'', ISTAT='',
     -           I4,'', ISW='',I4,''.'')') LCROSS,ICLUST,ISW
            IFAIL=0
            RETURN
       ENDIF
*** See whether the angular spread function has been translated.
       IF(LITAIL.AND.IENANG.LE.0.AND.NCANG.GT.0)THEN
*   Translate the function.
            VARLIS(1)='PHI'
            CALL ALGPRE(FCNANG(1:NCANG),NCANG,VARLIS,1,
     -           NRES,USE,IENANG,IFAIL1)
*   Check return code.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                ' translate the angular spread function;'//
     -                ' ion origin not smeared.'
                 CALL ALGCLR(IENANG)
                 IENANG=0
*   Ensure that the function gives only 1 result.
            ELSEIF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! SIGETR WARNING : The angular'//
     -                ' spread function does not give 1 result;'//
     -                ' ion origin not smeared.'
                 CALL ALGCLR(IENANG)
                 IENANG=0
            ENDIF
       ENDIF
*** Determine the angle of approach for this particular electron.
       IF(LITAIL)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -           '' Incidence angle is '',F10.3,'' degrees.'')')
     -           FCLUST*180/PI
**  Smear the angular distribution, if requested.
            IF(IENANG.NE.0)THEN
*   Mode of the angle is 2 (number).
                 MODVAR(1)=2
*   Initialise the number of arithmetic, mode and value errors to 0.
                 NERR=0
*   Initialise the sum of the bins.
                 SUM=0
*   Loop over the bins.
                 DO 330 I=1,NORIA
*   Newton-Raphson integration over the bin.
                 DO 340 J=-NASIMP,+NASIMP
                 IF(NASIMP.GT.0)THEN
                      VAR(1)=2*PI*(REAL(I)+REAL(J)/REAL(2*NASIMP))/
     -                     REAL(NORIA)-FCLUST
                 ELSE
                      VAR(1)=2*PI*REAL(I)/REAL(NORIA)-FCLUST
                 ENDIF
                 IF(VAR(1).GT.+PI)VAR(1)=VAR(1)-2*PI
                 IF(VAR(1).LT.-PI)VAR(1)=VAR(1)+2*PI
                 CALL ALGEXE(IENANG,VAR,MODVAR,1,RES,MODRES,1,IFAIL1)
                 IF(IFAIL1.NE.0.OR.MODRES(1).NE.2.OR.RES(1).LT.0)
     -                NERR=NERR+1
                 IF(J.EQ.-NASIMP)THEN
                      ANGION(I)=RES(1)
                 ELSEIF(J.EQ.+NASIMP)THEN
                      ANGION(I)=ANGION(I)+RES(1)
                 ELSEIF(J+NASIMP.EQ.2*((J+NASIMP)/2))THEN
                      ANGION(I)=ANGION(I)+2*RES(1)
                 ELSE
                      ANGION(I)=ANGION(I)+4*RES(1)
                 ENDIF
340              CONTINUE
*   Keep track of the integral.
                 SUM=SUM+ANGION(I)
330              CONTINUE
*   Check the error count.
                 CALL ALGERR
                 IF(NERR.GT.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Value, type'//
     -                     ' or arithmetic errors; no smearing.'
                      IENANG=0
                 ENDIF
            ENDIF
**  If no smearing has been requested, then simply put all in one bin.
            IF(IENANG.EQ.0)THEN
*   Set the whole distribution to zero.
                 DO 350 I=1,NORIA
                 ANGION(I)=0
350              CONTINUE
*   Transform the angle into an angular bin.
                 IA=NINT(NORIA*MOD(FCLUST-2*PI*ANINT(FCLUST/(2*PI))+
     -                2*PI,2*PI)/(2*PI))
                 IF(IA.EQ.0)IA=NORIA
*   And set just this element to non-zero.
                 ANGION(IA)=1
                 SUM=1
            ENDIF
**  Normalise the distribution.
            DO 360 I=1,NORIA
            ANGION(I)=ANGION(I)/SUM
360         CONTINUE
**  Output the distribution if debugging has been requested.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -           '' Angular distribution in %: ''/(2X,20I4/))')
     -           (NINT(ANGION(I)*100),I=1,NORIA)
       ENDIF
*** Follow the electron drift line for electron and detailed ion.
       IF(LEPULS.OR.LDTAIL)THEN
*   Initialisation for the loop.
            START=.FALSE.
            OVERFL=.FALSE.
            QAVAL=0
*   We'll have to cheat in case the point is located inside a wire.
            IF(ISTAT.GT.0)THEN
                 ILOCRS=MOD(ISTAT,MXWIRE)
                 DRES=D(ILOCRS)
            ELSE
                 ILOCRS=0
            ENDIF
**  Loop over the drift line.
            DO 100 IU=1,NU
*   Various initialisations.
            VDRIFT(1,IU)=0
            VDRIFT(2,IU)=0
            VDRIFT(3,IU)=0
            ORIION(IU)=0
            DELION(IU)=0
            XORIG(IU)=0
            YORIG(IU)=0
            ZORIG(IU)=0
            IWION(IU)=0
*   Set the wire diameter smaller.
            IF(ILOCRS.GT.0)D(ILOCRS)=DRES/2
**  Take care of charge integration, initialise the sum.
            TWNSUM=0
            ATTSUM=0
            IF(IU.GT.1)THEN
                 DO 10 J=1,6
*   Compute the fields.
                 CALL EFIELD(
     -                REAL(XU(IU-1)+(1+TG6(J))/2*(XU(IU)-XU(IU-1))),
     -                REAL(YU(IU-1)+(1+TG6(J))/2*(YU(IU)-YU(IU-1))),
     -                REAL(ZU(IU-1)+(1+TG6(J))/2*(ZU(IU)-ZU(IU-1))),
     -                EX,EY,EZ,ETOT,VOLT,0,ILOC)
                 CALL BFIELD(
     -                REAL(XU(IU-1)+(1+TG6(J))/2*(XU(IU)-XU(IU-1))),
     -                REAL(YU(IU-1)+(1+TG6(J))/2*(YU(IU)-YU(IU-1))),
     -                REAL(ZU(IU-1)+(1+TG6(J))/2*(ZU(IU)-ZU(IU-1))),
     -                BX,BY,BZ,BTOT)
*   Get Townsend and attachment coefficients and keep integrating.
                 IF(POLAR)THEN
                      IF(GASOK(4))TWNSUM=TWNSUM+WG6(J)*
     -                     GASTWN(EX/EXP(REAL(XU(IU))),
     -                     EY/EXP(REAL(XU(IU))),EZ,BX,BY,BZ)
                      IF(GASOK(6))ATTSUM=ATTSUM+WG6(J)*
     -                     GASATT(EX/EXP(REAL(XU(IU))),
     -                     EY/EXP(REAL(XU(IU))),EZ,BX,BY,BZ)
                 ELSE
                      IF(GASOK(4))TWNSUM=TWNSUM+WG6(J)*
     -                     GASTWN(EX,EY,EZ,BX,BY,BZ)
                      IF(GASOK(6))ATTSUM=ATTSUM+WG6(J)*
     -                     GASATT(EX,EY,EZ,BX,BY,BZ)
                 ENDIF
10               CONTINUE
            ENDIF
*   Compute the step length.
            IF(POLAR)THEN
                 IF(IU.GT.1)THEN
                      CALL CF2RTC(XU(IU),YU(IU),XAUX1,YAUX1,1)
                      CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX2,YAUX2,1)
                      STEP=SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                     (ZU(IU)-ZU(IU-1))**2)
                 ENDIF
            ELSE
                 IF(IU.GT.1)STEP=SQRT((XU(IU)-XU(IU-1))**2+
     -                (YU(IU)-YU(IU-1))**2+(ZU(IU)-ZU(IU-1))**2)
            ENDIF
*   Restore the wire diameter.
            IF(ILOCRS.GT.0)D(ILOCRS)=DRES
*   Record where the avalanche starts.
            IF(TWNSUM.GT.1E-6.AND..NOT.START)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -                '' Avalanche starts at step '',I4)') IU
                 START=.TRUE.
            ENDIF
*   Update the numbers of electrons and ions.
            IF(IU.GT.1)THEN
                 IF(LOG(MAX(1.0,PRSELE(IU-1)))+
     -                STEP*(TWNSUM-ATTSUM)/2.LE.EXPMAX)THEN
                      PRSELE(IU)=PRSELE(IU-1)*
     -                     EXP(STEP*(TWNSUM-ATTSUM)/2)
                 ELSE
                      OVERFL=.TRUE.
                      PRSELE(IU)=EXP(EXPMAX)
                 ENDIF
                 IF(LOG(MAX(1.0,PRSELE(IU-1)))+
     -                STEP*TWNSUM/2.LE.EXPMAX)THEN
                      PRSION(IU)=
     -                     PRSELE(IU-1)*(EXP(STEP*TWNSUM/2)-1)
                 ELSE
                      OVERFL=.TRUE.
                      PRSION(IU)=EXP(EXPMAX)
                 ENDIF
            ELSE
                 PRSELE(IU)=1
                 PRSION(IU)=0
            ENDIF
            QAVAL=QAVAL+PRSION(IU)
**  Next compute and store the local electron drift velocity.
            IF(LEPULS)THEN
                 IF(ILOCRS.GT.0)D(ILOCRS)=DRES/2
                 CALL DLCVEL(XU(IU),YU(IU),ZU(IU),VDRIFT(1,IU),
     -                -1.0,1,ILOC)
                 IF(ILOCRS.GT.0)D(ILOCRS)=DRES
            ENDIF
**  Compute the 'would have been' origin of the ions.
            IF(LDTAIL)THEN
*   In case we don't find any, we need the true starting point.
                 XORIG(IU)=XU(IU)
                 YORIG(IU)=YU(IU)
                 ZORIG(IU)=ZU(IU)
                 ORIION(IU)=0
*   Save electron drift line.
                 CALL DLCBCK('SAVE')
*   Compute the origin of the ion drift line.
                 IF(IU.EQ.1.OR..NOT.START)THEN
                      NU=1
                      TU(NU)=0
                      ISTAT=0
                 ELSE
                      CALL DLCALC(REAL(0.5*(XU(IU)+XU(IU-1))),
     -                     REAL(0.5*(YU(IU)+YU(IU-1))),
     -                     REAL(0.5*(ZU(IU)+ZU(IU-1))),-1.0,2)
                 ENDIF
*   Store the data.
                 IF(ISTAT.GE.1.AND.ISTAT.LE.NWIRE)THEN
                      ORIION(IU)=ATAN2(REAL(YU(NU))-Y(ISTAT),
     -                     REAL(XU(NU)-X(ISTAT)))
                      DELION(IU)=TU(NU)
                      IWION(IU)=ISTAT
                      XORIG(IU)=0
                      YORIG(IU)=0
                      ZORIG(IU)=0
                 ELSEIF((ISTAT.GT.2*MXWIRE.AND.
     -                ISTAT.LE.2*MXWIRE+MXSOLI).OR.
     -                (ISTAT.LE.-11.AND.ISTAT.GE.-15))THEN
                      ORIION(IU)=0
                      DELION(IU)=0
                      IWION(IU)=ISTAT
                 ELSEIF(ISTAT.NE.-3.AND.ISTAT.NE.0.AND.NU.GT.1)THEN
                      ORIION(IU)=0
                      DELION(IU)=0
                      IWION(IU)=-1
                 ELSE
                      IF(LDEBUG)WRITE(LUNOUT,
     -                     '(''  ++++++ SIGETR DEBUG   : Ion line for'',
     -                     '' IU='',I4,'' skipped, ISTAT='',I4)')
     -                     IU,ISTAT
                      ORIION(IU)=0
                      DELION(IU)=0
                      IWION(IU)=0
                      XORIG(IU)=0
                      YORIG(IU)=0
                      ZORIG(IU)=0
                 ENDIF
*   Restore the drift line.
                 CALL DLCBCK('RESTORE')
            ENDIF
100         CONTINUE
       ENDIF
**  Warn if there was an exponential overflow.
       IF(OVERFL)PRINT *,' !!!!!! SIGETR WARNING : Integrating the'//
     -      ' Townsend coefficients would lead to exponential'//
     -      ' overflow; avalanche truncated.'
*** Loop over the clusters.
       DO 200 IPAIR=1,NPAIR
*   Generate electron arrival time and multiplication.
       CALL SIGCRN(LDIFF,LAVAL,LATTA,TPAIR,QPAIR)
*   Print pair data if requested.
       IF(LCLPRT)WRITE(LUNOUT,'(79X,I5,2(1X,E12.5))') IPAIR,TPAIR,QPAIR
*** Compute simple ion currents in wires from the relevant angles.
       IF(LITAIL.AND.IW.GE.1.AND.IW.LE.NWIRE.AND.QPAIR.GT.1.0001)THEN
            DO 480 I=1,NORIA
            ANGLE=2*PI*REAL(I)/REAL(NORIA)
*   Skip bins with very small contributions.
            IF(ANGION(I)*NORIA.LT.1E-3)GOTO 480
**  Cross-induced signals requested, loop over all sense wires.
            IF(LCROSS)THEN
                 DO 490 JSW=1,NSW
*   Get the ion tail.
                 CALL SIGION(JSW,IW,ANGLE,NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                     ' obtain an ion tail ; tail not added.'
                 ELSE
                      CALL SIGADD(JSW,ISW.NE.JSW,NSIG,TIME,SIG,
     -                     ANGION(I)*(QPAIR-1),0.0,TPAIR,IFAIL1)
                 ENDIF
490              CONTINUE
**  For only direct signals, don't do the loop.
            ELSEIF(ISW.NE.0)THEN
*   Get the ion tail.
                 CALL SIGION(ISW,IW,ANGLE,NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                     ' obtain an ion tail ; tail not added.'
                 ELSE
                      CALL SIGADD(ISW,.FALSE.,NSIG,TIME,SIG,
     -                     ANGION(I)*(QPAIR-1),0.0,TPAIR,IFAIL1)
                 ENDIF
            ENDIF
**  Next orientation and pair.
480         CONTINUE
*** Non-sampled ion currents in wires from the relevant angles.
       ELSEIF(LRTAIL.AND.IW.GE.1.AND.IW.LE.NWIRE
     -      .AND.QPAIR.GT.1.0001)THEN
*   Starting point.
            XSTART=REAL(XU(NU))
            YSTART=REAL(YU(NU))
            ZSTART=REAL(ZU(NU))
**  Cross-induced signals requested, loop over all sense wires.
            IF(LCROSS)THEN
                 DO 530 JSW=1,NSW
*   Get the ion tail.
                 CALL SIGIOR(JSW,XSTART,YSTART,ZSTART,
     -                NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                     ' compute an ion tail ; tail not added.'
                 ELSE
                      CALL SIGADD(JSW,ISW.NE.JSW,NSIG,TIME,SIG,
     -                     QPAIR-1,0.0,TPAIR,IFAIL1)
                 ENDIF
530              CONTINUE
**  For only direct signals, don't do the loop.
            ELSEIF(ISW.NE.0)THEN
*   Get the ion tail.
                 CALL SIGIOR(ISW,XSTART,YSTART,ZSTART,
     -                NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                     ' compute an ion tail ; tail not added.'
                 ELSE
                      CALL SIGADD(ISW,.FALSE.,NSIG,TIME,SIG,
     -                     QPAIR-1,0.0,TPAIR,IFAIL1)
                 ENDIF
            ENDIF
*** Compute simple ion currents in solids from the relevant angles.
       ELSEIF(LITAIL.AND.IW.GE.2*MXWIRE+1.AND.
     -      IW.LE.2*MXWIRE+NSOLID.AND.QPAIR.GT.1.0001)THEN
*   Find out which solid.
            ISOLID=IW-2*MXWIRE
**  If a cylinder, take angular spread into account.
            IF(ISOLTP(ISOLID).EQ.1)THEN
                 DO 500 I=1,NORIA
                 ANGLE=2*PI*REAL(I)/REAL(NORIA)
*   Skip bins with very small contributions.
                 IF(ANGION(I)*NORIA.LT.1E-3)GOTO 500
*   Compute origin for this angle.
                 CALL SIGANG(ISOLID,ANGLE,XU(NU),YU(NU),ZU(NU),
     -                XSTART,YSTART,ZSTART)
*   Cross-induced signals requested, loop over all sense wires.
                 IF(LCROSS)THEN
                      DO 510 JSW=1,NSW
*   Get the ion tail.
                      CALL SIGIOR(JSW,XSTART,YSTART,ZSTART,
     -                     NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! SIGETR WARNING : Unable'//
     -                          ' to obtain an ion tail ; tail not'//
     -                          ' added.'
                      ELSE
                           CALL SIGADD(JSW,ISW.NE.JSW,NSIG,TIME,SIG,
     -                          ANGION(I)*(QPAIR-1),0.0,TPAIR,IFAIL1)
                      ENDIF
510                   CONTINUE
*   For only direct signals, don't do the loop.
                 ELSEIF(ISW.NE.0)THEN
*   Get the ion tail.
                      CALL SIGIOR(ISW,XSTART,YSTART,ZSTART,
     -                     NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! SIGETR WARNING : Unable'//
     -                          ' to obtain an ion tail ; tail not'//
     -                          ' added.'
                      ELSE
                           CALL SIGADD(ISW,.FALSE.,NSIG,TIME,SIG,
     -                          ANGION(I)*(QPAIR-1),0.0,TPAIR,IFAIL1)
                      ENDIF
                 ENDIF
500              CONTINUE
**  For other solids, drift an ion backwards.
            ELSE
*   Establish end point.
                 XSTART=REAL(XU(NU))
                 YSTART=REAL(YU(NU))
                 ZSTART=REAL(ZU(NU))
*   Cross-induced signals requested, loop over all sense wires.
                 IF(LCROSS)THEN
                      DO 520 JSW=1,NSW
*   Get the ion tail.
                      CALL SIGIOR(JSW,XSTART,YSTART,ZSTART,
     -                     NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! SIGETR WARNING : Unable'//
     -                          ' to obtain an ion tail ; tail not'//
     -                          ' added.'
                      ELSE
                           CALL SIGADD(JSW,ISW.NE.JSW,NSIG,TIME,SIG,
     -                          QPAIR-1,0.0,TPAIR,IFAIL1)
                      ENDIF
520                   CONTINUE
*   For only direct signals, don't do the loop.
                 ELSEIF(ISW.NE.0)THEN
*   Get the ion tail.
                      CALL SIGIOR(ISW,XSTART,YSTART,ZSTART,
     -                     NSIG,TIME,SIG,IFAIL1)
*   And add if it the tail is available.
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! SIGETR WARNING : Unable'//
     -                          ' to obtain an ion tail ; tail not'//
     -                          ' added.'
                      ELSE
                           CALL SIGADD(ISW,.FALSE.,NSIG,TIME,SIG,
     -                          QPAIR-1,0.0,TPAIR,IFAIL1)
                      ENDIF
                 ENDIF
            ENDIF
       ENDIF
*** Compute scaling for the number of pairs.
       IF(LEPULS.OR.LDTAIL)THEN
            IF(IW.EQ.0)THEN
                 SCALE=1
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -                '' Avalanche scaling = 1, no electrode hit.'')')
            ELSEIF(NU.LE.0)THEN
                 SCALE=1
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -                '' Avalanche scaling = 1, no steps.'')')
            ELSEIF(QAVAL.LT.0)THEN
                 SCALE=1
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -                '' Avalanche scaling = 1, no electrons left.'')')
            ELSEIF(QPAIR.LT.0)THEN
                 PRINT *,' !!!!!! SIGETR WARNING : Multiplication'//
     -                ' factor is less than 1; not scaled.'
                 SCALE=1
            ELSE
                 SCALE=(QPAIR+1)/(QAVAL+1)
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   :'',
     -                '' Avalanche scaling = '',E12.5)') SCALE
            ENDIF
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SIGETR DEBUG   : '',
     -               ''Final e- by alpha/eta integration: '',E12.5/
     -           26X,''Ions from alpha/eta integration:   '',E12.5/
     -           26X,''Requested ion+ charge:             '',E12.5/
     -           26X,''Charge scaling factor:             '',E12.5/
     -           26X,''Computed drift time:               '',E12.5/
     -           26X,''Requested drift time:              '',E12.5/
     -           26X,''Time scaling factor:               '',E12.5/
     -           2X,''Avalanche development: ''/
     -           ''     Time [microsec]    Electrons'',
     -           ''         Ions   Ion origin    Ion delay'',
     -           '' Status'')') PRSELE(NU),QAVAL,QPAIR,SCALE,
     -           TU(NU),TPAIR,TPAIR/TU(NU)
*   Loop over the electron drift line.
            DO 210 IU=1,NU
*   Scale a timing table.
            TIME(IU)=TU(IU)*TPAIR/TU(NU)
*   Debugging output.
            IF(LDEBUG)WRITE(LUNOUT,'(2X,I5,5(1X,E12.5),1X,I4)')
     -           IU,TIME(IU),SCALE*PRSELE(IU),SCALE*PRSION(IU),
     -           ORIION(IU),DELION(IU),IWION(IU)
210         CONTINUE
       ENDIF
*** Add the electron current.
       IF(LEPULS)THEN
**  Cross induction: loop over all sense wires.
            IF(LCROSS)THEN
*   Loop over the sense wires.
                 DO 240 JSW=1,NSW
*   Compute contribution of the current drift line to the signal
                 DO 250 IU=1,NU
                 CALL SIGFLS(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -                EX,EY,EZ,JSW)
                 SIG(IU)=SCALE*PRSELE(IU)*(
     -                VDRIFT(1,IU)*EX+
     -                VDRIFT(2,IU)*EY+
     -                VDRIFT(3,IU)*EZ)
250              CONTINUE
*   Add this current to the total.
                 CALL SIGADD(JSW,ISW.NE.JSW,NU,TIME,SIG,
     -                -1.0,0.0,0.0,IFAIL1)
*   Finish loop over the sense wires,
240              CONTINUE
**  Otherwise do not do the loop.
            ELSEIF(ISW.NE.0)THEN
*   Compute contribution of the current drift line to the signal
                 DO 220 IU=1,NU
                 CALL SIGFLS(REAL(XU(IU)),REAL(YU(IU)),REAL(ZU(IU)),
     -                EX,EY,EZ,ISW)
                 SIG(IU)=SCALE*PRSELE(IU)*(
     -                VDRIFT(1,IU)*EX+
     -                VDRIFT(2,IU)*EY+
     -                VDRIFT(3,IU)*EZ)
220              CONTINUE
*   Add this current to the total.
                 CALL SIGADD(ISW,.FALSE.,NU,TIME,SIG,
     -                -1.0,0.0,0.0,IFAIL1)
            ENDIF
       ENDIF
*** Add the ion currents.
       IF(LDTAIL)THEN
*   Loop over the electron track.
            DO 260 IU=2,NU
*   Skip points where there are no ions yet.
            IF(SCALE*PRSION(IU).LT.1.0)GOTO 260
*   Skip also points with a negligible contribution.
            IF(PRSION(IU).LT.PRSTHR*QAVAL)GOTO 260
*   Skip points where the ions don't come from an electrode.
            IF(IWION(IU).EQ.0)GOTO 260
**  Cross-induced signals requested, loop over all sense wires.
            IF(LCROSS)THEN
                 DO 270 JSW=1,NSW
*   Get the ion tail.
                 IF(IWION(IU).GE.1.AND.IWION(IU).LE.NWIRE)THEN
                      CALL SIGION(JSW,IWION(IU),ORIION(IU),
     -                     NSIG,TIME,SIG,IFAIL1)
                 ELSE
                      CALL SIGIOR(JSW,XORIG(IU),YORIG(IU),ZORIG(IU),
     -                     NSIG,TIME,SIG,IFAIL1)
                 ENDIF
*   And add if it the tail is available.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                     ' obtain an ion tail ; tail not added.'
                 ELSE
                      CALL SIGADD(JSW,ISW.NE.JSW,NSIG,TIME,SIG,
     -                     SCALE*PRSION(IU),DELION(IU),TPAIR-DELION(IU),
     -                     IFAIL1)
                 ENDIF
270              CONTINUE
**  For only direct signals, only process ions from the same source,
            ELSEIF(IWION(IU).EQ.IW)THEN
*   Get the ion tail.
                 IF(IWION(IU).GE.1.AND.IWION(IU).LE.NWIRE)THEN
                      CALL SIGION(ISW,IWION(IU),ORIION(IU),
     -                     NSIG,TIME,SIG,IFAIL1)
                 ELSE
                      CALL SIGIOR(ISW,XORIG(IU),YORIG(IU),ZORIG(IU),
     -                     NSIG,TIME,SIG,IFAIL1)
                 ENDIF
*   And add if it the tail is available.
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGETR WARNING : Unable to'//
     -                     ' obtain an ion tail ; tail not added.'
                 ELSE
                      CALL SIGADD(ISW,.FALSE.,NSIG,TIME,SIG,
     -                     SCALE*PRSION(IU),DELION(IU),TPAIR-DELION(IU),
     -                     IFAIL1)
                 ENDIF
            ENDIF
*   Next point on the electron track.
260         CONTINUE
       ENDIF
*** Next cluster.
200    CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
