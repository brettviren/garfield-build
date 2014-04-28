CDECK  ID>, INIT.
       SUBROUTINE INIT
*-----------------------------------------------------------------------
*   INIT   - Subroutine initialising most common blocks.
*   (Last changed on 13/ 3/14.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       CHARACTER*(MXINCH+1) STRING
       CHARACTER*(MXINCH)   ARGSTR
       CHARACTER*30         ERRCDE(MXWORD)
       CHARACTER*(MXCHAR)   WORD(MXWORD)
       CHARACTER*80         PROMPT,EOFSTR,SHELL
       CHARACTER            ESCAPE
       CHARACTER*(MXNAME)   FNINP,FNOUT
       INTEGER NCHAR(MXWORD),INDWRD(MXWORD),ICHSET,LUNSTR(5:MXLUN,3),
     -      NWORD,LUN,NCPROM,NCEOF,NCSH,NCARG,NCFNI,NCFNO
       LOGICAL ERRPRT(MXWORD),LPROM,DOEXEC,DOREAD,LINREC
       COMMON /INPCOM/ NCHAR,INDWRD,LUNSTR,NWORD,LUN,ICHSET,NCPROM,
     -      ERRPRT,LPROM,DOEXEC,DOREAD,NCEOF,LINREC,NCSH,NCARG,
     -      NCFNI,NCFNO
       COMMON /INPCHR/ ERRCDE,STRING,WORD,PROMPT,EOFSTR,ESCAPE,SHELL,
     -      ARGSTR,FNINP,FNOUT
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
*-----------------------------------------------------------------------
*   GMXDAT - Common block for gas mixing.
*   (Last changed on 20/ 2/97.)
*-----------------------------------------------------------------------
       REAL BREAK,FRAC,XLOSCH,EFLD,ESTEP,ECRIT
       INTEGER NBREAK
       COMMON /GMXDAT/ BREAK(MXLIST),FRAC(MXFRAC),XLOSCH,
     -      EFLD,ESTEP,ECRIT,NBREAK
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
       REAL XLIST(MXMCA),YLIST(MXMCA),ZLIST(MXMCA),TLIST(MXMCA),
     -      ELIST(MXMCA),
     -      XELIST(MXMCA),YELIST(MXMCA),ZELIST(MXMCA),TELIST(MXMCA)
       INTEGER NLIST(MXMCA),ISLIST(MXMCA),NMCA
       COMMON /MCAMAT/ XLIST,YLIST,ZLIST,TLIST,ELIST,
     -      XELIST,YELIST,ZELIST,TELIST,NLIST,ISLIST,NMCA
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
       CHARACTER*(MXCHAR) FUNFLD,FUNPOS,FUNWGT
       CHARACTER*10 VALTYP,PNTTYP
       REAL VST(MXWIRE),VPLST(5)
       LOGICAL EVALT,EVALD,EVALA
       INTEGER NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,IENFLD,IENPOS,IENWGT
       COMMON /OPTDAT/ VST,VPLST,NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,
     -      IENFLD,IENPOS,IENWGT,EVALT,EVALD,EVALA
       COMMON /OPTCHR/ FUNFLD,FUNPOS,FUNWGT,VALTYP,PNTTYP
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
       LOGICAL XDONE(0:MXGRID,0:MXGRID),YDONE(0:MXGRID,0:MXGRID),
     -      TRANS,CLAB
       REAL GRID(0:MXGRID,0:MXGRID),EPSTRA,EPSGRA,CXMIN,CXMAX,CYMIN,
     -      CYMAX,STINIT,DNTHR,DXGRA,DYGRA
       INTEGER ILOCGR(0:MXGRID,0:MXGRID),NBITER,NNITER,NFC,NGCMAX
       COMMON /CONDAT/ GRID,XDONE,YDONE,ILOCGR,
     -      NBITER,NNITER,EPSTRA,EPSGRA,DXGRA,DYGRA,
     -      STINIT,DNTHR,CXMIN,CXMAX,CYMIN,CYMAX,NFC,NGCMAX,TRANS,CLAB
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER DOREF,IFREF,LINREF,CURLIN,CDOLVL,CIFLVL,TRACDO,TRACIF,
     -      ISTATE,NDOLIN,NLOOP,NIF
       COMMON /DODAT/ LINREF(MXDLIN,8),DOREF(MXDLVL,12),IFREF(MXILVL,5),
     -      TRACDO(0:MXDLVL),TRACIF(0:MXILVL),CURLIN,CDOLVL,CIFLVL,
     -      NDOLIN,NLOOP,NIF,ISTATE
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
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
       EXTERNAL STDSTR,RNDM,RANFL
       LOGICAL STDSTR
       REAL DUMMY
       CHARACTER*8 DATE,TIME
       INTEGER IFAIL,J,IREF,IRNDM,JRNDM,KRNDM
       integer inpcmx,arg_length,iarg,nargs,inext
       character*128 args
       external inpcmx
*** Output unit.
       LUNOUT   =6
*** Write a record to the log file of the program.
       CALL JOBLOG('Version S7.45, C=11/3/14 ANSYS 15')
       CALL TIMED(DUMMY)
*** Initial data for the /PARMS/ common block.
       NLINED=20
       NINORD=2
       LINCAL=.TRUE.
       NGRIDX=25
       NGRIDY=25
       LEPSG=.FALSE.
       EPSGX=0
       EPSGY=0
       EPSGZ=0
       CALL PLAINT
*** Track initialisation.
       CALL TRAINT
*** Parameters for contour plotting in /CONDAT/.
       NBITER=10
       NNITER=10
       EPSTRA=1.0E-3
       EPSGRA=1.0E-3
       STINIT=0.174123
       DNTHR=0.1
       NGCMAX=500
*** Initial data for the /DRIFTL/ common block.
       MXDIFS   =MIN(2,MXSTCK)
       MXTWNS   =MIN(2,MXSTCK)
       MXATTS   =MIN(2,MXSTCK)
       LREPSK   =.TRUE.
       LKINK    =.TRUE.
       EPSDFI   =1.0E-4
       EPSTWI   =1.0E-4
       EPSATI   =1.0E-4
       RDF2     =5
       MDF2     =2
       TMC      =0.00002
       DMC      =0.001
       NMC      =100
       MCMETH   =0
       RTRAP    =2.0
       EPSDIF   =1.0E-8
       STMAX    =0.0
       LSTMAX   =.FALSE.
       IPTYPE   =0
       IPTECH   =0
       QPCHAR   =0.0
       NU       =0
       EQTTHR   =0.2
       EQTASP   =3
       EQTCLS   =0.2
       LEQSRT   =.TRUE.
       LEQCRS   =.TRUE.
       LEQMRK   =.FALSE.
       LAVPRO   =.FALSE.
       DSCMIN   =0.95
       DSCMAX   =1.05
       DTFACT   =1.0
*** Initial data for the /CELDAT/ common block.
       CALL CELINT
*   Memory allocation.
       CALL BOOK('INITIALISE','MATRIX',' ',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### INIT   ERROR   : Unable to declare the'//
     -           ' capacitance matrix; cell computations may fail.'
       ENDIF
*** Background field.
       IENBGF   =0
       LBGFMP   =.FALSE.
*** Initialise the field map.
       CALL MAPINT
*** Solids.
       NSOLID   =0
       ICCURR   =0
*** Initialise neBEM.
       CALL BEMINI(IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! INIT   WARNING : neBEM initialisation'//
     -           ' failed; neBEM calculations are likely to fail.'
       ENDIF
*** Initial data statements for the /PRTPLT/ common block.
       JFAIL    =1
       JEXMEM   =2
       LINPUT   =.NOT.STDSTR('INPUT')
       LCELPR   =.FALSE.
       LCELPL   =.FALSE.
       LWRMRK   =.FALSE.
       LISOCL   =.FALSE.
       LCHGCH   =.FALSE.
       LDRPLT   =.FALSE.
       LDRPRT   =.FALSE.
       LCLPRT   =.TRUE.
       LCLPLT   =.TRUE.
       LIDENT   =.FALSE.
       LDEBUG   =.FALSE.
       LRNDMI   =.TRUE.
       LPROPR   =STDSTR('INPUT')
       LPROF    =.TRUE.
       LMAPCH   =.FALSE.
       LCNTAM   =.TRUE.
       LINREC   =STDSTR('INPUT')
       LGSTOP   =.FALSE.
       LSYNCH   =.FALSE.
       LINPRD   = .FALSE.
*** GKS error logging file name.
       GKSLOG   ='GKS_error.log'
       NCGKS    =13
*** Input and output files
       FNINP    =' '
       NCFNI    =0
       FNOUT    =' '
       NCFNO    =0
*** Read the command line options, first preset the optional arguments.
       NCARG=1
       ARGSTR=' '
*   Count the number of arguments, pointer vector will not be used.
       nargs=iargc()
*   Loop over arguments, deleting those we recognise.
       inext=1
       do 30 iarg=1,nargs
       if(iarg.lt.inext)goto 30
       call argget(iarg,args,arg_length)
*   Debugging options.
       if(inpcmx(args(1:arg_length),'-deb#ug').ne.0)then
            ldebug=.true.
       elseif(inpcmx(args(1:arg_length),'-nodeb#ug').ne.0)then
            ldebug=.false.
*   Tracing options.
       elseif(inpcmx(args(1:arg_length),'-id#entification').ne.0)then
            lident=.true.
       elseif(inpcmx(args(1:arg_length),'-noid#entification').ne.0)then
            lident=.false.
*   Input listing.
       elseif(inpcmx(args(1:arg_length),'-in#put_listing').ne.0)then
            linput=.true.
       elseif(inpcmx(args(1:arg_length),'-noin#put_listing').ne.0)then
            linput=.false.
*   Random number initialisation.
       elseif(inpcmx(args(1:arg_length),
     -      '-RNDM#_initialisation').ne.0)then
            lrndmi=.true.
       elseif(inpcmx(args(1:arg_length),
     -      '-noRNDM#_initialisation').ne.0)then
            lrndmi=.false.
*   Progress printing.
       elseif(inpcmx(args(1:arg_length),'-pro#gress_print').ne.0)then
            lpropr=.true.
       elseif(inpcmx(args(1:arg_length),'-nopro#gress_print').ne.0)then
            lpropr=.false.
*   Input recording.
       elseif(inpcmx(args(1:arg_length),'-rec#ording').ne.0)then
            if(STDSTR('INPUT'))linrec=.true.
       elseif(inpcmx(args(1:arg_length),'-norec#ording').ne.0)then
            linrec=.false.
*   Reading of profile file.
       elseif(inpcmx(args(1:arg_length),'-pr#ofile').ne.0)then
            lprof=.true.
       elseif(inpcmx(args(1:arg_length),'-nopr#ofile').ne.0)then
            lprof=.false.
*   Synchronisation prompt.
       elseif(inpcmx(args(1:arg_length),'-synch#ronise').ne.0)then
            lsynch=.true.
       elseif(inpcmx(args(1:arg_length),'-nosynch#ronise').ne.0)then
            lsynch=.false.
*   GKS error logging file.
       elseif(inpcmx(args(1:arg_length),'-GKSlog').ne.0)then
            if(iarg+1.le.nargs)then
                 call argget(iarg+1,args,arg_length)
                 gkslog=args(1:arg_length)
                 ncgks=min(mxname,arg_length)
                 if(arg_length.gt.mxname)
     -                print *,' !!!!!! INIT   WARNING : Name of GKS'//
     -                ' error logging file too long; truncated.'
                 inext=iarg+2
            else
                 print *,' !!!!!! INIT   WARNING : File name missing'//
     -                ' following the -GKSlog option.'
            endif
*   Alternate input file to be opened on unit 5.
       elseif(inpcmx(args(1:arg_length),'-input_file')+
     -      inpcmx(args(1:arg_length),'-infile').ne.0)then
            if(iarg+1.le.nargs)then
                 call argget(iarg+1,args,arg_length)
                 fninp=args(1:arg_length)
                 ncfni=min(mxname,arg_length)
                 if(arg_length.gt.mxname)
     -                print *,' !!!!!! INIT   WARNING : Input file'//
     -                ' name too long; truncated.'
                 inext=iarg+2
            else
                 print *,' !!!!!! INIT   WARNING : File name missing'//
     -                ' following the -input_file option.'
            endif
*   Alternate output file to be opened on unit 6.
       elseif(inpcmx(args(1:arg_length),'-output#_file')+
     -      inpcmx(args(1:arg_length),'-outfile').ne.0)then
            if(iarg+1.le.nargs)then
                 call argget(iarg+1,args,arg_length)
                 fnout=args(1:arg_length)
                 ncfno=min(mxname,arg_length)
                 if(arg_length.gt.mxname)
     -                print *,' !!!!!! INIT   WARNING : Output file'//
     -                ' name too long; truncated.'
                 inext=iarg+2
            else
                 print *,' !!!!!! INIT   WARNING : File name missing'//
     -                ' following the -output_file option.'
            endif
*   Terminal and metafile type.
       elseif(inpcmx(args(1:arg_length),'-term#inal')+
     -      inpcmx(args(1:arg_length),'-meta#file')+
     -      inpcmx(args(1:arg_length),'-interact#ive')+
     -      inpcmx(args(1:arg_length),'-batch').ne.0)then
            do 50 j=iarg+1,nargs
            call argget(j,args,arg_length)
            if(args(1:1).eq.'-'.and.arg_length.gt.1)then
                 inext=j
                 goto 30
            endif
50          continue
            inext=nargs+1
*   Command line arguments.
       elseif(inpcmx(args(1:arg_length),'-arg#uments').ne.0)then
            ncarg=0
            do 60 j=iarg+1,nargs
            call argget(j,args,arg_length)
            if(inpcmx(args(1:arg_length),'-batch')+
     -           inpcmx(args(1:arg_length),'-interact#ive')+
     -           inpcmx(args(1:arg_length),'-GKSlog')+
     -           inpcmx(args(1:arg_length),'-deb#ug')+
     -           inpcmx(args(1:arg_length),'-nodeb#ug')+
     -           inpcmx(args(1:arg_length),'-id#entification')+
     -           inpcmx(args(1:arg_length),'-noid#entification')+
     -           inpcmx(args(1:arg_length),'-in#put_listing')+
     -           inpcmx(args(1:arg_length),'-noin#put_listing')+
     -           inpcmx(args(1:arg_length),'-input_file')+
     -           inpcmx(args(1:arg_length),'-infile')+
     -           inpcmx(args(1:arg_length),'-output_file')+
     -           inpcmx(args(1:arg_length),'-outfile')+
     -           inpcmx(args(1:arg_length),'-meta#file')+
     -           inpcmx(args(1:arg_length),'-nometa#file')+
     -           inpcmx(args(1:arg_length),'-pr#ofile')+
     -           inpcmx(args(1:arg_length),'-nopr#ofile')+
     -           inpcmx(args(1:arg_length),'-pro#gress_print')+
     -           inpcmx(args(1:arg_length),'-nopro#gress_print')+
     -           inpcmx(args(1:arg_length),'-rec#ording')+
     -           inpcmx(args(1:arg_length),'-norec#ording')+
     -           inpcmx(args(1:arg_length),
     -                '-RNDM#_initialisation')+
     -           inpcmx(args(1:arg_length),
     -                '-noRNDM#_initialisation')+
     -           inpcmx(args(1:arg_length),'-synch#ronise')+
     -           inpcmx(args(1:arg_length),'-nosynch#ronise')+
     -           inpcmx(args(1:arg_length),'-term#inal')+
     -           inpcmx(args(1:arg_length),'-noterm#inal').eq.0)then
                 if(ncarg+1.le.len(argstr))then
                      argstr(ncarg+1:)=args(1:arg_length)//' '
                      ncarg=min(len(argstr),ncarg+arg_length+1)
                 else
                      print *,' !!!!!! INIT   WARNING : Command'//
     -                     ' line arguments too long; truncated.'
                 endif
                 inext=j+1
            else
                 goto 70
            endif
60          continue
70          continue
            if(ncarg.gt.1)ncarg=ncarg-1
            if(ncarg.lt.1)then
                 argstr=' '
                 ncarg=1
            endif
*   Anything else is not valid.
       elseif(inpcmx(args(1:arg_length),'-noterm#inal')+
     -      inpcmx(args(1:arg_length),'-nometa#file').eq.0)then
            print *,' !!!!!! INIT   WARNING : Unrecognised option "'//
     -           args(1:arg_length)//'" found on the command line.'
       endif
30     continue
*** Global variable initialisation.
       GLBVAR(1)='TIME_LEFT '
       GLBMOD(1)=2
       CALL TIMEL(GLBVAL(1))
*   Type of compilation
       GLBVAR(2)='MACHINE   '
       IREF=-1
       CALL STRBUF('STORE',IREF,'Unix',4,IFAIL)
       IF(IREF.LT.0)
     -      CALL STRBUF('STORE',IREF,'< not known >',13,IFAIL)
       GLBMOD(2)=1
       GLBVAL(2)=IREF
*   Interactive or batch mode
       GLBVAR(3)='INTERACT  '
       GLBVAR(4)='BATCH     '
       GLBMOD(3)=3
       GLBMOD(4)=3
       IF(STDSTR('INPUT'))THEN
            GLBVAL(3)=1
            GLBVAL(4)=0
       ELSE
            GLBVAL(3)=0
            GLBVAL(4)=1
       ENDIF
*   Status for return of procedure calls
       GLBVAR(5)='OK        '
       GLBMOD(5)=3
       GLBVAL(5)=1
*   Output file.
       GLBVAR(7)='OUTPUT    '
       IF(FNOUT.EQ.' '.OR.NCFNO.LT.1)THEN
            CALL STRBUF('STORE',IREF,'Standard output',15,IFAIL)
       ELSE
            CLOSE(UNIT=6,ERR=2030)
            OPEN(UNIT=6,FILE=FNOUT(1:NCFNO),ERR=2020)
            CALL STRBUF('STORE',IREF,FNOUT(1:NCFNO),NCFNO,IFAIL)
       ENDIF
       GLBMOD(7)=1
       GLBVAL(7)=IREF
*   Parameter for plotting
       GLBVAR(8)='X         '
       GLBMOD(8)=2
       GLBVAL(8)=0
*   Plot frame number
       GLBVAR(9)='FRAME     '
       GLBMOD(9)=2
       GLBVAL(9)=0
*   Event number.
       GLBVAR(10)='EVENT     '
       GLBMOD(10)=2
       GLBVAL(10)=0
*   Read-out group number.
       GLBVAR(11)='GROUP     '
       GLBMOD(11)=2
       GLBVAL(11)=0
       NGLB=11
*** Plotting options for contours.
       LKEYPL   =.FALSE.
*** Initial data for the /MAGDAT/ common block.
       CALL MAGINT
*** Initial data for the /GASDAT/ common block.
       CALL GASINT
*** Initial data for the /SIGDAT/ common block.
       TSTART   =0.0
       TDEV     =0.01
       NTIME    =MXLIST
       RESSET   =.FALSE.
       PRSTHR   =0.0
       AVALAN(1)=100000.0
       AVALAN(2)=0.001
       AVATYP   ='NOT SET'
       NFOUR    =1
       LCROSS   =.TRUE.
       TRASET   =.FALSE.
       JIORD    =1
       NISIMP   =2
       NMQUAD   =1
       NASIMP   =2
       NORIA    =MIN(50,MXORIA)
       FCNANG   =' '
       NCANG    =0
       LITAIL   =.TRUE.
       LDTAIL   =.FALSE.
       LRTAIL   =.FALSE.
       LEPULS   =.FALSE.
       LIPULS   =.TRUE.
       SIGSET   =.FALSE.
*   Memory allocation.
       CALL BOOK('INITIALISE','MCAMAT',' ',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' ###### INIT   ERROR   : Unable to declare the'//
     -           ' avalanche buffer; avalanche calculations may fail.'
       ENDIF
       NMCA=0
*** Data for the /OPTDAT/ common block.
       NPOINT=20
       FUNFLD='V'
       NFLD=1
       FUNPOS='0'
       NPOS=1
       FUNWGT='1'
       NWGT=1
       VALTYP='AVERAGE'
       PNTTYP='GRID'
*** Random number initialisation.
       IF(LRNDMI)THEN
            CALL DATTIM(DATE,TIME)
            READ(TIME,'(I2,1X,I2,1X,I2)') IRNDM,JRNDM,KRNDM
            CALL RNDINI((1+IRNDM)*(1+JRNDM)*(1+KRNDM))
            IF(LDEBUG)PRINT *,' ++++++ INIT   DEBUG   : Number of'//
     -           ' generator initialisation calls: ',
     -           (1+IRNDM)*(1+JRNDM)*(1+KRNDM)
       ELSEIF(LDEBUG)THEN
            PRINT *,' ++++++ INIT   DEBUG   : No random initialisation.'
       ENDIF
*** DO loop initialisation.
       ISTATE=-2
*** Take care of algebra, graphics, histogram and matrix initialisation.
       CALL ALGINT
       CALL GRINIT
       CALL HISINT
       CALL MATINT
*** Command line reading routines initialisation.
       CALL INPINT
*** Output the dimensions for front-end programs.
       IF(LSYNCH)WRITE(6,'(''  >>>>>> set MX3D   '',I10/
     -      ''  >>>>>> set MXALGE '',I10/''  >>>>>> set MXARG  '',I10/
     -      ''  >>>>>> set MXBANG '',I10/''  >>>>>> set MXCHA  '',I10/
     -      ''  >>>>>> set MXCHAR '',I10/''  >>>>>> set MXCLUS '',I10/
     -      ''  >>>>>> set MXCONS '',I10/''  >>>>>> set MXDLIN '',I10/
     -      ''  >>>>>> set MXDLVL '',I10/''  >>>>>> set MXEDGE '',I10/
     -      ''  >>>>>> set MXEMAT '',I10/''  >>>>>> set MXEPS  '',I10/
     -      ''  >>>>>> set MXEPS  '',I10/''  >>>>>> set MXEQUT '',I10/
     -      ''  >>>>>> set MXFOUR '',I10/''  >>>>>> set MXFPAR '',I10/
     -      ''  >>>>>> set MXFPNT '',I10/''  >>>>>> set MXFRAC '',I10/
     -      ''  >>>>>> set MXGRID '',I10/''  >>>>>> set MXHIST '',I10/
     -      ''  >>>>>> set MXHLEV '',I10/''  >>>>>> set MXHLRL '',I10/
     -      ''  >>>>>> set MXILVL '',I10/''  >>>>>> set MXINCH '',I10/
     -      ''  >>>>>> set MXINS  '',I10/''  >>>>>> set MXLINE '',I10/
     -      ''  >>>>>> set MXLIST '',I10/''  >>>>>> set MXLIST '',I10/
     -      ''  >>>>>> set MXLUN  '',I10/''  >>>>>> set MXMAP  '',I10/
     -      ''  >>>>>> set MXMAP  '',I10/''  >>>>>> set MXMAT  '',I10/
     -      ''  >>>>>> set MXMATT '',I10/''  >>>>>> set MXMATT '',I10/
     -      ''  >>>>>> set MXMDIM '',I10/''  >>>>>> set MXNAME '',I10/
     -      ''  >>>>>> set MXORIA '',I10/''  >>>>>> set MXPAIR '',I10/
     -      ''  >>>>>> set MXPART '',I10/''  >>>>>> set MXPLAN '',I10/
     -      ''  >>>>>> set MXPOIN '',I10/''  >>>>>> set MXPOLE '',I10/
     -      ''  >>>>>> set MXRECL '',I10/''  >>>>>> set MXREG  '',I10/
     -      ''  >>>>>> set MXSBUF '',I10/''  >>>>>> set MXSHOT '',I10/
     -      ''  >>>>>> set MXSOLI '',I10/''  >>>>>> set MXSTCK '',I10/
     -      ''  >>>>>> set MXSUBT '',I10/''  >>>>>> set MXSW   '',I10/
     -      ''  >>>>>> set MXSW   '',I10/''  >>>>>> set MXVAR  '',I10/
     -      ''  >>>>>> set MXWIRE '',I10/''  >>>>>> set MXWIRE '',I10/
     -      ''  >>>>>> set MXWKLS '',I10/''  >>>>>> set MXWORD '',I10/
     -      ''  >>>>>> set MXZERO '',I10/''  >>>>>> set MXZPAR '',I10)')
     -      MX3D  ,MXALGE,MXARG ,MXBANG,MXCHA ,MXCHAR,MXCLUS,MXCONS,
     -      MXDLIN,MXDLVL,MXEDGE,MXEMAT,MXEPS ,MXEPS ,MXEQUT,MXFOUR,
     -      MXFPAR,MXFPNT,MXFRAC,MXGRID,MXHIST,MXHLEV,MXHLRL,MXILVL,
     -      MXINCH,MXINS ,MXLINE,MXLIST,MXLIST,MXLUN ,MXMAP ,MXMAP ,
     -      MXMAT ,MXMATT,MXMATT,MXMDIM,MXNAME,MXORIA,MXPAIR,MXPART,
     -      MXPLAN,MXPOIN,MXPOLE,MXRECL,MXREG ,MXSBUF,MXSHOT,MXSOLI,
     -      MXSTCK,MXSUBT,MXSW  ,MXSW  ,MXVAR ,MXWIRE,MXWIRE,MXWKLS,
     -      MXWORD,MXZERO,MXZPAR
*** Record the CPU time usage for initialisation.
       CALL TIMLOG('Initialisation:')
*** I/O error processing.
       RETURN
2020   CONTINUE
       PRINT *,' ###### INIT   ERROR   : Error opening a file; quit.'
       CALL QUIT
       RETURN
2030   CONTINUE
       PRINT *,' ###### INIT   ERROR   : Error closing a file; quit.'
       CALL QUIT
       END
