CDECK  ID>, DRFDRF.
       SUBROUTINE DRFDRF
*-----------------------------------------------------------------------
*   DRFDRF - This routine makes drift line plots.
*   VARIABLES : TSTEPR     : Value of TSTEP as read from the input file.
*               START      : Sort of call, should be obvious.
*               LEQTPL     : Plot equal time contours or not.
*               LLINPL     : Plotting of the drift lines.
*               LLINPR     : Printing of the drift lines.
*               TSTEP      : Distance between equal time contours.
*               MARKER     : If .TRUE., markers (*) will be plotted
*                            instead of a solid line.
*   (Last changed on 12/ 4/08.)
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
       LOGICAL LLINPL,LLINPR,LEQTPL,LDIFPL,LTIMPL,LVELPL,LAVAPL,
     -      LFUNPL,MARKER,SIDE(4),FLAG(MXWORD+3),LDRMC,OK,
     -      LEQRTR,LEQRWI,LEQRED,LEQRSO,LEQRZE
       CHARACTER*5 START
       CHARACTER*(MXCHAR) FUNCT
       REAL TSTEP,TSTEPR,Q,ANGMIN,ANGMAX,AMINR,AMAXR
       INTEGER NLINEE,NLINEW,NLINEV,NLINER,NCF,ITYPE,I,J,NWORD,INEXT,
     -      INPCMP,INPTYP,IFAIL,IFAIL1,IFAIL2,ISOREV
       EXTERNAL INPCMP,INPTYP
       SAVE LLINPL,LLINPR,LEQTPL,LDIFPL,LTIMPL,LVELPL,LAVAPL,
     -      LFUNPL,SIDE,TSTEP,Q,START,ITYPE,FUNCT,LDRMC,
     -      NLINEE,NLINEW,NLINEV,
     -      LEQRTR,LEQRWI,LEQRED,LEQRSO,LEQRZE,ANGMIN,ANGMAX
*** Initialise the parameters with DATA statements.
       DATA LLINPL , LLINPR , LEQTPL , LDRMC
     -     /.TRUE. , .FALSE., .FALSE., .FALSE./
       DATA LTIMPL , LVELPL , LDIFPL , LAVAPL , LFUNPL
     -     /.FALSE., .FALSE., .FALSE., .FALSE., .FALSE./
       DATA (SIDE(I),I=1,4) /.TRUE.,.TRUE.,.FALSE.,.FALSE./
       DATA MARKER /.FALSE./
       DATA START/' '/
       DATA FUNCT/' '/
       DATA NCF/1/
       DATA ITYPE/1/
       DATA NLINEE/20/,NLINEW/20/,NLINEV/20/
       DATA Q,TSTEP/-1.0,0.5/
       DATA ANGMIN/0/,ANGMAX/6.2831853/
       DATA LEQRTR , LEQRWI , LEQRED , LEQRSO , LEQRZE
     -     /.TRUE. , .FALSE., .TRUE. , .FALSE., .TRUE./
*** Decode the argument sring.
       CALL INPNUM(NWORD)
*** First mark the keywords.
       DO 10 I=1,MXWORD+3
       IF(I.EQ.1.OR.I.GT.NWORD)THEN
            FLAG(I)=.TRUE.
            GOTO 10
       ENDIF
       FLAG(I)=.FALSE.
       IF(INPCMP(I,'ANG#LES-#RANGE')+
     -      INPCMP(I,'A#VALANCHE-GR#APH')+
     -      INPCMP(I,'CONT#OUR-#INTERVAL')+
     -      INPCMP(I,'D#IFFUSION-GR#APH')+ INPCMP(I,'D#OWN')+
     -      INPCMP(I,'EDG#ES')+            INPCMP(I,'EL#ECTRON')+
     -      INPCMP(I,'F#UNCTION-GR#APH')+  INPCMP(I,'ISO#CHRONES')+
     -      INPCMP(I,'ISO#CHRONS')+        INPCMP(I,'I#ON')+
     -      INPCMP(I,'L#EFT')+             INPCMP(I,'L#INE-PL#OT')+
     -      INPCMP(I,'L#INE-PR#INT')+      INPCMP(I,'MAR#KERS')+
     -      INPCMP(I,'M#ONTE-C#ARLO-#DRIFT')+INPCMP(I,'MC-#DRIFT')+
     -      INPCMP(I,'NEG#ATIVE')+
     -      INPCMP(I,'NOA#VALANCHE-GR#APH')+
     -      INPCMP(I,'NOCONT#OUR')+
     -      INPCMP(I,'NOD#IFFUSION-GR#APH')+
     -      INPCMP(I,'NOF#UNCTION-GR#APH')+INPCMP(I,'NOISO#CHRONES')+
     -      INPCMP(I,'NOISO#CHRONS')+      INPCMP(I,'NOL#INE-PL#OT')+
     -      INPCMP(I,'NOL#INE-PR#INT')+    INPCMP(I,'NOT#IME-GR#APH')+
     -      INPCMP(I,'NOM#ONTE-C#ARLO-#DRIFT')+
     -      INPCMP(I,'NOMC-#DRIFT')+
     -      INPCMP(I,'RUN#GE-K#UTTA-#DRIFT-#LINES')+
     -      INPCMP(I,'RKF-#DRIFT-#LINES')+
     -      INPCMP(I,'NOTD#OWN')+          INPCMP(I,'NOTL#EFT')+
     -      INPCMP(I,'NOTR#IGHT')+         INPCMP(I,'NOTU#P')+
     -      INPCMP(I,'NOV#ELOCITY-GR#APH')+INPCMP(I,'POS#ITIVE')+
     -      INPCMP(I,'REV#ERSE-#ISOCHRONES')+
     -      INPCMP(I,'REV#ERSE-#ISOCHRONS')+
     -      INPCMP(I,'R#IGHT')+            INPCMP(I,'SOL#ID')+
     -      INPCMP(I,'THR#ESHOLD')+        INPCMP(I,'LINE#S')+
     -      INPCMP(I,'T#IME-GR#APH')+      INPCMP(I,'TR#ACK')+
     -      INPCMP(I,'SOL#IDS')+
     -      INPCMP(I,'U#P')+               INPCMP(I,'V#ELOCITY-GR#APH')+
     -      INPCMP(I,'WIR#ES')+
     -      INPCMP(I,'ZER#OS').NE.0)FLAG(I)=.TRUE.
10     CONTINUE
*** Initial settings.
       ISOREV=0
*** Next figure out which options are effectively there.
       INEXT=2
       OK=.TRUE.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
**  Check whether drift-lines have to start from the edges.
       IF(INPCMP(I,'EDG#ES').NE.0)THEN
            START='EDGE'
            DO 30 J=I+1,NWORD
            IF(J.LT.INEXT)GOTO 30
*   Look for the subkeyword RIGHT,
            IF(INPCMP(J,'NOTR#IGHT')+
     -           INPCMP(J,'NOR#IGHT').NE.0)THEN
                 SIDE(1)=.FALSE.
            ELSEIF(INPCMP(J,'R#IGHT').NE.0)THEN
                 SIDE(1)=.TRUE.
*   Look for the subkeyword LEFT,
            ELSEIF(INPCMP(J,'NOTL#EFT')+
     -           INPCMP(J,'NOL#EFT').NE.0)THEN
                 SIDE(2)=.FALSE.
            ELSEIF(INPCMP(J,'L#EFT').NE.0)THEN
                 SIDE(2)=.TRUE.
*   Look for the subkeyword UP,
            ELSEIF(INPCMP(J,'NOTU#P')+
     -           INPCMP(J,'NOU#P').NE.0)THEN
                 SIDE(3)=.FALSE.
            ELSEIF(INPCMP(J,'U#P').NE.0)THEN
                 SIDE(3)=.TRUE.
*   Look for the subkeyword DOWN,
            ELSEIF(INPCMP(J,'NOTD#OWN')+
     -           INPCMP(J,'NOD#OWN').NE.0)THEN
                 SIDE(4)=.FALSE.
            ELSEIF(INPCMP(J,'D#OWN').NE.0)THEN
                 SIDE(4)=.TRUE.
*   Look for the grouped options.
            ELSEIF(INPCMP(J,'ALL').NE.0)THEN
                 SIDE(1)=.TRUE.
                 SIDE(2)=.TRUE.
                 SIDE(3)=.TRUE.
                 SIDE(4)=.TRUE.
            ELSEIF(INPCMP(J,'NONE').NE.0)THEN
                 SIDE(1)=.FALSE.
                 SIDE(2)=.FALSE.
                 SIDE(3)=.FALSE.
                 SIDE(4)=.FALSE.
            ELSEIF(INPCMP(J,'HOR#IZONTAL').NE.0)THEN
                 SIDE(1)=.TRUE.
                 SIDE(2)=.TRUE.
            ELSEIF(INPCMP(J,'NOHOR#IZONTAL')+
     -           INPCMP(J,'NOTHOR#IZONTAL').NE.0)THEN
                 SIDE(1)=.FALSE.
                 SIDE(2)=.FALSE.
            ELSEIF(INPCMP(J,'VERT#ICAL').NE.0)THEN
                 SIDE(3)=.TRUE.
                 SIDE(4)=.TRUE.
            ELSEIF(INPCMP(J,'NOVERT#ICAL')+
     -           INPCMP(J,'NOTVERT#ICAL').NE.0)THEN
                 SIDE(3)=.FALSE.
                 SIDE(4)=.FALSE.
*   Perhaps a number of lines.
            ELSEIF(INPCMP(J,'LINE#S').NE.0)THEN
                 IF(INPTYP(J+1).NE.1.OR.FLAG(J+1))THEN
                      CALL INPMSG(J,'Misses an integer argument')
                      OK=.FALSE.
                      INEXT=J+1
                 ELSE
                      CALL INPCHK(J+1,1,IFAIL1)
                      CALL INPRDI(J+1,NLINER,NLINEE)
                      IF(NLINER.GT.0)THEN
                           NLINEE=NLINER
                      ELSE
                           CALL INPMSG(J+1,'Should be at least 1')
                           OK=.FALSE.
                      ENDIF
                      INEXT=J+2
                 ENDIF
*   Not known in this context, skip this processing.
            ELSE
                 INEXT=J
                 GOTO 20
            ENDIF
*   Next subkeyword.
30          CONTINUE
            INEXT=NWORD+1
**  Check whether drift-lines have to start from the wire surfaces.
       ELSEIF(INPCMP(I,'WIR#ES').NE.0)THEN
            START='WIRE'
            DO 50 J=I+1,NWORD
            IF(J.LT.INEXT)GOTO 50
*   Perhaps a number of lines.
            IF(INPCMP(J,'LINE#S').NE.0)THEN
                 IF(INPTYP(J+1).NE.1.OR.FLAG(J+1))THEN
                      CALL INPMSG(J,'Misses an integer argument')
                      INEXT=J+1
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(J+1,1,IFAIL1)
                      CALL INPRDI(J+1,NLINER,NLINEW)
                      IF(NLINER.GT.0)THEN
                           NLINEW=NLINER
                      ELSE
                           CALL INPMSG(J+1,'Should be at least 1')
                           OK=.FALSE.
                      ENDIF
                      INEXT=J+2
                 ENDIF
            ELSEIF(INPCMP(J,'ANG#LES-#RANGE').NE.0)THEN
                 IF(FLAG(J+1).OR.FLAG(J+2))THEN
                      CALL INPMSG(J,'Takes 2 real arguments')
                      INEXT=J+1
                      OK=.FALSE.
                 ELSE
                      CALL INPCHK(J+1,2,IFAIL1)
                      CALL INPCHK(J+2,2,IFAIL2)
                      CALL INPRDR(J+1,AMINR,ANGMIN*180/PI)
                      CALL INPRDR(J+2,AMAXR,ANGMAX*180/PI)
                      IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.
     -                     AMINR.EQ.AMAXR)THEN
                           CALL INPMSG(J+1,'Zero range not permitted.')
                           CALL INPMSG(J+2,'See previous message.')
                           OK=.FALSE.
                      ELSEIF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                           ANGMIN=AMINR*PI/180
                           ANGMAX=AMAXR*PI/180
                      ENDIF
                      INEXT=J+3
                 ENDIF
*   Not known in this context, skip this processing.
            ELSE
                 INEXT=J
                 GOTO 20
            ENDIF
50          CONTINUE
**  Check whether drift-lines have to start from the wire surfaces.
       ELSEIF(INPCMP(I,'SOL#IDS').NE.0)THEN
            START='SOLID'
            DO 60 J=I+1,NWORD
            IF(J.LT.INEXT)GOTO 60
*   Perhaps a number of lines.
            IF(INPCMP(J,'LINE#S').NE.0)THEN
                 IF(INPTYP(J+1).NE.1.OR.FLAG(J+1))THEN
                      CALL INPMSG(J,'Misses an integer argument')
                      OK=.FALSE.
                      INEXT=J+1
                 ELSE
                      CALL INPCHK(J+1,1,IFAIL1)
                      CALL INPRDI(J+1,NLINER,NLINEV)
                      IF(NLINER.GT.0)THEN
                           NLINEV=NLINER
                      ELSE
                           CALL INPMSG(J+1,'Should be at least 1')
                           OK=.FALSE.
                      ENDIF
                      INEXT=J+2
                 ENDIF
*   Not known in this context, skip this processing.
            ELSE
                 INEXT=J
                 GOTO 20
            ENDIF
60          CONTINUE
**  Check whether drift-lines have to start from the track.
       ELSEIF(INPCMP(I,'TR#ACK').NE.0)THEN
            IF(TRFLAG(1))THEN
                 START='TRACK'
*   Look for the subkeywords.
                 DO 40 J=I+1,NWORD
                 IF(J.LT.INEXT)GOTO 40
*   Look for the line type of the graphs.
                 IF(INPCMP(J,'MARK#ERS').NE.0)THEN
                      MARKER=.TRUE.
                 ELSEIF(INPCMP(J,'SOL#ID').NE.0)THEN
                      MARKER=.FALSE.
*   Look for the drift-time plotting option.
                 ELSEIF(INPCMP(J,'T#IME-GR#APH').NE.0)THEN
                      IF(.NOT.GASOK(1))THEN
                           CALL INPMSG(J,
     -                          'Drift velocity data absent.   ')
                           OK=.FALSE.
                      ELSE
                           LTIMPL=.TRUE.
                      ENDIF
                 ELSEIF(INPCMP(J,'NOT#IME-GR#APH').NE.0)THEN
                      LTIMPL=.FALSE.
*   Look for the drift-velocity plotting option.
                 ELSEIF(INPCMP(J,'V#ELOCITY-GR#APH').NE.0)THEN
                      IF(.NOT.GASOK(1))THEN
                           CALL INPMSG(J,
     -                          'Drift velocity data absent.   ')
                           OK=.FALSE.
                      ELSE
                           LVELPL=.TRUE.
                      ENDIF
                 ELSEIF(INPCMP(J,'NOV#ELOCITY-GR#APH').NE.0)THEN
                      LVELPL=.FALSE.
*   Look for the diffusion plotting option.
                 ELSEIF(INPCMP(J,'D#IFFUSION-GR#APH').NE.0)THEN
                      IF(.NOT.GASOK(3))THEN
                           CALL INPMSG(J,
     -                          'The diffusion data are absent.')
                           OK=.FALSE.
                      ELSE
                           LDIFPL=.TRUE.
                      ENDIF
                 ELSEIF(INPCMP(J,'NOD#IFFUSION-GR#APH').NE.0)THEN
                      LDIFPL=.FALSE.
*   Look for the avalanche plotting option.
                 ELSEIF(INPCMP(J,'A#VALANCHE-GR#APH').NE.0)THEN
                      IF(.NOT.GASOK(4))THEN
                           CALL INPMSG(J,
     -                           'The avalanche data are absent.')
                           OK=.FALSE.
                      ELSE
                           LAVAPL=.TRUE.
                      ENDIF
                 ELSEIF(INPCMP(J,'NOA#VALANCHE-GR#APH').NE.0)THEN
                      LAVAPL=.FALSE.
*   Look for the function graph plotting option.
                 ELSEIF(INPCMP(J,'F#UNCTION-GR#APH').NE.0)THEN
                      IF(FLAG(J+1).AND.
     -                     (NCF.LT.1.OR.FUNCT(1:NCF).EQ.' '))THEN
                           CALL INPMSG(J,
     -                         'Function not specified.       ')
                           OK=.FALSE.
                      ELSE
                           CALL INPSTR(J+1,J+1,FUNCT,NCF)
                           LFUNPL=.TRUE.
                           INEXT=J+2
                      ENDIF
                 ELSEIF(INPCMP(J,'NOF#UNCTION-GR#APH').NE.0)THEN
                      LFUNPL=.FALSE.
                      FUNCT=' '
                      NCF=1
*   Skip this processing if the keyword is not recognised.
                 ELSE
                      INEXT=J
                      GOTO 20
                 ENDIF
40               CONTINUE
                 INEXT=NWORD+1
*   Warn if no track has been defined.
            ELSE
                 CALL INPMSG(I,'The track has not been set.   ')
                 OK=.FALSE.
            ENDIF
*   Check whether the drift-lines have to start from the zeros.
       ELSEIF(INPCMP(I,'Z#EROS').NE.0)THEN
            START='ZERO'
*   Search for particle type,
       ELSEIF(INPCMP(I,'EL#ECTRON').NE.0)THEN
            Q=-1
            ITYPE=1
       ELSEIF(INPCMP(I,'I#ON').NE.0)THEN
            IF(GASOK(2))THEN
                 Q=+1
                 ITYPE=2
            ELSE
                 CALL INPMSG(I,'Ion mobility data are missing.')
                 OK=.FALSE.
            ENDIF
*   Look for the keyword CONTOUR,
       ELSEIF(INPCMP(I,'CONT#OURS-#INTERVAL')+
     -      INPCMP(I,'ISO#CHRONS-#INTERVAL')+
     -      INPCMP(I,'ISO#CHRONES-#INTERVAL').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Should have a delta t as arg. ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,TSTEPR,TSTEP)
                 IF(TSTEPR.LE.0.0)THEN
                      CALL INPMSG(I,'See the next message.         ')
                      CALL INPMSG(I+1,'Interval must be larger than 0')
                      OK=.FALSE.
                 ENDIF
                 IF(IFAIL.EQ.0.AND.TSTEPR.GT.0)THEN
                      LEQTPL=.TRUE.
                      TSTEP=TSTEPR
                 ENDIF
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'NOCONT#OURS')+
     -      INPCMP(I,'NOISO#CHRONS')+
     -      INPCMP(I,'NOISO#CHRONES').NE.0)THEN
            LEQTPL=.FALSE.
*   Reverse isochrones.
       ELSEIF(INPCMP(I,'REV#ERSE-#ISOCHRONES')+
     -      INPCMP(I,'REV#ERSE-#ISOCHRONS').NE.0)THEN
            ISOREV=1
       ELSEIF(INPCMP(I,'NOREV#ERSE-#ISOCHRONES')+
     -      INPCMP(I,'NOREV#ERSE-#ISOCHRONS').NE.0)THEN
            ISOREV=-1
*   Look for the drift-line plotting option.
       ELSEIF(INPCMP(I,'L#INE-PL#OT').NE.0)THEN
            LLINPL=.TRUE.
       ELSEIF(INPCMP(I,'NOL#INE-PL#OT').NE.0)THEN
            LLINPL=.FALSE.
*   Look for the drift-line printing option.
       ELSEIF(INPCMP(I,'L#INE-PR#INT').NE.0)THEN
            LLINPR=.TRUE.
       ELSEIF(INPCMP(I,'NOL#INE-PR#INT').NE.0)THEN
            LLINPR=.FALSE.
*   Look for the charge of the particles to be drifted.
       ELSEIF(INPCMP(I,'POS#ITIVE').NE.0)THEN
            Q=+1.0
       ELSEIF(INPCMP(I,'NEG#ATIVE').NE.0)THEN
            Q=-1.0
*   Look for the Monte-Carlo options.
       ELSEIF(INPCMP(I,'M#ONTE-C#ARLO-#DRIFT-#LINES')+
     -      INPCMP(I,'MC-#DRIFT-#LINES').NE.0)THEN
            LDRMC=.TRUE.
       ELSEIF(INPCMP(I,'NOM#ONTE-C#ARLO-#DRIFT-#LINES')+
     -      INPCMP(I,'NOMC-#DRIFT-#LINES')+
     -      INPCMP(I,'RUN#GE-K#UTTA-#DRIFT-#LINES')+
     -      INPCMP(I,'RKF-#DRIFT-#LINES').NE.0)THEN
            LDRMC=.FALSE.
*   Valid option out of context.
       ELSEIF(FLAG(I))THEN
            CALL INPMSG(I,'Valid option out of context.  ')
            OK=.FALSE.
*   Option not known.
       ELSE
            CALL INPMSG(I,'The option is not known.      ')
            OK=.FALSE.
       ENDIF
20     CONTINUE
       CALL INPERR
*** Check for errors.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### DRFDRF ERROR   : Instruction is not'//
     -           ' carried out because of the above errors.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### DRFDRF ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
       ENDIF
*** Check at least some output has been requested.
       IF(.NOT.(LEQTPL.OR.LLINPL.OR.LLINPR.OR.(START.EQ.'TRACK'.AND.
     -      (LTIMPL.OR.LVELPL.OR.LDIFPL.OR.LAVAPL.OR.LFUNPL))))THEN
            PRINT *,' !!!!!! DRFDRF WARNING : DRIFT statement not',
     -           ' executed because all output has been suppressed.'
            RETURN
       ENDIF
*   Carry out the drifting operation.
       IF(START.EQ.'WIRE')THEN
            IF(ISOREV.EQ.1)THEN
                 LEQRWI=.TRUE.
            ELSEIF(ISOREV.EQ.-1)THEN
                 LEQRWI=.FALSE.
            ENDIF
            IF(ITYPE.EQ.1)THEN
                 CALL DRFWIR(-Q,ITYPE,TSTEP,LEQTPL,LEQRWI,
     -                ANGMIN,ANGMAX,LLINPL,LLINPR,NLINEW)
            ELSE
                 CALL DRFWIR(+Q,ITYPE,TSTEP,LEQTPL,LEQRWI,
     -                ANGMIN,ANGMAX,LLINPL,LLINPR,NLINEW)
            ENDIF
       ELSEIF(START.EQ.'SOLID')THEN
            IF(ISOREV.EQ.1)THEN
                 LEQRSO=.TRUE.
            ELSEIF(ISOREV.EQ.-1)THEN
                 LEQRSO=.FALSE.
            ENDIF
            IF(ITYPE.EQ.1)THEN
                 CALL DRFSOL(-Q,ITYPE,TSTEP,LEQTPL,LEQRSO,
     -                LLINPL,LLINPR,NLINEV)
            ELSE
                 CALL DRFSOL(+Q,ITYPE,TSTEP,LEQTPL,LEQRSO,
     -                LLINPL,LLINPR,NLINEV)
            ENDIF
       ELSEIF(START.EQ.'EDGE')THEN
            IF(ISOREV.EQ.1)THEN
                 LEQRED=.TRUE.
            ELSEIF(ISOREV.EQ.-1)THEN
                 LEQRED=.FALSE.
            ENDIF
            CALL DRFEDG(Q,ITYPE,TSTEP,LEQTPL,LEQRED,
     -                LLINPL,LLINPR,SIDE,NLINEE)
       ELSEIF(START.EQ.'TRACK')THEN
            IF(ISOREV.EQ.1)THEN
                 LEQRTR=.TRUE.
            ELSEIF(ISOREV.EQ.-1)THEN
                 LEQRTR=.FALSE.
            ENDIF
            CALL DRFTRA(Q,ITYPE,TSTEP,LDRMC,LEQTPL,LEQRTR,
     -           LLINPL,LLINPR,LTIMPL,LVELPL,LDIFPL,LAVAPL,LFUNPL,
     -           FUNCT,NCF,MARKER)
       ELSEIF(START.EQ.'ZERO')THEN
            IF(ISOREV.EQ.1)THEN
                 LEQRZE=.TRUE.
            ELSEIF(ISOREV.EQ.-1)THEN
                 LEQRZE=.FALSE.
            ENDIF
            CALL DRFZRO(Q,ITYPE,LLINPL,LLINPR,LEQTPL,LEQRZE)
       ELSE
            PRINT *,' !!!!!! DRFDRF WARNING : Plot type has not been'//
     -           ' specified; no drift plot.'
       ENDIF
       END
