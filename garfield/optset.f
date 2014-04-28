CDECK  ID>, OPTSET.
       SUBROUTINE OPTSET
*-----------------------------------------------------------------------
*   OPTSET - Routine attempting to find proper voltage settings.
*   (Last changed on 20/10/99.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       CHARACTER*(MXCHAR) FUNFLD,FUNPOS,FUNWGT
       CHARACTER*10 VALTYP,PNTTYP
       REAL VST(MXWIRE),VPLST(5)
       LOGICAL EVALT,EVALD,EVALA
       INTEGER NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,IENFLD,IENPOS,IENWGT
       COMMON /OPTDAT/ VST,VPLST,NPOINT,NSWIRE,IOPT,NFLD,NPOS,NWGT,
     -      IENFLD,IENPOS,IENWGT,EVALT,EVALD,EVALA
       COMMON /OPTCHR/ FUNFLD,FUNPOS,FUNWGT,VALTYP,PNTTYP
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
       CHARACTER*10 VARLIS(MXVAR)
       LOGICAL FLAG(MXWORD+1),USE(MXVAR),LFITPR,OK
       INTEGER NITMAX,NWORD,I,INEXT,NRES,IFAIL,NPNT,NPAR,INPCMP
       DOUBLE PRECISION XFIT(MXFPNT),YFIT(MXFPNT),WEIGHT(MXFPNT),
     -      AFIT(MXFPAR),EAFIT(MXFPAR),CHI2,DIST,EPS
       REAL VFIT(MXWIRE),VPLFIT(5),DISTR,EPSR,AVER
       EXTERNAL OPTFUN,INPCMP
       SAVE DIST,EPS,NITMAX,LFITPR
       DATA DIST,EPS /1.0D0,1.0D-4/
       DATA NITMAX /10/
       DATA LFITPR /.TRUE./
*** Decode the argument string, first get the number of arguments.
       CALL INPNUM(NWORD)
**  Preset the flagging logicals.
       DO 10 I=1,MXWORD+1
       FLAG(I)=.FALSE.
       IF(INPCMP(I,'A#VERAGE')+INPCMP(I,'D#ISTANCE')+
     -      INPCMP(I,'EPS#ILON')+
     -      INPCMP(I,'I#TERATE-#LIMIT')+INPCMP(I,'G#RID')+
     -      INPCMP(I,'NOPR#INT')+INPCMP(I,'ON')+INPCMP(I,'PR#INT')+
     -      INPCMP(I,'TO')+INPCMP(I,'TR#ACK')+INPCMP(I,'W#IRE').NE.0.OR.
     -      I.GT.NWORD)FLAG(I)=.TRUE.
10     CONTINUE
*   The first arguments is normally the function.
       IF(NWORD.GT.1.AND..NOT.FLAG(2))THEN
            INEXT=3
            CALL INPSTR(2,2,FUNFLD,NFLD)
       ELSE
            INEXT=2
       ENDIF
*   Keep track of errors in the input.
       OK=.TRUE.
*   Loop over the arguments.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*   Set the maximum-norm.
       IF(INPCMP(I,'D#ISTANCE').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No value of the norm present. ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,DISTR,1.0)
                 DIST=DISTR
                 INEXT=I+2
            ENDIF
*   Set the differentiation and change parameter.
       ELSEIF(INPCMP(I,'EPS#ILON').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No value of EPSILON present.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL)
                 CALL INPRDR(I+1,EPSR,1.0E-4)
                 EPS=EPSR
                 INEXT=I+2
            ENDIF
*   Set the iteration limit.
       ELSEIF(INPCMP(I,'I#TERATE-#LIMIT').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No iteration bound present.   ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL)
                 CALL INPRDI(I+1,NITMAX,10)
                 INEXT=I+2
            ENDIF
*   Select the NOPRINT option.
       ELSEIF(INPCMP(I,'NOPR#INT').NE.0)THEN
            LFITPR=.FALSE.
*   Find the domain.
       ELSEIF(INPCMP(I,'ON').NE.0)THEN
            IF(INPCMP(I+1,'TR#ACK').NE.0)THEN
                 IF(TRFLAG(1))THEN
                      PNTTYP='TRACK'
                      INEXT=I+2
                 ELSE
                      CALL INPMSG(I+1,'No track has been defined.    ')
                      OK=.FALSE.
                 ENDIF
            ELSEIF(INPCMP(I+1,'G#RID').NE.0)THEN
                 PNTTYP='GRID'
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'W#IRE').NE.0)THEN
                 PNTTYP='WIRE'
                 INEXT=I+2
            ENDIF
*   Select the PRINT option.
       ELSEIF(INPCMP(I,'PR#INT').NE.0)THEN
            LFITPR=.TRUE.
*   Find the target function.
       ELSEIF(INPCMP(I,'TO').NE.0)THEN
            IF(INPCMP(I+1,'A#VERAGE').NE.0)THEN
                 VALTYP='AVERAGE'
                 INEXT=I+2
            ELSEIF(.NOT.FLAG(I+1))THEN
                 CALL INPSTR(I+1,I+1,FUNPOS,NPOS)
                 VALTYP='FUNCTION'
                 INEXT=I+2
            ENDIF
*   Valid keyword out of context.
       ELSEIF(INPCMP(I,'TR#ACK')+INPCMP(I,'G#RID')+
     -      INPCMP(I,'A#VERAGE').NE.0)THEN
            CALL INPMSG(I,'Valid keyword out of context. ')
            OK=.FALSE.
*   Weighting function.
       ELSEIF(INPCMP(I,'W#EIGHT').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No weighting function found.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,FUNWGT,NWGT)
                 INEXT=I+2
            ENDIF
*   Keyword unknown.
       ELSE
            CALL INPMSG(I,'Not a known keyword.          ')
            OK=.FALSE.
       ENDIF
20     CONTINUE
**  Dump error messages, if any.
       CALL INPERR
*** Take action depending on the state of OK.
       IF(.NOT.OK)THEN
            IF(JFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! OPTSET WARNING : Errors found in'//
     -                ' the command; trying with defaults.'
            ELSEIF(JFAIL.EQ.2)THEN
                 PRINT *,' !!!!!! OPTSET WARNING : Errors found in'//
     -                ' the command; no attempt to achieve settings.'
                 RETURN
            ELSE
                 PRINT *,' !!!!!! OPTSET WARNING : Errors found in'//
     -                ' the command; terminating program execution.'
                 CALL QUIT
            ENDIF
       ENDIF
*** Generate some debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ OPTSET DEBUG   : The function '',
     -           A,'' has to approximate'')') FUNFLD(1:NFLD)
            IF(VALTYP.EQ.'AVERAGE')WRITE(LUNOUT,'(26X,''the current'',
     -           '' average of the function,'')')
            IF(VALTYP.EQ.'FUNCTION')WRITE(LUNOUT,'(26X,''the value of'',
     -           '' the function '',A,'','')') FUNPOS(1:NPOS)
            WRITE(LUNOUT,'(26X,''using '',A,
     -           '' as weighting function.'')') FUNWGT(1:NWGT)
            WRITE(LUNOUT,'(26X,''Averaging takes place over the '',A)')
     -           PNTTYP
            WRITE(LUNOUT,'(26X,''Maximum distance='',E10.3,'', eps='',
     -           E10.3,'', NITMAX='',I3,''.'')') DIST,EPS,NITMAX
       ENDIF
*** Get the number of 'S' wires if the WIRE option has been selected.
       IF(PNTTYP.EQ.'WIRE')THEN
            NSWIRE=0
            DO 30 I=1,NWIRE
            IF(WIRTYP(I).EQ.'S')NSWIRE=NSWIRE+1
30          CONTINUE
       ENDIF
*** Convert the field function, first set variable names.
       IF(POLAR)THEN
            VARLIS(1)='R         '
            VARLIS(2)='PHI       '
            VARLIS(3)='ER        '
            VARLIS(4)='EPHI      '
       ELSE
            VARLIS(1)='X         '
            VARLIS(2)='Y         '
            VARLIS(3)='EX        '
            VARLIS(4)='EY        '
       ENDIF
       VARLIS(5)='E         '
       VARLIS(6)='V         '
*   Drift related information.
       VARLIS(7)='TIME      '
       VARLIS(8)='DIFFUSION '
       VARLIS(9)='AVALANCHE '
C      VARLIS(10)='LORENTZ   '
*   Conversion of the field-function (dependence check + average).
       CALL ALGPRE(FUNFLD(1:NFLD),NFLD,VARLIS, 9,NRES,USE,IENFLD,IFAIL)
*   Check the output.
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because of an error in the field function.'
            CALL ALGCLR(IENFLD)
            RETURN
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because the field function does not return a',
     -           ' single value.'
            CALL ALGCLR(IENFLD)
            RETURN
       ELSEIF(.NOT.(USE(3).OR.USE(4).OR.USE(5).OR.USE(6).OR.
     -      USE(7).OR.USE(8).OR.USE(9)))THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because the field function is field independent.'
            CALL ALGCLR(IENFLD)
            RETURN
       ENDIF
*   Check whether we have to evaluate V.
       IF(USE(6))THEN
            IOPT=1
       ELSE
            IOPT=0
       ENDIF
*   Drift velocity data.
       IF(USE(7).AND..NOT.GASOK(1))THEN
            PRINT *,' !!!!!! OPTSET WARNING : The field function uses'//
     -           ' the drift time but drift'
            PRINT *,'                         velocity data has not'//
     -           ' been entered; not executed.'
            CALL ALGCLR(IENFLD)
            RETURN
       ELSEIF(USE(7)          )THEN
            EVALT=.TRUE.
       ELSE
            EVALT=.FALSE.
       ENDIF
*   Diffusion data.
       IF(USE(8).AND..NOT.GASOK(3))THEN
            PRINT *,' !!!!!! OPTSET WARNING : The field function uses'//
     -           ' the diffusion but diffusion'
            PRINT *,'                         coefficients have not'//
     -           ' been entered; not executed.'
            CALL ALGCLR(IENFLD)
            RETURN
       ELSEIF(USE(8))THEN
            EVALD=.TRUE.
       ELSE
            EVALD=.FALSE.
       ENDIF
*   Avalanche data.
       IF(USE(9).AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! OPTSET WARNING : The field function uses'//
     -           ' the avalanche but Townsend'
            PRINT *,'                         coefficients have not'//
     -           ' been entered; not executed.'
            CALL ALGCLR(IENFLD)
            RETURN
       ELSEIF(USE(9))THEN
            EVALA=.TRUE.
       ELSE
            EVALA=.FALSE.
       ENDIF
*   Any of the above for other than TRACK or GRID.
       IF((USE(7).OR.USE(8).OR.USE(9)).AND.PNTTYP.EQ.'WIRE')THEN
            PRINT *,' !!!!!! OPTSET WARNING : Drift time, diffusion'//
     -           ' and multiplication not allowed with ON WIRE.'
            CALL ALGCLR(IENFLD)
            RETURN
       ENDIF
*** Get the average of the field function, if needed.
       IF(VALTYP.EQ.'AVERAGE')THEN
            CALL OPTAVE(AVER,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! OPTSET WARNING : Unable to evaluate'//
     -                ' the current function average.'
                 CALL ALGCLR(IENFLD)
                 RETURN
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ OPTSET DEBUG   : The'',
     -           '' current field-function average is '',E15.8)') AVER
            WRITE(FUNPOS,'(E15.8,65X)') AVER
            NPOS=15
       ENDIF
*** Convert the target and weight functions, set variable names.
       IF(POLAR)THEN
            VARLIS(1)='R         '
            VARLIS(2)='PHI       '
       ELSE
            VARLIS(1)='X         '
            VARLIS(2)='Y         '
       ENDIF
*   The conversion itself.
       CALL ALGPRE(FUNPOS(1:NPOS),NPOS,VARLIS,2,NRES,USE,IENPOS,IFAIL)
*   Check the output.
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because of an error in the position function.'
            CALL ALGCLR(IENFLD)
            CALL ALGCLR(IENPOS)
            RETURN
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because the position function does not return'//
     -           ' a single value.'
            CALL ALGCLR(IENFLD)
            CALL ALGCLR(IENPOS)
            RETURN
       ENDIF
**  Conversion of the weight-function.
       CALL ALGPRE(FUNWGT(1:NWGT),NWGT,VARLIS,2,NRES,USE,IENWGT,IFAIL)
*   Check the output.
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because of an error in the weight function.'
            CALL ALGCLR(IENFLD)
            CALL ALGCLR(IENWGT)
            CALL ALGCLR(IENPOS)
            RETURN
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because the weight function does not return a',
     -           ' single value.'
            CALL ALGCLR(IENFLD)
            CALL ALGCLR(IENWGT)
            CALL ALGCLR(IENPOS)
            RETURN
       ENDIF
*** Set the fitting input parameters.
       CALL OPTXYA(XFIT,YFIT,AFIT,WEIGHT,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! OPTSET WARNING : SET not executed'//
     -           ' because setting fitting parameters failed.'
            CALL ALGCLR(IENFLD)
            CALL ALGCLR(IENWGT)
            CALL ALGCLR(IENPOS)
            RETURN
       ENDIF
*** Carry out the fitting itself.
       IF(PNTTYP.EQ.'GRID')THEN
            NPNT=NGRIDX*NGRIDY
       ELSEIF(PNTTYP.EQ.'TRACK')THEN
            NPNT=NPOINT
       ELSEIF(PNTTYP.EQ.'WIRE')THEN
            NPNT=NSWIRE
       ENDIF
       NPAR=NSW
       CALL LSQFIT(OPTFUN,AFIT,EAFIT,NPAR,XFIT,YFIT,WEIGHT,NPNT,
     -      NITMAX,DIST,CHI2,EPS,LFITPR,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! OPTSET WARNING : The new'//
     -      ' potentials do not fulfill your requirements.'
*** And calculate the charges for the final result.
       DO 100 I=1,NWIRE
       IF(INDSW(I).NE.0)THEN
            VFIT(I)=VST(I)+AFIT(INDSW(I))
       ELSE
            VFIT(I)=VST(I)
       ENDIF
100    CONTINUE
       DO 110 I=1,4
       IF(YNPLAN(I).AND.INDPLA(I).NE.0)THEN
            VPLFIT(I)=VPLST(I)+AFIT(INDPLA(I))
       ELSE
            VPLFIT(I)=VPLST(I)
       ENDIF
110    CONTINUE
       IF(TUBE.AND.INDPLA(5).NE.0)THEN
            VPLFIT(5)=VPLST(5)+AFIT(INDPLA(5))
       ELSE
            VPLFIT(5)=VPLST(5)
       ENDIF
       CALL SETNEW(VFIT,VPLFIT,IFAIL)
       IF(IFAIL.NE.0)PRINT *,' !!!!!! OPTSET WARNING : Failure to'//
     -      ' compute the wire charges for the final settings.'
*** Release the field and position function instruction list.
       CALL ALGCLR(IENFLD)
       CALL ALGCLR(IENWGT)
       CALL ALGCLR(IENPOS)
*** Register the amount of CPU time spent on these calculations.
       CALL TIMLOG('Playing with the voltage settings:      ')
       END
