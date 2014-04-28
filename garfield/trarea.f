CDECK  ID>, TRAREA.
       SUBROUTINE TRAREA
*-----------------------------------------------------------------------
*   TRAREA - Reads a track definition
*   (Last changed on 19/ 9/11.)
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
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       INTEGER NWORD,INPCMP,INPTYP,IFAIL1,IFAIL2,IFAIL3,IFAIL4,IFAIL5,
     -      IFAIL6,NLINR,I,J,INEXT,NCAUX,NRES,NVAR,IENWGT,NCNAME,
     -      MODVAR(1),NREXP,MODRES(1),NSAMR,IRCOOR,IRWGT,ISCOOR,ISWGT,
     -      MATSLT,IORD,NC1,NC2,NC3,NC4,NC5,NC6,NFLXR
       REAL XMASS,XENER,XDIST,XCHAR,XNORM,XT0D,XT1D,YT0D,YT1D,ZT0D,ZT1D,
     -      FACT,XDIR,YDIR,ZDIR,RES(1),VAR(1),WGTSUM,FLXR,XELEC,CLUSTR
       LOGICAL START,END,DIST,DIR,ENER,MASS,CHARGE,USE(1),OK
       EXTERNAL INPCMP,INPTYP,MATSLT
       CHARACTER*10 VARLIS(1),NAME
       CHARACTER*40 AUX1,AUX2,AUX3,AUX4,AUX5,AUX6
       CHARACTER*20 AUX
*** Identify the procedure if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE TRAREA ///'
*** Count words.
       CALL INPNUM(NWORD)
*** Perhaps only printing has been requested.
       IF(NWORD.EQ.1)THEN
*   Track location.
            IF(TRFLAG(1))THEN
                 XT0D=XT0
                 YT0D=YT0
                 ZT0D=ZT0
                 XT1D=XT1
                 YT1D=YT1
                 ZT1D=ZT1
                 IF(POLAR)THEN
                      CALL CFMCTP(XT0D,YT0D,XT0D,YT0D,1)
                      CALL CFMCTP(XT1D,YT1D,XT1D,YT1D,1)
                 ENDIF
                 CALL OUTFMT(XT0D,2,AUX1,NC1,'LEFT')
                 CALL OUTFMT(YT0D,2,AUX2,NC2,'LEFT')
                 CALL OUTFMT(ZT0D,2,AUX3,NC3,'LEFT')
                 CALL OUTFMT(XT1D,2,AUX4,NC4,'LEFT')
                 CALL OUTFMT(YT1D,2,AUX5,NC5,'LEFT')
                 CALL OUTFMT(ZT1D,2,AUX6,NC6,'LEFT')
                 WRITE(LUNOUT,'(''  The current track runs from '',
     -                ''('',A,'','',A,'','',A,'') to '',
     -                ''('',A,'','',A,'','',A,'').'')')
     -                AUX1(1:NC1),AUX2(1:NC2),AUX3(1:NC3),
     -                AUX4(1:NC4),AUX5(1:NC5),AUX6(1:NC6)
            ELSE
                 WRITE(LUNOUT,'(''  The location of the track is'',
     -                '' not yet defined.'')')
            ENDIF
*   Clustering type: fixed.
            IF(ITRTYP.EQ.1.AND.TRFLAG(3))THEN
                 CALL OUTFMT(REAL(NTRLIN),2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  There will be '',A,'' equally'',
     -                '' spaced clusters on the track.'')') AUX1(1:NC1)
            ELSEIF(ITRTYP.EQ.1.AND..NOT.TRFLAG(3))THEN
                 WRITE(LUNOUT,'(''  There will be equally'',
     -                '' spaced clusters on the track.'')')
*   Clustering type: equal spacing.
            ELSEIF(ITRTYP.EQ.2)THEN
                 WRITE(LUNOUT,'(''  Clusters will be equally spaced'',
     -                '' respecting the mean from the gas section.'')')
*   Clustering type: exponential spacing.
            ELSEIF(ITRTYP.EQ.3)THEN
                 WRITE(LUNOUT,'(''  Clusters will be exponentially'',
     -                '' spaced with a mean distance as entered'',
     -                '' in the gas section.'')')
*   Clustering type: processing by HEED.
            ELSEIF(ITRTYP.EQ.4)THEN
                 WRITE(LUNOUT,'(''  Clusters will be generated by'',
     -                '' the Heed program.'')')
*   Particle type.
                 IF(TRFLAG(2))THEN
                      CALL OUTFMT(TRMASS,2,AUX1,NC1,'LEFT')
                      CALL OUTFMT(TRENER,2,AUX2,NC2,'LEFT')
                      CALL OUTFMT(TRCHAR,2,AUX3,NC3,'LEFT')
                      WRITE(LUNOUT,'(''  The particle is a '',A,
     -                     '' with a'','' mass of '',A,'' MeV,''/
     -                     ''  a kinetic energy of '',A,
     -                     '' MeV and charge '',A,''.'')')
     -                     PNAME(1:NCPNAM),AUX1(1:NC1),AUX2(1:NC2),
     -                     AUX3(1:NC3)
                      IF(ABS(TRMASS-0.51099907).LT.0.01.AND.
     -                     TRENER.LE.TRELEC)THEN
                           WRITE(LUNOUT,'(''  This electron is'',
     -                          '' treated as a delta.'')')
                      ELSE
                           WRITE(LUNOUT,'(''  This electron is'',
     -                          '' treated as a generic charged'',
     -                          '' particle.'')')
                      ENDIF
                 ELSE
                      WRITE(LUNOUT,'(''  The particle type, mass and'',
     -                     '' energy have not yet been specified.'')')
                 ENDIF
                 IF(LTRMS)THEN
                      WRITE(LUNOUT,'(''  The incoming particle'',
     -                     '' undergoes multiple scattering,'')')
                 ELSE
                      WRITE(LUNOUT,'(''  The incoming particle does'',
     -                     '' not undergo multiple scattering,'')')
                 ENDIF
                 IF(LTRDEL)THEN
                      WRITE(LUNOUT,'(''  delta electrons have a'',
     -                     '' spatial extent,'')')
                 ELSE
                      WRITE(LUNOUT,'(''  delta electrons are'',
     -                     '' compactified onto the main track,'')')
                 ENDIF
                 IF(LTREXB)THEN
                      WRITE(LUNOUT,'(''  the path of delta electrons'',
     -                     '' is influenced by the E and B fields,'')')
                 ELSE
                      WRITE(LUNOUT,'(''  the path of delta electrons'',
     -                     '' is not influenced by the E and B'',
     -                     '' fields,'')')
                 ENDIF
                 IF(LTRCUT)THEN
                      WRITE(LUNOUT,'(''  the track is truncated when'',
     -                     '' all energy has been deposited.'')')
                 ELSE
                      WRITE(LUNOUT,'(''  the track is not truncated'',
     -                     '' when all energy has been deposited.'')')
                 ENDIF
*   Weighted cluster location distribution.
            ELSEIF(ITRTYP.EQ.5)THEN
                 IF(TRFLAG(5))THEN
                      CALL OUTFMT(REAL(NTRSAM),2,AUX1,NC1,'LEFT')
                 ELSE
                      AUX1='an unknown number of'
                      NC1=20
                 ENDIF
                 WRITE(LUNOUT,'(''  There will be '',A,'' clusters'',
     -                '' at positions weighted according to '',A)')
     -                AUX1(1:NC1),FCNTRW(1:NCTRW)
*   Single cluster.
            ELSEIF(ITRTYP.EQ.6)THEN
                 WRITE(LUNOUT,'(''  There will be a single cluster'',
     -                '' at a random position.'')')
*   Equal flux lines.
            ELSEIF(ITRTYP.EQ.7)THEN
                 IF(TRFLAG(6))THEN
                      CALL OUTFMT(REAL(NTRFLX),2,AUX1,NC1,'LEFT')
                 ELSE
                      AUX1='an unknown number of'
                      NC1=20
                 ENDIF
                 WRITE(LUNOUT,'(''  There will be '',A,'' clusters'',
     -                '' at equal flux intervals.'')') AUX1(1:NC1)
*   Flux intervals.
            ELSEIF(ITRTYP.EQ.8)THEN
                 IF(TRFLAG(7))THEN
                      CALL OUTFMT(TRFLUX,2,AUX1,NC1,'LEFT')
                 ELSE
                      AUX1='an unknown'
                      NC1=10
                 ENDIF
                 CALL OUTFMT(TRFLUX,2,AUX1,NC1,'LEFT')
                 WRITE(LUNOUT,'(''  Clusters will be spaced by a'',
     -                '' flux of '',A,'' V.'')') AUX1(1:NC1)
*   SRIM generation.
            ELSEIF(ITRTYP.EQ.9)THEN
                 IF(.NOT.SRIMOK)THEN
                      WRITE(LUNOUT,'(''  Clustering requested'',
     -                     '' according to a SRIM table, but no'',
     -                     '' such data has yet been entered.'')')
                 ELSEIF(TRFLAG(2))THEN
                      CALL OUTFMT(TRMASS,2,AUX1,NC1,'LEFT')
                      CALL OUTFMT(TRENER,2,AUX2,NC2,'LEFT')
                      CALL OUTFMT(TRCHAR,2,AUX3,NC3,'LEFT')
                      WRITE(LUNOUT,'(''  Clusters will be generated'',
     -                     '' according to a SRIM table.''//
     -                     ''  The particle is a '',A,
     -                     '' with a'','' mass of '',A,'' MeV,''/
     -                     ''  a kinetic energy of '',A,
     -                     '' MeV and charge '',A,''.''/)')
     -                     PNAME(1:NCPNAM),AUX1(1:NC1),AUX2(1:NC2),
     -                     AUX3(1:NC3)
                 ELSE
                      WRITE(LUNOUT,'(''  Clustering requested'',
     -                     '' according to a SRIM table, but particle'',
     -                     '' properties are not complete yet.'')')
                 ENDIF
                 IF(TRNSRM.LT.-1.5)THEN
                      AUX2='Single cluster'
                      NC2=14
                 ELSEIF(TRNSRM.LT.0)THEN
                      AUX2='Automatic'
                      NC2=9
                 ELSE
                      CALL OUTFMT(TRNSRM,2,AUX2,NC2,'LEFT')
                 ENDIF
                 IF(ITFSRM.EQ.0)THEN
                      AUX4='no fluctuations'
                      NC4=15
                 ELSEIF(ITFSRM.EQ.1)THEN
                      AUX4='Landau'
                      NC4=6
                 ELSEIF(ITFSRM.EQ.2)THEN
                      AUX4='Vavilov'
                      NC4=7
                 ELSEIF(ITFSRM.EQ.3)THEN
                      AUX4='Gaussian'
                      NC4=8
                 ELSEIF(ITFSRM.EQ.4)THEN
                      AUX4='Combined'
                      NC4=8
                 ELSE
                      AUX4='? Unknown ?'
                      NC4=11
                 ENDIF
                 WRITE(LUNOUT,'(
     -                ''  Energy loss fluctuation model: '',A/
     -                ''  Longitudinal straggling:       '',L1/
     -                ''  Transverse straggling:         '',L1/
     -                ''  Fast (F)/precise (T) Vavilov:  '',L1/
     -                ''  Cluster size:                  '',A,''.'')')
     -                AUX4(1:NC4),LDLSRM,LDTSRM,LTRVVL,AUX2(1:NC2)
*   TRIM generation.
            ELSEIF(ITRTYP.EQ.10)THEN
               CALL OUTFMT(TRMASS,2,AUX1,NC1,'LEFT')
               CALL OUTFMT(TRENER,2,AUX2,NC2,'LEFT')
               CALL OUTFMT(TRCHAR,2,AUX3,NC3,'LEFT')
               WRITE(LUNOUT,'(''  Clusters will be generated'',
     -                '' according to TRIM files.''//
     -                ''  The particle is a '',A,
     -                '' with a'','' mass of '',A,'' MeV,''/
     -                ''  a kinetic energy of '',A,
     -                '' MeV and charge '',A,''.''/)')
     -                PNAME(1:NCPNAM),AUX1(1:NC1),AUX2(1:NC2),
     -                AUX3(1:NC3)
            ENDIF
            RETURN
       ENDIF
*** Preset flags.
       START =.FALSE.
       END   =.FALSE.
       DIST  =.FALSE.
       DIR   =.FALSE.
       ENER  =.FALSE.
       IF(SRIMOK.AND.TRMASS.GT.0.AND.TRCHAR.NE.0)THEN
            MASS  =.TRUE.
            CHARGE=.TRUE.
       ELSE
            MASS  =.FALSE.
            CHARGE=.FALSE.
       ENDIF
*** Compute default track parameters.
       XT0D=XT0
       YT0D=YT0
       ZT0D=ZT0
       XT1D=XT1
       YT1D=YT1
       ZT1D=ZT1
       IF(POLAR)THEN
            CALL CFMCTP(XT0D,YT0D,XT0D,YT0D,1)
            CALL CFMCTP(XT1D,YT1D,XT1D,YT1D,1)
       ENDIF
*** Format: (x0,y0,z0) (x1,y1,z1)
       IF(NWORD.GE.7.AND.INPTYP(2).GE.1.AND.INPTYP(3).GE.1.AND.
     -      INPTYP(4).GE.1.AND.INPTYP(5).GE.1.AND.
     -      INPTYP(6).GE.1.AND.INPTYP(7).GE.1)THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPCHK(5,2,IFAIL4)
            CALL INPCHK(6,2,IFAIL5)
            CALL INPCHK(7,2,IFAIL6)
            CALL INPRDR(2,XT0D,XT0D)
            CALL INPRDR(3,YT0D,YT0D)
            CALL INPRDR(4,ZT0D,ZT0D)
            CALL INPRDR(5,XT1D,XT1D)
            CALL INPRDR(6,YT1D,YT1D)
            CALL INPRDR(7,ZT1D,ZT1D)
            START=.TRUE.
            END=.TRUE.
            INEXT=8
*** Format: (x0,y0) (x1,y1)
       ELSEIF(NWORD.GE.5.AND.INPTYP(2).GE.1.AND.INPTYP(3).GE.1.AND.
     -      INPTYP(4).GE.1.AND.INPTYP(5).GE.1)THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPCHK(5,2,IFAIL4)
            CALL INPRDR(2,XT0D,XT0D)
            CALL INPRDR(3,YT0D,YT0D)
            ZT0D=0
            CALL INPRDR(4,XT1D,XT1D)
            CALL INPRDR(5,YT1D,YT1D)
            ZT1D=0
            START=.TRUE.
            END=.TRUE.
            INEXT=6
*** Format: (x0,y0,z0)
       ELSEIF(NWORD.GE.4.AND.INPTYP(2).GE.1.AND.INPTYP(3).GE.1.AND.
     -      INPTYP(4).GE.1)THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPCHK(4,2,IFAIL3)
            CALL INPRDR(2,XT0D,XT0D)
            CALL INPRDR(3,YT0D,YT0D)
            CALL INPRDR(4,ZT0D,ZT0D)
            START=.TRUE.
            INEXT=5
*** Format: (x0,y0)
       ELSEIF(NWORD.GE.3.AND.INPTYP(2).GE.1.AND.INPTYP(3).GE.1)THEN
            CALL INPCHK(2,2,IFAIL1)
            CALL INPCHK(3,2,IFAIL2)
            CALL INPRDR(2,XT0D,XT0D)
            CALL INPRDR(3,YT0D,YT0D)
            ZT0D=0
            START=.TRUE.
            INEXT=4
       ELSE
            INEXT=2
       ENDIF
*** Now scan from here on for further arguments.
       DO 10 I=1,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Could be a starting point.
       IF(INPCMP(I,'FR#OM')+INPCMP(I,'START#ING-#POINT').NE.0)THEN
            IF(NWORD.LT.I+2.OR.
     -           INPTYP(I+1).LE.0.OR.INPTYP(I+2).LE.0)THEN
                 CALL INPMSG(I,'Has 2 or 3 real arguments.')
            ELSEIF(INPTYP(I+3).LE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,XT0D,XT0D)
                 CALL INPRDR(I+2,YT0D,YT0D)
                 ZT0D=0
                 START=.TRUE.
                 INEXT=I+3
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPRDR(I+1,XT0D,XT0D)
                 CALL INPRDR(I+2,YT0D,YT0D)
                 CALL INPRDR(I+3,ZT0D,ZT0D)
                 START=.TRUE.
                 INEXT=I+4
            ENDIF
*   Could be an end point.
       ELSEIF(INPCMP(I,'TO')+INPCMP(I,'END-#POINT').NE.0)THEN
            IF(NWORD.LT.I+2.OR.
     -           INPTYP(I+1).LE.0.OR.INPTYP(I+2).LE.0)THEN
                 CALL INPMSG(I,'Has 2 or 3 real arguments.')
            ELSEIF(INPTYP(I+3).LE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,XT1D,XT1D)
                 CALL INPRDR(I+2,YT1D,YT1D)
                 ZT1D=0
                 END=.TRUE.
                 INEXT=I+3
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPRDR(I+1,XT1D,XT1D)
                 CALL INPRDR(I+2,YT1D,YT1D)
                 CALL INPRDR(I+3,ZT1D,ZT1D)
                 END=.TRUE.
                 INEXT=I+4
            ENDIF
*   Could be a direction vector.
       ELSEIF(INPCMP(I,'DIR#ECTION').NE.0)THEN
            IF(INPCMP(I+1,'X')+INPCMP(I+1,'POS#ITIVE-X').NE.0)THEN
                 TRXDIR=+1
                 TRYDIR= 0
                 TRZDIR= 0
                 DIR=.TRUE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'NEG#ATIVE-X').NE.0)THEN
                 TRXDIR=-1
                 TRYDIR= 0
                 TRZDIR= 0
                 DIR=.TRUE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'Y')+INPCMP(I+1,'POS#ITIVE-Y').NE.0)THEN
                 TRXDIR= 0
                 TRYDIR=+1
                 TRZDIR= 0
                 DIR=.TRUE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'NEG#ATIVE-Y').NE.0)THEN
                 TRXDIR= 0
                 TRYDIR=-1
                 TRZDIR= 0
                 DIR=.TRUE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'Z')+INPCMP(I+1,'POS#ITIVE-Z').NE.0)THEN
                 TRXDIR= 0
                 TRYDIR= 0
                 TRZDIR=+1
                 DIR=.TRUE.
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'NEG#ATIVE-Z').NE.0)THEN
                 TRXDIR= 0
                 TRYDIR= 0
                 TRZDIR=-1
                 DIR=.TRUE.
                 INEXT=I+2
            ELSEIF(NWORD.LT.I+2.OR.
     -           INPTYP(I+1).LE.0.OR.INPTYP(I+2).LE.0)THEN
                 CALL INPMSG(I,'Has 2 or 3 real arguments.')
            ELSEIF(INPTYP(I+3).LE.0)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,XDIR,0.0)
                 CALL INPRDR(I+2,YDIR,0.0)
                 ZDIR=0.0
                 DIR=.TRUE.
                 INEXT=I+3
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPRDR(I+1,XDIR,0.0)
                 CALL INPRDR(I+2,YDIR,0.0)
                 CALL INPRDR(I+3,ZDIR,0.0)
                 DIR=.TRUE.
                 INEXT=I+4
            ENDIF
            IF(DIR)THEN
                 XNORM=SQRT(XDIR**2+YDIR**2+ZDIR**2)
                 IF(XNORM.LE.0)THEN
                      CALL INPMSG(I,'Vector has norm 0')
                      DIR=.FALSE.
                 ELSE
                      XDIR=XDIR/XNORM
                      YDIR=YDIR/XNORM
                      ZDIR=ZDIR/XNORM
                 ENDIF
            ENDIF
*   Could be a range.
       ELSEIF(INPCMP(I,'DIST#ANCE').NE.0.OR.
     -      INPCMP(I,'RANGE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Has 1 real argument.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,XDIST,-1.0)
                 IF(XDIST.LT.0)THEN
                      CALL INPMSG(I+1,'Range is not >= 0.')
                 ELSE
                      TRDIST=XDIST
                      DIST=.TRUE.
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Could be a particle identifier [PDG, Phys Rev D 54 (1996)]
       ELSEIF(INPCMP(I,'ELE#CTRON')+INPCMP(I,'E-M#INUS').NE.0)THEN
            TRMASS=0.51099907
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='electron-'
            NCPNAM=9
            ITRTYP=4
       ELSEIF(INPCMP(I,'POS#ITRON')+INPCMP(I,'E-P#LUS')+
     -      INPCMP(I,'E+').NE.0)THEN
            TRMASS=0.51099907
            TRCHAR=+1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='electron+'
            NCPNAM=9
            ITRTYP=4
       ELSEIF(INPCMP(I,'MU#ON-#MINUS').NE.0)THEN
            TRMASS=105.658389
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='mu-'
            NCPNAM=3
            ITRTYP=4
       ELSEIF(INPCMP(I,'MU#ON-P#LUS')+INPCMP(I,'MU+').NE.0)THEN
            TRMASS=105.658389
            TRCHAR=+1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='mu+'
            NCPNAM=3
            ITRTYP=4
       ELSEIF(INPCMP(I,'TAU-#MINUS').NE.0)THEN
            TRMASS=1777.00
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='tau-'
            NCPNAM=4
            ITRTYP=4
       ELSEIF(INPCMP(I,'TAU-P#LUS')+INPCMP(I,'TAU+').NE.0)THEN
            TRMASS=1777.00
            TRCHAR=+1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='tau+'
            NCPNAM=4
            ITRTYP=4
       ELSEIF(INPCMP(I,'GAMMA')+INPCMP(I,'PHOTON').NE.0)THEN
            TRMASS=0.0
            TRCHAR=0.0
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='gamma'
            NCPNAM=5
            ITRTYP=4
       ELSEIF(INPCMP(I,'PI#ON-#MINUS').NE.0)THEN
            TRMASS=139.56995
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='pi-'
            NCPNAM=3
            ITRTYP=4
       ELSEIF(INPCMP(I,'PI#ON-0')+INPCMP(I,'PI#ON-Z#ERO')+
     -      INPCMP(I,'PI0').NE.0)THEN
            TRMASS=134.9764
            TRCHAR= 0
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='pi0'
            NCPNAM=3
            ITRTYP=4
       ELSEIF(INPCMP(I,'PI#ON-PLUS')+INPCMP(I,'PI+').NE.0)THEN
            TRMASS=139.56995
            TRCHAR=+1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='pi+'
            NCPNAM=3
            ITRTYP=4
       ELSEIF(INPCMP(I,'K#AON-#MINUS').NE.0)THEN
            TRMASS=493.677
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='K-'
            NCPNAM=2
            ITRTYP=4
       ELSEIF(INPCMP(I,'K#AON-0-#SHORT')+INPCMP(I,'K#AON-0-#LONG')+
     -      INPCMP(I,'K0-#SHORT')+INPCMP(I,'K0-#LONG')+
     -      INPCMP(I,'K#AON-Z#ERO-#SHORT')+
     -      INPCMP(I,'K#AON-Z#ERO-#LONG')+
     -      INPCMP(I,'K0-#SHORT')+INPCMP(I,'K0-#LONG').NE.0)THEN
            TRMASS=497.672
            TRCHAR= 0
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='K0'
            NCPNAM=2
            ITRTYP=4
       ELSEIF(INPCMP(I,'K#AON-P#LUS')+INPCMP(I,'K+').NE.0)THEN
            TRMASS=493.677
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='K+'
            NCPNAM=2
            ITRTYP=4
       ELSEIF(INPCMP(I,'PR#OTON').NE.0)THEN
            TRMASS=938.27231
            TRCHAR=+1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='proton'
            NCPNAM=6
            ITRTYP=4
       ELSEIF(INPCMP(I,'ANTI-PR#OTON').NE.0)THEN
            TRMASS=938.27231
            TRCHAR=-1
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='antiproton'
            NCPNAM=10
            ITRTYP=4
       ELSEIF(INPCMP(I,'N#EUTRON')+INPCMP(I,'ANTI-N#EUTRON').NE.0)THEN
            TRMASS=939.56563
            TRCHAR= 0
            MASS=.TRUE.
            CHARGE=.TRUE.
            PNAME='neutron'
            NCPNAM=7
            ITRTYP=4
*   Manually described particle, first mass.
       ELSEIF(INPCMP(I,'MASS').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 real argument')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,XMASS,TRMASS)
                 IF(I+2.LE.NWORD.AND.INPCMP(I+2,'EV').NE.0)THEN
                      FACT=1E-6
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'KEV').NE.0)THEN
                      FACT=1E-3
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'MEV').NE.0)THEN
                      FACT=1
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'GEV').NE.0)THEN
                      FACT=1E+3
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'TEV').NE.0)THEN
                      FACT=1E+6
                      INEXT=I+3
                 ELSE
                      FACT=1
                      INEXT=I+2
                 ENDIF
                 IF(XMASS.LT.0)THEN
                      CALL INPMSG(I+1,'Mass is not >= 0.')
                 ELSE
                      TRMASS=FACT*XMASS
                      MASS=.TRUE.
                      ITRTYP=4
                      IF(TRMASS.LE.1)THEN
                           CALL OUTFMT(ANINT(TRMASS*1000)/1000,2,
     -                          AUX,NCAUX,'LEFT')
                      ELSE
                           CALL OUTFMT(ANINT(TRMASS),2,
     -                          AUX,NCAUX,'LEFT')
                      ENDIF
                      PNAME='m('//AUX(1:NCAUX)//')'
                      NCPNAM=MIN(LEN(PNAME),NCAUX+3)
                 ENDIF
            ENDIF
*   Charge.
       ELSEIF(INPCMP(I,'CH#ARGE').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 real argument')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,XCHAR,TRCHAR)
                 IF(ABS(XCHAR).LT.0.99.OR.ABS(XCHAR).GT.1.01)
     -                PRINT *,' ------ TRAREA MESSAGE : Heed'//
     -                     ' will assume a charge of 1.'
                 TRCHAR=XCHAR
                 CHARGE=.TRUE.
                 ITRTYP=4
                 INEXT=I+2
            ENDIF
*   Switching energy for delta electrons.
       ELSEIF(INPCMP(I,'SWITCH-#ELECTRON-#TO-#CHARGED').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 real argument')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,XELEC,TRELEC)
                 IF(XELEC.LE.0)THEN
                      CALL INPMSG(I,'Switching energy not > 0.')
                 ELSE
                      TRELEC=XELEC
                      ITRTYP=4
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Energy of the particle.
       ELSEIF(INPCMP(I,'ENE#RGY').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 real argument')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,XENER,TRENER)
                 IF(I+2.LE.NWORD.AND.INPCMP(I+2,'EV').NE.0)THEN
                      FACT=1E-6
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'KEV').NE.0)THEN
                      FACT=1E-3
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'MEV').NE.0)THEN
                      FACT=1
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'GEV').NE.0)THEN
                      FACT=1E+3
                      INEXT=I+3
                 ELSEIF(I+2.LE.NWORD.AND.INPCMP(I+2,'TEV').NE.0)THEN
                      FACT=1E+6
                      INEXT=I+3
                 ELSE
                      FACT=1
                      INEXT=I+2
                 ENDIF
                 IF(XENER.LE.0)THEN
                      CALL INPMSG(I+1,'Energy is not > 0.')
                 ELSE
                      TRENER=FACT*XENER
                      ENER=.TRUE.
                 ENDIF
            ENDIF
*   Delta electrons or not.
       ELSEIF(INPCMP(I,'DELTA-#ELECTRONS').NE.0)THEN
            LTRDEL=.TRUE.
            ITRTYP=4
       ELSEIF(INPCMP(I,'NODELTA-#ELECTRONS').NE.0)THEN
            LTRDEL=.FALSE.
*   Trace delta electrons or not.
       ELSEIF(INPCMP(I,'TR#ACE-DELTA-#ELECTRONS').NE.0)THEN
            LTREXB=.TRUE.
            ITRTYP=4
       ELSEIF(INPCMP(I,'NOTR#ACE-DELTA-#ELECTRONS').NE.0)THEN
            LTREXB=.FALSE.
*   Apply energy cut or not.
       ELSEIF(INPCMP(I,'E#NERGY-CUT').NE.0)THEN
            LTRCUT=.TRUE.
            ITRTYP=4
       ELSEIF(INPCMP(I,'NOE#NERGY-CUT').NE.0)THEN
            LTRCUT=.FALSE.
*   Multiple scattering or not.
       ELSEIF(INPCMP(I,'MULT#IPLE-SC#ATTERING').NE.0)THEN
            LTRMS=.TRUE.
            ITRTYP=4
       ELSEIF(INPCMP(I,'NOMULT#IPLE-SC#ATTERING').NE.0)THEN
            LTRMS=.FALSE.
*   Track interpolation or not.
       ELSEIF(INPCMP(I,'INT#ERPOLATE-TR#ACK').NE.0)THEN
            LTRINT=.TRUE.
       ELSEIF(INPCMP(I,'NOINT#ERPOLATE-TR#ACK').NE.0)THEN
            LTRINT=.FALSE.
*   Number of points on the track.
       ELSEIF(INPCMP(I,'LINE#S')+INPCMP(I,'POINT#S').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 integer argument')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NLINR,NTRLIN)
                 IF(NLINR.LT.0)THEN
                      CALL INPMSG(I+1,'Number is not > 0.')
                 ELSE
                      NTRLIN=NLINR
                      TRFLAG(3)=.TRUE.
                      ITRTYP=1
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Number of sampling points on the track.
       ELSEIF(INPCMP(I,'SAMP#LING')+INPCMP(I,'SAMP#LES').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 integer argument')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NSAMR,NTRSAM)
                 IF(NLINR.LT.0)THEN
                      CALL INPMSG(I+1,'Number is not > 0.')
                 ELSE
                      NTRSAM=NSAMR
                      TRFLAG(5)=.TRUE.
                      ITRTYP=5
                 ENDIF
                 INEXT=I+2
            ENDIF
**  Weighting function.
       ELSEIF(INPCMP(I,'WEIGHT#ING-F#UNCTION').NE.0)THEN
            IF(NWORD.LT.I+1)THEN
                 CALL INPMSG(I,'Should have an argument')
*   In the form of matrices.
            ELSEIF(INPCMP(I+2,'VS').NE.0.AND.I+3.LE.NWORD)THEN
*   Locate the matrices.
                 IRCOOR=0
                 IRWGT=0
                 CALL INPSTR(I+1,I+1,NAME,NCNAME)
                 DO 110 J=1,NGLB
                 IF(GLBMOD(J).EQ.5.AND.GLBVAR(J).EQ.NAME(1:NCNAME))
     -                IRWGT=NINT(GLBVAL(J))
110              CONTINUE
                 ISWGT=MATSLT(IRWGT)
                 CALL INPSTR(I+3,I+3,NAME,NCNAME)
                 DO 120 J=1,NGLB
                 IF(GLBMOD(J).EQ.5.AND.GLBVAR(J).EQ.NAME(1:NCNAME))
     -                IRCOOR=NINT(GLBVAL(J))
120              CONTINUE
                 ISCOOR=MATSLT(IRCOOR)
                 IF(ISWGT.EQ.0)CALL INPMSG(I+1,'Not a known matrix.')
                 IF(ISCOOR.EQ.0)CALL INPMSG(I+3,'Not a known matrix.')
*   Carry out interpolation.
                 IF(ISCOOR.NE.0.AND.ISWGT.NE.0)THEN
                      IORD=2
                      WGTSUM=0
                      OK=.TRUE.
                      DO 130 J=1,MXLIST
                      VAR(1)=REAL(J-1)/REAL(MXLIST-1)
                      CALL MATIN1(IRCOOR,IRWGT,1,VAR(1),RES(1),
     -                     ISCOOR,ISWGT,IORD,IFAIL1)
                      WGT(J)=MAX(0.0,RES(1))
                      IF(RES(1).LT.0)OK=.FALSE.
                      WGTSUM=WGTSUM+WGT(J)
130                   CONTINUE
                      IF(WGTSUM.GT.0.AND.OK)THEN
                           CALL HISPRD(WGT,MXLIST)
                           ITRTYP=5
                           CALL INPSTR(I+1,I+3,FCNTRW,NCTRW)
                           TRFLAG(4)=.TRUE.
                      ELSEIF(.NOT.OK)THEN
                           CALL INPMSG(I+1,'Sometimes < 0.')
                      ELSE
                           CALL INPMSG(I+1,'Has a zero norm.')
                      ENDIF
                 ENDIF
                 INEXT=I+4
*   In the form of a function.
            ELSE
                 CALL INPSTR(I+1,I+1,FCNTRW,NCTRW)
                 VARLIS(1)='T'
                 NVAR=1
                 CALL ALGPRE(FCNTRW(1:NCTRW),NCTRW,VARLIS,NVAR,
     -                NRES,USE,IENWGT,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      CALL INPMSG(I+1,'Not a valid function.')
                 ELSE
                      WGTSUM=0
                      OK=.TRUE.
                      DO 30 J=1,MXLIST
                      VAR(1)=REAL(J-1)/REAL(MXLIST-1)
                      MODVAR(1)=2
                      NVAR=1
                      NREXP=1
                      CALL ALGEXE(IENWGT,VAR,MODVAR,NVAR,RES,
     -                     MODRES,NREXP,IFAIL1)
                      WGT(J)=MAX(0.0,RES(1))
                      IF(RES(1).LT.0)OK=.FALSE.
                      WGTSUM=WGTSUM+WGT(J)
30                    CONTINUE
                      CALL ALGCLR(IENWGT)
                      CALL ALGERR
                      IF(WGTSUM.GT.0.AND.OK)THEN
                           CALL HISPRD(WGT,MXLIST)
                           ITRTYP=5
                           TRFLAG(4)=.TRUE.
                      ELSEIF(.NOT.OK)THEN
                           CALL INPMSG(I+1,'Sometimes < 0.')
                      ELSE
                           CALL INPMSG(I+1,'Has a zero norm.')
                      ENDIF
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Number of sampling points on the track.
       ELSEIF(INPCMP(I,'FL#UX-L#INES').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 integer argument')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NFLXR,NTRFLX)
                 IF(NFLXR.LT.2)THEN
                      CALL INPMSG(I+1,'Number is not > 1.')
                 ELSE
                      NTRFLX=NFLXR
                      TRFLAG(6)=.TRUE.
                      ITRTYP=7
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Number of sampling points on the track.
       ELSEIF(INPCMP(I,'FL#UX-INT#ERVALS').NE.0)THEN
            IF(NWORD.LT.I+1.OR.INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I,'Must have 1 real argument')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FLXR,TRFLUX)
                 IF(FLXR.LE.0)THEN
                      CALL INPMSG(I+1,'Interval is not > 0.')
                 ELSE
                      TRFLUX=FLXR
                      TRFLAG(7)=.TRUE.
                      ITRTYP=8
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Cluster size: number of electrons per deposit
       ELSEIF(INPCMP(I,'GROUP#ING').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Argument missing')
            ELSEIF(INPCMP(I+1,'AUTO#MATIC').NE.0)THEN
                 TRNSRM=-1
                 ITRTYP=9
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'SING#LE-#CLUSTER').NE.0)THEN
                 TRNSRM=-2
                 ITRTYP=9
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'NONE').NE.0)THEN
                 TRNSRM=1
                 ITRTYP=9
                 INEXT=I+2
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,CLUSTR,0.0)
                 IF(CLUSTR.LT.1.0)THEN
                      CALL INPMSG(I,'Must be >= 1.')
                      OK=.FALSE.
                 ELSE
                      TRNSRM=MAX(1.0,ANINT(CLUSTR))
                      ITRTYP=9
                 ENDIF
                 INEXT=I+2
            ENDIF
*   Type of SRIM fluctuations
       ELSEIF(INPCMP(I,'FLUCT#UATION-MOD#EL').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Argument missing')
            ELSEIF(INPCMP(I+1,'NONE')+
     -           INPCMP(I+1,'NO-FLUCT#UATIONS').NE.0)THEN
                 ITFSRM=0
                 ITRTYP=9
            ELSEIF(INPCMP(I+1,'LAN#DAU-#DISTRIBUTION').NE.0)THEN
                 ITFSRM=1
                 ITRTYP=9
            ELSEIF(INPCMP(I+1,'VAV#ILOV-#DISTRIBUTION').NE.0)THEN
                 ITFSRM=2
                 ITRTYP=9
            ELSEIF(INPCMP(I+1,'GAU#SSIAN-#DISTRIBUTION')+
     -           INPCMP(I+1,'NORM#AL-#DISTRIBUTION').NE.0)THEN
                 ITFSRM=3
                 ITRTYP=9
            ELSEIF(INPCMP(I+1,'COMB#INED-#DISTRIBUTION').NE.0)THEN
                 ITFSRM=4
                 ITRTYP=9
            ELSE
                 CALL INPMSG(I+1,'Unknown model')
            ENDIF
            INEXT=I+2
*   SRIM straggling components.
       ELSEIF(INPCMP(I,'LONG#ITUDINAL-STR#AGGLING').NE.0)THEN
            LDLSRM=.TRUE.
            ITRTYP=9
       ELSEIF(INPCMP(I,'NOLONG#ITUDINAL-STR#AGGLING').NE.0)THEN
            LDLSRM=.FALSE.
            ITRTYP=9
       ELSEIF(INPCMP(I,'TRANS#VERSAL-STR#AGGLING')+
     -      INPCMP(I,'TRANS#VERSE-STR#AGGLING')+
     -      INPCMP(I,'LAT#ERAL-STR#AGGLING').NE.0)THEN
            LDTSRM=.TRUE.
            ITRTYP=9
       ELSEIF(INPCMP(I,'NOTRANS#VERSAL-STR#AGGLING')+
     -      INPCMP(I,'NOTRANS#VERSE-STR#AGGLING')+
     -      INPCMP(I,'NOLAT#ERAL-STR#AGGLING').NE.0)THEN
            LDTSRM=.FALSE.
            ITRTYP=9
*   Fast or precise Vavilov
       ELSEIF(INPCMP(I,'FAST-VAV#ILOV').NE.0)THEN
            LTRVVL=.FALSE.
       ELSEIF(INPCMP(I,'PRE#CISE-VAV#ILOV')+
     -      INPCMP(I,'ACC#URATE-VAV#ILOV').NE.0)THEN
            LTRVVL=.TRUE.
*   Kind of cluster generation.
       ELSEIF(INPCMP(I,'FIX#ED-#NUMBER').NE.0)THEN
            ITRTYP=1
       ELSEIF(INPCMP(I,'EQ#UAL-SP#ACING').NE.0)THEN
            ITRTYP=2
       ELSEIF(INPCMP(I,'EXP#ONENTIAL-#SPACING').NE.0)THEN
            ITRTYP=3
       ELSEIF(INPCMP(I,'HEED').NE.0)THEN
            ITRTYP=4
       ELSEIF(INPCMP(I,'WEIGHT#ED-D#ISTRIBUTION').NE.0)THEN
            ITRTYP=5
       ELSEIF(INPCMP(I,'SIN#GLE-#CLUSTER').NE.0)THEN
            ITRTYP=6
            IF(.NOT.GASOK(5))PRINT *,' ------ TRAREA MESSAGE :'//
     -           ' No cluster size distribution; cluster will'//
     -           ' have size 1.'
       ELSEIF(INPCMP(I,'EQ#UAL-FL#UX-#INTERVALS').NE.0)THEN
            ITRTYP=7
       ELSEIF(INPCMP(I,'CONS#TANT-FL#UX-#INTERVALS').NE.0)THEN
            ITRTYP=8
       ELSEIF(INPCMP(I,'SRIM').NE.0)THEN
            ITRTYP=9
*    Trim cluster generation.
       ELSEIF(INPCMP(I,'TRIM').NE.0)THEN
            ITRTYP=10
*   Not a known keyword.
       ELSE
            CALL INPMSG(I,'Not a known keyword.')
       ENDIF
10     CONTINUE
*   Print the error messages.
       CALL INPERR
*** If the cell is polar, then reconvert coordinates.
       IF(POLAR)THEN
            CALL CFMPTC(XT0D,YT0D,XT0,YT0,1)
            CALL CFMPTC(XT1D,YT1D,XT1,YT1,1)
            ZT0=ZT0D
            ZT1=ZT1D
       ELSE
            XT0=XT0D
            XT1=XT1D
            YT0=YT0D
            YT1=YT1D
            ZT0=ZT0D
            ZT1=ZT1D
       ENDIF
*** Check completeness, first geometry.
       IF(START.AND.END.AND.DIST)THEN
            PRINT *,' ------ TRAREA MESSAGE : Both end point'//
     -           ' and range specified; ignoring range.'
            XDIR=XT1-XT0
            YDIR=YT1-YT0
            ZDIR=ZT1-ZT0
            TRDIST=SQRT(XDIR**2+YDIR**2+ZDIR**2)
            IF(TRDIST.GT.0)THEN
                 XDIR=XDIR/TRDIST
                 YDIR=YDIR/TRDIST
                 ZDIR=ZDIR/TRDIST
            ELSE
                 XDIR=0
                 YDIR=0
                 ZDIR=0
            ENDIF
*   If neither end point nor direction and distance: assume point.
       ELSEIF(START.AND.(.NOT.END).AND.(.NOT.(DIST.AND.DIR)))THEN
            PRINT *,' ------ TRAREA MESSAGE : Only start point'//
     -           ' specified; assuming single point track.'
            XT1=XT0
            YT1=YT0
            ZT1=ZT0
            XDIR=0
            YDIR=0
            ZDIR=0
            TRDIST=0
*   If end point missing, compute from direction and range.
       ELSEIF(START.AND..NOT.END)THEN
            XT1=XT0+XDIR*TRDIST
            YT1=YT0+YDIR*TRDIST
            ZT1=ZT0+ZDIR*TRDIST
*   If direction and range missing, compute from end point.
       ELSEIF(START)THEN
            XDIR=XT1-XT0
            YDIR=YT1-YT0
            ZDIR=ZT1-ZT0
            TRDIST=SQRT(XDIR**2+YDIR**2+ZDIR**2)
            IF(TRDIST.GT.0)THEN
                 XDIR=XDIR/TRDIST
                 YDIR=YDIR/TRDIST
                 ZDIR=ZDIR/TRDIST
            ELSE
                 XDIR=0
                 YDIR=0
                 ZDIR=0
            ENDIF
       ENDIF
*   Set the track location flag if appropriate, reset preparation.
       IF(START)THEN
            TRFLAG(1)=.TRUE.
            CALL DLCTRR
       ENDIF
*   Check mass etc.
       IF(MASS.AND.CHARGE.AND.ENER)THEN
            TRFLAG(2)=.TRUE.
       ELSEIF(JFAIL.EQ.1.AND.(MASS.OR.CHARGE.OR.ENER))THEN
            IF(.NOT.CHARGE)THEN
                 TRCHAR=-1.0
                 PRINT *,' ------ TRAREA MESSAGE : Charge not'//
     -                ' specified; assuming negative charge.'
            ENDIF
            IF(.NOT.MASS)THEN
                 TRMASS=105.658389
                 IF(TRCHAR.LT.0)THEN
                      PNAME='mu-'
                 ELSE
                      PNAME='mu-'
                 ENDIF
                 NCPNAM=3
                 PRINT *,' ------ TRAREA MESSAGE : Mass not'//
     -                ' specified; assuming a muon.'
            ENDIF
            IF(.NOT.ENER)THEN
                 TRENER=1000.0
                 PRINT *,' ------ TRAREA MESSAGE : Energy not'//
     -                ' specified; assuming 1 GeV.'
            ENDIF
            TRFLAG(2)=.TRUE.
       ELSEIF(JFAIL.EQ.2.AND.(MASS.OR.CHARGE.OR.ENER))THEN
            PRINT *,' !!!!!! TRAREA WARNING : The mass, charge or'//
     -           ' energy has been specified, but at least one of'//
     -           ' these elements is missing; particle not defined.'
            TRFLAG(2)=.FALSE.
       ELSEIF(JFAIL.EQ.3.AND.(MASS.OR.CHARGE.OR.ENER))THEN
            PRINT *,' ###### TRAREA ERROR   : The mass, charge or'//
     -           ' energy has been specified, but at least one of'//
     -           ' these elements is missing; end of program.'
            CALL QUIT
       ENDIF
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRAREA DEBUG   : '',
     -      ''Start     ('',E15.8,'','',E15.8,'','',E15.8,'')''/26X,
     -      ''To        ('',E15.8,'','',E15.8,'','',E15.8,'')''/26X,
     -      ''Direction ('',E15.8,'','',E15.8,'','',E15.8,'')''/26X,
     -      ''Range     ='',E15.8,'' cm''/26X,
     -      ''Mass      ='',E15.8,'' MeV''/26X,
     -      ''Energy    ='',E15.8,'' MeV''/26X,
     -      ''Charge    ='',E15.8,'' electron charges''/26X,
     -      ''Lines     ='',I5/26X,
     -      ''Type      ='',I5,'' (1=fixed, 2=equal, 3=exp, 4=HEED,'',
     -      '' 5=weighted, 6=single, 7=flux)''/26X,
     -      ''Location '',L1,'', particle '',L1,'', lines '',L1/26X,
     -      ''weighting function '',L1,'', samples '',L1/26X,
     -      ''flux lines '',L1/26X,
     -      ''MS '',L1,'', delta '',L1,'', trace delta '',L1,
     -      '', energy cut '',L1,'', interpolate '',L1)')
     -      XT0,YT0,ZT0,XT1,YT1,ZT1,XDIR,YDIR,ZDIR,
     -      TRDIST,TRMASS,TRENER,TRCHAR,NTRLIN,ITRTYP,
     -      (TRFLAG(I),I=1,5),LTRMS,LTRDEL,LTREXB,LTRCUT,LTRINT
       END
