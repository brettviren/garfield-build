CDECK  ID>, DRFSIN.
       SUBROUTINE DRFSIN
*-----------------------------------------------------------------------
*   DRFSIN - Prints and plots information on a single drift-line.
*   (Last changed on  6/ 2/00.)
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
       DOUBLE PRECISION F0(3)
       REAL XPL(MXLIST),YPL(MXLIST),YPR(MXLIST),VAR(MXVAR),RES(3),
     -      GASDFL,GASTWN,GASATT,VOLT,XSTART,YSTART,ZSTART,QPART,PATH
       INTEGER MODVAR(MXVAR),MODRES(3),I,IFAIL1,IFAIL2,IFAIL3,IENTRY,
     -      NRES,ILOC,NCSTAT,IFAIL,NCFPL1,NCFPL2,NCFPR,INPTYP,
     -      INPCMP,IFROM,INEXT,IPART,NWORD
       LOGICAL KPLOT,KPRINT,USE(MXVAR)
       CHARACTER*(MXCHAR) FPL1,FPL2,FPR
       CHARACTER*80 STASTR
       CHARACTER*20 PARTID
       CHARACTER*10 VARLIS(MXVAR)
       EXTERNAL GASDFL,GASTWN,GASATT,INPTYP,INPCMP
       DATA (VARLIS(I),I=1,21) /
     -      'X         ','Y         ','PATH      ','EX        ',
     -      'EY        ','E         ','BX        ','BY        ',
     -      'BZ        ','B         ','VDX       ','VDY       ',
     -      'VDZ       ','VD        ','TIME      ','DIFFUSION ',
     -      'TOWNSEND  ','STATUS    ','ATTACHMENT','EZ        ',
     -      'Z         '/
*** Defaults.
       IFROM=0
       FPL1='0'
       NCFPL1=1
       FPL2='0'
       NCFPL2=1
       KPLOT=.FALSE.
       FPR='0'
       NCFPR=1
       KPRINT=.FALSE.
       QPART=-1.0
       IPART=1
*** Decode the argument list.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 10 I=2,NWORD
       IF(I.LT.INEXT)GOTO 10
*   FROM component.
       IF(INPCMP(I,'FR#OM').NE.0)THEN
            CALL INPCHK(I+1,2,IFAIL1)
            CALL INPCHK(I+2,2,IFAIL2)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.NWORD.LT.I+2)THEN
                 CALL INPMSG(I,'Invalid or incomplete args.   ')
            ELSE
                 CALL INPRDR(I+1,XSTART,0.0)
                 CALL INPRDR(I+2,YSTART,0.0)
                 IF(POLAR)THEN
                      CALL CFMPTR(XSTART,YSTART,XSTART,YSTART,1,IFAIL3)
                      IF(IFAIL3.NE.0)THEN
                           CALL INPMSG(I,
     -                          'Not a valid polar coordinate. ')
                           IFROM=0
                      ELSE
                           IFROM=1
                      ENDIF
                 ELSE
                      IFROM=1
                 ENDIF
                 IF(IFROM.EQ.1.AND.
     -                (XSTART.LT.DXMIN.OR.XSTART.GT.DXMAX.OR.
     -                YSTART.LT.DYMIN.OR.YSTART.GT.DYMAX))THEN
                      CALL INPMSG(I,'Starting point outside AREA.  ')
                      IFROM=0
                 ENDIF
            ENDIF
            IF(INPTYP(I+3).EQ.1.OR.INPTYP(I+3).EQ.2)THEN
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPRDR(I+3,ZSTART,0.0)
                 INEXT=I+4
                 IF(ZSTART.LT.DZMIN.OR.ZSTART.GT.DZMAX)THEN
                      CALL INPMSG(I,'Starting point outside AREA')
                      IFROM=0
                 ENDIF
            ELSE
                 IFAIL3=0
                 ZSTART=0.0
                 INEXT=I+3
            ENDIF
*   Functions to be plotted.
       ELSEIF(INPCMP(I,'PL#OT').NE.0)THEN
            IF(INPCMP(I+2,'VS').EQ.0.OR.I+3.GT.NWORD)THEN
                 CALL INPMSG(I,'Invalid or incomplete args.   ')
            ELSE
                 CALL INPSTR(I+1,I+1,FPL2,NCFPL2)
                 CALL INPSTR(I+3,I+3,FPL1,NCFPL1)
                 KPLOT=.TRUE.
                 INEXT=I+4
            ENDIF
       ELSEIF(INPCMP(I,'NOPL#OT').NE.0)THEN
            KPLOT=.FALSE.
*   Function to be printed.
       ELSEIF(INPCMP(I,'PR#INT').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Invalid or incomplete args.   ')
            ELSE
                 CALL INPSTR(I+1,I+1,FPR,NCFPR)
                 KPRINT=.TRUE.
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'NOPR#INT').NE.0)THEN
            KPRINT=.FALSE.
*   Particle type.
       ELSEIF(INPCMP(I,'E#LECTRON').NE.0)THEN
            IPART=1
       ELSEIF(INPCMP(I,'I#ON').NE.0)THEN
            IF(GASOK(2))THEN
                 IPART=2
            ELSE
                 CALL INPMSG(I,'Ion mobility data missing.    ')
            ENDIF
*   Particle charge.
       ELSEIF(INPCMP(I,'POS#ITIVE').NE.0)THEN
            QPART=+1.0
       ELSEIF(INPCMP(I,'NEG#ATIVE').NE.0)THEN
            QPART=-1.0
*   Anything else is not valid.
       ELSE
            CALL INPMSG(I,'Not recognised as a keyword.  ')
       ENDIF
10     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Check completeness of the arguments.
       IF(IFROM.EQ.0.OR..NOT.(KPRINT.OR.KPLOT))THEN
            PRINT *,' !!!!!! DRFSIN WARNING : FROM component missing'//
     -           ' or no output requested; not executed.'
            RETURN
       ENDIF
*** Translate the functions, assign appropriate variable names.
       IF(POLAR)THEN
            VARLIS(1)='R         '
            VARLIS(2)='PHI       '
            VARLIS(4)='ER        '
            VARLIS(5)='EPHI      '
            VARLIS(11)='VDR       '
            VARLIS(12)='VDPHI     '
       ELSE
            VARLIS(1)='X         '
            VARLIS(2)='Y         '
            VARLIS(4)='EX        '
            VARLIS(5)='EY        '
            VARLIS(11)='VDX       '
            VARLIS(12)='VDY       '
       ENDIF
*   Handle the case of user editor steps.
       IF(INDEX(FPL1(1:NCFPL1)//FPL2(1:NCFPL2)//
     -      FPR(1:NCFPR),'@').NE.0)THEN
            NRES=3
            CALL ALGEDT(VARLIS,21,IENTRY,USE,NRES)
            FPL1=' '
            NCFPL1=1
            FPL2='Edited function'
            NCFPL2=1
            FPR='Edited function'
            NCFPR=15
*   Ordinary formula translation.
       ELSE
            CALL ALGPRE(FPL1(1:NCFPL1)//','//FPL2(1:NCFPL2)//','//
     -           FPR(1:NCFPR),NCFPL1+NCFPL2+NCFPR+2,VARLIS,21,
     -           NRES,USE,IENTRY,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! DRFSIN WARNING : Graph and printed'//
     -                ' table not produced because of syntax errors.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF((USE(11).OR.USE(12).OR.USE(13).OR.USE(14)).AND.
     -           .NOT.((IPART.EQ.1.AND.GASOK(1)).OR.
     -           (IPART.EQ.2.AND.GASOK(2))))THEN
                 PRINT *,' !!!!!! DRFSIN WARNING : Drift velocity'//
     -                ' data used in formula, but data is absent.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF(USE(16).AND..NOT.(GASOK(3).OR.GASOK(8)))THEN
                 PRINT *,' !!!!!! DRFSIN WARNING : Diffusion'//
     -                ' data used in formula, but data is absent.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF(USE(17).AND..NOT.GASOK(4))THEN
                 PRINT *,' !!!!!! DRFSIN WARNING : Townsend'//
     -                ' data used in formula, but data is absent.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ELSEIF(USE(19).AND..NOT.GASOK(6))THEN
                 PRINT *,' !!!!!! DRFSIN WARNING : Attachment'//
     -                ' data used in formula, but data is absent.'
                 CALL ALGCLR(IENTRY)
                 RETURN
            ENDIF
       ENDIF
*   Check that there really are 3 results.
       IF(NRES.NE.3)THEN
            PRINT *,' !!!!!! DRFSIN WARNING : Graph and printed table'//
     -           ' not produced: incorrect number of formula elements.'
            CALL ALGCLR(IENTRY)
            RETURN
       ENDIF
*** Compute the drift line.
       CALL DLCALC(XSTART,YSTART,ZSTART,QPART,IPART)
*** Zero the output variables.
       DO 30 I=1,21
       MODVAR(I)=2
       VAR(I)=0.0
30     CONTINUE
*** Initialise the integrated path length.
       PATH=0.0
*** Loop over the resulting drift-line, filling plot and print vectors.
       DO 20 I=1,NU
*   Position, time and status.
       VAR(1)=XU(I)
       VAR(2)=YU(I)
       VAR(21)=ZU(I)
       VAR(15)=TU(I)
       VAR(18)=ISTAT
*   Field.
       IF(USE(4).OR.USE(5).OR.USE(6).OR.USE(20).OR.
     -      USE(16).OR.USE(17).OR.USE(19))
     -      CALL EFIELD(VAR(1),VAR(2),VAR(21),
     -      VAR(4),VAR(5),VAR(20),VAR(6),VOLT,0,ILOC)
       IF(USE(7).OR.USE(8).OR.USE(9).OR.USE(10).OR.
     -      USE(16).OR.USE(17).OR.USE(19))
     -      CALL BFIELD(VAR(1),VAR(2),VAR(21),
     -      VAR(7),VAR(8),VAR(9),VAR(10))
*   Drift velocity.
       IF(USE(11).OR.USE(12).OR.USE(13).OR.USE(14))THEN
            CALL DLCVEL(XU(I),YU(I),ZU(I),F0,QPART,IPART,ILOC)
            VAR(11)=REAL(F0(1))
            VAR(12)=REAL(F0(2))
            VAR(13)=REAL(F0(3))
       ENDIF
*   Diffusion, Townsend and attachment coefficients.
       IF(POLAR)THEN
            IF(USE(16))VAR(16)=
     -           GASDFL(VAR(4)/EXP(VAR(1)),VAR(5)/EXP(VAR(1)),VAR(20),
     -                VAR(7),VAR(8),VAR(9))
            IF(USE(17))VAR(17)=
     -           GASTWN(VAR(4)/EXP(VAR(1)),VAR(5)/EXP(VAR(1)),VAR(20),
     -                VAR(7),VAR(8),VAR(9))
            IF(USE(19))VAR(19)=
     -           GASATT(VAR(4)/EXP(VAR(1)),VAR(5)/EXP(VAR(1)),VAR(20),
     -                VAR(7),VAR(8),VAR(9))
       ELSE
            IF(USE(16))VAR(16)=GASDFL(VAR(4),VAR(5),VAR(20),
     -                VAR(7),VAR(8),VAR(9))
            IF(USE(17))VAR(17)=GASTWN(VAR(4),VAR(5),VAR(20),
     -                VAR(7),VAR(8),VAR(9))
            IF(USE(19))VAR(19)=GASATT(VAR(4),VAR(5),VAR(20),
     -                VAR(7),VAR(8),VAR(9))
       ENDIF
*   Transform vectors and covectors to polar coordinates if needed.
       IF(POLAR)THEN
            CALL CFMRTP(VAR(1),VAR(2),VAR(1),VAR(2),1)
            VAR(4)=VAR(4)/VAR(1)
            VAR(5)=VAR(5)/VAR(1)
            VAR(6)=VAR(6)/VAR(1)
            VAR(11)=VAR(11)*VAR(1)
            VAR(12)=VAR(12)*VAR(1)
       ENDIF
*   Store magnitude of drift velocity.
       IF(USE(14))VAR(14)=SQRT(VAR(11)**2+VAR(12)**2+VAR(13)**2)
*   Conversion of the location.
       IF(POLAR)CALL CF2RTC(XU(I),YU(I),XU(I),YU(I),1)
*   Path.
       IF(I.GT.1)PATH=PATH+SQRT((XU(I)-XU(I-1))**2+(YU(I)-YU(I-1))**2+
     -      (ZU(I)-ZU(I-1))**2)
       VAR(3)=PATH
*   Function evaluation.
       CALL ALGEXE(IENTRY,VAR,MODVAR,21,RES,MODRES,3,IFAIL)
       XPL(I)=RES(1)
       YPL(I)=RES(2)
       YPR(I)=RES(3)
*   Next point of the drift line.
20     CONTINUE
*** Prepare output strings.
       CALL DLCSTF(ISTAT,STASTR,NCSTAT)
       IF(QPART.GT.0)THEN
            PARTID='Positive'
       ELSE
            PARTID='Negative'
       ENDIF
       IF(IPART.EQ.1)THEN
            PARTID(9:)=' electron'
       ELSE
            PARTID(9:)=' ion'
       ENDIF
*** Remove the algebra entry point.
       CALL ALGCLR(IENTRY)
*** Print the results if requested.
       IF(KPRINT)THEN
            IF(POLAR)THEN
                 CALL CFMRTP(XSTART,YSTART,XSTART,YSTART,1)
                 CALL CF2CTP(XU,YU,XU,YU,NU)
                 WRITE(LUNOUT,'(/''  SINGLE DRIFT-LINE PRINT-OUT:''//
     -                ''  Starting point: ('',E10.3,2('','',E10.3),
     -                '')''/''  Drifting:       '',A/
     -                ''  Status code:    '',A/
     -                ''  Function:       '',A//
     -                ''           r [cm]     phi [degree]'',
     -                ''           z [cm]  time [microsec]'',
     -                ''         Function'')') XSTART,YSTART,ZSTART,
     -                PARTID,STASTR(1:NCSTAT),FPR(1:NCFPR)
            ELSE
                 WRITE(LUNOUT,'(/''  SINGLE DRIFT-LINE PRINT-OUT:''//
     -                ''  Starting point: ('',E10.3,2('','',E10.3),
     -                '')''/''  Drifting:       '',A/
     -                ''  Status code:    '',A/
     -                ''  Function:       '',A//
     -                ''           x [cm]          y [cm]]'',
     -                ''           z [cm]  time [microsec]'',
     -                ''         Function'')') XSTART,YSTART,ZSTART,
     -                PARTID,STASTR(1:NCSTAT),FPR(1:NCFPR)
            ENDIF
            DO 40 I=1,NU
            WRITE(LUNOUT,'(5(2X,E15.8))') REAL(XU(I)),REAL(YU(I)),
     -           REAL(ZU(I)),REAL(TU(I)),YPR(I)
40          CONTINUE
       ENDIF
*** Plot the results if requested.
       IF(KPLOT)THEN
            CALL GRGRPH(XPL,YPL,NU,FPL1(1:NCFPL1),FPL2(1:NCFPL2),
     -           'SINGLE DRIFT-LINE GRAPH')
            IF(CELLID.NE.' ')CALL GRCOMM(1,'Cell: '//CELLID)
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
            CALL GRCOMM(3,'Drifting: '//PARTID)
            CALL GRCOMM(4,'Status: '//STASTR(1:NCSTAT))
            CALL GRNEXT
            CALL GRALOG('Single drift line plot.                 ')
       ENDIF
       END
