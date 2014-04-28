CDECK  ID>, DLCCAL.
       SUBROUTINE DLCCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   DLCCAL - Processes drift line related procedure calls.
*   (Last changed on 15/12/10.)
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
       REAL XLIST(MXMCA),YLIST(MXMCA),ZLIST(MXMCA),TLIST(MXMCA),
     -      ELIST(MXMCA),
     -      XELIST(MXMCA),YELIST(MXMCA),ZELIST(MXMCA),TELIST(MXMCA)
       INTEGER NLIST(MXMCA),ISLIST(MXMCA),NMCA
       COMMON /MCAMAT/ XLIST,YLIST,ZLIST,TLIST,ELIST,
     -      XELIST,YELIST,ZELIST,TELIST,NLIST,ISLIST,NMCA
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER MXAHIS
       PARAMETER(MXAHIS=20)
       INTEGER INPCMX,ISIZ(MXMDIM),IRX,IRY,IRZ,IRT,ISX,ISY,ISZ,IST,
     -      NCOPT,ISTR,NARG,IPROC,INSTR,IFAIL,IFAIL1,IFAIL2,IFAIL3,
     -      IFAIL4,NPAIR,I,J,MATSLT,IAUX,NC,NREXP,ITYPE(2,MXAHIS),NHIST,
     -      NETOT,NITOT,IENTRY(MXAHIS),IHIST(MXAHIS),ISW,ICL,ILOC,NC1,
     -      NC2,NC3,NC4,IPART,IRRVC,IRRMC,IRRVM,IREXC,ISEXC,IRION,ISION,
     -      IRCS,IHDIST,INEXT
       REAL XCLS,YCLS,ZCLS,ECLS,VXMIN,VYMIN,VXMAX,VYMAX,X0,Y0,Z0,Q,
     -      FDIV1,FDIV2,FDIV3,EXTRA1,EXRATE(MXEXG),IORATE(MXEXG),
     -      ESTART,EFINAL,DIRX,DIRY,DIRZ,RNDUNI,THETA0,THETA1,DELAY
       DOUBLE PRECISION XPOS1,YPOS1,XPOS2,YPOS2,F0(3)
       LOGICAL DONE,USE(MXVAR),STAT(4)
       EXTERNAL INPCMX,MATSLT,RNDUNI
       CHARACTER*(MXINCH) TITLE,OPT
       CHARACTER*15 AUX1,AUX2,AUX3,AUX4
       CHARACTER*10 VARLIS(16)
*** Assume the CALL will fail.
       IFAIL=1
*** Verify that we really have a cell and a gas.
       IF(.NOT.CELSET)THEN
            PRINT *,' !!!!!! DLCCAL WARNING : Cell data not available'//
     -           ' ; call not executed.'
            RETURN
       ELSEIF(.NOT.GASSET)THEN
            PRINT *,' !!!!!! DLCCAL WARNING : Gas data not available'//
     -           ' ; call not executed.'
            RETURN
       ENDIF
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Start a new track.
       IF(IPROC.EQ.-501)THEN
*   Warn if there are arguments.
            IF(NARG.NE.0)PRINT *,' !!!!!! DLCCAL WARNING : The'//
     -           ' NEW_TRACK procedure has no arguments; ignored.'
*   Reinitialise the track.
            CALL TRACLI
*** Get a new cluster.
       ELSEIF(IPROC.EQ.-502)THEN
*   Check the arguments.
            IF(NARG.LT.6.OR.NARG.GT.7.OR.
     -           ARGREF(1,1).GE.2.OR.ARGREF(2,1).GE.2.OR.
     -           ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           ARGREF(5,1).GE.2.OR.
     -           (NARG.GE.7.AND.ARGREF(6,1).GE.2).OR.
     -           ARGREF(NARG,1).GE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for GET_CLUSTER; no cluster.'
                 RETURN
            ENDIF
*   Clean up space associated with the arguments.
            DO 40 ISTR=1,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
40          CONTINUE
*   Ask for a new cluster.
            CALL TRACLS(XCLS,YCLS,ZCLS,ECLS,NPAIR,EXTRA1,DONE,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Error ocurred'//
     -                ' during cluster generation; no data returned.'
                 RETURN
            ENDIF
*   Return the cluster position.
            ARG(1)=XCLS
            ARG(2)=YCLS
            ARG(3)=ZCLS
            MODARG(1)=2
            MODARG(2)=2
            MODARG(3)=2
*   Return the cluster size.
            ARG(4)=REAL(NPAIR)
            MODARG(4)=2
*   Return the cluster energy.
            ARG(5)=ECLS
            MODARG(5)=2
*   If there are extra fields, return them as 6th field.
            IF(NARG.GE.7)THEN
                 ARG(6)=EXTRA1
                 MODARG(6)=2
            ENDIF
*   Set the flag whether to continue or not.
            IF(DONE)THEN
                 ARG(NARG)=1
            ELSE
                 ARG(NARG)=0
            ENDIF
            MODARG(NARG)=3
*** Drift line calculation for electrons.
       ELSEIF(IPROC.EQ.-503)THEN
*   Check number of arguments.
            IF(NARG.LT.2.OR.NARG.GT.7)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_ELECTRON.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_ELECTRON are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_ELECTRON can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 270 ISTR=3,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
270         CONTINUE
*   Carry out the calculation.
            CALL DLCALC(ARG(1),ARG(2),0.0,-1.0,1)
*   Return status code.
            IF(NARG.GE.3)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(3)=REAL(IAUX)
                 MODARG(3)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_ELECTRON.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(4)=TU(NU)
            ELSE
                 ARG(4)=0.0
            ENDIF
            IF(NARG.GE.5)CALL DLCDIF(ARG(5))
            IF(NARG.GE.6)CALL DLCTWN(ARG(6))
            IF(NARG.GE.7)CALL DLCATT(ARG(7))
            MODARG(4)=2
            MODARG(5)=2
            MODARG(6)=2
            MODARG(7)=2
*** Drift line calculation for positrons.
       ELSEIF(IPROC.EQ.-521)THEN
*   Check number of arguments.
            IF(NARG.LT.2.OR.NARG.GT.4)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_POSITRON.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_POSITRON are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_POSITRON can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 300 ISTR=3,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
300         CONTINUE
*   Carry out the calculation.
            CALL DLCALC(ARG(1),ARG(2),0.0,+1.0,1)
*   Return status code.
            IF(NARG.GE.3)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(3)=REAL(IAUX)
                 MODARG(3)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_POSITRON.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(4)=TU(NU)
            ELSE
                 ARG(4)=0.0
            ENDIF
            MODARG(4)=2
*** Drift line calculation for ions.
       ELSEIF(IPROC.EQ.-504.OR.IPROC.EQ.-514)THEN
*   Check number of arguments.
            IF(NARG.LT.2.OR.NARG.GT.4)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_ION.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_ION are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_ION can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The mobility'//
     -                ' for ions is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            IF(NARG.GE.3)CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            IF(NARG.GE.4)CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Carry out the calculation.
            IF(IPROC.EQ.-504)THEN
                 CALL DLCALC(ARG(1),ARG(2),0.0,+1.0,2)
            ELSE
                 CALL DLCALC(ARG(1),ARG(2),0.0,-1.0,2)
            ENDIF
*   Return status code.
            IF(NARG.GE.3)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(3)=REAL(IAUX)
                 MODARG(3)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_ION.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(4)=TU(NU)
            ELSE
                 ARG(4)=0.0
            ENDIF
            MODARG(4)=2
*** 3D Drift line calculation for electrons.
       ELSEIF(IPROC.EQ.-505)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.8)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_ELECTRON_3.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_ELECTRON_3 are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_ELECTRON_3 can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 280 ISTR=4,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
280         CONTINUE
*   Carry out the calculation.
            CALL DLCALC(ARG(1),ARG(2),ARG(3),-1.0,1)
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_ELECTRON_3.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            IF(NARG.GE.6)CALL DLCDIF(ARG(6))
            IF(NARG.GE.7)CALL DLCTWN(ARG(7))
            IF(NARG.GE.8)CALL DLCATT(ARG(8))
            MODARG(5)=2
            MODARG(6)=2
            MODARG(7)=2
            MODARG(8)=2
*** 3D Drift line calculation for positrons.
       ELSEIF(IPROC.EQ.-522)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.5)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_POSITRON_3.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_POSITRON_3 are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_POSITRON_3 can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 310 ISTR=4,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
310         CONTINUE
*   Carry out the calculation.
            CALL DLCALC(ARG(1),ARG(2),ARG(3),+1.0,1)
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_POSITRON_3.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            MODARG(5)=2
*** 3D Drift line calculation for ions.
       ELSEIF(IPROC.EQ.-506.OR.IPROC.EQ.-515)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.5)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_ION_3.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_ION_3 are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_ION_3 can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The mobility'//
     -                ' for ions is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            IF(NARG.GE.4)CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            IF(NARG.GE.5)CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
*   Carry out the calculation.
            IF(IPROC.EQ.-506)THEN
                 CALL DLCALC(ARG(1),ARG(2),ARG(3),+1.0,2)
            ELSE
                 CALL DLCALC(ARG(1),ARG(2),ARG(3),-1.0,2)
            ENDIF
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_ION_3.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            MODARG(5)=2
*** Get the drift line.
       ELSEIF(IPROC.EQ.-507)THEN
*   Check the arguments.
            IF(NARG.LT.1.OR.NARG.GT.4.OR.
     -           (NARG.GE.1.AND.ARGREF(1,1).GE.2).OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2).OR.
     -           (NARG.GE.3.AND.ARGREF(3,1).GE.2).OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect argument'//
     -                ' list for GET_DRIFT_LINE.'
                 RETURN
            ELSEIF(ISTAT.EQ.0.OR.NU.LT.1)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : No drift line in'//
     -                ' memory currently.'
                 RETURN
            ENDIF
*   Clear the arguments.
            IF(NARG.GE.1)CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
            IF(NARG.GE.2)CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            IF(NARG.GE.3)CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            IF(NARG.GE.4)CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Allocate matrices.
            ISIZ(1)=NU
            IF(NARG.GE.1)THEN
                 CALL MATADM('ALLOCATE',IRX,1,ISIZ,2,IFAIL1)
            ELSE
                 IFAIL1=0
            ENDIF
            IF(NARG.GE.2)THEN
                 CALL MATADM('ALLOCATE',IRY,1,ISIZ,2,IFAIL2)
            ELSE
                 IFAIL2=0
            ENDIF
            IF(NARG.GE.3)THEN
                 CALL MATADM('ALLOCATE',IRZ,1,ISIZ,2,IFAIL3)
            ELSE
                 IFAIL3=0
            ENDIF
            IF(NARG.GE.4)THEN
                 CALL MATADM('ALLOCATE',IRT,1,ISIZ,2,IFAIL4)
            ELSE
                 IFAIL4=0
            ENDIF
            IF(NARG.GE.1)THEN
                 ISX=MATSLT(IRX)
            ELSE
                 ISX=1
            ENDIF
            IF(NARG.GE.2)THEN
                 ISY=MATSLT(IRY)
            ELSE
                 ISY=1
            ENDIF
            IF(NARG.GE.3)THEN
                 ISZ=MATSLT(IRZ)
            ELSE
                 ISZ=1
            ENDIF
            IF(NARG.GE.4)THEN
                 IST=MATSLT(IRT)
            ELSE
                 IST=1
            ENDIF
            IF(IFAIL1.NE.0.OR.ISX.LE.0.OR.IFAIL2.NE.0.OR.ISY.LE.0.OR.
     -           IFAIL3.NE.0.OR.ISZ.LE.0.OR.IFAIL4.NE.0.OR.IST.LE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to allocate'//
     -                ' output matrices for GET_DRIFT_LINE.'
                 RETURN
            ENDIF
*   Copy the vectors.
            DO 10 I=1,NU
            IF(NARG.GE.1)MVEC(MORG(ISX)+I)=REAL(XU(I))
            IF(NARG.GE.2)MVEC(MORG(ISY)+I)=REAL(YU(I))
            IF(NARG.GE.3)MVEC(MORG(ISZ)+I)=REAL(ZU(I))
            IF(NARG.GE.4)MVEC(MORG(IST)+I)=REAL(TU(I))
10          CONTINUE
*   Save the vectors.
            IF(NARG.GE.1)THEN
                 ARG(1)=IRX
                 MODARG(1)=5
            ENDIF
            IF(NARG.GE.2)THEN
                 ARG(2)=IRY
                 MODARG(2)=5
            ENDIF
            IF(NARG.GE.3)THEN
                 ARG(3)=IRZ
                 MODARG(3)=5
            ENDIF
            IF(NARG.GE.4)THEN
                 ARG(4)=IRT
                 MODARG(4)=5
            ENDIF
*** 3D MC drift line calculation for electrons.
       ELSEIF(IPROC.EQ.-508)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.7)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_ELECTRON_MC.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_ELECTRON_MC are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_ELECTRON_MC can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 20 ISTR=4,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
20          CONTINUE
*   Carry out the calculation.
            CALL DLCMC(ARG(1),ARG(2),ARG(3),-1.0,1)
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_ELECTRON_MC.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            IF(NARG.GE.6)CALL DLCTWN(ARG(6))
            IF(NARG.GE.7)CALL DLCATT(ARG(7))
            MODARG(5)=2
            MODARG(6)=2
            MODARG(7)=2
*** 3D MC test drift line calculation for electrons.
       ELSEIF(IPROC.EQ.-519)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.7)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_MC_TEST.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_MC_TEST are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_MC_TEST can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 21 ISTR=4,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
21          CONTINUE
*   Carry out the calculation.
            CALL DLCMC3(ARG(1),ARG(2),ARG(3),-1.0,1)
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_MC_TEST.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            IF(NARG.GE.6)CALL DLCTWN(ARG(6))
            IF(NARG.GE.7)CALL DLCATT(ARG(7))
            MODARG(5)=2
            MODARG(6)=2
            MODARG(7)=2
*** 3D MC drift line calculation for electrons.
       ELSEIF(IPROC.EQ.-523)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.5)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_MC_POSITRON.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_MC_POSITRON are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_MC_POSITRON can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 320 ISTR=4,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
320         CONTINUE
*   Carry out the calculation.
            CALL DLCMC(ARG(1),ARG(2),ARG(3),+1.0,1)
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_MC_POSITRON.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            MODARG(5)=2
*** 3D MC drift line calculation for ions.
       ELSEIF(IPROC.EQ.-509.OR.IPROC.EQ.-516)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.GT.5)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_ION_MC.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_ION_MC are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_ION_MC can not be modified.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The mobility'//
     -                ' for ions is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
*   Carry out the calculation.
            IF(IPROC.EQ.-509)THEN
                 CALL DLCMC(ARG(1),ARG(2),ARG(3),+1.0,2)
            ELSE
                 CALL DLCMC(ARG(1),ARG(2),ARG(3),-1.0,2)
            ENDIF
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_ION_MC.'
            ENDIF
*   Compute and return requested numerical data.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            MODARG(5)=2
*** Drift line calculation in vacuum for electrons.
       ELSEIF(IPROC.EQ.-517)THEN
*   Check number of arguments.
            IF(NARG.LT.6.OR.NARG.GT.8)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_VACUUM_ELECTRON.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -           MODARG(5).NE.2.OR.MODARG(6).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_VACUUM_ELECTRON are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_VACUUM_ELECTRON can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 290 ISTR=7,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
290         CONTINUE
*   Carry out the calculation.
            CALL DLCVAC(ARG(1),ARG(2),ARG(3),ARG(4),ARG(5),ARG(6),
     -           -1.0,EMASS/MEV2KG)
*   Return status code.
            IF(NARG.GE.7)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(7)=REAL(IAUX)
                 MODARG(7)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing a string for'//
     -                ' DRIFT_VACUUM_ELECTRON.'
            ENDIF
*   Return drift time.
            IF(NARG.GE.8)THEN
                 IF(NU.GE.1)THEN
                      ARG(8)=TU(NU)
                 ELSE
                      ARG(8)=0.0
                 ENDIF
                 MODARG(8)=2
            ENDIF
*** Drift line calculation in vacuum for ions.
       ELSEIF(IPROC.EQ.-518)THEN
*   Check number of arguments.
            IF(NARG.LT.8.OR.NARG.GT.10)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_VACUUM_ION.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -           MODARG(5).NE.2.OR.MODARG(6).NE.2.OR.
     -           MODARG(7).NE.2.OR.MODARG(8).NE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_VACUUM_ION are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.9.AND.ARGREF(9,1).GE.2).OR.
     -           (NARG.GE.10.AND.ARGREF(10,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_VACUUM_ION can not be modified.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 291 ISTR=9,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
291         CONTINUE
*   Carry out the calculation.
            CALL DLCVAC(ARG(1),ARG(2),ARG(3),ARG(4),ARG(5),ARG(6),
     -           ARG(7),ARG(8))
*   Return status code.
            IF(NARG.GE.9)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(9)=REAL(IAUX)
                 MODARG(9)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing a string for'//
     -                ' DRIFT_VACUUM_ION.'
            ENDIF
*   Return drift time.
            IF(NARG.GE.10)THEN
                 IF(NU.GE.1)THEN
                      ARG(10)=TU(NU)
                 ELSE
                      ARG(10)=0.0
                 ENDIF
                 MODARG(10)=2
            ENDIF
*** Plot the drift line.
       ELSEIF(IPROC.EQ.-510)THEN
*   There are no arguments for this procedure.
            IF(NARG.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -           ' PLOT_DRIFT_LINE has no arguments; ignored.'
*   Save the drift line.
            CALL DLCBCK('SAVE')
*   Plot the requested projection.
            CALL DLCPLT
*   Restore the drift line.
            CALL DLCBCK('RESTORE')
*** Plot the track.
       ELSEIF(IPROC.EQ.-511)THEN
*   Warn if there are arguments.
            IF(NARG.NE.0)PRINT *,' !!!!!! DLCCAL WARNING : The'//
     -           '  PLOT_TRACK procedure has no arguments; ignored.'
*   Plot the track.
            CALL TRAPLT
*** 3D MC drift line calculation for electrons with avalanche.
       ELSEIF(IPROC.EQ.-512)THEN
**  Check number of arguments.
            IF(NARG.LT.3.OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.NARG.NE.2*(NARG/2)).OR.
     -           NARG.GT.6+2*MXAHIS)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect list of'//
     -                ' arguments for AVALANCHE; not executed'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
*   Make sure there are Townsend coefficients.
            ELSEIF(.NOT.GASOK(4))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The Townsend'//
     -                ' coefficient is not defined ; not executed.'
                 RETURN
            ENDIF
**  Fetch the option string.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),OPT,NCOPT,IFAIL1)
                 CALL CLTOU(OPT(1:NCOPT))
            ELSE
                 OPT=' '
                 NCOPT=1
            ENDIF
**  Liberate storage associated with the electron and ion count.
            IF(NARG.GE.5)CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            IF(NARG.GE.6)CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
**  Create the entry point for the histogram formulae.
            IF(NARG.GE.7)THEN
*   Initialise the usage list.
                 STAT(1)=.FALSE.
                 STAT(2)=.FALSE.
                 STAT(3)=.FALSE.
                 STAT(4)=.FALSE.
*   Establish the variable list.
                 IF(POLAR)THEN
                      VARLIS(1)= 'R_CREATED'
                      VARLIS(5)= 'R_LOST'
                      VARLIS(9)= 'R_E'
                      VARLIS(13)='R_ION'
                      VARLIS(2)= 'PHI_CREATED'
                      VARLIS(6)= 'PHI_LOST'
                      VARLIS(10)='PHI_E'
                      VARLIS(14)='PHI_ION'
                 ELSE
                      VARLIS(1)= 'X_CREATED'
                      VARLIS(5)= 'X_LOST'
                      VARLIS(9)= 'X_E'
                      VARLIS(13)='X_ION'
                      VARLIS(2)= 'Y_CREATED'
                      VARLIS(6)= 'Y_LOST'
                      VARLIS(10)='Y_E'
                      VARLIS(14)='Y_ION'
                 ENDIF
                 VARLIS(3)= 'Z_CREATED'
                 VARLIS(7)= 'Z_LOST'
                 VARLIS(11)='Z_E'
                 VARLIS(15)='Z_ION'
                 VARLIS(4)= 'T_CREATED'
                 VARLIS(8)= 'T_LOST'
                 VARLIS(12)='T_E'
                 VARLIS(16)='T_ION'
*   Number of histograms.
                 NHIST=NARG/2-3
*   Loop over the histograms.
                 DO 30 I=1,NHIST
*   Fetch the histogram string.
                 CALL STRBUF('READ',NINT(ARG(5+2*I)),TITLE,NC,IFAIL1)
                 IF(IFAIL1.NE.0.OR.NC.LT.1)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Unable to get'//
     -                     ' an histogram formula; no avalanche.'
                      RETURN
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
*   Translate the formula.
                 CALL ALGPRE(TITLE(1:NC),NC,VARLIS,16,NREXP,USE,
     -                IENTRY(I),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : The histogram'//
     -                     ' function '//TITLE(1:NC)//' can not be'//
     -                     ' translated; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(NREXP.LT.1.OR.NREXP.GT.2)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : The histogram'//
     -                     ' function '//TITLE(1:NC)//' does not'//
     -                     ' return 1 or 2 results; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ENDIF
                 ITYPE(2,I)=NREXP
*   Work out which quantities are to be computed.
                 ITYPE(1,I)=0
                 IF((USE( 1).OR.USE( 2).OR.USE( 3).OR.USE( 4)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE( 1).OR.USE( 2).OR.USE( 3).OR.USE( 4))THEN
                      ITYPE(1,I)=1
                 ENDIF
                 IF((USE( 5).OR.USE( 6).OR.USE( 7).OR.USE( 8)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE( 5).OR.USE( 6).OR.USE( 7).OR.USE( 8))THEN
                      ITYPE(1,I)=2
                 ENDIF
                 IF((USE( 9).OR.USE(10).OR.USE(11).OR.USE(12)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE( 9).OR.USE(10).OR.USE(11).OR.USE(12))THEN
                      ITYPE(1,I)=3
                 ENDIF
                 IF((USE(13).OR.USE(14).OR.USE(15).OR.USE(16)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE(13).OR.USE(14).OR.USE(15).OR.USE(16))THEN
                      ITYPE(1,I)=4
                 ENDIF
                 IF(ITYPE(1,I).EQ.0)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses no'//
     -                     ' variables; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ENDIF
                 STAT(1)=STAT(1).OR.(ITYPE(1,I).EQ.1)
                 STAT(2)=STAT(2).OR.(ITYPE(1,I).EQ.2)
                 STAT(3)=STAT(3).OR.(ITYPE(1,I).EQ.3)
                 STAT(4)=STAT(4).OR.(ITYPE(1,I).EQ.4)
*   Generate the histogram index list and check the number.
                 IF(ARGREF(6+2*I,1).GE.2)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Histogram'//
     -                     ' argument ',I,' can not be modified;'//
     -                     ' no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(MODARG(6+2*I).EQ.4)THEN
                      IHIST(I)=NINT(ARG(6+2*I))
                 ELSE
                      CALL ALGREU(NINT(ARG(6+2*I)),MODARG(6+2*I),
     -                     ARGREF(6+2*I,1))
                      CALL HISADM('ALLOCATE',IHIST(I),100,0.0,0.0,
     -                     .TRUE.,IFAIL1)
                 ENDIF
30               CONTINUE
*   No histograms to be made.
            ELSE
                 STAT(1)=.FALSE.
                 STAT(2)=.FALSE.
                 STAT(3)=.FALSE.
                 STAT(4)=.FALSE.
                 NHIST=0
            ENDIF
**  Carry out the calculation.
            CALL DLCMCA(ARG(1),ARG(2),ARG(3),NETOT,NITOT,
     -           STAT,NHIST,IHIST,ITYPE,IENTRY,OPT(1:NCOPT))
*   Print algebra errors if there were any.
            CALL ALGERR
**  Return the arguments and delete the instruction lists.
            IF(NARG.GE.5)THEN
                 ARG(5)=REAL(NETOT)
                 MODARG(5)=2
            ENDIF
            IF(NARG.GE.6)THEN
                 ARG(6)=REAL(NITOT)
                 MODARG(6)=2
            ENDIF
            DO 50 I=1,NHIST
            ARG(6+2*I)=REAL(IHIST(I))
            MODARG(6+2*I)=4
            CALL ALGCLR(IENTRY(I))
50          CONTINUE
*** Plot the drift area.
       ELSEIF(IPROC.EQ.-513)THEN
*   Check arguments.
            IF((NARG.NE.0.AND.NARG.NE.1).OR.
     -           (NARG.EQ.1.AND.MODARG(1).NE.1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect list'//
     -                ' of arguments for PLOT_DRIFT_AREA; no plot.'
                 RETURN
            ENDIF
*   See whether there is a title.
            IF(NARG.EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(1)),TITLE,NC,IFAIL1)
            ELSEIF(CELLID.EQ.' ')THEN
                 TITLE='Layout of the cell'
                 NC=18
            ELSE
                 TITLE=CELLID
                 NC=LEN(CELLID)
            ENDIF
*   Plot the frame.
            CALL GRASET(DXMIN,DYMIN,DZMIN,DXMAX,DYMAX,DZMAX)
            CALL GRCELL(VXMIN,VYMIN,VXMAX,VYMAX,TITLE(1:NC))
            IF(GASID.NE.' ')CALL GRCOMM(2,'Gas: '//GASID)
*** Return the status code and other pieces of information.
       ELSEIF(IPROC.EQ.-520)THEN
            IF(NARG.LT.2.OR.2*(NARG/2).NE.NARG)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : DRIFT_INFORMATION'//
     -                ' received an odd number of arguments;'//
     -                ' procedure not called.'
                 RETURN
            ELSEIF(NU.LT.1)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The current'//
     -                ' drift line has no steps; DRIFT_INFORMATION'//
     -                ' not executed.'
                 RETURN
            ENDIF
*   Loop over the options.
            DO 80 I=1,NARG-1,2
*   Check the argument type.
            IF(MODARG(I).NE.1)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Argument ',I,' of',
     -                ' DRIFT_INFORMATION is not of type String; no',
     -                ' value returned.'
                 GOTO 80
            ENDIF
*   Fetch option.
            CALL STRBUF('READ',NINT(ARG(I)),TITLE,NC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Error retrieving'//
     -                ' the DRIFT_INFORMATION option.'
                 GOTO 80
            ENDIF
            IF(NC.GE.1)CALL CLTOU(TITLE(1:NC))
*   Check we can return a value.
            IF(ARGREF(I+1,1).GE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Can not return'//
     -                ' a value for '//TITLE(1:NC)//' because the'//
     -                ' following argument is not modifiable.'
                 GOTO 80
            ENDIF
*   Delete old contents of return variable.
            CALL ALGREU(NINT(ARG(I+1)),MODARG(I+1),ARGREF(I+1,1))
*   Total drift time.
            IF(INPCMX(TITLE(1:NC),'DR#IFT-T#IME')+
     -           INPCMX(TITLE(1:NC),'TIME').NE.0)THEN
                 IF(NU.GE.1)THEN
                      ARG(I+1)=TU(NU)
                 ELSE
                      ARG(I+1)=0.0
                 ENDIF
                 MODARG(I+1)=2
*   Charge of the particle.
            ELSEIF(INPCMX(TITLE(1:NC),'CHA#RGE').NE.0)THEN
                 ARG(I+1)=QPCHAR
                 MODARG(I+1)=2
*   Particle being drifted.
            ELSEIF(INPCMX(TITLE(1:NC),'PART#ICLE').NE.0)THEN
                 IF(IPTYPE.EQ.1)THEN
                      CALL STRBUF('STORE',IAUX,'electron',8,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ELSEIF(IPTYPE.EQ.2)THEN
                      CALL STRBUF('STORE',IAUX,'ion',3,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ELSE
                      CALL STRBUF('STORE',IAUX,'unknown',7,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ENDIF
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the DRIFT_INFORMATION result.'
*   Integration technique used.
            ELSEIF(INPCMX(TITLE(1:NC),'TECH#NIQUE').NE.0)THEN
                 IF(IPTECH.EQ.1)THEN
                      CALL STRBUF('STORE',IAUX,'Runge-Kutta-Fehlberg',
     -                     20,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ELSEIF(IPTECH.EQ.2)THEN
                      CALL STRBUF('STORE',IAUX,'Monte-Carlo',11,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ELSEIF(IPTECH.EQ.3)THEN
                      CALL STRBUF('STORE',IAUX,'vacuum',6,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ELSEIF(IPTECH.EQ.4)THEN
                      CALL STRBUF('STORE',IAUX,'microscopic',11,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ELSE
                      CALL STRBUF('STORE',IAUX,'unknown',7,IFAIL1)
                      ARG(I+1)=REAL(IAUX)
                      MODARG(I+1)=1
                 ENDIF
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the DRIFT_INFORMATION result.'
*   Numeric status code.
            ELSEIF(INPCMX(TITLE(1:NC),'STAT#US-#CODE').NE.0)THEN
                 ARG(I+1)=REAL(ISTAT)
                 MODARG(I+1)=2
*   Electrode group.
            ELSEIF(INPCMX(TITLE(1:NC),'ELEC#TRODE').NE.0)THEN
                 CALL DLCISW(ISTAT,ISW)
                 ARG(I+1)=REAL(ISW)
                 MODARG(I+1)=2
*   String status code.
            ELSEIF(INPCMX(TITLE(1:NC),'STAT#US-#STRING').NE.0)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(I+1)=REAL(IAUX)
                 MODARG(I+1)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the DRIFT_INFORMATION result.'
*   Number of steps.
            ELSEIF(INPCMX(TITLE(1:NC),'STEP#S').NE.0)THEN
                 ARG(I+1)=REAL(NU)
                 MODARG(I+1)=2
*   Path length.
            ELSEIF(INPCMX(TITLE(1:NC),'PATH-#LENGTH')+
     -           INPCMX(TITLE(1:NC),'LENGTH').NE.0)THEN
                 ARG(I+1)=0.0
                 DO 70 J=2,NU
                 IF(POLAR)THEN
                      CALL CF2RTC(XU(J-1),YU(J-1),XPOS1,YPOS1,1)
                      CALL CF2RTC(XU(J)  ,YU(J)  ,XPOS2,YPOS2,1)
                      ARG(I+1)=ARG(I+1)+SQRT((XPOS2-XPOS1)**2+
     -                     (YPOS2-YPOS1)**2+(ZU(J)-ZU(J-1))**2)
                 ELSE
                      ARG(I+1)=ARG(I+1)+SQRT((XU(J)-XU(J-1))**2+
     -                     (YU(J)-YU(J-1))**2+(ZU(J)-ZU(J-1))**2)
                 ENDIF
70               CONTINUE
                 MODARG(I+1)=2
*   Start/end points.
            ELSEIF(INPCMX(TITLE(1:NC),'X-ST#ART')+
     -           INPCMX(TITLE(1:NC),'X_ST#ART')+
     -           INPCMX(TITLE(1:NC),'XST#ART').NE.0)THEN
                 ARG(I+1)=XU(1)
                 MODARG(I+1)=2
            ELSEIF(INPCMX(TITLE(1:NC),'X-END')+
     -           INPCMX(TITLE(1:NC),'X_END')+
     -           INPCMX(TITLE(1:NC),'XEND').NE.0)THEN
                 IF(NU.GE.1)THEN
                      ARG(I+1)=XU(NU)
                 ELSE
                      ARG(I+1)=0.0
                 ENDIF
                 MODARG(I+1)=2
            ELSEIF(INPCMX(TITLE(1:NC),'Y-START')+
     -           INPCMX(TITLE(1:NC),'Y_ST#ART')+
     -           INPCMX(TITLE(1:NC),'YST#ART').NE.0)THEN
                 ARG(I+1)=YU(1)
                 MODARG(I+1)=2
            ELSEIF(INPCMX(TITLE(1:NC),'Y-END')+
     -           INPCMX(TITLE(1:NC),'Y_END')+
     -           INPCMX(TITLE(1:NC),'YEND').NE.0)THEN
                 IF(NU.GE.1)THEN
                      ARG(I+1)=YU(NU)
                 ELSE
                      ARG(I+1)=0.0
                 ENDIF
                 MODARG(I+1)=2
            ELSEIF(INPCMX(TITLE(1:NC),'Z-START')+
     -           INPCMX(TITLE(1:NC),'Z_ST#ART')+
     -           INPCMX(TITLE(1:NC),'ZST#ART').NE.0)THEN
                 ARG(I+1)=ZU(1)
                 MODARG(I+1)=2
            ELSEIF(INPCMX(TITLE(1:NC),'Z-END')+
     -           INPCMX(TITLE(1:NC),'Z_END')+
     -           INPCMX(TITLE(1:NC),'ZEND').NE.0)THEN
                 IF(NU.GE.1)THEN
                      ARG(I+1)=ZU(NU)
                 ELSE
                      ARG(I+1)=0.0
                 ENDIF
                 MODARG(I+1)=2
*   Unknown things.
            ELSE
                 PRINT *,' !!!!!! DLCCAL WARNING : Unknown item "'//
     -                TITLE(1:NC)//'" received ; no return value.'
            ENDIF
80          CONTINUE
*** Return the status code and other pieces of information.
       ELSEIF(IPROC.EQ.-535)THEN
            IF(NMCA.LT.1)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The current'//
     -                ' avalanche is empty; AVALANCHE_INFORMATION'//
     -                ' not executed.'
                 RETURN
            ENDIF
**  Loop over the options.
            INEXT=1
            DO 90 I=1,NARG
            IF(I.LT.INEXT)GOTO 90
*   Check the argument type.
            IF(MODARG(I).NE.1)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Argument ',I,' of'//
     -                ' AVALANCHE_INFORMATION is not of type String;'//
     -                ' no value returned.'
                 GOTO 90
            ENDIF
*   Fetch the option.
            CALL STRBUF('READ',NINT(ARG(I)),TITLE,NC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Error retrieving'//
     -                ' the AVALANCHE_INFORMATION option.'
                 GOTO 90
            ENDIF
            IF(NC.GE.1)THEN
                 CALL CLTOU(TITLE(1:NC))
            ELSE
                 PRINT *,' !!!!!! DLCCAL WARNING : Retrieving empty'//
     -                ' AVALANCHE_INFORMATION option; ignored.'
                 GOTO 90
            ENDIF
**  Number of electrons
            IF(INPCMX(TITLE(1:NC),'ELEC#TRONS').NE.0)THEN
                 IF(I+1.GT.NARG)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Not enough'//
     -                     ' arguments for AVALANCHE_INFORMATION;'//
     -                     ' data not returned.'
                      GOTO 90
                 ELSEIF(ARGREF(I+1,1).GE.2)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Can not'//
     -                     ' return a value for '//TITLE(1:NC)//
     -                     ' because the following argument is not'//
     -                     ' modifiable.'
                      GOTO 90
                 ENDIF
*   Delete old contents of return variable.
                 CALL ALGREU(NINT(ARG(I+1)),MODARG(I+1),ARGREF(I+1,1))
*   Store the value
                 ARG(I+1)=REAL(NMCA)
                 MODARG(I+1)=2
*   Next field
                 INEXT=I+2
**  Starting and end points: get the electron number
            ELSEIF(INPCMX(TITLE(1:NC),'X-ST#ART')+
     -           INPCMX(TITLE(1:NC),'Y-ST#ART')+
     -           INPCMX(TITLE(1:NC),'Z-ST#ART')+
     -           INPCMX(TITLE(1:NC),'T-ST#ART')+
     -           INPCMX(TITLE(1:NC),'X-END')+
     -           INPCMX(TITLE(1:NC),'Y-END')+
     -           INPCMX(TITLE(1:NC),'Z-END')+
     -           INPCMX(TITLE(1:NC),'T-END')+
     -           INPCMX(TITLE(1:NC),'ST#ATUS-#CODE')+
     -           INPCMX(TITLE(1:NC),'ST#ATUS-#STRING').NE.0)THEN
                 IF(I+2.GT.NARG)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Not enough'//
     -                     ' arguments for AVALANCHE_INFORMATION;'//
     -                     ' data not returned.'
                      GOTO 90
                 ELSEIF(MODARG(I+1).NE.2)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Argument ',I+1,
     -                     ' of AVALANCHE_INFORMATION not a number;'//
     -                     ' data not returned.'
                      GOTO 90
                 ELSEIF(NINT(ARG(I+1)).LT.1.OR.
     -                NINT(ARG(I+1)).GT.NMCA)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Argument ',I+1,
     -                     ' of AVALANCHE_INFORMATION is out of'//
     -                     ' range; data not returned.'
                      GOTO 90
                 ELSEIF(ARGREF(I+2,1).GE.2)THEN
                      PRINT *,' !!!!!! DLCCAL WARNING : Can not'//
     -                     ' return a value for '//TITLE(1:NC)//
     -                     ' because the following argument is not'//
     -                     ' modifiable.'
                      GOTO 90
                 ENDIF
*   Delete old contents of return variable.
                 CALL ALGREU(NINT(ARG(I+2)),MODARG(I+2),ARGREF(I+2,1))
*   Return the desired value.
                 IF(INPCMX(TITLE(1:NC),'X-ST#ART').NE.0)THEN
                      ARG(I+2)=100.0*XLIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'Y-ST#ART').NE.0)THEN
                      ARG(I+2)=100.0*YLIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'Z-ST#ART').NE.0)THEN
                      ARG(I+2)=100.0*ZLIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'T-ST#ART').NE.0)THEN
                      ARG(I+2)=1.0E-6*TLIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'X-END').NE.0)THEN
                      ARG(I+2)=100.0*XELIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'Y-END').NE.0)THEN
                      ARG(I+2)=100.0*YELIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'Z-END').NE.0)THEN
                      ARG(I+2)=100.0*ZELIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'T-END').NE.0)THEN
                      ARG(I+2)=1.0E-6*TELIST(NINT(ARG(I+1)))
                      MODARG(I+2)=2
                 ELSEIF(INPCMX(TITLE(1:NC),'ST#ATUS-#STRING').NE.0)THEN
                      CALL DLCSTF(ISLIST(NINT(ARG(I+1))),OPT,NCOPT)
                      CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                      ARG(I+2)=REAL(IAUX)
                      MODARG(I+2)=1
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING'//
     -                     ' : Error storing the'//
     -                     ' AVALANCHE_INFORMATION result.'
                 ELSEIF(INPCMX(TITLE(1:NC),'ST#ATUS-#CODE').NE.0)THEN
                      ARG(I+2)=REAL(ISLIST(NINT(ARG(I+1))))
                      MODARG(I+2)=2
                 ENDIF
*   Set next I value
                 INEXT=I+3
            ELSE
                 PRINT *,' !!!!!! DLCCAL WARNING : Unknown option '//
     -                TITLE(1:NC)//' for AVALANCHE_INFORMATION;'//
     -                ' data not returned.'
                 GOTO 90
            ENDIF
90          CONTINUE
*** Interpolate in a track.
       ELSEIF(IPROC.EQ.-524)THEN
*   Check the arguments.
            IF(NARG.LT.4.OR.NARG.GT.9.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2).OR.
     -           (NARG.GE.8.AND.ARGREF(8,1).GE.2).OR.
     -           (NARG.GE.9.AND.ARGREF(9,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' INTERPOLATE_TRACK of wrong type or not'//
     -                ' modifiable; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 330 ISTR=4,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
330         CONTINUE
*   Perform the interpolation.
            CALL DLCTRI(ARG(1),ARG(2),ARG(3),
     -           ARG(5),ICL,ARG(6),ARG(7),ARG(8),ARG(9),
     -           NARG.GE.6,NARG.GE.7,NARG.GE.8,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Interpolating the'//
     -                ' track failed; no values returned.'
                 DO 340 ISTR=4,NARG
                 MODARG(ISTR)=0
340              CONTINUE
                 RETURN
            ENDIF
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ICL,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for INTERPOLATE_TRACK.'
            ENDIF
*   Set the modes of the arguments.
            MODARG(5)=2
            MODARG(6)=2
            MODARG(7)=2
            MODARG(8)=2
            MODARG(9)=2
*** Avalanche.
       ELSEIF(IPROC.EQ.-525)THEN
            IF(NARG.LT.1.OR.NARG.GT.2.OR.
     -           (NARG.GE.1.AND.ARGREF(1,1).GE.2).OR.
     -           (NARG.GE.2.AND.ARGREF(2,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for RND_MULTIPLICATION; not executed.'
                 RETURN
            ENDIF
*   Call the routine.
            CALL DLCMCT(NETOT,NITOT)
*   Clear the return space.
            DO 60 I=1,NARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
60          CONTINUE
*   Return the results.
            IF(NARG.GE.1)THEN
                 ARG(1)=REAL(NETOT)
                 MODARG(1)=2
            ENDIF
            IF(NARG.GE.2)THEN
                 ARG(2)=REAL(NITOT)
                 MODARG(2)=2
            ENDIF
*** Velocity vector for electrons.
       ELSEIF(IPROC.EQ.-526)THEN
            IF(NARG.LT.6.OR.NARG.GT.7.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2.OR.
     -           ARGREF(6,1).GE.2.OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for ELECTRON_VELOCITY; not executed.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Clear the return space.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
            IF(NARG.GE.7)CALL ALGREU(NINT(ARG(7)),MODARG(7),ARGREF(7,1))
*   Evaluate the velocity.
            CALL DLCVEL(DBLE(ARG(1)),DBLE(ARG(2)),DBLE(ARG(3)),
     -           F0,-1.0,1,ILOC)
*   Return the arguments.
            ARG(4)=REAL(F0(1))
            ARG(5)=REAL(F0(2))
            ARG(6)=REAL(F0(3))
            MODARG(4)=2
            MODARG(5)=2
            MODARG(6)=2
            IF(NARG.GE.7)THEN
                 IF(ILOC.EQ.-10)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Unknown potential',17,IFAIL1)
                 ELSEIF(ILOC.EQ.-5)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'In a material',13,IFAIL1)
                 ELSEIF(ILOC.EQ.-6)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Outside mesh',12,IFAIL1)
                 ELSEIF(ILOC.LT.0)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Outside plane',13,IFAIL1)
                 ELSEIF(ILOC.EQ.0)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Normal',6,IFAIL1)
                 ELSEIF(ILOC.LE.NWIRE)THEN
                      CALL OUTFMT(REAL(ILOC),2,AUX1,NC1,'LEFT')
                      CALL STRBUF('STORE',IAUX,'In '//WIRTYP(ILOC)//
     -                     '-wire '//AUX1(1:NC1),10+NC1,IFAIL1)
                 ELSEIF(ILOC.GT.2*MXWIRE.AND.
     -                ILOC.LE.2*MXWIRE+NSOLID)THEN
                      CALL OUTFMT(REAL(ILOC-2*MXWIRE),2,AUX1,NC1,'LEFT')
                      CALL STRBUF('STORE',IAUX,'In '//
     -                     SOLTYP(ILOC-2*MXWIRE)//'-solid '//
     -                     AUX1(1:NC1),11+NC1,IFAIL1)
                 ELSE
                      CALL STRBUF('STORE',IAUX,'Unknown',7,IFAIL1)
                 ENDIF
                 ARG(7)=REAL(IAUX)
                 MODARG(7)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING : '//
     -                'Error storing a string for ELECTRON_VELOCITY.'
            ENDIF
*** Velocity vector for ions.
       ELSEIF(IPROC.EQ.-527)THEN
            IF(NARG.LT.6.OR.NARG.GT.7.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2.OR.
     -           ARGREF(6,1).GE.2.OR.
     -           (NARG.GE.7.AND.ARGREF(7,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for ION_VELOCITY; not executed.'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for ions is not defined ; not executed.'
                 RETURN
            ENDIF
*   Clear the return space.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
            IF(NARG.GE.7)CALL ALGREU(NINT(ARG(7)),MODARG(7),ARGREF(7,1))
*   Evaluate the velocity.
            CALL DLCVEL(DBLE(ARG(1)),DBLE(ARG(2)),DBLE(ARG(3)),
     -           F0,+1.0,2,ILOC)
*   Return the arguments.
            ARG(4)=REAL(F0(1))
            ARG(5)=REAL(F0(2))
            ARG(6)=REAL(F0(3))
            MODARG(4)=2
            MODARG(5)=2
            MODARG(6)=2
            IF(NARG.GE.7)THEN
                 IF(ILOC.EQ.-10)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Unknown potential',17,IFAIL1)
                 ELSEIF(ILOC.EQ.-5)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'In a material',13,IFAIL1)
                 ELSEIF(ILOC.EQ.-6)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Outside mesh',12,IFAIL1)
                 ELSEIF(ILOC.LT.0)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Outside plane',13,IFAIL1)
                 ELSEIF(ILOC.EQ.0)THEN
                      CALL STRBUF('STORE',IAUX,
     -                     'Normal',6,IFAIL1)
                 ELSEIF(ILOC.LE.NWIRE)THEN
                      CALL OUTFMT(REAL(ILOC),2,AUX1,NC1,'LEFT')
                      CALL STRBUF('STORE',IAUX,'In '//WIRTYP(ILOC)//
     -                     '-wire '//AUX1(1:NC1),10+NC1,IFAIL1)
                 ELSEIF(ILOC.GT.2*MXWIRE.AND.
     -                ILOC.LE.2*MXWIRE+NSOLID)THEN
                      CALL OUTFMT(REAL(ILOC-2*MXWIRE),2,AUX1,NC1,'LEFT')
                      CALL STRBUF('STORE',IAUX,'In '//
     -                     SOLTYP(ILOC-2*MXWIRE)//'-solid '//
     -                     AUX1(1:NC1),11+NC1,IFAIL1)
                 ELSE
                      CALL STRBUF('STORE',IAUX,'Unknown',7,IFAIL1)
                 ENDIF
                 ARG(7)=REAL(IAUX)
                 MODARG(7)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING : '//
     -                'Error storing a string for ION_VELOCITY.'
            ENDIF
*** Print the current drift line.
       ELSEIF(IPROC.EQ.-528)THEN
*   Check arguments.
            IF(NARG.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -           ' PRINT_DRIFT_LINE takes no arguments ; arguments'//
     -           ' ignored.'
*   Print a header.
            WRITE(LUNOUT,'(''  CURRENT DRIFT LINE: ''/)')
            IF(IPTYPE.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Particle:  electron'')')
            ELSEIF(IPTYPE.EQ.2)THEN
                 WRITE(LUNOUT,'(''  Particle:  ion'')')
            ELSE
                 WRITE(LUNOUT,'(''  Particle:  not set'')')
            ENDIF
            CALL OUTFMT(QPCHAR,2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  Charge:    '',A)') AUX1(1:NC1)
            IF(IPTECH.EQ.1)THEN
                 WRITE(LUNOUT,'(''  Technique: Runge Kutta Fehlberg'')')
            ELSEIF(IPTECH.EQ.2)THEN
                 WRITE(LUNOUT,'(''  Technique: Monte Carlo'')')
            ELSEIF(IPTECH.EQ.3)THEN
                 WRITE(LUNOUT,'(''  Technique: vacuum drift'')')
            ELSE
                 WRITE(LUNOUT,'(''  Technique: not set'')')
            ENDIF
            CALL DLCSTF(ISTAT,OPT,NCOPT)
            WRITE(LUNOUT,'(''  Status:    '',A)') OPT(1:NCOPT)
            CALL OUTFMT(REAL(NU),2,AUX1,NC1,'LEFT')
            WRITE(LUNOUT,'(''  Steps:     '',A)') AUX1(1:NC1)
*   Print also the path, if non-zero.
            IF(NU.GT.0.AND.POLAR)THEN
                 WRITE(LUNOUT,'(/''  Path:''/
     -                16X,''r'',14X,''phi'',16X,''z'',13X,''time''/
     -                13X,''[cm]'',8X,''[degrees]'',
     -                13X,''[cm]'',7X,''[microsec]''/)')
            ELSEIF(NU.GT.0)THEN
                 WRITE(LUNOUT,'(/''  Path:''/
     -                16X,''x'',16X,''y'',16X,''z'',13X,''time''/
     -                13X,''[cm]'',13X,''[cm]'',
     -                13X,''[cm]'',7X,''[microsec]''/)')
            ENDIF
            DO 100 I=1,NU
            IF(POLAR)THEN
                 CALL CF2RTP(XU(I),YU(I),XPOS1,YPOS1,1)
            ELSE
                 XPOS1=XU(I)
                 YPOS1=YU(I)
            ENDIF
            CALL OUTFMT(REAL(XPOS1),2,AUX1,NC1,'RIGHT')
            CALL OUTFMT(REAL(YPOS1),2,AUX2,NC2,'RIGHT')
            CALL OUTFMT(REAL(ZU(I)),2,AUX3,NC3,'RIGHT')
            CALL OUTFMT(REAL(TU(I)),2,AUX4,NC4,'RIGHT')
            WRITE(LUNOUT,'(4(2X,A15))') AUX1,AUX2,AUX3,AUX4
100         CONTINUE
*** Drift field divergence.
       ELSEIF(IPROC.EQ.-529)THEN
*   Check the arguments.
            IF(NARG.LT.2.OR.NARG.GT.7.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2.OR.
     -           ARGREF(6,1).GE.2.OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for DRIFT_DIVERGENCE; not executed.'
                 RETURN
            ENDIF
*   Get the location.
            X0=ARG(1)
            Y0=ARG(2)
            Z0=ARG(3)
*   Retrieve the options.
            IF(NARG.GE.7)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),OPT,NC,IFAIL1)
            ELSE
                 OPT='electron,negative'
                 NC=17
            ENDIF
            CALL CLTOU(OPT(1:NC))
            Q=-1
            IPART=1
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to retrieve'//
     -                ' the options of  DRIFT_DIVERGENCE; assuming e-.'
            ELSE
                 IF(INDEX(OPT(1:NC),'ELEC').NE.0)IPART=1
                 IF(INDEX(OPT(1:NC),'ION').NE.0)IPART=2
                 IF(INDEX(OPT(1:NC),'POS').NE.0)Q=+1.0
                 IF(INDEX(OPT(1:NC),'NEG').NE.0)Q=-1.0
            ENDIF
*   Clear the return space.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
*   Call the procedure interface.
            CALL DLCDIC(X0,Y0,Z0,FDIV1,FDIV2,FDIV3,Q,IPART,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Error computing'//
     -                ' the divergence; set to 1.'
                 ARG(4)=1
                 ARG(5)=1
                 ARG(6)=1
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
            ELSE
                 ARG(4)=FDIV1
                 ARG(5)=FDIV2
                 ARG(6)=FDIV3
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
            ENDIF
*** Drift field rotation matrices.
       ELSEIF(IPROC.EQ.-530)THEN
*   Check the arguments.
            IF(NARG.LT.2.OR.NARG.GT.7.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2.OR.
     -           ARGREF(6,1).GE.2.OR.
     -           (NARG.GE.7.AND.MODARG(7).NE.1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for DRIFT_ROTATION; not executed.'
                 RETURN
            ENDIF
*   Get the location.
            X0=ARG(1)
            Y0=ARG(2)
            Z0=ARG(3)
*   Retrieve the options.
            IF(NARG.GE.7)THEN
                 CALL STRBUF('READ',NINT(ARG(7)),OPT,NC,IFAIL1)
            ELSE
                 OPT='electron,negative'
                 NC=17
            ENDIF
            CALL CLTOU(OPT(1:NC))
            Q=-1
            IPART=1
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to retrieve'//
     -                ' the options of  DRIFT_ROTATION; assuming e-.'
            ELSE
                 IF(INDEX(OPT(1:NC),'ELEC').NE.0)IPART=1
                 IF(INDEX(OPT(1:NC),'ION').NE.0)IPART=2
                 IF(INDEX(OPT(1:NC),'POS').NE.0)Q=+1.0
                 IF(INDEX(OPT(1:NC),'NEG').NE.0)Q=-1.0
            ENDIF
*   Clear the return space.
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
*   Call the procedure interface.
            CALL DLCROC(X0,Y0,Z0,Q,IPART,IRRVC,IRRMC,IRRVM,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Error computing'//
     -                ' the rotation matrices; set to Nill.'
                 ARG(4)=0
                 ARG(5)=0
                 ARG(6)=0
                 MODARG(4)=0
                 MODARG(5)=0
                 MODARG(6)=0
            ELSE
                 ARG(4)=REAL(IRRVC)
                 ARG(5)=REAL(IRRMC)
                 ARG(6)=REAL(IRRVM)
                 MODARG(4)=5
                 MODARG(5)=5
                 MODARG(6)=5
            ENDIF
*** Integrate excitations.
       ELSEIF(IPROC.EQ.-531)THEN
*   Check the arguments.
            IF(NARG.NE.1.OR.ARGREF(1,1).GE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for INTEGRATE_EXCITATIONS;'//
     -                ' not executed.'
                 RETURN
            ENDIF
*   Clear up old storage.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Call the procedure
            CALL DLCEX1(EXRATE)
            ISIZ(1)=NEXGAS
            CALL MATADM('ALLOCATE',IREXC,1,ISIZ,2,IFAIL1)
            ISEXC=MATSLT(IREXC)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to obtain'//
     -                ' matrix storage for excitation rates.'
                 RETURN
            ELSEIF(ISEXC.LE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to locate'//
     -                ' matrix storage for excitation rates.'
                 RETURN
            ELSE
                 DO 110 I=1,NEXGAS
                 MVEC(MORG(ISEXC)+I)=EXRATE(I)
110              CONTINUE
                 ARG(1)=IREXC
                 MODARG(1)=5
            ENDIF
*** Integrate ionisations.
       ELSEIF(IPROC.EQ.-532)THEN
*   Check the arguments.
            IF(NARG.NE.1.OR.ARGREF(1,1).GE.2)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect set of'//
     -                ' arguments for INTEGRATE_IONISATIONS;'//
     -                ' not executed.'
                 RETURN
            ENDIF
*   Clear up old storage.
            CALL ALGREU(NINT(ARG(1)),MODARG(1),ARGREF(1,1))
*   Call the procedure
            CALL DLCIO1(IORATE)
            ISIZ(1)=NIOGAS
            CALL MATADM('ALLOCATE',IRION,1,ISIZ,2,IFAIL1)
            ISION=MATSLT(IRION)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to obtain'//
     -                ' matrix storage for ionisation rates.'
                 RETURN
            ELSEIF(ISION.LE.0)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Unable to locate'//
     -                ' matrix storage for ionisation rates.'
                 RETURN
            ELSE
                 DO 120 I=1,NIOGAS
                 MVEC(MORG(ISION)+I)=IORATE(I)
120              CONTINUE
                 ARG(1)=IRION
                 MODARG(1)=5
            ENDIF
*** Mean free path microscopic tracking
       ELSEIF(IPROC.EQ.-533)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.EQ.10.OR.NARG.EQ.11.OR.
     -           NARG.GT.13)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for DRIFT_MICROSCOPIC.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -             MODARG(3).NE.2.OR.
     -           (NARG.GE. 6.AND.MODARG( 6).NE.1).OR.
     -           (NARG.GE. 7.AND.MODARG( 7).NE.2).OR.
     -           (NARG.GE. 8.AND.MODARG( 8).NE.2).OR.
     -           (NARG.GE. 9.AND.MODARG( 9).NE.2).OR.
     -           (NARG.GE.10.AND.MODARG(10).NE.2).OR.
     -           (NARG.GE.11.AND.MODARG(11).NE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' DRIFT_MICROSCOPIC are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE. 4.AND.ARGREF( 4,1).GE.2).OR.
     -             (NARG.GE. 5.AND.ARGREF( 5,1).GE.2).OR.
     -             (NARG.GE.12.AND.ARGREF(12,1).GE.2).OR.
     -             (NARG.GE.13.AND.ARGREF(13,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of DRIFT_MICROSCOPIC can not be modified.'
                 RETURN
*   Make sure there are drift velocities (for termination).
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 22 ISTR=4,NARG
            IF((ISTR.GE.6.AND.ISTR.LE.11).OR.ISTR.EQ.12)GOTO 22
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
22          CONTINUE
*   Fetch the option string.
            IF(NARG.GE.6)THEN
                 CALL STRBUF('READ',NINT(ARG(6)),OPT,NCOPT,IFAIL1)
                 CALL CLTOU(OPT(1:NCOPT))
            ELSE
                 OPT=' '
                 NCOPT=1
            ENDIF
*   Get hold of the energies and direction
            IF(NARG.GE.7)THEN
                 EFINAL=ARG(7)
            ELSE
                 EFINAL=5.0
            ENDIF
            IF(NARG.GE.8)THEN
                 ESTART=ARG(8)
            ELSE
                 ESTART=EFINAL/50.0
            ENDIF
            DIRX=0
            DIRY=0
            DIRZ=0
            IF(NARG.GE.9)THEN
                 DIRX=ARG(9)
                 DIRY=ARG(10)
                 DIRZ=ARG(11)
            ENDIF
            IF(NARG.LT.9.OR.DIRX**2+DIRY**2+DIRZ**2.LE.0)THEN
                 THETA0=2*PI*RNDUNI(1.0)
                 THETA1=ACOS(MIN(1.0,MAX(-1.0,1.0-2.0*RNDUNI(1.0))))
                 DIRX=COS(THETA1)
                 DIRY=SIN(THETA1)*COS(THETA0)
                 DIRZ=SIN(THETA1)*SIN(THETA0)
            ENDIF
*   Clear the histogram only if it isn't already an histogram.
            IF(NARG.GE.12.AND.MODARG(12).NE.4)THEN
                 CALL ALGREU(NINT(ARG(12)),MODARG(12),ARGREF(12,1))
                 IHDIST=-1
            ELSEIF(NARG.GE.12)THEN
                 IHDIST=NINT(ARG(12))
            ELSE
                 IHDIST=0
            ENDIF
*   Rate references.
            IF(NARG.GE.13)THEN
                 IRCS=1
            ELSE
                 IRCS=-1
            ENDIF
*   Carry out the calculation.
            CALL DLCMIC(ARG(1),ARG(2),ARG(3),OPT(1:NCOPT),NCOPT,
     -           EFINAL,ESTART,DIRX,DIRY,DIRZ,IRCS,IHDIST)
*   Return status code.
            IF(NARG.GE.4)THEN
                 CALL DLCSTF(ISTAT,OPT,NCOPT)
                 CALL STRBUF('STORE',IAUX,OPT,NCOPT,IFAIL1)
                 ARG(4)=REAL(IAUX)
                 MODARG(4)=1
                 IF(IFAIL1.NE.0)PRINT *,' !!!!!! DLCCAL WARNING :'//
     -                ' Error storing the status for DRIFT_MICROSCOPIC.'
            ENDIF
*   Return the time.
            IF(NU.GE.1)THEN
                 ARG(5)=TU(NU)
            ELSE
                 ARG(5)=0.0
            ENDIF
            MODARG(5)=2
*   Return the histogram.
            IF(NARG.GE.12)THEN
                 ARG(12)=IHDIST
                 MODARG(12)=4
            ENDIF
*   Return the rate vectors.
            IF(NARG.GE.13)THEN
                 ARG(13)=IRCS
                 MODARG(13)=5
            ENDIF
*** Mean free path microscopic avalanche.
       ELSEIF(IPROC.EQ.-534)THEN
*   Check number of arguments.
            IF(NARG.LT.3.OR.NARG.EQ.8.OR.NARG.EQ.9.OR.
     -           NARG.GT.14)THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Incorrect number'//
     -                ' of arguments for AVALANCHE_MICROSCOPIC.'
                 RETURN
*   Check argument mode.
            ELSEIF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.
     -             MODARG(3).NE.2.OR.
     -           (NARG.GE. 4.AND.MODARG( 4).NE.1).OR.
     -           (NARG.GE. 5.AND.MODARG( 5).NE.2).OR.
     -           (NARG.GE. 6.AND.MODARG( 6).NE.2).OR.
     -           (NARG.GE. 7.AND.MODARG( 7).NE.2).OR.
     -           (NARG.GE. 8.AND.MODARG( 8).NE.2).OR.
     -           (NARG.GE. 9.AND.MODARG( 9).NE.2).OR.
     -           (NARG.GE.14.AND.MODARG(14).NE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments of'//
     -                ' AVALANCHE_MICROSCOPIC are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF((NARG.GE.10.AND.ARGREF(10,1).GE.2).OR.
     -             (NARG.GE.11.AND.ARGREF(11,1).GE.2).OR.
     -             (NARG.GE.12.AND.ARGREF(12,1).GE.2).OR.
     -             (NARG.GE.13.AND.ARGREF(13,1).GE.2))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : Some arguments'//
     -                ' of AVALANCHE_MICROSCOPIC can not be modified.'
                 RETURN
*   Make sure there are drift velocities (for termination).
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! DLCCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
            ENDIF
*   Variables already in use ?
            DO 23 ISTR=11,NARG
            CALL ALGREU(NINT(ARG(ISTR)),MODARG(ISTR),ARGREF(ISTR,1))
23          CONTINUE
*   Fetch the option string.
            IF(NARG.GE.4)THEN
                 CALL STRBUF('READ',NINT(ARG(4)),OPT,NCOPT,IFAIL1)
                 CALL CLTOU(OPT(1:NCOPT))
            ELSE
                 OPT=' '
                 NCOPT=1
            ENDIF
*   Get hold of the energies and direction
            IF(NARG.GE.5)THEN
                 EFINAL=ARG(5)
            ELSE
                 EFINAL=200.0
            ENDIF
            IF(NARG.GE.6)THEN
                 ESTART=ARG(6)
            ELSE
                 ESTART=EFINAL/50.0
            ENDIF
            DIRX=0
            DIRY=0
            DIRZ=0
            IF(NARG.GE.7)THEN
                 DIRX=ARG(7)
                 DIRY=ARG(8)
                 DIRZ=ARG(9)
            ENDIF
            IF(NARG.LT.7.OR.DIRX**2+DIRY**2+DIRZ**2.LE.0)THEN
                 THETA0=2*PI*RNDUNI(1.0)
                 THETA1=ACOS(MIN(1.0,MAX(-1.0,1.0-2.0*RNDUNI(1.0))))
                 DIRX=COS(THETA1)
                 DIRY=SIN(THETA1)*COS(THETA0)
                 DIRZ=SIN(THETA1)*SIN(THETA0)
            ENDIF
*   Clear the histogram only if it isn't already an histogram.
            IF(NARG.GE.10.AND.MODARG(10).NE.4)THEN
                 CALL ALGREU(NINT(ARG(10)),MODARG(10),ARGREF(10,1))
                 IHDIST=-1
            ELSEIF(NARG.GE.10)THEN
                 IHDIST=NINT(ARG(10))
            ELSE
                 IHDIST=0
            ENDIF
*   Rate references.
            IF(NARG.GE.11)THEN
                 IRCS=1
            ELSE
                 IRCS=-1
            ENDIF
*   Delay
            IF(NARG.GE.14)THEN
                 DELAY=ARG(14)
            ELSE
                 DELAY=0
            ENDIF
*   Carry out the calculation.
            CALL DLCMIA(ARG(1),ARG(2),ARG(3),OPT(1:NCOPT),EFINAL,
     -           ESTART,DIRX,DIRY,DIRZ,IRCS,IHDIST,DELAY,
     -           NETOT,NITOT,IFAIL1)
*   Set the OK flag according to the IFAIL flag.
            IF(IFAIL1.NE.0)RETURN
*   Return the histogram.
            IF(NARG.GE.10)THEN
                 ARG(10)=IHDIST
                 MODARG(10)=4
            ENDIF
*   Return the rate vectors.
            IF(NARG.GE.11)THEN
                 ARG(11)=IRCS
                 MODARG(11)=5
            ENDIF
*   Return electron and ion count.
            IF(NARG.GE.12)THEN
                 ARG(12)=REAL(NETOT)
                 MODARG(12)=2
            ENDIF
            IF(NARG.GE.13)THEN
                 ARG(13)=REAL(NITOT)
                 MODARG(13)=2
            ENDIF
*** Unknown drift line operation.
       ELSE
            PRINT *,' !!!!!! DLCCAL WARNING : Unknown procedure code'//
     -           ' received; nothing done.'
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
