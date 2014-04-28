CDECK  ID>, SIGCAL.
       SUBROUTINE SIGCAL(INSTR,IFAIL)
*-----------------------------------------------------------------------
*   SIGCAL - Processes signal related procedure calls.
*   (Last changed on  3/ 7/12.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
       INTEGER INS(MXINS,4),ALGENT(MXALGE,10),MODREG(MXCONS:MXREG),
     -      ISYNCH,IINS0,ICONS0,ARGREF(MXARG,2),MODARG(MXARG),
     -      NREG,NCONS,NINS,NERR,NRES,NALGE,IENTRL,NAERR(100)
       REAL REG(MXCONS:MXREG),ARG(MXARG),EXPMAX
       PARAMETER(EXPMAX=40.0)
       LOGICAL EXEC(MXINS),LIGUND,LINUND
       COMMON /ALGDAT/ REG,ARG,MODARG,ARGREF,INS,MODREG,ALGENT,
     -      NREG,NCONS,NINS,NERR,NAERR,
     -      NRES,NALGE,IENTRL,ISYNCH,IINS0,ICONS0,EXEC,LIGUND,LINUND
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
       INTEGER MXAHIS
       PARAMETER(MXAHIS=20)
       INTEGER MATSLT,IPROC,IWRONG,NC,NARG,IFAIL,IFAIL1,IFAIL2,IFAIL3,
     -      NCR,IQ,IA,IW,ISW,ISIZ(MXMDIM),INSTR,IRTIM,ISTIM,IRDIR,ISDIR,
     -      IRCROS,ISCROS,NSIG,IREF(6),ISLOT(6),NDAT,IDIM,
     -      I,J,JTYPE,NCOPT,NREXP,ITYPE(2,MXAHIS),NHIST,
     -      NETOT,NITOT,IENTRY(MXAHIS),IHIST(MXAHIS)
       LOGICAL USE(MXVAR),STAT(4)
       CHARACTER*(MXINCH) TITLE,OPT
       CHARACTER*10 VARLIS(16)
       REAL TCR(MXLIST),EX,EY,EZ,QDRIFT,TSHIFT,WEIGHT
       DOUBLE PRECISION TIME(MXLIST),SIG(MXLIST),TMIN,TMAX
       EXTERNAL MATSLT
*** Assume the CALL will fail.
       IFAIL=1
*** Check the signal initialisation has been done.
       IF(.NOT.CELSET)THEN
            PRINT *,' !!!!!! SIGCAL WARNING : Cell data not'//
     -           ' available ; procedure not executed.'
            RETURN
       ELSEIF(.NOT.GASSET)THEN
            PRINT *,' !!!!!! SIGCAL WARNING : Gas data not'//
     -           ' available ; procedure not executed.'
            RETURN
       ELSEIF(.NOT.SIGSET)THEN
            CALL SIGINI(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Initialisation of'//
     -                ' signal calculation failed; no signals.'
                 RETURN
            ENDIF
       ENDIF
*** Some easy reference variables.
       NARG=INS(INSTR,3)
       IPROC=INS(INSTR,1)
*** Threshold crossings.
       IF(IPROC.EQ.-70)THEN
*   Check number and type of arguments.
            IWRONG=0
            DO 150 I=4,NARG
            IF(ARGREF(I,1).GE.2)IWRONG=1
150         CONTINUE
            IF(MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.1.OR.
     -           NARG.LT.5.OR.IWRONG.EQ.1)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect argument'//
     -                ' list provided for THRESHOLD_CROSSING.'
                 RETURN
            ENDIF
*   Fetch the option string.
            CALL STRBUF('READ',NINT(ARG(3)),TITLE,NC,IFAIL1)
            CALL CLTOU(TITLE(1:NC))
*   Clear previous use of storage for the results.
            DO 160 I=4,NARG
            CALL ALGREU(NINT(ARG(I)),MODARG(I),ARGREF(I,1))
160         CONTINUE
*   Get the threshold crossings.
            CALL SIGTHC(NINT(ARG(1)),ARG(2),
     -           TITLE(1:NC),NCR,TCR,IFAIL1)
            ARG(4)=REAL(NCR)
            MODARG(4)=2
            DO 170 I=1,MIN(NCR,MXARG-4)
            ARG(4+I)=TCR(I)
            MODARG(4+I)=2
170         CONTINUE
*   Check the error flag.
            IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCAL WARNING : Error'//
     -           ' computing threshold crossings.'
*** Return a signal.
       ELSEIF(IPROC.EQ.-71)THEN
*   Check argument list validity.
            IF(NARG.LT.3.OR.NARG.GT.4.OR.
     -           MODARG(1).NE.2.OR.
     -           ARGREF(2,1).GE.2.OR.ARGREF(3,1).GE.2.OR.
     -           (NARG.GE.4.AND.ARGREF(4,1).GE.2))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect argument'//
     -                ' list given to GET_SIGNAL; no signal returned.'
                 RETURN
            ENDIF
*   Verify the electrode number.
            IF(ABS(ARG(1)-ANINT(ARG(1))).GT.1E-4)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Electrode number'//
     -                ' is not integer; no signal returned.'
                 RETURN
            ENDIF
            ISW=NINT(ARG(1))
            IF(ISW.LT.1.OR.ISW.GT.NSW)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Electrode number'//
     -                'is out of range; no signal returned.'
                 RETURN
            ENDIF
*   De-allocate the current arguments.
            CALL ALGREU(NINT(ARG(2)),MODARG(2),ARGREF(2,1))
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            IF(NARG.GE.4)CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
*   Allocate matrices.
            ISIZ(1)=NTIME
            CALL MATADM('ALLOCATE',IRTIM,1,ISIZ,2,IFAIL1)
            CALL MATADM('ALLOCATE',IRDIR,1,ISIZ,2,IFAIL2)
            IF(NARG.GE.4)THEN
                 CALL MATADM('ALLOCATE',IRCROS,1,ISIZ,2,IFAIL3)
            ELSE
                 IRCROS=0
                 IFAIL3=0
            ENDIF
            ISTIM=MATSLT(IRTIM)
            ISDIR=MATSLT(IRDIR)
            IF(NARG.GE.4)THEN
                 ISCROS=MATSLT(IRCROS)
            ELSE
                 ISCROS=0
            ENDIF
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           ISTIM.LE.0.OR.ISDIR.LE.0.OR.
     -           (NARG.GE.4.AND.ISCROS.LE.0))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Failure to'//
     -                ' allocate output matrices; signal not returned.'
                 RETURN
            ENDIF
*   Copy the signals.
            DO 10 I=1,NTIME
            MVEC(MORG(ISTIM)+I)=TIMSIG(I)
            MVEC(MORG(ISDIR)+I)=SIGNAL(I,ISW,1)
            IF(NARG.GE.4)MVEC(MORG(ISCROS)+I)=SIGNAL(I,ISW,2)
10          CONTINUE
*   And save the matrices.
            ARG(2)=IRTIM
            MODARG(2)=5
            ARG(3)=IRDIR
            MODARG(3)=5
            IF(NARG.GE.4)THEN
                 ARG(4)=IRCROS
                 MODARG(4)=5
            ENDIF
*** Store a signal.
       ELSEIF(IPROC.EQ.-72)THEN
*   Check argument list validity.
            IF(NARG.LT.2.OR.NARG.GT.3.OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.5.OR.
     -           (NARG.GE.3.AND.MODARG(3).NE.5))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect argument'//
     -                ' list given to STORE_SIGNAL; no signal saved.'
                 RETURN
            ENDIF
*   Verify the wire number.
            ISW=NINT(ARG(1))
            IF(ISW.LE.0.OR.ISW.GT.NSW)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Invalid electrode'//
     -                ' number given to STORE_SIGNAL; no signal saved.'
                 RETURN
            ENDIF
*   Locate the matrices.
            ISDIR=MATSLT(NINT(ARG(2)))
            IF(NARG.GE.3)THEN
                 ISCROS=MATSLT(NINT(ARG(3)))
            ELSE
                 ISCROS=0
            ENDIF
            IF(ISDIR.LE.0.OR.(NARG.GE.3.AND.ISCROS.LE.0))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Failure to'//
     -                ' locate a signal vector; signal not saved.'
                 RETURN
            ELSEIF(MLEN(ISDIR).NE.NTIME.OR.
     -           (NARG.GE.3.AND.MLEN(ISCROS).NE.NTIME))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Signal vector'//
     -                ' has wrong length; signal not saved.'
                 RETURN
            ENDIF
*   Copy the signals.
            DO 20 I=1,NTIME
            SIGNAL(I,ISW,1)=MVEC(MORG(ISDIR)+I)
            IF(NARG.GE.3)SIGNAL(I,ISW,2)=MVEC(MORG(ISCROS)+I)
            TIMSIG(I)=TSTART+(I-1)*TDEV
20          CONTINUE
*** Extract a raw signal.
       ELSEIF(IPROC.EQ.-73)THEN
*   Check argument list validity.
            IF(NARG.NE.6.OR.MODARG(1).NE.1.OR.MODARG(2).NE.2.OR.
     -           MODARG(3).NE.2.OR.MODARG(4).NE.2.OR.
     -           ARGREF(5,1).GE.2.OR.ARGREF(6,1).GE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect argument'//
     -                ' list for GET_RAW_SIGNAL; no signal returned.'
                 RETURN
            ENDIF
*   Get the type.
            CALL STRBUF('READ',NINT(ARG(1)),TITLE,NC,IFAIL1)
            CALL CLTOU(TITLE(1:NC))
            IF(TITLE(1:NC).EQ.'E-'.OR.TITLE(1:NC).EQ.'-'.OR.
     -           TITLE(1:NC).EQ.'ELECTRON')THEN
                 IQ=-1
            ELSEIF(TITLE(1:NC).EQ.'ION'.OR.TITLE(1:NC).EQ.'+')THEN
                 IQ=+1
            ELSE
                 PRINT *,' !!!!!! SIGCAL WARNING : Signal type '//
     -                TITLE(1:NC)//' not known; assuming ION.'
                 IQ=+1
            ENDIF
*   Verify the sense wire number.
            ISW=NINT(ARG(2))
            IF(ISW.LE.0.OR.ISW.GT.NSW)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Invalid electrode'//
     -                ' number given to GET_RAW_SIGNAL;'//
     -                ' no signal returned.'
                 RETURN
            ENDIF
*   Verify the avalanche wire number.
            IW=NINT(ARG(3))
            IF(IW.LE.0.OR.IW.GT.NWIRE)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Invalid avalanche'//
     -                ' wire number given to GET_RAW_SIGNAL;'//
     -                ' no signal returned.'
                 RETURN
            ENDIF
*   Get the incidence angle.
            IA=NINT(NORIA*MOD(ARG(4)-2*PI*ANINT(ARG(4)/(2*PI))+2*PI,
     -           2*PI)/(2*PI))
            IF(IA.EQ.0)IA=NORIA
*   Fetch the signal.
            CALL SIGIST('READ',NSIG,TIME,SIG,ISW,IW,IA,IQ,IFAIL1)
            IF(IFAIL1.NE.0.OR.NSIG.LE.0)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Requested signal'//
     -                ' is not in store; no signal returned.'
                 RETURN
            ENDIF
*   De-allocate the current arguments.
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
*   Allocate matrices.
            ISIZ(1)=NSIG
            CALL MATADM('ALLOCATE',IRTIM,1,ISIZ,2,IFAIL1)
            CALL MATADM('ALLOCATE',IRDIR,1,ISIZ,2,IFAIL2)
            ISTIM=MATSLT(IRTIM)
            ISDIR=MATSLT(IRDIR)
            IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.IFAIL3.NE.0.OR.
     -           ISTIM.LE.0.OR.ISDIR.LE.0)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Failure to'//
     -                ' allocate output matrices; signal not returned.'
                 RETURN
            ENDIF
*   Copy the signals.
            DO 40 I=1,NSIG
            MVEC(MORG(ISTIM)+I)=TIME(I)
            MVEC(MORG(ISDIR)+I)=SIG(I)
40          CONTINUE
*   And save the matrices.
            ARG(5)=IRTIM
            MODARG(5)=5
            ARG(6)=IRDIR
            MODARG(6)=5
*** List the raw signals.
       ELSEIF(IPROC.EQ.-74)THEN
            IF(NARG.NE.0)PRINT *,' !!!!!! SIGCAL WARNING :'//
     -           ' LIST_RAW_SIGNALS doesn''t have arguments.'
            CALL SIGIST('LIST',NSIG,TIME,SIG,ISW,IW,IA,IQ,IFAIL1)
*** Compute the weighting field.
       ELSEIF(IPROC.EQ.-75)THEN
*   Check number of arguments.
            IF(NARG.NE.6)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect number'//
     -                ' of arguments for WEIGHTING_FIELD.'
                 RETURN
*   Check argument mode.
            ELSEIF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5).OR.
     -           MODARG(6).NE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Some arguments of'//
     -                ' WEIGHTING_FIELD are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(3,1).GE.2.OR.ARGREF(4,1).GE.2.OR.
     -           ARGREF(5,1).GE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Some arguments'//
     -                ' of WEIGHTING_FIELD can not be modified.'
                 RETURN
            ENDIF
*   Get sense wire number etc.
            ISW=NINT(ARG(6))
            IF(ISW.LE.0.OR.ISW.GT.NSW)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Sense wire'//
     -                ' number out of range.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(3)),MODARG(3),ARGREF(3,1))
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
**  Carry out the calculation for scalar coordinates.
            IF(MODARG(1).EQ.2.AND.MODARG(2).EQ.2)THEN
                 CALL SIGFLS(ARG(1),ARG(2),0.0,EX,EY,EZ,ISW)
                 ARG(3)=EX
                 ARG(4)=EY
                 ARG(5)=EZ
                 MODARG(3)=2
                 MODARG(4)=2
                 MODARG(5)=2
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 30 I=1,2
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                          ' locate a input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : x Or y'//
     -                          ' matrix is of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                           DO 130 J=1,MDIM(ISLOT(I))
                           ISIZ(J)=MSIZ(ISLOT(I),J)
130                        CONTINUE
                           IDIM=MDIM(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : x And y'//
     -                          ' have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
30               CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                     ' to find an x or y matrix.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 60 I=1,2
                 IF(MODARG(I).NE.5)THEN
                      CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                          ' to get a replacement matrix.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                          ' to locate a replacement matrix.'
                           RETURN
                      ENDIF
                      DO 70 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
70                    CONTINUE
                 ENDIF
60               CONTINUE
*   Allocate the output arrays (Ewx, Ewy, Ewz).
                 DO 140 I=4,6
                 CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                     ' to get an output matrix.'
                      RETURN
                 ENDIF
140              CONTINUE
*   And finally locate all matrices.
                 DO 180 I=1,6
                 IF(I.EQ.3)GOTO 180
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                     ' to locate an input or output matrix.'
                      RETURN
                 ENDIF
180              CONTINUE
*   And compute the data.
                 DO 90 I=1,NDAT
                 CALL SIGFLS(MVEC(MORG(ISLOT(1))+I),
     -                MVEC(MORG(ISLOT(2))+I),0.0,EX,EY,EZ,ISW)
                 MVEC(MORG(ISLOT(4))+I)=EX
                 MVEC(MORG(ISLOT(5))+I)=EY
                 MVEC(MORG(ISLOT(6))+I)=EZ
90               CONTINUE
                 ARG(3)=IREF(4)
                 ARG(4)=IREF(5)
                 ARG(5)=IREF(6)
                 MODARG(3)=5
                 MODARG(4)=5
                 MODARG(5)=5
*   Delete temporary input matrices.
                 DO 120 I=1,2
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCAL WARNING'//
     -                     ' : Unable to delete a replacement matrix.'
                 ENDIF
120              CONTINUE
            ENDIF
*** Compute the weighting field in 3 dimensions.
       ELSEIF(IPROC.EQ.-76)THEN
*   Check number of arguments.
            IF(NARG.NE.7)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect number'//
     -                ' of arguments for WEIGHTING_FIELD_3.'
                 RETURN
*   Check argument mode.
            ELSEIF((MODARG(1).NE.2.AND.MODARG(1).NE.5).OR.
     -           (MODARG(2).NE.2.AND.MODARG(2).NE.5).OR.
     -           (MODARG(3).NE.2.AND.MODARG(3).NE.5).OR.
     -           MODARG(7).NE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Some arguments of'//
     -                ' WEIGHTING_FIELD_3 are of incorrect type.'
                 RETURN
*   Check the the results can be transferred back.
            ELSEIF(ARGREF(4,1).GE.2.OR.ARGREF(5,1).GE.2.OR.
     -           ARGREF(6,1).GE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Some arguments'//
     -                ' of WEIGHTING_FIELD_3 can not be modified.'
                 RETURN
            ENDIF
*   Get sense wire number etc.
            ISW=NINT(ARG(7))
            IF(ISW.LE.0.OR.ISW.GT.NSW)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Sense wire'//
     -                ' number out of range.'
                 RETURN
            ENDIF
*   Variables already in use ?
            CALL ALGREU(NINT(ARG(4)),MODARG(4),ARGREF(4,1))
            CALL ALGREU(NINT(ARG(5)),MODARG(5),ARGREF(5,1))
            CALL ALGREU(NINT(ARG(6)),MODARG(6),ARGREF(6,1))
**  Carry out the calculation for scalar coordinates.
            IF(MODARG(1).EQ.2.AND.MODARG(2).EQ.2.AND.MODARG(3).EQ.2)THEN
                 CALL SIGFLS(ARG(1),ARG(2),ARG(3),EX,EY,EZ,ISW)
                 ARG(4)=EX
                 ARG(5)=EY
                 ARG(6)=EZ
                 MODARG(4)=2
                 MODARG(5)=2
                 MODARG(6)=2
**  At least one of them is a matrix.
            ELSE
*   Figure out what the dimensions are.
                 NDAT=-1
                 DO 220 I=1,3
                 IF(MODARG(I).EQ.5)THEN
                      IREF(I)=NINT(ARG(I))
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                          ' locate a input matrix.'
                           RETURN
                      ELSEIF(MMOD(ISLOT(I)).NE.2)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : x, y'//
     -                          ' Or z matrix is of incorrect type.'
                           RETURN
                      ENDIF
                      IF(NDAT.LT.0)THEN
                           NDAT=MLEN(ISLOT(I))
                           DO 230 J=1,MDIM(ISLOT(I))
                           ISIZ(J)=MSIZ(ISLOT(I),J)
230                        CONTINUE
                           IDIM=MDIM(ISLOT(I))
                      ELSEIF(NDAT.NE.MLEN(ISLOT(I)))THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : x, y'//
     -                          ' And z have inconsistent lengths.'
                           RETURN
                      ENDIF
                 ENDIF
220              CONTINUE
                 IF(NDAT.LT.1)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                     ' to find an x, y or z matrix.'
                      RETURN
                 ENDIF
*   Now book matrices for the missing elements and initialise them.
                 DO 240 I=1,3
                 IF(MODARG(I).NE.5)THEN
                      CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                          ' to get a replacement matrix.'
                           RETURN
                      ENDIF
                      ISLOT(I)=MATSLT(IREF(I))
                      IF(ISLOT(I).LE.0)THEN
                           PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                          ' to locate a replacement matrix.'
                           RETURN
                      ENDIF
                      DO 250 J=1,MLEN(ISLOT(I))
                      MVEC(MORG(ISLOT(I))+J)=ARG(I)
250                   CONTINUE
                 ENDIF
240              CONTINUE
*   Allocate the output arrays (Ewx, Ewy, Ewz).
                 DO 260 I=4,6
                 CALL MATADM('ALLOCATE',IREF(I),IDIM,ISIZ,2,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                     ' to get an output matrix.'
                      RETURN
                 ENDIF
260              CONTINUE
*   And finally locate all matrices.
                 DO 270 I=1,6
                 ISLOT(I)=MATSLT(IREF(I))
                 IF(ISLOT(I).LE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable'//
     -                     ' to locate an input or output matrix.'
                      RETURN
                 ENDIF
270              CONTINUE
*   And compute the data.
                 DO 280 I=1,NDAT
                 CALL SIGFLS(MVEC(MORG(ISLOT(1))+I),
     -                MVEC(MORG(ISLOT(2))+I),
     -                MVEC(MORG(ISLOT(3))+I),
     -                EX,EY,EZ,ISW)
                 MVEC(MORG(ISLOT(4))+I)=EX
                 MVEC(MORG(ISLOT(5))+I)=EY
                 MVEC(MORG(ISLOT(6))+I)=EZ
280              CONTINUE
                 ARG(4)=IREF(4)
                 ARG(5)=IREF(5)
                 ARG(6)=IREF(6)
                 MODARG(4)=5
                 MODARG(5)=5
                 MODARG(6)=5
*   Delete temporary input matrices.
                 DO 310 I=1,3
                 IF(MODARG(I).NE.5)THEN
                      ISIZ(1)=NDAT
                      CALL MATADM('DELETE',IREF(I),1,ISIZ,2,IFAIL1)
                      IF(IFAIL1.NE.0)PRINT *,' !!!!!! SIGCAL WARNING'//
     -                     ' : Unable to delete a replacement matrix.'
                 ENDIF
310              CONTINUE
            ENDIF
*** Induced charge.
       ELSEIF(IPROC.EQ.-77)THEN
            IF((NARG.NE.6.AND.NARG.NE.8).OR.
     -           MODARG(1).NE.2.OR.MODARG(2).NE.2.OR.MODARG(3).NE.2.OR.
     -           (NARG.EQ.8.AND.(MODARG(6).NE.2.OR.MODARG(7).NE.2)).OR.
     -           MODARG(4).NE.1.OR.MODARG(5).NE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect argument'//
     -                ' list for INDUCED_CHARGE; not executed.'
                 RETURN
            ELSEIF(ARGREF(NARG,1).GE.2)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : An argument of'//
     -                ' INDUCED_CHARGE can not be modified.'
                 RETURN
            ENDIF
*   Fetch particle type.
            CALL STRBUF('READ',NINT(ARG(4)),TITLE,NC,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Error retrieving'//
     -                ' the INDUCED_CHARGE particle type.'
                 RETURN
            ENDIF
            IF(NC.GE.1)CALL CLTOU(TITLE(1:NC))
*   Decode it.
            IF(TITLE(1:NC).EQ.'ELECTRON'.OR.TITLE(1:NC).EQ.'E-')THEN
                 JTYPE=1
                 QDRIFT=-1
            ELSEIF(TITLE(1:NC).EQ.'POSITRON'.OR.TITLE(1:NC).EQ.'E+')THEN
                 JTYPE=1
                 QDRIFT=+1
            ELSEIF(TITLE(1:NC).EQ.'ION'.OR.TITLE(1:NC).EQ.'ION+')THEN
                 JTYPE=2
                 QDRIFT=+1
            ELSEIF(TITLE(1:NC).EQ.'ION-')THEN
                 JTYPE=2
                 QDRIFT=-1
            ELSE
                 PRINT *,' !!!!!! SIGCAL WARNING : Unknown particle'//
     -                ' type received by INDUCED_CHARGE.'
                 RETURN
            ENDIF
*   Pick up the electrode number.
            ISW=NINT(ARG(5))
            IF(ISW.LE.0.OR.ISW.GT.NSW)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : INDUCED_CHARGE'//
     -                ' received an invalid group number.'
                 RETURN
            ENDIF
*   Delete old contents of return variable.
            CALL ALGREU(NINT(ARG(NARG)),MODARG(NARG),ARGREF(NARG,1))
*   Compute the drift line.
            CALL DLCALC(ARG(1),ARG(2),ARG(3),QDRIFT,JTYPE)
*   Time limits.
            IF(NARG.EQ.6)THEN
                 TMIN=TU(1)
                 TMAX=TU(NU)
            ELSE
                 TMIN=ARG(6)
                 TMAX=ARG(7)
            ENDIF
*   Compute the induced charge.
            CALL SIGQIN(ARG(NARG),ISW,TMIN,TMAX)
            MODARG(NARG)=2
*** Add a signal.
       ELSEIF(IPROC.EQ.-78)THEN
*   Check argument list.
            IF(NARG.GT.3.OR.
     -           (NARG.EQ.1.AND.
     -                 MODARG(1).NE.2.AND.MODARG(1).NE.1).OR.
     -           (NARG.EQ.2.AND.
     -                (MODARG(1).NE.2.OR.
     -                (MODARG(2).NE.2.AND.MODARG(2).NE.1))).OR.
     -           (NARG.EQ.3.AND.
     -                (MODARG(1).NE.2.OR.
     -                 MODARG(2).NE.2.OR.
     -                 MODARG(3).NE.1)))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect argument'//
     -                ' list received for ADD_SIGNALS; not called.'
                 RETURN
            ENDIF
*   Get the time shift.
            IF(NARG.GE.1.AND.MODARG(1).EQ.2)THEN
                 TSHIFT=ARG(1)
            ELSE
                 TSHIFT=0.0
            ENDIF
*   Get the time weight.
            IF(NARG.GE.2.AND.MODARG(2).EQ.2)THEN
                 WEIGHT=ARG(2)
            ELSE
                 WEIGHT=1.0
            ENDIF
*   Fetch the options.
            IF(NARG.GE.1.AND.MODARG(MAX(1,NARG)).EQ.1)THEN
                 CALL STRBUF('READ',NINT(ARG(NARG)),TITLE,NC,IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Error'//
     -                     ' retrieving the ADD_SIGNALS options.'
                      RETURN
                 ENDIF
                 IF(NC.GE.1)CALL CLTOU(TITLE(1:NC))
            ELSE
                 TITLE='CROSS'
                 NC=5
            ENDIF
*   Call the procedure.
            IF(INDEX(TITLE(1:NC),'DIRECT')+
     -           INDEX(TITLE(1:NC),'NOCROSS').NE.0)THEN
                 CALL SIGADS(TSHIFT,.FALSE.,WEIGHT,IFAIL1)
            ELSE
                 CALL SIGADS(TSHIFT,.TRUE.,WEIGHT,IFAIL1)
            ENDIF
*   Check return code.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Error computing'//
     -                ' or adding a signal; signal incomplete.'
                 RETURN
            ENDIF
*** 3D MC drift line calculation for electrons with avalanche.
       ELSEIF(IPROC.EQ.-79)THEN
**  Check number of arguments.
            IF(NARG.LT.3.OR.
     -           (NARG.GE.4.AND.MODARG(4).NE.1).OR.
     -           (NARG.GE.5.AND.ARGREF(5,1).GE.2).OR.
     -           (NARG.GE.6.AND.ARGREF(6,1).GE.2).OR.
     -           (NARG.GE.7.AND.NARG.NE.2*(NARG/2)).OR.
     -           NARG.GT.6+2*MXAHIS)THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : Incorrect list of'//
     -                ' arguments for AVALANCHE; not executed'
                 RETURN
*   Make sure there are drift velocities.
            ELSEIF(.NOT.GASOK(1))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : The drift velocity'//
     -                ' for electrons is not defined ; not executed.'
                 RETURN
*   Make sure there are Townsend coefficients.
            ELSEIF(.NOT.GASOK(4))THEN
                 PRINT *,' !!!!!! SIGCAL WARNING : The Townsend'//
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
                 DO 100 I=1,NHIST
*   Fetch the histogram string.
                 CALL STRBUF('READ',NINT(ARG(5+2*I)),TITLE,NC,IFAIL1)
                 IF(IFAIL1.NE.0.OR.NC.LT.1)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Unable to get'//
     -                     ' an histogram formula; no avalanche.'
                      RETURN
                 ENDIF
                 CALL CLTOU(TITLE(1:NC))
*   Translate the formula.
                 CALL ALGPRE(TITLE(1:NC),NC,VARLIS,16,NREXP,USE,
     -                IENTRY(I),IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : The histogram'//
     -                     ' function '//TITLE(1:NC)//' can not be'//
     -                     ' translated; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(NREXP.LT.1.OR.NREXP.GT.2)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : The histogram'//
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
                      PRINT *,' !!!!!! SIGCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE( 1).OR.USE( 2).OR.USE( 3).OR.USE( 4))THEN
                      ITYPE(1,I)=1
                 ENDIF
                 IF((USE( 5).OR.USE( 6).OR.USE( 7).OR.USE( 8)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE( 5).OR.USE( 6).OR.USE( 7).OR.USE( 8))THEN
                      ITYPE(1,I)=2
                 ENDIF
                 IF((USE( 9).OR.USE(10).OR.USE(11).OR.USE(12)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE( 9).OR.USE(10).OR.USE(11).OR.USE(12))THEN
                      ITYPE(1,I)=3
                 ENDIF
                 IF((USE(13).OR.USE(14).OR.USE(15).OR.USE(16)).AND.
     -                ITYPE(1,I).NE.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Histogram'//
     -                     ' function '//TITLE(1:NC)//' uses an'//
     -                     ' invalid mix of parameters; no avalanche.'
                      CALL ALGCLR(IENTRY(I))
                      RETURN
                 ELSEIF(USE(13).OR.USE(14).OR.USE(15).OR.USE(16))THEN
                      ITYPE(1,I)=4
                 ENDIF
                 IF(ITYPE(1,I).EQ.0)THEN
                      PRINT *,' !!!!!! SIGCAL WARNING : Histogram'//
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
                      PRINT *,' !!!!!! SIGCAL WARNING : Histogram'//
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
100              CONTINUE
*   No histograms to be made.
            ELSE
                 STAT(1)=.FALSE.
                 STAT(2)=.FALSE.
                 STAT(3)=.FALSE.
                 STAT(4)=.FALSE.
                 NHIST=0
            ENDIF
**  Carry out the calculation.
            CALL SIGMCA(ARG(1),ARG(2),ARG(3),NETOT,NITOT,
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
            DO 110 I=1,NHIST
            ARG(6+2*I)=REAL(IHIST(I))
            MODARG(6+2*I)=4
            CALL ALGCLR(IENTRY(I))
110         CONTINUE
*** Other signal calls not known.
       ELSE
            PRINT *,' !!!!!! SIGCAL WARNING : Invalid signal'//
     -           ' procedure code received; nothing done.'
            RETURN
       ENDIF
*** Seems to have worked.
       IFAIL=0
       END
