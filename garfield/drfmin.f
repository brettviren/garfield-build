CDECK  ID>, DRFMIN.
       SUBROUTINE DRFMIN
*-----------------------------------------------------------------------
*   DRFMIN - Minimises a function along a track segment.
*   (Last changed on 18/ 4/10.)
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
       CHARACTER*(MXCHAR) FUNMIN,FUNSEL,FUNTRA
       CHARACTER*(MXNAME) FILE
       CHARACTER*(MXINCH) STRING
       CHARACTER*10 VARLIS(MXVAR)
       CHARACTER*20 STATUS
       CHARACTER*29 REMARK
       CHARACTER*8 TIME,DATE,MEMBER
       DOUBLE PRECISION F0(3)
       REAL RES(2),VAR(MXVAR),XPOS,YPOS,ZPOS,EPST,EPSP,EPSTR,EPSPR,
     -      TMIN,TMAX,TMINR,TMAXR,QMIN,EX,EY,EZ,BX,BY,BZ
       DOUBLE PRECISION TPARA,FTPARA,T1,FT1,T2,FT2,T3,FT3,XAUX1,YAUX1,
     -      XAUX2,YAUX2,DRLENG
       INTEGER MODRES(2),MODVAR(MXVAR),INPCMP,INPTYP,I,NWORD,NCFMIN,
     -      NCFSEL,NCFTRA,NCFILE,NCMEMB,NCREM,NSTEP,NSTEPR,NITMAX,NITR,
     -      ITYPE,IFAIL,IFAIL1,IFAIL2,IENTRA,IENMIN,IENSEL,IU,ILOC,
     -      NCSTAT,NREXP,NVAR,ISET1,ISET2,ISET3,I1,I2,I3,IAUX,IOS,NRES,
     -      INEXT
       LOGICAL USE(MXVAR),LPRINT,LDIFF,LTOWN,LATTA,LLENG,LVELOC,
     -      LFIELD,LMINWR,FLAG(MXWORD+3),EXMEMB,OK
       EXTERNAL INPCMP,INPTYP
*** Initial values.
       FUNSEL='TRUE'
       NCFSEL=4
       FUNMIN='TIME'
       NCFMIN=4
       FUNTRA='?'
       NCFTRA=1
       LPRINT=.TRUE.
       EPST=1.0E-4
       EPSP=1.0E-4
       TMIN=0.0
       TMAX=0.0
       NSTEP=20
       NITMAX=20
       QMIN=-1.0
       ITYPE=1
       OK=.TRUE.
*** Dataset initial information.
       FILE=' '
       NCFILE=1
       MEMBER='< none >'
       NCMEMB=8
       REMARK='none'
       NCREM=4
       LMINWR=.FALSE.
*** Decode the command line.
       CALL INPNUM(NWORD)
*   The function to be minimised.
       IF(NWORD.GT.1)CALL INPSTR(2,2,FUNMIN,NCFMIN)
*   Flag keywords.
       DO 20 I=1,MXWORD+3
       IF(I.EQ.1.OR.I.GT.NWORD)THEN
            FLAG(I)=.TRUE.
            GOTO 20
       ENDIF
       FLAG(I)=.FALSE.
       IF(INPCMP(I,'SEL#ECTION-#FUNCTION')+INPCMP(I,'ON')+
     -      INPCMP(I,'PR#INT')+INPCMP(I,'NOPR#INT')+
     -      INPCMP(I,'F#UNCTION-PREC#ISION')+
     -      INPCMP(I,'POS#ITIONAL-RES#OLUTION')+
     -      INPCMP(I,'RANGE')+INPCMP(I,'N')+
     -      INPCMP(I,'ITER#ATE-#LIMIT')+
     -      INPCMP(I,'E#LECTRON')+INPCMP(I,'I#ON')+
     -      INPCMP(I,'POS#ITIVE')+INPCMP(I,'NEG#ATIVE')+
     -      INPCMP(I,'D#ATASET')+INPCMP(I,'REM#ARK').NE.0)FLAG(I)=.TRUE.
20     CONTINUE
*   Scan the input.
       INEXT=3
       DO 10 I=3,NWORD
       IF(I.LT.INEXT)GOTO 10
*   Drift line selection criteria.
       IF(INPCMP(I,'SEL#ECTION-#FUNCTION').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No selection function given.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,FUNSEL,NCFSEL)
            ENDIF
            INEXT=I+2
*   Printing of intermediate results.
       ELSEIF(INPCMP(I,'PR#INT').NE.0)THEN
            LPRINT=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT').NE.0)THEN
            LPRINT=.FALSE.
*   Target accuracy.
       ELSEIF(INPCMP(I,'F#UNCTION-PREC#ISION').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No target precision given.    ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I+1,'Wrong data type.              ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EPSTR,EPST)
                 IF(IFAIL1.EQ.0.AND.EPSTR.LE.0.0.OR.EPSTR.GT.1.0)THEN
                      CALL INPMSG(I+1,'Target precision out of range.')
                      OK=.FALSE.
                 ELSEIF(IFAIL1.EQ.0)THEN
                      EPST=EPSTR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Positional accuracy.
       ELSEIF(INPCMP(I,'POS#ITIONAL-RES#OLUTION').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No resolution found.          ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).LE.0)THEN
                 CALL INPMSG(I+1,'Wrong data type.              ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EPSPR,EPSP)
                 IF(IFAIL1.EQ.0.AND.EPSPR.LE.0.0.OR.EPSPR.GT.1.0)THEN
                      CALL INPMSG(I+1,'Target precision out of range.')
                      OK=.FALSE.
                 ELSEIF(IFAIL1.EQ.0)THEN
                      EPSP=EPSPR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Track selection.
       ELSEIF(INPCMP(I,'ON').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No track parameters given.    ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,FUNTRA,NCFTRA)
            ENDIF
            INEXT=I+2
*   Track range.
       ELSEIF(INPCMP(I,'RANGE').NE.0)THEN
            IF(FLAG(I+1).OR.FLAG(I+2))THEN
                 CALL INPMSG(I,'No parameter range given.     ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).EQ.0)THEN
                 CALL INPMSG(I+1,'Wrong data type.              ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+2).EQ.0)THEN
                 CALL INPMSG(I+2,'Wrong data type.              ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,TMINR,0.0)
                 CALL INPRDR(I+2,TMAXR,0.0)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      TMIN=TMINR
                      TMAX=TMAXR
                 ENDIF
            ENDIF
            INEXT=I+3
*   Number of steps.
       ELSEIF(INPCMP(I,'N').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No number of steps found.     ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I+1,'Wrong data type.              ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NSTEPR,20)
                 IF(IFAIL1.EQ.0.AND.NSTEPR.LE.0.OR.NSTEPR.GT.MXLIST)THEN
                      CALL INPMSG(I+1,'Number of steps out of range. ')
                      OK=.FALSE.
                 ELSEIF(IFAIL1.EQ.0)THEN
                      NSTEP=NSTEPR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Number of iterations.
       ELSEIF(INPCMP(I,'ITER#ATE-#LIMIT').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No iteration limit found.     ')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).NE.1)THEN
                 CALL INPMSG(I+1,'Wrong data type.              ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NITR,20)
                 IF(IFAIL1.EQ.0.AND.NITR.LE.0)THEN
                      CALL INPMSG(I+1,'Iteration limit out of range. ')
                      OK=.FALSE.
                 ELSEIF(IFAIL1.EQ.0)THEN
                      NITMAX=NITR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Particle type.
       ELSEIF(INPCMP(I,'E#LECTRON').NE.0)THEN
            ITYPE=1
       ELSEIF(INPCMP(I,'I#ON').NE.0)THEN
            IF(GASOK(2))THEN
                 ITYPE=2
            ELSE
                 CALL INPMSG(I,'Ion mobility data missing.    ')
                 OK=.FALSE.
            ENDIF
*   Particle charge.
       ELSEIF(INPCMP(I,'POS#ITIVE').NE.0)THEN
            QMIN=+1.0
       ELSEIF(INPCMP(I,'NEG#ATIVE').NE.0)THEN
            QMIN=-1.0
*   Look for a DATASET (and perhaps a member) receiving the data.
       ELSEIF(INPCMP(I,'D#ATASET').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'the dataset name is missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCFILE)
                 FILE=STRING
                 IF(.NOT.FLAG(I+2))THEN
                      CALL INPSTR(I+2,I+2,STRING,NCMEMB)
                      MEMBER=STRING
                      INEXT=I+3
                 ELSE
                      INEXT=I+2
                 ENDIF
                 LMINWR=.TRUE.
            ENDIF
*   Look for a REMARK replacing the default remark in the header,
       ELSEIF(INPCMP(I,'REM#ARK').NE.0)THEN
            IF(FLAG(I+1))THEN
                 CALL INPMSG(I,'No remark has been found.     ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCREM)
                 REMARK=STRING(1:NCREM)
                 INEXT=I+2
            ENDIF
*   Anything else is not valid.
       ELSE
            CALL INPMSG(I,'Not a valid keyword.          ')
            OK=.FALSE.
       ENDIF
10     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Debug output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFMIN DEBUG   : '',
     -      ''Function to be minimised:    '',A/
     -      26X,''Selection function:          '',A/
     -      26X,''Curve function:              '',A/
     -      26X,''Curve parameter range:       '',2E15.8/
     -      26X,''Number of curve points:      '',I10/
     -      26X,''Attempted function accuracy: '',E15.8/
     -      26X,''Positional resolution:       '',E15.8/
     -      26X,''Iteration limit:             '',I10/
     -      26X,''Particle type and charge:    '',I10,3X,F4.1)')
     -      FUNMIN(1:NCFMIN),FUNSEL(1:NCFSEL),FUNTRA(1:NCFTRA),
     -      TMIN,TMAX,NSTEP,EPST,EPSP,NITMAX,ITYPE,QMIN
       IF(LMINWR.AND.LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DRFMIN DEBUG   : '',
     -           ''Output data set: '',A/
     -           26X,''Data set member: '',A/
     -           26X,''Remark string:   '',A)')
     -           FILE(1:NCFILE),MEMBER(1:NCMEMB),REMARK(1:NCREM)
       ELSEIF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ DRFMIN DEBUG   : '',
     -           ''No dataset output has been requested.'')')
       ENDIF
*   Check whether the member already exists.
       IF(LMINWR)THEN
            CALL DSNREM(FILE(1:NCFILE),MEMBER(1:NCMEMB),'MINIMUM',
     -           EXMEMB)
            IF(JEXMEM.EQ.2.AND.EXMEMB)THEN
                 PRINT *,' ------ DRFMIN MESSAGE : A copy of the'//
     -                ' member exists; new member will be appended.'
            ELSEIF(JEXMEM.EQ.3.AND.EXMEMB)THEN
                 PRINT *,' !!!!!! DRFMIN WARNING : A copy of the'//
     -                ' member exists already; member will not be'//
     -                ' written.'
                 LMINWR=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Quit now if OK is no longer true and if JFAIL is set.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### DRFMIN ERROR   : Instruction is not'//
     -           ' carried out because of the above errors.'
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### DRFMIN ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
       ENDIF
*** Check that the important things have been specified.
       IF(FUNTRA.EQ.'?')THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The curve over which'//
     -           ' the minimisation is to be done, is missing.'
            RETURN
       ENDIF
*** Translate the various functions, first the track function.
       VARLIS(1)='T'
       NVAR=1
       CALL ALGPRE(FUNTRA,NCFTRA,VARLIS,NVAR,NRES,USE,IENTRA,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : Translation of the'//
     -           ' curve-function failed; nothing done.'
            CALL ALGCLR(IENTRA)
            RETURN
       ELSEIF(.NOT.USE(1))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The curve-function'//
     -           ' does not depend on T; nothing done.'
            CALL ALGCLR(IENTRA)
            RETURN
       ELSEIF(NRES.NE.2)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The curve-function'//
     -           ' does not return 2 results; nothing done.'
            CALL ALGCLR(IENTRA)
            RETURN
       ENDIF
*** Next the selection function.
       VARLIS(1)='TIME'
       VARLIS(2)='LENGTH'
       VARLIS(3)='DIFFUSION'
       VARLIS(4)='AVALANCHE'
       VARLIS(5)='LOSS'
       VARLIS(6)='E'
       VARLIS(7)='V'
       VARLIS(8)='B'
       VARLIS(9)='VELOCITY'
       VARLIS(10)='STATUS'
       NVAR=10
       CALL ALGPRE(FUNSEL,NCFSEL,VARLIS,NVAR,NRES,USE,IENSEL,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : Translation of the'//
     -           ' selection-function failed; nothing done.'
            CALL ALGCLR(IENTRA)
            CALL ALGCLR(IENSEL)
            RETURN
       ELSEIF(USE(3).AND..NOT.GASOK(3))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The selection function'//
     -           ' uses diffusion data which is absent; nothing done.'
            CALL ALGCLR(IENTRA)
            CALL ALGCLR(IENSEL)
            RETURN
       ELSEIF(USE(4).AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The selection function'//
     -           ' uses Townsend data which is absent; nothing done.'
            CALL ALGCLR(IENTRA)
            CALL ALGCLR(IENSEL)
            RETURN
       ELSEIF(USE(5).AND..NOT.GASOK(6))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The selection function'//
     -           ' uses attachment data which is absent; nothing done.'
            CALL ALGCLR(IENTRA)
            CALL ALGCLR(IENSEL)
            RETURN
       ELSEIF(USE(8).AND..NOT.MAGOK)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The selection function'//
     -           ' uses the B field which is absent; nothing done.'
            CALL ALGCLR(IENTRA)
            CALL ALGCLR(IENSEL)
            RETURN
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The selection function'//
     -           ' does not return 1 result; nothing done.'
            CALL ALGCLR(IENTRA)
            CALL ALGCLR(IENSEL)
            RETURN
       ENDIF
*   Set flags for items to be computed.
       LLENG=USE(2)
       LDIFF=USE(3)
       LTOWN=USE(4)
       LATTA=USE(5)
       LFIELD=USE(6).OR.USE(7).OR.USE(8)
       LVELOC=USE(9)
*** Next the function to be minimised.
       VARLIS(1)='TIME'
       VARLIS(2)='LENGTH'
       VARLIS(3)='DIFFUSION'
       VARLIS(4)='AVALANCHE'
       VARLIS(5)='LOSS'
       VARLIS(6)='E'
       VARLIS(7)='V'
       VARLIS(8)='B'
       VARLIS(9)='VELOCITY'
       NVAR=9
       CALL ALGPRE(FUNMIN,NCFMIN,VARLIS,NVAR,NRES,USE,IENMIN,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : Translation of the'//
     -           ' function to be minimised failed; nothing done.'
            GOTO 3000
       ELSEIF(USE(3).AND..NOT.GASOK(3))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The function to be'//
     -           ' minimised uses absent diffusion data; nothing done.'
            GOTO 3000
       ELSEIF(USE(4).AND..NOT.GASOK(4))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The function to be'//
     -           ' minimised uses absent Townsend data; nothing done.'
            GOTO 3000
       ELSEIF(USE(5).AND..NOT.GASOK(6))THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The function to be'//
     -           ' minimised uses absent attachment data; nothing done.'
            GOTO 3000
       ELSEIF(USE(8).AND..NOT.MAGOK)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The function to be'//
     -           ' minimised uses absent B field data; nothing done.'
            GOTO 3000
       ELSEIF(NRES.NE.1)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The function to be'//
     -           ' minimised does not return 1 result; nothing done.'
            GOTO 3000
       ENDIF
*   Update the flags.
       IF(USE(2))LLENG=.TRUE.
       IF(USE(3))LDIFF=.TRUE.
       IF(USE(4))LTOWN=.TRUE.
       IF(USE(5))LATTA=.TRUE.
       IF(USE(6).OR.USE(7).OR.USE(8))LFIELD=.TRUE.
       IF(USE(9))LVELOC=.TRUE.
*   Debugging information.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFMIN DEBUG   : '',
     -      ''Evaluation flags: Diffusion='',L1,'' Townsend='',L1,
     -      '' Length='',L1/26X,''Loss='',L1,'' Field='',L1,
     -      '' Velocity='',L1,''.'')')
     -      LDIFF,LTOWN,LLENG,LATTA,LFIELD,LVELOC
*** Prepare dataset output.
       IF(LMINWR)THEN
*   Open the file.
            CALL DSNOPN(FILE,NCFILE,12,'WRITE-LIBRARY',IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! DRFMIN WARNING : Opening '//
     -                FILE(1:NCFILE)//'; minimisation data not written.'
                 RETURN
            ENDIF
*   Record that the file has been opened.
            CALL DSNLOG(FILE,'Minimum   ','Sequential','Write     ')
            IF(LDEBUG)PRINT *,' ++++++ DRFMIN DEBUG   : Dataset '//
     -           FILE(1:NCFILE)//' opened on unit 12 for seq write.'
*   Write a heading record to the file.
            CALL DATTIM(DATE,TIME)
            WRITE(12,'(''% Created '',A8,'' At '',A8,1X,A8,
     -           '' MINIMUM '',1X,''"'',A29,''"'')',IOSTAT=IOS,ERR=2010)
     -           DATE,TIME,MEMBER,REMARK
            WRITE(12,'(''  GENERAL INFORMATION:''//
     -           ''  Function to be minimised:    '',A/
     -           ''  Selection function:          '',A/
     -           ''  Curve function:              '',A/
     -           ''  Curve parameter range:       '',2E15.8/
     -           ''  Number of curve points:      '',I10/
     -           ''  Attempted function accuracy: '',E15.8/
     -           ''  Positional resolution:       '',E15.8/
     -           ''  Iteration limit:             '',I10/
     -           ''  Particle type and charge:    '',I10,3X,F4.1/)')
     -           FUNMIN(1:NCFMIN),FUNSEL(1:NCFSEL),FUNTRA(1:NCFTRA),
     -           TMIN,TMAX,NSTEP,EPST,EPSP,NITMAX,ITYPE,QMIN
       ENDIF
*** Preset some parameters needed for minimisation.
       ISET1=0
       ISET2=0
       ISET3=0
       I1=0
       I2=0
       I3=0
       T1=0
       T2=0
       T3=0
       FT1=0
       FT2=0
       FT3=0
*** Start the minimisation procedure itself.
       DO 100 I=0,NSTEP
*   First calculate a position.
       VAR(1)=TMIN+REAL(I)*(TMAX-TMIN)/REAL(NSTEP)
       MODVAR(1)=2
       NVAR=1
       NREXP=2
       CALL ALGEXE(IENTRA,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
*   Then the drift-line from there.
       XPOS=RES(1)
       YPOS=RES(2)
       ZPOS=0
       IF(POLAR)THEN
            CALL CFMPTR(XPOS,YPOS,XPOS,YPOS,1,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DRFMIN WARNING : Illegal polar'//
     -                ' curve coordinate seen at T=',VAR(1),
     -                '; no further minimisation.'
                 IF(LMINWR)WRITE(12,'(/''  # Minimisation abandoned:'',
     -                '' illegal polar coordinate seen.''/)')
                 GOTO 3000
            ENDIF
       ENDIF
       CALL DLCALC(XPOS,YPOS,ZPOS,QMIN,ITYPE)
*   And the derived information.
       VAR(1)=TU(NU)
       VAR(2)=0.0
       VAR(3)=0.0
       VAR(4)=0.0
       VAR(5)=0.0
       VAR(6)=0.0
       VAR(7)=0.0
       VAR(8)=0.0
       VAR(9)=0.0
       VAR(10)=0
       IF(LLENG)THEN
            DRLENG=0.0
            DO 110 IU=2,NU
            IF(POLAR)THEN
                 CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX1,YAUX1,1)
                 CALL CF2RTC(XU(IU),YU(IU),XAUX2,YAUX2,1)
                 DRLENG=DRLENG+
     -                SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ELSE
                 DRLENG=DRLENG+
     -                SQRT((XU(IU)-XU(IU-1))**2+(YU(IU)-YU(IU-1))**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ENDIF
110         CONTINUE
            VAR(2)=DRLENG
       ENDIF
       IF(LDIFF)CALL DLCDIF(VAR(3))
       IF(LTOWN)CALL DLCTWN(VAR(4))
       IF(LATTA)CALL DLCATT(VAR(5))
       IF(LFIELD)THEN
            CALL EFIELD(XPOS,YPOS,ZPOS,EX,EY,EZ,VAR(6),VAR(7),1,ILOC)
            IF(MAGOK)CALL BFIELD(XPOS,YPOS,ZPOS,BX,BY,BZ,VAR(8))
       ENDIF
       IF(LVELOC)THEN
            CALL DLCVEL(DBLE(XPOS),DBLE(YPOS),DBLE(ZPOS),
     -           F0,-1.0,1,ILOC)
            VAR(9)=REAL(SQRT(F0(1)**2+F0(2)**2+F0(3)**2))
       ENDIF
       IF(ISTAT.EQ.-1)THEN
            STATUS='Left_Area'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-2)THEN
            STATUS='Too_Many_Steps'
            NCSTAT=14
       ELSEIF(ISTAT.EQ.-3)THEN
            STATUS='Abandoned'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-4)THEN
            STATUS='Hit_Plane'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-5)THEN
            STATUS='Left_Drift_Medium'
            NCSTAT=17
       ELSEIF(ISTAT.EQ.-6)THEN
            STATUS='Left_Mesh'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-7)THEN
            STATUS='Attached'
            NCSTAT=8
       ELSEIF(ISTAT.EQ.-8)THEN
            STATUS='Sharp_bend'
            NCSTAT=10
       ELSEIF(ISTAT.EQ.-9)THEN
            STATUS='Energy_too_large'
            NCSTAT=16
       ELSEIF(ISTAT.GT.0.AND.ISTAT.LE.MXWIRE)THEN
            STATUS='Hit_'//WIRTYP(ISTAT)//'_Wire'
            NCSTAT=10
       ELSEIF(ISTAT.GT.MXWIRE.AND.ISTAT.LE.2*MXWIRE)THEN
            STATUS='Hit_'//WIRTYP(ISTAT-MXWIRE)//'_Replica'
            NCSTAT=13
       ELSE
            STATUS='Unknown'
            NCSTAT=7
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFMIN DEBUG   : From ('',
     -      3E15.8,'') Status = '',A,'' ('',I4,'') Time ='',E15.8,
     -      ''.'')') XPOS,YPOS,ZPOS,STATUS(1:NCSTAT),ISTAT,TU(NU)
       CALL STRBUF('STORE',IAUX,STATUS(1:NCSTAT),NCSTAT,IFAIL1)
       IF(IFAIL1.NE.0)PRINT *,' !!!!!! DRFMIN WARNING : Unable to'//
     -      ' store the status code string; trouble in case you use it.'
       VAR(10)=IAUX
       MODVAR(1)=2
       MODVAR(2)=2
       MODVAR(3)=2
       MODVAR(4)=2
       MODVAR(5)=2
       MODVAR(6)=2
       MODVAR(7)=2
       MODVAR(8)=2
       MODVAR(9)=2
       MODVAR(10)=1
*   Evaluate the selection function, skip the rest if FALSE.
       NREXP=1
       NVAR=10
       CALL ALGEXE(IENSEL,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
       IF(ABS(RES(1)).LT.1.0E-3)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Drift line rejected by'',
     -           '' the selection function.'')')
            GOTO 100
       ENDIF
*   Evaluate the function to be minimised.
       NREXP=1
       NVAR=9
       CALL ALGEXE(IENMIN,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Function value: '',E15.8)') RES(1)
*   Get rid of the status string.
       CALL STRBUF('DELETE',IAUX,STATUS,NCSTAT,IFAIL1)
*   Keep track of the 3 smallest numbers.
       IF(RES(1).LT.FT1.OR.ISET1.EQ.0)THEN
            FT3=FT2
            T3=T2
            I3=I2
            IF(ISET2.EQ.1)ISET3=1
            FT2=FT1
            T2=T1
            I2=I1
            IF(ISET1.EQ.1)ISET2=1
            FT1=RES(1)
            T1=TMIN+REAL(I)*(TMAX-TMIN)/REAL(NSTEP)
            I1=I
            ISET1=1
       ELSEIF(RES(1).LT.FT2.OR.ISET2.EQ.0)THEN
            FT3=FT2
            T3=T2
            I3=I2
            IF(ISET2.EQ.1)ISET3=1
            FT2=RES(1)
            T2=TMIN+REAL(I)*(TMAX-TMIN)/REAL(NSTEP)
            I2=I
            ISET2=1
       ELSEIF(RES(1).LT.FT3.OR.ISET3.EQ.0)THEN
            FT3=RES(1)
            T3=TMIN+REAL(I)*(TMAX-TMIN)/REAL(NSTEP)
            I3=I
            ISET3=1
       ENDIF
100    CONTINUE
*** Now make sure that we have 3 contiguous points.
       IF(ISET3.EQ.0)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : Failed to find a set of'//
     -           ' 3 initial points; no minimisation.'
            IF(LMINWR)WRITE(12,'(/''  # Minimisation not performed:'',
     -           '' number of starting points < 3.''/)')
            GOTO 3000
       ELSEIF(MAX(I1,I2,I3).NE.MIN(I1,I2,I3)+2)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : The initial set of 3'//
     -           ' minimal points is not consecutive; no minimisation.'
            IF(LMINWR)WRITE(12,'(/''  # Minimisation not performed:'',
     -           '' starting points are not consecutive.''/)')
            GOTO 3000
       ENDIF
*** And make a few parabolic steps.
       DO 120 I=1,NITMAX
*   Estimate parabolic minimum.
       TPARA=(  (FT1-FT2)*T3**2+(FT3-FT1)*T2**2+(FT2-FT3)*T1**2)/
     -      (2*((FT1-FT2)*T3   +(FT3-FT1)*T2   +(FT2-FT3)*T1))
       FTPARA=-(4*((FT1*T2**2-FT2*T1**2)*T3-(FT1*T2-FT2*T1)*T3**2-
     -      T2**2*FT3*T1+T2*FT3*T1**2)*((FT1-FT2)*T3-FT1*T2+
     -      T2*FT3+FT2*T1-FT3*T1)+((FT1-FT2)*T3**2-FT1*T2**2+T2**2*FT3+
     -      FT2*T1**2-FT3*T1**2)**2)/(4*((FT1-FT2)*T3-FT1*T2+
     -      T2*FT3+FT2*T1-FT3*T1)*(T3-T2)*(T3-T1)*(T2-T1))
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'('' ++++++ DRFMIN DEBUG   : Iteration '',
     -      I3//26X,''Point 1:  T='',E15.8,'' F='',E15.8/
     -      26X,''Point 2:  T='',E15.8,'' F='',E15.8/
     -      26X,''Point 3:  T='',E15.8,'' F='',E15.8//
     -      26X,''Parabola: T='',E15.8,'' F='',E15.8)')
     -      I,T1,FT1,T2,FT2,T3,FT3,TPARA,FTPARA
*   Check that the parabolic estimate is within range.
       IF((TMIN-TPARA)*(TPARA-TMAX).LT.0)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : Estimated parabolic'//
     -           ' minimum is located outside curve range.'
            IF(LMINWR)WRITE(12,'(/''  ! Minimisation abandoned:'',
     -           '' parabolic minimum outside of T-range.''/)')
            GOTO 3000
       ENDIF
*   Check that the new estimate doesn't coincide with an old point.
       IF(ABS(TPARA-T1).LT.EPSP*(EPSP+ABS(TPARA)).OR.
     -      ABS(TPARA-T2).LT.EPSP*(EPSP+ABS(TPARA)).OR.
     -      ABS(TPARA-T3).LT.EPSP*(EPSP+ABS(TPARA)))THEN
            IF(LPRINT)WRITE(LUNOUT,'(/''  Parabolic minimum'',
     -           '' coincides with a previous point.''/)')
            IF(LMINWR)WRITE(12,'(/''  Minimisation halted: parabolic'',
     -           '' minimum coincides with a previous point.''/)')
            GOTO 3000
       ENDIF
*   Evaluate things over there.
       VAR(1)=TPARA
       MODVAR(1)=2
       NVAR=1
       NREXP=2
       CALL ALGEXE(IENTRA,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
*   Then the drift-line from there.
       XPOS=RES(1)
       YPOS=RES(2)
       ZPOS=0
       IF(POLAR)THEN
            CALL CFMPTR(XPOS,YPOS,XPOS,YPOS,1,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! DRFMIN WARNING : Illegal polar'//
     -                ' curve coordinate seen at T=',VAR(1),
     -                '; no further minimisation.'
                 IF(LMINWR)WRITE(12,'(/''  # Minimisation abandoned:'',
     -                '' illegal polar coordinate seen.''/)')
                 GOTO 3000
            ENDIF
       ENDIF
       CALL DLCALC(XPOS,YPOS,ZPOS,QMIN,ITYPE)
       XPOS=RES(1)
       YPOS=RES(2)
*   And the derived information.
       VAR(1)=TU(NU)
       VAR(2)=0.0
       VAR(3)=0.0
       VAR(4)=0.0
       VAR(5)=0.0
       VAR(6)=0.0
       VAR(7)=0.0
       VAR(8)=0.0
       VAR(9)=0.0
       VAR(10)=0
       IF(LLENG)THEN
            DRLENG=0.0
            DO 130 IU=2,NU
            IF(POLAR)THEN
                 CALL CF2RTC(XU(IU-1),YU(IU-1),XAUX1,YAUX1,1)
                 CALL CF2RTC(XU(IU),YU(IU),XAUX2,YAUX2,1)
                 DRLENG=DRLENG+
     -                SQRT((XAUX2-XAUX1)**2+(YAUX2-YAUX1)**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ELSE
                 DRLENG=DRLENG+
     -                SQRT((XU(IU)-XU(IU-1))**2+(YU(IU)-YU(IU-1))**2+
     -                (ZU(IU)-ZU(IU-1))**2)
            ENDIF
130         CONTINUE
            VAR(2)=DRLENG
       ENDIF
       IF(LDIFF)CALL DLCDIF(VAR(3))
       IF(LTOWN)CALL DLCTWN(VAR(4))
       IF(LATTA)CALL DLCATT(VAR(5))
       IF(LFIELD)THEN
            CALL EFIELD(XPOS,YPOS,ZPOS,EX,EY,EZ,VAR(6),VAR(7),1,ILOC)
            IF(MAGOK)CALL BFIELD(XPOS,YPOS,ZPOS,BX,BY,BZ,VAR(8))
       ENDIF
       IF(LVELOC)THEN
            CALL DLCVEL(DBLE(XPOS),DBLE(YPOS),DBLE(ZPOS),
     -           F0,-1.0,1,ILOC)
            VAR(9)=REAL(SQRT(F0(1)**2+F0(2)**2+F0(3)**2))
       ENDIF
       IF(ISTAT.EQ.-1)THEN
            STATUS='Left_Area'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-2)THEN
            STATUS='Too_Many_Steps'
            NCSTAT=14
       ELSEIF(ISTAT.EQ.-3)THEN
            STATUS='Abandoned'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-4)THEN
            STATUS='Hit_Plane'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-5)THEN
            STATUS='Left_Drift_Medium'
            NCSTAT=17
       ELSEIF(ISTAT.EQ.-6)THEN
            STATUS='Left_Mesh'
            NCSTAT=9
       ELSEIF(ISTAT.EQ.-7)THEN
            STATUS='Attached'
            NCSTAT=8
       ELSEIF(ISTAT.EQ.-8)THEN
            STATUS='Sharp_bend'
            NCSTAT=10
       ELSEIF(ISTAT.EQ.-9)THEN
            STATUS='Energy_too_large'
            NCSTAT=16
       ELSEIF(ISTAT.GT.0.AND.ISTAT.LE.MXWIRE)THEN
            STATUS='Hit_'//WIRTYP(ISTAT)//'_Wire'
            NCSTAT=10
       ELSEIF(ISTAT.GT.MXWIRE.AND.ISTAT.LE.2*MXWIRE)THEN
            STATUS='Hit_'//WIRTYP(ISTAT-MXWIRE)//'_Replica'
            NCSTAT=13
       ELSEIF(ISTAT.GT.2*MXWIRE.AND.ISTAT.LE.2*MXWIRE+MXSOLI)THEN
            STATUS='Hit_'//SOLTYP(ISTAT-2*MXWIRE)//'_Solid'
            NCSTAT=11
       ELSE
            STATUS='Unknown'
            NCSTAT=7
       ENDIF
       CALL STRBUF('STORE',IAUX,STATUS(1:NCSTAT),NCSTAT,IFAIL1)
       IF(IFAIL1.NE.0)PRINT *,' !!!!!! DRFMIN WARNING : Unable to'//
     -      ' store the status code string; trouble in case you use it.'
       VAR(10)=IAUX
       MODVAR(1)=2
       MODVAR(2)=2
       MODVAR(3)=2
       MODVAR(4)=2
       MODVAR(5)=2
       MODVAR(6)=2
       MODVAR(7)=2
       MODVAR(8)=2
       MODVAR(9)=2
       MODVAR(10)=1
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ DRFMIN DEBUG   : From ('',
     -      3E15.8,'') Status = '',A,'' ('',I4,'') Time ='',E15.8,
     -      ''.'')') XPOS,YPOS,ZPOS,STATUS(1:NCSTAT),ISTAT,TU(NU)
*   Evaluate the selection function, skip the rest if FALSE.
       NREXP=1
       NVAR=10
       CALL ALGEXE(IENSEL,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
       IF(ABS(RES(1)).LT.1.0E-3)THEN
            PRINT *,' !!!!!! DRFMIN WARNING : Estimated parabolic'//
     -           ' minimum does not satisfy selection criterion.'
            WRITE(12,'(/''  ! Minimisation halted: parabolic'',
     -           '' minimum does not satisfy selection criterion.''/)')
            CALL STRBUF('DELETE',IAUX,STATUS,NCSTAT,IFAIL1)
            GOTO 3000
       ENDIF
*   Evaluate the function to be minimised.
       NREXP=1
       NVAR=9
       CALL ALGEXE(IENMIN,VAR,MODVAR,NVAR,RES,MODRES,NREXP,IFAIL)
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Function value: '',E15.8)') RES(1)
       FTPARA=RES(1)
*   Get rid of the status string.
       CALL STRBUF('DELETE',IAUX,STATUS,NCSTAT,IFAIL1)
*   Dataset output.
       IF(LMINWR)WRITE(12,'(''  Iteration '',I3,'' T='',E15.8,
     -      '': ('',E15.8,'','',E15.8,'','',E15.8,'') Function = '',
     -      E15.8,''.'')') I,TPARA,XPOS,YPOS,ZPOS,FTPARA
*   Normal printout.
       IF(LPRINT)WRITE(LUNOUT,'(''  Iteration '',I3,'' T='',E15.8,
     -      '': ('',E15.8,'','',E15.8,'','',E15.8,'') Function = '',
     -      E15.8,''.'')') I,TPARA,XPOS,YPOS,ZPOS,FTPARA
*   Check convergence.
       IF(ABS(FTPARA-FT1).LT.EPST*(ABS(FTPARA)+ABS(FT1)+EPST))THEN
            IF(LMINWR)WRITE(12,'(/''  Minimisation converged.''/)')
            IF(LPRINT)WRITE(LUNOUT,'(/''  Minimisation converged.''/)')
            GOTO 3000
       ENDIF
*   Store the value in the table.
       IF(FTPARA.LT.FT1)THEN
            FT3=FT2
            T3=T2
            FT2=FT1
            T2=T1
            FT1=FTPARA
            T1=TPARA
       ELSEIF(FTPARA.LT.FT2)THEN
            FT3=FT2
            T3=T2
            FT2=FTPARA
            T2=TPARA
       ELSEIF(FTPARA.LT.FT3)THEN
            FT3=FTPARA
            T3=TPARA
       ELSE
            IF(LMINWR)WRITE(12,'(''  # Minimisation abandoned:'',
     -           '' Estimated minimum is far from minimum found.'')')
            PRINT *,' !!!!!! DRFMIN WARNING : The estimated minimum'//
     -           ' is too far from the minimum found sofar.'
       ENDIF
120    CONTINUE
*** No convergence.
       PRINT *,' !!!!!! DRFMIN WARNING : No convergence after maximum'//
     -      ' number of steps.'
       PRINT *,'                         Current minimum F=',FT1
       PRINT *,'                         Found for T=',T1
       IF(LMINWR)WRITE(12,'(''  # Minimisation halted: maximum'',
     -      '' number of iterations reached.''/''  Current minimum '',
     -      '' at T='',E15.8,'', function= '',E15.8)') T1,FT1
*** Clean up.
3000   CONTINUE
*   Close the dataset if open.
       IF(LMINWR)CLOSE(12,ERR=2030,IOSTAT=IOS)
*   Display number of algebra errors.
       CALL ALGERR
*   Kill algebra entry points.
       CALL ALGCLR(IENTRA)
       CALL ALGCLR(IENMIN)
       CALL ALGCLR(IENSEL)
       RETURN
*   Errors while writing the dataset.
2010   CONTINUE
       PRINT *,' ###### DRFMIN ERROR   : Error error while writing'//
     -      ' to ',FILE(1:NCFILE),' via unit 12.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
*   Errors while closing the dataset.
2030   CONTINUE
       PRINT *,' ###### DRFMIN ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' unit 12 cannot be closed ; results not predictable'
       CALL INPIOS(IOS)
       END
