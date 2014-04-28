CDECK  ID>, MAPREA.
       SUBROUTINE MAPREA(IFAIL)
*-----------------------------------------------------------------------
*   MAPREA - Reads an interpolation table.
*   (Last changed on 24/ 3/12.)
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
       INTEGER IFAIL,IFAIL1,IFAIL2,IFAIL3,I,J,K,K0,
     -      NWORD,INEXT,INPCMP,NCMAP,IFORM,IFILE(MXWORD),NFILE,
     -      ISEL,INPTYP,IDATA(MXWORD),IWMAP(MXWORD),MAPMAX,MAPMXR,IORD,
     -      NCAUX,IDRDEF
       REAL EPSSEL,ZMMINR,ZMMAXR,XFMOFR,YFMOFR,ZFMOFR,AUX,UNITD
       CHARACTER*(MXNAME) FMAP
       CHARACTER*20 AUXSTR
       LOGICAL OK,FLAG(MXWORD+2),NEWDRM,OLDFL9,LHISMP,DELBKG
       EXTERNAL INPCMP,INPTYP
*** Inform that the routine has been called.
       IF(LIDENT)PRINT *,' /// ROUTINE MAPREA ///'
*** Preset error flag.
       IFAIL=1
*** Count words.
       CALL INPNUM(NWORD)
*** Without arguments, print current field map status.
       IF(NWORD.LE.1)THEN
            CALL MAPPRT
            IFAIL=0
            RETURN
       ENDIF
*** Preset all other parameters.
       NCMAP=0
       NFILE=0
       ISEL=0
       EPSSEL=-1
       NEWDRM=.FALSE.
       ZMMIN=-50
       ZMMAX=+50
       UNITD=1.0
       DELBKG=.TRUE.
       IORD=0
       PERX=.FALSE.
       PERY=.FALSE.
       PERZ=.FALSE.
       PERMX=.FALSE.
       PERMY=.FALSE.
       PERMZ=.FALSE.
       PERAX=.FALSE.
       PERAY=.FALSE.
       PERAZ=.FALSE.
       SETAX=.FALSE.
       SETAY=.FALSE.
       SETAZ=.FALSE.
       PERRX=.FALSE.
       PERRY=.FALSE.
       PERRZ=.FALSE.
       LHISMP=.FALSE.
       IFORM=0
*** Prepare for progress printing.
       CALL PROINT('FIELD-MAP',1,6)
       CALL PROFLD(1,'Reading command',-1.0)
       CALL PROSTA(1,0.0)
*** Scan for known keywords.
       DO 10 I=1,MXWORD+2
       FLAG(I)=.FALSE.
       IF(INPCMP(I,'FILE#S')+INPCMP(I,'RES#ET')+INPCMP(I,'Z-RAN#GE')+
     -      INPCMP(I,'DR#IFT-#MEDIUM')+
     -      INPCMP(I,'DEL#ETE-BACK#GROUND')+
     -      INPCMP(I,'KEEP-BACK#GROUND')+
     -      INPCMP(I,'X-PER#IODIC')+INPCMP(I,'X-MIR#ROR-PER#IODIC')+
     -      INPCMP(I,'Y-PER#IODIC')+INPCMP(I,'Y-MIR#ROR-PER#IODIC')+
     -      INPCMP(I,'Z-PER#IODIC')+INPCMP(I,'Z-MIR#ROR-PER#IODIC')+
     -      INPCMP(I,'X-AX#IALLY-PER#IODIC')+
     -      INPCMP(I,'Y-AX#IALLY-PER#IODIC')+
     -      INPCMP(I,'Z-AX#IALLY-PER#IODIC')+
     -      INPCMP(I,'NOT-X-PER#IODIC')+INPCMP(I,'NOT-Y-PER#IODIC')+
     -      INPCMP(I,'NOT-Z-PER#IODIC')+
     -      INPCMP(I,'NOPL#OT-MAP')+INPCMP(I,'PL#OT-MAP')+
     -      INPCMP(I,'NOHIST#OGRAM-#MAP')+INPCMP(I,'HIST#OGRAM-#MAP')+
     -      INPCMP(I,'LIN#EAR-#INTERPOLATION')+
     -      INPCMP(I,'QUA#DRATIC-#INTERPOLATION')+
     -      INPCMP(I,'CUB#IC-#INTERPOLATION')+
     -      INPCMP(I,'COMP#UTE-E#LECTRIC-#FIELD')+
     -      INPCMP(I,'INT#ERPOLATE-E#LECTRIC-#FIELD')+
     -      INPCMP(I,'MAX#WELL-PAR#AMETER-EX#TRACTOR-2D')+
     -      INPCMP(I,'PAR#AMETER-EX#TRACTOR-2D')+
     -      INPCMP(I,'MAX#WELL-PAR#AMETER-EX#TRACTOR-3D')+
     -      INPCMP(I,'PAR#AMETER-EX#TRACTOR-3D')+
     -      INPCMP(I,'MAX#WELL-F#IELD-SIM#ULATOR-#3D')+
     -      INPCMP(I,'F#IELD-SIM#ULATOR-#3D')+
     -      INPCMP(I,'MAX#WELL-11')+
     -      INPCMP(I,'MAX#WELL-2D-SV')+
     -      INPCMP(I,'FEMLAB-2D-LIN#EAR')+
     -      INPCMP(I,'COMSOL-2D-LIN#EAR')+
     -      INPCMP(I,'FEMLAB-2D-#QUADRATIC')+
     -      INPCMP(I,'COMSOL-2D-#QUADRATIC')+
     -      INPCMP(I,'FEMLAB-3D-LIN#EAR')+
     -      INPCMP(I,'COMSOL-3D-LIN#EAR')+
     -      INPCMP(I,'FEMLAB-3D-#QUADRATIC')+
     -      INPCMP(I,'COMSOL-3D-#QUADRATIC')+
     -      INPCMP(I,'FEMLAB-LIN#EAR-2D')+
     -      INPCMP(I,'COMSOL-LIN#EAR-2D')+
     -      INPCMP(I,'FEMLAB-QUAD#RATIC-2D')+
     -      INPCMP(I,'COMSOL-QUAD#RATIC-2D')+
     -      INPCMP(I,'FEMLAB-LIN#EAR-3D')+
     -      INPCMP(I,'COMSOL-LIN#EAR-3D')+
     -      INPCMP(I,'FEMLAB-QUAD#RATIC-3D')+
     -      INPCMP(I,'COMSOL-QUAD#RATIC-3D')+
     -      INPCMP(I,'TOSCA')+
     -      INPCMP(I,'TOSCA-118')+
     -      INPCMP(I,'UNIT#S')+
     -      INPCMP(I,'QUICKFIELD')+
     -      INPCMP(I,'ANSYS-#SOLID123')+
     -      INPCMP(I,'ANSYS-#SOLID-#123')+
     -      INPCMP(I,'ANSYS-PL#ANE121')+
     -      INPCMP(I,'ANSYS-PL#ANE-#121')+
     -      INPCMP(I,'OFFSET').GT.0.OR.I.GT.NWORD)
     -      FLAG(I)=.TRUE.
10     CONTINUE
*** Read the arguments,
       INEXT=2
       OK=.TRUE.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
**  File name.
       IF(INPCMP(I,'FILE#S').NE.0)THEN
*   Ensure that at least 1 is present.
            IF(I+1.GT.NWORD.OR.FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have an argument.')
                 OK=.FALSE.
            ELSE
*   Reset number of weighting fields.
                 NWMAP=0
*   Loop over the candidate names.
                 DO 30 J=I+1,NWORD
                 IF(J.LT.INEXT)GOTO 30
*   Skip remainder if a keyword.
                 IF(FLAG(J))THEN
                      INEXT=J
                      GOTO 40
*   Store the file name with contents and format.
                 ELSE
*   See whether this can be stored at all.
                      IF(NFILE.GE.MXWORD)THEN
                           CALL INPMSG(J,'Unable to store name.')
                           INEXT=J+1
                           GOTO 40
                      ENDIF
                      NFILE=NFILE+1
*   Will usually not be a weighting field.
                      IWMAP(NFILE)=0
*   Look for contents.
                      IF(INPCMP(J,'MESH').NE.0)THEN
                           IDATA(NFILE)=1
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'MODEL').NE.0)THEN
                           IDATA(NFILE)=-1
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'BACK#GROUND').NE.0)THEN
                           IDATA(NFILE)=-2
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'POT#ENTIAL')+
     -                     INPCMP(J,'VOLT#AGE').NE.0)THEN
                           IDATA(NFILE)=5
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'MAT#ERIAL')+
     -                     INPCMP(J,'D-#FIELD').NE.0)THEN
                           IDATA(NFILE)=9
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'E#LECTRIC-#FIELD').NE.0)THEN
                           IDATA(NFILE)=2
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'B-#FIELD')+
     -                     INPCMP(J,'MAG#NETIC-#FIELD').NE.0)THEN
                           IDATA(NFILE)=6
                           INEXT=J+1
                      ELSEIF(INPCMP(J,'W#EIGHTING-#FIELD').NE.0)THEN
                           IF(NWMAP+1.LE.MXWMAP)THEN
                                NWMAP=NWMAP+1
                                IWMAP(NFILE)=NWMAP
                                IDATA(NFILE)=10
                                INEXT=J+1
                           ELSE
                                CALL INPMSG(J,
     -                               'Too many weighting fields.')
                                INEXT=J+1
                                NFILE=NFILE-1
                                GOTO 40
                           ENDIF
                      ELSE
                           IDATA(NFILE)=0
                           INEXT=J
                      ENDIF
*   Pick up the file name.
                      IF(FLAG(INEXT).OR.INEXT.GT.NWORD)THEN
                           CALL INPMSG(J,'File name is missing.')
                           INEXT=J+1
                           GOTO 40
                      ENDIF
                      CALL INPSTR(INEXT,INEXT,FMAP,NCMAP)
                      CALL STRBUF('STORE',IFILE(NFILE),
     -                     FMAP,NCMAP,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           CALL INPMSG(INEXT,'String buffer error.')
                           IFILE(NFILE)=0
                      ENDIF
                      INEXT=INEXT+1
*   See whether there is a format etc.
                      K0=INEXT
                      IF(IDATA(NFILE).EQ.10)EWSTYP(IWMAP(NFILE))='?'
                      DO 60 K=K0,NWORD
                      IF(K.LT.INEXT)THEN
                           GOTO 60
                      ELSEIF(FLAG(K))THEN
                           INEXT=K
                           GOTO 40
                      ELSEIF(INPCMP(K,'SOL#IDS')+
     -                     INPCMP(K,'LAB#EL').NE.0)THEN
                           IF(FLAG(K+1).OR.K+1.GT.NWORD)THEN
                                CALL INPMSG(K,'Solid label missing.')
                                OK=.FALSE.
                           ELSEIF(IDATA(NFILE).NE.10)THEN
                                CALL INPMSG(K,'Only applicable to Ew.')
                           ELSE
                                CALL INPSTR(K+1,K+1,AUXSTR,NCAUX)
                                IF(INDEX('ABCDEFGHIJKLMNOPQRSTUVWXYZ',
     -                               AUXSTR(1:1)).EQ.0.OR.NCAUX.LE.
     -                               0)THEN
                                     CALL INPMSG(K+1,
     -                                    'The label must be a letter.')
                                     OK=.FALSE.
                                ELSE
                                     EWSTYP(IWMAP(NFILE))=AUXSTR(1:1)
                                ENDIF
                           ENDIF
                           INEXT=K+2
                      ELSE
                           INEXT=K
                           GOTO 30
                      ENDIF
60                    CONTINUE
                 ENDIF
*   Next file.
30               CONTINUE
                 INEXT=NWORD+1
*   Leave file loop.
40               CONTINUE
            ENDIF
**  Field map format.
       ELSEIF(INPCMP(I,'MAX#WELL-PAR#AMETER-EX#TRACTOR-2D')+
     -      INPCMP(I,'PAR#AMETER-EX#TRACTOR-2D').NE.0)THEN
            IFORM=1
       ELSEIF(INPCMP(I,'MAX#WELL-PAR#AMETER-EX#TRACTOR-3D')+
     -      INPCMP(I,'PAR#AMETER-EX#TRACTOR-3D').NE.0)THEN
            IFORM=2
       ELSEIF(INPCMP(I,'MAX#WELL-F#IELD-SIM#ULATOR-#3D')+
     -      INPCMP(I,'F#IELD-SIM#ULATOR-#3D').NE.0)THEN
            IFORM=4
       ELSEIF(INPCMP(I,'TOSCA-118').NE.0)THEN
            IFORM=13
       ELSEIF(INPCMP(I,'TOSCA').NE.0)THEN
            IFORM=5
       ELSEIF(INPCMP(I,'QUICKFIELD').NE.0)THEN
            IFORM=10
       ELSEIF(INPCMP(I,'ANSYS-#SOLID123')+
     -      INPCMP(I,'ANSYS-#SOLID-#123').NE.0)THEN
            IFORM=11
       ELSEIF(INPCMP(I,'ANSYS-PL#ANE121')+
     -      INPCMP(I,'ANSYS-PL#ANE-#121').NE.0)THEN
            IFORM=12
       ELSEIF(INPCMP(I,'MAX#WELL-2D-#SV').NE.0)THEN
            IFORM=6
       ELSEIF(INPCMP(I,'FEMLAB-2D-#QUADRATIC')+
     -      INPCMP(I,'COMSOL-2D-#QUADRATIC')+
     -      INPCMP(I,'FEMLAB-QUAD#RATIC-2D')+
     -      INPCMP(I,'COMSOL-QUAD#RATIC-2D').NE.0)THEN
            IFORM=8
       ELSEIF(INPCMP(I,'FEMLAB-2D-LIN#EAR')+
     -      INPCMP(I,'COMSOL-2D-LIN#EAR')+
     -      INPCMP(I,'FEMLAB-LIN#EAR-2D')+
     -      INPCMP(I,'COMSOL-LIN#EAR-2D').NE.0)THEN
            IFORM=14
       ELSEIF(INPCMP(I,'FEMLAB-3D-#QUADRATIC')+
     -      INPCMP(I,'COMSOL-3D-#QUADRATIC')+
     -      INPCMP(I,'FEMLAB-QUAD#RATIC-3D')+
     -      INPCMP(I,'COMSOL-QUAD#RATIC-3D').NE.0)THEN
            IFORM=7
       ELSEIF(INPCMP(I,'FEMLAB-3D-LIN#EAR')+
     -      INPCMP(I,'COMSOL-3D-LIN#EAR')+
     -      INPCMP(I,'FEMLAB-LIN#EAR-3D')+
     -      INPCMP(I,'COMSOL-LIN#EAR-3D').NE.0)THEN
            IFORM=15
       ELSEIF(INPCMP(I,'MAX#WELL-11').NE.0)THEN
            IFORM=9
**  Select a drift medium.
       ELSEIF(INPCMP(I,'DR#IFT-#MEDIUM').NE.0)THEN
            IF(FLAG(I+1).OR.I+1.GT.NWORD)THEN
                 CALL INPMSG(I,'Should have an argument')
                 OK=.FALSE.
            ELSEIF(INPTYP(I+1).EQ.1)THEN
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,ISEL,0)
                 IF(ISEL.NE.0)THEN
                      NEWDRM=.TRUE.
                      EPSSEL=-1
                 ELSE
                      CALL INPMSG(I+1,'Must be non-zero.')
                      OK=.FALSE.
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).EQ.2)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EPSSEL,-1.0)
                 IF(EPSSEL.GT.0)THEN
                      NEWDRM=.TRUE.
                      ISEL=0
                 ELSE
                      CALL INPMSG(I+1,'Must be > 0.')
                      OK=.FALSE.
                 ENDIF
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'SMALL#EST-#EPSILON')+
     -           INPCMP(I+1,'LOW#EST-#EPSILON')+
     -           INPCMP(I+1,'SMALL#EST-#SIGMA')+
     -           INPCMP(I+1,'LOW#EST-#SIGMA').NE.0)THEN
                 NEWDRM=.TRUE.
                 ISEL=1
                 EPSSEL=-1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'SEC#OND-SM#ALLEST-#EPSILON')+
     -           INPCMP(I+1,'ONE-BUT-SM#ALLEST-#EPSILON')+
     -           INPCMP(I+1,'SEC#OND-LOW#EST-#EPSILON')+
     -           INPCMP(I+1,'ONE-BUT-LOW#EST-#EPSILON')+
     -           INPCMP(I+1,'SEC#OND-SM#ALLEST-#SIGMA')+
     -           INPCMP(I+1,'ONE-BUT-SM#ALLEST-#SIGMA')+
     -           INPCMP(I+1,'SEC#OND-LOW#EST-#SIGMA')+
     -           INPCMP(I+1,'ONE-BUT-LOW#EST-#SIGMA').NE.0)THEN
                 NEWDRM=.TRUE.
                 ISEL=2
                 EPSSEL=-1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'LARG#EST-#EPSILON')+
     -           INPCMP(I+1,'BIG#GEST-#EPSILON')+
     -           INPCMP(I+1,'LARG#EST-#SIGMA')+
     -           INPCMP(I+1,'BIG#GEST-#SIGMA').NE.0)THEN
                 NEWDRM=.TRUE.
                 ISEL=-1
                 EPSSEL=-1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'SEC#OND-LARG#EST-#EPSILON')+
     -           INPCMP(I+1,'SEC#OND-BIG#GEST-#EPSILON')+
     -           INPCMP(I+1,'ONE-BUT-LARG#EST-#EPSILON')+
     -           INPCMP(I+1,'ONE-BUT-BIG#GEST-#EPSILON')+
     -           INPCMP(I+1,'SEC#OND-LARG#EST-#SIGMA')+
     -           INPCMP(I+1,'SEC#OND-BIG#GEST-#SIGMA')+
     -           INPCMP(I+1,'ONE-BUT-LARG#EST-#SIGMA')+
     -           INPCMP(I+1,'ONE-BUT-BIG#GEST-#SIGMA').NE.0)THEN
                 NEWDRM=.TRUE.
                 ISEL=-2
                 EPSSEL=-1
                 INEXT=I+2
            ELSE
                 CALL INPMSG(I+1,'Not a known keyword.')
                 OK=.FALSE.
                 INEXT=I+2
            ENDIF
**  Reset of the field maps.
       ELSEIF(INPCMP(I,'RES#ET').NE.0)THEN
            CALL MAPINT
            NEWDRM=.FALSE.
**  Periodicities.
       ELSEIF(INPCMP(I,'NOT-X-PER#IODIC').NE.0)THEN
            PERX=.FALSE.
            PERAX=.FALSE.
            PERMX=.FALSE.
            PERRX=.FALSE.
       ELSEIF(INPCMP(I,'NOT-Y-PER#IODIC').NE.0)THEN
            PERY=.FALSE.
            PERAY=.FALSE.
            PERMY=.FALSE.
            PERRY=.FALSE.
       ELSEIF(INPCMP(I,'NOT-Z-PER#IODIC').NE.0)THEN
            PERZ=.FALSE.
            PERAZ=.FALSE.
            PERMZ=.FALSE.
            PERRZ=.FALSE.
       ELSEIF(INPCMP(I,'X-PER#IODIC').NE.0)THEN
            PERX=.TRUE.
            PERMX=.FALSE.
       ELSEIF(INPCMP(I,'Y-PER#IODIC').NE.0)THEN
            PERY=.TRUE.
            PERMY=.FALSE.
       ELSEIF(INPCMP(I,'Z-PER#IODIC').NE.0)THEN
            PERZ=.TRUE.
            PERMZ=.FALSE.
       ELSEIF(INPCMP(I,'X-MIR#ROR-PER#IODIC').NE.0)THEN
            PERMX=.TRUE.
            PERX=.FALSE.
       ELSEIF(INPCMP(I,'Y-MIR#ROR-PER#IODIC').NE.0)THEN
            PERMY=.TRUE.
            PERY=.FALSE.
       ELSEIF(INPCMP(I,'Z-MIR#ROR-PER#IODIC').NE.0)THEN
            PERMZ=.TRUE.
            PERZ=.FALSE.
       ELSEIF(INPCMP(I,'X-AX#IALLY-PER#IODIC').NE.0)THEN
            PERAX=.TRUE.
       ELSEIF(INPCMP(I,'Y-AX#IALLY-PER#IODIC').NE.0)THEN
            PERAY=.TRUE.
       ELSEIF(INPCMP(I,'Z-AX#IALLY-PER#IODIC').NE.0)THEN
            PERAZ=.TRUE.
       ELSEIF(INPCMP(I,'X-ROT#ATIONALLY-SYMM#ETRIC').NE.0)THEN
            PERRX=.TRUE.
       ELSEIF(INPCMP(I,'Y-ROT#ATIONALLY-SYMM#ETRIC').NE.0)THEN
            PERRY=.TRUE.
       ELSEIF(INPCMP(I,'Z-ROT#ATIONALLY-SYMM#ETRIC').NE.0)THEN
            PERRZ=.TRUE.
**  Plotting options.
       ELSEIF(INPCMP(I,'PL#OT-MAP').NE.0)THEN
            LMAPPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-MAP').NE.0)THEN
            LMAPPL=.FALSE.
       ELSEIF(INPCMP(I,'HIST#OGRAM-#MAP').NE.0)THEN
            LHISMP=.TRUE.
       ELSEIF(INPCMP(I,'NOHIST#OGRAM-#MAP').NE.0)THEN
            LHISMP=.FALSE.
**  Interpolation orders.
       ELSEIF(INPCMP(I,'LIN#EAR-#INTERPOLATION').NE.0)THEN
            IORD=1
       ELSEIF(INPCMP(I,'QUA#DRATIC-#INTERPOLATION').NE.0)THEN
            IORD=2
       ELSEIF(INPCMP(I,'CUB#IC-#INTERPOLATION').NE.0)THEN
            IORD=3
**  Computation of Ex, Ey and Ez
       ELSEIF(INPCMP(I,'COMP#UTE-E#LECTRIC-#FIELD').NE.0)THEN
            LSFDER=.TRUE.
       ELSEIF(INPCMP(I,'INT#ERPOLATE-E#LECTRIC-#FIELD').NE.0)THEN
            LSFDER=.FALSE.
**  Specification of a range in z (for 2-dimensional field maps).
       ELSEIF(INPCMP(I,'Z-RAN#GE').NE.0)THEN
            IF(I+2.GT.NWORD.OR.FLAG(I+1).OR.FLAG(I+2).OR.
     -           (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2))THEN
                 CALL INPMSG(I,'Should have 2 arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPRDR(I+1,ZMMINR,ZMMIN)
                 CALL INPRDR(I+2,ZMMAXR,ZMMAX)
                 IF(ZMMINR.EQ.ZMMAXR)THEN
                      CALL INPMSG(I+1,'Zero range not permitted.')
                      CALL INPMSG(I+2,'See previous message.')
                 ELSEIF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0)THEN
                      ZMMIN=MIN(ZMMINR,ZMMAXR)
                      ZMMAX=MAX(ZMMINR,ZMMAXR)
                 ENDIF
            ENDIF
            INEXT=I+3
**  Specification of a coordinate offset.
       ELSEIF(INPCMP(I,'OFFSET').NE.0)THEN
            IF(I+3.GT.NWORD.OR.FLAG(I+1).OR.FLAG(I+2).OR.FLAG(I+3).OR.
     -           (INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2).OR.
     -           (INPTYP(I+2).NE.1.AND.INPTYP(I+2).NE.2).OR.
     -           (INPTYP(I+3).NE.1.AND.INPTYP(I+3).NE.2))THEN
                 CALL INPMSG(I,'Should have 3 arguments.')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPCHK(I+2,2,IFAIL2)
                 CALL INPCHK(I+3,2,IFAIL3)
                 CALL INPRDR(I+1,XFMOFR,XFMOFF)
                 CALL INPRDR(I+2,YFMOFR,YFMOFF)
                 CALL INPRDR(I+3,ZFMOFR,ZFMOFF)
                 IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.IFAIL3.EQ.0)THEN
                      XFMOFF=XFMOFR
                      YFMOFF=YFMOFR
                      ZFMOFF=ZFMOFR
                 ENDIF
            ENDIF
            INEXT=I+4
**  Space units (needed for Ansys).
       ELSEIF(INPCMP(I,'UNIT#S').NE.0)THEN
            IF(I+1.GT.NWORD.OR.FLAG(I+1))THEN
                 CALL INPMSG(I,'Should have an argument.')
                 OK=.FALSE.
            ELSEIF(INPCMP(I+1,'MICRO#N')+INPCMP(I+1,'MICRO#METRE')+
     -           INPCMP(I+1,'MICRO#METER').NE.0)THEN
                 UNITD=1.0E-4
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'MM')+INPCMP(I+1,'MILLI#METRE')+
     -           INPCMP(I+1,'MILLI#METER').NE.0)THEN
                 UNITD=1.0E-1
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'CM')+INPCMP(I+1,'CENTI#METRE')+
     -           INPCMP(I+1,'CENTI#METER').NE.0)THEN
                 UNITD=1.0
                 INEXT=I+2
            ELSEIF(INPCMP(I+1,'M')+INPCMP(I+1,'METRE')+
     -           INPCMP(I+1,'METER').NE.0)THEN
                 UNITD=100.0
                 INEXT=I+2
            ELSE
                 CALL INPMSG(I+1,'Not a known unit.')
                 OK=.FALSE.
            ENDIF
**  Background deletion.
       ELSEIF(INPCMP(I,'DEL#ETE-BACK#GROUND').NE.0)THEN
            DELBKG=.TRUE.
       ELSEIF(INPCMP(I,'KEEP-BACK#GROUND').NE.0)THEN
            DELBKG=.FALSE.
**  Other options not known.
       ELSE
            CALL INPMSG(I,'Not a known option')
            OK=.FALSE.
       ENDIF
20     CONTINUE
*** Print the error messages.
       CALL INPERR
*** Initial settings.
       MAPMAX=0
       OLDFL9=MAPFLG(9)
*** Read the model, mesh, background and field map files.
       DO 51 J=1,4
       IF(J.EQ.1)THEN
            CALL PROFLD(1,'Model file',-1.0)
       ELSEIF(J.EQ.2)THEN
            CALL PROFLD(1,'Background',-1.0)
       ELSEIF(J.EQ.3)THEN
            CALL PROFLD(1,'Mesh files',-1.0)
       ELSE
            CALL PROFLD(1,'Field maps',REAL(NFILE))
       ENDIF
       DO 50 I=1,NFILE
*   Progress print.
       CALL PROSTA(1,REAL(I))
*   Skip if this is not the appropriate type.
       IF(  (IDATA(I).NE.-1.AND.J.EQ.1).OR.
     -      (IDATA(I).NE.-2.AND.J.EQ.2).OR.
     -      (IDATA(I).NE. 1.AND.J.EQ.3).OR.
     -      ((IDATA(I).EQ.-1.OR.IDATA(I).EQ.-2.OR.
     -        IDATA(I).EQ. 1).AND.J.EQ.4))GOTO 50
*   Retrieve file name.
       CALL STRBUF('READ',IFILE(I),FMAP,NCMAP,IFAIL1)
*   Ensure that there was no string buffer error.
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPREA WARNING : String buffer error',
     -           ' retrieving name of file ',I,' to be read.'
            OK=.FALSE.
*   Be sure the name is not empty.
       ELSEIF(NCMAP.LT.1)THEN
            PRINT *,' !!!!!! MAPREA WARNING : File ',I,' to be',
     -           ' read has name of length zero; file not read.'
            OK=.FALSE.
       ELSE
*   And read the file.
            CALL PRORED(2)
            CALL MAPFMR(FMAP,NCMAP,IFORM,IDATA(I),IWMAP(I),DELBKG,
     -           MAPMXR,UNITD,IFAIL1)
            CALL PRORED(1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : File '//
     -                FMAP(1:NCMAP)//' could not be read.'
                 OK=.FALSE.
            ENDIF
            IF(MAPMAX.EQ.0)THEN
                 MAPMAX=MAPMXR
            ELSE
                 MAPMAX=MIN(MAPMAX,MAPMXR)
            ENDIF
       ENDIF
*   Delete the string.
       CALL STRBUF('DELETE',IFILE(I),' ',1,IFAIL1)
50     CONTINUE
51     CONTINUE
*** Final progress printing.
       CALL PROFLD(1,'Post processing',-1.0)
       CALL PROSTA(1,0.0)
*** Establish the final interpolation order.
       IF(MAPMAX.LE.0)THEN
            IF(NFILE.GE.1)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : Reading routines'//
     -                ' did not specify a maximum interpolation'//
     -                ' order; set to 1.'
                 MAPORD=1
            ENDIF
       ELSEIF(IORD.EQ.0)THEN
            MAPORD=MAPMAX
       ELSEIF(IORD.GT.MAPMAX)THEN
            OK=.FALSE.
            PRINT *,' !!!!!! MAPREA WARNING : Requested interpolation'//
     -           ' order exceeds field map granularity; set to maximum.'
            MAPORD=MAPMAX
       ELSE
            MAPORD=IORD
       ENDIF
*** Sort the epsilons if a new epsilon map has been provided.
       IF(MAPFLG(9).AND..NOT.OLDFL9)THEN
            CALL MAPEPS(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : Sorting the'//
     -                ' material properties failed.'
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Find the lowest positive epsilon.
       DO 150 I=1,NEPS
       IF(EPSMAT(I).GT.0)THEN
            IDRDEF=I
            GOTO 160
       ENDIF
150    CONTINUE
       IDRDEF=1
160    CONTINUE
*** Figure out which material is drift medium.
       IF(NEWDRM.AND..NOT.MAPFLG(9))THEN
            PRINT *,' !!!!!! MAPREA WARNING : Cannot set a drift'//
     -           ' medium since there are no material properties.'
            OK=.FALSE.
            IDRMAT=-1
       ELSEIF(NEWDRM.OR.(MAPFLG(9).AND..NOT.OLDFL9))THEN
            IF(NEPS.LT.1)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : No dielectric'//
     -                ' media found; cannot select drift medium.'
                 OK=.FALSE.
                 IDRMAT=-1
            ELSEIF(ISEL.GT.NEPS.OR.
     -           (ISEL.LT.0.AND.NEPS+ISEL+1.LE.0).OR.
     -           (ISEL.LT.0.AND.NEPS+ISEL+1.GT.NEPS))THEN
                 PRINT *,' !!!!!! MAPREA WARNING : Selection of'//
     -                ' dielectric constant via invalid sequence'//
     -                ' number; no assignment.'
                 OK=.FALSE.
                 IDRMAT=IDRDEF
            ELSEIF(ISEL.LT.0)THEN
                 IDRMAT=NEPS+ISEL+1
            ELSEIF(ISEL.EQ.0.AND.EPSSEL.LT.0)THEN
                 PRINT *,' ------ MAPREA MESSAGE : No drift medium'//
     -                ' has been selected ; choosing'
                 PRINT *,'                         the one with'//
     -                ' the lowest positive dielectric constant.'
                 IDRMAT=IDRDEF
            ELSEIF(ISEL.EQ.0)THEN
                 IDRMAT=IDRDEF
                 DO 130 I=1,NEPS
                 IF(ABS(EPSSEL-EPSMAT(I)).LT.
     -                ABS(EPSSEL-EPSMAT(IDRMAT)))IDRMAT=I
130              CONTINUE
                 PRINT *,' ------ MAPREA MESSAGE : Dielectric'//
     -                ' constant nearest to ',EPSSEL,' is ',
     -                EPSMAT(IDRMAT)
            ELSE
                 IDRMAT=ISEL
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPREA DEBUG   :'',
     -           '' Drift medium index='',I3)') IDRMAT
       ENDIF
*** Verify that there is no x or y axial symmetry in 2D.
       IF((PERAX.OR.PERAY).AND.MAPTYP.LT.10)THEN
            PRINT *,' !!!!!! MAPREA WARNING : Axial symmetry has been'//
     -           ' requested around x or y for a 2D map; reset.'
            PERAX=.FALSE.
            PERAY=.FALSE.
            OK=.FALSE.
       ENDIF
*** For rotational symmetries, ensure that the fields are present.
       IF((PERRX.AND.(PERRY.OR.PERRZ)).OR.
     -      (PERRY.AND.(PERRX.OR.PERRZ)).OR.
     -      (PERRZ.AND.(PERRX.OR.PERRY)))THEN
            PRINT *,' !!!!!! MAPREA WARNING : More than one'//
     -           ' rotational symmetry; reset.'
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ELSEIF((PERRX.OR.PERRY.OR.PERRZ).AND.MAPTYP.GE.11)THEN
            PRINT *,' !!!!!! MAPREA WARNING : Rotational symmetry'//
     -           ' declared for a 3D field map; reset.'
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ELSEIF((PERRX.AND.(MAPFLG(3).OR.MAPFLG(7))).OR.
     -      (PERRY.AND.(MAPFLG(4).OR.MAPFLG(8))).OR.
     -      (PERRZ.AND.(MAPFLG(3).OR.MAPFLG(7))))THEN
            PRINT *,' !!!!!! MAPREA WARNING : Rotational symmetry'//
     -           ' for the axis perpendicular to the map; reset.'
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ENDIF
*** Verify the ranges for axial symmetry have been set.
       IF(PERAX.AND.((.NOT.SETAX).OR.
     -      ABS(MOD(XAMAX-XAMIN,2*PI)).LT.0.01))THEN
            PRINT *,' !!!!!! MAPREA WARNING : Axial symmetry around x'//
     -           ' requested but range could not be set; reset.'
            PERAX=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(PERAY.AND.((.NOT.SETAY).OR.
     -      ABS(MOD(YAMAX-YAMIN,2*PI)).LT.0.01))THEN
            PRINT *,' !!!!!! MAPREA WARNING : Axial symmetry around y'//
     -           ' requested but range could not be set; reset.'
            PERAY=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(PERAZ.AND.((.NOT.SETAZ).OR.
     -      ABS(MOD(ZAMAX-ZAMIN,2*PI)).LT.0.01))THEN
            PRINT *,' !!!!!! MAPREA WARNING : Axial symmetry around z'//
     -           ' requested but range could not be set; reset.'
            PERAZ=.FALSE.
            OK=.FALSE.
       ENDIF
*** Correct the axial range if needed.
       IF(PERAX.AND.XAMAX-XAMIN.GT.PI)THEN
            AUX=XAMIN
            XAMIN=XAMAX
            XAMAX=AUX+2*PI
       ENDIF
       IF(PERAY.AND.YAMAX-YAMIN.GT.PI)THEN
            AUX=YAMIN
            YAMIN=YAMAX
            YAMAX=AUX+2*PI
       ENDIF
       IF(PERAZ.AND.ZAMAX-ZAMIN.GT.PI)THEN
            AUX=ZAMIN
            ZAMIN=ZAMAX
            ZAMAX=AUX+2*PI
       ENDIF
*** Verify that the range is a integral fraction of 2 pi.
       IF(PERAX)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPREA DEBUG   :'',
     -           '' x-Angular coverage: '',2F10.3/26X,
     -           ''Periods: '',F10.3)')
     -           180*XAMIN/PI,180*XAMAX/PI,ABS(2*PI/(XAMAX-XAMIN))
            IF(ABS(2*PI/(XAMAX-XAMIN)-ANINT(2*PI/(XAMAX-XAMIN))).GT.
     -           0.001.OR.ANINT(2*PI/(XAMAX-XAMIN)).LT.2)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : The map doesn''t'//
     -                ' cover an integral fraction of 2 pi around x;'//
     -                ' axial periodicity reset.'
                 PERAX=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
       IF(PERAY)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPREA DEBUG   :'',
     -           '' y-Angular coverage: '',2F10.3/26X,
     -           ''Periods: '',F10.3)')
     -           180*YAMIN/PI,180*YAMAX/PI,ABS(2*PI/(YAMAX-YAMIN))
            IF(ABS(2*PI/(YAMAX-YAMIN)-ANINT(2*PI/(YAMAX-YAMIN))).GT.
     -           0.001.OR.ANINT(2*PI/(YAMAX-YAMIN)).LT.2)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : The map doesn''t'//
     -                ' cover an integral fraction of 2 pi around y;'//
     -                ' axial periodicity reset.'
                 PERAY=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
       IF(PERAZ)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPREA DEBUG   :'',
     -           '' z-Angular coverage: '',2F10.3/26X,
     -           ''Periods: '',F10.3)')
     -           180*ZAMIN/PI,180*ZAMAX/PI,ABS(2*PI/(ZAMAX-ZAMIN))
            IF(ABS(2*PI/(ZAMAX-ZAMIN)-ANINT(2*PI/(ZAMAX-ZAMIN))).GT.
     -           0.001.OR.ANINT(2*PI/(ZAMAX-ZAMIN)).LT.2)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : The map doesn''t'//
     -                ' cover an integral fraction of 2 pi around z;'//
     -                ' axial periodicity reset.'
                 PERAZ=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Verify that the weighting field has received a label.
       DO 70 I=1,NWMAP
       IF(MAPFLG(10+4*I-3).OR.MAPFLG(11+4*I-3).OR.
     -      MAPFLG(12+4*I-3).OR.MAPFLG(13+4*I-3))THEN
            IF(EWSTYP(I).EQ.'?')THEN
                 PRINT *,' ------ MAPREA MESSAGE : Assigning label'//
     -                ' "S" to weighting field ',I
                 EWSTYP(I)='S'
            ENDIF
       ENDIF
70     CONTINUE
*** End of progress printing.
       CALL PROEND
*** Set magnetic field flag.
       IF(MAPFLG(6).OR.MAPFLG(7).OR.MAPFLG(8))THEN
            MAGOK=.TRUE.
            IF(MAGSRC.EQ.1)PRINT *,' ------ MAGREA MESSAGE : B field'//
     -           ' from &MAGNETIC replaced by a field map.'
            MAGSRC=2
            IF(GASSET)PRINT *,' ------ MAPREA MESSAGE : Previous gas'//
     -           ' data deleted.'
            GASSET=.FALSE.
       ELSEIF(MAGSRC.EQ.2)THEN
            PRINT *,' ------ MAGREA MESSAGE : The new field map has'//
     -           ' no magnetic field; currently no magnetic field.'
            MAGSRC=0
            MAGOK=.FALSE.
            IF(GASSET)PRINT *,' ------ MAPREA MESSAGE : Previous gas'//
     -           ' data deleted.'
            GASSET=.FALSE.
       ENDIF
*** Check the map if requested.
       IF(LHISMP)THEN
            CALL MAPCHK(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPREA WARNING : Histogramming'//
     -                ' found map errors ; map rejected.'
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Check that reading worked,
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### MAPREA ERROR   : Field maps reset'//
     -           ' because of the above errors.'
            CALL MAPINT
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### MAPREA ERROR   : Program terminated'//
     -           ' because of the above errors.'
            CALL QUIT
            RETURN
       ENDIF
*** Set the same limits for the cell.
       XMIN=XMMIN
       XMAX=XMMAX
       YMIN=YMMIN
       YMAX=YMMAX
       ZMIN=ZMMIN
       ZMAX=ZMMAX
       VMIN=VMMIN
       VMAX=VMMAX
       IF(PERX.OR.PERMX)SX=ABS(XMMAX-XMMIN)
       IF(PERY.OR.PERMY)SY=ABS(YMMAX-YMMIN)
       IF(PERZ.OR.PERMZ)SZ=ABS(ZMMAX-ZMMIN)
       IF(PERRX)THEN
            XMIN=YMMIN
            XMAX=YMMAX
            YMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            YMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            ZMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            ZMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
       ELSEIF(PERRY)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            YMIN=YMMIN
            YMAX=YMMAX
            ZMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            ZMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
       ELSEIF(PERRZ)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            YMIN=-MAX(ABS(XMMIN),ABS(XMMAX))
            YMAX=+MAX(ABS(XMMIN),ABS(XMMAX))
            ZMIN=YMMIN
            ZMAX=YMMAX
       ENDIF
       IF(PERAX)THEN
            YMIN=-MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
            YMAX=+MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMIN=-MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMAX=+MAX(ABS(YMMIN),ABS(YMMAX),ABS(ZMMIN),ABS(ZMMAX))
       ELSEIF(PERAY)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
            ZMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(ZMMIN),ABS(ZMMAX))
       ELSEIF(PERAZ)THEN
            XMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
            XMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
            YMIN=-MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
            YMAX=+MAX(ABS(XMMIN),ABS(XMMAX),ABS(YMMIN),ABS(YMMAX))
       ENDIF
*** Return with flag "successful".
       IFAIL=0
       END
