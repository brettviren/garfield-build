CDECK  ID>, GASBMC.
      SUBROUTINE GASBMC(IFAIL)
*-----------------------------------------------------------------------
*   GASBMC - Interface routine for Magboltz 7 called from Garfield.
*   (Last changed on  4/ 4/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL GLBVAL(MXVAR)
       INTEGER NGLB,GLBMOD(MXVAR)
       CHARACTER*10 GLBVAR(MXVAR)
       COMMON /GLBDAT/ GLBVAL,GLBMOD,NGLB
       COMMON /GLBCHR/ GLBVAR
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
*-----------------------------------------------------------------------
*   MAGPAR - Interface parameters for gas mixing with Magboltz.
*   (Last changed on  2/ 3/08.)
*-----------------------------------------------------------------------
       INTEGER MXGNAM
       PARAMETER(MXGNAM=60)
       DOUBLE PRECISION FRAMIX
       LOGICAL LF0PLT,LCSPLT,LGKEEP,LBMCPR
       COMMON /MAGPAR/ FRAMIX(MXGNAM),LF0PLT,LCSPLT,LGKEEP,LBMCPR
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
       DOUBLE PRECISION FRTOT
       REAL AUX,EPMIN,EPMAX,EPMINR,EPMAXR,SSTTHR,SSTHRR,
     -      BANGMN,BANGMX,BAMINR,BAMAXR,BTABMN,BTABMX,BTMINR,BTMAXR,
     -      VAR(MXVAR),RES(1),EFLDR,BFLDR,ANGR,
     -      VBOL,XBOL,YBOL,WBOL,DBOL,OBOL,ABOL,BBOL,SBOL(6),
     -      EXBOL(MXEXG),IOBOL(MXIOG)
       INTEGER INPTYP,INPCMP,MODVAR(MXVAR),MODRES(1),NCMOB,ISLOT,
     -      I,J,K,L,IFAIL,IFAIL1,IFAIL2,INEXT,NGASR,NBANGR,NBTABR,
     -      NWORD,NCOUT,NRES,NC,IENTRY,NNMAX,NMAXR,IGLB,NCSTR,
     -      MATSLT
       LOGICAL EPLOG,USE(MXVAR),OK,ESET,BSET,ASET
       CHARACTER*(MXCHAR) STRING
       CHARACTER*500 STR
       CHARACTER*20 OUTSTR
       CHARACTER*10 VARLIS(MXVAR)
       EXTERNAL INPTYP,INPCMP,MATSLT
*** Identify.
       IF(LIDENT)PRINT *,' /// ROUTINE GASBMC ///'
       PRINT *,' ------ GASBMC MESSAGE : Calling interfaced Magboltz'//
     -      ' version 7.08, last changed on 1/3/12.'
*** Initial values for the E/p scale.
       EPMIN=100.0/PGAS
       EPMAX=100000.0/PGAS
       EPLOG=.TRUE.
       NGAS=20
       ESET=.FALSE.
*   E-B angles.
       IF(MAGOK)THEN
            BANGMN=0
            BANGMX=PI/2
            NBANG=4
       ELSE
            BANGMN=PI/2
            BANGMX=PI/2
            NBANG=1
       ENDIF
       ASET=.FALSE.
*   B field magnitude.
       IF(MAGOK)THEN
            IF(ABS((BFMIN-BFMAX)*BSCALE).LT.0.0001)THEN
                 BTABMN=BFMIN*BSCALE
                 BTABMX=BFMAX*BSCALE
                 NBTAB=1
            ELSE
                 BTABMN=BFMIN*BSCALE
                 BTABMX=BFMAX*BSCALE
                 NBTAB=6
            ENDIF
       ELSE
            BTABMN=0
            BTABMX=0
            NBTAB=1
       ENDIF
       BSET=.FALSE.
*   Plotting distribution functions.
       LF0PLT=.FALSE.
*   Cross section plot.
       LCSPLT=.FALSE.
*   Saving cross section and energy distribution data.
       LGKEEP=.FALSE.
*   Printing Magboltz output.
       LBMCPR=.FALSE.
*   Mobility.
       VARLIS(1)='EP'
       NCMOB=0
*   MC accuracy parameters.
       NNMAX=2
*   SST threshold.
C       SSTTHR=60
*   Next line changed at the request of Steve Biagi (6/9/2007)
       SSTTHR=30.0D0
*** Prepare for progress printing.
       CALL PROINT('MAGBOLTZ',1,6)
*** Preset failure flag to 0: success.
       IFAIL=0
*** Preset the gas mixture fractions.
       DO 10 I=1,MXGNAM
       FRAMIX(I)=0.0
10     CONTINUE
*** Read the command line.
       CALL PROFLD(1,'Reading command',-1.0)
       CALL PROSTA(1,0.0)
       CALL INPNUM(NWORD)
       INEXT=2
*   Control of whether to proceed.
       OK=.TRUE.
       DO 20 I=2,NWORD
       IF(I.LT.INEXT)GOTO 20
*** Fractions, CF4 (current data).
       IF(INPCMP(I,'CF4')+
     -      INPCMP(I,'FREON-#14')+
     -      INPCMP(I,'TETRAFLUOROMETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(1)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Argon (isotropic).
       ELSEIF(INPCMP(I,'AR#GON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(2)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 4 (isotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-4-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(3)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 3 (isotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-3-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(4)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Neon (isotropic).
       ELSEIF(INPCMP(I,'NEON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(5)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Krypton (isotropic).
       ELSEIF(INPCMP(I,'KR#YPTON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(6)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Xenon (isotropic).
       ELSEIF(INPCMP(I,'XE#NON-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(7)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Methane (only version, isotropic).
       ELSEIF(INPCMP(I,'METHANE')+
     -      INPCMP(I,'CH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(8)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ethane.
       ELSEIF(INPCMP(I,'ETHANE')+
     -      INPCMP(I,'C2H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(9)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Propane.
       ELSEIF(INPCMP(I,'PROPA#NE')+
     -      INPCMP(I,'C3H8').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(10)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Isobutane.
       ELSEIF(INPCMP(I,'ISO#BUTANE')+
     -      INPCMP(I,'IC4H10')+
     -      INPCMP(I,'ISO-C4H10')+
     -      INPCMP(I,'C4H10').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(11)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CO2 (isotropic).
       ELSEIF(INPCMP(I,'CO2')+
     -      INPCMP(I,'CARB#ON-DIOX#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(12)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Neopentane (current data)
       ELSEIF(INPCMP(I,'NEOPENT#ANE')+
     -      INPCMP(I,'NEO-PENT#ANE')+
     -      INPCMP(I,'NEO-C5H12')+
     -      INPCMP(I,'C5H12').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(13)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Water (current data).
       ELSEIF(INPCMP(I,'WA#TER-#VAPOUR')+
     -      INPCMP(I,'H2O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(14)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Oxygen (current data).
       ELSEIF(INPCMP(I,'OX#YGEN')+
     -      INPCMP(I,'O2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(15)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitrogen (isotropic).
       ELSEIF(INPCMP(I,'NI#TROGEN-ISO#TROPIC')+
     -      INPCMP(I,'N2-ISO#TROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(16)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitric oxide (NO).
       ELSEIF(INPCMP(I,'NITRI#C-OX#IDE')+
     -      INPCMP(I,'NITROGEN-MONOXIDE')+
     -      INPCMP(I,'NO').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(17)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitrous oxide (N2O).
       ELSEIF(INPCMP(I,'NITRO#US-OX#IDE')+
     -      INPCMP(I,'DINITROGEN-MONOXIDE')+
     -      INPCMP(I,'LAUGHING-GAS')+
     -      INPCMP(I,'N2O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(18)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ethene (C2H4).
       ELSEIF(INPCMP(I,'ETHE#NE')+
     -      INPCMP(I,'ETHYLENE')+
     -      INPCMP(I,'C2H4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(19)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Acetylene (C2H2).
       ELSEIF(INPCMP(I,'ACETYL#ENE')+
     -      INPCMP(I,'ETHYNE')+
     -      INPCMP(I,'C2H2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(20)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Hydrogen.
       ELSEIF(INPCMP(I,'HYDROGEN')+
     -      INPCMP(I,'H2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(21)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Deuterium.
       ELSEIF(INPCMP(I,'DEUTERIUM')+
     -      INPCMP(I,'D2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(22)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Carbon monoxide (CO).
       ELSEIF(INPCMP(I,'CO')+
     -      INPCMP(I,'CARB#ON-MONOX#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(23)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Methylal (dimethoxymethane, CH3-O-CH2-O-CH3, "hot" version).
       ELSEIF(INPCMP(I,'METHY#LAL-#HOT')+
     -      INPCMP(I,'DIMETHOXYMETHANE-#HOT')+
     -      INPCMP(I,'DMM-#HOT')+
     -      INPCMP(I,'C3H8O2-#HOT').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(24)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   DME.
       ELSEIF(INPCMP(I,'DME')+
     -      INPCMP(I,'DIMETHYL-ETHER')+
     -      INPCMP(I,'METHOXYMETHANE')+
     -      INPCMP(I,'METHYL-ETHER')+
     -      INPCMP(I,'WOOD-ETHER')+
     -      INPCMP(I,'C2H6O').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(25)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Reid step.
       ELSEIF(INPCMP(I,'REID-STEP').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(26)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Maxwell model.
       ELSEIF(INPCMP(I,'MAXWELL-MODEL').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(27)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Reid ramp.
       ELSEIF(INPCMP(I,'REID-RAMP').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(28)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   C2F6.
       ELSEIF(INPCMP(I,'C2F6')+
     -      INPCMP(I,'FREON-116')+
     -      INPCMP(I,'ZYRON-116-#N5')+
     -      INPCMP(I,'HEXAFLUOROETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(29)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   SF6.
       ELSEIF(INPCMP(I,'SF6')+
     -      INPCMP(I,'SULPHUR-HEXAFLUORIDE')+
     -      INPCMP(I,'SULFUR-HEXAFLUORIDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(30)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   NH3.
       ELSEIF(INPCMP(I,'AMMONIA')+
     -      INPCMP(I,'NH3').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(31)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Propene.
       ELSEIF(INPCMP(I,'PROPE#NE')+
     -      INPCMP(I,'PROPYLENE')+
     -      INPCMP(I,'C3H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(32)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Cyclopropane.
       ELSEIF(INPCMP(I,'C#YCLO-PROPA#NE')+
     -      INPCMP(I,'CYCLOPROPA#NE')+
     -      INPCMP(I,'C#YCLO-C3H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(33)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Methanol.
       ELSEIF(INPCMP(I,'METHANOL')+
     -      INPCMP(I,'METHYL-ALCOHOL')+
     -      INPCMP(I,'WOOD-ALCOHOL')+
     -      INPCMP(I,'CH3OH').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(34)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ethanol.
       ELSEIF(INPCMP(I,'ETHANOL')+
     -      INPCMP(I,'ETHYL-ALCOHOL')+
     -      INPCMP(I,'GRAIN-ALCOHOL')+
     -      INPCMP(I,'C2H5OH').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(35)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Propanol.
       ELSEIF(INPCMP(I,'PROPANOL')+
     -      INPCMP(I,'2-PROP#ANOL')+
     -      INPCMP(I,'ISO-PROP#ANOL')+
     -      INPCMP(I,'ISOPROP#ANOL')+
     -      INPCMP(I,'ISOPROPYL-#ALCOHOL')+
     -      INPCMP(I,'C3H7OH').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(36)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Cesium / Caesium.
       ELSEIF(INPCMP(I,'CE#SIUM')+
     -      INPCMP(I,'CAE#SIUM')+
     -      INPCMP(I,'CS').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(37)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Fluorine.
       ELSEIF(INPCMP(I,'FLUOR#INE')+
     -      INPCMP(I,'F2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(38)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CS2.
       ELSEIF(INPCMP(I,'CS2')+
     -      INPCMP(I,'C#ARBON-DISULPH#IDE')+
     -      INPCMP(I,'C#ARBON-DISULF#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(39)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   COS.
       ELSEIF(INPCMP(I,'COS')+
     -      INPCMP(I,'C#ARBONYL-SULPH#IDE')+
     -      INPCMP(I,'C#ARBONYL-SULF#IDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(40)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Deuterated methane.
       ELSEIF(INPCMP(I,'DEUT#ERIUM-METHANE')+
     -      INPCMP(I,'DEUT#ERATED-METHANE')+
     -      INPCMP(I,'CD4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(41)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   BF3.
       ELSEIF(INPCMP(I,'BF3')+
     -      INPCMP(I,'BORON-TRIFLUORIDE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(42)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   C2HF5 and C2H2F4.
       ELSEIF(INPCMP(I,'C2HF5')+
     -      INPCMP(I,'C2H2F4')+
     -      INPCMP(I,'C2F5H')+
     -      INPCMP(I,'C2F4H2')+
     -      INPCMP(I,'FREON-134-#A')+
     -      INPCMP(I,'FREON-125')+
     -      INPCMP(I,'ZYRON-125')+
     -      INPCMP(I,'TETRAFLUOROETHANE')+
     -      INPCMP(I,'PENTAFLUOROETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(43)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 3 (anisotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-3-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(44)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Helium 4 (anisotropic).
       ELSEIF(INPCMP(I,'HE#LIUM-#4-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(45)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Neon (anisotropic).
       ELSEIF(INPCMP(I,'NEON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(46)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Argon (anisotropic).
       ELSEIF(INPCMP(I,'AR#GON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(47)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Krypton (anisotropic).
       ELSEIF(INPCMP(I,'KR#YPTON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(48)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Xenon (anisotropic).
       ELSEIF(INPCMP(I,'XE#NON-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(49)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CHF3.
       ELSEIF(INPCMP(I,'CHF3')+
     -      INPCMP(I,'FREON-23')+
     -      INPCMP(I,'TRIFLUOROMETHANE').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(50)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   CF3Br.
       ELSEIF(INPCMP(I,'CF3BR')+
     -      INPCMP(I,'TRIFLUOROBROMOMETHANE')+
     -      INPCMP(I,'HALON-1301')+
     -      INPCMP(I,'FREON-13B1').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(51)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   C3F8.
       ELSEIF(INPCMP(I,'C3F8')+
     -      INPCMP(I,'OCTAFLUOROPROPANE')+
     -      INPCMP(I,'R218')+
     -      INPCMP(I,'FREON-218')+
     -      INPCMP(I,'PERFLUOROPROPANE')+
     -      INPCMP(I,'RC-218')+
     -      INPCMP(I,'PFC-218').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(52)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Ozone.
       ELSEIF(INPCMP(I,'OZONE')+
     -      INPCMP(I,'O3').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(53)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Mercury.
       ELSEIF(INPCMP(I,'MERCURY')+
     -      INPCMP(I,'HG')+
     -      INPCMP(I,'HG2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(54)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   H2S.
       ELSEIF(INPCMP(I,'H2S')+
     -      INPCMP(I,'HYDROGEN-SULPHIDE')+
     -      INPCMP(I,'HYDROGEN-SULFIDE')+
     -      INPCMP(I,'HEPATIC-ACID')+
     -      INPCMP(I,'SEWER-GAS')+
     -      INPCMP(I,'SULFUR-HYDRIDE')+
     -      INPCMP(I,'DIHYDROGEN-MONOSULFIDE')+
     -      INPCMP(I,'DIHYDROGEN-MONOSULPHIDE')+
     -      INPCMP(I,'SULPHUR-HYDRIDE')+
     -      INPCMP(I,'STINK-DAMP')+
     -      INPCMP(I,'SULFURETED-HYDROGEN ').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(55)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   n-butane.
       ELSEIF(INPCMP(I,'N-BUTANE')+
     -      INPCMP(I,'N-C4H10').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(56)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   n-pentane.
       ELSEIF(INPCMP(I,'N-PENTANE')+
     -      INPCMP(I,'N-C5H12').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(57)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Nitrogen.
       ELSEIF(INPCMP(I,'NI#TROGEN-#ANISOTROPIC')+
     -      INPCMP(I,'N2-#ANISOTROPIC').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(58)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Germane, GeH4.
       ELSEIF(INPCMP(I,'GERM#ANE')+
     -      INPCMP(I,'GERMANIUM-HYDRIDE')+
     -      INPCMP(I,'GERMANIUM-TETRAHYDRIDE')+
     -      INPCMP(I,'GERMANOMETHANE')+
     -      INPCMP(I,'MONOGERMANE')+
     -      INPCMP(I,'GEH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(59)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Silane, SiH4.
       ELSEIF(INPCMP(I,'SIL#ANE')+
     -      INPCMP(I,'SILICON-HYDRIDE')+
     -      INPCMP(I,'SILICON-TETRAHYDRIDE')+
     -      INPCMP(I,'SILICANE')+
     -      INPCMP(I,'MONOSILANE')+
     -      INPCMP(I,'SIH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,AUX,0.0)
                 FRAMIX(60)=AUX
                 IF(IFAIL1.NE.0)OK=.FALSE.
            ENDIF
            INEXT=I+2
*   Range of E/p.
       ELSEIF(INPCMP(I,'E/P-RAN#GE').NE.0)THEN
            CALL INPCHK(I+1,2,IFAIL1)
            CALL INPCHK(I+2,2,IFAIL2)
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.NWORD.GE.I+2)THEN
                 CALL INPRDR(I+1,EPMINR,EPMIN)
                 CALL INPRDR(I+2,EPMAXR,EPMAX)
                 IF(EPMINR.NE.EPMAXR.AND.EPMINR.GT.0.0.AND.
     -                EPMAXR.GT.0.0)THEN
                      EPMIN=MIN(EPMINR,EPMAXR)
                      EPMAX=MAX(EPMINR,EPMAXR)
                 ELSE
                      CALL INPMSG(I+1,'Zero range and negative values')
                      CALL INPMSG(I+2,'are not permitted in RANGE.   ')
                      OK=.FALSE.
                 ENDIF
            ELSE
                 CALL INPMSG(I,'Missing or invalid arguments. ')
                 OK=.FALSE.
            ENDIF
            ESET=.FALSE.
            INEXT=I+3
*   Listed E/p values.
       ELSEIF(INPCMP(I,'E/P').NE.0)THEN
            IF(INPTYP(I+1).EQ.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 DO 250 IGLB=1,NGLB
                 IF(STRING(1:NCSTR).EQ.GLBVAR(IGLB))THEN
                      ISLOT=MATSLT(NINT(GLBVAL(IGLB)))
                      IF(GLBMOD(IGLB).NE.5)THEN
                           CALL INPMSG(I+1,'Not of type Matrix.')
                           OK=.FALSE.
                      ELSEIF(ISLOT.LE.0)THEN
                           CALL INPMSG(I+1,'Matrix inaccessible.')
                           OK=.FALSE.
                      ELSEIF(MLEN(ISLOT).GT.MXLIST)THEN
                           CALL INPMSG(I+1,'More than MXLIST elements.')
                           OK=.FALSE.
                      ELSE
                           NGAS=MLEN(ISLOT)
                           DO 270 J=1,NGAS
                           IF(MVEC(MORG(ISLOT)+J).LE.0)THEN
                                CALL INPMSG(I+1,'Contains values <= 0.')
                                OK=.FALSE.
                                GOTO 260
                           ELSE
                                EGAS(J)=MVEC(MORG(ISLOT)+J)
                           ENDIF
270                        CONTINUE
                           ESET=.TRUE.
                      ENDIF
                      GOTO 260
                 ENDIF
250              CONTINUE
                 CALL INPMSG(I+1,'Not a global variable.')
                 OK=.FALSE.
260              CONTINUE
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).EQ.1.OR.INPTYP(I+1).EQ.2)THEN
                 NGAS=0
                 DO 280 J=I+1,NWORD
                 IF(INPTYP(J).NE.1.AND.INPTYP(J).NE.2)THEN
                      INEXT=J
                      GOTO 290
                 ELSEIF(NGAS+1.GT.MXLIST)THEN
                      CALL INPMSG(J,'Too many values, ignored.')
                      OK=.FALSE.
                      GOTO 280
                 ENDIF
                 CALL INPCHK(J,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J,EFLDR,0.0)
                      IF(EFLDR.GT.0.0)THEN
                           NGAS=NGAS+1
                           EGAS(NGAS)=EFLDR
                      ELSE
                           CALL INPMSG(J,'Not strictly positive.')
                           OK=.FALSE.
                      ENDIF
                 ELSE
                      CALL INPMSG(J,'Invalid field value.')
                      OK=.FALSE.
                 ENDIF
280              CONTINUE
                 INEXT=NWORD+1
290              CONTINUE
                 ESET=.TRUE.
            ELSE
                 CALL INPMSG(I,'Invalid field specification.')
                 OK=.FALSE.
                 INEXT=I+2
            ENDIF
*   Range of E.
       ELSEIF(INPCMP(I,'E#LECTRIC-RAN#GE')+
     -      INPCMP(I,'E#LECTRIC-F#IELD-RAN#GE').NE.0)THEN
            CALL INPCHK(I+1,2,IFAIL1)
            CALL INPCHK(I+2,2,IFAIL2)
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.NWORD.GE.I+2)THEN
                 CALL INPRDR(I+1,EPMINR,EPMIN*PGAS)
                 CALL INPRDR(I+2,EPMAXR,EPMAX*PGAS)
                 IF(EPMINR.NE.EPMAXR.AND.EPMINR.GT.0.0.AND.
     -                EPMAXR.GT.0.0)THEN
                      EPMIN=MIN(EPMINR,EPMAXR)/PGAS
                      EPMAX=MAX(EPMINR,EPMAXR)/PGAS
                 ELSE
                      CALL INPMSG(I+1,'Zero range and negative values')
                      CALL INPMSG(I+2,'are not permitted in RANGE.   ')
                      OK=.FALSE.
                 ENDIF
            ELSE
                 CALL INPMSG(I,'Missing or invalid arguments. ')
                 OK=.FALSE.
            ENDIF
            ESET=.FALSE.
            INEXT=I+3
*   Listed values of E.
       ELSEIF(INPCMP(I,'E#LECTRIC-FIELD').NE.0)THEN
            IF(INPTYP(I+1).EQ.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 DO 200 IGLB=1,NGLB
                 IF(STRING(1:NCSTR).EQ.GLBVAR(IGLB))THEN
                      ISLOT=MATSLT(NINT(GLBVAL(IGLB)))
                      IF(GLBMOD(IGLB).NE.5)THEN
                           CALL INPMSG(I+1,'Not of type Matrix.')
                           OK=.FALSE.
                      ELSEIF(ISLOT.LE.0)THEN
                           CALL INPMSG(I+1,'Matrix inaccessible.')
                           OK=.FALSE.
                      ELSEIF(MLEN(ISLOT).GT.MXLIST)THEN
                           CALL INPMSG(I+1,'More than MXLIST elements.')
                           OK=.FALSE.
                      ELSE
                           NGAS=MLEN(ISLOT)
                           DO 220 J=1,NGAS
                           IF(MVEC(MORG(ISLOT)+J).LE.0)THEN
                                CALL INPMSG(I+1,'Contains values <= 0.')
                                OK=.FALSE.
                                GOTO 210
                           ELSE
                                EGAS(J)=MVEC(MORG(ISLOT)+J)/PGAS
                           ENDIF
220                        CONTINUE
                           ESET=.TRUE.
                      ENDIF
                      GOTO 210
                 ENDIF
200              CONTINUE
                 CALL INPMSG(I+1,'Not a global variable.')
                 OK=.FALSE.
210              CONTINUE
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).EQ.1.OR.INPTYP(I+1).EQ.2)THEN
                 NGAS=0
                 DO 230 J=I+1,NWORD
                 IF(INPTYP(J).NE.1.AND.INPTYP(J).NE.2)THEN
                      INEXT=J
                      GOTO 240
                 ELSEIF(NGAS+1.GT.MXLIST)THEN
                      CALL INPMSG(J,'Too many values, ignored.')
                      OK=.FALSE.
                      GOTO 230
                 ENDIF
                 CALL INPCHK(J,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J,EFLDR,0.0)
                      IF(EFLDR.GT.0.0)THEN
                           NGAS=NGAS+1
                           EGAS(NGAS)=EFLDR/PGAS
                      ELSE
                           CALL INPMSG(J,'Not strictly positive.')
                           OK=.FALSE.
                      ENDIF
                 ELSE
                      CALL INPMSG(J,'Invalid field value.')
                      OK=.FALSE.
                 ENDIF
230              CONTINUE
                 INEXT=NWORD+1
240              CONTINUE
                 ESET=.TRUE.
            ELSE
                 CALL INPMSG(I,'Invalid field specification.')
                 OK=.FALSE.
                 INEXT=I+2
            ENDIF
*   Plot distribution functions.
       ELSEIF(INPCMP(I,'PL#OT-DIST#RIBUTION-#FUNCTIONS').NE.0)THEN
            LF0PLT=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-DIST#RIBUTION-#FUNCTIONS').NE.0)THEN
            LF0PLT=.FALSE.
*   Plot cross sections.
       ELSEIF(INPCMP(I,'PL#OT-CR#OSS-#SECTIONS').NE.0)THEN
            LCSPLT=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-CR#OSS-#SECTIONS').NE.0)THEN
            LCSPLT=.FALSE.
*   Keep cross section and distribution functions.
       ELSEIF(INPCMP(I,'KEEP').NE.0)THEN
            LGKEEP=.TRUE.
       ELSEIF(INPCMP(I,'NOKEEP').NE.0)THEN
            LGKEEP=.FALSE.
*   Keep cross section and distribution functions.
       ELSEIF(INPCMP(I,'PRINT').NE.0)THEN
            LBMCPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPRINT').NE.0)THEN
            LBMCPR=.FALSE.
*   Kind of E/p scale.
       ELSEIF(INPCMP(I,'LIN#EAR-#E/P-#SCALE')+
     -      INPCMP(I,'LIN#EAR-#ELECTRIC-#SCALE')+
     -      INPCMP(I,'LIN#EAR-#ELECTRIC-#FIELD-#SCALE').NE.0)THEN
            EPLOG=.FALSE.
            ESET=.FALSE.
       ELSEIF(INPCMP(I,'LOG#ARITHMIC-#E/P-#SCALE')+
     -      INPCMP(I,'LOG#ARITHMIC-#ELECTRIC-#SCALE')+
     -      INPCMP(I,'LOG#ARITHMIC-#ELECTRIC-#FIELD-#SCALE').NE.0)THEN
            EPLOG=.TRUE.
            ESET=.FALSE.
*   Number of points.
       ELSEIF(INPCMP(I,'N-E#/P')+
     -      INPCMP(I,'N-E#LECTRIC-#FIELD').NE.0)THEN
            IF(INPTYP(I+1).NE.1.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.NE.0)OK=.FALSE.
                 CALL INPRDI(I+1,NGASR,20)
                 IF(NGASR.LE.0.OR.NGASR.GT.MXLIST)THEN
                      CALL INPMSG(I+1,'Value is out of range.        ')
                      OK=.FALSE.
                 ELSE
                      NGAS=NGASR
                 ENDIF
            ENDIF
            ESET=.FALSE.
            INEXT=I+2
*   Range of E-B angle.
       ELSEIF(MAGOK.AND.INPCMP(I,'ANG#LE-RAN#GE').NE.0)THEN
            CALL INPCHK(I+1,2,IFAIL1)
            CALL INPCHK(I+2,2,IFAIL2)
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.NWORD.GE.I+2)THEN
                 CALL INPRDR(I+1,BAMINR,180*BANGMN/PI)
                 CALL INPRDR(I+2,BAMAXR,180*BANGMX/PI)
                 IF(BAMINR.LT.0.OR.BAMINR.GT.90.0.OR.
     -                BAMAXR.LT.0.OR.BAMAXR.GT.90.0)THEN
                      IF(BAMINR.LT.0.OR.BAMINR.GT.90.0)
     -                     CALL INPMSG(I+1,'Out of range [0,90].')
                      IF(BAMAXR.LT.0.OR.BAMAXR.GT.90.0)
     -                     CALL INPMSG(I+2,'Out of range [0,90].')
                      OK=.FALSE.
                 ELSEIF(BAMINR.NE.BAMAXR)THEN
                      BANGMN=PI*MIN(BAMINR,BAMAXR)/180
                      BANGMX=PI*MAX(BAMINR,BAMAXR)/180
                 ELSE
                      CALL INPMSG(I+1,'A zero range is not permitted ')
                      CALL INPMSG(I+2,'for the E-B angular range.    ')
                      OK=.FALSE.
                 ENDIF
            ELSE
                 CALL INPMSG(I,'Missing or invalid arguments. ')
                 OK=.FALSE.
            ENDIF
            ASET=.FALSE.
            INEXT=I+3
       ELSEIF(INPCMP(I,'ANG#LE-RAN#GE').NE.0)THEN
            CALL INPMSG(I,'Not meaningful since B=0.')
            CALL INPMSG(I+1,'Has been ignored.')
            CALL INPMSG(I+2,'Has been ignored.')
            OK=.FALSE.
            INEXT=I+3
       ELSEIF(MAGOK.AND.INPCMP(I,'ANG#LES').NE.0)THEN
            IF(INPTYP(I+1).EQ.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 DO 350 IGLB=1,NGLB
                 IF(STRING(1:NCSTR).EQ.GLBVAR(IGLB))THEN
                      ISLOT=MATSLT(NINT(GLBVAL(IGLB)))
                      IF(GLBMOD(IGLB).NE.5)THEN
                           CALL INPMSG(I+1,'Not of type Matrix.')
                           OK=.FALSE.
                      ELSEIF(ISLOT.LE.0)THEN
                           CALL INPMSG(I+1,'Matrix inaccessible.')
                           OK=.FALSE.
                      ELSEIF(MLEN(ISLOT).GT.MXBANG)THEN
                           CALL INPMSG(I+1,'More than MXBANG elements.')
                           OK=.FALSE.
                      ELSE
                           NBANG=MLEN(ISLOT)
                           DO 370 J=1,NBANG
                           IF(MVEC(MORG(ISLOT)+J).LT.0.OR.
     -                          MVEC(MORG(ISLOT)+J).GT.90)THEN
                                CALL INPMSG(I+1,'Out of range [0,90].')
                                OK=.FALSE.
                                GOTO 360
                           ELSE
                                BANG(J)=PI*MVEC(MORG(ISLOT)+J)/180
                           ENDIF
370                        CONTINUE
                           ASET=.TRUE.
                      ENDIF
                      GOTO 360
                 ENDIF
350              CONTINUE
                 CALL INPMSG(I+1,'Not a global variable.')
                 OK=.FALSE.
360              CONTINUE
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).EQ.1.OR.INPTYP(I+1).EQ.2)THEN
                 NBANG=0
                 DO 380 J=I+1,NWORD
                 IF(INPTYP(J).NE.1.AND.INPTYP(J).NE.2)THEN
                      INEXT=J
                      GOTO 390
                 ELSEIF(NBANG+1.GT.MXBANG)THEN
                      CALL INPMSG(J,'Too many values, ignored.')
                      OK=.FALSE.
                      GOTO 380
                 ENDIF
                 CALL INPCHK(J,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J,ANGR,0.0)
                      IF(ANGR.GE.0.0.AND.ANGR.LE.90.0)THEN
                           NBANG=NBANG+1
                           BANG(NBANG)=PI*ANGR/180
                      ELSE
                           CALL INPMSG(J,'Out of range [0,90].')
                           OK=.FALSE.
                      ENDIF
                 ELSE
                      CALL INPMSG(J,'Invalid angle value.')
                      OK=.FALSE.
                 ENDIF
380              CONTINUE
                 INEXT=NWORD+1
390              CONTINUE
                 ASET=.TRUE.
            ELSE
                 CALL INPMSG(I,'Invalid angle specification.')
                 OK=.FALSE.
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'ANG#LE').NE.0)THEN
            CALL INPMSG(I,'Not meaningful since B=0.')
            CALL INPMSG(I+1,'Has been ignored.')
            OK=.FALSE.
            INEXT=I+2
*   Number of points.
       ELSEIF(MAGOK.AND.INPCMP(I,'N-ANG#LES').NE.0)THEN
            IF(INPTYP(I+1).NE.1.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.NE.0)OK=.FALSE.
                 CALL INPRDI(I+1,NBANGR,NBANG)
                 IF(NBANGR.LE.0.OR.NBANGR.GT.MXBANG)THEN
                      CALL INPMSG(I+1,'Value is out of range.        ')
                      OK=.FALSE.
                 ELSEIF(NBANGR.GT.1.AND..NOT.MAGOK)THEN
                      CALL INPMSG(I+1,'Not meaningful since B=0.     ')
                      OK=.FALSE.
                 ELSE
                      NBANG=NBANGR
                 ENDIF
            ENDIF
            ASET=.FALSE.
            INEXT=I+2
       ELSEIF(INPCMP(I,'N-ANG#LES').NE.0)THEN
            CALL INPMSG(I,'Not meaningful since B=0.')
            CALL INPMSG(I+1,'Has been ignored.')
            OK=.FALSE.
            INEXT=I+2
*   Range of B field.
       ELSEIF(MAGOK.AND.INPCMP(I,'B-RAN#GE')+
     -      INPCMP(I,'MAG#NETIC-F#IELD-RAN#GE')+
     -      INPCMP(I,'B-F#IELD-RAN#GE').NE.0)THEN
            CALL INPCHK(I+1,2,IFAIL1)
            CALL INPCHK(I+2,2,IFAIL2)
            IF(IFAIL1.EQ.0.AND.IFAIL2.EQ.0.AND.NWORD.GE.I+2)THEN
                 CALL INPRDR(I+1,BTMINR,BTABMN/100)
                 CALL INPRDR(I+2,BTMAXR,BTABMX/100)
                 IF(BTMINR.LT.0.OR.BTMAXR.LT.0)THEN
                      IF(BTMINR.LT.0)CALL INPMSG(I+1,'Is not > 0.')
                      IF(BTMAXR.LT.0)CALL INPMSG(I+1,'Is not > 0.')
                      OK=.FALSE.
                 ELSEIF(BTMINR.NE.BTMAXR)THEN
                      BTABMN=100*MIN(BTMINR,BTMAXR)
                      BTABMX=100*MAX(BTMINR,BTMAXR)
                 ELSE
                      CALL INPMSG(I+1,'A zero range is not permitted')
                      CALL INPMSG(I+2,'for the B field range.')
                      OK=.FALSE.
                 ENDIF
            ELSE
                 CALL INPMSG(I,'Missing or invalid arguments. ')
                 OK=.FALSE.
            ENDIF
            INEXT=I+3
       ELSEIF(INPCMP(I,'B-RAN#GE')+INPCMP(I,'B-FIELD-RAN#GE').NE.0)THEN
            CALL INPMSG(I,'Not meaningful since B=0.')
            CALL INPMSG(I+1,'Has been ignored.')
            CALL INPMSG(I+2,'Has been ignored.')
            OK=.FALSE.
            INEXT=I+3
*   Listed B fields.
       ELSEIF(MAGOK.AND.INPCMP(I,'B-FIELD').NE.0)THEN
            IF(INPTYP(I+1).EQ.0)THEN
                 CALL INPSTR(I+1,I+1,STRING,NCSTR)
                 DO 300 IGLB=1,NGLB
                 IF(STRING(1:NCSTR).EQ.GLBVAR(IGLB))THEN
                      ISLOT=MATSLT(NINT(GLBVAL(IGLB)))
                      IF(GLBMOD(IGLB).NE.5)THEN
                           CALL INPMSG(I+1,'Not of type Matrix.')
                           OK=.FALSE.
                      ELSEIF(ISLOT.LE.0)THEN
                           CALL INPMSG(I+1,'Matrix inaccessible.')
                           OK=.FALSE.
                      ELSEIF(MLEN(ISLOT).GT.MXBTAB)THEN
                           CALL INPMSG(I+1,'More than MXBTAB elements.')
                           OK=.FALSE.
                      ELSE
                           NBTAB=MLEN(ISLOT)
                           DO 320 J=1,NBTAB
                           IF(MVEC(MORG(ISLOT)+J).LE.0)THEN
                                CALL INPMSG(I+1,'Contains values <= 0.')
                                OK=.FALSE.
                                GOTO 310
                           ELSE
                                BTAB(J)=100*MVEC(MORG(ISLOT)+J)
                           ENDIF
320                        CONTINUE
                           BSET=.TRUE.
                      ENDIF
                      GOTO 310
                 ENDIF
300              CONTINUE
                 CALL INPMSG(I+1,'Not a global variable.')
                 OK=.FALSE.
310              CONTINUE
                 INEXT=I+2
            ELSEIF(INPTYP(I+1).EQ.1.OR.INPTYP(I+1).EQ.2)THEN
                 NBTAB=0
                 DO 330 J=I+1,NWORD
                 IF(INPTYP(J).NE.1.AND.INPTYP(J).NE.2)THEN
                      INEXT=J
                      GOTO 340
                 ELSEIF(NBTAB+1.GT.MXBTAB)THEN
                      CALL INPMSG(J,'Too many values, ignored.')
                      OK=.FALSE.
                      GOTO 330
                 ENDIF
                 CALL INPCHK(J,2,IFAIL1)
                 IF(IFAIL1.EQ.0)THEN
                      CALL INPRDR(J,BFLDR,0.0)
                      IF(BFLDR.GE.0.0)THEN
                           NBTAB=NBTAB+1
                           BTAB(NBTAB)=100*BFLDR
                      ELSE
                           CALL INPMSG(J,'Negative values not allowed.')
                           OK=.FALSE.
                      ENDIF
                 ELSE
                      CALL INPMSG(J,'Invalid field value.')
                      OK=.FALSE.
                 ENDIF
330              CONTINUE
                 INEXT=NWORD+1
340              CONTINUE
                 BSET=.TRUE.
            ELSE
                 CALL INPMSG(I,'Invalid field specification.')
                 OK=.FALSE.
                 INEXT=I+2
            ENDIF
       ELSEIF(INPCMP(I,'B-FIELD').NE.0)THEN
            CALL INPMSG(I,'Not meaningful since B=0.')
            CALL INPMSG(I+1,'Has been ignored.')
            OK=.FALSE.
            INEXT=I+2
*   Number of points.
       ELSEIF(MAGOK.AND.INPCMP(I,'N-B-#FIELD')+
     -      INPCMP(I,'N-MAG#NETIC-#FIELD').NE.0)THEN
            IF(INPTYP(I+1).NE.1.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.NE.0)OK=.FALSE.
                 CALL INPRDI(I+1,NBTABR,NBTAB)
                 IF(NBTABR.LE.0.OR.NBTABR.GT.MXBTAB)THEN
                      CALL INPMSG(I+1,'Value is out of range.        ')
                      OK=.FALSE.
                 ELSEIF(NBTABR.GT.1.AND..NOT.MAGOK)THEN
                      CALL INPMSG(I+1,'Not meaningful since B=0.     ')
                      OK=.FALSE.
                 ELSE
                      NBTAB=NBTABR
                 ENDIF
            ENDIF
            INEXT=I+2
       ELSEIF(INPCMP(I,'N-B-#FIELD')+
     -      INPCMP(I,'N-MAG#NETIC-#FIELD').NE.0)THEN
            CALL INPMSG(I,'Not meaningful since B=0.')
            CALL INPMSG(I+1,'Has been ignored.')
            OK=.FALSE.
            INEXT=I+2
*   Threshold setting.
       ELSEIF(INPCMP(I,'SST-THRESHOLD').NE.0)THEN
            IF(INPTYP(I+1).EQ.1.OR.INPTYP(I+1).EQ.2)THEN
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,SSTHRR,SSTTHR)
                 IF(IFAIL1.EQ.0.AND.SSTHRR.LE.0)THEN
                      CALL INPMSG(I+1,'Should be > 0.')
                      OK=.FALSE.
                 ELSEIF(IFAIL1.EQ.0)THEN
                      SSTTHR=SSTHRR
                 ENDIF
                 INEXT=I+2
            ELSE
                 CALL INPMSG(I,'Takes a numeric argument.')
                 OK=.FALSE.
            ENDIF
*   Number of MC collisions.
       ELSEIF(INPCMP(I,'COLL#ISIONS').NE.0)THEN
            IF(INPTYP(I+1).NE.1.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 IF(IFAIL1.NE.0)OK=.FALSE.
                 CALL INPRDI(I+1,NMAXR,5)
                 IF(NMAXR.LE.0)THEN
                      CALL INPMSG(I+1,'Value is out of range.        ')
                      OK=.FALSE.
                 ELSE
                      NNMAX=NMAXR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Mobility.
       ELSEIF(INPCMP(I,'ION-MOB#ILITY')+
     -      INPCMP(I,'MOB#ILITY').NE.0)THEN
            IF(I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
                 OK=.FALSE.
            ELSE
                 CALL INPSTR(I+1,I+1,STRING,NCMOB)
            ENDIF
            INEXT=I+2
*   Other options are not known.
       ELSE
            CALL INPMSG(I,'Not a recognised keyword.     ')
            OK=.FALSE.
       ENDIF
20     CONTINUE
*** Dump error messages.
       CALL INPERR
*** Check whether we have to continue or not.
       IF(JFAIL.EQ.2.AND..NOT.OK)THEN
            PRINT *,' ###### GASBMC ERROR   : Magboltz not called'//
     -           ' because of the above errors.'
            IFAIL=1
            NGAS=0
            CALL PROEND
            RETURN
       ELSEIF(JFAIL.EQ.3.AND..NOT.OK)THEN
            PRINT *,' ###### GASBMC ERROR   : Program terminated'//
     -           ' because of the above errors.'
            IFAIL=1
            NGAS=0
            CALL PROEND
            CALL QUIT
            RETURN
       ENDIF
*** Progress printing.
       CALL PROFLD(1,'Initialisation',-1.0)
       CALL PROSTA(1,0.0)
*** Set the scale of the E/p points.
       IF(NGAS.GT.1.AND..NOT.ESET)THEN
            DO 30 I=1,NGAS
            IF(EPLOG)THEN
                 EGAS(I)=EPMIN*(EPMAX/EPMIN)**
     -                (REAL(I-1)/REAL(MAX(1,NGAS-1)))
            ELSE
                 EGAS(I)=EPMIN+(EPMAX-EPMIN)*
     -                (REAL(I-1)/REAL(MAX(1,NGAS-1)))
            ENDIF
30          CONTINUE
       ELSEIF(.NOT.ESET)THEN
            IF(EPLOG)THEN
                 EGAS(1)=SQRT(EPMIN*EPMAX)
            ELSE
                 EGAS(1)=(EPMIN+EPMAX)/2
            ENDIF
       ENDIF
*** Compute the E-B angles.
       IF(NBANG.GT.1.AND..NOT.ASET)THEN
            DO 40 J=1,NBANG
            BANG(J)=BANGMN+REAL(J-1)*(BANGMX-BANGMN)/REAL(NBANG-1)
40          CONTINUE
       ELSEIF(.NOT.ASET)THEN
            BANG(1)=(BANGMN+BANGMX)/2
       ENDIF
*** Compute the B field strengths.
       IF(NBTAB.GT.1.AND..NOT.BSET)THEN
            DO 50 J=1,NBTAB
            BTAB(J)=(BTABMN+REAL(J-1)*(BTABMX-BTABMN)/REAL(NBTAB-1))
50          CONTINUE
       ELSEIF(.NOT.BSET)THEN
            BTAB(1)=(BTABMN+BTABMX)/2
       ENDIF
*** Renormalise the fractions.
       FRTOT=0.0
       DO 60 I=1,MXGNAM
       IF(FRAMIX(I).LT.0)FRAMIX(I)=0.0
       FRTOT=FRTOT+FRAMIX(I)
60     CONTINUE
       IF(FRTOT.LE.0.0)THEN
            PRINT *,' !!!!!! GASBMC WARNING : Please have at least'//
     -           ' one gas in your mixture; nothing done.'
            NGAS=0
            IFAIL=1
            RETURN
       ELSE
            DO 70 I=1,MXGNAM
            FRAMIX(I)=100*FRAMIX(I)/FRTOT
            GASFRM(I)=FRAMIX(I)
70          CONTINUE
       ENDIF
*** Name of the mixture.
       STR=' '
       NC=0
*   Loop over the gases.
       DO 80 I=1,MXGNAM
*   Skip gases that are absent.
       IF(FRAMIX(I).LE.0)GOTO 80
*   Format the percentage.
       CALL OUTFMT(REAL(FRAMIX(I)),2,OUTSTR,NCOUT,'LEFT')
*   Hydrogen and Deuterium.
       IF(I.EQ.21)THEN
            STR(NC+1:NC+6+NCOUT)='H2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
       ELSEIF(I.EQ.22)THEN
            STR(NC+1:NC+6+NCOUT)='D2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Helium 3 and 4.
       ELSEIF(I.EQ.3)THEN
            STR(NC+1:NC+18+NCOUT)='isotropic He-4 '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+18+NCOUT
       ELSEIF(I.EQ.4)THEN
            STR(NC+1:NC+18+NCOUT)='isotropic He-3 '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+18+NCOUT
       ELSEIF(I.EQ.45)THEN
            STR(NC+1:NC+8+NCOUT)='He-4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
       ELSEIF(I.EQ.44)THEN
            STR(NC+1:NC+8+NCOUT)='He-3 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Neon.
       ELSEIF(I.EQ.5)THEN
            STR(NC+1:NC+16+NCOUT)='isotropic Ne '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+16+NCOUT
       ELSEIF(I.EQ.46)THEN
            STR(NC+1:NC+6+NCOUT)='Ne '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Argon.
       ELSEIF(I.EQ.2)THEN
            STR(NC+1:NC+16+NCOUT)='isotropic Ar '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+16+NCOUT
       ELSEIF(I.EQ.47)THEN
            STR(NC+1:NC+6+NCOUT)='Ar '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Krypton.
       ELSEIF(I.EQ.6)THEN
            STR(NC+1:NC+16+NCOUT)='isotropic Kr '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+16+NCOUT
       ELSEIF(I.EQ.48)THEN
            STR(NC+1:NC+6+NCOUT)='Kr '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Xenon.
       ELSEIF(I.EQ.7)THEN
            STR(NC+1:NC+16+NCOUT)='isotropic Xe '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+16+NCOUT
       ELSEIF(I.EQ.49)THEN
            STR(NC+1:NC+6+NCOUT)='Xe '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Methane.
       ELSEIF(I.EQ.8)THEN
            STR(NC+1:NC+7+NCOUT)='CH4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   Deuterium methane, silane and germane.
       ELSEIF(I.EQ.41)THEN
            STR(NC+1:NC+7+NCOUT)='CD4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
       ELSEIF(I.EQ.59)THEN
            STR(NC+1:NC+8+NCOUT)='GeH4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
       ELSEIF(I.EQ.60)THEN
            STR(NC+1:NC+8+NCOUT)='SiH4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Ethane.
       ELSEIF(I.EQ.9)THEN
            STR(NC+1:NC+8+NCOUT)='C2H6 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Ethene.
       ELSEIF(I.EQ.19)THEN
            STR(NC+1:NC+8+NCOUT)='C2H4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Acetylene.
       ELSEIF(I.EQ.20)THEN
            STR(NC+1:NC+8+NCOUT)='C2H2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Propane.
       ELSEIF(I.EQ.10)THEN
            STR(NC+1:NC+8+NCOUT)='C3H8 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Propene.
       ELSEIF(I.EQ.32)THEN
            STR(NC+1:NC+9+NCOUT)='C3H6 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   n-Butane and Isobutane.
       ELSEIF(I.EQ.11)THEN
            STR(NC+1:NC+10+NCOUT)='iC4H10 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+10+NCOUT
       ELSEIF(I.EQ.56)THEN
            STR(NC+1:NC+10+NCOUT)='nC4H10 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+10+NCOUT
*   neo-Pentane and n-Pentane.
       ELSEIF(I.EQ.13)THEN
            STR(NC+1:NC+12+NCOUT)='neoC5H12 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+12+NCOUT
       ELSEIF(I.EQ.57)THEN
            STR(NC+1:NC+10+NCOUT)='nC5H12 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+10+NCOUT
*   Nitrogen.
       ELSEIF(I.EQ.16)THEN
            STR(NC+1:NC+16+NCOUT)='isotropic N2 '//OUTSTR(1:NCOUT)//
     -           '%, '
            NC=NC+16+NCOUT
       ELSEIF(I.EQ.58)THEN
            STR(NC+1:NC+6+NCOUT)='N2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Water vapour.
       ELSEIF(I.EQ.14)THEN
            STR(NC+1:NC+7+NCOUT)='H2O '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   Carbon monoxide and dioxide.
       ELSEIF(I.EQ.23)THEN
            STR(NC+1:NC+6+NCOUT)='CO '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
       ELSEIF(I.EQ.12)THEN
            STR(NC+1:NC+7+NCOUT)='CO2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   Methylal.
       ELSEIF(I.EQ.24)THEN
            STR(NC+1:NC+10+NCOUT)='C3H8O2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+10+NCOUT
*   Sulphur compounds.
       ELSEIF(I.EQ.39)THEN
            STR(NC+1:NC+7+NCOUT)='CS2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
       ELSEIF(I.EQ.40)THEN
            STR(NC+1:NC+7+NCOUT)='COS '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
       ELSEIF(I.EQ.55)THEN
            STR(NC+1:NC+7+NCOUT)='H2S '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   SF6.
       ELSEIF(I.EQ.30)THEN
            STR(NC+1:NC+7+NCOUT)='SF6 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   NH3.
       ELSEIF(I.EQ.31)THEN
            STR(NC+1:NC+7+NCOUT)='NH3 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   Freons (Freon 14, 23, Zyron 116N5) and related compounds.
       ELSEIF(I.EQ.1)THEN
            STR(NC+1:NC+7+NCOUT)='CF4 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
       ELSEIF(I.EQ.29)THEN
            STR(NC+1:NC+8+NCOUT)='C2F6 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
       ELSEIF(I.EQ.42)THEN
            STR(NC+1:NC+7+NCOUT)='BF3 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
       ELSEIF(I.EQ.43)THEN
            STR(NC+1:NC+16+NCOUT)=
     -           'C2F4H2/C2HF5 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+16+NCOUT
       ELSEIF(I.EQ.50)THEN
            STR(NC+1:NC+8+NCOUT)='CHF3 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
       ELSEIF(I.EQ.51)THEN
            STR(NC+1:NC+9+NCOUT)='CF3Br '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+9+NCOUT
       ELSEIF(I.EQ.52)THEN
            STR(NC+1:NC+8+NCOUT)='C3F8 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+8+NCOUT
*   Cesium and Mercury.
       ELSEIF(I.EQ.37)THEN
            STR(NC+1:NC+6+NCOUT)='Cs '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Cesium and Mercury.
       ELSEIF(I.EQ.54)THEN
            STR(NC+1:NC+6+NCOUT)='Hg '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Oxygen and Ozone.
       ELSEIF(I.EQ.15)THEN
            STR(NC+1:NC+6+NCOUT)='O2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
       ELSEIF(I.EQ.53)THEN
            STR(NC+1:NC+6+NCOUT)='O3 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Fluorine.
       ELSEIF(I.EQ.38)THEN
            STR(NC+1:NC+6+NCOUT)='F2 '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
*   Nitrous and nitric oxide.
       ELSEIF(I.EQ.17)THEN
            STR(NC+1:NC+6+NCOUT)='NO '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+6+NCOUT
       ELSEIF(I.EQ.18)THEN
            STR(NC+1:NC+7+NCOUT)='N2O '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   DME.
       ELSEIF(I.EQ.25)THEN
            STR(NC+1:NC+7+NCOUT)='DME '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
*   Alcohols.
       ELSEIF(I.EQ.34)THEN
            STR(NC+1:NC+9+NCOUT)='CH3OH '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+9+NCOUT
       ELSEIF(I.EQ.35)THEN
            STR(NC+1:NC+10+NCOUT)='C2H5OH '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+10+NCOUT
       ELSEIF(I.EQ.36)THEN
            STR(NC+1:NC+10+NCOUT)='C3H7OH '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+10+NCOUT
*   Reid step, Reid ramp and Maxwell model.
       ELSEIF(I.EQ.26)THEN
            STR(NC+1:NC+13+NCOUT)='Reid-step '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+13+NCOUT
       ELSEIF(I.EQ.28)THEN
            STR(NC+1:NC+13+NCOUT)='Reid-ramp '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+13+NCOUT
       ELSEIF(I.EQ.27)THEN
            STR(NC+1:NC+11+NCOUT)='Maxwell '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+11+NCOUT
*   Obsolete components.
       ELSE
            STR(NC+1:NC+7+NCOUT)='??? '//OUTSTR(1:NCOUT)//'%, '
            NC=NC+7+NCOUT
       ENDIF
*   Next component.
80     CONTINUE
*   Gas temperature.
       CALL OUTFMT(TGAS,2,OUTSTR,NCOUT,'LEFT')
       STR(NC+1:NC+NCOUT+6)='T='//OUTSTR(1:NCOUT)//' K, '
       NC=NC+NCOUT+6
*   Gas pressure.
       CALL OUTFMT(PGAS/760.0,2,OUTSTR,NCOUT,'LEFT')
       STR(NC+1:NC+NCOUT+6)='p='//OUTSTR(1:NCOUT)//' atm'
       NC=NC+NCOUT+6
*   Copy to the gas identifier.
       IF(NC.GT.LEN(GASID))THEN
            GASID=STR(1:NC-2)//'...'
            NC=LEN(GASID)
       ELSE
            GASID=STR(1:NC)
       ENDIF
*** Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASBMC DEBUG   : Mixing the'',
     -           '' following gases:''//
     -           ''       CF4        '',F6.3,''  Argon      '',F6.3/
     -           ''       Helium 4   '',F6.3,''  Helium 3   '',F6.3/
     -           ''       Neon       '',F6.3,''  Krypton    '',F6.3/
     -           ''       Xenon      '',F6.3,''  CH4        '',F6.3/
     -           ''       C2H6       '',F6.3,''  C3H8       '',F6.3/
     -           ''       iso-C4H10  '',F6.3,''  CO2        '',F6.3/
     -           ''       neo-C5H12  '',F6.3,''  H2O        '',F6.3/
     -           ''       Oxygen     '',F6.3,''  Nitrogen   '',F6.3/
     -           ''       NO         '',F6.3,''  N2O        '',F6.3/
     -           ''       C2H4       '',F6.3,''  C2H2       '',F6.3/
     -           ''       Hydrogen   '',F6.3,''  Deuterium  '',F6.3/
     -           ''       CO         '',F6.3,''  Methylal   '',F6.3/
     -           ''       DME        '',F6.3,''  Reid step  '',F6.3/
     -           ''       Maxwell    '',F6.3,''  Reid ramp  '',F6.3/
     -           ''       C2F6       '',F6.3,''  SF6        '',F6.3/
     -           ''       NH3        '',F6.3,''  C3H6       '',F6.3/
     -           ''       cyclo-C3H6 '',F6.3,''  CH3OH      '',F6.3/
     -           ''       C2H5OH     '',F6.3,''  C3H7OH     '',F6.3/
     -           ''       Cs         '',F6.3,''  F          '',F6.3/
     -           ''       CS2        '',F6.3,''  COS        '',F6.3/
     -           ''       CD4        '',F6.3,''  BF3        '',F6.3/
     -           ''       C2F4H2     '',F6.3,''  He 3 aniso '',F6.3/
     -           ''       He 4 aniso '',F6.3,''  Ne aniso   '',F6.3/
     -           ''       Ar aniso   '',F6.3,''  Kr aniso   '',F6.3/
     -           ''       Xe aniso   '',F6.3,''  CHF3       '',F6.3/
     -           ''       CF3Br      '',F6.3,''  C3F8       '',F6.3/
     -           ''       O3         '',F6.3,''  Mercury    '',F6.3/
     -           ''       H2S        '',F6.3,''  n-C4H10    '',F6.3/
     -           ''       n-C5H12    '',F6.3,''  N2         '',F6.3/
     -           ''       GeH4       '',F6.3,''  SiH4       '',F6.3)')
     -           (0.01*REAL(FRAMIX(I)),I=1,60)
            WRITE(LUNOUT,'(''  With the following parameters:''//
     -           ''       E/p range:                '',2F10.3,
     -           '' V/cm.Torr''/
     -           ''       Number of E/p points:     '',I6/
     -           ''       Magnetic field range:     '',2F10.3,'' T''/
     -           ''       Number of B field points: '',I6/
     -           ''       angle(E,B) range:         '',2F10.3,
     -           '' degrees''/
     -           ''       Number of (E,B) points:   '',I6/
     -           ''       Pressure of the gas:      '',F10.3,'' Torr''/
     -           ''       Temperature of the gas:   '',F10.3,'' K'')')
     -           EPMIN,EPMAX,NGAS,BTABMN/100,BTABMX/100,NBTAB,
     -           180*BANGMN/PI,180*BANGMX/PI,NBANG,PGAS,TGAS
            WRITE(LUNOUT,'(''  Accuracy settings: '')')
            WRITE(LUNOUT,'(''       SST threshold: '',F10.3,'' 1/cm'')')
     -           SSTTHR
            WRITE(LUNOUT,'(''       MC iterations: '',I5)') NNMAX
            WRITE(LUNOUT,'(''  Identifier: '',A)') GASID(1:NC)
       ENDIF
*** Compute the mobilities for the various points.
       IF(NCMOB.GT.0)THEN
            CALL PROFLD(1,'Adding mobility',-1.0)
            CALL PROSTA(1,0.0)
*   Call editor of specified as @.
            IF(INDEX(STRING(1:NCMOB),'@').NE.0)THEN
                 NRES=1
                 CALL ALGEDT(VARLIS,1,IENTRY,USE,NRES)
                 IFAIL1=0
*   Usual function translation if not.
            ELSE
                 CALL ALGPRE(STRING,NCMOB,VARLIS,1,NRES,USE,IENTRY,
     -                IFAIL1)
            ENDIF
*   Check return code of translation.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASBMC WARNING : Ion mobility'//
     -                ' function rejected; no ion mobility in table.'
                 CALL ALGCLR(IENTRY)
                 NCMOB=0
            ENDIF
*   Check number of results returned by the function.
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! GASBMC WARNING : Number of'//
     -                ' results returned by the mobility function'//
     -                ' is not 1; rejected.'
                 CALL ALGCLR(IENTRY)
                 NCMOB=0
            ENDIF
*   Evaluate.
            DO 90 I=1,NGAS
            VAR(1)=EGAS(I)
            MODVAR(1)=2
            CALL ALGEXE(IENTRY,VAR,MODVAR,1,RES,MODRES,1,IFAIL1)
            MGAS(I)=RES(1)
            DO 100 J=1,NBANG
            DO 110 K=1,NBTAB
            MGAS2(I,J,K)=RES(1)
110         CONTINUE
100         CONTINUE
90          CONTINUE
*   Clear the mobility entry point - no longer needed.
            CALL ALGCLR(IENTRY)
*   Dump algebra error messages.
            CALL ALGERR
       ENDIF
*** Reset frequencies
       NEXGAS=0
       NIOGAS=0
*** Fill the gas tables, first with a magnetic field.
       IF(MAGOK)THEN
*   Header.
            TAB2D = .TRUE.
            IF(LDEBUG)PRINT *,' ++++++ GASBMC DEBUG   : Preparing a'//
     -           ' 3D table.'
*   Loop over the B fields.
            CALL PRORED(3)
            CALL PROFLD(1,'B-field',REAL(NBTAB))
            DO 120 K=1,NBTAB
            CALL PROSTA(1,REAL(K))
*   Loop over the angles.
            CALL PROFLD(2,'angle(E,B)',REAL(NBANG))
            DO 130 J=1,NBANG
            CALL PROSTA(2,REAL(J))
**  Loop over the electric field.
            CALL PROFLD(3,'E-field',REAL(NGAS))
            DO 140 I=1,NGAS
*   Progress printing.
            CALL PROSTA(3,REAL(I))
C      print *,' Starting for:'
C      print *,' E     = ',EGAS(I)*PGAS
C      print *,' B     = ',BTAB(K)/100,' T'
C      print *,' angle = ',180*BANG(J)/PI,' degrees'
*   Run Magboltz.
            CALL GASB7(DBLE(EGAS(I)*PGAS),DBLE(BTAB(K)/10),
     -           DBLE(180*BANG(J)/PI),DBLE(TGAS),DBLE(PGAS),
     -           DBLE(SSTTHR),NNMAX,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! GASBMC WARNING : Running'//
     -                ' Magboltz 7 for E/p=',EGAS(I),
     -                ' angle=',180*BANG(J)/PI,' failed; no gas tables.'
                 IFAIL=1
                 NGAS=0
                 RETURN
            ENDIF
*   Plot distribution function if requested.
            IF(LF0PLT)CALL F0PLT7(GASID)
*   Plot cross sections if requested.
            IF(LCSPLT)CALL GASPCS(GASID)
*   Keep cross sections and distribution function if requested.
            IF(LGKEEP)CALL GASSAV
*   Output the transport results.
            CALL OUTB7(VBOL,XBOL,YBOL,WBOL,DBOL,OBOL,ABOL,BBOL,SBOL)
            VGAS2(I,J,K)=VBOL
            XGAS2(I,J,K)=XBOL
            YGAS2(I,J,K)=YBOL
            WGAS2(I,J,K)=WBOL
            DGAS2(I,J,K)=DBOL
            OGAS2(I,J,K)=OBOL
            AGAS2(I,J,K)=ABOL
            AORIG2(I,J,K)=ABOL
            BGAS2(I,J,K)=BBOL
            DO 150 L=1,6
            SGAS2(I,J,K,L)=SBOL(L)
150         CONTINUE
*   Output the frequencies
            CALL OUTEI7(EXBOL,IOBOL)
            DO 151 L=1,NEXGAS
            EXGAS2(I,J,K,L)=EXBOL(L)
151         CONTINUE
            DO 152 L=1,NIOGAS
            IOGAS2(I,J,K,L)=IOBOL(L)
152         CONTINUE
*   Next E field.
140         CONTINUE
*   Next angle.
130         CONTINUE
*   Next B field
120         CONTINUE
*   Transfer the data from the VGAS2 etc to VGAS.
            IF(NBANG.EQ.1.AND.NBTAB.EQ.1)THEN
                 PRINT *,' ------ GASBMC MESSAGE : The table is'//
     -                ' 1-dimensional even though B/=0.'
                 TAB2D=.FALSE.
                 DO 160 I=1,NGAS
                 VGAS(I)=VGAS2(I,1,1)
                 XGAS(I)=XGAS2(I,1,1)
                 YGAS(I)=YGAS2(I,1,1)
                 DGAS(I)=DGAS2(I,1,1)
                 AGAS(I)=AGAS2(I,1,1)
                 AORIG(I)=AGAS2(I,1,1)
                 BGAS(I)=BGAS2(I,1,1)
                 OGAS(I)=OGAS2(I,1,1)
                 WGAS(I)=WGAS2(I,1,1)
                 DO 170 L=1,6
                 SGAS(I,L)=SGAS2(I,1,1,L)
170              CONTINUE
160              CONTINUE
            ENDIF
**  If there is no magnetic field.
       ELSE
            CALL PRORED(1)
            CALL PROFLD(1,'Electric field',REAL(NGAS))
*   First fill the 2 dimensional arrays as for the B field case.
            NBANG=1
            IF(LDEBUG)PRINT *,' ++++++ GASBMC DEBUG   : Preparing a'//
     -           ' 1D table.'
*   Loop over the electric field.
            CALL PROFLD(1,'Electric field',REAL(NGAS))
            DO 180 I=1,NGAS
*   Progress printing.
            CALL PROSTA(1,REAL(I))
*   Run Magboltz.
C      print *,' Starting for:'
C      print *,' E     = ',EGAS(I)*PGAS
            CALL GASB7(DBLE(EGAS(I)*PGAS),0.0D0,0.0D0,DBLE(TGAS),
     -           DBLE(PGAS),DBLE(SSTTHR),NNMAX,IFAIL)
            IF(IFAIL.NE.0)THEN
                 PRINT *,' !!!!!! GASBMC WARNING : Running'//
     -                ' Magboltz 7 for E/p=',EGAS(I),' failed;'//
     -                ' no gas tables.'
                 IFAIL=1
                 NGAS=0
                 RETURN
            ENDIF
*   Plot distribution function if requested.
            IF(LF0PLT)CALL F0PLT7(GASID)
*   Plot cross sections if requested.
            IF(LCSPLT)CALL GASPCS(GASID)
*   Keep cross sections and distribution function if requested.
            IF(LGKEEP)CALL GASSAV
*   Output the results.
            CALL OUTB7(VBOL,XBOL,YBOL,WBOL,DBOL,OBOL,ABOL,BBOL,SBOL)
            VGAS(I)=VBOL
            XGAS(I)=XBOL
            YGAS(I)=YBOL
            WGAS(I)=WBOL
            DGAS(I)=DBOL
            OGAS(I)=OBOL
            AGAS(I)=ABOL
            AORIG(I)=ABOL
            BGAS(I)=BBOL
            DO 190 L=1,6
            SGAS(I,L)=SBOL(L)
190         CONTINUE
*   Output the frequencies
            CALL OUTEI7(EXBOL,IOBOL)
            DO 191 L=1,MXEXG
            EXGAS(I,L)=EXBOL(L)
191         CONTINUE
            DO 192 L=1,MXIOG
            IOGAS(I,L)=IOBOL(L)
192         CONTINUE
*   Next E field.
180         CONTINUE
*   Declare the table to be 1-dimensional.
            TAB2D=.FALSE.
       ENDIF
*   End of progress printing.
       CALL PROEND
*   Check error flags.
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! GASBMC WARNING : Computing the transport'//
     -           ' properties failed ; no gas tables.'
            RETURN
       ENDIF
*** Set the GASOK flags.
       GASOK(1)=.TRUE.
       IF(NCMOB.GT.0)THEN
            GASOK(2)=.TRUE.
       ELSE
            GASOK(2)=.FALSE.
       ENDIF
       GASOK(3)=.TRUE.
       GASOK(4)=.TRUE.
       GASOK(6)=.TRUE.
       IF(MAGOK)THEN
            GASOK(7)=.TRUE.
            GASOK(9)=.TRUE.
            GASOK(10)=.TRUE.
       ELSE
            GASOK(7)=.FALSE.
            GASOK(9)=.FALSE.
            GASOK(10)=.FALSE.
       ENDIF
       GASOK(8)=.TRUE.
       GASOK(11)=.TRUE.
       GASOK(15)=.TRUE.
       GASOK(16)=.TRUE.
*** Interpolation flags.
       IVMETH=2
       IMMETH=2
       IDMETH=2
       IOMETH=2
       IAMETH=2
       IBMETH=2
       IWMETH=2
       IEMETH=2
       IZMETH=2
*** Extrapolation flags.
       IF(NGAS.LE.1)THEN
            IVEXTR=0
            IMEXTR=0
            IDEXTR=0
            IAEXTR=0
            IBEXTR=0
            IWEXTR=0
            IOEXTR=0
            IEEXTR=0
            IZEXTR=0
            JVEXTR=0
            JMEXTR=0
            JDEXTR=0
            JAEXTR=0
            JBEXTR=0
            JWEXTR=0
            JOEXTR=0
            JEEXTR=0
            JZEXTR=0
       ENDIF
*** Record CPU time used.
       CALL TIMLOG('Magboltz gas mixing:                    ')
       END
