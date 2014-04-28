CDECK  ID>, GASMIX.
       SUBROUTINE GASMIX
*-----------------------------------------------------------------------
*   GASMIX - Calculates the drift velocity and diffusion coefficient
*            for various gas mixtures.
*   REFERENCES: G. Schultz, Thesis, Universite Louis Pasteur,
*               Strasbourg, No 1015 (1976).
*               G. Schultz and J. Gresser, NIM 151 (1978) 413-431.
*   (Last changed on  1/ 2/99.)
*-----------------------------------------------------------------------
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
*-----------------------------------------------------------------------
*   GMXDAT - Common block for gas mixing.
*   (Last changed on 20/ 2/97.)
*-----------------------------------------------------------------------
       REAL BREAK,FRAC,XLOSCH,EFLD,ESTEP,ECRIT
       INTEGER NBREAK
       COMMON /GMXDAT/ BREAK(MXLIST),FRAC(MXFRAC),XLOSCH,
     -      EFLD,ESTEP,ECRIT,NBREAK
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER INPCMP,INPTYP,MODVAR(MXVAR),MODRES(1)
       LOGICAL LDISPL,LOSSPL,LPTHPL,LCSPL,LTABPR,EPLOG,USE(MXVAR)
       REAL XPL(MXLIST),YPL1(MXLIST),YPL2(MXLIST),YPL3(MXLIST),
     -      VAR(MXVAR),RES(1)
       DOUBLE PRECISION GASMG1,GASMG2,X(2),F0NORM,F0OK
       CHARACTER*(MXCHAR) STRMOB,STRTWN,STRATT
       CHARACTER*10 VARLIS(MXVAR)
       EXTERNAL INPCMP,INPTYP
       EXTERNAL FGAS1,FGAS2D,FGAS2N,FGAS2V,GASMG1,GASMG2
       SAVE LDISPL,LOSSPL,LPTHPL,LCSPL,LTABPR,EMIN,EMAX,
     -      FRCRIT,EPMIN,EPMAX,EPLOG
       DATA LDISPL , LOSSPL , LPTHPL , LCSPL   , LTABPR
     -     /.TRUE. , .FALSE., .FALSE., .TRUE.  , .FALSE./
       DATA EMIN,EMAX,FRCRIT,EPMIN,EPMAX
     -     /0.01,25.0,0.01  ,0.5  ,50.0 /
       DATA EPLOG /.TRUE./
*** Initial values.
       XLOSCH=2.687E19*(PGAS/760.0)*(273.0/TGAS)
       EPMIN=100.0/PGAS
       EPMAX=10000.0/PGAS
       NGAS=20
       DO 110 I=1,MXFRAC
       FRAC(I)=-1.0
110    CONTINUE
       VARLIS(1)='EP'
       NCMOB=0
       NCTWN=0
       NCATT=0
*** Progress printing.
       CALL PROINT('MIX',1,6)
       CALL PROFLD(1,'Reading the command',-1.0)
       CALL PROSTA(1,0.0)
*** Read the command line.
       CALL INPNUM(NWORD)
       INEXT=2
       DO 100 I=2,NWORD
       IF(I.LT.INEXT)GOTO 100
*** Fractions, first Argon.
       IF(INPCMP(I,'AR#GON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(1),0.0)
            ENDIF
            INEXT=I+2
*   Methane
       ELSEIF(INPCMP(I,'METHA#NE')+INPCMP(I,'CH4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(2),0.0)
            ENDIF
            INEXT=I+2
*   Neon
       ELSEIF(INPCMP(I,'NE#ON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(3),0.0)
            ENDIF
            INEXT=I+2
*   Isobutane
       ELSEIF(INPCMP(I,'ISO#BUTANE')+INPCMP(I,'C4H10').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(4),0.0)
            ENDIF
            INEXT=I+2
*   CO2
       ELSEIF(INPCMP(I,'CO2').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(5),0.0)
            ENDIF
            INEXT=I+2
*   Helium
       ELSEIF(INPCMP(I,'HE#LIUM-#4').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(6),0.0)
            ENDIF
            INEXT=I+2
*   Ethane
       ELSEIF(INPCMP(I,'ETH#ANE')+INPCMP(I,'C2H6').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(7),0.0)
            ENDIF
            INEXT=I+2
*   Nitrogen
       ELSEIF(INPCMP(I,'NITR#OGEN').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(8),0.0)
            ENDIF
            INEXT=I+2
*   Xenon
       ELSEIF(INPCMP(I,'XE#NON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(9),0.0)
            ENDIF
            INEXT=I+2
*   Methylal (dimethoxymethane).
       ELSEIF(INPCMP(I,'METHYL#AL')+INPCMP(I,'C3H8O2')+
     -      INPCMP(I,'DMM').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(10),0.0)
            ENDIF
            INEXT=I+2
*   Krypton.
       ELSEIF(INPCMP(I,'KR#YPTON').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(11),0.0)
            ENDIF
            INEXT=I+2
*   Ammonia.
       ELSEIF(INPCMP(I,'AMM#ONIA')+INPCMP(I,'NH3')+
     -      INPCMP(I,'H3N').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(12),0.0)
            ENDIF
            INEXT=I+2
*   Test gas mixture.
       ELSEIF(INPCMP(I,'TEST').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRAC(13),0.0)
            ENDIF
            INEXT=I+2
*   Maximum energy for cross-section calculations and plots.
       ELSEIF(INPCMP(I,'MAX#IMUM-E#NERGY').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EMAXR,50.0)
                 IF(EMAXR.LE.0.0)THEN
                      CALL INPMSG(I+1,'Maximum energy not > 0.       ')
                 ELSE
                      EMAX=EMAXR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Minimum energy for cross-section plots.
       ELSEIF(INPCMP(I,'MIN#IMUM-E#NERGY').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,EMINR,0.01)
                 IF(EMINR.LE.0.0)THEN
                      CALL INPMSG(I+1,'Minimum energy not > 0.       ')
                 ELSE
                      EMIN=EMINR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Energy step-size for integrations.
       ELSEIF(INPCMP(I,'STEP#SIZE-#ENERGY').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,ESTEPR,10.0)
                 IF(ESTEPR.LE.0.0)THEN
                      CALL INPMSG(I+1,'Stepsize is not larger than 0.')
                 ELSE
                      ESTEP=ESTEPR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Critical F0 fraction for warnings
       ELSEIF(INPCMP(I,'CRIT#ICAL-F0-FR#ACTION').NE.0)THEN
            IF(INPTYP(I+1).NE.1.AND.INPTYP(I+1).NE.2.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,2,IFAIL1)
                 CALL INPRDR(I+1,FRCRIR,0.1)
                 IF(FRCRIR.LE.0.0.OR.FRCRIR.GE.1.0)THEN
                      CALL INPMSG(I+1,'Fraction not within <0,1>.    ')
                 ELSE
                      FRCRIT=FRCRIR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Range of E/p.
       ELSEIF(INPCMP(I,'RAN#GE')+INPCMP(I,'E/P-RAN#GE').NE.0)THEN
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
                 ENDIF
            ELSE
                 CALL INPMSG(I,'Missing or invalid arguments. ')
            ENDIF
            INEXT=I+3
*   Kind of E/p scale.
       ELSEIF(INPCMP(I,'LIN#EAR-#E/P-#SCALE').NE.0)THEN
            EPLOG=.FALSE.
       ELSEIF(INPCMP(I,'LOG#ARITHMIC-#E/P-#SCALE').NE.0)THEN
            EPLOG=.TRUE.
*   Number of points.
       ELSEIF(INPCMP(I,'N-#E/P').NE.0)THEN
            IF(INPTYP(I+1).NE.1.OR.I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPCHK(I+1,1,IFAIL1)
                 CALL INPRDI(I+1,NGASR,20)
                 IF(NGASR.LE.0.OR.NGASR.GT.MXLIST)THEN
                      CALL INPMSG(I+1,'Value is out of range.        ')
                 ELSE
                      NGAS=NGASR
                 ENDIF
            ENDIF
            INEXT=I+2
*   Plotting options.
       ELSEIF(INPCMP(I,'PL#OT-DIST#RIBUTION-#FUNCTIONS')+
     -      INPCMP(I,'PL#OT-F0').NE.0)THEN
            LDISPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-DIST#RIBUTION-#FUNCTIONS')+
     -      INPCMP(I,'NOPL#OT-F0').NE.0)THEN
            LDISPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-E#NERGY-#LOSS').NE.0)THEN
            LOSSPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-E#NERGY-#LOSS').NE.0)THEN
            LOSSPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-CR#OSS-#SECTION').NE.0)THEN
            LCSPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-CR#OSS-#SECTION').NE.0)THEN
            LCSPL=.FALSE.
       ELSEIF(INPCMP(I,'PL#OT-PATH').NE.0)THEN
            LPTHPL=.TRUE.
       ELSEIF(INPCMP(I,'NOPL#OT-PATH').NE.0)THEN
            LPTHPL=.FALSE.
       ELSEIF(INPCMP(I,'PR#INT-TAB#LES').NE.0)THEN
            LTABPR=.TRUE.
       ELSEIF(INPCMP(I,'NOPR#INT-TAB#LES').NE.0)THEN
            LTABPR=.FALSE.
*   Mobility.
       ELSEIF(INPCMP(I,'ION-MOB#ILITY')+
     -      INPCMP(I,'MOB#ILITY').NE.0)THEN
            IF(I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRMOB,NCMOB)
            ENDIF
            INEXT=I+2
*   Townsend coefficient.
       ELSEIF(INPCMP(I,'TOWN#SEND-#COEFFICIENT').NE.0)THEN
            IF(I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRTWN,NCTWN)
            ENDIF
            INEXT=I+2
*   Attachment coefficient.
       ELSEIF(INPCMP(I,'ATT#ACHMENT-#COEFFICIENT').NE.0)THEN
            IF(I.GE.NWORD)THEN
                 CALL INPMSG(I,'Argument invalid or missing.  ')
            ELSE
                 CALL INPSTR(I+1,I+1,STRATT,NCATT)
            ENDIF
            INEXT=I+2
*   Other options not valid.
       ELSE
            CALL INPMSG(I,'Not a recognised keyword.     ')
       ENDIF
100    CONTINUE
*** Dump error messages.
       CALL INPERR
*** Renormalise the fractions.
       FRTOT=0.0
       DO 120 I=1,MXFRAC
       IF(FRAC(I).LT.0)FRAC(I)=0.0
       FRTOT=FRTOT+FRAC(I)
120    CONTINUE
       IF(FRTOT.LE.0.0)THEN
            PRINT *,' !!!!!! GASMIX WARNING : Please have at least'//
     -           ' gas in your mixture; nothing done.'
            NGAS=0
            CALL PROEND
            RETURN
       ELSE
            DO 130 I=1,MXFRAC
            FRAC(I)=FRAC(I)/FRTOT
130         CONTINUE
       ENDIF
*** Break-point initialisation.
       CALL PROFLD(1,'Setting breakpoints',-1.0)
       CALL PROSTA(1,0.0)
       CALL GASMXB
*** Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   : Mixing the'',
     -           '' following gasses:''//
     -           ''       Argon     '',F6.3,''  Methane   '',F6.3/
     -           ''       Neon      '',F6.3,''  Isobutane '',F6.3/
     -           ''       CO2       '',F6.3,''  Helium    '',F6.3/
     -           ''       Ethane    '',F6.3,''  Nitrogen  '',F6.3/
     -           ''       Xenon     '',F6.3,''  Methylal  '',F6.3/
     -           ''       Krypton   '',F6.3,''  Ammonia   '',F6.3/)')
     -           (FRAC(I),I=1,12)
            WRITE(LUNOUT,'(''  With the following parameters:''//
     -           ''       Lower plotting bound:    '',F10.3,'' [eV]''/
     -           ''       Upper integration bound: '',F10.3,'' [eV]''/
     -           ''       Step size limit:         '',F10.3,'' [eV]''/
     -           ''       Onset of ionisation:     '',F10.3,'' [eV]''/
     -           ''       Warning level:           '',F10.3/
     -           ''       E/p range:               '',2F10.3,
     -           '' [V/cm.torr]''/
     -           ''       Number of E/p points:    '',I6/
     -           ''       Pressure of the gas:     '',F10.3,'' [torr]''/
     -           ''       Temperature of the gas:  '',F10.3,'' [K]'')')
     -           EMIN,EMAX,ESTEP,ECRIT,FRCRIT,EPMIN,EPMAX,NGAS,PGAS,TGAS
            IF(NCMOB.GT.0)WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   :'',
     -           '' Mobility   = '',A)') STRMOB(1:NCMOB)
            IF(NCTWN.GT.0)WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   :'',
     -           '' Townsend   = '',A)') STRTWN(1:NCTWN)
            IF(NCATT.GT.0)WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   :'',
     -           '' Attachment = '',A)') STRATT(1:NCATT)
       ENDIF
*** Some preliminary plots.
       IF(LOSSPL.OR.LCSPL.OR.LPTHPL.OR.LTABPR)THEN
            CALL PROFLD(1,'Plotting cs and mfp',-1.0)
            CALL PROSTA(1,0.0)
            IF(LTABPR)WRITE(LUNOUT,'(''  TABLE OF INPUT GAS DATA''//5X,
     -           ''         Energy [eV]      Free path [cm]'',
     -           ''         Energy loss Cross section [cm2]''//)')
            DO 200 I=1,MXLIST
            XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
            CALL GASMXD(XPL(I),YPL1(I),YPL2(I))
            YPL3(I)=1/(XLOSCH*YPL1(I))
            IF(LTABPR)WRITE(LUNOUT,'(5X,4(5X,E15.8))')
     -           XPL(I),YPL1(I),YPL2(I),YPL3(I)
200         CONTINUE
            CALL GRAOPT('LOG-X')
            CALL GRAOPT('LOG-Y')
            IF(LPTHPL)THEN
                 CALL GRGRPH(XPL,YPL1,MXLIST,'Energy [eV]',
     -                'Mean path length [cm]','Mean path length')
                 CALL GRCOMM(1,'Gas: '//GASID)
                 CALL GRNEXT
                 CALL GRALOG('Mean free path of electrons in the gas: ')
            ENDIF
            IF(LOSSPL)THEN
                 CALL GRGRPH(XPL,YPL2,MXLIST,'Energy [eV]',
     -                'Fraction','Energy loss per collision')
                 CALL GRCOMM(1,'Gas: '//GASID)
                 CALL GRNEXT
                 CALL GRALOG('Average energy loss per collision:      ')
            ENDIF
            IF(LCSPL)THEN
                 CALL GRGRPH(XPL,YPL3,MXLIST,'Energy [eV]',
     -                'Cross section [cm2]','Cross section')
                 CALL GRCOMM(1,'Gas: '//GASID)
                 CALL GRNEXT
                 CALL GRALOG('Elastic cross section of the gas:       ')
            ENDIF
            CALL GRAOPT('LIN-X')
            CALL GRAOPT('LIN-Y')
       ENDIF
*** Translate the various functions if they have been specified.
       IF(NCMOB.GT.0)THEN
            CALL PROFLD(1,'Setting the mobility',-1.0)
            CALL PROSTA(1,0.0)
*   Call editor of specified as @.
            IF(INDEX(STRMOB(1:NCMOB),'@').NE.0)THEN
                 NRES=1
                 PRINT *,' ------ GASMIX MESSAGE : Please edit the'//
     -                ' mobility, function of EP (= E/p).'
                 CALL ALGEDT(VARLIS,1,IENMOB,USE,NRES)
                 IFAIL1=0
*   Usual function translation if not.
            ELSE
                 CALL ALGPRE(STRMOB,NCMOB,VARLIS,1,NRES,USE,IENMOB,
     -                IFAIL1)
            ENDIF
*   Check return code of translation.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASMIX WARNING : Ion mobility'//
     -                ' function rejected; no ion mobility in table.'
                 CALL ALGCLR(IENMOB)
                 NCMOB=0
            ENDIF
*   Check number of results returned by the function.
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! GASMIX WARNING : Number of'//
     -                ' results returned by the mobility function'//
     -                ' is not 1; rejected.'
                 CALL ALGCLR(IENMOB)
                 NCMOB=0
            ENDIF
       ENDIF
**  Townsend coefficient.
       IF(NCTWN.GT.0)THEN
            CALL PROFLD(1,'Setting Townsend',-1.0)
            CALL PROSTA(1,0.0)
*   Call editor of specified as @.
            IF(INDEX(STRTWN(1:NCTWN),'@').NE.0)THEN
                 NRES=1
                 PRINT *,' ------ GASMIX MESSAGE : Please edit the'//
     -                ' Townsend coefficient, function of EP (=E/p).'
                 CALL ALGEDT(VARLIS,1,IENTWN,USE,NRES)
                 IFAIL1=0
*   Usual function translation if not.
            ELSE
                 CALL ALGPRE(STRTWN,NCTWN,VARLIS,1,NRES,USE,IENTWN,
     -                IFAIL1)
            ENDIF
*   Check return code of translation.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASMIX WARNING : Townsend function'//
     -                ' rejected; no Townsend coefficient in table.'
                 CALL ALGCLR(IENTWN)
                 NCTWN=0
            ENDIF
*   Check number of results returned by the function.
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! GASMIX WARNING : Number of'//
     -                ' results returned by the Townsend function'//
     -                ' is not 1; rejected.'
                 CALL ALGCLR(IENTWN)
                 NCTWN=0
            ENDIF
       ENDIF
*** Attachment coefficient.
       IF(NCATT.GT.0)THEN
            CALL PROFLD(1,'Setting attachment',-1.0)
            CALL PROSTA(1,0.0)
*   Call editor of specified as @.
            IF(INDEX(STRATT(1:NCATT),'@').NE.0)THEN
                 NRES=1
                 PRINT *,' ------ GASMIX MESSAGE : Please edit the'//
     -                ' attachment coefficient, function of EP (=E/p).'
                 CALL ALGEDT(VARLIS,1,IENATT,USE,NRES)
                 IFAIL1=0
*   Usual function translation if not.
            ELSE
                 CALL ALGPRE(STRATT,NCATT,VARLIS,1,NRES,USE,IENATT,
     -                IFAIL1)
            ENDIF
*   Check return code of translation.
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASMIX WARNING : Attachment'//
     -                ' function rejected; no attachment in table.'
                 CALL ALGCLR(IENATT)
                 NCATT=0
            ENDIF
*   Check number of results returned by the function.
            IF(NRES.NE.1)THEN
                 PRINT *,' !!!!!! GASMIX WARNING : Number of'//
     -                ' results returned by the attachment function'//
     -                ' is not 1; rejected.'
                 CALL ALGCLR(IENATT)
                 NCATT=0
            ENDIF
       ENDIF
*** Loop over the electric field.
       EPCRIT=-1.0
       IF(LDISPL)CALL GRAOPT('LOG-X')
       CALL PROFLD(1,'Electric field',REAL(NGAS))
       DO 10 I=1,NGAS
       CALL PROSTA(1,REAL(I))
*** Logarithmic or linear spacing of the E/p points.
       IF(EPLOG)THEN
            EGAS(I)=EPMIN*(EPMAX/EPMIN)**(REAL(I-1)/REAL(MAX(1,NGAS-1)))
       ELSE
            EGAS(I)=EPMIN+(EPMAX-EPMIN)*(REAL(I-1)/REAL(MAX(1,NGAS-1)))
       ENDIF
*** Compute the mobility if requested.
       IF(NCMOB.GT.0)THEN
            VAR(1)=EGAS(I)
            MODVAR(1)=2
            CALL ALGEXE(IENMOB,VAR,MODVAR,1,RES,MODRES,1,IFAIL1)
            MGAS(I)=RES(1)
       ENDIF
*** Compute the Townsend coefficient if requested.
       IF(NCTWN.GT.0)THEN
            VAR(1)=EGAS(I)
            MODVAR(1)=2
            CALL ALGEXE(IENTWN,VAR,MODVAR,1,RES,MODRES,1,IFAIL1)
            AGAS(I)=RES(1)
       ENDIF
*** Compute the attachment coefficient if requested.
       IF(NCATT.GT.0)THEN
            VAR(1)=EGAS(I)
            MODVAR(1)=2
            CALL ALGEXE(IENATT,VAR,MODVAR,1,RES,MODRES,1,IFAIL1)
            BGAS(I)=RES(1)
       ENDIF
*** Copy for the gas-mixing common block.
       EFLD=PGAS*EGAS(I)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   : E='',
     -      E10.3,'' V/cm'')') EFLD
*** Find the maximum relevant energy.
       EEMAX=EMAX
40     CONTINUE
       ARG=GASMG1(FGAS1,DBLE(EEMAX),X)
       IF(ARG.LT.50.0)THEN
            EEMAX=EEMAX/0.9
            GOTO 30
       ELSE
            EEMAX=EEMAX*0.9
            GOTO 40
       ENDIF
30     CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   : Largest'',
     -      '' relevant electron energy: '',F10.3,'' eV.'')') EEMAX
*** Get the F0 normalisation straight.
       F0NORM=GASMG2(FGAS2N,DBLE(EEMAX),X)
*** Monitor electron excitation.
       F0OK=GASMG2(FGAS2N,DBLE(ECRIT),X)/F0NORM
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMIX DEBUG   : Fraction'',
     -      '' of F0 above ionisation: '',E10.3,''.'')') 1.0D0-F0OK
*** Plot the distribution if requested.
       IF(LDISPL)THEN
            F0MAX=0
            DO 20 J=1,MXLIST
            XPL(J)=EMIN*(EMAX/EMIN)**(REAL(J-1)/REAL(MXLIST-1))
            YPL1(J)=SQRT(XPL(J))*EXP(MAX(-60.0D0,
     -           -GASMG1(FGAS1,DBLE(XPL(J)),X)))/F0NORM
            IF(YPL1(J).GT.F0MAX)F0MAX=YPL1(J)
20          CONTINUE
            IF(I.EQ.1)CALL GRCART(EMIN,0.0,EMAX,1.1*F0MAX,
     -           'Energy [eV]','F0 [1/eV]','Distribution function')
            IF(REAL(1.0D0-F0OK).LT.FRCRIT)THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
            ELSE
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 IF(EPCRIT.LE.0)EPCRIT=EGAS(I)
            ENDIF
            CALL GRLINE(MXLIST,XPL,YPL1)
       ENDIF
*** Compute the drift velocity.
       VGAS(I)=1.0E-4*(2.0/3.0)*SQRT(0.5*ECHARG/EMASS)*EFLD*
     -      GASMG2(FGAS2V,DBLE(EEMAX),X)/F0NORM
*** Compute the diffusion coefficient.
       DGAS(I)=0.01*SQRT(2*PGAS*GASMG2(FGAS2D,DBLE(EEMAX),X)/
     -      (3*F0NORM*VGAS(I)))
10     CONTINUE
*** Close the plot.
       IF(LDISPL)THEN
            CALL GRCOMM(1,'Gas: '//GASID)
            IF(EPCRIT.GT.0)CALL GRCOMM(2,
     -           'WARNING: F0 for high E/p is affected by ionisation.')
            CALL GRNEXT
            CALL GRALOG('Distribution function F0')
            CALL GRAOPT('LIN-X')
       ENDIF
       CALL PROEND
*** Clear the mobility etc entry points - no longer needed.
       IF(NCMOB.GT.0)CALL ALGCLR(IENMOB)
       IF(NCTWN.GT.0)CALL ALGCLR(IENTWN)
       IF(NCATT.GT.0)CALL ALGCLR(IENATT)
*** Dump algebra error messages.
       IF(NCMOB.GT.0.OR.NCTWN.GT.0.OR.NCATT.GT.0)CALL ALGERR
*** Issue warnings if needed.
       IF(EPCRIT.GT.0.0)PRINT *,' !!!!!! GASMIX WARNING : Ionisation'//
     -      ' effects play a role for E/p > ',EPCRIT
*** Set the gas bits.
       GASOK(1)=.TRUE.
       IF(NCMOB.GT.0)GASOK(2)=.TRUE.
       GASOK(3)=.TRUE.
       IF(NCTWN.GT.0)GASOK(4)=.TRUE.
       IF(NCATT.GT.0)GASOK(6)=.TRUE.
*** Register the amount of CPU time with TIMLOG.
       CALL TIMLOG('Computing a gas mixture:                ')
       END
