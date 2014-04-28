CDECK  ID>, GASPLT.
       SUBROUTINE GASPLT
*-----------------------------------------------------------------------
*   GASPLT - Routine plotting the drift velocity, the diffusion coeff
*            and the cluster size distribution.
*   VARIABLES : XPL, YPL   : Arrays used for plotting.
*   (Last changed on  6/12/08.)
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
       REAL XPL(MXLIST),YPL(MXLIST),YPL3(0:MXPAIR+1),YPLMIN,YPLMAX,
     -      GASATT,GASTWN,GASDFL,GASDFT,GASMOB,GASLOR,GASVEL,GASVT1,
     -      GASVT2,GASDIS,EMIN,EMAX,AUX1(1),AUX2(1),DY,EXVECT(MXEXG),
     -      IOVECT(MXIOG)
       CHARACTER*20 STR1,STR2,STR3
       INTEGER I,J,K,L,NC1,NC2,NC3
       EXTERNAL GASATT,GASTWN,GASDFL,GASDFT,GASMOB,GASLOR,GASVEL,
     -      GASVT1,GASVT2
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE GASPLT ///'
*** Check that logarithmic plotting is possible.
       IF((GASOK(1).OR.GASOK(2).OR.GASOK(3).OR.GASOK(4).OR.
     -      GASOK(6).OR.GASOK(7).OR.GASOK(8).OR.GASOK(9).OR.
     -      GASOK(10).OR.GASOK(11).OR.GASOK(12).OR.GASOK(15).OR.
     -      GASOK(16)).AND.EGAS(1).LE.0.0)
     -      PRINT *,' !!!!!! GASPLT WARNING : First point in the gas'//
     -           ' table is not > 0 ; logarithmic plotting impossible'
*** Broaden the scale a little to show the extrapolation.
       IF(NGAS.LT.1)THEN
            PRINT *,' !!!!!! GASPLT WARNING : No gas data points; '//
     -           ' no plots made.'
            RETURN
       ENDIF
*** Plot the drift velocity.
       IF(GASOPT(1,3).AND.(GASOK(1).OR.GASOK(9).OR.GASOK(10)))THEN
*   Set the electric field range.
            IF(GASOPT(1,1))THEN
                 EMIN=PGAS*EGAS(1)/1.5
                 EMAX=PGAS*EGAS(NGAS)*1.5
                 DO 101 I=1,MXLIST
                 XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
101              CONTINUE
            ELSE
                 EMIN=MAX(0.0,PGAS*(EGAS(1)-ABS(EGAS(NGAS)-EGAS(1))/20))
                 EMAX=PGAS*(EGAS(NGAS)+ABS(EGAS(NGAS)-EGAS(1))/20)
                 DO 102 I=1,MXLIST
                 XPL(I)=EMIN+REAL(I-1)*(EMAX-EMIN)/REAL(MXLIST-1)
102              CONTINUE
            ENDIF
*   Determine the scale of the graph.
            IF(GASOPT(1,4))THEN
                 YPLMIN=GASRNG(1,1)
                 YPLMAX=GASRNG(1,2)
            ELSEIF(TAB2D)THEN
                 IF(GASOK(1))THEN
                      YPLMIN=VGAS2(1,1,1)
                      YPLMAX=VGAS2(1,1,1)
                 ELSEIF(GASOK(9))THEN
                      YPLMIN=XGAS2(1,1,1)
                      YPLMAX=XGAS2(1,1,1)
                 ELSE
                      YPLMIN=YGAS2(1,1,1)
                      YPLMAX=YGAS2(1,1,1)
                 ENDIF
                 DO 100 K=1,NBTAB
                 DO 110 I=1,NGAS
                 DO 120 J=1,NBANG
                 IF(GASOK(1))THEN
                      YPLMIN=MIN(YPLMIN,VGAS2(I,J,K))
                      YPLMAX=MAX(YPLMAX,VGAS2(I,J,K))
                 ENDIF
                 IF(GASOK(9))THEN
                      YPLMIN=MIN(YPLMIN,XGAS2(I,J,K))
                      YPLMAX=MAX(YPLMAX,XGAS2(I,J,K))
                 ENDIF
                 IF(GASOK(10))THEN
                      YPLMIN=MIN(YPLMIN,YGAS2(I,J,K))
                      YPLMAX=MAX(YPLMAX,YGAS2(I,J,K))
                 ENDIF
120              CONTINUE
110              CONTINUE
100              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ELSE
                 IF(GASOK(1))THEN
                      YPLMIN=VGAS(1)
                      YPLMAX=VGAS(1)
                 ELSEIF(GASOK(9))THEN
                      YPLMIN=XGAS(1)
                      YPLMAX=XGAS(1)
                 ELSE
                      YPLMIN=YGAS(1)
                      YPLMAX=YGAS(1)
                 ENDIF
                 DO 130 I=1,NGAS
                 IF(GASOK(1))THEN
                      YPLMIN=MIN(YPLMIN,VGAS(I))
                      YPLMAX=MAX(YPLMAX,VGAS(I))
                 ENDIF
                 IF(GASOK(9))THEN
                      YPLMIN=MIN(YPLMIN,XGAS(I))
                      YPLMAX=MAX(YPLMAX,XGAS(I))
                 ENDIF
                 IF(GASOK(10))THEN
                      YPLMIN=MIN(YPLMIN,YGAS(I))
                      YPLMAX=MAX(YPLMAX,YGAS(I))
                 ENDIF
130              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ENDIF
*   Can be that the range is still nil or negative and log.
            IF(YPLMAX.LE.0)THEN
                 PRINT *,' !!!!!! GASPLT WARNING : Drift velocity'//
     -                ' is zero everywhere ; not plotted.'
                 GOTO 199
            ENDIF
            IF(GASOPT(1,2))THEN
                 IF(YPLMIN.LE.0)YPLMIN=1
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=100
            ELSE
                 IF(YPLMIN.GT.0)YPLMIN=0
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=100
            ENDIF
*   Loop over the B fields.
            DO 140 K=1,NBTAB
*   Plot the frame.
            IF(GASOPT(1,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(1,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
            CALL GRCART(EMIN,YPLMIN,EMAX,YPLMAX,
     -           'E [V/cm]','Drift velocity [cm/microsec]',
     -           'Drift velocity vs E')
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            IF(TAB2D)THEN
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 CALL GRCOMM(2,'B = '//STR1(1:NC1)//' T')
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,'LEFT')
                 CALL OUTFMT(REAL(NBANG),2,STR3,NC3,'LEFT')
                 CALL GRCOMM(4,STR1(1:NC1)//' < angle(E,B) < '//
     -                STR2(1:NC2)//' degrees in '//STR3(1:NC3)//
     -                ' steps')
            ENDIF
*   Plot and mark the various curves, first the E component.
            IF(GASOK(1))THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
                 IF(TAB2D)THEN
                      DO 150 I=1,NBANG
                      DO 160 J=1,MXLIST
                      YPL(J)=GASVEL(XPL(J),0.0,0.0,BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
160                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
150                   CONTINUE
                 ELSE
                      DO 170 I=1,MXLIST
                      YPL(I)=GASVEL(XPL(I),0.0,0.0,0.0,0.0,0.0)
170                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-1','POLYMARKER')
                 DO 190 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 180 I=1,NBANG
                      AUX2(1)=VGAS2(J,I,K)
                      CALL GRMARK(1,AUX1,AUX2)
180                   CONTINUE
                 ELSE
                      AUX2(1)=VGAS(J)
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
190              CONTINUE
            ENDIF
*   Next the B component.
            IF(GASOK(9))THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 IF(TAB2D)THEN
                      DO 151 I=1,NBANG
                      DO 161 J=1,MXLIST
                      YPL(J)=GASVT1(XPL(J),0.0,0.0,BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
161                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
151                   CONTINUE
                 ELSE
                      DO 171 I=1,MXLIST
                      YPL(I)=GASVT1(XPL(I),0.0,0.0,0.0,0.0,0.0)
171                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-2','POLYMARKER')
                 DO 191 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 181 I=1,NBANG
                      AUX2(1)=XGAS2(J,I,K)
                      CALL GRMARK(1,AUX1,AUX2)
181                   CONTINUE
                 ELSE
                      AUX2(1)=XGAS(J)
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
191              CONTINUE
            ENDIF
*   And finally the ExB component.
            IF(GASOK(10))THEN
                 CALL GRATTS('FUNCTION-3','POLYLINE')
                 IF(TAB2D)THEN
                      DO 152 I=1,NBANG
                      DO 162 J=1,MXLIST
                      YPL(J)=GASVT2(XPL(J),0.0,0.0,BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
162                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
152                   CONTINUE
                 ELSE
                      DO 172 I=1,MXLIST
                      YPL(I)=GASVT2(XPL(I),0.0,0.0,0.0,0.0,0.0)
172                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-3','POLYMARKER')
                 DO 192 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 182 I=1,NBANG
                      AUX2(1)=YGAS2(J,I,K)
                      CALL GRMARK(1,AUX1,AUX2)
182                   CONTINUE
                 ELSE
                      AUX2(1)=YGAS(J)
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
192              CONTINUE
            ENDIF
            CALL GRNEXT
            CALL GRALOG('Graph of the drift velocity vs E.')
*   Next B field.
140         CONTINUE
       ENDIF
*   Continue here if the plot was skipped.
199    CONTINUE
*** Plot the ion mobility.
       IF(GASOPT(2,3).AND.GASOK(2))THEN
*   Set the electric field range.
            IF(GASOPT(2,1))THEN
                 EMIN=PGAS*EGAS(1)/1.5
                 EMAX=PGAS*EGAS(NGAS)*1.5
                 DO 201 I=1,MXLIST
                 XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
201              CONTINUE
            ELSE
                 EMIN=MAX(0.0,PGAS*(EGAS(1)-ABS(EGAS(NGAS)-EGAS(1))/20))
                 EMAX=PGAS*(EGAS(NGAS)+ABS(EGAS(NGAS)-EGAS(1))/20)
                 DO 202 I=1,MXLIST
                 XPL(I)=EMIN+REAL(I-1)*(EMAX-EMIN)/REAL(MXLIST-1)
202              CONTINUE
            ENDIF
*   Determine the scale of the graph.
            IF(GASOPT(2,4))THEN
                 YPLMIN=GASRNG(2,1)
                 YPLMAX=GASRNG(2,2)
            ELSEIF(TAB2D)THEN
                 YPLMIN=MGAS2(1,1,1)
                 YPLMAX=MGAS2(1,1,1)
                 DO 200 K=1,NBTAB
                 DO 210 I=1,NGAS
                 DO 220 J=1,NBANG
                 YPLMIN=MIN(YPLMIN,MGAS2(I,J,K))
                 YPLMAX=MAX(YPLMAX,MGAS2(I,J,K))
220              CONTINUE
210              CONTINUE
200              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ELSE
                 YPLMIN=MGAS(1)
                 YPLMAX=MGAS(1)
                 DO 230 I=2,NGAS
                 YPLMIN=MIN(YPLMIN,MGAS(I))
                 YPLMAX=MAX(YPLMAX,MGAS(I))
230              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ENDIF
*   Loop over the B fields.
            DO 240 K=1,NBTAB
*   Plot the frame.
            IF(GASOPT(2,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(2,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
            CALL GRCART(EMIN,YPLMIN,EMAX,YPLMAX,
     -           'E [V/cm]','Ion mobility [cm2/V.microsec]',
     -           'Ion mobility vs E')
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            IF(TAB2D)THEN
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 CALL GRCOMM(2,'B = '//STR1(1:NC1)//' T')
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,'LEFT')
                 CALL OUTFMT(REAL(NBANG),2,STR3,NC3,'LEFT')
                 CALL GRCOMM(4,STR1(1:NC1)//' < angle(E,B) < '//
     -                STR2(1:NC2)//' degrees in '//STR3(1:NC3)//
     -                ' steps')
            ENDIF
*   Plot the various curves.
            CALL GRATTS('FUNCTION-1','POLYLINE')
            IF(TAB2D)THEN
                 DO 250 I=1,NBANG
                 DO 260 J=1,MXLIST
                 YPL(J)=GASMOB(XPL(J),0.0,0.0,BTAB(K)*COS(BANG(I)),
     -                BTAB(K)*SIN(BANG(I)),0.0)
260              CONTINUE
                 CALL GRLINE(MXLIST,XPL,YPL)
250              CONTINUE
            ELSE
                 DO 270 I=1,MXLIST
                 YPL(I)=GASMOB(XPL(I),0.0,0.0,0.0,0.0,0.0)
270              CONTINUE
                 CALL GRLINE(MXLIST,XPL,YPL)
            ENDIF
*   Polymark the data, allowing a check on the interpolation.
            CALL GRATTS('FUNCTION-1','POLYMARKER')
            DO 290 J=1,NGAS
            AUX1(1)=PGAS*EGAS(J)
            IF(TAB2D)THEN
                 DO 280 I=1,NBANG
                 CALL GRMARK(1,AUX1,MGAS2(J,I,K))
280              CONTINUE
            ELSE
                 CALL GRMARK(1,AUX1,MGAS(J))
            ENDIF
290         CONTINUE
            CALL GRNEXT
            CALL GRALOG('Graph of the ion mobility vs E.')
*   Next B field.
240         CONTINUE
       ENDIF
*** Plot the diffusion coefficients.
       IF(GASOPT(3,3).AND.(GASOK(3).OR.GASOK(8)))THEN
*   Set the electric field range.
            IF(GASOPT(3,1))THEN
                 EMIN=PGAS*EGAS(1)/1.5
                 EMAX=PGAS*EGAS(NGAS)*1.5
                 DO 301 I=1,MXLIST
                 XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
301              CONTINUE
            ELSE
                 EMIN=MAX(0.0,PGAS*(EGAS(1)-ABS(EGAS(NGAS)-EGAS(1))/20))
                 EMAX=PGAS*(EGAS(NGAS)+ABS(EGAS(NGAS)-EGAS(1))/20)
                 DO 302 I=1,MXLIST
                 XPL(I)=EMIN+REAL(I-1)*(EMAX-EMIN)/REAL(MXLIST-1)
302              CONTINUE
            ENDIF
*   Determine the scale of the graph.
            IF(GASOPT(3,4))THEN
                 YPLMIN=GASRNG(3,1)
                 YPLMAX=GASRNG(3,2)
            ELSEIF(TAB2D)THEN
                 IF(GASOK(3))THEN
                      YPLMIN=DGAS2(1,1,1)*10000/SQRT(PGAS)
                      YPLMAX=DGAS2(1,1,1)*10000/SQRT(PGAS)
                 ELSE
                      YPLMIN=OGAS2(1,1,1)*10000/SQRT(PGAS)
                      YPLMAX=OGAS2(1,1,1)*10000/SQRT(PGAS)
                 ENDIF
                 DO 300 K=1,NBTAB
                 DO 310 I=1,NGAS
                 DO 320 J=1,NBANG
                 IF(GASOK(3))THEN
                      YPLMIN=MIN(YPLMIN,DGAS2(I,J,K)*10000/SQRT(PGAS))
                      YPLMAX=MAX(YPLMAX,DGAS2(I,J,K)*10000/SQRT(PGAS))
                 ENDIF
                 IF(GASOK(8))THEN
                      YPLMIN=MIN(YPLMIN,OGAS2(I,J,K)*10000/SQRT(PGAS))
                      YPLMAX=MAX(YPLMAX,OGAS2(I,J,K)*10000/SQRT(PGAS))
                 ENDIF
320              CONTINUE
310              CONTINUE
300              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ELSE
                 IF(GASOK(3))THEN
                      YPLMIN=DGAS(1)*10000/SQRT(PGAS)
                      YPLMAX=DGAS(1)*10000/SQRT(PGAS)
                 ELSE
                      YPLMIN=OGAS(1)
                      YPLMAX=OGAS(1)
                 ENDIF
                 DO 330 I=1,NGAS
                 IF(GASOK(3))THEN
                      YPLMIN=MIN(YPLMIN,DGAS(I)*10000/SQRT(PGAS))
                      YPLMAX=MAX(YPLMAX,DGAS(I)*10000/SQRT(PGAS))
                 ENDIF
                 IF(GASOK(8))THEN
                      YPLMIN=MIN(YPLMIN,OGAS(I)*10000/SQRT(PGAS))
                      YPLMAX=MAX(YPLMAX,OGAS(I)*10000/SQRT(PGAS))
                 ENDIF
330              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ENDIF
*   Can be that the range is still nil or negative and log.
            IF(YPLMAX.LE.0)THEN
                 PRINT *,' !!!!!! GASPLT WARNING : Diffusion'//
     -                ' coefficients = 0 ; not plotted.'
                 GOTO 399
            ENDIF
            IF(GASOPT(3,2))THEN
                 IF(YPLMIN.LE.0)YPLMIN=1
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=1000
            ELSE
                 YPLMIN=0
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=1000
            ENDIF
*   Loop over the B fields.
            DO 340 K=1,NBTAB
*   Plot the frame.
            IF(GASOPT(3,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(3,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
            CALL GRCART(EMIN,YPLMIN,EMAX,YPLMAX,
     -           'E [V/cm]','Diffusion [micron for 1 cm]',
     -           'Diffusion coefficients vs E')
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            IF(TAB2D)THEN
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 CALL GRCOMM(2,'B = '//STR1(1:NC1)//' T')
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,'LEFT')
                 CALL OUTFMT(REAL(NBANG),2,STR3,NC3,'LEFT')
                 CALL GRCOMM(4,STR1(1:NC1)//' < angle(E,B) < '//
     -                STR2(1:NC2)//' degrees in '//STR3(1:NC3)//
     -                ' steps')
            ENDIF
*   Plot and mark the various curves.
            IF(GASOK(3))THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
                 IF(TAB2D)THEN
                      DO 350 I=1,NBANG
                      DO 360 J=1,MXLIST
                      YPL(J)=10000*GASDFL(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
360                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
350                   CONTINUE
                 ELSE
                      DO 370 I=1,MXLIST
                      YPL(I)=10000*GASDFL(XPL(I),0.0,0.0,0.0,0.0,0.0)
370                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-1','POLYMARKER')
                 DO 390 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 380 I=1,NBANG
                      AUX2(1)=10000*DGAS2(J,I,K)/SQRT(PGAS)
                      CALL GRMARK(1,AUX1,AUX2)
380                   CONTINUE
                 ELSE
                      AUX2(1)=10000*DGAS(J)/SQRT(PGAS)
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
390              CONTINUE
            ENDIF
            IF(GASOK(8))THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 IF(TAB2D)THEN
                      DO 355 I=1,NBANG
                      DO 365 J=1,MXLIST
                      YPL(J)=10000*GASDFT(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
365                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
355                   CONTINUE
                 ELSE
                      DO 375 I=1,MXLIST
                      YPL(I)=10000*GASDFT(XPL(I),0.0,0.0,0.0,0.0,0.0)
375                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-2','POLYMARKER')
                 DO 395 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 385 I=1,NBANG
                      AUX2(1)=10000*OGAS2(J,I,K)/SQRT(PGAS)
                      CALL GRMARK(1,AUX1,AUX2)
385                   CONTINUE
                 ELSE
                      AUX2(1)=10000*OGAS(J)/SQRT(PGAS)
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
395              CONTINUE
            ENDIF
            CALL GRNEXT
            CALL GRALOG('Graph of the diffusion coefficients vs E.')
*   Next B field.
340         CONTINUE
       ENDIF
*   Continue here if the plot was skipped.
399    CONTINUE
*** Plot the Townsend, attachment and dissociation coefficients.
       IF(GASOPT(4,3).AND.(GASOK(4).OR.GASOK(6).OR.GASOK(12)))THEN
*   Set the electric field range.
            IF(GASOPT(4,1))THEN
                 EMIN=PGAS*EGAS(1)/1.5
                 EMAX=PGAS*EGAS(NGAS)*1.5
                 DO 401 I=1,MXLIST
                 XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
401              CONTINUE
            ELSE
                 EMIN=MAX(0.0,PGAS*(EGAS(1)-ABS(EGAS(NGAS)-EGAS(1))/20))
                 EMAX=PGAS*(EGAS(NGAS)+ABS(EGAS(NGAS)-EGAS(1))/20)
                 DO 402 I=1,MXLIST
                 XPL(I)=EMIN+REAL(I-1)*(EMAX-EMIN)/REAL(MXLIST-1)
402              CONTINUE
            ENDIF
*   Determine the scale of the graph.
            IF(GASOPT(4,4))THEN
                 YPLMIN=GASRNG(4,1)
                 YPLMAX=GASRNG(4,2)
            ELSEIF(TAB2D)THEN
                 IF(GASOK(4))THEN
                      YPLMIN=EXP(AGAS2(1,1,1))*PGAS
                      YPLMAX=EXP(AGAS2(1,1,1))*PGAS
                 ELSEIF(GASOK(6))THEN
                      YPLMIN=EXP(BGAS2(1,1,1))*PGAS
                      YPLMAX=EXP(BGAS2(1,1,1))*PGAS
                 ELSE
                      YPLMIN=EXP(HGAS2(1,1,1))*PGAS
                      YPLMAX=EXP(HGAS2(1,1,1))*PGAS
                 ENDIF
                 DO 400 K=1,NBTAB
                 DO 410 I=1,NGAS
                 DO 420 J=1,NBANG
                 IF(GASOK(4))THEN
                      YPLMIN=MIN(YPLMIN,EXP(AGAS2(I,J,K))*PGAS)
                      YPLMAX=MAX(YPLMAX,EXP(AGAS2(I,J,K))*PGAS)
                 ENDIF
                 IF(GASOK(6))THEN
                      YPLMIN=MIN(YPLMIN,EXP(BGAS2(I,J,K))*PGAS)
                      YPLMAX=MAX(YPLMAX,EXP(BGAS2(I,J,K))*PGAS)
                 ENDIF
                 IF(GASOK(12))THEN
                      YPLMIN=MIN(YPLMIN,EXP(HGAS2(I,J,K))*PGAS)
                      YPLMAX=MAX(YPLMAX,EXP(HGAS2(I,J,K))*PGAS)
                 ENDIF
420              CONTINUE
410              CONTINUE
400              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ELSE
                 IF(GASOK(4))THEN
                      YPLMIN=EXP(AGAS(1))*PGAS
                      YPLMAX=EXP(AGAS(1))*PGAS
                 ELSEIF(GASOK(6))THEN
                      YPLMIN=EXP(AGAS(1))*PGAS
                      YPLMAX=EXP(AGAS(1))*PGAS
                 ELSE
                      YPLMIN=EXP(HGAS(1))*PGAS
                      YPLMAX=EXP(HGAS(1))*PGAS
                 ENDIF
                 DO 430 I=1,NGAS
                 IF(GASOK(4))THEN
                      YPLMIN=MIN(YPLMIN,EXP(AGAS(I))*PGAS)
                      YPLMAX=MAX(YPLMAX,EXP(AGAS(I))*PGAS)
                 ENDIF
                 IF(GASOK(6))THEN
                      YPLMIN=MIN(YPLMIN,EXP(BGAS(I))*PGAS)
                      YPLMAX=MAX(YPLMAX,EXP(BGAS(I))*PGAS)
                 ENDIF
                 IF(GASOK(12))THEN
                      YPLMIN=MIN(YPLMIN,EXP(HGAS(I))*PGAS)
                      YPLMAX=MAX(YPLMAX,EXP(HGAS(I))*PGAS)
                 ENDIF
430              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ENDIF
*   Can be that the range is still nil or negative and log.
            IF(YPLMAX.LE.-20)THEN
                 PRINT *,' !!!!!! GASPLT WARNING : The Townsend,'//
     -                ' attachment and dissociation coefficients'//
     -                ' are all nearly 0 ; not plotted.'
                 GOTO 499
            ENDIF
            IF(GASOPT(4,2))THEN
                 IF(YPLMIN.LE.0.01)YPLMIN=0.01
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=YPLMIN*2
            ELSE
                 YPLMIN=0
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=1000
            ENDIF
*   Loop over the B fields.
            DO 440 K=1,NBTAB
*   Plot the frame.
            IF(GASOPT(4,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(4,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
            CALL GRCART(EMIN,YPLMIN,EMAX,YPLMAX,
     -           'E [V/cm]',
     -           'Townsend, attachment, dissociation coeff. [1/cm]',
     -           'Townsend, attachment, dissociation coeff. vs E')
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            IF(TAB2D)THEN
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 CALL GRCOMM(2,'B = '//STR1(1:NC1)//' T')
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,'LEFT')
                 CALL OUTFMT(REAL(NBANG),2,STR3,NC3,'LEFT')
                 CALL GRCOMM(4,STR1(1:NC1)//' < angle(E,B) < '//
     -                STR2(1:NC2)//' degrees in '//STR3(1:NC3)//
     -                ' steps')
            ENDIF
*   Plot and mark the various curves.
            IF(GASOK(4))THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
                 IF(TAB2D)THEN
                      DO 450 I=1,NBANG
                      DO 460 J=1,MXLIST
                      YPL(J)=GASTWN(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
460                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
450                   CONTINUE
                 ELSE
                      DO 470 I=1,MXLIST
                      YPL(I)=GASTWN(XPL(I),0.0,0.0,0.0,0.0,0.0)
470                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-1','POLYMARKER')
                 DO 490 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 480 I=1,NBANG
                      AUX2(1)=EXP(AGAS2(J,I,K))*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
480                   CONTINUE
                 ELSE
                      AUX2(1)=EXP(AGAS(J))*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
490              CONTINUE
            ENDIF
            IF(GASOK(6))THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 IF(TAB2D)THEN
                      DO 455 I=1,NBANG
                      DO 465 J=1,MXLIST
                      YPL(J)=GASATT(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
465                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
455                   CONTINUE
                 ELSE
                      DO 475 I=1,MXLIST
                      YPL(I)=GASATT(XPL(I),0.0,0.0,0.0,0.0,0.0)
475                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-2','POLYMARKER')
                 DO 495 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 485 I=1,NBANG
                      AUX2(1)=EXP(BGAS2(J,I,K))*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
485                   CONTINUE
                 ELSE
                      AUX2(1)=EXP(BGAS(J))*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
495              CONTINUE
            ENDIF
            IF(GASOK(12))THEN
                 CALL GRATTS('FUNCTION-3','POLYLINE')
                 IF(TAB2D)THEN
                      DO 457 I=1,NBANG
                      DO 467 J=1,MXLIST
                      YPL(J)=GASDIS(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0)
467                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
457                   CONTINUE
                 ELSE
                      DO 477 I=1,MXLIST
                      YPL(I)=GASDIS(XPL(I),0.0,0.0,0.0,0.0,0.0)
477                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
                 ENDIF
                 CALL GRATTS('FUNCTION-3','POLYMARKER')
                 DO 497 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 487 I=1,NBANG
                      AUX2(1)=EXP(HGAS2(J,I,K))*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
487                   CONTINUE
                 ELSE
                      AUX2(1)=EXP(HGAS(J))*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
                 ENDIF
497              CONTINUE
            ENDIF
            CALL GRNEXT
            CALL GRALOG('Townsend, attachment & dissociation')
*   Next B field.
440         CONTINUE
       ENDIF
*   Continue here if the plot was skipped.
499    CONTINUE
*** Plot the Lorentz angle.
       IF(GASOPT(7,3).AND.GASOK(7))THEN
*   Set the electric field range.
            IF(GASOPT(7,1))THEN
                 EMIN=PGAS*EGAS(1)/1.5
                 EMAX=PGAS*EGAS(NGAS)*1.5
                 DO 501 I=1,MXLIST
                 XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
501              CONTINUE
            ELSE
                 EMIN=MAX(0.0,PGAS*(EGAS(1)-ABS(EGAS(NGAS)-EGAS(1))/20))
                 EMAX=PGAS*(EGAS(NGAS)+ABS(EGAS(NGAS)-EGAS(1))/20)
                 DO 502 I=1,MXLIST
                 XPL(I)=EMIN+REAL(I-1)*(EMAX-EMIN)/REAL(MXLIST-1)
502              CONTINUE
            ENDIF
*   Determine the scale of the graph.
            IF(GASOPT(7,4))THEN
                 YPLMIN=180*GASRNG(7,1)/PI
                 YPLMAX=180*GASRNG(7,2)/PI
            ELSEIF(TAB2D)THEN
                 YPLMIN=180*WGAS2(1,1,1)/PI
                 YPLMAX=180*WGAS2(1,1,1)/PI
                 DO 500 K=1,NBTAB
                 DO 510 I=1,NGAS
                 DO 520 J=1,NBANG
                 YPLMIN=MIN(YPLMIN,180*WGAS2(I,J,K)/PI)
                 YPLMAX=MAX(YPLMAX,180*WGAS2(I,J,K)/PI)
520              CONTINUE
510              CONTINUE
500              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ELSE
                 YPLMIN=180*WGAS(1)/PI
                 YPLMAX=180*WGAS(1)/PI
                 DO 530 I=2,NGAS
                 YPLMIN=MIN(YPLMIN,180*WGAS(I)/PI)
                 YPLMAX=MAX(YPLMAX,180*WGAS(I)/PI)
530              CONTINUE
                 DY=(YPLMAX-YPLMIN)/20
                 YPLMAX=YPLMAX+DY
                 YPLMIN=YPLMIN-DY
            ENDIF
*   Loop over the B fields.
            DO 540 K=1,NBTAB
*   Plot the frame.
            IF(GASOPT(7,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(7,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
            CALL GRCART(EMIN,YPLMIN,EMAX,YPLMAX,
     -           'E [V/cm]','Angle between v and E [degrees]',
     -           'Angle between v and E vs E')
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            IF(TAB2D)THEN
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 CALL GRCOMM(2,'B = '//STR1(1:NC1)//' T')
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,'LEFT')
                 CALL OUTFMT(REAL(NBANG),2,STR3,NC3,'LEFT')
                 CALL GRCOMM(4,STR1(1:NC1)//' < angle(E,B) < '//
     -                STR2(1:NC2)//' degress in '//STR3(1:NC3)//
     -                ' steps')
            ENDIF
*   Plot the various curves.
            CALL GRATTS('FUNCTION-1','POLYLINE')
            IF(TAB2D)THEN
                 DO 550 I=1,NBANG
                 DO 560 J=1,MXLIST
                 YPL(J)=180*GASLOR(XPL(J),0.0,0.0,BTAB(K)*COS(BANG(I)),
     -                BTAB(K)*SIN(BANG(I)),0.0)/PI
560              CONTINUE
                 CALL GRLINE(MXLIST,XPL,YPL)
550              CONTINUE
            ELSE
                 DO 570 I=1,MXLIST
                 YPL(I)=180*GASLOR(XPL(I),0.0,0.0,0.0,0.0,0.0)/PI
570              CONTINUE
                 CALL GRLINE(MXLIST,XPL,YPL)
            ENDIF
*   Polymark the data, allowing a check on the interpolation.
            CALL GRATTS('FUNCTION-1','POLYMARKER')
            DO 590 J=1,NGAS
            AUX1(1)=PGAS*EGAS(J)
            IF(TAB2D)THEN
                 DO 580 I=1,NBANG
                 AUX2(1)=180*WGAS2(J,I,K)/PI
                 CALL GRMARK(1,AUX1,AUX2)
580              CONTINUE
            ELSE
                 AUX2(1)=180*WGAS(J)/PI
                 CALL GRMARK(1,AUX1,AUX2)
            ENDIF
590         CONTINUE
            CALL GRNEXT
            CALL GRALOG('Graph of the (v,E) angle vs E.')
*   Next B field.
540         CONTINUE
       ENDIF
*** Cluster size distribution.
       IF(GASOPT(5,3).AND.GASOK(5))THEN
*   Set log or linear axes, as requested.
            IF(GASOPT(5,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(5,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
*   Recover the cluster size distribution.
            YPL3(0)=0
            DO 60 I=1,MIN(MXPAIR,NCLS)
            IF(I.EQ.1)THEN
                 YPL3(I)=CLSDIS(I)
            ELSE
                 YPL3(I)=CLSDIS(I)-CLSDIS(I-1)
            ENDIF
60          CONTINUE
            YPL3(MIN(NCLS,MXPAIR)+1)=0
*   Plot the histogram.
            CALL GRHIST(YPL3,MIN(MXPAIR,NCLS),
     -           0.0,REAL(MIN(MXPAIR,NCLS)),
     -           'Number of pairs in a cluster',
     -           'Cluster size distribution',.TRUE.)
*   Add a bit of information to the plot.
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            CALL GRCOMM(2,'Origin: '//CLSTYP)
            CALL GRALOG('Graph of the cluster size distribution  ')
            CALL GRNEXT
       ENDIF
*** Plot the excitation and ionisation rates.
       IF(GASOPT(15,3).AND.(GASOK(15).OR.GASOK(16)).AND.
     -      (NEXGAS.GE.1.OR.NIOGAS.GE.1))THEN
*   Set the electric field range.
            IF(GASOPT(15,1))THEN
                 EMIN=PGAS*EGAS(1)/1.5
                 EMAX=PGAS*EGAS(NGAS)*1.5
                 DO 601 I=1,MXLIST
                 XPL(I)=EMIN*(EMAX/EMIN)**(REAL(I-1)/REAL(MXLIST-1))
601              CONTINUE
            ELSE
                 EMIN=MAX(0.0,PGAS*(EGAS(1)-ABS(EGAS(NGAS)-EGAS(1))/20))
                 EMAX=PGAS*(EGAS(NGAS)+ABS(EGAS(NGAS)-EGAS(1))/20)
                 DO 602 I=1,MXLIST
                 XPL(I)=EMIN+REAL(I-1)*(EMAX-EMIN)/REAL(MXLIST-1)
602              CONTINUE
            ENDIF
*   Determine the scale of the graph.
            IF(GASOPT(15,4))THEN
                 YPLMIN=GASRNG(15,1)
                 YPLMAX=GASRNG(15,2)
            ELSEIF(TAB2D)THEN
                 YPLMIN=-1
                 YPLMAX=-1
                 DO 600 K=1,NBTAB
                 DO 610 I=1,NGAS
                 DO 620 J=1,NBANG
                 IF(GASOK(15))THEN
                      DO 625 L=1,NEXGAS
                      IF(EXGAS2(I,J,K,L).GT.0.AND.YPLMIN.LE.0)THEN
                           YPLMIN=EXGAS2(I,J,K,L)*PGAS
                      ELSE
                           YPLMIN=MIN(YPLMIN,EXGAS2(I,J,K,L)*PGAS)
                      ENDIF
                      IF(EXGAS2(I,J,K,L).GT.0.AND.YPLMAX.LE.0)THEN
                           YPLMAX=EXGAS2(I,J,K,L)*PGAS
                      ELSE
                           YPLMAX=MAX(YPLMAX,EXGAS2(I,J,K,L)*PGAS)
                      ENDIF
625                   CONTINUE
                 ENDIF
                 IF(GASOK(16))THEN
                      DO 626 L=1,NIOGAS
                      IF(IOGAS2(I,J,K,L).GT.0.AND.YPLMIN.LE.0)THEN
                           YPLMIN=IOGAS2(I,J,K,L)*PGAS
                      ELSE
                           YPLMIN=MIN(YPLMIN,IOGAS2(I,J,K,L)*PGAS)
                      ENDIF
                      IF(IOGAS2(I,J,K,L).GT.0.AND.YPLMAX.LE.0)THEN
                           YPLMAX=IOGAS2(I,J,K,L)*PGAS
                      ELSE
                           YPLMAX=MAX(YPLMAX,IOGAS2(I,J,K,L)*PGAS)
                      ENDIF
626                   CONTINUE
                 ENDIF
620              CONTINUE
610              CONTINUE
600              CONTINUE
            ELSE
                 YPLMIN=-1
                 YPLMAX=-1
                 DO 630 I=1,NGAS
                 IF(GASOK(15))THEN
                      DO 635 L=1,NEXGAS
                      IF(EXGAS(I,L).GT.0.AND.YPLMIN.LE.0)THEN
                           YPLMIN=EXGAS(I,L)*PGAS
                      ELSE
                           YPLMIN=MIN(YPLMIN,EXGAS(I,L)*PGAS)
                      ENDIF
                      IF(EXGAS(I,L).GT.0.AND.YPLMAX.LE.0)THEN
                           YPLMAX=EXGAS(I,L)*PGAS
                      ELSE
                           YPLMAX=MAX(YPLMAX,EXGAS(I,L)*PGAS)
                      ENDIF
635                   CONTINUE
                 ENDIF
                 IF(GASOK(16))THEN
                      DO 636 L=1,NIOGAS
                      IF(IOGAS(I,L).GT.0.AND.YPLMIN.LE.0)THEN
                           YPLMIN=IOGAS(I,L)*PGAS
                      ELSE
                           YPLMIN=MIN(YPLMIN,IOGAS(I,L)*PGAS)
                      ENDIF
                      IF(IOGAS(I,L).GT.0.AND.YPLMAX.LE.0)THEN
                           YPLMAX=IOGAS(I,L)*PGAS
                      ELSE
                           YPLMAX=MAX(YPLMAX,IOGAS(I,L)*PGAS)
                      ENDIF
636                   CONTINUE
                 ENDIF
630              CONTINUE
            ENDIF
*   Can be that the range is still nil or negative and log.
            IF(YPLMAX.LE.0.OR.YPLMIN.LE.0)THEN
                 PRINT *,' !!!!!! GASPLT WARNING : The ionisation'//
     -                ' and excitation rates are all nearly 0 ;'//
     -                ' not plotted.'
                 GOTO 699
            ENDIF
            IF(GASOPT(15,2))THEN
                 YPLMIN=YPLMIN*0.9
                 YPLMAX=YPLMAX*1.1
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=YPLMIN*2
            ELSE
                 YPLMAX=YPLMAX+(YPLMAX-YPLMIN)/20
                 YPLMIN=0
                 IF(YPLMAX.LE.YPLMIN)YPLMAX=YPLMIN+1
            ENDIF
*   Loop over the B fields.
            DO 640 K=1,NBTAB
*   Plot the frame.
            IF(GASOPT(15,1))THEN
                 CALL GRAOPT('LOG-X')
            ELSE
                 CALL GRAOPT('LIN-X')
            ENDIF
            IF(GASOPT(15,2))THEN
                 CALL GRAOPT('LOG-Y')
            ELSE
                 CALL GRAOPT('LIN-Y')
            ENDIF
            CALL GRCART(EMIN,YPLMIN,EMAX,YPLMAX,
     -           'E [V/cm]',
     -           'Ionisation and excitation rates [THz]',
     -           'Ionisation and excitation rates')
            IF(GASID.NE.' ')CALL GRCOMM(1,'Gas: '//GASID)
            IF(TAB2D)THEN
                 CALL OUTFMT(BTAB(K)/100,2,STR1,NC1,'LEFT')
                 CALL GRCOMM(2,'B = '//STR1(1:NC1)//' T')
                 CALL OUTFMT(180*BANG(1)/PI,2,STR1,NC1,'LEFT')
                 CALL OUTFMT(180*BANG(NBANG)/PI,2,STR2,NC2,'LEFT')
                 CALL OUTFMT(REAL(NBANG),2,STR3,NC3,'LEFT')
                 CALL GRCOMM(4,STR1(1:NC1)//' < angle(E,B) < '//
     -                STR2(1:NC2)//' degrees in '//STR3(1:NC3)//
     -                ' steps')
            ENDIF
*   Plot and mark the excitations.
            IF(GASOK(15))THEN
                 CALL GRATTS('FUNCTION-1','POLYLINE')
                 IF(TAB2D)THEN
                      DO 651 L=1,NEXGAS
                      DO 650 I=1,NBANG
                      DO 660 J=1,MXLIST
                      CALL GASEXR(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0,EXVECT)
                      YPL(J)=EXVECT(L)
660                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
650                   CONTINUE
651                   CONTINUE
                 ELSE
                      DO 652 L=1,NEXGAS
                      DO 670 I=1,MXLIST
                      CALL GASEXR(XPL(I),0.0,0.0,0.0,0.0,0.0,EXVECT)
                      YPL(I)=EXVECT(L)
670                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
652                   CONTINUE
                 ENDIF
                 CALL GRATTS('FUNCTION-1','POLYMARKER')
                 DO 690 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 680 I=1,NBANG
                      DO 681 L=1,NEXGAS
                      AUX2(1)=EXGAS2(J,I,K,L)*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
681                   CONTINUE
680                   CONTINUE
                 ELSE
                      DO 682 L=1,NEXGAS
                      AUX2(1)=EXGAS(J,L)*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
682                   CONTINUE
                 ENDIF
690              CONTINUE
            ENDIF
*   Plot and mark the ionisations.
            IF(GASOK(16))THEN
                 CALL GRATTS('FUNCTION-2','POLYLINE')
                 IF(TAB2D)THEN
                      DO 653 L=1,NIOGAS
                      DO 655 I=1,NBANG
                      DO 665 J=1,MXLIST
                      CALL GASIOR(XPL(J),0.0,0.0,
     -                     BTAB(K)*COS(BANG(I)),
     -                     BTAB(K)*SIN(BANG(I)),0.0,IOVECT)
                      YPL(J)=IOVECT(L)
665                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
655                   CONTINUE
653                   CONTINUE
                 ELSE
                      DO 654 L=1,NIOGAS
                      DO 675 I=1,MXLIST
                      CALL GASIOR(XPL(I),0.0,0.0,0.0,0.0,0.0,IOVECT)
                      YPL(I)=IOVECT(L)
675                   CONTINUE
                      CALL GRLINE(MXLIST,XPL,YPL)
654                   CONTINUE
                 ENDIF
                 CALL GRATTS('FUNCTION-2','POLYMARKER')
                 DO 695 J=1,NGAS
                 AUX1(1)=PGAS*EGAS(J)
                 IF(TAB2D)THEN
                      DO 685 I=1,NBANG
                      DO 686 L=1,NIOGAS
                      AUX2(1)=IOGAS2(J,I,K,L)*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
686                   CONTINUE
685                   CONTINUE
                 ELSE
                      DO 687 L=1,NIOGAS
                      AUX2(1)=IOGAS(J,L)*PGAS
                      CALL GRMARK(1,AUX1,AUX2)
687                   CONTINUE
                 ENDIF
695              CONTINUE
            ENDIF
            CALL GRNEXT
            CALL GRALOG('Townsend, attachment & dissociation')
*   Next B field.
640         CONTINUE
       ENDIF
*   Continue here if the plot was skipped.
699    CONTINUE
*** SRIM distributions
       IF(SRIMOK.AND.GASOPT(13,3))CALL SRMPLT
*** TRIM distributions
       IF(TRIMOK.AND.GASOPT(16,3))CALL TRMPLT
*** Restore the axes.
       CALL GRAOPT('LINEAR-X')
       CALL GRAOPT('LINEAR-Y')
*** Call TIMLOG to register the amount of CPU time used.
       CALL TIMLOG('Making various gas plots:               ')
       END
