CDECK  ID>, MAPFMF.
       SUBROUTINE MAPFMF(IFAIL)
*-----------------------------------------------------------------------
*   MAPFMF - Retrieves the field map data in binary format.
*   (Last changed on  8/11/07.)
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
       INTEGER I,J,K,IOS,NWORD,NC,IFAIL,IFAIL1,IVERS,INPTYP,INPCMP,
     -      INEXT,ISEL
       REAL EPSSEL,AUX
       CHARACTER*(MXNAME) FILE
       LOGICAL NEWDRM,OK,LHISMP
       EXTERNAL INPTYP,INPCMP
*** Assume for the time being that this will fail.
       IFAIL=1
*** Get hold of the file name.
       CALL INPNUM(NWORD)
*   Make sure there is at least one argument.
       IF(NWORD.LT.2)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : FETCH-FIELD-MAP needs'//
     -           ' a file name; no map will not be read.'
            RETURN
       ENDIF
*   Store the file name
       CALL INPSTR(2,2,FILE,NC)
*   Check the length.
       IF(NC.GT.MXNAME)PRINT *,' !!!!!! MAPFMF WARNING : The file',
     -      ' name is truncated to MXNAME (=',MXNAME,') characters.'
       NC=MIN(NC,MXNAME)
*** Open the file for sequential binary read.
       CALL DSNOPN(FILE,NC,12,'READ-BINARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Opening '//FILE(1:NC)//
     -           ' failed ; the field map will not be read.'
            CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ENDIF
*** Reset the field map.
       CALL MAPINT
*** Read version number.
       READ(12,ERR=2010,IOSTAT=IOS) IVERS
       IF(IVERS.NE.8)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Format of '//FILE(1:NC)//
     -           ' is not compatible with program version; not read.'
            PRINT *,'                         File version: ',IVERS,
     -           ', program version: 8.'
            CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ENDIF
*** Read # triangles, map order, availability, 3D, plot flag.
       READ(12,ERR=2010,IOSTAT=IOS) NMAP,MAPORD,
     -      (MAPFLG(I),I=1,10+4*MXWMAP),MAPTYP,LMAPPL,NWMAP
*   Verify that the dimensions match.
       IF(NMAP.GT.MXMAP)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : The map in '//FILE(1:NC)//
     -           ' exceeds dimensions of this compilation; not read.'
            CALL MAPINT
            CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ELSEIF(NWMAP.GT.MXWMAP)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Too many weighting'//
     -           ' fields in '//FILE(1:NC)//' for this compilation;'//
     -           ' not read.'
            CALL MAPINT
            CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
*   Make sure there is a field map.
       ELSEIF(NMAP.LE.0.OR..NOT.MAPFLG(1))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : The map in '//FILE(1:NC)//
     -           ' is empty; file not read.'
            CALL MAPINT
            CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ENDIF
*** Read the triangles, tetrahedrons etc., dimensions and periodicities.
       IF(MAPFLG(1))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((XMAP(I,J),I=1,NMAP),J=1,10),
     -      ((YMAP(I,J),I=1,NMAP),J=1,10),
     -      ((ZMAP(I,J),I=1,NMAP),J=1,10),
     -      (ELMDGN(I),I=1,NMAP),
     -      XMMIN,XMMAX,YMMIN,YMMAX,ZMMIN,ZMMAX,
     -      XAMIN,XAMAX,YAMIN,YAMAX,ZAMIN,ZAMAX,
     -      XFMOFF,YFMOFF,ZFMOFF,
     -      SETAX,SETAY,SETAZ,SX,SY,SZ,PERX,PERY,PERZ,LSFDER,
     -      PERMX,PERMY,PERMZ,PERAX,PERAY,PERAZ,PERRX,PERRY,PERRZ
*   The (Ex,Ey,Ez) field, if available.
       IF(MAPFLG(2))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((EXMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(3))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((EYMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(4))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((EZMAP(I,J),I=1,NMAP),J=1,10)
*   The potential and potential range, if available.
       IF(MAPFLG(5))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((VMAP(I,J),I=1,NMAP),J=1,10),VMMIN,VMMAX
*   The (Bx,By,Bz) field, if available.
       IF(MAPFLG(6))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((BXMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(7))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((BYMAP(I,J),I=1,NMAP),J=1,10)
       IF(MAPFLG(8))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((BZMAP(I,J),I=1,NMAP),J=1,10)
*   The material map, if available.
       IF(MAPFLG(9))READ(12,ERR=2010,IOSTAT=IOS)
     -      (MATMAP(I),I=1,NMAP)
*   The weighting (Ex,Ey,Ez) field and label, if available.
       DO 10 K=1,NWMAP
       IF(MAPFLG(10+4*K-3))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((EWXMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(11+4*K-3))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((EWYMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(12+4*K-3))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((EWZMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(13+4*K-3))READ(12,ERR=2010,IOSTAT=IOS)
     -      ((VWMAP(I,J,K),I=1,NMAP),J=1,10)
       IF(MAPFLG(10+4*K-3).OR.MAPFLG(11+4*K-3).OR.
     -      MAPFLG(12+4*K-3).OR.MAPFLG(13+4*K-3))
     -      READ(12,ERR=2010,IOSTAT=IOS) EWSTYP(K)
10     CONTINUE
*** Read the number of materials and the drift medium.
       READ(12,ERR=2010,IOSTAT=IOS) NEPS,IDRMAT
*   Verify that the dimensions match.
       IF(NEPS.GT.MXEPS)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : The map in '//FILE(1:NC)//
     -           ' exceeds dimensions of this compilation; not read.'
            CALL MAPINT
            CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
            RETURN
       ENDIF
*** Read the material table.
       READ(12,ERR=2010,IOSTAT=IOS) (EPSMAT(I),I=1,NEPS),
     -      (EPSSUR(I),I=1,NEPS)
*** Close the file.
       CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
*** Read the command line options.
       OK=.TRUE.
       NEWDRM=.FALSE.
       EPSSEL=-1
       ISEL=0
       LHISMP=.FALSE.
       INEXT=3
       DO 20 I=3,NWORD
       IF(I.LT.INEXT)GOTO 20
**  Select a drift medium.
       IF(INPCMP(I,'DR#IFT-#MEDIUM').NE.0)THEN
            IF(I+1.GT.NWORD)THEN
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
            MAPORD=1
       ELSEIF(INPCMP(I,'QUA#DRATIC-#INTERPOLATION').NE.0)THEN
            IF(MAPORD.GE.2)THEN
                 MAPORD=2
            ELSE
                 CALL INPMSG(I,'Order can not be increased.')
            ENDIF
       ELSEIF(INPCMP(I,'CUB#IC-#INTERPOLATION').NE.0)THEN
            IF(MAPORD.GE.3)THEN
                 MAPORD=3
            ELSE
                 CALL INPMSG(I,'Order can not be increased.')
            ENDIF
**  Computation of Ex, Ey and Ez
       ELSEIF(INPCMP(I,'COMP#UTE-E#LECTRIC-#FIELD').NE.0)THEN
            LSFDER=.TRUE.
       ELSEIF(INPCMP(I,'INT#ERPOLATE-E#LECTRIC-#FIELD').NE.0)THEN
            LSFDER=.FALSE.
*   Unknown options.
       ELSE
            CALL INPMSG(I,'Not a known option')
            OK=.FALSE.
       ENDIF
20     CONTINUE
*** Print the error messages.
       CALL INPERR
*** Figure out which material is drift medium.
       IF(NEWDRM.AND..NOT.MAPFLG(9))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Cannot set a drift'//
     -           ' medium since there are no material properties.'
            OK=.FALSE.
            IDRMAT=-1
       ELSEIF(NEWDRM)THEN
            IF(NEPS.LT.1)THEN
                 PRINT *,' !!!!!! MAPFMF WARNING : No dielectric'//
     -                ' media found; cannot select drift medium.'
                 OK=.FALSE.
                 IDRMAT=-1
            ELSEIF(ISEL.LT.0)THEN
                 IDRMAT=NEPS+ISEL+1
            ELSEIF(ISEL.EQ.0.AND.EPSSEL.LT.0)THEN
                 PRINT *,' ------ MAPFMF MESSAGE : No drift medium'//
     -                ' has been selected ; choosing'
                 PRINT *,'                         the one with'//
     -                ' the lowest positive dielectric constant.'
                 IDRMAT=1
            ELSEIF(ISEL.EQ.0)THEN
                 IDRMAT=1
                 DO 130 I=1,NEPS
                 IF(ABS(EPSSEL-EPSMAT(I)).LT.
     -                ABS(EPSSEL-EPSMAT(IDRMAT)))IDRMAT=I
130              CONTINUE
                 PRINT *,' ------ MAPFMF MESSAGE : Dielectric'//
     -                ' constant nearest to ',EPSSEL,' is ',
     -                EPSMAT(IDRMAT)
            ELSE
                 IDRMAT=ISEL
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMF DEBUG   :'',
     -           '' Drift medium index='',I3)') IDRMAT
       ENDIF
*** Verify that there is no x or y axial symmetry in 2D.
       IF((PERAX.OR.PERAY).AND.MAPTYP.LT.10)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Axial symmetry has been'//
     -           ' requested around x or y for a 2D map; reset.'
            PERAX=.FALSE.
            PERAY=.FALSE.
            OK=.FALSE.
       ENDIF
*** For rotational symmetries, ensure that the fields are present.
       IF((PERRX.AND.(PERRY.OR.PERRZ)).OR.
     -      (PERRY.AND.(PERRX.OR.PERRZ)).OR.
     -      (PERRZ.AND.(PERRX.OR.PERRY)))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : More than one'//
     -           ' rotational symmetry; reset.'
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ELSEIF((PERRX.OR.PERRY.OR.PERRZ).AND.MAPTYP.GE.11)THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Rotational symmetry'//
     -           ' declared for a 3D field map; reset.'
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ELSEIF((PERRX.AND.(MAPFLG(3).OR.MAPFLG(7))).OR.
     -      (PERRY.AND.(MAPFLG(4).OR.MAPFLG(8))).OR.
     -      (PERRZ.AND.(MAPFLG(3).OR.MAPFLG(7))))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Rotational symmetry'//
     -           ' for the axis perpendicular to the map; reset.'
            PERRX=.FALSE.
            PERRY=.FALSE.
            PERRZ=.FALSE.
            OK=.FALSE.
       ENDIF
*** Verify the ranges for axial symmetry have been set.
       IF(PERAX.AND.((.NOT.SETAX).OR.
     -      ABS(MOD(XAMAX-XAMIN,2*PI)).LT.0.01))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Axial symmetry around x'//
     -           ' requested but range could not be set; reset.'
            PERAX=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(PERAY.AND.((.NOT.SETAY).OR.
     -      ABS(MOD(YAMAX-YAMIN,2*PI)).LT.0.01))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Axial symmetry around y'//
     -           ' requested but range could not be set; reset.'
            PERAY=.FALSE.
            OK=.FALSE.
       ENDIF
       IF(PERAZ.AND.((.NOT.SETAZ).OR.
     -      ABS(MOD(ZAMAX-ZAMIN,2*PI)).LT.0.01))THEN
            PRINT *,' !!!!!! MAPFMF WARNING : Axial symmetry around z'//
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
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMF DEBUG   :'',
     -           '' x-Angular coverage: '',2F10.3/26X,
     -           ''Periods: '',F10.3)')
     -           180*XAMIN/PI,180*XAMAX/PI,ABS(2*PI/(XAMAX-XAMIN))
            IF(ABS(2*PI/(XAMAX-XAMIN)-ANINT(2*PI/(XAMAX-XAMIN))).GT.
     -           0.001.OR.ANINT(2*PI/(XAMAX-XAMIN)).LT.2)THEN
                 PRINT *,' !!!!!! MAPFMF WARNING : The map doesn''t'//
     -                ' cover an integral fraction of 2 pi around x;'//
     -                ' axial periodicity reset.'
                 PERAX=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
       IF(PERAY)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMF DEBUG   :'',
     -           '' y-Angular coverage: '',2F10.3/26X,
     -           ''Periods: '',F10.3)')
     -           180*YAMIN/PI,180*YAMAX/PI,ABS(2*PI/(YAMAX-YAMIN))
            IF(ABS(2*PI/(YAMAX-YAMIN)-ANINT(2*PI/(YAMAX-YAMIN))).GT.
     -           0.001.OR.ANINT(2*PI/(YAMAX-YAMIN)).LT.2)THEN
                 PRINT *,' !!!!!! MAPFMF WARNING : The map doesn''t'//
     -                ' cover an integral fraction of 2 pi around y;'//
     -                ' axial periodicity reset.'
                 PERAY=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
       IF(PERAZ)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MAPFMF DEBUG   :'',
     -           '' z-Angular coverage: '',2F10.3/26X,
     -           ''Periods: '',F10.3)')
     -           180*ZAMIN/PI,180*ZAMAX/PI,ABS(2*PI/(ZAMAX-ZAMIN))
            IF(ABS(2*PI/(ZAMAX-ZAMIN)-ANINT(2*PI/(ZAMAX-ZAMIN))).GT.
     -           0.001.OR.ANINT(2*PI/(ZAMAX-ZAMIN)).LT.2)THEN
                 PRINT *,' !!!!!! MAPFMF WARNING : The map doesn''t'//
     -                ' cover an integral fraction of 2 pi around z;'//
     -                ' axial periodicity reset.'
                 PERAZ=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Set magnetic field flag.
       IF(MAPFLG(6).OR.MAPFLG(7).OR.MAPFLG(8))THEN
            MAGOK=.TRUE.
            IF(MAGSRC.EQ.1)PRINT *,' ------ MAPFMF MESSAGE : B field'//
     -           ' from &MAGNETIC replaced by a field map.'
            MAGSRC=2
            IF(GASSET)PRINT *,' ------ MAPFMF MESSAGE : Previous gas'//
     -           ' data deleted.'
            GASSET=.FALSE.
       ELSEIF(MAGSRC.EQ.2)THEN
            PRINT *,' ------ MAGFMF MESSAGE : The new field map has'//
     -           ' no magnetic field; currently no magnetic field.'
            MAGSRC=0
            MAGOK=.FALSE.
            IF(GASSET)PRINT *,' ------ MAPFMF MESSAGE : Previous gas'//
     -           ' data deleted.'
            GASSET=.FALSE.
       ENDIF
*** Check the map if requested.
       IF(LHISMP)THEN
            CALL MAPCHK(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! MAPFMF WARNING : Histogramming'//
     -                ' found map errors ; map rejected.'
                 OK=.FALSE.
            ENDIF
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
*** Register file access.
       CALL DSNLOG(FILE(1:NC),'Field map ','Sequential',
     -      'Bin Read  ')
*** Seems to have worked.
       IFAIL=0
       RETURN
*** Handle I/O errors.
2010   CONTINUE
       PRINT *,' !!!!!! MAPFMF WARNING : Error during binary read'//
     -      ' to file '//FILE(1:NC)//'; resetting field map.'
       CALL INPIOS(IOS)
       CALL MAPINT
       CLOSE(UNIT=12,STATUS='KEEP',ERR=2030,IOSTAT=IOS)
       RETURN
2030   CONTINUE
       PRINT *,' !!!!!! MAPFMF WARNING : Error closing '//FILE(1:NC)//
     -      ' after binary read; effect not known.'
       CALL INPIOS(IOS)
       END
