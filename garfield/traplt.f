CDECK  ID>, TRAPLT.
       SUBROUTINE TRAPLT
*-----------------------------------------------------------------------
*   TRAPLT - Plots the track with the delta electrons.
*   (Last changed on 28/ 2/12.)
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
       REAL SRMDEN,ESRIM,SRMEM,SRMHD,SRMRNG,SRMDT,SRMDL,WSRIM,FSRIM,
     -      XSRIM,YSRIM,ZSRIM,ECSRIM,EKSRIM
       INTEGER NSRIM,NCSRIM,NESRIM
       COMMON /SRMDAT/
     -      ESRIM(MXLIST),SRMEM(MXLIST),
     -      SRMHD(MXLIST),SRMRNG(MXLIST),SRMDT(MXLIST),SRMDL(MXLIST),
     -      XSRIM(MXCLUS),YSRIM(MXCLUS),ZSRIM(MXCLUS),ECSRIM(MXCLUS),
     -      EKSRIM(MXCLUS),SRMDEN,WSRIM,FSRIM,
     -      NSRIM,NCSRIM,NESRIM(MXCLUS)
       REAL WTRIM, FTRIM, TRMLMN, TRMLMX, TRMDEN, TRMEMI,
     -      TRMTGD, TRMIOE, ECTRIM, EKTRIM, XTRIM, YTRIM, ZTRIM, NETRIM,
     -      TRMHDI, TRMY, TRMZ
       INTEGER NTRIM, NCTRIM, LTRIM, ITRIM
       COMMON /TRMDAT/
     -      NTRIM, NCTRIM, WTRIM, FTRIM, LTRIM, TRMLMN, TRMLMX, TRMDEN,
     -      TRMEMI(MXLIST), TRMHDI(MXLIST), TRMTGD(MXLIST),
     -      TRMIOE(MXLIST), TRMY(MXLIST), TRMZ(MXLIST),
     -      XTRIM(MXCLUS), YTRIM(MXCLUS), ZTRIM(MXCLUS), ECTRIM(MXCLUS),
     -      EKTRIM(MXCLUS), NETRIM(MXCLUS), ITRIM
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
c	descriptions of the geometry of the setup
*** Decreased number of volumes (RV 22/11/06).
	integer pqvol		! Max. quantity of volumes
*	parameter (pqvol=150)	
	parameter (pqvol=3)	
	integer pQSVol		! Max. quantity of sensitive volumes
*	parameter (pQSVol=130)
	parameter (pQSVol=2)
	integer pQIVol		! Max. quantity of ionization volumes
*	parameter (pQIVol=130)
	parameter (pQIVol=2)
	integer QSVol
	integer QIVol
	integer qvol		! quantity of volumes
	integer upVol		! user's volume parameter
	integer nMatVol		! Material number for volume
	integer sSensit		! Sign of sensitivity
	integer sIonizat	! Sign of ionization
	real*8 wall1,wall2,wide	! Left, right side and wide of volume
	integer numSensVol,numVolSens	! pass from Volume number
					! to Sensitive volume number
	integer numIoniVol,numVolIoni	! The same for ionization
	real RLenRVol, RLenRAVol	! Radiation lengt for each volumes
					! and for whole detector.
	integer xxxVol		! dummy, for efficient alignment
	common / cvolum /
     +		qvol,
     +		QSVol,QIVol, xxxVol,
     +		upVol(pqvol), nMatVol(pqvol), sSensit(pqvol),
     +		sIonizat(pqvol),
     +		wall1(pqvol),wall2(pqvol),wide(pqvol),
     +		numSensVol(pqvol),numVolSens(pQSVol),
     +		numIoniVol(pqvol),numVolIoni(pQIVol),
     +		RLenRVol(pqvol),RLenRAVol
	save / cvolum /
c      Main control variables


	integer soo		   ! Flag, allowing to print
				   !  to stream 'oo'
				   ! If it is 0, no print will be at all,
				   ! except the case of serious problems.
	integer oo		   ! The output stream logical number.
	integer qevt		   ! Quantity of events to produce.
	integer nevt		   ! Current number of the event.
	integer ninfo		   ! Quantity of the first events
				   ! to print debug info.
	integer ssimioni	   ! Flag to simulate ionization loss,
				   ! 0 - no ionization,
				   ! 1 - to simulate ionization.
				   !
				   !
				   !
	integer srandoff	   ! Flag to swich off the randomization
				   ! in function treatdel.
				   ! It is for debug and without guarantee.
	parameter (srandoff=0)	   ! Normal regim with randommization.

	integer pqup		   ! dimensions of arrays of auxiliary
				   ! parameters in abs.inc, rga.inc,
				   ! del.inc
	parameter (pqup=1)


	integer sret_err	! Sign to return the control from current
		! subroutine to which is called it if error is occured.
		! 1 - to return, 0 - to stop.
		! It is intended for handling with subroutine SHEED.
		! In the case of error it can return the control instead of
		! stop. But not for every possible errors return is done.
		! Some of the most original errors could lead to stop.
		! When working with HEED program, sret_err must be zero.
	integer s_err	! Sign of error.
		! 1 - error, 0 - no error

	character*9  TaskName	   ! Name of task, using for generating
				   ! file names.
	character*40 OutputFile	   ! Name of file with output listing.
				   ! Using only in IniHeed.
*** Common split in character and non-character parts (RV 26/10/2007).
	common / cGoEve /
     +	soo, oo,
     +	qevt,nevt,ninfo,
     +	ssimioni,
     +	sret_err, s_err
        common / cGoEveCHR /
     +	TaskName,
     +	OutputFile

	save / cGoEve /
	save / cGoEveCHR /
*** End of change

c		Delta electrons

	integer pqdel		! Max. q. of electrons
	parameter (pqdel=120000)	
	integer qdel 	! Q. of electrons
C	integer cdel	! Current electron (not used, RV 27/2/97)
			! number of el. which must be treated next
	real veldel	! direction of the velocity
	real*8 pntdel	! point
	
	real zdel, edel		! charge of current electrons
			! which must be produced and energy of Delta
	integer Stdel   ! Generation number
	integer Ptdel	! pointer to parent virtual photon
	integer updel	! additional parameters
	integer SOdel           ! 1 for ouger electrons 0 for other
	integer nVoldel		! Number of volume
	real*8 rangedel		! range
	real*8 rangepdel		! practical range
	integer qstepdel	! quantity of steps of simulation
				! of stopping
        integer sOverflowDel    ! sign of overflow in the current event
        integer qsOverflowDel   ! quantity of the overflows in all events
        integer qOverflowDel    ! quantity of the lossed electrons
                                ! in all events
	integer ii1del		! not used. only for alingment.
	common / comdel /
     +	qdel, ii1del,
     +	pntdel(3,pqdel), veldel(3,pqdel),
     +	rangedel(pqdel),rangepdel(pqdel), qstepdel(pqdel),
     +	zdel(pqdel), edel(pqdel), nVoldel(pqdel),
     +	Stdel(pqdel), Ptdel(pqdel), updel(pqup,pqdel), SOdel(pqdel),
     +  sOverflowDel, qsOverflowDel,qOverflowDel
        save / comdel /

c		Conductin electrons in sensitive volumes
c		Currently each the electron is considered as cluster

	integer pqcel		! Max. q of clusters
	parameter (pqcel=50000)
c	parameter (pqcel=1000000)	! If this, reduce numbers of volumes
c	parameter (pqcel=100000)	! If this, reduce numbers of volumes
	integer qcel 		! Q. of clusters
	real*8 pntcel	 	! point of cluster
	real zcel		! charge in unit of quantity of electron
				! in this cluster (now it is always 1)
	real szcel	! sum quantity of charge in the volume
	integer Ndelcel	! number of parent delta electron
	integer sOverflowCel	! sign of overflow in the current event
	integer qsOverflowCel	! quantity of the overflows in all events
	integer qOverflowCel	! quantity of the lossed electrons
				! in all events
	integer sactcel		! auxiliary sing.
			! It set to one if the delta-electron either
			! was born in an insensitive lawer or
			! after it had flied through an insensitive lawer.
	common / comcel /
     +	pntcel(3,pqcel,pQSVol),
     +	qcel(pQSVol),
     +	zcel(pqcel,pQSVol),
     +	szcel(pQSVol),
     +	Ndelcel(pqcel,pQSVol),
     +	sactcel(pqcel,pQSVol),
     +	sOverflowCel(pQSVol), qsOverflowCel(pQSVol),qOverflowCel(pQSVol)
	save / comcel /

c		Gamma which is ready to absorb
c		There are two sorts of gamma
c		Real gamma after their absorbtion points are known and
c		virtual gamma from ionization loss
	integer pqtagam		 ! Max quantity of absorbtion gamma
	parameter (pqtagam=100000)
	integer qtagam, ctagam	 ! Full quantity and current number
				 ! of gamma which will be treat next.
				 ! If ctagam>qtagam then
				 ! there is no gamma to treat.
	real etagam,  vtagam ! Energy,  and velocity
				 ! direction of absorbtion gamma
	real*8 rtagam 		 ! position of absorbtion gamma
	integer nVolagam	 ! Volume number for this point
	integer nAtagam,nshlagam ! Number of atom and shell
				 ! which  absorbe this photon
	integer Stagam		 ! Generation number
	integer upagam		 ! additional parameters
        integer sOverflowagam    ! sign of overflow in the current event
        integer qsOverflowagam   ! quantity of the overflows in all events
        integer qOverflowagam    ! quantity of the lossed electrons
                                ! in all events

	common / comabs /
     +	qtagam, ctagam, etagam(pqtagam),
     +	rtagam(3,pqtagam), vtagam(3,pqtagam),
     +	nVolagam(pqtagam),nAtagam(pqtagam),nShlagam(pqtagam),
     +	Stagam(pqtagam), upagam(pqup,pqtagam),
     +  sOverflowagam, qsOverflowagam,qOverflowagam
	save / comabs /
c		Real photons
*** Check INDPOS when changing pqrga.
	integer pqrga	
	parameter (pqrga=1000)
	integer qrga, crga
	real velrga, erga
*** Added origin of real gamma (RV 27/11/01)
	real*8 pntrga,  ! Current point of real gamma
     +         orirga   ! Location of gamma production
	integer Strga	! generation
	integer Ptrga 	! pointer to parent
	integer uprga	! number of trans vol
	integer SFrga 	! sign of fly out
	integer nVolrga
        integer sOverflowrga    ! sign of overflow in the current event
        integer qsOverflowrga   ! quantity of the overflows in all events
        integer qOverflowrga    ! quantity of the lossed photons
                                ! in all events

	common / comrga /
     +	qrga, crga,
     +	pntrga(3,pqrga), orirga(3,pqrga), velrga(3,pqrga), erga(pqrga),
     +	nVolrga(pqrga), Strga(pqrga), Ptrga(pqrga), uprga(pqup,pqrga),
     +	SFrga(pqrga),
     +  sOverflowrga, qsOverflowrga,qOverflowrga
	save / comrga /
c		Results of ionization loss calculations
c		It is used only for hist filling

	integer pqgvga
*** Increased buffer (RV 22/11/06) - check INDPOS dimensions when changing !
	parameter (pqgvga=10000)
*	parameter (pqgvga=1000)
	integer qgvga,ganumat,ganumshl
*** Added overflow tracing (RV 22/11/06)
        integer sOverflowvga    ! sign of overflow in the current event
        integer qsOverflowvga   ! quantity of the overflows in all events
        integer qOverflowvga    ! quantity of the lossed photons
                                ! in all events
*** End of addition.
	real esgvga,egvga,velgvga
	real*8 pntgvga
	common / clsgva /
     +	qgvga(pQIVol),
     +	esgvga(pQIVol),
     +	egvga(pqgvga,pQIVol),
     +	pntgvga(3,pqgvga,pQIVol),
     +	velgvga(3,pqgvga,pQIVol),
     +	ganumat(pqgvga,pQIVol),
     +	ganumshl(pqgvga,pQIVol),
     +  sOverflowvga, qsOverflowvga,qOverflowvga
	save / clsgva /
       REAL XCLS,YCLS,ZCLS
       DOUBLE PRECISION XPLDEL(pqcel),YPLDEL(pqcel),ZPLDEL(pqcel),
     -      XPLVGA(pqgvga),YPLVGA(pqgvga),ZPLVGA(pqgvga),
     -      XPL(2),YPL(2),ZPL(2),ETOT
       INTEGER NELEC,I,J,K,NPL
*** Apparently a HEED generated track.
       IF(HEEDOK.AND.ITRTYP.EQ.4)THEN
**  Pick up the virtual gamma's along the track.
            ETOT=0
*   Initialise with the track starting point.
            NPL=1
            XPLVGA(NPL)=XT0
            YPLVGA(NPL)=YT0
            ZPLVGA(NPL)=ZT0
*   Loop over all virtual gamma's.
            DO 20 I=1,qgvga(1)+qrga
            IF(INDPOS(I).GT.qgvga(1))GOTO 20
            XCLS=XT0+COS(TRPHI)*pntgvga(1,INDPOS(I),1)-
     -           SIN(TRPHI)*SIN(TRTH)*pntgvga(2,INDPOS(I),1)+
     -           SIN(TRPHI)*COS(TRTH)*pntgvga(3,INDPOS(I),1)
            YCLS=YT0+COS(TRTH)*pntgvga(2,INDPOS(I),1)+
     -           SIN(TRTH)*pntgvga(3,INDPOS(I),1)
            ZCLS=ZT0-SIN(TRPHI)*pntgvga(1,INDPOS(I),1)-
     -           COS(TRPHI)*SIN(TRTH)*pntgvga(2,INDPOS(I),1)+
     -           COS(TRPHI)*COS(TRTH)*pntgvga(3,INDPOS(I),1)
            ETOT=ETOT+egvga(INDPOS(I),1)
            IF(NPL+1.LE.pqgvga)THEN
                 NPL=NPL+1
                 XPLVGA(NPL)=XCLS
                 YPLVGA(NPL)=YCLS
                 ZPLVGA(NPL)=ZCLS
            ELSE
                 PRINT *,' !!!!!! TRAPLT WARNING : Plot buffer size'//
     -                ' exhausted; not plotting further photons.'
                 GOTO 25
            ENDIF
            IF(LTRCUT.AND.ETOT.GE.TRENER)GOTO 25
20          CONTINUE
*   Energy limit reached or all relevant virtual photons taken.
25          CONTINUE
*   Set the appropriate representations.
            CALL GRATTS('TRACK','POLYLINE')
            CALL GRATTS('TRACK','POLYMARKER')
*   Plot the particle trajectory.
            IF(POLAR)CALL CF2CTR(XPLVGA,YPLVGA,XPLVGA,YPLVGA,NPL)
            IF(NPL.GT.1)THEN
                 CALL PLAGPL(NPL,XPLVGA,YPLVGA,ZPLVGA)
            ELSEIF(NPL.EQ.1)THEN
                 CALL PLAGPM(NPL,XPLVGA,YPLVGA,ZPLVGA)
            ENDIF
**  Next plot each of the deltas and Auger electrons.
            ETOT=0
*   Loop over the virtual and real photons.
            DO 50 K=1,qgvga(1)+qrga
*   Loop over the associated delta's.
            DO 30 I=1,qdel
            IF(ptdel(I).NE.INDPOS(K).OR.edel(I).LE.0)GOTO 30
*   Set the attributes depending on the type.
            IF(sodel(I).EQ.0)THEN
                 CALL GRATTS('DELTA-ELECTRON','POLYLINE')
                 CALL GRATTS('DELTA-ELECTRON','POLYMARKER')
            ELSE
                 CALL GRATTS('AUGER-ELECTRON','POLYLINE')
                 CALL GRATTS('AUGER-ELECTRON','POLYMARKER')
            ENDIF
*   Store the starting point.
            IF(INDPOS(K).LE.qgvga(1))THEN
                 XCLS=XT0+COS(TRPHI)*pntgvga(1,INDPOS(K),1)-
     -                SIN(TRPHI)*SIN(TRTH)*pntgvga(2,INDPOS(K),1)+
     -                SIN(TRPHI)*COS(TRTH)*pntgvga(3,INDPOS(K),1)
                 YCLS=YT0+COS(TRTH)*pntgvga(2,INDPOS(K),1)+
     -                SIN(TRTH)*pntgvga(3,INDPOS(K),1)
                 ZCLS=ZT0-SIN(TRPHI)*pntgvga(1,INDPOS(K),1)-
     -                COS(TRPHI)*SIN(TRTH)*pntgvga(2,INDPOS(K),1)+
     -                COS(TRPHI)*COS(TRTH)*pntgvga(3,INDPOS(K),1)
            ELSE
                 XCLS=XT0+COS(TRPHI)*pntrga(1,INDPOS(K)-qgvga(1))-
     -                SIN(TRPHI)*SIN(TRTH)*pntrga(2,INDPOS(K)-qgvga(1))+
     -                SIN(TRPHI)*COS(TRTH)*pntrga(3,INDPOS(K)-qgvga(1))
                 YCLS=YT0+COS(TRTH)*pntrga(2,INDPOS(K)-qgvga(1))+
     -                SIN(TRTH)*pntrga(3,INDPOS(K)-qgvga(1))
                 ZCLS=ZT0-SIN(TRPHI)*pntrga(1,INDPOS(K)-qgvga(1))-
     -                COS(TRPHI)*SIN(TRTH)*pntrga(2,INDPOS(K)-qgvga(1))+
     -                COS(TRPHI)*COS(TRTH)*pntrga(3,INDPOS(K)-qgvga(1))
            ENDIF
            NELEC=1
            XPLDEL(NELEC)=XCLS
            YPLDEL(NELEC)=YCLS
            ZPLDEL(NELEC)=ZCLS
*   Find the associated electrons.
            DO 40 J=1,qcel(1)
            IF(ndelcel(J,1).EQ.I)THEN
                 NELEC=NELEC+1
                 XCLS=XT0+COS(TRPHI)*pntcel(1,J,1)-
     -                SIN(TRPHI)*SIN(TRTH)*pntcel(2,J,1)+
     -                SIN(TRPHI)*COS(TRTH)*pntcel(3,J,1)
                 YCLS=YT0+COS(TRTH)*pntcel(2,J,1)+
     -                SIN(TRTH)*pntcel(3,J,1)
                 ZCLS=ZT0-SIN(TRPHI)*pntcel(1,J,1)-
     -                COS(TRPHI)*SIN(TRTH)*pntcel(2,J,1)+
     -                COS(TRPHI)*COS(TRTH)*pntcel(3,J,1)
                 XPLDEL(NELEC)=XCLS
                 YPLDEL(NELEC)=YCLS
                 ZPLDEL(NELEC)=ZCLS
            ENDIF
40          CONTINUE
*   Keep track of total energy.
            IF(LTRCUT.AND.ETOT+edel(I).GT.TRENER)THEN
                 NELEC=NELEC*(TRENER-ETOT)/edel(I)
                 ETOT=TRENER+1
            ELSE
                 ETOT=ETOT+edel(I)
            ENDIF
*   Plot the particle trajectory.
            IF(POLAR)CALL CF2CTR(XPLDEL,YPLDEL,XPLDEL,YPLDEL,NELEC)
            IF(NELEC.GT.2)THEN
                 CALL PLAGPL(NELEC,XPLDEL,YPLDEL,ZPLDEL)
            ELSEIF(NELEC.GE.2)THEN
                 CALL PLAGPM(NELEC-1,XPLDEL(2),YPLDEL(2),ZPLDEL(2))
            ENDIF
*   Quit if energy limit reached.
            IF(LTRCUT.AND.ETOT.GE.TRENER)GOTO 60
*   Next delta.
30          CONTINUE
*   Next virtual gamma.
50          CONTINUE
*   Energy limit.
60          CONTINUE
**  Plot the real photons.
            CALL GRATTS('PHOTON','POLYLINE')
            CALL GRATTS('PHOTON','POLYMARKER')
*   Loop over the real photons.
            DO 130 I=1,qrga
            XCLS=XT0+COS(TRPHI)*orirga(1,I)-
     -           SIN(TRPHI)*SIN(TRTH)*orirga(2,I)+
     -           SIN(TRPHI)*COS(TRTH)*orirga(3,I)
            YCLS=YT0+COS(TRTH)*orirga(2,I)+
     -           SIN(TRTH)*orirga(3,I)
            ZCLS=ZT0-SIN(TRPHI)*orirga(1,I)-
     -           COS(TRPHI)*SIN(TRTH)*orirga(2,I)+
     -           COS(TRPHI)*COS(TRTH)*orirga(3,I)
            XPL(1)=XCLS
            YPL(1)=YCLS
            ZPL(1)=ZCLS
*   Find the matching absorption point.
            DO 150 J=1,qtagam
            IF(stagam(J).NE.strga(I))GOTO 150
            XCLS=XT0+COS(TRPHI)*rtagam(1,J)-
     -           SIN(TRPHI)*SIN(TRTH)*rtagam(2,J)+
     -           SIN(TRPHI)*COS(TRTH)*rtagam(3,J)
            YCLS=YT0+COS(TRTH)*rtagam(2,J)+
     -           SIN(TRTH)*rtagam(3,J)
            ZCLS=ZT0-SIN(TRPHI)*rtagam(1,J)-
     -           COS(TRPHI)*SIN(TRTH)*rtagam(2,J)+
     -           COS(TRPHI)*COS(TRTH)*rtagam(3,J)
            XPL(2)=XCLS
            YPL(2)=YCLS
            ZPL(2)=ZCLS
*   Plot the connecting line.
            IF(POLAR)CALL CF2CTR(XPL,YPL,XPL,YPL,2)
            CALL PLAGPL(2,XPL,YPL,ZPL)
*   Next absorbing gamma.
150         CONTINUE
*   Next real photon.
130         CONTINUE
*** SRIM clustering
       ELSEIF(ITRTYP.EQ.9.AND.SRIMOK)THEN
*   Set attributes
            CALL GRATTS('TRACK','POLYLINE')
            CALL GRATTS('DELTA-ELECTRON','POLYMARKER')
*   Loop over the segments
            DO 100 I=1,NCSRIM-1
            XPL(1)=XSRIM(I)
            YPL(1)=YSRIM(I)
            ZPL(1)=ZSRIM(I)
            XPL(2)=XSRIM(I+1)
            YPL(2)=YSRIM(I+1)
            ZPL(2)=ZSRIM(I+1)
            IF(POLAR)CALL CF2CTR(XPL,YPL,XPL,YPL,2)
            CALL PLAGPL(2,XPL,YPL,ZPL)
            IF(I.EQ.NCSRIM-1)THEN
                 CALL PLAGPM(2,XPL,YPL,ZPL)
            ELSE
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
100         CONTINUE
*** TRIM clustering.
       ELSEIF(ITRTYP.EQ.10.AND.TRIMOK)THEN
*   Set attributes
            CALL GRATTS('TRACK','POLYLINE')
            CALL GRATTS('DELTA-ELECTRON','POLYMARKER')
*   Loop over the segments
            DO 200 I=1,NCTRIM-1
            XPL(1)=XTRIM(I)
            YPL(1)=YTRIM(I)
            ZPL(1)=ZTRIM(I)
            XPL(2)=XTRIM(I+1)
            YPL(2)=YTRIM(I+1)
            ZPL(2)=ZTRIM(I+1)
            IF(POLAR)CALL CF2CTR(XPL,YPL,XPL,YPL,2)
            CALL PLAGPL(2,XPL,YPL,ZPL)
            IF(I.EQ.NCTRIM-1)THEN
                 CALL PLAGPM(2,XPL,YPL,ZPL)
            ELSE
                 CALL PLAGPM(1,XPL,YPL,ZPL)
            ENDIF
200         CONTINUE
*** Any other kind of track.
       ELSE
*   Set the appropriate representations.
            CALL GRATTS('TRACK','POLYLINE')
            CALL GRATTS('TRACK','POLYMARKER')
*   And plot the track as a straight line.
            XPL(1)=XT0
            YPL(1)=YT0
            ZPL(1)=ZT0
            XPL(2)=XT1
            YPL(2)=YT1
            ZPL(2)=ZT1
            IF(POLAR)CALL CF2CTR(XPL,YPL,XPL,YPL,2)
            CALL PLAGPL(2,XPL,YPL,ZPL)
       ENDIF
       END
