CDECK  ID>, TRACLS.
       SUBROUTINE TRACLS(XCLS,YCLS,ZCLS,ECLS,NPAIR,EXTRA1,DONE,IFAIL)
*-----------------------------------------------------------------------
*   TRACLS - Generates new clusters along the track.
*   TRACLI - Initialisation.
*   (Last changed on  5/ 2/13.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
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
       REAL XCLS,YCLS,ZCLS,ECLS,TRALEN,DIST,RNDEXP,RNDM,XAUX,YAUX,ZAUX,
     -      DISVGA(pqgvga+pqrga),EDELTA,ETOT,XP,YP,ZP,Q,FLXSUM,
     -      FLXCOO(MXLIST),FLXTAB(MXLIST),DIVDIF,XL,XL0FLX,XL1FLX,
     -      EXTRA1
       DOUBLE PRECISION XRAN
       INTEGER NPAIR,NTOT,NDELTA,IVGA,ICEL,I,J,IERROR,IFAIL,IPRINT,
     -      NCAUX,NV,ISIGN,JPRINT,IFAIL1
       LOGICAL DONE,OK
       CHARACTER*20 AUX
       EXTERNAL RNDEXP,RNDM
       SAVE NTOT,TRALEN,DIST,OK,IVGA,ICEL,ETOT,FLXCOO,FLXTAB,FLXSUM,
     -      XL0FLX,XL1FLX
       DATA OK/.FALSE./
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE TRACLS ///'
*** Initial settings.
       XCLS=0.0
       YCLS=0.0
       ZCLS=0.0
       ECLS=0.0
       NPAIR=0
       EXTRA1=0.0
       DONE=.TRUE.
       IFAIL=1
*** Make sure the routine is in the proper state.
       IF(.NOT.OK)THEN
            PRINT *,' !!!!!! TRACLS WARNING : Track initialisation'//
     -           ' not done or track complete; no clusters.'
            RETURN
*** Verify that track parameters are available.
       ELSEIF(.NOT.TRFLAG(1))THEN
            PRINT *,' !!!!!! TRACLS WARNING : Track location is not'//
     -           ' set; no clusters.'
            RETURN
       ENDIF
*** Handle the case of a fixed number of clusters.
       IF(ITRTYP.EQ.1)THEN
*   Ensure that the number is reasonable.
            IF(.NOT.TRFLAG(3))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Number of points'//
     -                ' on the track not defined; no clusters.'
                 RETURN
            ENDIF
*   Increment cluster counter.
            NTOT=NTOT+1
*   Compute new cluster position.
            IF(NTRLIN.GT.1)THEN
                 XCLS=XT0+REAL(NTOT-1)*(XT1-XT0)/REAL(NTRLIN-1)
                 YCLS=YT0+REAL(NTOT-1)*(YT1-YT0)/REAL(NTRLIN-1)
                 ZCLS=ZT0+REAL(NTOT-1)*(ZT1-ZT0)/REAL(NTRLIN-1)
            ELSE
                 XCLS=0.5*(XT0+XT1)
                 YCLS=0.5*(YT0+YT1)
                 ZCLS=0.5*(ZT0+ZT1)
            ENDIF
*   Set cluster size and energy.
            NPAIR=1
            ECLS=-1
*   See whether we were already done.
            IF(NTOT.GT.NTRLIN)THEN
                 DONE=.TRUE.
                 OK=.FALSE.
            ELSE
                 DONE=.FALSE.
            ENDIF
*** Fixed number of clusters at weighted positions.
       ELSEIF(ITRTYP.EQ.5)THEN
*   Ensure that the number is reasonable.
            IF(.NOT.TRFLAG(4))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Weighting function'//
     -                ' on the track not defined; no clusters.'
                 RETURN
            ELSEIF(.NOT.TRFLAG(5))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Number of points'//
     -                ' on the track not defined; no clusters.'
                 RETURN
            ENDIF
*   Increment cluster counter.
            NTOT=NTOT+1
*   Compute new cluster position.
            CALL HISRAD(WGT,MXLIST,0.0D0,1.0D0/MXLIST,XRAN)
            XCLS=XT0+REAL(XRAN)*(XT1-XT0)
            YCLS=YT0+REAL(XRAN)*(YT1-YT0)
            ZCLS=ZT0+REAL(XRAN)*(ZT1-ZT0)
*   Set cluster size and energy.
            NPAIR=1
            ECLS=-1
*   See whether we were already done.
            IF(NTOT.GT.NTRSAM)THEN
                 DONE=.TRUE.
                 OK=.FALSE.
            ELSE
                 DONE=.FALSE.
            ENDIF
*** One cluster at a random location.
       ELSEIF(ITRTYP.EQ.6)THEN
*   Increment cluster counter.
            NTOT=NTOT+1
*   Compute new cluster position.
            XRAN=DBLE(RNDM(NTOT))
            XCLS=XT0+REAL(XRAN)*(XT1-XT0)
            YCLS=YT0+REAL(XRAN)*(YT1-YT0)
            ZCLS=ZT0+REAL(XRAN)*(ZT1-ZT0)
*   Set the cluster size and energy.
            IF(GASOK(5))THEN
                 CALL HISRAD(CLSDIS,NCLS,0.0D0,1.0D0,XRAN)
                 NPAIR=INT(XRAN)
                 ECLS=NPAIR*EPAIR/1E6
            ELSE
                 NPAIR=1
                 ECLS=0
            ENDIF
*   See whether we were already done.
            IF(NTOT.GT.1)THEN
                 DONE=.TRUE.
                 OK=.FALSE.
            ELSE
                 DONE=.FALSE.
            ENDIF
*** Handle the case of equally spaced clusters according to CMEAN.
       ELSEIF(ITRTYP.EQ.2)THEN
*   Ensure that the appropriate gas data is present.
            IF(.NOT.GASOK(5))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Clustering data'//
     -                ' from gas section missing; track not set.'
                 RETURN
            ENDIF
*   Store track length.
            IF(NTOT.EQ.0)
     -           TRALEN=SQRT((XT1-XT0)**2+(YT1-YT0)**2+(ZT1-ZT0)**2)
*   Increment cluster counter.
            NTOT=NTOT+1
*   Generate new cluster position.
            IF(TRALEN.GT.0)THEN
                 XCLS=XT0+(REAL(NTOT-1)/CMEAN)*(XT1-XT0)/TRALEN
                 YCLS=YT0+(REAL(NTOT-1)/CMEAN)*(YT1-YT0)/TRALEN
                 ZCLS=ZT0+(REAL(NTOT-1)/CMEAN)*(ZT1-ZT0)/TRALEN
            ELSE
                 XCLS=0.5*(XT0+XT1)
                 YCLS=0.5*(YT0+YT1)
                 ZCLS=0.5*(ZT0+ZT1)
            ENDIF
*   See whether we're ready.
            IF((XT0-XCLS)*(XCLS-XT1).LT.0.OR.
     -          (YT0-YCLS)*(YCLS-YT1).LT.0.OR.
     -          (ZT0-ZCLS)*(ZCLS-ZT1).LT.0.OR.
     -          (TRALEN.LE.0.AND.NTOT.GT.1))THEN
                DONE=.TRUE.
                OK=.FALSE.
            ELSE
                DONE=.FALSE.
            ENDIF
*   Set the cluster size and energy.
            CALL HISRAD(CLSDIS,NCLS,0.0D0,1.0D0,XRAN)
            NPAIR=INT(XRAN)
            ECLS=NPAIR*EPAIR/1E6
*** Handle the case of exponentially spaced clusters.
       ELSEIF(ITRTYP.EQ.3)THEN
*   Ensure that the appropriate gas data is present.
            IF(.NOT.GASOK(5))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Clustering data'//
     -                ' from gas section missing; track not set.'
                 RETURN
            ENDIF
*   Store track length.
            IF(NTOT.EQ.0)THEN
                 TRALEN=SQRT((XT1-XT0)**2+(YT1-YT0)**2+(ZT1-ZT0)**2)
                 DIST=0
            ENDIF
*   Increment cluster counter.
            NTOT=NTOT+1
*   Generate new cluster position.
            IF(TRALEN.GT.0)THEN
                 DIST=DIST+RNDEXP(1.0/CMEAN)
                 XCLS=XT0+DIST*(XT1-XT0)/TRALEN
                 YCLS=YT0+DIST*(YT1-YT0)/TRALEN
                 ZCLS=ZT0+DIST*(ZT1-ZT0)/TRALEN
            ELSE
                 XCLS=0.5*(XT0+XT1)
                 YCLS=0.5*(YT0+YT1)
                 ZCLS=0.5*(ZT0+ZT1)
            ENDIF
*   See whether we're ready.
            IF((XT0-XCLS)*(XCLS-XT1).LT.0.OR.
     -          (YT0-YCLS)*(YCLS-YT1).LT.0.OR.
     -          (ZT0-ZCLS)*(ZCLS-ZT1).LT.0.OR.
     -          (TRALEN.LE.0.AND.NTOT.GT.1))THEN
                DONE=.TRUE.
                OK=.FALSE.
            ELSE
                DONE=.FALSE.
            ENDIF
*   Set the cluster size and energy.
            CALL HISRAD(CLSDIS,NCLS,0.0D0,1.0D0,XRAN)
            NPAIR=INT(XRAN)
            ECLS=EPAIR*NPAIR/1E6
*** Deal with  HEED generated clusters.
       ELSEIF(ITRTYP.EQ.4)THEN
**  Check for zero charge tracks.
            IF(TRCHAR.EQ.0.AND.TRMASS.GT.0)THEN
                 DONE=.TRUE.
                 XCLS=0
                 YCLS=0
                 ZCLS=0
                 ECLS=0
                 NPAIR=0
                 OK=.FALSE.
                 IFAIL=0
                 RETURN
            ENDIF
**  If this is a request for the first cluster ...
            IF(IVGA.EQ.0)THEN
*   Ensure that proper data is available.
                 IF(.NOT.HEEDOK)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : HEED gas'//
     -                     ' mix not defined; track not set.'
                      RETURN
                 ELSEIF(.NOT.TRFLAG(2))THEN
                      PRINT *,' !!!!!! TRACLS WARNING : Particle'//
     -                     ' properties not present; no clusters.'
                      RETURN
                 ENDIF
*   Store track length and rotation angles.
                 IF((XT1-XT0)**2+(ZT1-ZT0)**2.LE.0)THEN
                      IF(YT1-YT0.LT.0)THEN
                           TRTH=-PI/2
                      ELSEIF(YT1-YT0.GT.0)THEN
                           TRTH=+PI/2
                      ELSE
                           TRTH=0
                      ENDIF
                      TRPHI=0
                 ELSE
                      TRPHI=ATAN2(XT1-XT0,ZT1-ZT0)
                      TRTH=ATAN2(YT1-YT0,SQRT((XT1-XT0)**2+
     -                     (ZT1-ZT0)**2))
                 ENDIF
                 TRALEN=SQRT((XT1-XT0)**2+(YT1-YT0)**2+(ZT1-ZT0)**2)
                 IF(TRALEN.LE.0)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : Track length'//
     -                     ' 0 not compatible with HEED; no clusters.'
                      RETURN
                 ENDIF
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                     '' Transformation matrix:'',3(/26X,3F10.3)/
     -                     26X,''Track length: '',E15.8,'' cm.'')')
     -                     COS(TRPHI),-SIN(TRPHI)*SIN(TRTH),
     -                     +SIN(TRPHI)*COS(TRTH),0.0,COS(TRTH),
     -                     +SIN(TRTH),-SIN(TRPHI),-COS(TRPHI)*SIN(TRTH),
     -                     +COS(TRPHI)*COS(TRTH),TRALEN
                 ENDIF
*   Set the HEED error flag to false.
                 IF(LDEBUG)THEN
                      soo=1
                 ELSE
                      soo=0
                 ENDIF
                 oo=LUNOUT
                 s_err=0
*   Set the tracking volume.
                 CALL IniFVolume(0,1,1,1,0.0,TRALEN)
**  Generate a track for electrons and positrons.
                 IF(ABS(TRMASS-0.51099907).LT.0.01.AND.
     -                TRENER.LE.TRELEC)THEN
                      CALL TRADEL(1,1)
**  Generate a track for a charged particle other than e- and e+.
                 ELSEIF(ABS(TRCHAR).GT.0.1)THEN
                      IF(LDEBUG)THEN
                           IPRINT=2
                      ELSE
                           IPRINT=1
                      ENDIF
                      IERROR=0
                      CALL ipheed(
     -                     TRENER,       ! Particle kinetic energy [MeV]
     -                     TRMASS,       ! Particle mass [MeV]
     -                     IPRINT,       ! 1/2 Short/Medium listing
     -                     IERROR)       ! Error indicator.
                      IF(IERROR.NE.0)THEN
                           PRINT *,' !!!!!! TRACLS WARNING : Setting'//
     -                          ' particle properties in HEED failed.'
                           RETURN
                      ENDIF
*   Set the track.
                      CALL IniRTrack(
     -                     0.0,0.0,      ! Heed starting y-interval [cm]
     -                     0.0,0.0)      ! Track orientation
*   Optionally add multiple scattering.
                      IF(LTRMS)CALL IniMTrack(
     -                     1,            ! Sign of Rutherford angle
     -                     0.01*GASDEN,  ! Step
     -                     0.001)        ! Minimum angle
*   Generate a track.
                      CALL GoEventn(1,1)
**  Take care of photons.
                 ELSE
                      CALL TRAPHO(1,1)
                 ENDIF
*   Check for overflow.
                 IF(qsOverflowagam.GT.0)
     -                PRINT *,' !!!!!! TRACLS WARNING : Overflow of'//
     -                ' energy deposition buffer in HEED; no clusters.'
                 IF(qsOverflowrga.GT.0)
     -                PRINT *,' !!!!!! TRACLS WARNING : Overflow of'//
     -                ' real photon buffer in HEED; no clusters.'
                 IF(qsOverflowvga.GT.0)
     -                PRINT *,' !!!!!! TRACLS WARNING : Overflow of'//
     -                ' virtual photon buffer in HEED; no clusters.'
                 IF(qsOverflowDel.GT.0)
     -                PRINT *,' !!!!!! TRACLS WARNING : Overflow of'//
     -                ' delta electron buffer in HEED; no clusters.'
                 IF(qsOverflowCel(1).GT.0)
     -                PRINT *,' !!!!!! TRACLS WARNING : Overflow of'//
     -                ' deposited electron buffer in HEED; no clusters.'
                 IF(qsOverflowagam.GT.0.OR.qsOverflowrga.GT.0.OR.
     -                qsOverflowDel.GT.0.OR.qsOverflowvga.GT.0.OR.
     -                qsOverflowCel(1).GT.0)THEN
                      OK=.FALSE.
                      DONE=.TRUE.
                      RETURN
                 ENDIF
*   Sort the virtual gamma's by location.
C                 CALL SORTZV(DISVGA,INDPOS,qgvga(1)+qrga,1,0,0)
                 DO 50 I=1,qgvga(1)
                 DISVGA(I)=pntgvga(3,I,1)
                 INDPOS(I)=I
50               CONTINUE
                 DO 55 I=1,qrga
                 DISVGA(qgvga(1)+I)=pntrga(3,I)
                 INDPOS(qgvga(1)+I)=qgvga(1)+I
55               CONTINUE
                 IF(qgvga(1)+qrga.GT.0)
     -                 CALL SORTTF(DISVGA,INDPOS,qgvga(1)+qrga)
*   If debugging is on, print the Virtual GAmma's.
                 IF(LDEBUG)THEN
                      WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                     '' Virtual gammas: '',I5,'' total dE='',
     -                     E15.8,'' MeV:''/''  Index'',
     -                     ''          x [cm]          y [cm]'',
     -                     ''          z [cm]        dE [MeV]'',
     -                     '' order'')')
     -                     qgvga(1),esgvga(1)
                      DO 10 I=1,qgvga(1)
                      JPRINT=0
                      DO 80 J=1,qgvga(1)+qrga
                      IF(INDPOS(J).EQ.I)JPRINT=J
80                    CONTINUE
                      WRITE(LUNOUT,'(2X,I5,4(1X,E15.8),I6)')
     -                     I,(pntgvga(J,I,1),J=1,3),egvga(I,1),JPRINT
10                    CONTINUE
*   Same for the delta's.
                      WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                     '' Delta + Auger electrons: '',I5/
     -                     ''  Index'',
     -                     ''          x [cm]          y [cm]'',
     -                     ''          z [cm]    energy [MeV]'',
     -                     '' charge gamma type'')') qdel
                      DO 20 I=1,qdel
                      IF(SOdel(I).EQ.0)THEN
                           WRITE(LUNOUT,'(2X,I5,4(1X,E15.8),F7.1,I6,
     -                          '' delta'')') I,(pntdel(j,i),j=1,3),
     -                          edel(i),zdel(i),ptdel(i)
                      ELSE
                           WRITE(LUNOUT,'(2X,I5,4(1X,E15.8),F7.1,I6,
     -                          '' Auger'')') I,(pntdel(j,i),j=1,3),
     -                          edel(i),zdel(i),ptdel(i)
                      ENDIF
20                    CONTINUE
*   Same for the real photons.
                      WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                     '' Real photons: '',I5/''  Index'',
     -                     ''     x orig [cm]     y orig [cm]'',
     -                     ''     z orig [cm]'',
     -                     ''      x abs [cm]      y abs [cm]'',
     -                     ''      z abs [cm]    energy [MeV]'',
     -                     '' index gamma'')') qrga
                      DO 30 I=1,qrga
                      JPRINT=0
                      DO 35 J=1,qgvga(1)+qrga
                      IF(INDPOS(J).EQ.qgvga(1)+I)JPRINT=J
35                    CONTINUE
                      WRITE(LUNOUT,'(2X,I5,7(1X,E15.8),I6,I6)')
     -                     I,(orirga(j,i),j=1,3),(pntrga(j,i),j=1,3),
     -                     erga(i),JPRINT,ptrga(i)
30                    CONTINUE
*   And finally also the electrons.
                      WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                     '' Electrons: '',I5/''  Index'',
     -                     ''          x [cm]          y [cm]'',
     -                     ''          z [cm]'',
     -                     '' charge delta'')') qcel(1)
                      DO 40 I=1,qcel(1)
                      WRITE(LUNOUT,'(2X,I5,3(1X,E15.8),F7.1,I6)')
     -                     I,(pntcel(j,i,1),j=1,3),zcel(i,1),
     -                     ndelcel(i,1)
40                    CONTINUE
                 ENDIF
*   Store first virtual gamma and electron to deal with.
                 IVGA=1
                 ICEL=0
*   Reset total energy.
                 ETOT=0
            ENDIF
**  If delta's have to be taken into account.
            IF(LTRDEL)THEN
70               CONTINUE
*   Increment the electron counter.
                 ICEL=ICEL+1
*   Check whether we've reached the last electron.
                 IF(ICEL.GT.qcel(1))THEN
*   If so, increment the virtual/real gamma counter.
                      IVGA=IVGA+1
                      ICEL=0
*   Check whether we've reached the last virtual gamma.
                      IF(IVGA.GT.qgvga(1)+qrga)THEN
                           DONE=.TRUE.
                           XCLS=0
                           YCLS=0
                           ZCLS=0
                           ECLS=0
                           NPAIR=0
                           OK=.FALSE.
                           IFAIL=0
                           RETURN
                      ENDIF
*   Otherwise go for another iteration.
                      GOTO 70
                 ELSE
                      DONE=.FALSE.
                 ENDIF
*   See whether this electron belongs to the right gamma.
                 IF(ptdel(ndelcel(ICEL,1)).NE.INDPOS(IVGA))GOTO 70
*   Fetch the location of this electron.
                 XAUX=pntcel(1,ICEL,1)
                 YAUX=pntcel(2,ICEL,1)
                 ZAUX=pntcel(3,ICEL,1)
C        print *,' Taking electron ',icel,' from gamma ',ivga
*   Record ancestry.
                 EXTRA1=ndelcel(ICEL,1)
*   Compute the energy deposited in this electron.
                 EDELTA=edel(ndelcel(ICEL,1))
                 NDELTA=0
                 DO 60 I=1,qcel(1)
                 IF(ndelcel(I,1).EQ.ndelcel(ICEL,1))NDELTA=NDELTA+1
60               CONTINUE
                 IF(NDELTA.LE.0)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : Encountered'//
     -                     ' a delta electron without electrons.'
                      ECLS=-1
                 ELSE
                      ECLS=EDELTA/NDELTA
                 ENDIF
*   Check whether we exceeded the total energy.
                 ETOT=ETOT+MAX(0.0,ECLS)
                 IF(LTRCUT.AND.ETOT.GT.TRENER)THEN
                      PRINT *,' ------ TRACLS MESSAGE : Track'//
     -                     ' truncated because the deposited'//
     -                     ' energy exceeds the particle energy.'
                      DONE=.TRUE.
                      XCLS=0
                      YCLS=0
                      ZCLS=0
                      ECLS=0
                      NPAIR=0
                      OK=.FALSE.
                      IFAIL=0
                      RETURN
                 ENDIF
*   There is only 1 electron in this case.
                 NPAIR=1
**  If we don't want deltas ...
            ELSE
*   Check whether we've already had all energy deposits.
                 IF(IVGA.GT.qgvga(1)+qrga)THEN
                      DONE=.TRUE.
                      XCLS=0
                      YCLS=0
                      ZCLS=0
                      ECLS=0
                      NPAIR=0
                      OK=.FALSE.
                      IFAIL=0
                      RETURN
                 ELSE
                      DONE=.FALSE.
                 ENDIF
*   Fetch the location of this deposit, first virtual gamma's.
                 IF(INDPOS(IVGA).LE.qgvga(1))THEN
                      XAUX=pntgvga(1,INDPOS(IVGA),1)
                      YAUX=pntgvga(2,INDPOS(IVGA),1)
                      ZAUX=pntgvga(3,INDPOS(IVGA),1)
*   Next real gamma's.
                 ELSE
                      XAUX=pntrga(1,INDPOS(IVGA)-qgvga(1))
                      YAUX=pntrga(2,INDPOS(IVGA)-qgvga(1))
                      ZAUX=pntrga(3,INDPOS(IVGA)-qgvga(1))
                 ENDIF
*   Count the number of electrons associated with it.
                 NPAIR=0
                 DO 100 I=1,qcel(1)
                 IF(ptdel(ndelcel(I,1)).EQ.INDPOS(IVGA))NPAIR=NPAIR+1
100              CONTINUE
*   Store energy, checking the total energy.
                 IF(IVGA.le.qgvga(1))THEN
                      IF(LTRCUT.AND.
     -                     ETOT+egvga(INDPOS(IVGA),1).GT.TRENER)THEN
                           ECLS=TRENER-ETOT
                           IVGA=qgvga(1)+1
                      ELSE
                           ECLS=egvga(INDPOS(IVGA),1)
                      ENDIF
                      ETOT=ETOT+ECLS
                 ENDIF
*   Increment the cluster counter.
                 IVGA=IVGA+1
            ENDIF
**  Rotate the cluster position so that it matches the track.
            XCLS=XT0+COS(TRPHI)*XAUX-SIN(TRPHI)*SIN(TRTH)*YAUX+
     -           SIN(TRPHI)*COS(TRTH)*ZAUX
            YCLS=YT0+COS(TRTH)*YAUX+SIN(TRTH)*ZAUX
            ZCLS=ZT0-SIN(TRPHI)*XAUX-COS(TRPHI)*SIN(TRTH)*YAUX+
     -           COS(TRPHI)*COS(TRTH)*ZAUX
*** Fixed number of flux intervals.
       ELSEIF(ITRTYP.EQ.7)THEN
*   Verify that the number of flux lines has been set.
            IF(.NOT.TRFLAG(6))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Number of flux'//
     -                ' lines has not been set; no clusters.'
                 RETURN
            ENDIF
**  On first call, compute the flux intervals.
            IF(NTOT.EQ.0)THEN
*   Set integration intervals.
                 NV=5
*   Compute the inplane vector normal to the track.
                 XP=(YT1-YT0)*FPROJC-(ZT1-ZT0)*FPROJB
                 YP=(ZT1-ZT0)*FPROJA-(XT1-XT0)*FPROJC
                 ZP=(XT1-XT0)*FPROJB-(YT1-YT0)*FPROJA
*   Compute the total flux, accepting positive and negative parts.
                 CALL FLDIN5(XT0,YT0,ZT1,XT1,YT1,ZT1,XP,YP,ZP,Q,
     -                20*NV,0)
                 IF(Q.GT.0)THEN
                      ISIGN=+1
                 ELSE
                      ISIGN=-1
                 ENDIF
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                '' Total flux: '',E15.8,'', selected sign '',I1)')
     -                Q,ISIGN
*   Compute the 1-sided flux in a number of steps.
                 FLXSUM=0
                 IERROR=0
                 XL0FLX=-1
                 XL1FLX=-1
                 DO 110 I=1,MXLIST
                 CALL FLDIN5(
     -                XT0+REAL(I-1)*(XT1-XT0)/REAL(MXLIST),
     -                YT0+REAL(I-1)*(YT1-YT0)/REAL(MXLIST),
     -                ZT0+REAL(I-1)*(ZT1-ZT0)/REAL(MXLIST),
     -                XT0+REAL(I)*(XT1-XT0)/REAL(MXLIST),
     -                YT0+REAL(I)*(YT1-YT0)/REAL(MXLIST),
     -                ZT0+REAL(I)*(ZT1-ZT0)/REAL(MXLIST),
     -                XP,YP,ZP,Q,NV,ISIGN)
                 FLXCOO(I)=REAL(I)/REAL(MXLIST)
                 IF(Q.GT.0)THEN
                      FLXSUM=FLXSUM+Q
                      IF(XL0FLX.LT.-0.5)XL0FLX=REAL(I-1)/REAL(MXLIST)
                      XL1FLX=REAL(I)/REAL(MXLIST)
                 ENDIF
                 IF(Q.LT.0)IERROR=IERROR+1
                 FLXTAB(I)=FLXSUM
110              CONTINUE
*   Make sure that the sum is positive.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                '' Used flux: '',E15.8,'' V''/26X,''Start: '',
     -                F10.3,'' End: '',F10.3)') FLXSUM,XL0FLX,XL1FLX
                 IF(FLXSUM.LE.0)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : 1-Sided flux'//
     -                     ' integral is not > 0; no clusters.'
                      RETURN
                 ELSEIF(XL0FLX.LT.-0.5.OR.XL1FLX.LT.-0.5.OR.
     -                XL1FLX.LE.XL0FLX)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : No flux'//
     -                     ' interval without sign change found.'
                      RETURN
                 ELSEIF(IERROR.NE.0)THEN
                      PRINT *,' ------ TRACLS MESSAGE : The flux'//
     -                     ' changes sign over the track; part of'//
     -                     ' the track not used.'
                 ENDIF
*   Normalise the flux.
                 DO 120 I=1,MXLIST
                 FLXTAB(I)=REAL(NTRFLX-1)*FLXTAB(I)/FLXSUM
120              CONTINUE
            ENDIF
**  Increment cluster counter.
            NTOT=NTOT+1
*   Compute new cluster position.
            IF(NTOT.EQ.1)THEN
                 XL=XL0FLX
            ELSEIF(NTOT.GE.1.AND.NTOT.LT.NTRFLX)THEN
                 XL=MIN(XL1FLX,MAX(XL0FLX,
     -                DIVDIF(FLXCOO,FLXTAB,MXLIST,REAL(NTOT-1),1)))
            ELSEIF(NTOT.EQ.NTRFLX)THEN
                 XL=XL1FLX
            ELSE
                 XL=0.5*(XL1FLX-XL0FLX)
            ENDIF
            XCLS=XT0+XL*(XT1-XT0)
            YCLS=YT0+XL*(YT1-YT0)
            ZCLS=ZT0+XL*(ZT1-ZT0)
*   Set the cluster size and energy.
            NPAIR=1
            ECLS=0
*   See whether we were already done.
            IF(NTOT.GT.NTRFLX)THEN
                 DONE=.TRUE.
                 OK=.FALSE.
            ELSE
                 DONE=.FALSE.
            ENDIF
*** Fixed flux interval.
       ELSEIF(ITRTYP.EQ.8)THEN
*   Verify that the number of flux lines has been set.
            IF(.NOT.TRFLAG(7))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : The flux interval'//
     -                ' has not been set; no clusters.'
                 RETURN
            ENDIF
**  On first call, compute the flux intervals.
            IF(NTOT.EQ.0)THEN
*   Set integration intervals.
                 NV=5
*   Compute the inplane vector normal to the track.
                 XP=(YT1-YT0)*FPROJC-(ZT1-ZT0)*FPROJB
                 YP=(ZT1-ZT0)*FPROJA-(XT1-XT0)*FPROJC
                 ZP=(XT1-XT0)*FPROJB-(YT1-YT0)*FPROJA
*   Compute the total flux, accepting positive and negative parts.
                 CALL FLDIN5(XT0,YT0,ZT1,XT1,YT1,ZT1,XP,YP,ZP,Q,
     -                NTRFLX*NV,0)
                 IF(Q.GT.0)THEN
                      ISIGN=+1
                 ELSE
                      ISIGN=-1
                 ENDIF
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                '' Total flux: '',E15.8,'' V, sign '',I1)')
     -                Q,ISIGN
*   Compute the 1-sided flux in a number of steps.
                 FLXSUM=0
                 IERROR=0
                 XL0FLX=-1
                 DO 130 I=1,MXLIST
                 CALL FLDIN5(
     -                XT0+REAL(I-1)*(XT1-XT0)/REAL(MXLIST),
     -                YT0+REAL(I-1)*(YT1-YT0)/REAL(MXLIST),
     -                ZT0+REAL(I-1)*(ZT1-ZT0)/REAL(MXLIST),
     -                XT0+REAL(I)*(XT1-XT0)/REAL(MXLIST),
     -                YT0+REAL(I)*(YT1-YT0)/REAL(MXLIST),
     -                ZT0+REAL(I)*(ZT1-ZT0)/REAL(MXLIST),
     -                XP,YP,ZP,Q,NV,ISIGN)
                 FLXCOO(I)=REAL(I)/REAL(MXLIST)
                 IF(Q.GT.0)THEN
                      FLXSUM=FLXSUM+Q
                      IF(XL0FLX.LT.-0.5)XL0FLX=REAL(I-1)/REAL(MXLIST)
                 ENDIF
                 IF(Q.LT.0)IERROR=IERROR+1
                 FLXTAB(I)=FLXSUM
130              CONTINUE
*   Make sure that the sum is positive.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ TRACLS DEBUG   :'',
     -                '' Used flux: '',E15.8,'' V ''/26X,
     -                ''Start offset: '',F10.3)') FLXSUM,XL0FLX
                 IF(FLXSUM.LE.0)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : 1-Sided flux'//
     -                     ' integral is not > 0; no clusters.'
                      RETURN
                 ELSEIF(XL0FLX.LT.-0.5)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : No flux'//
     -                     ' interval without sign change found.'
                      RETURN
                 ELSEIF(IERROR.NE.0)THEN
                      PRINT *,' ------ TRACLS MESSAGE : The flux'//
     -                     ' changes sign over the track; part of'//
     -                     ' the track not used.'
                 ENDIF
            ENDIF
**  Increment cluster counter.
            NTOT=NTOT+1
*   Compute new cluster position.
            IF(NTOT.EQ.1)THEN
                 XL=XL0FLX
                 DONE=.FALSE.
            ELSEIF((NTOT-1)*TRFLUX.LE.FLXSUM)THEN
                 XL=DIVDIF(FLXCOO,FLXTAB,MXLIST,REAL(NTOT-1)*TRFLUX,1)
                 DONE=.FALSE.
            ELSE
                 XL=XL0FLX
                 DONE=.TRUE.
                 OK=.FALSE.
            ENDIF
            XCLS=XT0+XL*(XT1-XT0)
            YCLS=YT0+XL*(YT1-YT0)
            ZCLS=ZT0+XL*(ZT1-ZT0)
*   Set the cluster size and energy.
            NPAIR=1
            ECLS=0
*** SRIM clustering
       ELSEIF(ITRTYP.EQ.9)THEN
*   Check that SRIM data is available
            IF(.NOT.SRIMOK)THEN
                 PRINT *,' !!!!!! TRACLS WARNING : No SRIM data'//
     -                ' found; no clusters.'
                 RETURN
            ELSEIF(A.LE.0.OR.Z.LE.0)THEN
                 PRINT *,' !!!!!! TRACLS WARNING : The A and Z'//
     -                ' parameters of the gas are missing; no clusters.'
            ELSEIF(.NOT.TRFLAG(2))THEN
                 PRINT *,' !!!!!! TRACLS WARNING : Particle'//
     -                ' properties not present; no clusters.'
                 RETURN
            ENDIF
*   On first call, generate the SRIM clusters
            IF(NTOT.EQ.0)THEN
                 CALL SRMGEN(IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : Generation'//
     -                     ' of SRIM clusters failed; no clusters.'
                      RETURN
                 ENDIF
            ENDIF
*   Copy a new cluster from the buffer
            NTOT=NTOT+1
            IF(NTOT.LE.NCSRIM)THEN
                 XCLS=XSRIM(NTOT)
                 YCLS=YSRIM(NTOT)
                 ZCLS=ZSRIM(NTOT)
                 NPAIR=NESRIM(NTOT)
                 ECLS=ECSRIM(NTOT)
                 EXTRA1=EKSRIM(NTOT)
                 DONE=.FALSE.
                 OK=.TRUE.
            ELSE
                 XCLS=0.0
                 YCLS=0.0
                 ZCLS=0.0
                 NPAIR=0
                 ECLS=0.0
                 DONE=.TRUE.
                 OK=.FALSE.
            ENDIF
*** TRIM clustering.
       ELSEIF(ITRTYP.EQ.10)THEN
*   Check that TRIM data is available
            IF(.NOT.TRIMOK)THEN
                 PRINT *,' !!!!!! TRACLS WARNING : No TRIM data'//
     -                ' found; no clusters.'
                 RETURN
            ELSEIF(A.LE.0.OR.Z.LE.0)THEN
                 PRINT *,' !!!!!! TRACLS WARNING : The A and Z'//
     -                ' parameters of the gas are missing; no clusters.'
            ENDIF
*   On first call, generate the TRIM clusters
            IF(NTOT.EQ.0)THEN
                 CALL TRMGEN(IFAIL1)
                 IF(IFAIL1.NE.0)THEN
                      PRINT *,' !!!!!! TRACLS WARNING : Generation'//
     -                     ' of TRIM clusters failed; no clusters.'
                      RETURN
                 ENDIF
            ENDIF
*   Copy a new cluster from the buffer
            NTOT=NTOT+1
            IF(NTOT.LE.NCTRIM)THEN
                 XCLS=XTRIM(NTOT)
                 YCLS=YTRIM(NTOT)
                 ZCLS=ZTRIM(NTOT)
                 NPAIR=NETRIM(NTOT)
                 ECLS=ECTRIM(NTOT)
                 EXTRA1=EKTRIM(NTOT)
                 DONE=.FALSE.
                 OK=.TRUE.
            ELSE
                 XCLS=0.0
                 YCLS=0.0
                 ZCLS=0.0
                 NPAIR=0
                 ECLS=0.0
                 DONE=.TRUE.
                 OK=.FALSE.
            ENDIF
*** Other track types.
       ELSE
            PRINT *,' !!!!!! TRACLS WARNING : Unknown track type'//
     -           ' requested; no clusters'
            XCLS=0
            YCLS=0
            ZCLS=0
            ECLS=0
            NPAIR=0
            DONE=.TRUE.
            OK=.FALSE.
            IFAIL=1
            RETURN
       ENDIF
*** Seems to have worked, set the IFAIL flag.
       IFAIL=0
       RETURN
*** Entry point for initialisation.
       ENTRY TRACLI
       IF(LIDENT)PRINT *,' /// ENTRY TRACLI ///'
*   Reset the number of clusters generated sofar.
       NTOT=0
       IVGA=0
       ETOT=0
*   Set flag that clustering can proceed.
       OK=.TRUE.
*** Set the particle identifier, fixed number.
       IF(ITRTYP.EQ.1)THEN
            CALL OUTFMT(REAL(NTRLIN),2,AUX,NCAUX,'LEFT')
            PARTID=AUX(1:NCAUX)//' equally spaced points'
*   Equal.
       ELSEIF(ITRTYP.EQ.2)THEN
            PARTID='Equally spaced clusters'
*   Exponential.
       ELSEIF(ITRTYP.EQ.3)THEN
            PARTID='Exponentially spaced clusters'
*   Heed.
       ELSEIF(ITRTYP.EQ.4)THEN
            IF(TRENER.LT.0.001)THEN
                 CALL OUTFMT(TRENER*1000000,2,AUX,NCAUX,'LEFT')
                 PARTID=PNAME(1:NCPNAM)//', Ekin='//AUX(1:NCAUX)//' eV'
            ELSEIF(TRENER.LT.1)THEN
                 CALL OUTFMT(TRENER*1000,2,AUX,NCAUX,'LEFT')
                 PARTID=PNAME(1:NCPNAM)//', Ekin='//AUX(1:NCAUX)//' keV'
            ELSEIF(TRENER.LT.1000)THEN
                 CALL OUTFMT(TRENER,2,AUX,NCAUX,'LEFT')
                 PARTID=PNAME(1:NCPNAM)//', Ekin='//AUX(1:NCAUX)//' MeV'
            ELSEIF(TRENER.LT.1000000)THEN
                 CALL OUTFMT(TRENER/1000,2,AUX,NCAUX,'LEFT')
                 PARTID=PNAME(1:NCPNAM)//', Ekin='//AUX(1:NCAUX)//' GeV'
            ELSE
                 CALL OUTFMT(TRENER/1000000,2,AUX,NCAUX,'LEFT')
                 PARTID=PNAME(1:NCPNAM)//', Ekin='//AUX(1:NCAUX)//' TeV'
            ENDIF
            qgvga(1)=0
            qdel=0
            qcel(1)=0
            qrga=0
*   Weighted.
       ELSEIF(ITRTYP.EQ.5)THEN
            CALL OUTFMT(REAL(NTRSAM),2,AUX,NCAUX,'LEFT')
            PARTID=AUX(1:NCAUX)//' samples of '//FCNTRW(1:NCTRW)
*   Single cluster.
       ELSEIF(ITRTYP.EQ.6)THEN
            PARTID='Single cluster'
*   Fixed number of flux lines.
       ELSEIF(ITRTYP.EQ.7)THEN
            CALL OUTFMT(REAL(NTRFLX),2,AUX,NCAUX,'LEFT')
            PARTID=AUX(1:NCAUX)//' flux lines'
*   Constant flux intervals.
       ELSEIF(ITRTYP.EQ.8)THEN
            CALL OUTFMT(TRFLUX,2,AUX,NCAUX,'LEFT')
            PARTID='Flux intervals of '//AUX(1:NCAUX)//' V'
*   Constant flux intervals.
       ELSEIF(ITRTYP.EQ.9)THEN
            PARTID='SRIM-based ionisation'
*   Anything else.
       ELSE
            PARTID='Unknown'
       ENDIF
       END
