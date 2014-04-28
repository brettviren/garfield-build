CDECK  ID>, SRMREA.
       SUBROUTINE SRMREA(FILE,NC,IFAIL)
*-----------------------------------------------------------------------
*   SRMREA - Reads SRIM energy loss tables
*   (Last changed on  8/ 3/13.)
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
       REAL SRMDEN,ESRIM,SRMEM,SRMHD,SRMRNG,SRMDT,SRMDL,WSRIM,FSRIM,
     -      XSRIM,YSRIM,ZSRIM,ECSRIM,EKSRIM
       INTEGER NSRIM,NCSRIM,NESRIM
       COMMON /SRMDAT/
     -      ESRIM(MXLIST),SRMEM(MXLIST),
     -      SRMHD(MXLIST),SRMRNG(MXLIST),SRMDT(MXLIST),SRMDL(MXLIST),
     -      XSRIM(MXCLUS),YSRIM(MXCLUS),ZSRIM(MXCLUS),ECSRIM(MXCLUS),
     -      EKSRIM(MXCLUS),SRMDEN,WSRIM,FSRIM,
     -      NSRIM,NCSRIM,NESRIM(MXCLUS)
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
       CHARACTER*(*) FILE
       CHARACTER*80 LINE
       INTEGER NC,IFAIL,IFAIL1,NCLINE,NWORD,INPCMP,INPTYP,I,J,ISKIP
       REAL SCALE
       LOGICAL OK
       EXTERNAL INPCMP,INPTYP
*** Assume this will fail.
       IFAIL=1
*** Open the file
       CALL DSNOPN(FILE,NC,12,'READ-FILE',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! SRMREA WARNING : Unable to open the'//
     -           ' energy loss table ',FILE(1:NC),'; not read.'
            RETURN
       ENDIF
*   Record the opening.
       CALL DSNLOG(FILE(1:NC),'SRIM table','Sequential',
     -      'Read only ')
*   Read the header records, switch to the data file.
       CALL INPSWI('UNIT12')
*** Obtain header information, start with the version
       ISKIP=0
 100   CONTINUE
       CALL INPGET
       CALL INPNUM(NWORD)
       CALL INPSTR(1,NWORD,LINE,NCLINE)
       IF(INDEX(LINE(1:MAX(1,NCLINE)),'SRIM VERSION').NE.0.AND.
     -      LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -           '' SRIM version: '',A)') LINE(1:NCLINE)
       ELSEIF(INDEX(LINE(1:MAX(1,NCLINE)),'CALC. DATE').NE.0.AND.
     -      LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -           '' Run date: '',A)') LINE(1:NCLINE)
       ELSEIF(INDEX(LINE(1:MAX(1,NCLINE)),'ION =').NE.0)THEN
            GOTO 110
       ENDIF
       ISKIP=ISKIP+1
       IF(ISKIP.GT.20)THEN
            PRINT *,' ###### SRMREA ERROR   : Did not find the header'//
     -           ' in SRIM file "',FILE(1:NC),'"; not read.'
            IFAIL=1
            RETURN
       ENDIF
       GOTO 100
 110   CONTINUE
*   Obtain the ion (amu to eV factor from NIST).
       CALL INPSTR(2,NWORD,LINE,NCLINE)
       IF(NCLINE.LE.0)NCLINE=1
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -      '' Ion: '',A)') LINE(1:NCLINE)
       CALL INPSTR(2,2,PNAME,NCPNAM)
       CALL INPCHK(5,2,IFAIL1)
       CALL INPRDR(5,TRMASS,-1.0)
       TRMASS=TRMASS*931.494061
       CALL INPSTR(3,3,LINE,NCLINE)
       LINE(1:1)=' '
       LINE(NCLINE:NCLINE)=' '
       CALL INPRRC(LINE(1:NCLINE),TRCHAR,0.0,IFAIL1)
       IF(TRMASS.LE.0.OR.TRCHAR.LE.0)THEN
            PRINT *,' !!!!!! SRMREA WARNING : Found no valid'//
     -           ' projectile mass and charge in SRIM file;'//
     -           ' data rejected.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
*   Obtain the target density and copy to /GASDAT/ RHO
       CALL INPGET
       CALL INPGET
       IF(INPTYP(2).EQ.2)THEN
            CALL INPCHK(2,2,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 CALL INPERR
                 PRINT *,' !!!!!! SRMREA WARNING : Error reading'//
     -                ' the SRIM gas density; file not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            CALL INPRDR(2,SRMDEN,-1.0)
       ELSEIF(INPTYP(3).EQ.2)THEN
            CALL INPCHK(3,2,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 CALL INPERR
                 PRINT *,' !!!!!! SRMREA WARNING : Error reading'//
     -                ' the SRIM gas density; file not read.'
                 CALL INPSWI('RESTORE')
                 RETURN
            ENDIF
            CALL INPRDR(3,SRMDEN,-1.0)
       ELSE
            PRINT *,' !!!!!! SRMREA WARNING : Did not find the'//
     -           ' SRIM gas density; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
       IF(SRMDEN.GT.0)THEN
            IF(RHO.GT.0)PRINT *,' ------ SRMREA MESSAGE : Earlier'//
     -           ' gas density has been replaced by SRIM data.'
            RHO=SRMDEN
       ELSE
            PRINT *,' !!!!!! SRMREA WARNING : Gas density obtained'//
     -           ' from SRIM file is not positive; file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -      '' Density: '',E10.3,'' g/cm3'')') SRMDEN
*   Obtain the target composition
       CALL INPGET
       CALL INPGET
       CALL INPGET
       CALL INPGET
10     CONTINUE
       CALL INPGET
       CALL INPSTR(1,1,LINE,NCLINE)
       CALL INPNUM(NWORD)
       IF(NWORD.EQ.0.OR.LINE(2:10).EQ.'=========')GOTO 20
       CALL INPNUM(NWORD)
       CALL INPSTR(1,NWORD,LINE,NCLINE)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -      '' Target material: '',A)') LINE(1:NCLINE)
       GOTO 10
20     CONTINUE
*   Skip the Bragg correction.
       CALL INPGET
*   Obtain the stopping units.
       CALL INPGET
       CALL INPNUM(NWORD)
       CALL INPSTR(3,NWORD,LINE,NCLINE)
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -      '' Stopping unit: '',A)') LINE(1:NCLINE)
*   Skip until the line below the table header
       ISKIP=0
80     CONTINUE
       ISKIP=ISKIP+1
       IF(ISKIP.GT.10)THEN
            PRINT *,' !!!!!! SRMREA WARNING : Unable to locate the'//
     -           ' table header; SRIM file not read.'
            CALL INPSWI('RESTORE')
            RETURN
       ENDIF
       CALL INPGET
       CALL INPSTR(1,1,LINE,NCLINE)
       IF(LINE(1:11).NE.'-----------')GOTO 80
*** Read the tables.
       NSRIM=0
       OK=.TRUE.
30     CONTINUE
*   Get a line
       CALL INPGET
*   See whether this is the end
       CALL INPSTR(1,1,LINE,NCLINE)
       IF(LINE(1:10).EQ.'----------')GOTO 40
       CALL INPNUM(NWORD)
       IF(NWORD.NE.10)THEN
            PRINT *,' !!!!!! SRMREA WARNING : Unexpected end of file'//
     -           ' encountered on file ',FILE(1:NC),'.'
            OK=.FALSE.
            GOTO 40
       ENDIF
*   Increment the record
       IF(NSRIM.LT.MXLIST)THEN
            NSRIM=NSRIM+1
       ELSE
            PRINT *,' !!!!!! SRMREA WARNING : Too many input records;'//
     -           ' stopped reading after MXLIST lines.'
            OK=.FALSE.
            GOTO 40
       ENDIF
*   Read the energy [SRIM unit: follows in field 2]
       CALL INPCHK(1,2,IFAIL1)
       CALL INPRDR(1,ESRIM(NSRIM),0.0)
       IF(INPCMP(2,'EV').NE.0)THEN
            ESRIM(NSRIM)=ESRIM(NSRIM)*1e-6
       ELSEIF(INPCMP(2,'KEV').NE.0)THEN
            ESRIM(NSRIM)=ESRIM(NSRIM)*1e-3
       ELSEIF(INPCMP(2,'MEV').NE.0)THEN
            ESRIM(NSRIM)=ESRIM(NSRIM)*1
       ELSEIF(INPCMP(2,'GEV').NE.0)THEN
            ESRIM(NSRIM)=ESRIM(NSRIM)*1e3
       ELSE
            CALL INPSTR(2,2,LINE,NCLINE)
            PRINT *,' !!!!!! SRMREA WARNING : Unknown energy unit'//
     -           ' encountered: "'//LINE(1:NCLINE)//'".'
            OK=.FALSE.
       ENDIF
*   Read the em dedx [SRIM unit: see above, usually 1e3 MeV.cm2/g]
       CALL INPCHK(3,2,IFAIL1)
       CALL INPRDR(3,SRMEM(NSRIM),0.0)
*   Read the hd dedx [SRIM unit: see above, usually 1e3 MeV.cm2/g]
       CALL INPCHK(4,2,IFAIL1)
       CALL INPRDR(4,SRMHD(NSRIM),0.0)
*   Read range [SRIM unit: follows in field 6]
       CALL INPCHK(5,2,IFAIL1)
       CALL INPRDR(5,SRMRNG(NSRIM),0.0)
       IF(INPCMP(6,'A').NE.0)THEN
            SRMRNG(NSRIM)=SRMRNG(NSRIM)*1E-8
       ELSEIF(INPCMP(6,'UM').NE.0)THEN
            SRMRNG(NSRIM)=SRMRNG(NSRIM)*1E-4
       ELSEIF(INPCMP(6,'MM').NE.0)THEN
            SRMRNG(NSRIM)=SRMRNG(NSRIM)*1E-1
       ELSEIF(INPCMP(6,'CM').NE.0)THEN
            SRMRNG(NSRIM)=SRMRNG(NSRIM)*1
       ELSEIF(INPCMP(6,'M').NE.0)THEN
            SRMRNG(NSRIM)=SRMRNG(NSRIM)*1E2
       ELSE
            CALL INPSTR(6,6,LINE,NCLINE)
            PRINT *,' !!!!!! SRMREA WARNING : Range unit is not'//
     -           ' known: "'//LINE(1:NCLINE)//'".'
            OK=.FALSE.
       ENDIF
*   Read straggling [SRIM unit: follows in fields 8 and 10]
       CALL INPCHK(7,2,IFAIL1)
       CALL INPRDR(7,SRMDL(NSRIM),0.0)
       IF(INPCMP(8,'A').NE.0)THEN
            SRMDL(NSRIM)=SRMDL(NSRIM)*1E-8
       ELSEIF(INPCMP(8,'UM').NE.0)THEN
            SRMDL(NSRIM)=SRMDL(NSRIM)*1E-4
       ELSEIF(INPCMP(8,'MM').NE.0)THEN
            SRMDL(NSRIM)=SRMDL(NSRIM)*1E-1
       ELSEIF(INPCMP(8,'CM').NE.0)THEN
            SRMDL(NSRIM)=SRMDL(NSRIM)*1
       ELSEIF(INPCMP(6,'M').NE.0)THEN
            SRMDL(NSRIM)=SRMDL(NSRIM)*1E2
       ELSE
            CALL INPSTR(8,8,LINE,NCLINE)
            PRINT *,' !!!!!! SRMREA WARNING : Longitudinal straggling'//
     -           ' unit is not known: "'//LINE(1:NCLINE)//'".'
            OK=.FALSE.
       ENDIF
       CALL INPCHK(9,2,IFAIL1)
       CALL INPRDR(9,SRMDT(NSRIM),0.0)
       IF(INPCMP(10,'A').NE.0)THEN
            SRMDT(NSRIM)=SRMDT(NSRIM)*1E-8
       ELSEIF(INPCMP(10,'UM').NE.0)THEN
            SRMDT(NSRIM)=SRMDT(NSRIM)*1E-4
       ELSEIF(INPCMP(10,'MM').NE.0)THEN
            SRMDT(NSRIM)=SRMDT(NSRIM)*1E-1
       ELSEIF(INPCMP(10,'CM').NE.0)THEN
            SRMDT(NSRIM)=SRMDT(NSRIM)*1
       ELSEIF(INPCMP(6,'M').NE.0)THEN
            SRMDT(NSRIM)=SRMDT(NSRIM)*1E2
       ELSE
            CALL INPSTR(10,10,LINE,NCLINE)
            PRINT *,' !!!!!! SRMREA WARNING : Transverse straggling'//
     -           ' unit is not known: "'//LINE(1:NCLINE)//'".'
            OK=.FALSE.
       ENDIF
       GOTO 30
40     CONTINUE
*** End of table
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   : Read '',
     -      I5,'' records.'')') NSRIM
*   Skip 2 records of text
       CALL INPGET
       CALL INPGET
*   Read the conversion units and compare with the unit we want
       DO 50 I=1,8
       CALL INPGET
       CALL INPNUM(NWORD)
       CALL INPSTR(2,NWORD,LINE,NCLINE)
       IF(LINE(1:NCLINE).EQ.'MEV / (MG/CM2)'.OR.
     -      LINE(1:NCLINE).EQ.'MEV/(MG/CM2)')THEN
            CALL INPCHK(1,2,IFAIL1)
            CALL INPRDR(1,SCALE,1.0)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ SRMREA DEBUG   :'',
     -          '' Stopping scaling: '',E12.5)') SCALE*1E3
            DO 60 J=1,NSRIM
            SRMEM(J)=SRMEM(J)*SCALE*1E3
            SRMHD(J)=SRMHD(J)*SCALE*1E3
60          CONTINUE
            GOTO 70
       ENDIF
50     CONTINUE
       PRINT *,' !!!!!! SRMREA WARNING : Did not find conversion'//
     -      ' factor for scaling to MeV cm2/mg; no scaling applied.'
       OK=.FALSE.
70     CONTINUE
*** Revert to normal reading
       CALL INPSWI('RESTORE')
*** Close the unit
       CLOSE(UNIT=12,ERR=2030)
*** Seems to have been successful.
       IF(OK)THEN
            IFAIL=0
       ELSE
            IFAIL=1
       ENDIF
       RETURN
*** I/O error handling
2030   CONTINUE
       PRINT *,' !!!!!! SRMREA WARNING : Error closing the input'//
     -      ' file ',FILE(1:NC),'; no immediate problems expected.'
       END
