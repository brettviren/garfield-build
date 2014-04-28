CDECK  ID>, GASMRG.
       SUBROUTINE GASMRG(IFAIL)
*-----------------------------------------------------------------------
*   GASMRG - Merges gas data from a file with existing gas tables.
*   VARIABLES : NWORD       : Number of parameters provided.
*               STRING      : String for character manipulation.
*   (Last changed on 16/ 2/11.)
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
       REAL SRMDEN,ESRIM,SRMEM,SRMHD,SRMRNG,SRMDT,SRMDL,WSRIM,FSRIM,
     -      XSRIM,YSRIM,ZSRIM,ECSRIM,EKSRIM
       INTEGER NSRIM,NCSRIM,NESRIM
       COMMON /SRMDAT/
     -      ESRIM(MXLIST),SRMEM(MXLIST),
     -      SRMHD(MXLIST),SRMRNG(MXLIST),SRMDT(MXLIST),SRMDL(MXLIST),
     -      XSRIM(MXCLUS),YSRIM(MXCLUS),ZSRIM(MXCLUS),ECSRIM(MXCLUS),
     -      EKSRIM(MXCLUS),SRMDEN,WSRIM,FSRIM,
     -      NSRIM,NCSRIM,NESRIM(MXCLUS)
       CHARACTER*(MXCHAR) STRING
       CHARACTER*8 MEMBER
       CHARACTER*(MXNAME) FILE
       CHARACTER*45 DESC
       INTEGER IFAIL,NCFILE,NCMEMB,I,J,K,L,M,N,IOS,IFAIL1,NWORD,INPCMP,
     -      NGASN,NBANGN,NBTABN,INDEXE,INDEXA,INDEXB,
     -      IEMODE,IAMODE,IBMODE,
     -      IVEXTN,IXEXTN,IYEXTN,IDEXTN,IAEXTN,IBEXTN,IMEXTN,IWEXTN,
     -      IOEXTN,IHEXTN,ISEXTN,IEEXTN,IZEXTN,
     -      JVEXTN,JXEXTN,JYEXTN,JDEXTN,JAEXTN,JBEXTN,JMEXTN,JWEXTN,
     -      JOEXTN,JHEXTN,JSEXTN,JEEXTN,JZEXTN,
     -      IATHRN,IBTHRN,IHTHRN,
     -      IVMETN,IXMETN,IYMETN,IDMETN,IAMETN,IBMETN,IMMETN,IWMETN,
     -      IHMETN,IOMETN,ISMETN,IEMETN,IZMETN,
     -      NCLSN,NEXGSN,NIOGSN,IEXC,IION,LOOKEX(MXEXG),LOOKIO(MXIOG),
     -      NQMOL
       LOGICAL DSNCMP,EXIS,GASOKN(20),TAB2DN,REPOLD,HEEDN,SRIMN,
     -      NEWE(MXLIST),NEWB(MXBTAB),NEWA(MXBANG)
       EXTERNAL DSNCMP,INPCMP
       REAL EGASN(MXLIST),BTABN(MXBTAB),BANGN(MXBANG),
     -      VGASN,CVGASN,XGASN,CXGASN,YGASN,CYGASN,
     -      DGASN,CDGASN,OGASN,COGASN,
     -      AGASN,CAGASN,AORIN,BGASN,CBGASN,MGASN,CMGASN,
     -      WGASN,CWGASN,HGASN,CHGASN,SGASN(6),CSGASN(6),
     -      EXGASN(MXEXG),IOGASN(MXIOG),CEXGSN(MXEXG),CIOGSN(MXIOG),
     -      EPSE,EPSA,EPSB,
     -      AN,ZN,EMPRBN,EPAIRN,DLIONN,DTIONN,CMEANN,RHON,
     -      PGASN,TGASN,GASFRN(MXNBMC)
       CHARACTER*10 CLSTPN
       PARAMETER(EPSE=1E-3,EPSA=1E-3,EPSB=1E-3)
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE GASMRG ///'
*** Initialise IFAIL on 1 (i.e. fail).
       IFAIL=1
       FILE=' '
       MEMBER='*'
       NCFILE=8
       NCMEMB=1
*** First decode the argument string, setting file name + member name.
       CALL INPNUM(NWORD)
*   If there's only one argument, it's the dataset name.
       IF(NWORD.GE.2)THEN
            CALL INPSTR(2,2,STRING,NCFILE)
            FILE=STRING
       ENDIF
*   If there's a second argument, it is the member name.
       IF(NWORD.GE.3)THEN
            CALL INPSTR(3,3,STRING,NCMEMB)
            MEMBER=STRING
       ENDIF
*   Other options ?
       REPOLD=.FALSE.
       DO 10 I=4,NWORD
       IF(INPCMP(I,'KEEP-#OLD').NE.0)THEN
            REPOLD=.FALSE.
       ELSEIF(INPCMP(I,'REP#LACE-#OLD').NE.0)THEN
            REPOLD=.TRUE.
       ELSE
            CALL INPMSG(I,'Not a known option')
       ENDIF
10     CONTINUE
*   Print error messages.
       CALL INPERR
*** Check file ane member name lengths.
       IF(NCFILE.GT.MXNAME)THEN
            PRINT *,' !!!!!! GASMRG WARNING : The file name is'//
     -           ' truncated to MXNAME (=',MXNAME,') characters.'
            NCFILE=MIN(NCFILE,MXNAME)
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! GASMRG WARNING : The member name is'//
     -           ' shortened to ',MEMBER,', first 8 characters.'
            NCMEMB=MIN(NCMEMB,8)
       ELSEIF(NCMEMB.LE.0)THEN
            PRINT *,' !!!!!! GASMRG WARNING : The member'//
     -           ' name has zero length, replaced by "*".'
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Reject the empty file name case.
       IF(FILE.EQ.' '.OR.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! GASMRG WARNING : MERGE must be at least'//
     -           ' followed by a dataset name ; no data are read.'
            RETURN
       ENDIF
*** Open a dataset and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GASMRG WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; gas data are not read.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Gas data  ','Sequential','Read data ')
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : Dataset '',
     -      A,'' opened on unit 12 for data reading.'')') FILE(1:NCFILE)
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,NCMEMB,'GAS     ',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,NCMEMB,'GAS     ',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### GASMRG ERROR   : Gas description '//
     -                MEMBER(1:NCMEMB)//' has been deleted from '//
     -                FILE(1:NCFILE)//'; not read.'
            ELSE
                 PRINT *,' ###### GASMRG ERROR   : Gas description '//
     -                MEMBER(1:NCMEMB)//' not found in '//FILE(1:NCFILE)
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   Check that this member contains indeed gas data.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : Dataset'',
     -      '' header record follows:''/2X,A)') STRING(1:80)
       WRITE(*,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      STRING(32:39),STRING(11:18),STRING(23:30),STRING(51:79)
*   Check the version.
       READ(12,'(A15)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(STRING(1:15).NE.' Version   : 12')THEN
            PRINT *,' !!!!!! GASMRG WARNING : This member'//
     -           ' can not be read because of a change in format.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*** Read the GASOK bits.
       READ(12,'(13X,20L1)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (GASOKN(I),I=1,20)
*** Take the new identifier if there was no identifier yet.
       IF(GASID.EQ.' '.OR.REPOLD)THEN
            READ(12,'(13X,A)',END=2000,IOSTAT=IOS,ERR=2010) GASID
       ELSE
            READ(12,'(13X)',END=2000,IOSTAT=IOS,ERR=2010)
       ENDIF
*** Never take the table function, and reset the old function.
       READ(12,'(13X)',END=2000,IOSTAT=IOS,ERR=2010)
       FCNTAB=' '
       NFTAB=1
*** Dimensions of the table to be merged.
       READ(12,'(13X,L1,5I10)',END=2000,IOSTAT=IOS,ERR=2010)
     -      TAB2DN,NGASN,NBANGN,NBTABN,NEXGSN,NIOGSN
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : Dataset'',
     -      '' to be merged has the following dimensions:''/
     -      26X,''2D = '',L1,'', n_E = '',I5,'', n_angle = '',I5,
     -      '', n_B = '',I5,'', n_exc = '',I5,'', n_ion = '',I5)')
     -      TAB2DN,NGASN,NBANGN,NBTABN,NEXGSN,NIOGSN
*   Read the E field.
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (EGASN(I),I=1,NGASN)
*   Determine which mode we have to use for the E field.
       IEMODE=1
       DO 60 I=1,MIN(NGAS,NGASN)
       IF(ABS(EGAS(I)-EGASN(I)).GT.EPSE*(ABS(EGAS(I))+ABS(EGASN(I))))
     -      IEMODE=0
60     CONTINUE
       IF(NGAS.NE.NGASN)IEMODE=0
*   Read the angles.
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (BANGN(I),I=1,NBANGN)
*   Determine which mode we have to use for the angles.
       IAMODE=1
       DO 140 I=1,MIN(NBANG,NBANGN)
       IF(ABS(BANG(I)-BANGN(I)).GT.EPSA*(ABS(BANG(I))+ABS(BANGN(I))))
     -      IAMODE=0
140    CONTINUE
       IF(NBANG.NE.NBANGN)IAMODE=0
*   Read the B field values.
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (BTABN(I),I=1,NBTABN)
*   Determine which mode we have to use for the B field.
       IBMODE=1
       DO 150 I=1,MIN(NBTAB,NBTABN)
       IF(ABS(BTAB(I)-BTABN(I)).GT.EPSB*(ABS(BTAB(I))+ABS(BTABN(I))))
     -      IBMODE=0
150    CONTINUE
       IF(NBTAB.NE.NBTABN)IBMODE=0
*   Debugging.
       IF(LDEBUG.AND.IEMODE.EQ.0)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : E vectors'',
     -           '' differ.'')')
       ELSEIF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : E vectors'',
     -           '' identical.'')')
       ENDIF
       IF(LDEBUG.AND.IAMODE.EQ.0)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : The angle'',
     -           '' vectors differ.'')')
       ELSEIF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : The angle'',
     -           '' vectors are identical.'')')
       ENDIF
       IF(LDEBUG.AND.IBMODE.EQ.0)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : The B field'',
     -           '' vectors differ.'')')
       ELSEIF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   : The B field'',
     -           '' vectors are identical.'')')
       ENDIF
*   Ensure there is a common mode.
       IF(IEMODE+IAMODE+IBMODE.LT.2)THEN
            PRINT *,' !!!!!! GASMRG WARNING : Existing data and data'//
     -           ' in the file don''t have 2 common axis; not merged.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            IFAIL=1
            RETURN
       ENDIF
*** Decide whether we have to produce a 3D table or a 1D table.
       IF((TAB2DN.OR.IBMODE*IAMODE.EQ.0).AND..NOT.TAB2D)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Expanding existing table to 3D mode.'')')
            DO 110 I=1,NGAS
            DO 120 J=1,NBANG
            DO 130 K=1,NBTAB
            VGAS2(I,J,K)=VGAS(I)
            XGAS2(I,J,K)=XGAS(I)
            YGAS2(I,J,K)=YGAS(I)
            DGAS2(I,J,K)=DGAS(I)
            OGAS2(I,J,K)=OGAS(I)
            AGAS2(I,J,K)=AGAS(I)
            AORIG2(I,J,K)=AORIG(I)
            BGAS2(I,J,K)=BGAS(I)
            MGAS2(I,J,K)=MGAS(I)
            WGAS2(I,J,K)=WGAS(I)
            DO 160 L=1,6
            SGAS2(I,J,K,L)=SGAS(I,L)
160         CONTINUE
            HGAS2(I,J,K)=HGAS(I)
            DO 170 L=1,NEXGAS
            EXGAS2(I,J,K,L)=EXGAS(I,L)
170         CONTINUE
            DO 171 L=1,NIOGAS
            IOGAS2(I,J,K,L)=IOGAS(I,L)
171         CONTINUE
130         CONTINUE
120         CONTINUE
110         CONTINUE
            TAB2D=.TRUE.
       ENDIF
*** If the E values differ, warn for lost GASOK bits.
       IF(IEMODE*IBMODE*IAMODE.EQ.0)THEN
            DO 70 I=1,16
            IF(I.EQ.13.OR.I.EQ.14)GOTO 70
*   Check for data currently present which is absent in the new data.
            IF(GASOK(I).AND..NOT.GASOKN(I))THEN
                 IF(I.EQ.1)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' drift velocity is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.2)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' ion mobility is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.3)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' longitudinal diffusion is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.4)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' Townsend coefficient is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.6)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' attachment coefficient is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.7)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' Lorentz angle is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.8)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' transverse diffusion is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.9)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' velocity || Bt is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.10)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' velocity || ExB is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.11)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' diffusion tensor is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.12)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' ion dissociation data is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.15)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' excitation data is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ELSEIF(I.EQ.16)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' ionisation data is absent in'//
     -                     ' the dataset to be added; data reset.'
                 ENDIF
                 GASOK(I)=.FALSE.
*   And for data present in the file but not currently present.
            ELSEIF(GASOKN(I).AND..NOT.GASOK(I))THEN
                 IF(I.EQ.1)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' drift velocity is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.2)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' ion mobility is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.3)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' longitudinal diffusion is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.4)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' Townsend coefficient is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.6)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' attachment coefficient is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.7)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' Lorentz angle is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.8)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' transverse diffusion is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.9)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' velocity || Bt is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.10)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' velocity || ExB is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.11)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' diffusion tensor is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.12)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' ion dissociation data is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.15)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' excitation data is absent in'//
     -                     ' the existing data; new data not used.'
                 ELSEIF(I.EQ.16)THEN
                      PRINT *,' !!!!!! GASMRG WARNING : The'//
     -                     ' ionisation data is absent in'//
     -                     ' the existing data; new data not used.'
                 ENDIF
                 GASOKN(I)=.FALSE.
            ENDIF
70          CONTINUE
       ENDIF
*** Initialise the "new" flags.
       DO 80 I=1,NGAS
       NEWE(I)=.FALSE.
80     CONTINUE
       DO 90 I=1,NBANG
       NEWA(I)=.FALSE.
90     CONTINUE
       DO 100 I=1,NBTAB
       NEWB(I)=.FALSE.
100    CONTINUE
*** Extend an existing 2D table.
       IF(TAB2D)THEN
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Extending the table in 3D mode.'')')
**  Insert room in the tables for new columns in E.
            IF(IEMODE.EQ.0)THEN
                 DO 200 I=1,NGASN
                 DO 210 J=1,NGAS
*   If it overlaps with existing E, either keep old or new data.
                 IF(ABS(EGASN(I)-EGAS(J)).LE.EPSE*(ABS(EGASN(I))+
     -                ABS(EGAS(J))))THEN
                      IF(REPOLD)THEN
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Replacing existing data for E = ',
     -                          EGAS(J)*PGAS,' V/cm by data from file.'
                           INDEXE=J
                           GOTO 250
                      ELSE
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Keeping existing data for E = ',
     -                          EGAS(J)*PGAS,' V/cm, not using data',
     -                          ' from the file.'
                           GOTO 200
                      ENDIF
*   Otherwise shift all data at higher E values.
                 ELSEIF(EGASN(I).LT.EGAS(J))THEN
                      IF(NGAS+1.LE.MXLIST)THEN
                           IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                          '' DEBUG   : Inserting E = '',E15.8,
     -                          '' V/cm at slot '',I5)') EGASN(I)*PGAS,J
                           DO 220 K=1,NBANG
                           DO 230 L=1,NBTAB
                           DO 240 M=NGAS,J,-1
                           VGAS2(M+1,K,L)=VGAS2(M,K,L)
                           XGAS2(M+1,K,L)=XGAS2(M,K,L)
                           YGAS2(M+1,K,L)=YGAS2(M,K,L)
                           DGAS2(M+1,K,L)=DGAS2(M,K,L)
                           OGAS2(M+1,K,L)=OGAS2(M,K,L)
                           AGAS2(M+1,K,L)=AGAS2(M,K,L)
                           AORIG2(M+1,K,L)=AORIG2(M,K,L)
                           BGAS2(M+1,K,L)=BGAS2(M,K,L)
                           MGAS2(M+1,K,L)=MGAS2(M,K,L)
                           WGAS2(M+1,K,L)=WGAS2(M,K,L)
                           DO 290 N=1,6
                           SGAS2(M+1,K,L,N)=SGAS2(M,K,L,N)
290                        CONTINUE
                           HGAS2(M+1,K,L)=HGAS2(M,K,L)
                           DO 291 N=1,NEXGAS
                           EXGAS2(M+1,K,L,N)=EXGAS2(M,K,L,N)
291                        CONTINUE
                           DO 292 N=1,NIOGAS
                           IOGAS2(M+1,K,L,N)=IOGAS2(M,K,L,N)
292                        CONTINUE
                           NEWE(M+1)=NEWE(M)
240                        CONTINUE
230                        CONTINUE
220                        CONTINUE
                           DO 280 M=NGAS,J,-1
                           EGAS(M+1)=EGAS(M)
280                        CONTINUE
                           NGAS=NGAS+1
                           INDEXE=J
                           GOTO 250
                      ELSE
                           GOTO 3010
                      ENDIF
                 ENDIF
*   Next old value.
210              CONTINUE
*   If there is no higher E, then add the line at the end.
                 IF(NGAS+1.GT.MXLIST)GOTO 3010
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                '' DEBUG   : Adding E = '',E15.8,
     -                '' V/cm at the end '',I5)') EGASN(I)*PGAS
                 NGAS=NGAS+1
                 INDEXE=NGAS
*   Zero the new row.
250              CONTINUE
                 EGAS(INDEXE)=EGASN(I)
                 NEWE(INDEXE)=.TRUE.
                 DO 260 K=1,NBANG
                 DO 270 L=1,NBTAB
                 VGAS2(INDEXE,K,L)=0
                 XGAS2(INDEXE,K,L)=0
                 YGAS2(INDEXE,K,L)=0
                 DGAS2(INDEXE,K,L)=0
                 OGAS2(INDEXE,K,L)=0
                 AGAS2(INDEXE,K,L)=0
                 AORIG2(INDEXE,K,L)=0
                 BGAS2(INDEXE,K,L)=0
                 MGAS2(INDEXE,K,L)=0
                 WGAS2(INDEXE,K,L)=0
                 DO 295 N=1,6
                 SGAS2(INDEXE,K,L,N)=0
295              CONTINUE
                 HGAS2(INDEXE,K,L)=0
                 DO 296 N=1,NEXGAS
                 EXGAS2(INDEXE,K,L,N)=0
296              CONTINUE
                 DO 297 N=1,NIOGAS
                 IOGAS2(INDEXE,K,L,N)=0
297              CONTINUE
270              CONTINUE
260              CONTINUE
*   Next new value.
200              CONTINUE
            ENDIF
**  Insert room in the tables for new columns in B.
            IF(IBMODE.EQ.0)THEN
                 DO 300 I=1,NBTABN
                 DO 310 J=1,NBTAB
*   If it overlaps with existing B, either keep old or new data.
                 IF(ABS(BTABN(I)-BTAB(J)).LE.EPSB*(ABS(BTABN(I))+
     -                ABS(BTAB(J))))THEN
                      IF(REPOLD)THEN
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Replacing old data for B = ',BTAB(J)/
     -                          100,' T by data from file.'
                           INDEXB=J
                           GOTO 350
                      ELSE
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Keeping old data for B = ',BTAB(J)/
     -                          100,' T, not using data from file.'
                           GOTO 300
                      ENDIF
*   Otherwise shift all data at higher B values.
                 ELSEIF(BTABN(I).LT.BTAB(J))THEN
                      IF(NBTAB+1.LE.MXBTAB)THEN
                           IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                          '' DEBUG   : Inserting B = '',E15.8,
     -                          '' T at slot '',I5)') BTABN(I)/100,J
                           DO 320 K=1,NGAS
                           DO 330 L=1,NBANG
                           DO 340 M=NBTAB,J,-1
                           VGAS2(K,L,M+1)=VGAS2(K,L,M)
                           XGAS2(K,L,M+1)=XGAS2(K,L,M)
                           YGAS2(K,L,M+1)=YGAS2(K,L,M)
                           DGAS2(K,L,M+1)=DGAS2(K,L,M)
                           OGAS2(K,L,M+1)=OGAS2(K,L,M)
                           AGAS2(K,L,M+1)=AGAS2(K,L,M)
                           AORIG2(K,L,M+1)=AORIG2(K,L,M)
                           BGAS2(K,L,M+1)=BGAS2(K,L,M)
                           MGAS2(K,L,M+1)=MGAS2(K,L,M)
                           WGAS2(K,L,M+1)=WGAS2(K,L,M)
                           DO 345 N=1,6
                           SGAS2(K,L,M+1,N)=SGAS2(K,L,M,N)
345                        CONTINUE
                           HGAS2(K,L,M+1)=HGAS2(K,L,M)
                           DO 346 N=1,NEXGAS
                           EXGAS2(K,L,M+1,N)=EXGAS2(K,L,M,N)
346                        CONTINUE
                           DO 347 N=1,NIOGAS
                           IOGAS2(K,L,M+1,N)=IOGAS2(K,L,M,N)
347                        CONTINUE
                           NEWB(M+1)=NEWB(M)
340                        CONTINUE
330                        CONTINUE
320                        CONTINUE
                           DO 380 M=NBTAB,J,-1
                           BTAB(M+1)=BTAB(M)
380                        CONTINUE
                           NBTAB=NBTAB+1
                           INDEXB=J
                           GOTO 350
                      ELSE
                           GOTO 3020
                      ENDIF
                 ENDIF
*   Next old value.
310              CONTINUE
*   If there is no higher B, then add the line at the end.
                 IF(NBTAB+1.GT.MXBTAB)GOTO 3020
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                '' DEBUG   : Adding B = '',E15.8,
     -                '' T at the end '',I5)') BTABN(I)/100
                 NBTAB=NBTAB+1
                 INDEXB=NBTAB
*   Zero the new row.
350              CONTINUE
                 BTAB(INDEXB)=BTABN(I)
                 NEWB(INDEXB)=.TRUE.
                 DO 360 K=1,NGAS
                 DO 370 L=1,NBANG
                 VGAS2(K,L,INDEXB)=0
                 XGAS2(K,L,INDEXB)=0
                 YGAS2(K,L,INDEXB)=0
                 DGAS2(K,L,INDEXB)=0
                 OGAS2(K,L,INDEXB)=0
                 AGAS2(K,L,INDEXB)=0
                 AORIG2(K,L,INDEXB)=0
                 BGAS2(K,L,INDEXB)=0
                 MGAS2(K,L,INDEXB)=0
                 WGAS2(K,L,INDEXB)=0
                 DO 375 N=1,6
                 SGAS2(K,L,INDEXB,N)=0
375              CONTINUE
                 HGAS2(K,L,INDEXB)=0
                 DO 376 N=1,NEXGAS
                 EXGAS2(K,L,INDEXB,N)=0
376              CONTINUE
                 DO 377 N=1,NIOGAS
                 IOGAS2(K,L,INDEXB,N)=0
377              CONTINUE
370              CONTINUE
360              CONTINUE
*   Next new value.
300              CONTINUE
            ENDIF
**  Insert room in the tables for new columns in angle.
            IF(IAMODE.EQ.0)THEN
                 DO 400 I=1,NBANGN
                 DO 410 J=1,NBANG
*   If it overlaps with an existing angle, either keep old or new data.
                 IF(ABS(BANGN(I)-BANG(J)).LE.EPSA*(ABS(BANGN(I))+
     -                ABS(BANG(J))))THEN
                      IF(REPOLD)THEN
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Replacing old data for angle(E,B) = ',
     -                          BANG(J)*180/PI,' degrees by data from'//
     -                          ' the file.'
                           INDEXA=J
                           GOTO 450
                      ELSE
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Keeping old data for angle(E,B) = ',
     -                          BANG(J)*180/PI,' degrees, not using'//
     -                          ' data from file.'
                           GOTO 400
                      ENDIF
*   Otherwise shift all data at higher angles.
                 ELSEIF(BANGN(I).LT.BANG(J))THEN
                      IF(NBANG+1.LE.MXBANG)THEN
                           IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                          '' DEBUG   : Inserting angle = '',E15.8,
     -                          '' degrees at slot '',I5)')
     -                          BANGN(I)*180/PI,J
                           DO 420 K=1,NGAS
                           DO 430 L=1,NBTAB
                           DO 440 M=NBANG,J,-1
                           VGAS2(K,M+1,L)=VGAS2(K,M,L)
                           XGAS2(K,M+1,L)=XGAS2(K,M,L)
                           YGAS2(K,M+1,L)=YGAS2(K,M,L)
                           DGAS2(K,M+1,L)=DGAS2(K,M,L)
                           OGAS2(K,M+1,L)=OGAS2(K,M,L)
                           AGAS2(K,M+1,L)=AGAS2(K,M,L)
                           AORIG2(K,M+1,L)=AORIG2(K,M,L)
                           BGAS2(K,M+1,L)=BGAS2(K,M,L)
                           MGAS2(K,M+1,L)=MGAS2(K,M,L)
                           WGAS2(K,M+1,L)=WGAS2(K,M,L)
                           DO 445 N=1,6
                           SGAS2(K,M+1,L,N)=SGAS2(K,M,L,N)
445                        CONTINUE
                           HGAS2(K,M+1,L)=HGAS2(K,M,L)
                           DO 446 N=1,NEXGAS
                           EXGAS2(K,M+1,L,N)=EXGAS2(K,M,L,N)
446                        CONTINUE
                           DO 447 N=1,NIOGAS
                           IOGAS2(K,M+1,L,N)=IOGAS2(K,M,L,N)
447                        CONTINUE
                           NEWA(M+1)=NEWA(M)
440                        CONTINUE
430                        CONTINUE
420                        CONTINUE
                           DO 480 M=NBANG,J,-1
                           BANG(M+1)=BANG(M)
480                        CONTINUE
                           NBANG=NBANG+1
                           INDEXA=J
                           GOTO 450
                      ELSE
                           GOTO 3030
                      ENDIF
                 ENDIF
*   Next old value.
410              CONTINUE
*   If there is no higher angle, then add the line at the end.
                 IF(NBANG+1.GT.MXBANG)GOTO 3030
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                '' DEBUG   : Adding angle = '',E15.8,
     -                '' degrees at the end '',I5)') BANGN(I)*180/PI
                 NBANG=NBANG+1
                 INDEXA=NBANG
*   Zero the new row.
450              CONTINUE
                 BANG(INDEXA)=BANGN(I)
                 NEWA(INDEXA)=.TRUE.
                 DO 460 K=1,NGAS
                 DO 470 L=1,NBTAB
                 VGAS2(K,INDEXA,L)=0
                 XGAS2(K,INDEXA,L)=0
                 YGAS2(K,INDEXA,L)=0
                 DGAS2(K,INDEXA,L)=0
                 OGAS2(K,INDEXA,L)=0
                 AGAS2(K,INDEXA,L)=0
                 AORIG2(K,INDEXA,L)=0
                 BGAS2(K,INDEXA,L)=0
                 MGAS2(K,INDEXA,L)=0
                 WGAS2(K,INDEXA,L)=0
                 DO 475 N=1,6
                 SGAS2(K,INDEXA,L,N)=0
475              CONTINUE
                 DO 476 N=1,NEXGAS
                 EXGAS2(K,INDEXA,L,N)=0
476              CONTINUE
                 DO 477 N=1,NIOGAS
                 IOGAS2(K,INDEXA,L,N)=0
477              CONTINUE
                 HGAS2(K,INDEXA,L)=0
470              CONTINUE
460              CONTINUE
*   Next new value.
400              CONTINUE
            ENDIF
*** Extend a 1-dimensional table.
       ELSE
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Extending the table in 1D mode.'')')
**  Insert room in the tables for new columns in E.
            IF(IEMODE.EQ.0)THEN
                 DO 700 I=1,NGASN
                 DO 710 J=1,NGAS
*   If it overlaps with existing E, either keep old or new data.
                 IF(ABS(EGASN(I)-EGAS(J)).LE.EPSE*(ABS(EGASN(I))+
     -                ABS(EGAS(J))))THEN
                      IF(REPOLD)THEN
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Replacing existing data for E = ',
     -                          EGAS(J)*PGAS,' V/cm by data from file.'
                           INDEXE=J
                           GOTO 730
                      ELSE
                           PRINT *,' ------ GASMRG MESSAGE :'//
     -                          ' Keeping existing data for E = ',
     -                          EGAS(J)*PGAS,' V/cm, not using data',
     -                          ' from the file.'
                           GOTO 700
                      ENDIF
*   Otherwise shift all data at higher E values.
                 ELSEIF(EGASN(I).LT.EGAS(J))THEN
                      IF(NGAS+1.GT.MXLIST)GOTO 3010
                      IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                     '' DEBUG   : Inserting E = '',E15.8,
     -                     '' V/cm at slot '',I5)') EGASN(I)*PGAS,J
                      DO 720 M=NGAS,J,-1
                      EGAS(M+1)=EGAS(M)
                      VGAS(M+1)=VGAS(M)
                      XGAS(M+1)=XGAS(M)
                      YGAS(M+1)=YGAS(M)
                      DGAS(M+1)=DGAS(M)
                      OGAS(M+1)=OGAS(M)
                      AGAS(M+1)=AGAS(M)
                      AORIG(M+1)=AORIG(M)
                      BGAS(M+1)=BGAS(M)
                      MGAS(M+1)=MGAS(M)
                      WGAS(M+1)=WGAS(M)
                      DO 725 N=1,6
                      SGAS(M+1,N)=SGAS(M,N)
725                   CONTINUE
                      HGAS(M+1)=HGAS(M)
                      DO 726 N=1,NEXGAS
                      EXGAS(M+1,N)=EXGAS(M,N)
726                   CONTINUE
                      DO 727 N=1,NIOGAS
                      IOGAS(M+1,N)=IOGAS(M,N)
727                   CONTINUE
                      NEWE(M+1)=NEWE(M)
720                   CONTINUE
                      NGAS=NGAS+1
                      INDEXE=J
                      GOTO 730
                 ENDIF
*   Next old value.
710              CONTINUE
*   If there is no higher E, then add the line at the end.
                 IF(NGAS+1.GT.MXLIST)GOTO 3010
                 NGAS=NGAS+1
                 INDEXE=NGAS
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG'',
     -                '' DEBUG   : Adding E = '',E15.8,
     -                '' V/cm at end '',I5)') EGASN(I)*PGAS
*   Zero the new row.
730              CONTINUE
                 EGAS(INDEXE)=EGASN(I)
                 NEWE(INDEXE)=.TRUE.
                 VGAS(INDEXE)=0
                 XGAS(INDEXE)=0
                 YGAS(INDEXE)=0
                 DGAS(INDEXE)=0
                 OGAS(INDEXE)=0
                 AGAS(INDEXE)=0
                 AORIG(INDEXE)=0
                 BGAS(INDEXE)=0
                 MGAS(INDEXE)=0
                 WGAS(INDEXE)=0
                 DO 735 N=1,6
                 SGAS(INDEXE,N)=0
735              CONTINUE
                 HGAS(INDEXE)=0
                 DO 736 N=1,NEXGAS
                 EXGAS(INDEXE,N)=0
736              CONTINUE
                 DO 737 N=1,NIOGAS
                 IOGAS(INDEXE,N)=0
737              CONTINUE
*   Next new value.
700              CONTINUE
            ENDIF
       ENDIF
*** Read the gas composition and check.
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (GASFRN(I),I=1,MXNBMC)
       DO 743 I=1,MXNBMC
       IF(ABS(GASFRN(I)-GASFRM(I)).GT.
     -      1.0E-6*(1+ABS(GASFRN(I))+ABS(GASFRM(I))))THEN
            PRINT *,' !!!!!! GASMRG WARNING : Gas composition'//
     -           ' differs at molecule ',I,'; file not read.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
743    CONTINUE
*** Check that the excitations and ionisations match.
       DO 741 I=1,NEXGSN
*   Version 12 update
       read(12,'(19x,a45,2x,4e15.8)',iostat=ios,err=2010)
     -      desc,enexg(i),penprb(i),penrms(i),pendt(i)
C       READ(12,'(19X,A)',IOSTAT=IOS,ERR=2010) DESC
       CALL GASIDE(IEXC,DESC,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GASMRG WARNING : Excitation'//
     -           ' description ',DESC,' not found.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ELSE
            LOOKEX(I)=IEXC
       ENDIF
741    CONTINUE
       DO 742 I=1,NIOGSN
*   Version 12 update
       read(12,'(19x,a45,2x,e15.8)',iostat=ios,err=2010)
     -      desc,eniog(i)
C       READ(12,'(19X,A)',IOSTAT=IOS,ERR=2010) DESC
       CALL GASIDI(IION,DESC,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GASMRG WARNING : Ionisation'//
     -           ' description ',DESC,' not found.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ELSE
            LOOKIO(I)=IION
       ENDIF
742    CONTINUE
*** Skip the table header line.
       READ(12,'(1X)',END=2000,IOSTAT=IOS,ERR=2010)
*** Read a table written in 3D format (output always 3D).
       IF(TAB2DN)THEN
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Reading a 3D table.'')')
*   Loop over the table.
            DO 610 I=1,NGASN
            DO 620 J=1,NBANGN
            DO 630 K=1,NBTABN
*   Read the record.
            READ(12,'(8E15.8)',IOSTAT=IOS,ERR=2010,END=2000)
     -           VGASN,XGASN,YGASN,DGASN,OGASN,AGASN,AORIN,BGASN,MGASN,
     -           WGASN,HGASN,(SGASN(N),N=1,6),(EXGASN(N),N=1,NEXGSN),
     -           (IOGASN(N),N=1,NIOGSN)
*   Locate the index at which these values are to be stored.
            INDEXE=0
            DO 640 L=1,NGAS
            IF(ABS(EGASN(I)-EGAS(L)).LE.EPSE*(ABS(EGASN(I))+
     -           ABS(EGAS(L))))INDEXE=L
640         CONTINUE
            INDEXA=0
            DO 650 L=1,NBANG
            IF(ABS(BANGN(J)-BANG(L)).LE.EPSA*(ABS(BANGN(J))+
     -           ABS(BANG(L))))INDEXA=L
650         CONTINUE
            INDEXB=0
            DO 660 L=1,NBTAB
            IF(ABS(BTABN(K)-BTAB(L)).LE.EPSB*(ABS(BTABN(K))+
     -           ABS(BTAB(L))))INDEXB=L
660         CONTINUE
            IF(INDEXE.EQ.0.OR.INDEXA.EQ.0.OR.INDEXB.EQ.0)THEN
                 PRINT *,' !!!!!! GASMRG WARNING : Unable to locate'//
     -                ' the (E,angle,B) insertion point; no gas data'//
     -                ' read.'
                 CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
                 RETURN
            ENDIF
*   Store the data.
            IF(GASOKN( 1).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 1)))
     -           VGAS2(INDEXE,INDEXA,INDEXB)=VGASN
            IF(GASOKN( 2).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 2)))
     -           MGAS2(INDEXE,INDEXA,INDEXB)=MGASN
            IF(GASOKN( 3).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 3)))
     -           DGAS2(INDEXE,INDEXA,INDEXB)=DGASN
            IF(GASOKN( 4).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 4)))THEN
                 AGAS2(INDEXE,INDEXA,INDEXB)=AGASN
                 AORIG2(INDEXE,INDEXA,INDEXB)=AORIN
            ENDIF
            IF(GASOKN( 6).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 6)))
     -           BGAS2(INDEXE,INDEXA,INDEXB)=BGASN
            IF(GASOKN( 7).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 7)))
     -           WGAS2(INDEXE,INDEXA,INDEXB)=WGASN
            IF(GASOKN( 8).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 8)))
     -           OGAS2(INDEXE,INDEXA,INDEXB)=OGASN
            IF(GASOKN( 9).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 9)))
     -           XGAS2(INDEXE,INDEXA,INDEXB)=XGASN
            IF(GASOKN(10).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(10)))
     -           YGAS2(INDEXE,INDEXA,INDEXB)=YGASN
            IF(GASOKN(11).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(11)))THEN
                 DO 635 N=1,6
                 SGAS2(INDEXE,INDEXA,INDEXB,N)=SGASN(N)
635              CONTINUE
            ENDIF
            IF(GASOKN(12).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(12)))
     -           HGAS2(INDEXE,INDEXA,INDEXB)=HGASN
            IF(GASOKN(15).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(15)))THEN
                 DO 636 N=1,NEXGSN
                 EXGAS2(INDEXE,INDEXA,INDEXB,LOOKEX(N))=EXGASN(N)
636              CONTINUE
            ENDIF
            IF(GASOKN(16).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -           NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(16)))THEN
                 DO 637 N=1,NIOGSN
                 IOGAS2(INDEXE,INDEXA,INDEXB,LOOKIO(N))=IOGASN(N)
637              CONTINUE
            ENDIF
630         CONTINUE
620         CONTINUE
610         CONTINUE
**  In case of full overlap, update the GASOK bits.
            IF(IEMODE+IAMODE+IBMODE.EQ.3)THEN
                 DO 500 I=1,12
                 IF(I.EQ.5)GOTO 500
                 IF(GASOKN(I))GASOK(I)=.TRUE.
500              CONTINUE
            ENDIF
*** Read a table written in 1D mode.
       ELSE
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Reading a 1D table.'')')
*   Read the tables proper.
            DO 750 I=1,NGASN
            READ(12,'(8E15.8)',END=2000,IOSTAT=IOS,ERR=2010)
     -           VGASN,CVGASN,XGASN,CXGASN,YGASN,CYGASN,
     -           DGASN,CDGASN,OGASN,COGASN,AGASN,CAGASN,AORIN,
     -           BGASN,CBGASN,MGASN,CMGASN,WGASN,CWGASN,
     -           HGASN,CHGASN,
     -           (SGASN(N),CSGASN(N),N=1,6),
     -           (EXGASN(N),CEXGSN(N),N=1,NEXGSN),
     -           (IOGASN(N),CIOGSN(N),N=1,NIOGSN)
*   Locate the index at which these values are to be stored.
            INDEXE=0
            DO 760 L=1,NGAS
            IF(ABS(EGASN(I)-EGAS(L)).LE.EPSE*(ABS(EGASN(I))+
     -           ABS(EGAS(L))))INDEXE=L
760         CONTINUE
            INDEXA=0
            DO 770 L=1,NBANG
            IF(ABS(BANGN(1)-BANG(L)).LE.EPSA*(ABS(BANGN(1))+
     -           ABS(BANG(L))))INDEXA=L
770         CONTINUE
            INDEXB=0
            DO 780 L=1,NBTAB
            IF(ABS(BTABN(1)-BTAB(L)).LE.EPSB*(ABS(BTABN(1))+
     -           ABS(BTAB(L))))INDEXB=L
780         CONTINUE
            IF(INDEXE.EQ.0.OR.INDEXA.EQ.0.OR.INDEXB.EQ.0)THEN
                 PRINT *,' !!!!!! GASMRG WARNING : Unable to locate'//
     -                ' the (E,angle,B) insertion point; no gas data'//
     -                ' read.'
                 CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
                 RETURN
            ENDIF
*   Store the data.
            IF(TAB2D)THEN
                 IF(GASOKN( 1).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 1)))
     -                VGAS2(INDEXE,INDEXA,INDEXB)=VGASN
                 IF(GASOKN( 2).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 2)))
     -                MGAS2(INDEXE,INDEXA,INDEXB)=MGASN
                 IF(GASOKN( 3).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 3)))
     -                DGAS2(INDEXE,INDEXA,INDEXB)=DGASN
                 IF(GASOKN( 4).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 4)))THEN
                      AGAS2(INDEXE,INDEXA,INDEXB)=AGASN
                      AORIG2(INDEXE,INDEXA,INDEXB)=AORIN
                 ENDIF
                 IF(GASOKN( 6).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 6)))
     -                BGAS2(INDEXE,INDEXA,INDEXB)=BGASN
                 IF(GASOKN( 7).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 7)))
     -                WGAS2(INDEXE,INDEXA,INDEXB)=WGASN
                 IF(GASOKN( 8).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 8)))
     -                OGAS2(INDEXE,INDEXA,INDEXB)=OGASN
                 IF(GASOKN( 9).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK( 9)))
     -                XGAS2(INDEXE,INDEXA,INDEXB)=XGASN
                 IF(GASOKN(10).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(10)))
     -                YGAS2(INDEXE,INDEXA,INDEXB)=YGASN
                 IF(GASOKN(11).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(11)))THEN
                      DO 785 N=1,6
                      SGAS2(INDEXE,INDEXA,INDEXB,N)=SGASN(N)
785                   CONTINUE
                 ENDIF
                 IF(GASOKN(12).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(12)))
     -                HGAS2(INDEXE,INDEXA,INDEXB)=HGASN
                 IF(GASOKN(15).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(15)))THEN
                      DO 786 N=1,NEXGSN
                      EXGAS2(INDEXE,INDEXA,INDEXB,LOOKEX(N))=EXGASN(N)
786                   CONTINUE
                 ENDIF
                 IF(GASOKN(16).AND.(NEWE(INDEXE).OR.NEWA(INDEXA).OR.
     -                NEWB(INDEXB).OR.REPOLD.OR..NOT.GASOK(16)))THEN
                      DO 787 N=1,NIOGSN
                      IOGAS2(INDEXE,INDEXA,INDEXB,LOOKIO(N))=IOGASN(N)
787                   CONTINUE
                 ENDIF
            ELSE
                 IF(GASOKN( 1).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 1)))VGAS(INDEXE)=VGASN
                 IF(GASOKN( 2).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 2)))MGAS(INDEXE)=MGASN
                 IF(GASOKN( 3).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 3)))DGAS(INDEXE)=DGASN
                 IF(GASOKN( 4).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 4)))AGAS(INDEXE)=AGASN
                 IF(GASOKN( 4).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 4)))AORIG(INDEXE)=AORIN
                 IF(GASOKN( 6).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 6)))BGAS(INDEXE)=BGASN
                 IF(GASOKN( 7).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 7)))WGAS(INDEXE)=WGASN
                 IF(GASOKN( 8).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 8)))OGAS(INDEXE)=OGASN
                 IF(GASOKN( 9).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK( 9)))XGAS(INDEXE)=XGASN
                 IF(GASOKN(10).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK(10)))YGAS(INDEXE)=YGASN
                 IF(GASOKN(11).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK(11)))THEN
                      DO 755 N=1,6
                      SGAS(INDEXE,N)=SGASN(N)
755                   CONTINUE
                 ENDIF
                 IF(GASOKN(12).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK(12)))HGAS(INDEXE)=HGASN
                 IF(GASOKN(15).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK(15)))THEN
                      DO 756 N=1,NEXGSN
                      EXGAS(INDEXE,LOOKEX(N))=EXGASN(N)
756                   CONTINUE
                 ENDIF
                 IF(GASOKN(16).AND.(REPOLD.OR.NEWE(INDEXE).OR.
     -                .NOT.GASOK(16)))THEN
                      DO 757 N=1,NIOGSN
                      IOGAS(INDEXE,LOOKIO(N))=IOGASN(N)
757                   CONTINUE
                 ENDIF
            ENDIF
750         CONTINUE
**  Consider extrapolation data only if REPLACE-OLD is set.
            IF(REPOLD)THEN
*   Debugging.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -                '' Replacing extrapolation data.'')')
*   Read upper extrapolation data.
                 READ(12,'(9X,13I5)',END=2000,IOSTAT=IOS,ERR=2010)
     -                IVEXTN,IXEXTN,IYEXTN,IDEXTN,IAEXTN,IBEXTN,IMEXTN,
     -                IWEXTN,IOEXTN,IHEXTN,ISEXTN,IEEXTN,IZEXTN
*   Replace where appropriate.
                 IF(GASOKN(1))IVEXTR=IVEXTN
                 IF(GASOKN(2))IMEXTR=IMEXTN
                 IF(GASOKN(3))IDEXTR=IDEXTN
                 IF(GASOKN(4))IAEXTR=IAEXTN
                 IF(GASOKN(6))IBEXTR=IBEXTN
                 IF(GASOKN(7))IWEXTR=IWEXTN
                 IF(GASOKN(8))IOEXTR=IOEXTN
                 IF(GASOKN(9))IXEXTR=IXEXTN
                 IF(GASOKN(10))IYEXTR=IYEXTN
                 IF(GASOKN(11))ISEXTR=ISEXTN
                 IF(GASOKN(12))IHEXTR=IHEXTN
                 IF(GASOKN(15))IEEXTR=IEEXTN
                 IF(GASOKN(16))IZEXTR=IZEXTN
*   Read lower extrapolation data.
                 READ(12,'(9X,13I5)',END=2000,IOSTAT=IOS,ERR=2010)
     -                JVEXTN,JXEXTN,JYEXTN,JDEXTN,JAEXTN,JBEXTN,JMEXTN,
     -                JWEXTN,JOEXTN,JHEXTN,JSEXTN,JEEXTN,JZEXTN
*   Replace where appropriate.
                 IF(GASOKN(1))JVEXTR=JVEXTN
                 IF(GASOKN(2))JMEXTR=JMEXTN
                 IF(GASOKN(3))JDEXTR=JDEXTN
                 IF(GASOKN(4))JAEXTR=JAEXTN
                 IF(GASOKN(6))JBEXTR=JBEXTN
                 IF(GASOKN(7))JWEXTR=JWEXTN
                 IF(GASOKN(8))JOEXTR=JOEXTN
                 IF(GASOKN(9))JXEXTR=JXEXTN
                 IF(GASOKN(10))JYEXTR=JYEXTN
                 IF(GASOKN(11))JSEXTR=JSEXTN
                 IF(GASOKN(12))JHEXTR=JHEXTN
                 IF(GASOKN(15))JEEXTR=JEEXTN
                 IF(GASOKN(16))JZEXTR=JZEXTN
**  Otherwise skip the records associated with this.
            ELSE
*   Debugging.
                 IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -                '' Skipping extrapolation data.'')')
*   Skip the 2 records with data.
                 READ(12,'(/)',IOSTAT=IOS,ERR=2010)
            ENDIF
**  Update the GASOK bits for merge mode.
            IF(IEMODE.NE.0)THEN
                 DO 740 I=1,16
                 IF(I.EQ.5.OR.I.EQ.13.OR.I.EQ.14)GOTO 740
                 IF(GASOKN(I))GASOK(I)=.TRUE.
740              CONTINUE
            ENDIF
       ENDIF
*** Replace interpolation methods only when REPLACE-OLD is set.
       IF(REPOLD)THEN
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing interpolation + threshold data.'')')
*   Read the Townsend and attachment threshold data.
            READ(12,'(13X,BN,3I10)',IOSTAT=IOS,ERR=2010)
     -           IATHRN,IBTHRN,IHTHRN
            IF(GASOKN(4))IATHR=IATHRN
            IF(GASOKN(6))IBTHR=IBTHRN
            IF(GASOKN(12))IHTHR=IHTHRN
*   Read the interpolation data.
            READ(12,'(9X,BN,13I5)',IOSTAT=IOS,ERR=2010)
     -           IVMETN,IXMETN,IYMETN,IDMETN,IAMETN,IBMETN,IMMETN,
     -           IWMETN,IOMETN,IHMETN,ISMETN,IEMETN,IZMETN
            IF(GASOK( 1))IVMETH=IVMETN
            IF(GASOK( 2))IMMETH=IMMETN
            IF(GASOK( 3))IDMETH=IDMETN
            IF(GASOK( 4))IAMETH=IAMETN
            IF(GASOK( 6))IBMETH=IBMETN
            IF(GASOK( 7))IWMETH=IWMETN
            IF(GASOK( 8))IOMETH=IOMETN
            IF(GASOK( 9))IXMETH=IXMETN
            IF(GASOK(10))IYMETH=IYMETN
            IF(GASOK(11))ISMETH=ISMETN
            IF(GASOK(12))IHMETH=IHMETN
            IF(GASOK(15))IEMETH=IEMETN
            IF(GASOK(16))IZMETH=IZMETN
**  Otherwise skip the record.
       ELSE
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping interpolation + threshold data.'')')
*   Skip the records.
            READ(12,'(/)',IOSTAT=IOS,ERR=2010)
       ENDIF
*** Read cluster data.
       READ(12,'(4(8X,E15.8,1X))',END=2000,IOSTAT=IOS,ERR=2010)
     -      AN,ZN,EMPRBN,EPAIRN
       IF(AN.GT.0.AND.ZN.GT.0.AND.EMPRBN.GT.0.AND.EPAIRN.GT.0.AND.
     -      REPOLD)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing A, Z, EMPROB and EPAIR.'')')
            A=AN
            Z=ZN
            EMPROB=EMPRBN
            EPAIR=EPAIRN
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping A, Z, EMPROB and EPAIR.'')')
       ENDIF
*   Ion diffusion.
       READ(12,'(16X,2E15.8)') DLIONN,DTIONN
       IF(DLIONN.GT.0.AND.DTIONN.GT.0.AND.REPOLD)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing ion diffusion.'')')
            DLION=DLIONN
            DTION=DTIONN
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping ion diffusion.'')')
       ENDIF
*   Further cluster data.
       READ(12,'(4(8X,E15.8,1X))',END=2000,IOSTAT=IOS,ERR=2010)
     -      CMEANN,RHON,PGASN,TGASN
       IF(CMEANN.GT.0.AND.RHON.GT.0.AND.REPOLD)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing mean number of clusters + density.'')')
            CMEAN=CMEANN
            RHO=RHON
            IF(SRMDEN.GT.0)PRINT *,' ------ GASMRG MESSAGE :'//
     -           ' SRIM gas density replaced by new value.'
            SRMDEN=RHO
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping mean number of clusters + density.'')')
       ENDIF
       IF(ABS(PGASN-PGAS).GT.1E-3*(ABS(PGAS)+ABS(PGASN)))THEN
            PRINT *,' ------ GASMRG MESSAGE : The gas pressure of'//
     -           ' the file differs from the currently set pressure.'
            IF(REPOLD)PGAS=PGASN
       ENDIF
       IF(ABS(TGASN-TGAS).GT.1E-3*(ABS(TGAS)+ABS(TGASN)))THEN
            PRINT *,' ------ GASMRG MESSAGE : The gas temperature of'//
     -           ' the file differs from the currently set temperature.'
            IF(REPOLD)TGAS=TGASN
       ENDIF
*   Clustering model and cluster size distribution.
       READ(12,'(13X,A10)',END=2000,IOSTAT=IOS,ERR=2010) CLSTPN
       IF(CLSTPN.NE.'NOT SET'.AND.(CLSTYP.EQ.'NOT SET'.OR.REPOLD))THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing cluster tables.'')')
            READ(12,'(13X,A80)',END=2000,IOSTAT=IOS,ERR=2010) FCNCLS
            READ(12,'(13X,BN,I10)',END=2000,IOSTAT=IOS,ERR=2010) NCLS
            READ(12,'(13X,D25.18)',END=2000,IOSTAT=IOS,ERR=2010) CLSAVE
            DO 800 J=1,NCLS,5
            READ(12,'(5D25.18)',END=2000,IOSTAT=IOS,ERR=2010)
     -           (CLSDIS(I),I=J,MIN(J+4,NCLS))
800         CONTINUE
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping cluster tables.'')')
            READ(12,'(/13X,BN,I10/)',END=2000,IOSTAT=IOS,ERR=2010) NCLSN
            DO 810 J=1,NCLSN,5
            READ(12,'(/)',END=2000,IOSTAT=IOS,ERR=2010)
810         CONTINUE
       ENDIF
*   Clustering GASOK bit.
       IF(GASOKN(5))GASOK(5)=.TRUE.
*   Heed initialisation data.
       READ(12,'(28X,L1)',END=2000,ERR=2010,IOSTAT=IOS) HEEDN
       IF(HEEDN.AND.(REPOLD.OR..NOT.HEEDOK))THEN
            BACKSPACE(12,ERR=2040,IOSTAT=IOS)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing Heed initialisation data.'')')
            CALL GASHGT(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASNRG WARNING : Reading Heed data'//
     -                ' failed ; gas data not available.'
                 RETURN
            ENDIF
       ELSEIF(HEEDN)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Reading but not using Heed initialisation data.'')')
            READ(12,'(18X,I5)',END=2000,ERR=2010,IOSTAT=IOS) NQMOL
            DO 820 I=1,NQMOL
            READ(12,'()',END=2000,ERR=2010,IOSTAT=IOS)
820         CONTINUE
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping Heed initialisation data.'')')
       ENDIF
*   SRIM initialisation data.
       READ(12,'(28X,L1)',END=2000,ERR=2010,IOSTAT=IOS) SRIMN
       IF(SRIMN.AND.(REPOLD.OR..NOT.SRIMOK))THEN
            BACKSPACE(12,ERR=2040,IOSTAT=IOS)
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Replacing SRIM initialisation data.'')')
            CALL GASSGT(IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! GASNRG WARNING : Reading SRIM data'//
     -                ' failed ; gas data not available.'
                 RETURN
            ENDIF
       ELSE
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASMRG DEBUG   :'',
     -           '' Skipping SRIM initialisation data.'')')
       ENDIF
*** Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IFAIL=0
       CALL TIMLOG('Reading the gas data from a dataset:    ')
       RETURN
*** Handle the I/O error conditions.
2000   CONTINUE
       PRINT *,' ###### GASMRG ERROR   : EOF encountered while'//
     -      ' reading '//FILE(1:NCFILE)//' via unit 12 ;'//
     -      ' no gas data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### GASMRG ERROR   : Error while reading '//
     -      FILE(1:NCFILE)//' via unit 12 ; no gas data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### GASMRG ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' on unit 12 cannot be closed ; results not predictable.'
       CALL INPIOS(IOS)
       RETURN
2040   CONTINUE
       PRINT *,' ###### GASMRG ERROR   : Error while backspacing on '//
     -      FILE(1:NCFILE)//' via unit 12 ; no gas data read.'
       CALL INPIOS(IOS)
       RETURN
*** Handle dimension errors.
3010   CONTINUE
       PRINT *,' !!!!!! GASMRG WARNING : Number of E fields in the'//
     -      ' table grows beyond MXLIST; reading the file is aborted.'
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
3020   CONTINUE
       PRINT *,' !!!!!! GASMRG WARNING : Number of B fields in the'//
     -      ' table grows beyond MXBTAB; reading the file is aborted.'
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
3030   CONTINUE
       PRINT *,' !!!!!! GASMRG WARNING : Number of E,B angles in the'//
     -      ' table grows beyond MXBANG; reading the file is aborted.'
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IFAIL=1
       RETURN
       END
