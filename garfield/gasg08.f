CDECK  ID>, GASG08.
       SUBROUTINE GASG08(IFAIL)
*-----------------------------------------------------------------------
*   GASG08 - This routine retrieves the gas data written to an external
*            dataset written by a WRITE instruction. For format 8.
*   VARIABLES : NWORD       : Number of parameters provided.
*               STRING      : String for character manipulation.
*   (Last changed on 27/ 6/07, adapted until 18/ 9/11.)
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
       CHARACTER*(MXCHAR) STRING
       CHARACTER*8 MEMBER
       CHARACTER*(MXNAME) FILE
       INTEGER IFAIL,NCFILE,NCMEMB,I,II,J,K,L,IOS,IFAIL1,NWORD
       LOGICAL DSNCMP,EXIS
       EXTERNAL DSNCMP
*** Identify the routine.
       IF(LIDENT)PRINT *,' /// ROUTINE GASG08 ///'
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
*   Check the various lengths.
       IF(NCFILE.GT.MXNAME)THEN
            PRINT *,' !!!!!! GASG08 WARNING : The file name is'//
     -           ' truncated to MXNAME (=',MXNAME,') characters.'
            NCFILE=MIN(NCFILE,MXNAME)
       ENDIF
       IF(NCMEMB.GT.8)THEN
            PRINT *,' !!!!!! GASG08 WARNING : The member name is'//
     -           ' shortened to ',MEMBER,', first 8 characters.'
            NCMEMB=MIN(NCMEMB,8)
       ELSEIF(NCMEMB.LE.0)THEN
            PRINT *,' !!!!!! GASG08 WARNING : The member'//
     -           ' name has zero length, replaced by "*".'
            MEMBER='*'
            NCMEMB=1
       ENDIF
*   Reject the empty file name case.
       IF(FILE.EQ.' '.OR.NWORD.EQ.1)THEN
            PRINT *,' !!!!!! GASG08 WARNING : GET must be at least'//
     -           ' followed by a dataset name ; no data are read.'
            RETURN
       ENDIF
*   If there are even more args, warn they are ignored.
       IF(NWORD.GT.3)PRINT *,' !!!!!! GASG08 WARNING : GET takes'//
     -     ' at most two arguments (dataset and member); rest ignored.'
*** Open a dataset and inform DSNLOG.
       CALL DSNOPN(FILE,NCFILE,12,'READ-LIBRARY',IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GASG08 WARNING : Opening ',FILE(1:NCFILE),
     -           ' failed ; gas data are not read.'
            RETURN
       ENDIF
       CALL DSNLOG(FILE,'Gas data  ','Sequential','Read only ')
       IF(LDEBUG)PRINT *,' ++++++ GASG08 DEBUG    : Dataset ',
     -      FILE(1:NCFILE),' opened on unit 12 for seq read.'
*   Locate the pointer on the header of the requested member.
       CALL DSNLOC(MEMBER,NCMEMB,'GAS     ',12,EXIS,'RESPECT')
       IF(.NOT.EXIS)THEN
            CALL DSNLOC(MEMBER,NCMEMB,'GAS     ',12,EXIS,'IGNORE')
            IF(EXIS)THEN
                 PRINT *,' ###### GASG08 ERROR   : Gas description '//
     -                MEMBER(1:NCMEMB)//' has been deleted from '//
     -                FILE(1:NCFILE)//'; not read.'
            ELSE
                 PRINT *,' ###### GASG08 ERROR   : Gas description '//
     -                MEMBER(1:NCMEMB)//' not found in '//FILE(1:NCFILE)
            ENDIF
            CLOSE(UNIT=12,IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   Check that this member contains indeed gas data.
       READ(12,'(A80)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(LDEBUG)THEN
            PRINT *,' ++++++ GASG08 DEBUG    : Dataset header'//
     -           ' record follows:'
            PRINT *,STRING
       ENDIF
       WRITE(*,'(''  Member '',A8,'' was created on '',A8,
     -      '' at '',A8/''  Remarks: '',A29)')
     -      STRING(32:39),STRING(11:18),STRING(23:30),STRING(51:79)
*   Check the version.
       READ(12,'(A14)',END=2000,IOSTAT=IOS,ERR=2010) STRING
       IF(STRING(1:14).NE.' Version   : 8')THEN
            PRINT *,' !!!!!! GASG08 WARNING : This member'//
     -           ' can not be read because of a change in format.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*** Read the rest of the dataset.
       READ(12,'(13X,12L1)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (GASOK(I),I=1,12)
       READ(12,'(13X,A)',END=2000,IOSTAT=IOS,ERR=2010) GASID
       READ(12,'(13X,A80)',END=2000,IOSTAT=IOS,ERR=2010) FCNTAB
       READ(12,'(13X,L1,3I10)',END=2000,IOSTAT=IOS,ERR=2010)
     -      TAB2D,NGAS,NBANG,NBTAB
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (EGAS(I),I=1,NGAS)
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (BANG(I),I=1,NBANG)
       READ(12,'(/(5E15.8))',IOSTAT=IOS,ERR=2010,END=2000)
     -      (BTAB(I),I=1,NBTAB)
*   Skip the header line.
       READ(12,*,END=2000,IOSTAT=IOS,ERR=2010)
*   Check the number of E points.
       IF(NGAS.LE.0.OR.NGAS.GT.MXLIST)THEN
            PRINT *,' !!!!!! GASG08 WARNING : Number of gas points in',
     -           ' dataset ',FILE(1:NCFILE),' out of range: ',NGAS
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
*   Check the number of angles.
       ELSEIF(TAB2D.AND.(NBANG.LE.0.OR.NBANG.GT.MXBANG))THEN
            PRINT *,' !!!!!! GASG08 WARNING : Number of E-B angles in',
     -           ' dataset ',FILE(1:NCFILE),' out of range: ',NBANG
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
*   Check the number of B points.
       ELSEIF(TAB2D.AND.(NBTAB.LE.0.OR.NBTAB.GT.MXBTAB))THEN
            PRINT *,' !!!!!! GASG08 WARNING : Number of B fields in',
     -           ' dataset ',FILE(1:NCFILE),' out of range: ',NBTAB
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
*   Read a 2-dimensional table.
       ELSEIF(TAB2D)THEN
            DO 210 I=1,NGAS
            DO 220 J=1,NBANG
            DO 230 K=1,NBTAB
            READ(12,'(8E15.8/8E15.8)',IOSTAT=IOS,ERR=2010,END=2000)
     -           VGAS2(I,J,K),XGAS2(I,J,K),YGAS2(I,J,K),DGAS2(I,J,K),
     -           OGAS2(I,J,K),AGAS2(I,J,K),BGAS2(I,J,K),MGAS2(I,J,K),
     -           WGAS2(I,J,K),HGAS2(I,J,K),(SGAS2(I,J,K,L),L=1,6)
            AORIG2(I,J,K)=AGAS2(I,J,K)
230         CONTINUE
220         CONTINUE
210         CONTINUE
*   Read a 1-dimensional table.
       ELSE
            DO 212 I=1,NGAS
            READ(12,'(8E15.8/8E15.8/8E15.8/8E15.8)',END=2000,
     -           IOSTAT=IOS,ERR=2010)
     -           VGAS(I),CVGAS(I),XGAS(I),CXGAS(I),YGAS(I),CYGAS(I),
     -           DGAS(I),CDGAS(I),OGAS(I),COGAS(I),
     -           AGAS(I),CAGAS(I),BGAS(I),CBGAS(I),MGAS(I),CMGAS(I),
     -           WGAS(I),CWGAS(I),HGAS(I),CHGAS(I),
     -           (SGAS(I,L),CSGAS(I,L),L=1,6)
            AORIG(I)=AGAS(I)
212         CONTINUE
            READ(12,'(9X,10(/I2,2E15.8)/I2/6E15.8/6E15.8)',
     -           END=2000,IOSTAT=IOS,ERR=2010)
     -           IVEXTR,VEXTR1,VEXTR2,
     -           IXEXTR,XEXTR1,XEXTR2,IYEXTR,YEXTR1,YEXTR2,
     -           IDEXTR,DEXTR1,DEXTR2,
     -           IAEXTR,AEXTR1,AEXTR2,IBEXTR,BEXTR1,BEXTR2,
     -           IMEXTR,MEXTR1,MEXTR2,IWEXTR,WEXTR1,WEXTR2,
     -           IOEXTR,OEXTR1,OEXTR2,IHEXTR,HEXTR1,HEXTR2,
     -           ISEXTR,(SEXTR1(L),SEXTR2(L),L=1,6)
            READ(12,'(9X,10(/I2,2E15.8)/I2/6E15.8/6E15.8)',
     -           END=2000,IOSTAT=IOS,ERR=2010)
     -           JVEXTR,VEXTR3,VEXTR4,
     -           JXEXTR,XEXTR3,XEXTR4,JYEXTR,YEXTR3,YEXTR4,
     -           JDEXTR,DEXTR3,DEXTR4,
     -           JAEXTR,AEXTR3,AEXTR4,JBEXTR,BEXTR3,BEXTR4,
     -           JMEXTR,MEXTR3,MEXTR4,JWEXTR,WEXTR3,WEXTR4,
     -           JOEXTR,OEXTR3,OEXTR4,JHEXTR,HEXTR3,HEXTR4,
     -           JSEXTR,(SEXTR3(L),SEXTR4(L),L=1,6)
       ENDIF
*   Read interpolation methods.
       READ(12,'(13X,BN,3I10)',IOSTAT=IOS,ERR=2010) IATHR,IBTHR,IHTHR
       READ(12,'(9X,BN,11I10)',IOSTAT=IOS,ERR=2010)
     -      IVMETH,IXMETH,IYMETH,IDMETH,IAMETH,IBMETH,IMMETH,
     -      IWMETH,IOMETH,IHMETH,ISMETH
*   Read cluster data.
       READ(12,'(4(8X,E15.8,1X))',END=2000,IOSTAT=IOS,ERR=2010)
     -      A,Z,EMPROB,EPAIR
*   Ion diffusion.
       READ(12,'(16X,2E15.8)') DLION,DTION
*   Further cluster data.
       READ(12,'(4(8X,E15.8,1X))',END=2000,IOSTAT=IOS,ERR=2010)
     -      CMEAN,RHO,PGAS,TGAS
*   Clustering model and cluster size distribution.
       READ(12,'(13X,A10)',END=2000,IOSTAT=IOS,ERR=2010) CLSTYP
       READ(12,'(13X,A80)',END=2000,IOSTAT=IOS,ERR=2010) FCNCLS
       READ(12,'(13X,BN,I10)',END=2000,IOSTAT=IOS,ERR=2010) NCLS
       READ(12,'(13X,D25.18)',END=2000,IOSTAT=IOS,ERR=2010) CLSAVE
       DO 240 II=1,NCLS,5
       READ(12,'(5D25.18)',END=2000,IOSTAT=IOS,ERR=2010)
     -      (CLSDIS(I),I=II,MIN(II+4,NCLS))
240    CONTINUE
*   Heed initialisation data.
       CALL GASHGT(IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! GASG08 WARNING : Reading Heed data'//
     -           ' failed ; gas data not available.'
            CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
            RETURN
       ENDIF
*   SRIM initialisation data.
       CALL GASSG8(IFAIL1)
       PRINT *,' !!!!!! GASG08 WARNING : SRIM data from version 8'//
     -      ' is incomplete; SRIM disabled, re-read SRIM file.'
       SRIMOK=.FALSE.
*** This version contains no excitations and ionisations
       DO 300 I=15,20
       GASOK(I)=.FALSE.
300    CONTINUE
       NEXGAS=0
       NIOGAS=0
*** Close the file after the operation.
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       IFAIL=0
       CALL TIMLOG('Reading the gas data from a dataset:    ')
       RETURN
*** Handle the I/O error conditions.
2000   CONTINUE
       PRINT *,' ###### GASG08 ERROR   : EOF encountered while reading',
     -      ' '//FILE(1:NCFILE)//' from unit 12 ; no gas data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2010   CONTINUE
       PRINT *,' ###### GASG08 ERROR   : Error while reading'//
     -      ' '//FILE(1:NCFILE)//' from unit 12 ; no gas data read.'
       CALL INPIOS(IOS)
       CLOSE(UNIT=12,STATUS='KEEP',IOSTAT=IOS,ERR=2030)
       RETURN
2030   CONTINUE
       PRINT *,' ###### GASG08 ERROR   : Dataset '//FILE(1:NCFILE)//
     -      ' on unit 12 cannot be closed ; results not predictable.'
       CALL INPIOS(IOS)
       END
