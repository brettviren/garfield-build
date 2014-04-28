CDECK  ID>, GASCHK.
       SUBROUTINE GASCHK(IFAIL)
*-----------------------------------------------------------------------
*   GASCHK - Checks the validity of tha gas data entered in GASINP.
*   VARIABLES : IFAIL        : 1 if routine failed 0 if succesful
*   (Last changed on  2/ 8/10.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       DOUBLE PRECISION SUMCLS
       INTEGER IFAIL,I,J,K,L
       LOGICAL OK
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GASCHK ///'
*** Preset IFAIL to 0, i.e. pass.
       IFAIL=0
       OK=.TRUE.
*** Table check: check the number of data points.
       IF(NGAS.LT.1)THEN
            PRINT *,' !!!!!! GASCHK WARNING : The electron transport'//
     -           ' properties table is empty.'
            GASOK(1)=.FALSE.
            GASOK(2)=.FALSE.
            GASOK(3)=.FALSE.
            GASOK(4)=.FALSE.
            GASOK(6)=.FALSE.
            GASOK(7)=.FALSE.
            GASOK(8)=.FALSE.
            GASOK(9)=.FALSE.
            GASOK(10)=.FALSE.
            GASOK(11)=.FALSE.
            GASOK(12)=.FALSE.
            GASOK(15)=.FALSE.
            GASOK(16)=.FALSE.
            OK=.FALSE.
       ENDIF
       DO 10 I=1,NGAS
*   Check that the E/p array is all positive.
       IF(EGAS(I).LE.0.0)THEN
            PRINT *,' !!!!!! GASCHK WARNING : E/p is not strictly',
     -           ' positive in table entry ',I,'; table is rejected.'
            GASOK(1)=.FALSE.
            GASOK(2)=.FALSE.
            GASOK(3)=.FALSE.
            GASOK(4)=.FALSE.
            GASOK(6)=.FALSE.
            GASOK(7)=.FALSE.
            GASOK(8)=.FALSE.
            GASOK(9)=.FALSE.
            GASOK(10)=.FALSE.
            GASOK(11)=.FALSE.
            GASOK(12)=.FALSE.
            GASOK(15)=.FALSE.
            GASOK(16)=.FALSE.
            OK=.FALSE.
       ENDIF
*   Check that the E/p array is in increasing order.
       IF(I.GT.1)THEN
            IF(EGAS(I).LE.EGAS(I-1))THEN
                 PRINT *,' !!!!!! GASCHK WARNING : E/p is not in'//
     -                ' increasing order; table is rejected.'
                 GASOK(1)=.FALSE.
                 GASOK(2)=.FALSE.
                 GASOK(3)=.FALSE.
                 GASOK(4)=.FALSE.
                 GASOK(6)=.FALSE.
                 GASOK(7)=.FALSE.
                 GASOK(8)=.FALSE.
                 GASOK(9)=.FALSE.
                 GASOK(10)=.FALSE.
                 GASOK(11)=.FALSE.
                 GASOK(12)=.FALSE.
                 GASOK(15)=.FALSE.
                 GASOK(16)=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
**  Case of a 2 dimensional table.
       IF(TAB2D)THEN
            DO 20 J=1,NBANG
            DO 30 K=1,NBTAB
*   Check that the v || E is all positive (leave other components).
            IF(GASOK(1).AND.VGAS2(I,J,K).LE.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : v || E = ',
     -                VGAS2(I,J,K),' at entry ',I,J,K,', not > 0 ;',
     -                ' v || E is rejected.'
                 GASOK(1)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Check that the ion mobility is all positive.
            IF(GASOK(2).AND.MGAS2(I,J,K).LE.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Ion mobility < 0',
     -                ' at table entry ',I,J,K,'; mobility rejected.'
                 GASOK(2)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Check that the sigma-diffusion array is all positive.
            IF(GASOK(3).AND.DGAS2(I,J,K).LT.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Long. diffusion < 0',
     -                ' at table entry ',I,J,K,'; data are rejected.'
                 GASOK(3)=.FALSE.
                 OK=.FALSE.
            ENDIF
            IF(GASOK(8).AND.OGAS2(I,J,K).LT.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Tr. diffusion < 0',
     -                ' at table entry ',I,J,K,'; data are rejected.'
                 OK=.FALSE.
                 GASOK(8)=.FALSE.
            ENDIF
            IF(GASOK(11).AND.(SGAS2(I,J,K,1).LT.0.0.OR.
     -           SGAS2(I,J,K,2).LT.0.0.OR.
     -           SGAS2(I,J,K,3).LT.0.0))THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Diagonal element(s)',
     -                ' of diffusion tensor < 0 at table entry ',
     -                I,J,K,'; data are rejected.'
                 OK=.FALSE.
                 GASOK(11)=.FALSE.
            ENDIF
*   Check that the Townsend coefficients are all reasonable.
            IF(GASOK(4).AND.AGAS2(I,J,K).LT.-30.001)THEN
                 PRINT *,' ------ GASCHK MESSAGE : Setting alpha/p ='//
     -                ' 0 in table entry ',I,J,K,'.'
                 AGAS2(I,J,K)=-30
            ENDIF
*   Check that the attachment coefficients are all positive.
            IF(GASOK(6).AND.BGAS2(I,J,K).LT.-30.001)THEN
                 PRINT *,' ------ GASCHK MESSAGE : Setting eta/p ='//
     -                ' 0 in table entry ',I,J,K,'.'
                 BGAS2(I,J,K)=-30
            ENDIF
*   Check that the attachment coefficients are all positive.
            IF(GASOK(12).AND.HGAS2(I,J,K).LT.-30.001)THEN
                 PRINT *,' ------ GASCHK MESSAGE : Setting diss/p ='//
     -                ' 0 in table entry ',I,J,K,'.'
                 HGAS2(I,J,K)=-30
            ENDIF
*   Check the excitation rates
            IF(GASOK(15))THEN
                 DO 40 L=1,NEXGAS
                 IF(EXGAS2(I,J,K,L).LT.0.0)THEN
                      PRINT *,' !!!!!! GASCHK WARNING : Excitation ',L,
     -                     ' has negative rate at table entry ',I,J,K,
     -                     '; data are rejected.'
                      OK=.FALSE.
                      GASOK(15)=.FALSE.
                 ENDIF
40               CONTINUE
            ENDIF
*   Check the ionisation rates
            IF(GASOK(16))THEN
                 DO 50 L=1,NIOGAS
                 IF(IOGAS2(I,J,K,L).LT.0.0)THEN
                      PRINT *,' !!!!!! GASCHK WARNING : Ionisation ',L,
     -                     ' has negative rate at table entry ',I,J,K,
     -                     '; data are rejected.'
                      OK=.FALSE.
                      GASOK(16)=.FALSE.
                 ENDIF
50               CONTINUE
            ENDIF
30          CONTINUE
20          CONTINUE
**  Case of a 1-dimensional table.
       ELSE
*   Check that the v || E is all positive (leave other components).
            IF(GASOK(1).AND.VGAS(I).LE.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : v || E is not > 0'//
     -                ' in table entry ',I,'; Vdrift is rejected.'
                 OK=.FALSE.
                 GASOK(1)=.FALSE.
            ENDIF
*   Check that the ion mobility is all positive.
            IF(GASOK(2).AND.MGAS(I).LE.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Ion mobility < 0',
     -                ' at table entry ',I,'; ion mobility rejected.'
                 OK=.FALSE.
                 GASOK(2)=.FALSE.
            ENDIF
*   Check that the sigma-diffusion array is all positive.
            IF(GASOK(3).AND.DGAS(I).LT.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Long. diffusion < 0',
     -                ' at table entry ',I,'; data are rejected.'
                 OK=.FALSE.
                 GASOK(3)=.FALSE.
            ENDIF
            IF(GASOK(8).AND.OGAS(I).LT.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Tr. diffusion < 0',
     -                ' at table entry ',I,'; data are rejected.'
                 GASOK(8)=.FALSE.
                 OK=.FALSE.
            ENDIF
            IF(GASOK(11).AND.(SGAS(I,1).LT.0.0.OR.SGAS(I,2).LT.0.0.OR.
     -           SGAS(I,3).LT.0.0))THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Diagonal element(s)',
     -                ' of diffusion tensor < 0 at table entry ',I,
     -                '; data are rejected.'
                 GASOK(11)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Check that the Townsend coefficients are reasonable.
            IF(GASOK(4).AND.AGAS(I).LT.-30.001)THEN
                 PRINT *,' ------ GASCHK MESSAGE : Setting alpha/p ='//
     -                ' 0 in table entry ',I,'.'
                 AGAS(I)=-30
            ENDIF
*   Check that the attachment coefficients are all positive.
            IF(GASOK(6).AND.BGAS(I).LT.-30.001)THEN
                 PRINT *,' ------ GASCHK MESSAGE : Setting eta/p ='//
     -                ' 0 in table entry ',I,'.'
                 BGAS(I)=-30
            ENDIF
*   Check that the attachment coefficients are all positive.
            IF(GASOK(12).AND.HGAS(I).LT.-30.001)THEN
                 PRINT *,' ------ GASCHK MESSAGE : Setting diss/p ='//
     -                ' 0 in table entry ',I,'.'
                 HGAS(I)=-30
            ENDIF
*   Check the excitation rates
            IF(GASOK(15))THEN
                 DO 60 L=1,NEXGAS
                 IF(EXGAS(I,L).LT.0.0)THEN
                      PRINT *,' !!!!!! GASCHK WARNING : Excitation ',L,
     -                     ' has negative rate at table entry ',I,
     -                     '; data are rejected.'
                      OK=.FALSE.
                      GASOK(15)=.FALSE.
                 ENDIF
60               CONTINUE
            ENDIF
*   Check the ionisation rates
            IF(GASOK(16))THEN
                 DO 70 L=1,NIOGAS
                 IF(IOGAS(I,L).LT.0.0)THEN
                      PRINT *,' !!!!!! GASCHK WARNING : Ionisation ',L,
     -                     ' has negative rate at table entry ',I,
     -                     '; data are rejected.'
                      OK=.FALSE.
                      GASOK(16)=.FALSE.
                 ENDIF
70               CONTINUE
            ENDIF
       ENDIF
10     CONTINUE
*** Check the transfer probabilities.
       IF(GASOK(15))THEN
            DO 90 L=1,NEXGAS
            IF(PENPRB(L).LT.0.0.OR.PENPRB(L).GT.1.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Excitation ',L,
     -                ' has a transfer probability outside [0,1];'//
     -                ' data are rejected.'
                 OK=.FALSE.
                 GASOK(15)=.FALSE.
            ENDIF
            IF(PENRMS(L).LT.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Excitation ',L,
     -                ' has a transfer distance RMS < 0;;'//
     -                ' data are rejected.'
                 OK=.FALSE.
                 GASOK(15)=.FALSE.
            ENDIF
            IF(PENDT(L).LT.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : Excitation ',L,
     -                ' has a time delay < 0;;'//
     -                ' data are rejected.'
                 OK=.FALSE.
                 GASOK(15)=.FALSE.
            ENDIF
90          CONTINUE
       ENDIF
*** Check interpolation and extrapolation methods.
       IF(NGAS.GT.1.AND.GASOK(1).AND.(((.NOT.TAB2D).AND.
     -      (IVMETH.LT.0.OR.IVMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IVMETH.LT.0.OR.IVMETH.GT.2))))THEN
            IVMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid drift velocity'//
     -           ' interpolation; taking polynomial of order ',IVMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(1).AND.(IVEXTR.LT.0.OR.IVEXTR.GT.2.OR.
     -      JVEXTR.LT.0.OR.JVEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for v; assuming linear.'
            IVEXTR=1
            JVEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(2).AND.(((.NOT.TAB2D).AND.
     -      (IMMETH.LT.0.OR.IMMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IMMETH.LT.0.OR.IMMETH.GT.2))))THEN
            IMMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid ion mobility'//
     -           ' interpolation; taking polynomial of order ',IMMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(2).AND.(IMEXTR.LT.0.OR.IMEXTR.GT.2.OR.
     -      JMEXTR.LT.0.OR.JMEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the ion mobility; assuming linear.'
            IMEXTR=1
            JMEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(3).AND.(((.NOT.TAB2D).AND.
     -      (IDMETH.LT.0.OR.IDMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IDMETH.LT.0.OR.IDMETH.GT.2))))THEN
            IDMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid sigma L'//
     -           ' interpolation; taking polynomial of order ',IDMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(3).AND.(IDEXTR.LT.0.OR.IDEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for sigma L; assuming linear.'
            IDEXTR=1
            JDEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(4).AND.(((.NOT.TAB2D).AND.
     -      (IAMETH.LT.0.OR.IAMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IAMETH.LT.0.OR.IAMETH.GT.2))))THEN
            IAMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid Townsend'//
     -           ' interpolation; taking polynomial of order ',IAMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(4).AND.(IAEXTR.LT.0.OR.IAEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for Townsend coefficient; assuming linear.'
            IAEXTR=1
            JAEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(6).AND.(((.NOT.TAB2D).AND.
     -      (IBMETH.LT.0.OR.IBMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IBMETH.LT.0.OR.IBMETH.GT.2))))THEN
            IBMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid attachment'//
     -           ' interpolation; taking polynomial of order ',IBMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(6).AND.(IBEXTR.LT.0.OR.IBEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the attachment; assuming linear.'
            IBEXTR=1
            JBEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(7).AND.(((.NOT.TAB2D).AND.
     -      (IWMETH.LT.0.OR.IWMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IWMETH.LT.0.OR.IWMETH.GT.2))))THEN
            IWMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid (v,E) angle'//
     -           ' interpolation; taking polynomial of order ',IWMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(7).AND.(IWEXTR.LT.0.OR.IWEXTR.GT.2.OR.
     -      JWEXTR.LT.0.OR.JWEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the (v,E) angle; assuming linear.'
            IWEXTR=1
            JWEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(8).AND.(((.NOT.TAB2D).AND.
     -      (IOMETH.LT.0.OR.IOMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IOMETH.LT.0.OR.IOMETH.GT.2))))THEN
            IOMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid sigma T'//
     -           ' interpolation; taking polynomial of order ',IOMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(8).AND.(IOEXTR.LT.0.OR.IOEXTR.GT.2.OR.
     -      JOEXTR.LT.0.OR.JOEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for sigma T; assuming linear.'
            IOEXTR=1
            JOEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(9).AND.(((.NOT.TAB2D).AND.
     -      (IXMETH.LT.0.OR.IXMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IXMETH.LT.0.OR.IXMETH.GT.2))))THEN
            IXMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid v || Btrans'//
     -           ' interpolation; using polynomial of order ',IXMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(9).AND.(IXEXTR.LT.0.OR.IXEXTR.GT.2.OR.
     -      JXEXTR.LT.0.OR.JXEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for v || Btrans; assuming linear.'
            IXEXTR=1
            JXEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(10).AND.(((.NOT.TAB2D).AND.
     -      (IYMETH.LT.0.OR.IYMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IYMETH.LT.0.OR.IYMETH.GT.2))))THEN
            IYMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid v || ExB'//
     -           ' interpolation; taking polynomial of order ',IYMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(10).AND.(IYEXTR.LT.0.OR.IYEXTR.GT.2.OR.
     -      JYEXTR.LT.0.OR.JYEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for v || ExB; assuming linear.'
            IYEXTR=1
            JYEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(11).AND.(((.NOT.TAB2D).AND.
     -      (ISMETH.LT.0.OR.ISMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(ISMETH.LT.0.OR.ISMETH.GT.2))))THEN
            ISMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid diff. tensor'//
     -           ' interpolation; taking polynomial of order ',ISMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(11).AND.(ISEXTR.LT.0.OR.ISEXTR.GT.2.OR.
     -      JSEXTR.LT.0.OR.JSEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the diffusion tensor; assuming linear.'
            ISEXTR=1
            JSEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(12).AND.(((.NOT.TAB2D).AND.
     -      (IHMETH.LT.0.OR.IHMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IHMETH.LT.0.OR.IHMETH.GT.2))))THEN
            IHMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid dissociation'//
     -           ' interpolation; taking polynomial of order ',IHMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(12).AND.(IHEXTR.LT.0.OR.IHEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the dissociation; assuming linear.'
            IHEXTR=1
            JHEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(15).AND.(((.NOT.TAB2D).AND.
     -      (IEMETH.LT.0.OR.IEMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IEMETH.LT.0.OR.IEMETH.GT.2))))THEN
            IEMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid excitation'//
     -           ' interpolation; taking polynomial of order ',IEMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(15).AND.(IEEXTR.LT.0.OR.IEEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the excitations; assuming linear.'
            IEEXTR=1
            JEEXTR=1
            OK=.FALSE.
       ENDIF
       IF(NGAS.GT.1.AND.GASOK(16).AND.(((.NOT.TAB2D).AND.
     -      (IZMETH.LT.0.OR.IZMETH.GT.MIN(10,NGAS-1))).OR.
     -      (TAB2D.AND.(IZMETH.LT.0.OR.IZMETH.GT.2))))THEN
            IZMETH=MIN(2,NGAS-1)
            PRINT *,' !!!!!! GASCHK WARNING : Invalid ionisation'//
     -           ' interpolation; taking polynomial of order ',IZMETH
            OK=.FALSE.
       ENDIF
       IF(GASOK(16).AND.(IZEXTR.LT.0.OR.IZEXTR.GT.2).AND..NOT.TAB2D)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Invalid extrapolation'//
     -           ' method for the ionisations; assuming linear.'
            IZEXTR=1
            JZEXTR=1
            OK=.FALSE.
       ENDIF
*** Mean check: should be positive if cluster data have been entered.
       IF(GASOK(5).AND.CMEAN.LE.0)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Number of clusters/cm'//
     -           ' is absent or not positive; cluster data reset.'
            GASOK(5)=.FALSE.
            OK=.FALSE.
       ENDIF
*   MEAN makes no sense if no other cluster data are present.
       IF(CLSTYP.EQ.'NOT SET'.AND.GASOK(5))THEN
            PRINT *,' !!!!!! GASCHK WARNING : Insufficient data'//
     -           ' to generate clusters. Use HEED, or specify the'
            PRINT *,'                         mean cluster spacing'//
     -           '  (MEAN) together with Landau parameters (E-M-PROB,'
            PRINT *,'                         E-PAIR etc.) or'//
     -           ' CLUSTER, or specify A and Z and use SRIM.'
            GASOK(5)=.FALSE.
            OK=.FALSE.
       ENDIF
*** Cluster check: parameters for the Landau approximation must be > 0.
       IF(CLSTYP.EQ.'LANDAU'.AND.GASOK(5))THEN
            IF(A     .LE.0)PRINT *,' !!!!!! GASCHK WARNING : The'//
     -           ' number of nucleons is absent or not positive;'//
     -           ' cluster data reset.'
            IF(Z     .LE.0)PRINT *,' !!!!!! GASCHK WARNING : The'//
     -           ' nuclear charge is absent or not positive;'//
     -           ' cluster data reset.'
            IF(EMPROB.LE.0)PRINT *,' !!!!!! GASCHK WARNING : The most'//
     -           ' probable energy loss is absent or not positive;'//
     -           ' cluster data reset.'
            IF(EPAIR .LE.0)PRINT *,' !!!!!! GASCHK WARNING : The pair'//
     -           ' creation energy is absent or not positive;'//
     -           ' cluster data reset.'
            IF(RHO   .LE.0)PRINT *,' !!!!!! GASCHK WARNING : The gas'//
     -           ' density is absent or not positive;'//
     -           ' cluster data reset.'
            IF(A.LE.0.OR.Z.LE.0.OR.EMPROB.LE.0.OR.EPAIR.LE.0.OR.
     -           RHO.LE.0.OR.CMEAN.LE.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : No Landau based'//
     -                ' cluster size distribution will be generated.'
                 GASOK(5)=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*   Check that SRIM can operate
       IF(SRIMOK)THEN
            IF(A     .LE.0)PRINT *,' !!!!!! GASCHK WARNING : The'//
     -           ' number of nucleons is absent or not positive;'//
     -           ' SRIM initialisation reset.'
            IF(Z     .LE.0)PRINT *,' !!!!!! GASCHK WARNING : The'//
     -           ' nuclear charge is absent or not positive;'//
     -           ' SRIM initialisation reset.'
            IF(A.LE.0.OR.Z.LE.0)THEN
                 SRIMOK=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*   Direct cluster data, check number of points.
       IF((CLSTYP.EQ.'TABLE'.OR.CLSTYP.EQ.'FUNCTION').AND.
     -      GASOK(5).AND.NCLS.LT.1)THEN
            PRINT *,' !!!!!! GASCHK WARNING : The number of cluster',
     -           ' size distribution data points is insufficient.'
            GASOK(5)=.FALSE.
            OK=.FALSE.
       ENDIF
*   Direct cluster data, check positiveness.
       IF((CLSTYP.EQ.'TABLE'.OR.CLSTYP.EQ.'FUNCTION').AND.GASOK(5))THEN
            SUMCLS=0
            DO 80 I=1,NCLS
            IF(CLSDIS(I).LT.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : The probability for',
     -                ' cluster size ',I,' is set to 0, was ',CLSDIS(I)
                 CLSDIS(I)=0
            ENDIF
            SUMCLS=SUMCLS+CLSDIS(I)
80          CONTINUE
*   Direct cluster data, check integral.
            IF(SUMCLS.LE.0.0)THEN
                 PRINT *,' !!!!!! GASCHK WARNING : The integral'//
     -                ' over the cluster size distribution is'//
     -                ' zero ; distribution rejected.'
                 GASOK(5)=.FALSE.
                 OK=.FALSE.
            ENDIF
       ENDIF
*   Check the consitency between CLSTYP and GASOK(5).
       IF(CLSTYP.NE.'FUNCTION'.AND.CLSTYP.NE.'TABLE'.AND.CLSTYP.NE.
     -      'LANDAU'.AND.CLSTYP.NE.'OVERLAP'.AND.GASOK(5))THEN
            PRINT *,' ###### GASCHK ERROR   : Inconsistent cluster'//
     -           ' type and flag; program bug, please report.'
            GASOK(5)=.FALSE.
            OK=.FALSE.
       ENDIF
*** Flag data as unuseable if not a single table is present.
       IF(.NOT.(GASOK(1).OR.GASOK(2).OR.GASOK(3).OR.GASOK(4).OR.
     -      GASOK(5).OR.GASOK(6).OR.GASOK(7).OR.GASOK(8).OR.
     -      GASOK(9).OR.GASOK(10).OR.GASOK(12)))THEN
            PRINT *,' !!!!!! GASCHK WARNING : Not a single gas'//
     -           ' element left in the description; gas rejected.'
            IFAIL=1
       ELSEIF((JFAIL.EQ.2.OR.JFAIL.EQ.3).AND..NOT.OK)THEN
            PRINT *,' !!!!!! GASCHK WARNING : Gas marked as'//
     -           ' unuseable because of the above errors.'
            IFAIL=1
       ENDIF
*** Generate some debugging output.
       IF(LDEBUG)PRINT *,' ++++++ GASCHK DEBUG   : After checking the'//
     -      ' GASOK bits are: ',(GASOK(I),I=1,20)
*** And register the amount of CPU time used for checking.
       CALL TIMLOG('Checking the gas data makes sense:      ')
       END
