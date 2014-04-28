CDECK  ID>, GASPRE.
       SUBROUTINE GASPRE(IFAIL)
*-----------------------------------------------------------------------
*   GASPRE - Prepares the gas data for further use by other routines.
*   VARIABLES : IFAIL        : 1 if routine failed 0 if succesful
*   (Last changed on 21/ 2/08.)
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
       DOUBLE PRECISION CLSSUM
       REAL DENLAN
       CHARACTER*20 AUX1
       CHARACTER*45 NEWID
       INTEGER I,J,K,L,N,IFAIL,NFAIL,NC1
       LOGICAL OK
       EXTERNAL DENLAN
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE GASPRE ///'
*** For the time being assume this will work.
       IFAIL=0
       OK=.TRUE.
*** Drift velocity preparation, start with a table of 1 point.
       IF(GASOK(1).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IVEXTR.NE.0.OR.JVEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IVEXTR=0
                 JVEXTR=0
                 OK=.FALSE.
            ENDIF
*   Calculate the spline coefficients for the drift speed,
       ELSEIF(GASOK(1).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,VGAS,CVGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The drift velocity'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(1)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(VGAS(NGAS).LE.0.OR.(IVEXTR.NE.1.AND.IVEXTR.NE.2))THEN
                 VEXTR1=0.0
                 VEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no v extrapolation to higher E/p.'
                 IVEXTR=0
                 OK=.FALSE.
            ELSEIF(IVEXTR.EQ.1)THEN
                 VEXTR2=(VGAS(NGAS)-VGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 VEXTR1=VGAS(NGAS)-VEXTR2*EGAS(NGAS)
                 IF(VEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*VEXTR1/VEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the drift velocity is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IVEXTR.EQ.2)THEN
                 VEXTR2=LOG(VGAS(NGAS)/VGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 VEXTR1=LOG(VGAS(NGAS))-VEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(VGAS(1).LE.0.OR.(JVEXTR.NE.1.AND.JVEXTR.NE.2))THEN
                 VEXTR3=0.0
                 VEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no v extrapolation to lower E/p.'
                 JVEXTR=0
                 OK=.FALSE.
            ELSEIF(JVEXTR.EQ.1)THEN
                 VEXTR4=(VGAS(2)-VGAS(1))/(EGAS(2)-EGAS(1))
                 VEXTR3=VGAS(1)-VEXTR4*EGAS(1)
                 IF(VEXTR4.GT.0.0.AND.VEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*VEXTR3/VEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the drift velocity is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JVEXTR.EQ.2)THEN
                 VEXTR4=LOG(VGAS(2)/VGAS(1))/(EGAS(2)-EGAS(1))
                 VEXTR3=LOG(VGAS(1))-VEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(1).AND.(IVMETH.NE.1.AND.IVMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the drift velocity.'
            IVMETH=2
            OK=.FALSE.
       ENDIF
*** Drift velocity B preparation, start with a table of 1 point.
       IF(GASOK(9).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IXEXTR.NE.0.OR.JXEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IXEXTR=0
                 JXEXTR=0
                 OK=.FALSE.
            ENDIF
*   Calculate the spline coefficients for the drift speed,
       ELSEIF(GASOK(9).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,XGAS,CXGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : v || Btrans'//
     -                ' can not be interpolated; data deleted.'
                 GASOK(9)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(XGAS(NGAS).LE.0.OR.(IXEXTR.NE.1.AND.IXEXTR.NE.2))THEN
                 XEXTR1=0.0
                 XEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no v || Btrans extrapolation to'//
     -                ' high E/p.'
                 IXEXTR=0
                 OK=.FALSE.
            ELSEIF(IXEXTR.EQ.1)THEN
                 XEXTR2=(XGAS(NGAS)-XGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 XEXTR1=XGAS(NGAS)-XEXTR2*EGAS(NGAS)
                 IF(XEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*XEXTR1/XEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of v || Btrans is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IXEXTR.EQ.2)THEN
                 XEXTR2=LOG(XGAS(NGAS)/XGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 XEXTR1=LOG(XGAS(NGAS))-XEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(XGAS(1).LE.0.OR.(JXEXTR.NE.1.AND.JXEXTR.NE.2))THEN
                 XEXTR3=0.0
                 XEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no v || Btrans extrapolation to'//
     -                ' low E/p.'
                 JXEXTR=0
                 OK=.FALSE.
            ELSEIF(JXEXTR.EQ.1)THEN
                 XEXTR4=(XGAS(2)-XGAS(1))/(EGAS(2)-EGAS(1))
                 XEXTR3=XGAS(1)-XEXTR4*EGAS(1)
                 IF(XEXTR4.GT.0.0.AND.XEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*XEXTR3/XEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of v || Btrans is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JXEXTR.EQ.2)THEN
                 XEXTR4=LOG(XGAS(2)/XGAS(1))/(EGAS(2)-EGAS(1))
                 XEXTR3=LOG(XGAS(1))-XEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(9).AND.(IXMETH.NE.1.AND.IXMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for v || Btrans.'
            IXMETH=2
            OK=.FALSE.
       ENDIF
*** Drift velocity ExB preparation, start with a table of 1 point.
       IF(GASOK(10).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IYEXTR.NE.0.OR.JYEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IYEXTR=0
                 JYEXTR=0
                 OK=.FALSE.
            ENDIF
*   Calculate the spline coefficients for the drift speed,
       ELSEIF(GASOK(10).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,YGAS,CYGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : v || ExB'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(10)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(YGAS(NGAS).LE.0.OR.(IYEXTR.NE.1.AND.IYEXTR.NE.2))THEN
                 YEXTR1=0.0
                 YEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no v || ExB extrapolation to high'//
     -                ' E/p.'
                 IYEXTR=0
                 OK=.FALSE.
            ELSEIF(IYEXTR.EQ.1)THEN
                 YEXTR2=(YGAS(NGAS)-YGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 YEXTR1=YGAS(NGAS)-YEXTR2*EGAS(NGAS)
                 IF(YEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*YEXTR1/YEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of v || ExB is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IYEXTR.EQ.2)THEN
                 YEXTR2=LOG(YGAS(NGAS)/YGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 YEXTR1=LOG(YGAS(NGAS))-YEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(YGAS(1).LE.0.OR.(JYEXTR.NE.1.AND.JYEXTR.NE.2))THEN
                 YEXTR3=0.0
                 YEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no v || ExB extrapolation to low'//
     -                ' E/p.'
                 JYEXTR=0
                 OK=.FALSE.
            ELSEIF(JYEXTR.EQ.1)THEN
                 YEXTR4=(YGAS(2)-YGAS(1))/(EGAS(2)-EGAS(1))
                 YEXTR3=YGAS(1)-YEXTR4*EGAS(1)
                 IF(YEXTR4.GT.0.0.AND.YEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*YEXTR3/YEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of v || ExB is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JYEXTR.EQ.2)THEN
                 YEXTR4=LOG(YGAS(2)/YGAS(1))/(EGAS(2)-EGAS(1))
                 YEXTR3=LOG(YGAS(1))-YEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(10).AND.(IYMETH.NE.1.AND.IYMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for v || ExB.'
            IYMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for the ion mobility.
       IF(GASOK(2).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IMEXTR.NE.0.OR.JMEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IMEXTR=0
                 JMEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(2).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,MGAS,CMGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The ion mobility'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(2)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(MGAS(NGAS).LE.0.OR.(IMEXTR.NE.1.AND.IMEXTR.NE.2))THEN
                 MEXTR1=0.0
                 MEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no mu extrapolation to higher E/p.'
                 IMEXTR=0
                 OK=.FALSE.
            ELSEIF(IMEXTR.EQ.1)THEN
                 MEXTR2=(MGAS(NGAS)-MGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 MEXTR1=MGAS(NGAS)-MEXTR2*EGAS(NGAS)
                 IF(MEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*MEXTR1/MEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the ion mobility is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IMEXTR.EQ.2)THEN
                 MEXTR2=LOG(MGAS(NGAS)/MGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 MEXTR1=LOG(MGAS(NGAS))-MEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(MGAS(1).LE.0.OR.(JMEXTR.NE.1.AND.JMEXTR.NE.2))THEN
                 MEXTR3=0.0
                 MEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no mu extrapolation to lower E/p.'
                 JMEXTR=0
                 OK=.FALSE.
            ELSEIF(JMEXTR.EQ.1)THEN
                 MEXTR4=(MGAS(2)-MGAS(1))/(EGAS(2)-EGAS(1))
                 MEXTR3=MGAS(1)-MEXTR4*EGAS(1)
                 IF(MEXTR4.GT.0.0.AND.MEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*MEXTR3/MEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the ion mobility is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JMEXTR.EQ.2)THEN
                 MEXTR4=LOG(MGAS(2)/MGAS(1))/(EGAS(2)-EGAS(1))
                 MEXTR3=LOG(MGAS(1))-MEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(2).AND.(IMMETH.NE.1.AND.IMMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the ion mobility.'
            IMMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for sigma L.
       IF(GASOK(3).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IDEXTR.NE.0.OR.JDEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IDEXTR=0
                 JDEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(3).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,DGAS,CDGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The long. diff.'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(3)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(DGAS(NGAS).LE.0.OR.(IDEXTR.NE.1.AND.IDEXTR.NE.2))THEN
                 DEXTR1=0.0
                 DEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no DL extrapolation to higher E/p.'
                 IDEXTR=0
                 OK=.FALSE.
            ELSEIF(IDEXTR.EQ.1)THEN
                 DEXTR2=(DGAS(NGAS)-DGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 DEXTR1=DGAS(NGAS)-DEXTR2*EGAS(NGAS)
                 IF(DEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*DEXTR1/DEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the long. diff. is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IDEXTR.EQ.2)THEN
                 DEXTR2=LOG(DGAS(NGAS)/DGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 DEXTR1=LOG(DGAS(NGAS))-DEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(DGAS(1).LE.0.OR.(JDEXTR.NE.1.AND.JDEXTR.NE.2))THEN
                 DEXTR3=0.0
                 DEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no DL extrapolation to lower E/p.'
                 JDEXTR=0
                 OK=.FALSE.
            ELSEIF(JDEXTR.EQ.1)THEN
                 DEXTR4=(DGAS(2)-DGAS(1))/(EGAS(2)-EGAS(1))
                 DEXTR3=DGAS(1)-DEXTR4*EGAS(1)
                 IF(DEXTR4.GT.0.0.AND.DEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*DEXTR3/DEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the long. diff. is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JDEXTR.EQ.2)THEN
                 DEXTR4=LOG(DGAS(2)/DGAS(1))/(EGAS(2)-EGAS(1))
                 DEXTR3=LOG(DGAS(1))-DEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(3).AND.(IDMETH.NE.1.AND.IDMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the long. diff.'
            IDMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for sigma T.
       IF(GASOK(8).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IOEXTR.NE.0.OR.JOEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IOEXTR=0
                 JOEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(8).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,OGAS,COGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The trans. diff.'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(8)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(OGAS(NGAS).LE.0.OR.(IOEXTR.NE.1.AND.IOEXTR.NE.2))THEN
                 OEXTR1=0.0
                 OEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no DT extrapolation to higher E/p.'
                 IOEXTR=0
                 OK=.FALSE.
            ELSEIF(IOEXTR.EQ.1)THEN
                 OEXTR2=(OGAS(NGAS)-OGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 OEXTR1=OGAS(NGAS)-OEXTR2*EGAS(NGAS)
                 IF(OEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*OEXTR1/OEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the trans. diff. is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IOEXTR.EQ.2)THEN
                 OEXTR2=LOG(OGAS(NGAS)/OGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 OEXTR1=LOG(OGAS(NGAS))-OEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(OGAS(1).LE.0.OR.(JOEXTR.NE.1.AND.JOEXTR.NE.2))THEN
                 OEXTR3=0.0
                 OEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no DT extrapolation to lower E/p.'
                 JOEXTR=0
                 OK=.FALSE.
            ELSEIF(JOEXTR.EQ.1)THEN
                 OEXTR4=(OGAS(2)-OGAS(1))/(EGAS(2)-EGAS(1))
                 OEXTR3=OGAS(1)-OEXTR4*EGAS(1)
                 IF(OEXTR4.GT.0.0.AND.OEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*OEXTR3/OEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the trans. diff. is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JOEXTR.EQ.2)THEN
                 OEXTR4=LOG(OGAS(2)/OGAS(1))/(EGAS(2)-EGAS(1))
                 OEXTR3=LOG(OGAS(1))-OEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(8).AND.(IOMETH.NE.1.AND.IOMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the trans. diff.'
            IOMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for the diffusion tensor.
       IF(GASOK(11).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(ISEXTR.NE.0.OR.JSEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 ISEXTR=0
                 JSEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(11).AND..NOT.TAB2D)THEN
            DO 30 L=1,6
            CALL SPLINE(EGAS,SGAS(1,L),CSGAS(1,L),NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The diff. tensor.'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(11)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(SGAS(NGAS,L).LE.0.OR.(ISEXTR.NE.1.AND.ISEXTR.NE.2))THEN
                 SEXTR1(L)=0.0
                 SEXTR2(L)=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no diffusion tensor extrapolation'//
     -                ' to higher E/p.'
                 ISEXTR=0
                 OK=.FALSE.
            ELSEIF(ISEXTR.EQ.1)THEN
                 SEXTR2(L)=(SGAS(NGAS,L)-SGAS(NGAS-1,L))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 SEXTR1(L)=SGAS(NGAS,L)-SEXTR2(L)*EGAS(NGAS)
                 IF(SEXTR2(L).LT.0.0)THEN
                      CALL OUTFMT(-PGAS*SEXTR1(L)/SEXTR2(L),2,
     -                     AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the diff. tensor is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(ISEXTR.EQ.2)THEN
                 SEXTR2(L)=LOG(SGAS(NGAS,L)/SGAS(NGAS-1,L))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 SEXTR1(L)=LOG(SGAS(NGAS,L))-SEXTR2(L)*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(SGAS(1,L).LE.0.OR.(JSEXTR.NE.1.AND.JSEXTR.NE.2))THEN
                 SEXTR3(L)=0.0
                 SEXTR4(L)=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no diffusion tensor extrapolation'//
     -                ' to lower E/p.'
                 JSEXTR=0
                 OK=.FALSE.
            ELSEIF(JSEXTR.EQ.1)THEN
                 SEXTR4(L)=(SGAS(2,L)-SGAS(1,L))/(EGAS(2)-EGAS(1))
                 SEXTR3(L)=SGAS(1,L)-SEXTR4(L)*EGAS(1)
                 IF(SEXTR4(L).GT.0.0.AND.SEXTR3(L).LT.0)THEN
                      CALL OUTFMT(-PGAS*SEXTR3(L)/SEXTR4(L),2,
     -                     AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the diff. tensor is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JSEXTR.EQ.2)THEN
                 SEXTR4(L)=LOG(SGAS(2,L)/SGAS(1,L))/(EGAS(2)-EGAS(1))
                 SEXTR3(L)=LOG(SGAS(1,L))-SEXTR4(L)*EGAS(1)
            ENDIF
30          CONTINUE
*   2D interpolation.
       ELSEIF(GASOK(11).AND.(ISMETH.NE.1.AND.ISMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the diffusion tensor.'
            ISMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for the Townsend coefficient.
       IF(GASOK(4).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IAEXTR.NE.0.OR.JAEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IAEXTR=0
                 JAEXTR=0
                 OK=.FALSE.
            ENDIF
            IATHR=1
       ELSEIF(GASOK(4).AND..NOT.TAB2D)THEN
*   Set threshold.
            DO 100 I=1,NGAS
            IF(AGAS(I).LE.-20)GOTO 100
            IATHR=MIN(NGAS,I+1)
            GOTO 110
100         CONTINUE
            IATHR=1
110         CONTINUE
*   Prepare spline coefficients.
            CALL SPLINE(EGAS,AGAS,CAGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The Townsend'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(4)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(IAEXTR.NE.1.AND.IAEXTR.NE.2)THEN
                 AEXTR1=0.0
                 AEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no alpha extrapolation to higher E/p.'
                 IAEXTR=0
                 OK=.FALSE.
            ELSEIF(IAEXTR.EQ.1)THEN
                 AEXTR2=(AGAS(NGAS)-AGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 AEXTR1=AGAS(NGAS)-AEXTR2*EGAS(NGAS)
            ELSEIF(IAEXTR.EQ.2)THEN
                 AEXTR2=LOG(AGAS(NGAS)/AGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 AEXTR1=LOG(AGAS(NGAS))-AEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(JAEXTR.NE.1.AND.JAEXTR.NE.2)THEN
                 AEXTR3=0.0
                 AEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no alpha extrapolation to lower E/p.'
                 JAEXTR=0
                 OK=.FALSE.
            ELSEIF(JAEXTR.EQ.1)THEN
                 AEXTR4=(AGAS(2)-AGAS(1))/(EGAS(2)-EGAS(1))
                 AEXTR3=AGAS(1)-AEXTR4*EGAS(1)
            ELSEIF(JAEXTR.EQ.2)THEN
                 AEXTR4=LOG(AGAS(2)/AGAS(1))/(EGAS(2)-EGAS(1))
                 AEXTR3=LOG(AGAS(1))-AEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(4))THEN
*   Set threshold.
            DO 120 I=1,NGAS
            DO 130 J=1,NBANG
            DO 140 K=1,NBTAB
            IF(AGAS2(I,J,K).LT.-20)GOTO 120
140         CONTINUE
130         CONTINUE
            IATHR=MIN(NGAS,I+1)
            GOTO 150
120         CONTINUE
            IATHR=1
150         CONTINUE
*   Check interpolation method.
            IF(IAMETH.NE.1.AND.IAMETH.NE.2)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -                ' 2D tables can only be linear or quadratic;'
                 PRINT *,'                         will use parabolic'//
     -                ' interpolation for the Townsend coeff.'
                 IAMETH=2
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Calculate the spline coefficients for the attachment.
       IF(GASOK(6).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IBEXTR.NE.0.OR.JBEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IBEXTR=0
                 JBEXTR=0
                 OK=.FALSE.
            ENDIF
            IBTHR=1
       ELSEIF(GASOK(6).AND..NOT.TAB2D)THEN
*   Set threshold.
            DO 200 I=1,NGAS
            IF(BGAS(I).LE.-20)GOTO 200
            IBTHR=MIN(NGAS,I+1)
            GOTO 210
200         CONTINUE
            IBTHR=1
210         CONTINUE
*   Prepare spline coefficients.
            CALL SPLINE(EGAS,BGAS,CBGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The attachment'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(6)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(IBEXTR.NE.1.AND.IBEXTR.NE.2)THEN
                 BEXTR1=0.0
                 BEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no eta extrapolation to higher E/p.'
                 IBEXTR=0
                 OK=.FALSE.
            ELSEIF(IBEXTR.EQ.1)THEN
                 BEXTR2=(BGAS(NGAS)-BGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 BEXTR1=BGAS(NGAS)-BEXTR2*EGAS(NGAS)
            ELSEIF(IBEXTR.EQ.2)THEN
                 BEXTR2=LOG(BGAS(NGAS)/BGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 BEXTR1=LOG(BGAS(NGAS))-BEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(JBEXTR.NE.1.AND.JBEXTR.NE.2)THEN
                 BEXTR3=0.0
                 BEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no eta extrapolation to lower E/p.'
                 JBEXTR=0
                 OK=.FALSE.
            ELSEIF(JBEXTR.EQ.1)THEN
                 BEXTR4=(BGAS(2)-BGAS(1))/(EGAS(2)-EGAS(1))
                 BEXTR3=BGAS(1)-BEXTR4*EGAS(1)
            ELSEIF(JBEXTR.EQ.2)THEN
                 BEXTR4=LOG(BGAS(2)/BGAS(1))/(EGAS(2)-EGAS(1))
                 BEXTR3=LOG(BGAS(1))-BEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(6))THEN
*   Set threshold.
            DO 220 I=1,NGAS
            DO 230 J=1,NBANG
            DO 240 K=1,NBTAB
            IF(BGAS2(I,J,K).LE.-20)GOTO 220
240         CONTINUE
230         CONTINUE
            IBTHR=MIN(NGAS,I+1)
            GOTO 250
220         CONTINUE
            IBTHR=1
250         CONTINUE
*   Check interpolation method.
            IF(IBMETH.NE.1.AND.IBMETH.NE.2)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -                ' 2D tables can only be linear or quadratic;'
                 PRINT *,'                         will use parabolic'//
     -                ' interpolation for the attachment.'
                 IBMETH=2
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Calculate the spline coefficients for the dissociation.
       IF(GASOK(12).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IHEXTR.NE.0.OR.JHEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IHEXTR=0
                 JHEXTR=0
                 OK=.FALSE.
            ENDIF
            IHTHR=1
       ELSEIF(GASOK(12).AND..NOT.TAB2D)THEN
*   Set threshold.
            DO 300 I=1,NGAS
            IF(HGAS(I).LE.-20)GOTO 300
            IHTHR=MIN(NGAS,I+1)
            GOTO 310
300         CONTINUE
            IHTHR=1
310         CONTINUE
*   Prepare spline coefficients.
            CALL SPLINE(EGAS,HGAS,CHGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The dissociation'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(12)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(IHEXTR.NE.1.AND.IHEXTR.NE.2)THEN
                 HEXTR1=0.0
                 HEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no dissociation extrapolation to'//
     -                ' higher E/p.'
                 IHEXTR=0
                 OK=.FALSE.
            ELSEIF(IHEXTR.EQ.1)THEN
                 HEXTR2=(HGAS(NGAS)-HGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 HEXTR1=HGAS(NGAS)-HEXTR2*EGAS(NGAS)
            ELSEIF(IHEXTR.EQ.2)THEN
                 HEXTR2=LOG(HGAS(NGAS)/HGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 HEXTR1=LOG(HGAS(NGAS))-HEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(JHEXTR.NE.1.AND.JHEXTR.NE.2)THEN
                 HEXTR3=0.0
                 HEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no dissociation extrapolation to'//
     -                ' lower E/p.'
                 JHEXTR=0
                 OK=.FALSE.
            ELSEIF(JHEXTR.EQ.1)THEN
                 HEXTR4=(HGAS(2)-HGAS(1))/(EGAS(2)-EGAS(1))
                 HEXTR3=HGAS(1)-HEXTR4*EGAS(1)
            ELSEIF(JHEXTR.EQ.2)THEN
                 HEXTR4=LOG(HGAS(2)/HGAS(1))/(EGAS(2)-EGAS(1))
                 HEXTR3=LOG(HGAS(1))-HEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(12))THEN
*   Set threshold.
            DO 320 I=1,NGAS
            DO 330 J=1,NBANG
            DO 340 K=1,NBTAB
            IF(HGAS2(I,J,K).LE.-20)GOTO 320
340         CONTINUE
330         CONTINUE
            IHTHR=MIN(NGAS,I+1)
            GOTO 350
320         CONTINUE
            IHTHR=1
350         CONTINUE
*   Check interpolation method.
            IF(IHMETH.NE.1.AND.IHMETH.NE.2)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -                ' 2D tables can only be linear or quadratic;'
                 PRINT *,'                         will use parabolic'//
     -                ' interpolation for the dissociation.'
                 IHMETH=2
                 OK=.FALSE.
            ENDIF
       ENDIF
*** Calculate the spline coefficients for the Lorentz angle.
       IF(GASOK(7).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IWEXTR.NE.0.OR.JWEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IWEXTR=0
                 JWEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(7).AND..NOT.TAB2D)THEN
            CALL SPLINE(EGAS,WGAS,CWGAS,NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The (v,E) angle'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(7)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(WGAS(NGAS).LE.0.OR.(IWEXTR.NE.1.AND.IWEXTR.NE.2))THEN
                 WEXTR1=0.0
                 WEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no Lorentz extrapolation to higher'//
     -                ' E/p.'
                 IWEXTR=0
                 OK=.FALSE.
            ELSEIF(IWEXTR.EQ.1)THEN
                 WEXTR2=(WGAS(NGAS)-WGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 WEXTR1=WGAS(NGAS)-WEXTR2*EGAS(NGAS)
                 IF(WEXTR2.LT.0.0)THEN
                      CALL OUTFMT(-PGAS*WEXTR1/WEXTR2,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the (v,E) angle is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IWEXTR.EQ.2)THEN
                 WEXTR2=LOG(WGAS(NGAS)/WGAS(NGAS-1))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 WEXTR1=LOG(WGAS(NGAS))-WEXTR2*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(WGAS(1).LE.0.OR.(JWEXTR.NE.1.AND.JWEXTR.NE.2))THEN
                 WEXTR3=0.0
                 WEXTR4=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no Lorentz extrapolation to higher'//
     -                ' E/p.'
                 JWEXTR=0
                 OK=.FALSE.
            ELSEIF(JWEXTR.EQ.1)THEN
                 WEXTR4=(WGAS(2)-WGAS(1))/(EGAS(2)-EGAS(1))
                 WEXTR3=WGAS(1)-WEXTR4*EGAS(1)
                 IF(WEXTR4.GT.0.0.AND.WEXTR3.LT.0)THEN
                      CALL OUTFMT(-PGAS*WEXTR3/WEXTR4,2,AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the (v,E) angle is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JWEXTR.EQ.2)THEN
                 WEXTR4=LOG(WGAS(2)/WGAS(1))/(EGAS(2)-EGAS(1))
                 WEXTR3=LOG(WGAS(1))-WEXTR4*EGAS(1)
            ENDIF
*   2D interpolation.
       ELSEIF(GASOK(7).AND.(IWMETH.NE.1.AND.IWMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the (v,E) angle.'
            IWMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for the excitation rates.
       IF(GASOK(15).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IEEXTR.NE.0.OR.JEEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IEEXTR=0
                 JEEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(15).AND..NOT.TAB2D)THEN
            DO 40 L=1,NEXGAS
            CALL SPLINE(EGAS,EXGAS(1,L),CEXGAS(1,L),NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The excitation'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(15)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(EXGAS(NGAS,L).LE.0.OR.(IEEXTR.NE.1.AND.IEEXTR.NE.2))THEN
                 EEXTR1(L)=0.0
                 EEXTR2(L)=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no excitation extrapolation'//
     -                ' to higher E/p.'
                 IEEXTR=0
                 OK=.FALSE.
            ELSEIF(IEEXTR.EQ.1)THEN
                 EEXTR2(L)=(EXGAS(NGAS,L)-EXGAS(NGAS-1,L))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 EEXTR1(L)=EXGAS(NGAS,L)-EEXTR2(L)*EGAS(NGAS)
                 IF(EEXTR2(L).LT.0.0)THEN
                      CALL OUTFMT(-PGAS*EEXTR1(L)/EEXTR2(L),2,
     -                     AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the excitation is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IEEXTR.EQ.2)THEN
                 EEXTR2(L)=LOG(EXGAS(NGAS,L)/EXGAS(NGAS-1,L))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 EEXTR1(L)=LOG(EXGAS(NGAS,L))-EEXTR2(L)*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(EXGAS(1,L).LE.0.OR.(JEEXTR.NE.1.AND.JEEXTR.NE.2))THEN
                 EEXTR3(L)=0.0
                 EEXTR4(L)=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no excitation extrapolation'//
     -                ' to lower E/p.'
                 JEEXTR=0
                 OK=.FALSE.
            ELSEIF(JEEXTR.EQ.1)THEN
                 EEXTR4(L)=(EXGAS(2,L)-EXGAS(1,L))/(EGAS(2)-EGAS(1))
                 EEXTR3(L)=EXGAS(1,L)-EEXTR4(L)*EGAS(1)
                 IF(EEXTR4(L).GT.0.0.AND.EEXTR3(L).LT.0)THEN
                      CALL OUTFMT(-PGAS*EEXTR3(L)/EEXTR4(L),2,
     -                     AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the excitation is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JEEXTR.EQ.2)THEN
                 EEXTR4(L)=LOG(EXGAS(2,L)/EXGAS(1,L))/(EGAS(2)-EGAS(1))
                 EEXTR3(L)=LOG(EXGAS(1,L))-EEXTR4(L)*EGAS(1)
            ENDIF
40          CONTINUE
*   2D interpolation.
       ELSEIF(GASOK(15).AND.(IEMETH.NE.1.AND.IEMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the excitation.'
            IEMETH=2
            OK=.FALSE.
       ENDIF
*** Calculate the spline coefficients for the ionisation rates.
       IF(GASOK(16).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IZEXTR.NE.0.OR.JZEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : For a 1-point'//
     -                ' table, only constant extrapolation is valid.'
                 IZEXTR=0
                 JZEXTR=0
                 OK=.FALSE.
            ENDIF
       ELSEIF(GASOK(16).AND..NOT.TAB2D)THEN
            DO 50 L=1,NIOGAS
            CALL SPLINE(EGAS,IOGAS(1,L),CIOGAS(1,L),NGAS,IFAIL)
            IF(IFAIL.EQ.1)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : The ionisation'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(16)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(IOGAS(NGAS,L).LE.0.OR.(IZEXTR.NE.1.AND.IZEXTR.NE.2))THEN
                 ZEXTR1(L)=0.0
                 ZEXTR2(L)=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Last 2 E/p values'//
     -                ' coincide; no ionisation extrapolation'//
     -                ' to higher E/p.'
                 IZEXTR=0
                 OK=.FALSE.
            ELSEIF(IZEXTR.EQ.1)THEN
                 ZEXTR2(L)=(IOGAS(NGAS,L)-IOGAS(NGAS-1,L))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 ZEXTR1(L)=IOGAS(NGAS,L)-ZEXTR2(L)*EGAS(NGAS)
                 IF(ZEXTR2(L).LT.0.0)THEN
                      CALL OUTFMT(-PGAS*ZEXTR1(L)/ZEXTR2(L),2,
     -                     AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the ionisation is'//
     -                     ' negative for E > '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(IZEXTR.EQ.2)THEN
                 ZEXTR2(L)=LOG(IOGAS(NGAS,L)/IOGAS(NGAS-1,L))/
     -                (EGAS(NGAS)-EGAS(NGAS-1))
                 ZEXTR1(L)=LOG(IOGAS(NGAS,L))-ZEXTR2(L)*EGAS(NGAS)
            ENDIF
*   Calculate the L extrapolation parameters, using the last 2 points.
            IF(IOGAS(1,L).LE.0.OR.(JZEXTR.NE.1.AND.JZEXTR.NE.2))THEN
                 ZEXTR3(L)=0.0
                 ZEXTR4(L)=0.0
            ELSEIF(EGAS(2).LE.EGAS(1))THEN
                 PRINT *,' !!!!!! GASPRE WARNING : First 2 E/p values'//
     -                ' coincide; no ionisation extrapolation'//
     -                ' to lower E/p.'
                 JZEXTR=0
                 OK=.FALSE.
            ELSEIF(JZEXTR.EQ.1)THEN
                 ZEXTR4(L)=(IOGAS(2,L)-IOGAS(1,L))/(EGAS(2)-EGAS(1))
                 ZEXTR3(L)=IOGAS(1,L)-ZEXTR4(L)*EGAS(1)
                 IF(ZEXTR4(L).GT.0.0.AND.ZEXTR3(L).LT.0)THEN
                      CALL OUTFMT(-PGAS*ZEXTR3(L)/ZEXTR4(L),2,
     -                     AUX1,NC1,'LEFT')
                      PRINT *,' ------ GASPRE MESSAGE : The linear'//
     -                     ' extrapolation of the ionisation is'//
     -                     ' negative for E < '//AUX1(1:NC1)//' V/cm.'
                 ENDIF
            ELSEIF(JZEXTR.EQ.2)THEN
                 ZEXTR4(L)=LOG(IOGAS(2,L)/IOGAS(1,L))/(EGAS(2)-EGAS(1))
                 ZEXTR3(L)=LOG(IOGAS(1,L))-ZEXTR4(L)*EGAS(1)
            ENDIF
50          CONTINUE
*   2D interpolation.
       ELSEIF(GASOK(16).AND.(IZMETH.NE.1.AND.IZMETH.NE.2))THEN
            PRINT *,' !!!!!! GASPRE WARNING : Interpolation in'//
     -           ' 2D tables can only be linear or quadratic;'
            PRINT *,'                         will use parabolic'//
     -           ' interpolation for the ionisation.'
            IZMETH=2
            OK=.FALSE.
       ENDIF
*** Look up the levels.
       DO 60 I=1,NEXGAS
       CALL GASTTR(DSCEXG(I),NEWID,ENEXG(I))
       DSCEXG(I)=NEWID
60     CONTINUE
       DO 70 I=1,NIOGAS
       CALL GASTTR(DSCIOG(I),NEWID,ENIOG(I))
       DSCIOG(I)=NEWID
70     CONTINUE
*** Reset the IFAIL's from the splines (they are now stored in GASOK).
       IFAIL=0
*** Calculate the cluster size distr hist from parameters, call HISPRD.
       IF(CLSTYP.EQ.'LANDAU'.AND.GASOK(5))THEN
            IF(LDEBUG)PRINT *,' ++++++ GASPRE DEBUG   : First order',
     -           ' energy loss according to Bethe-Bloch: ',
     -           (1.54E5*(Z/A)*RHO)-LOG(CMEAN)
*   Fix the maximum number of clusters.
            NCLS=MXPAIR
            NFAIL=0
            DO 10 N=1,NCLS
*   If the argument of DENLAN is smaller than -6, error 208 occurs.
            IF((CMEAN*(N-1.0)*EPAIR-EMPROB)/(1.54E5*(Z/A)*RHO)-
     -           LOG(CMEAN).LT.-5.0)THEN
                 CLSDIS(N)=0
                 NFAIL=NFAIL+1
*   Otherwise, use the library function.
            ELSE
                 CLSDIS(N)=(DENLAN((CMEAN*REAL(N-1)*EPAIR-EMPROB)/
     -                (1.54E5*(Z/A)*RHO)-LOG(CMEAN))+4*
     -                DENLAN((CMEAN*(N-0.5)*EPAIR-EMPROB)/
     -                (1.54E5*(Z/A)*RHO)-LOG(CMEAN))+
     -                DENLAN((CMEAN*REAL(N)*EPAIR-EMPROB)/
     -                (1.54E5*(Z/A)*RHO)-LOG(CMEAN)))/6
            ENDIF
10          CONTINUE
*   Check there are some non-zero elements.
            IF(NFAIL.EQ.NCLS)THEN
                 PRINT *,' !!!!!! GASPRE WARNING : Your parameters'//
     -                ' are such that all cluster sizes up to ',NCLS
                 PRINT *,'                         have probability'//
     -                ' zero; cluster size distribution deleted.'
                 GASOK(5)=.FALSE.
            ENDIF
       ENDIF
*** Call HISPRD to prepare random number extraction.
       IF(GASOK(5).AND.(CLSTYP.EQ.'LANDAU'.OR.CLSTYP.EQ.'FUNCTION'.OR.
     -      CLSTYP.EQ.'TABLE'.OR.CLSTYP.EQ.'OVERLAP'))THEN
*   Debugging output.
            IF(LDEBUG)THEN
                 PRINT *,' ++++++ GASPRE DEBUG   : HISPRD to be called',
     -                ' for ',CLSTYP,', NCLS=',NCLS,', distribution:'
                 PRINT *,(CLSDIS(I),I=1,NCLS)
            ENDIF
*   Compute average number of pairs per cluster.
            CLSAVE=0
            CLSSUM=0
            DO 20 I=1,NCLS
            CLSAVE=CLSAVE+I*CLSDIS(I)
            CLSSUM=CLSSUM+CLSDIS(I)
20          CONTINUE
            CLSAVE=CLSAVE/CLSSUM
*   Prepare the histogram for random number generation.
            CALL HISPRD(CLSDIS,NCLS)
       ENDIF
*** Call TIMLOG to register the amount of CPU time used.
       CALL TIMLOG('Reading and preparing gas data:         ')
       END
