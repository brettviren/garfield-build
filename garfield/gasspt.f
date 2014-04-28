CDECK  ID>, GASSPT.
       SUBROUTINE GASSPT
*-----------------------------------------------------------------------
*   GASSPT - Changes the Penning transfer probabilities.
*   (Last changed on 25/ 8/09.)
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
       INTEGER I,J,K,L,IFAIL
       REAL REXC,RION,AOLD,ANEW
       LOGICAL OK
*** Identify.
       IF(LIDENT)PRINT *,' /// ROUTINE GASSPT ///'
*** Ensure that there are Townsend coefficients.
       IF(.NOT.GASOK(4))THEN
            PRINT *,' !!!!!! GASSPT WARNING : Townsend coefficients'//
     -           ' have not been entered; not setting transfers.'
            RETURN
       ENDIF
*** Loop over the gas table.
       DO 10 I=1,NGAS
       IF(TAB2D)THEN
*   Loop over the table
            DO 20 J=1,NBANG
            DO 30 K=1,NBTAB
*   Compute total ionisation rate.
            RION=0
            DO 40 L=1,NIOGAS
            RION=RION+IOGAS2(I,J,K,L)*PGAS
40          CONTINUE
*   Compute the rate of selected excitations.
            REXC=0
            DO 50 L=1,NEXGAS
            REXC=REXC+PENPRB(L)*EXGAS2(I,J,K,L)*PGAS
50          CONTINUE
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASSPT DEBUG   :'',
     -           '' Entry '',3I3,'' exc: '',E12.5,'' ion: '',E12.5)')
     -           I,J,K,REXC,RION
*   Adjust the Townsend coefficient.
            IF(AORIG2(I,J,K).LT.-20)THEN
                 AOLD=0
            ELSE
                 AOLD=PGAS*EXP(AORIG2(I,J,K))
            ENDIF
            IF(RION.GT.0)THEN
                 ANEW=AOLD*(REXC+RION)/RION
            ELSE
                 ANEW=AOLD
            ENDIF
            IF(ANEW.GT.0)THEN
                 AGAS2(I,J,K)=LOG(ANEW/PGAS)
            ELSE
                 AGAS2(I,J,K)=-30
            ENDIF
30          CONTINUE
20          CONTINUE
       ELSE
*   Compute total ionisation rate.
            RION=0
            DO 60 L=1,NIOGAS
            RION=RION+IOGAS(I,L)*PGAS
60          CONTINUE
*   Compute the rate of selected excitations.
            REXC=0
            DO 70 L=1,NEXGAS
            REXC=REXC+PENPRB(L)*EXGAS(I,L)*PGAS
70          CONTINUE
*   Debugging.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GASSPT DEBUG   :'',
     -           '' Entry '',I3,'' exc: '',E12.5,'' ion: '',E12.5)')
     -           I,REXC,RION
*   Adjust the Townsend coefficient.
            IF(AORIG(I).LT.-20)THEN
                 AOLD=0
            ELSE
                 AOLD=PGAS*EXP(AORIG(I))
            ENDIF
            IF(RION.GT.0)THEN
                 ANEW=AOLD*(REXC+RION)/RION
            ELSE
                 ANEW=AOLD
            ENDIF
            IF(ANEW.GT.0)THEN
                 AGAS(I)=LOG(ANEW/PGAS)
            ELSE
                 AGAS(I)=-30
            ENDIF
       ENDIF
10     CONTINUE
*** Adjust the interpolation and extrapolation coeffiients.
       OK=.TRUE.
       IF(GASOK(4).AND.NGAS.LE.1.AND..NOT.TAB2D)THEN
            IF(IAEXTR.NE.0.OR.JAEXTR.NE.0)THEN
                 PRINT *,' !!!!!! GASSPT WARNING : For a 1-point'//
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
                 PRINT *,' !!!!!! GASSPT WARNING : The Townsend'//
     -                ' data can not be interpolated; data deleted.'
                 GASOK(4)=.FALSE.
                 OK=.FALSE.
            ENDIF
*   Calculate the H extrapolation parameters, using the last 2 points.
            IF(IAEXTR.NE.1.AND.IAEXTR.NE.2)THEN
                 AEXTR1=0.0
                 AEXTR2=0.0
            ELSEIF(EGAS(NGAS).LE.EGAS(NGAS-1))THEN
                 PRINT *,' !!!!!! GASSPT WARNING : Last 2 E/p values'//
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
                 PRINT *,' !!!!!! GASSPT WARNING : First 2 E/p values'//
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
                 PRINT *,' !!!!!! GASSPT WARNING : Interpolation in'//
     -                ' 2D tables can only be linear or quadratic;'
                 PRINT *,'                         will use parabolic'//
     -                ' interpolation for the Townsend coeff.'
                 IAMETH=2
                 OK=.FALSE.
            ENDIF
       ENDIF
*** If not OK, disable Townsend coefficients.
       IF(.NOT.OK)THEN
            PRINT *,' !!!!!! GASSPT WARNING : Disabling the Townsend'//
     -           ' coefficients as a result of the above errors.'
            GASOK(4)=.FALSE.
       ENDIF
       END
