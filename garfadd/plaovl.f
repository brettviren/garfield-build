CDECK  ID>, PLAOVL.
       SUBROUTINE PLAOVL(IREF1,IREF2,NREFO,IREFO,ITYPO,EPSX,EPSY,IFAIL)
*-----------------------------------------------------------------------
*   PLAOVL - Isolates the parts of plane 1 that are not hidden by 2.
*   (Last changed on 13/ 5/10.)
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
       INTEGER MXCORN
       PARAMETER(MXCORN=3*MXEDGE)
       DOUBLE PRECISION
     -      XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),APL1,BPL1,CPL1,DPL1,
     -      XPL2(MXEDGE),YPL2(MXEDGE),ZPL2(MXEDGE),APL2,BPL2,CPL2,DPL2,
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      EPSD,XMEAN,YMEAN,ZMEAN,XL(MXCORN,2),YL(MXCORN,2),
     -      Q(MXCORN,2),QMIN,XAUX,YAUX,QAUX,
     -      XC,YC,EPSX,EPSY,EPSZ,XMIN,YMIN,XMAX,YMAX,
     -      XMIN1,YMIN1,XMAX1,YMAX1,
     -      XMIN2,YMIN2,XMAX2,YMAX2
       INTEGER NPL1,NPL2,NPL,IFAIL1,IFAIL2,IFAIL,I,J,K,N1,N2,IP1,IP2,NP,
     -      IP1L,IP1LL,IS1,IS2,IL,M1,M2,IQMIN,IAUX,IT(MXCORN,2),
     -      IREF(MXCORN,2,2),NFOUND,IREFO(MXPLAN),ITYPO(MXPLAN),
     -      IREF1,IREF2,NREFO,ICOL1,ICOL2,IDIR,JP1,JP2,KP1,KP2,IMAX
C    -      ,l
       LOGICAL ADD,ONLIND,OK,KEEP,MARK1(MXEDGE),MARK2(MXEDGE),
     -      ADDED,INSIDE,EDGE,FIRST,CROSS
       EXTERNAL ONLIND
*** Initial setting of the number of produced planes.
       NREFO=0
*** Retrieve both planes.
       CALL PLABU2('READ',IREF1,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -      ICOL1,IFAIL1)
       CALL PLABU2('READ',IREF2,NPL2,XPL2,YPL2,ZPL2,APL2,BPL2,CPL2,DPL2,
     -      ICOL2,IFAIL2)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Unable to retrieve a'//
     -           ' projected polygon; skipped.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAOVL DEBUG   :''//
     -           '' Reference numbers: '',2I4)') IREF1,IREF2
            IFAIL=1
            RETURN
       ENDIF
*** If the size of either is 0, simply return.
       IF(NPL1.LE.2.OR.NPL2.LE.2)THEN
            KEEP=.TRUE.
            IFAIL=0
            RETURN
       ENDIF
*** Compute the various tolerances.
       XMIN1=XPL1(1)
       YMIN1=YPL1(1)
       XMAX1=XPL1(1)
       YMAX1=YPL1(1)
       XMEAN=0
       YMEAN=0
       ZMEAN=0
       DO 10 I=1,NPL1
       XMIN1=MIN(XMIN1,XPL1(I))
       YMIN1=MIN(YMIN1,YPL1(I))
       XMAX1=MAX(XMAX1,XPL1(I))
       YMAX1=MAX(YMAX1,YPL1(I))
       XMEAN=XMEAN+XPL1(I)
       YMEAN=YMEAN+YPL1(I)
       ZMEAN=ZMEAN+ZPL1(I)
10     CONTINUE
       XMIN2=XPL2(1)
       YMIN2=YPL2(1)
       XMAX2=XPL2(1)
       YMAX2=YPL2(1)
       DO 20 I=1,NPL2
       XMIN2=MIN(XMIN2,XPL2(I))
       YMIN2=MIN(YMIN2,YPL2(I))
       XMAX2=MAX(XMAX2,XPL2(I))
       YMAX2=MAX(YMAX2,YPL2(I))
       XMEAN=XMEAN+XPL2(I)
       YMEAN=YMEAN+YPL2(I)
       ZMEAN=ZMEAN+ZPL2(I)
20     CONTINUE
       XMIN=MIN(XMIN1,XMIN2)
       YMIN=MIN(YMIN1,YMIN2)
       XMAX=MAX(XMAX1,XMAX2)
       YMAX=MAX(YMAX1,YMAX2)
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
       ELSE
            EPSX=1.0D-6*MAX(ABS(XMAX),ABS(XMIN))
            EPSY=1.0D-6*MAX(ABS(YMAX),ABS(YMIN))
       ENDIF
       XMEAN=XMEAN/DBLE(NPL1+NPL2)
       YMEAN=YMEAN/DBLE(NPL1+NPL2)
       ZMEAN=ZMEAN/DBLE(NPL1+NPL2)
*   Override the z-tolerance.
       EPSD=1.0E-6
       EPSZ=1.0E-6
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAOVL DEBUG   :'',
     -      '' Tolerances: x='',E12.5,'', y='',E12.5)')
     -      EPSX,EPSY
*   Preset the ZPL array to the mean z value.
       DO 40 I=1,MXEDGE
       ZPL(I)=ZMEAN
40     CONTINUE
*** Establish the list of special points around polygon 1.
       N1=0
       OK=.TRUE.
       DO 100 I=1,NPL1
*   Add the vertex.
       IF(N1+1.GT.MXCORN)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Too many special'//
     -           ' points around a polygon ; list reduced.'
            OK=.FALSE.
            GOTO 150
       ENDIF
       N1=N1+1
       XL(N1,1)=XPL1(I)
       YL(N1,1)=YPL1(I)
       IT(N1,1)=1
       Q(N1,1)=0
*   If also on 2 or vertex of 2, flag it as crossing or foreign.
       DO 160 J=1,NPL2
       IF(ABS(XPL2(J)-XPL1(I)).LT.EPSX.AND.
     -      ABS(YPL2(J)-YPL1(I)).LT.EPSY)IT(N1,1)=2
       IF(ONLIND(XPL2(1+MOD(J-1,NPL2)),YPL2(1+MOD(J-1,NPL2)),
     -           XPL2(1+MOD(J  ,NPL2)),YPL2(1+MOD(J  ,NPL2)),
     -           XPL1(I              ),YPL1(I)             ).AND.
     -      (ABS(XPL2(1+MOD(J-1,NPL2))-XPL1(I)).GT.EPSX.OR.
     -       ABS(YPL2(1+MOD(J-1,NPL2))-YPL1(I)).GT.EPSY).AND.
     -      (ABS(XPL2(1+MOD(J  ,NPL2))-XPL1(I)).GT.EPSX.OR.
     -       ABS(YPL2(1+MOD(J  ,NPL2))-YPL1(I)).GT.EPSY))IT(N1,1)=3
160    CONTINUE
*   Remember the starting point for the next list.
       M1=N1+1
*   Go over the line segments of the other polygon.
       DO 110 J=1,NPL2
*   Add vertices of 2 that are on this line.
       IF(ONLIND(XPL1(1+MOD(I-1,NPL1)),YPL1(1+MOD(I-1,NPL1)),
     -      XPL1(1+MOD(I,NPL1)),YPL1(1+MOD(I,NPL1)),
     -      XPL2(J),YPL2(J)).AND.
     -      (ABS(XPL1(1+MOD(I-1,NPL1))-XPL2(J)).GT.EPSX.OR.
     -       ABS(YPL1(1+MOD(I-1,NPL1))-YPL2(J)).GT.EPSY).AND.
     -      (ABS(XPL1(1+MOD(I  ,NPL1))-XPL2(J)).GT.EPSX.OR.
     -       ABS(YPL1(1+MOD(I  ,NPL1))-YPL2(J)).GT.EPSY))THEN
            IF(N1+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLAOVL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 150
            ENDIF
            N1=N1+1
            XL(N1,1)=XPL2(J)
            YL(N1,1)=YPL2(J)
            IT(N1,1)=2
       ENDIF
*   Add crossing points.
       CALL CRSPND(
     -      XPL1(1+MOD(I-1,NPL1)),YPL1(1+MOD(I-1,NPL1)),
     -      XPL1(1+MOD(I  ,NPL1)),YPL1(1+MOD(I  ,NPL1)),
     -      XPL2(1+MOD(J-1,NPL2)),YPL2(1+MOD(J-1,NPL2)),
     -      XPL2(1+MOD(J  ,NPL2)),YPL2(1+MOD(J  ,NPL2)),
     -      XC,YC,ADD)
       IF(ADD)THEN
            IF((ABS(XPL1(1+MOD(I-1,NPL1))-XC).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(I-1,NPL1))-YC).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(I  ,NPL1))-XC).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(I  ,NPL1))-YC).LT.EPSY))ADD=.FALSE.
            IF((ABS(XPL2(1+MOD(J-1,NPL2))-XC).LT.EPSX.AND.
     -          ABS(YPL2(1+MOD(J-1,NPL2))-YC).LT.EPSY).OR.
     -         (ABS(XPL2(1+MOD(J  ,NPL2))-XC).LT.EPSX.AND.
     -          ABS(YPL2(1+MOD(J  ,NPL2))-YC).LT.EPSY))ADD=.FALSE.
            IF((ABS(XPL1(1+MOD(I-1,NPL1))-
     -              XPL2(1+MOD(J-1,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(I-1,NPL1))-
     -              YPL2(1+MOD(J-1,NPL2))).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(I-1,NPL1))-
     -              XPL2(1+MOD(J  ,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(I-1,NPL1))-
     -              YPL2(1+MOD(J  ,NPL2))).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(I  ,NPL1))-
     -              XPL2(1+MOD(J-1,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(I  ,NPL1))-
     -              YPL2(1+MOD(J-1,NPL2))).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(I-1,NPL1))-
     -              XPL2(1+MOD(J-1,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(I-1,NPL1))-
     -              YPL2(1+MOD(J-1,NPL2))).LT.EPSY))ADD=.FALSE.
       ENDIF
       IF(ADD)THEN
            IF(N1+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLAOVL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 150
            ENDIF
            N1=N1+1
            XL(N1,1)=XC
            YL(N1,1)=YC
            IT(N1,1)=3
       ENDIF
110    CONTINUE
*   Compute the lambda's for these points.
       DO 120 J=M1,N1
       CALL PLALAM(XPL1(1+MOD(I-1,NPL1)),XL(J,1),XPL1(1+MOD(I,NPL1)),
     -      YPL1(1+MOD(I-1,NPL1)),YL(J,1),YPL1(1+MOD(I,NPL1)),Q(J,1))
120    CONTINUE
*   Sort the list by using the lambda's.
       DO 140 J=M1,N1
       QMIN=Q(J,1)
       IQMIN=J
       DO 130 K=J+1,N1
       IF(Q(K,1).LT.QMIN)THEN
            IQMIN=K
            QMIN=Q(K,1)
       ENDIF
130    CONTINUE
       IF(J.NE.IQMIN)THEN
            XAUX=XL(J,1)
            YAUX=YL(J,1)
            QAUX=Q (J,1)
            IAUX=IT(J,1)
            XL(J,1)=XL(IQMIN,1)
            YL(J,1)=YL(IQMIN,1)
            Q (J,1)=Q (IQMIN,1)
            IT(J,1)=IT(IQMIN,1)
            XL(IQMIN,1)=XAUX
            YL(IQMIN,1)=YAUX
            Q (IQMIN,1)=QAUX
            IT(IQMIN,1)=IAUX
       ENDIF
140    CONTINUE
*   Next vertex.
100    CONTINUE
*** Establish the list of special points around polygon 2.
150    CONTINUE
       N2=0
       DO 200 I=1,NPL2
*   Add the vertex.
       IF(N2+1.GT.MXCORN)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Too many special'//
     -           ' points around a polygon ; list reduced.'
            OK=.FALSE.
            GOTO 250
       ENDIF
       N2=N2+1
       XL(N2,2)=XPL2(I)
       YL(N2,2)=YPL2(I)
       IT(N2,2)=1
       Q(N2,2)=0
*   If also on 1 or a vertex of 1, flag it as crossing or foreign.
       DO 260 J=1,NPL1
       IF(ABS(XPL1(J)-XPL2(I)).LT.EPSX.AND.
     -      ABS(YPL1(J)-YPL2(I)).LT.EPSY)IT(N2,2)=2
       IF(ONLIND(XPL1(1+MOD(J-1,NPL1)),YPL1(1+MOD(J-1,NPL1)),
     -           XPL1(1+MOD(J  ,NPL1)),YPL1(1+MOD(J  ,NPL1)),
     -           XPL2(I              ),YPL2(I)             ).AND.
     -      (ABS(XPL1(1+MOD(J-1,NPL1))-XPL2(I)).GT.EPSX.OR.
     -       ABS(YPL1(1+MOD(J-1,NPL1))-YPL2(I)).GT.EPSY).AND.
     -      (ABS(XPL1(1+MOD(J  ,NPL1))-XPL2(I)).GT.EPSX.OR.
     -       ABS(YPL1(1+MOD(J  ,NPL1))-YPL2(I)).GT.EPSY))IT(N2,2)=3
260    CONTINUE
*   Remember the starting point for the next list.
       M2=N2+1
*   Go over the line segments of the other polygon.
       DO 210 J=1,NPL1
*   Add vertices of 1 that are on this line.
       IF(ONLIND(XPL2(1+MOD(I-1,NPL2)),YPL2(1+MOD(I-1,NPL2)),
     -      XPL2(1+MOD(I,NPL2)),YPL2(1+MOD(I,NPL2)),
     -      XPL1(J),YPL1(J)).AND.
     -      (ABS(XPL2(1+MOD(I-1,NPL2))-XPL1(J)).GT.EPSX.OR.
     -       ABS(YPL2(1+MOD(I-1,NPL2))-YPL1(J)).GT.EPSY).AND.
     -      (ABS(XPL2(1+MOD(I  ,NPL2))-XPL1(J)).GT.EPSX.OR.
     -       ABS(YPL2(1+MOD(I  ,NPL2))-YPL1(J)).GT.EPSY))THEN
            IF(N2+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLAOVL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 250
            ENDIF
            N2=N2+1
            XL(N2,2)=XPL1(J)
            YL(N2,2)=YPL1(J)
            IT(N2,2)=2
       ENDIF
*   Add crossing points.
       CALL CRSPND(
     -      XPL2(1+MOD(I-1,NPL2)),YPL2(1+MOD(I-1,NPL2)),
     -      XPL2(1+MOD(I  ,NPL2)),YPL2(1+MOD(I  ,NPL2)),
     -      XPL1(1+MOD(J-1,NPL1)),YPL1(1+MOD(J-1,NPL1)),
     -      XPL1(1+MOD(J  ,NPL1)),YPL1(1+MOD(J  ,NPL1)),
     -      XC,YC,ADD)
       IF(ADD)THEN
            IF((ABS(XPL2(1+MOD(I-1,NPL2))-XC).LT.EPSX.AND.
     -           ABS(YPL2(1+MOD(I-1,NPL2))-YC).LT.EPSY).OR.
     -         (ABS(XPL2(1+MOD(I,NPL2))-XC).LT.EPSX.AND.
     -           ABS(YPL2(1+MOD(I,NPL2))-YC).LT.EPSY))ADD=.FALSE.
            IF((ABS(XPL1(1+MOD(J-1,NPL1))-XC).LT.EPSX.AND.
     -           ABS(YPL1(1+MOD(J-1,NPL1))-YC).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(J,NPL1))-XC).LT.EPSX.AND.
     -           ABS(YPL1(1+MOD(J,NPL1))-YC).LT.EPSY))ADD=.FALSE.
            IF((ABS(XPL1(1+MOD(J-1,NPL1))-
     -              XPL2(1+MOD(I-1,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(J-1,NPL1))-
     -              YPL2(1+MOD(I-1,NPL2))).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(J-1,NPL1))-
     -              XPL2(1+MOD(I  ,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(J-1,NPL1))-
     -              YPL2(1+MOD(I  ,NPL2))).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(J  ,NPL1))-
     -              XPL2(1+MOD(I-1,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(J  ,NPL1))-
     -              YPL2(1+MOD(I-1,NPL2))).LT.EPSY).OR.
     -         (ABS(XPL1(1+MOD(J-1,NPL1))-
     -              XPL2(1+MOD(I-1,NPL2))).LT.EPSX.AND.
     -          ABS(YPL1(1+MOD(J-1,NPL1))-
     -              YPL2(1+MOD(I-1,NPL2))).LT.EPSY))ADD=.FALSE.
       ENDIF
       IF(ADD)THEN
            IF(N2+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLAOVL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 250
            ENDIF
            N2=N2+1
            XL(N2,2)=XC
            YL(N2,2)=YC
            IT(N2,2)=3
       ENDIF
210    CONTINUE
*   Compute the lambda's for these points.
       DO 220 J=M2,N2
       CALL PLALAM(XPL2(1+MOD(I-1,NPL2)),XL(J,2),XPL2(1+MOD(I,NPL2)),
     -      YPL2(1+MOD(I-1,NPL2)),YL(J,2),YPL2(1+MOD(I,NPL2)),Q(J,2))
220    CONTINUE
*   Sort the list by using the lambda's.
       DO 240 J=M2,N2
       QMIN=Q(J,2)
       IQMIN=J
       DO 230 K=J+1,N2
       IF(Q(K,2).LT.QMIN)THEN
            IQMIN=K
            QMIN=Q(K,2)
       ENDIF
230    CONTINUE
       IF(J.NE.IQMIN)THEN
            XAUX=XL(J,2)
            YAUX=YL(J,2)
            QAUX=Q (J,2)
            IAUX=IT(J,2)
            XL(J,2)=XL(IQMIN,2)
            YL(J,2)=YL(IQMIN,2)
            Q (J,2)=Q (IQMIN,2)
            IT(J,2)=IT(IQMIN,2)
            XL(IQMIN,2)=XAUX
            YL(IQMIN,2)=YAUX
            Q (IQMIN,2)=QAUX
            IT(IQMIN,2)=IAUX
       ENDIF
240    CONTINUE
*   Next vertex.
200    CONTINUE
*** Look up the cross-links: from plane 1 to plane 2.
       DO 300 I=1,N1
       IREF(I,1,1)=I
       NFOUND=0
       IREF(I,1,2)=0
       DO 310 J=1,N2
       IF(ABS(XL(I,1)-XL(J,2)).LT.EPSX.AND.
     -      ABS(YL(I,1)-YL(J,2)).LT.EPSY)THEN
            NFOUND=NFOUND+1
            IREF(I,1,2)=J
       ENDIF
310    CONTINUE
       IF(NFOUND.EQ.0.AND.(IT(I,1).EQ.2.OR.IT(I,1).EQ.3))THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Expected match not'//
     -           ' found (1-2)'
            IREF(I,1,2)=0
            OK=.FALSE.
       ELSEIF(NFOUND.GT.1)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : More than 1 match'//
     -           ' found (1-2).'
            IREF(I,1,2)=0
            OK=.FALSE.
       ENDIF
300    CONTINUE
**  Links from plane 2 to plane 1.
       DO 320 I=1,N2
       IREF(I,2,2)=I
       NFOUND=0
       IREF(I,2,1)=0
       DO 330 J=1,N1
       IF(ABS(XL(I,2)-XL(J,1)).LT.EPSX.AND.
     -      ABS(YL(I,2)-YL(J,1)).LT.EPSY)THEN
            NFOUND=NFOUND+1
            IREF(I,2,1)=J
       ENDIF
330    CONTINUE
       IF(NFOUND.EQ.0.AND.(IT(I,2).EQ.2.OR.IT(I,2).EQ.3))THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Expected match not'//
     -           ' found (2-1).'
            IREF(I,2,1)=0
            OK=.FALSE.
       ELSEIF(NFOUND.GT.1)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : More than 1 match'//
     -           ' found (2-1).'
            IREF(I,2,1)=0
            OK=.FALSE.
       ENDIF
320    CONTINUE
*   List the points for debugging.
       IF(LDEBUG)THEN
            DO 340 J=1,2
            WRITE(LUNOUT,'(''  ++++++ PLAOVL DEBUG   : Polygon '',I1,
     -           '':''/26X,''No Type            x            y'',
     -           ''         Q links'')') J
            CALL GSMK(2)
            IF(J.EQ.1)THEN
                 NP=N1
                 CALL GSMK(2)
            ELSEIF(J.EQ.2)THEN
                 NP=N2
                 CALL GSMK(4)
            ENDIF
            DO 350 I=1,NP
            WRITE(LUNOUT,'(25X,I3,I5,2F13.6,F10.3,2I3)') I,IT(I,J),
     -           XL(I,J),YL(I,J),Q(I,J),(IREF(I,J,K),K=1,2)
            CALL GPM2(1,XL(I,J),YL(I,J))
350         CONTINUE
340         CONTINUE
       ENDIF
       IF(.NOT.OK)RETURN
*** See whether all of 1 is inside 2.
       DO 1000 I=1,N1
       IF(IT(I,1).NE.1)GOTO 1200
       CALL INTERD(NPL2,XPL2,YPL2,XL(I,1),YL(I,1),INSIDE,EDGE)
       IF(.NOT.(INSIDE.OR.EDGE))GOTO 1200
1000   CONTINUE
**  Apparently 1 really is fully inside 2, write out curve 1.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Curve 1 fully inside 2'')')
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            ITYPO(NREFO)=3
            CALL PLABU2('STORE',IREFO(NREFO),NPL1,XPL1,YPL1,ZPL1,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                ' too long.'
            RETURN
       ENDIF
C       call gpl2(npl1,xpl1,ypl1)
*   Check there will be room for this.
       IF(NPL1+NPL2.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Output panel has too'//
     -           ' many vertices; abandoned.'
            RETURN
       ENDIF
*   Find 2 non-crossing connections: JP1-JP2 and KP1-KP2.
       FIRST=.TRUE.
       DO 1020 IP1=1,N1
       DO 1030 IP2=1,N2
       IF((.NOT.FIRST).AND.IP2.EQ.JP2)GOTO 1030
       DO 1040 K=1,N1
       IF(K.EQ.IP1.OR.1+MOD(K,N1).EQ.IP1)GOTO 1040
       CALL CRSPND(
     -      XL(IP1,1),        YL(IP1,1)        ,
     -      XL(IP2,2),        YL(IP2,2)        ,
     -      XL(K,1),          YL(K,1)          ,
     -      XL(1+MOD(K,N1),1),YL(1+MOD(K,N1),1),
     -      XC,YC,CROSS)
       IF(CROSS)GOTO 1030
1040   CONTINUE
       DO 1050 K=1,N2
       IF(K.EQ.IP2.OR.1+MOD(K,N2).EQ.IP2)GOTO 1050
       CALL CRSPND(
     -      XL(IP1,1),        YL(IP1,1)        ,
     -      XL(IP2,2),        YL(IP2,2)        ,
     -      XL(K,2),          YL(K,2)          ,
     -      XL(1+MOD(K,N2),2),YL(1+MOD(K,N2),2),
     -      XC,YC,CROSS)
       IF(CROSS)GOTO 1030
1050   CONTINUE
       IF(FIRST)THEN
            JP1=IP1
            JP2=IP2
            FIRST=.FALSE.
C            print *,' First junction: ',jp1,' (1) ',jp2,' (2)'
            GOTO 1020
       ELSE
            KP1=IP1
            KP2=IP2
            CALL CRSPND(
     -           XL(IP1,1), YL(IP1,1),
     -           XL(IP2,2), YL(IP2,2),
     -           XL(JP1,1), YL(JP1,1),
     -           XL(JP2,2), YL(JP2,2),
     -           XC,YC,CROSS)
            IF(.NOT.CROSS)GOTO 1060
       ENDIF
1030   CONTINUE
1020   CONTINUE
       PRINT *,' !!!!!! PLAOVL WARNING : Found no cut-out.'
       RETURN
1060   CONTINUE
C       print *,' Second junction: ',kp1,' (1) ',kp2,' (2)'
**  Create part 1 of area 2.
       NPL=0
       DO 1070 IP1=JP1,KP1
       NPL=NPL+1
       XPL(NPL)=XL(IP1,1)
       YPL(NPL)=YL(IP1,1)
1070   CONTINUE
*   Try one way.
       IF(JP2.LT.KP2)THEN
            IMAX=JP2+N2
       ELSE
            IMAX=JP2
       ENDIF
       IDIR=+1
       DO 1080 I=KP2,IMAX
       IP2=1+MOD(I-1,N2)
       NPL=NPL+1
       XPL(NPL)=XL(IP2,2)
       YPL(NPL)=YL(IP2,2)
1080   CONTINUE
*   Check for undesirable crossings.
       DO 1090 IP1=1,N1
       IF(IP1.EQ.JP1.OR.IP1.EQ.KP1)GOTO 1090
       CALL INTERD(NPL,XPL,YPL,XL(IP1,1),YL(IP1,1),INSIDE,EDGE)
       IF(INSIDE)GOTO 1110
1090   CONTINUE
       GOTO 1120
*   Use the other way if this failed
1110   CONTINUE
       NPL=KP1-JP1+1
       IF(JP2.LT.KP2)THEN
            IMAX=KP2
       ELSE
            IMAX=KP2+N2
       ENDIF
       IDIR=-1
       DO 1130 I=IMAX,JP2,-1
       IP2=1+MOD(I-1,N2)
       NPL=NPL+1
       XPL(NPL)=XL(IP2,2)
       YPL(NPL)=YL(IP2,2)
1130   CONTINUE
1120   CONTINUE
*   Save this part.
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            ITYPO(NREFO)=2
            CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -           APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                ' too long.'
       ENDIF
C       call gpl2(npl,xpl,ypl)
**  Create part 2 of area 2.
       NPL=0
       DO 1140 IP1=KP1,JP1+N1
       NPL=NPL+1
       XPL(NPL)=XL(1+MOD(IP1-1,N1),1)
       YPL(NPL)=YL(1+MOD(IP1-1,N1),1)
1140   CONTINUE
*   Add the part over area 2
       IF(IDIR.EQ.-1)THEN
            IF(JP2.GT.KP2)THEN
                 IMAX=JP2
            ELSE
                 IMAX=JP2+N2
            ENDIF
            IDIR=-1
            DO 1150 I=IMAX,KP2,-1
            IP2=1+MOD(I-1,N2)
            NPL=NPL+1
            XPL(NPL)=XL(IP2,2)
            YPL(NPL)=YL(IP2,2)
1150        CONTINUE
       ELSE
            IF(JP2.GT.KP2)THEN
                 IMAX=KP2+N2
            ELSE
                 IMAX=KP2
            ENDIF
            DO 1160 I=JP2,IMAX
            IP2=1+MOD(I-1,N2)
            NPL=NPL+1
            XPL(NPL)=XL(IP2,2)
            YPL(NPL)=YL(IP2,2)
1160        CONTINUE
       ENDIF
*   Save this part.
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            ITYPO(NREFO)=2
            CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -           APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                ' too long.'
       ENDIF
C       call gpl2(npl,xpl,ypl)
       RETURN
*** See whether all of 2 is inside 1.
1200   CONTINUE
       DO 1210 I=1,N2
       IF(IT(I,2).NE.1)GOTO 1400
       CALL INTERD(NPL1,XPL1,YPL1,XL(I,2),YL(I,2),INSIDE,EDGE)
       IF(.NOT.(INSIDE.OR.EDGE))GOTO 1400
1210   CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Curve 2 inside 1'')')
**  Apparently 2 really is fully inside 1, write out curve 2.
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            ITYPO(NREFO)=3
            CALL PLABU2('STORE',IREFO(NREFO),NPL2,XPL2,YPL2,ZPL2,
     -           APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                ' too long.'
       ENDIF
C       call gpl2(npl2,xpl2,ypl2)
*   Check there will be room for this.
       IF(NPL1+NPL2.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Output panel has too'//
     -           ' many vertices; abandoned.'
            RETURN
       ENDIF
*   Find 2 non-crossing connections: JP2-JP1 and KP2-KP1.
       FIRST=.TRUE.
       DO 1220 IP2=1,N2
       DO 1230 IP1=1,N1
       IF((.NOT.FIRST).AND.IP1.EQ.JP1)GOTO 1230
       DO 1240 K=1,N2
       IF(K.EQ.IP2.OR.1+MOD(K,N2).EQ.IP2)GOTO 1240
       CALL CRSPND(
     -      XL(IP2,2),        YL(IP2,2)        ,
     -      XL(IP1,1),        YL(IP1,1)        ,
     -      XL(K,2),          YL(K,2)          ,
     -      XL(1+MOD(K,N2),2),YL(1+MOD(K,N2),2),
     -      XC,YC,CROSS)
       IF(CROSS)GOTO 1230
1240   CONTINUE
C       print *,' no crossing with 2'
       DO 1250 K=1,N1
       IF(K.EQ.IP1.OR.1+MOD(K,N1).EQ.IP1)GOTO 1250
       CALL CRSPND(
     -      XL(IP2,2),        YL(IP2,2)        ,
     -      XL(IP1,1),        YL(IP1,1)        ,
     -      XL(K,1),          YL(K,1)          ,
     -      XL(1+MOD(K,N1),1),YL(1+MOD(K,N1),1),
     -      XC,YC,CROSS)
       IF(CROSS)GOTO 1230
1250   CONTINUE
       IF(FIRST)THEN
            JP2=IP2
            JP1=IP1
            FIRST=.FALSE.
C            print *,' First junction: ',jp2,' (2) ',jp1,' (1)'
            GOTO 1220
       ELSE
            KP2=IP2
            KP1=IP1
            CALL CRSPND(
     -           XL(IP2,2), YL(IP2,2),
     -           XL(IP1,1), YL(IP1,1),
     -           XL(JP2,2), YL(JP2,2),
     -           XL(JP1,1), YL(JP1,1),
     -           XC,YC,CROSS)
            IF(.NOT.CROSS)GOTO 1260
       ENDIF
1230   CONTINUE
1220   CONTINUE
       PRINT *,' !!!!!! PLAOVL WARNING : Found no cut-out.'
       RETURN
1260   CONTINUE
C       print *,' Second junction: ',kp2,' (2) ',kp1,' (1)'
**  Create part 1 of area 1.
       NPL=0
       DO 1270 IP2=JP2,KP2
       NPL=NPL+1
       XPL(NPL)=XL(IP2,2)
       YPL(NPL)=YL(IP2,2)
1270   CONTINUE
*   Try one way.
       IF(JP1.LT.KP1)THEN
            IMAX=JP1+N1
       ELSE
            IMAX=JP1
       ENDIF
       IDIR=+1
       DO 1280 I=KP1,IMAX
       IP1=1+MOD(I-1,N1)
       NPL=NPL+1
       XPL(NPL)=XL(IP1,1)
       YPL(NPL)=YL(IP1,1)
1280   CONTINUE
*   Check for undesirable crossings.
       DO 1290 IP2=1,N2
       IF(IP2.EQ.JP2.OR.IP2.EQ.KP2)GOTO 1290
       CALL INTERD(NPL,XPL,YPL,XL(IP2,2),YL(IP2,2),INSIDE,EDGE)
       IF(INSIDE)GOTO 1310
1290   CONTINUE
       GOTO 1320
*   Use the other way if this failed
1310   CONTINUE
       NPL=KP2-JP2+1
       IF(JP1.LT.KP1)THEN
            IMAX=KP1
       ELSE
            IMAX=KP1+N1
       ENDIF
       IDIR=-1
       DO 1330 I=IMAX,JP1,-1
       IP1=1+MOD(I-1,N1)
       NPL=NPL+1
       XPL(NPL)=XL(IP1,1)
       YPL(NPL)=YL(IP1,1)
1330   CONTINUE
1320   CONTINUE
*   Save this part.
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            ITYPO(NREFO)=1
            CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                ' too long.'
       ENDIF
C       call gpl2(npl,xpl,ypl)
**  Create part 2 of area 1.
       NPL=0
       DO 1340 IP2=KP2,JP2+N2
       NPL=NPL+1
       XPL(NPL)=XL(1+MOD(IP2-1,N2),2)
       YPL(NPL)=YL(1+MOD(IP2-1,N2),2)
1340   CONTINUE
*   Add the part over area 1
       IF(IDIR.EQ.-1)THEN
            IF(JP1.GT.KP1)THEN
                 IMAX=JP1
            ELSE
                 IMAX=JP1+N1
            ENDIF
            IDIR=-1
            DO 1350 I=IMAX,KP1,-1
            IP1=1+MOD(I-1,N1)
            NPL=NPL+1
            XPL(NPL)=XL(IP1,1)
            YPL(NPL)=YL(IP1,1)
1350        CONTINUE
       ELSE
            IF(JP1.GT.KP1)THEN
                 IMAX=KP1+N1
            ELSE
                 IMAX=KP1
            ENDIF
            DO 1360 I=JP1,IMAX
            IP1=1+MOD(I-1,N1)
            NPL=NPL+1
            XPL(NPL)=XL(IP1,1)
            YPL(NPL)=YL(IP1,1)
1360        CONTINUE
       ENDIF
*   Save this part.
       IF(NREFO+1.LE.MXPLAN)THEN
            NREFO=NREFO+1
            ITYPO(NREFO)=1
            CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
       ELSE
            PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                ' too long.'
       ENDIF
C       call gpl2(npl,xpl,ypl)
       RETURN
1400   CONTINUE
*** Identify the parts of 1 that are not overlapped, first mark.
       DO 400 I=1,MXEDGE
       MARK1(I)=.FALSE.
       MARK2(I)=.FALSE.
400    CONTINUE
**  Try and find a new starting point
430    CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Starting search for'',
     -      '' starting point on 1'')')
       DO 410 I=1,N1
*   Skip parts already processed.
       IF(MARK1(I).OR.MARK1(1+MOD(I,N1)))GOTO 410
*   Skip if mid point is inside other volume.
       CALL INTERD(NPL2,XPL2,YPL2,
     -      (XL(I,1)+XL(1+MOD(I,N1),1))/2,
     -      (YL(I,1)+YL(1+MOD(I,N1),1))/2,
     -      INSIDE,EDGE)
       IF(INSIDE.OR.EDGE)GOTO 410
*   Found one.
       IP1=I
       XPL(1)=XL(IP1,1)
       YPL(1)=YL(IP1,1)
       IS1=IP1
       MARK1(IP1)=.TRUE.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Start from point '',I5,
     -       '' on curve 1.'')') IP1
       IP1=1+MOD(IP1,N1)
       XPL(2)=XL(IP1,1)
       YPL(2)=YL(IP1,1)
       MARK1(IP1)=.TRUE.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Next point is '',I5,
     -      '' on 1.'')') IP1
       NPL=2
       IL=1
       IDIR=0
       GOTO 420
410    CONTINUE
*   Finished
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''No further non-overlapped'',
     -      '' areas of 1.'')')
       GOTO 500
*   Trace this part of 1 outside 2
420    CONTINUE
       IF(NPL+1.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Too many vertices on 1.'
            RETURN
*   On curve 1 and not on the edge of curve 2 ?
       ELSEIF(IL.EQ.1.AND.IT(MAX(1,IP1),IL).EQ.1)THEN
            IP1=1+MOD(IP1,N1)
            IF(IP1.EQ.IS1)THEN
                 IF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=1
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                     ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points'')') NPL
                 GOTO 430
            ENDIF
            MARK1(IP1)=.TRUE.
            NPL=NPL+1
            XPL(NPL)=XL(IP1,IL)
            YPL(NPL)=YL(IP1,IL)
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Went to point '',I5,
     -           '' on curve 1.'')') IP1
*   On curve 1 and on the edge of curve 2 ?
       ELSEIF(IL.EQ.1)THEN
            IP2=IREF(IP1,1,2)
            ADDED=.FALSE.
            IF(IDIR.EQ.+1.OR.IDIR.EQ.0)THEN
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                (XL(IP2,2)+XL(1+MOD(IP2,N2),2))/2,
     -                (YL(IP2,2)+YL(1+MOD(IP2,N2),2))/2,
     -                INSIDE,EDGE)
                 IF(INSIDE)THEN
                      IP2=1+MOD(IP2,N2)
                      IL=2
                      IDIR=+1
                      IP1=IREF(IP2,2,1)
                      IF(IP1.EQ.IS1)THEN
                           IF(NREFO+1.LE.MXPLAN)THEN
                                NREFO=NREFO+1
                                ITYPO(NREFO)=1
                                CALL PLABU2('STORE',IREFO(NREFO),
     -                               NPL,XPL,YPL,ZPL,
     -                               APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                           ELSE
                                PRINT *,' !!!!!! PLAOVL WARNING :'//
     -                               ' Output list too long.'
                           ENDIF
C                           call gpl2(npl,xpl,ypl)
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,''End'',
     -                          '' reached, '',I5,'' points.'')') NPL
                           GOTO 430
                      ELSEIF(IP1.NE.0)THEN
                           MARK1(IP1)=.TRUE.
                      ENDIF
                      NPL=NPL+1
                      XPL(NPL)=XL(IP2,2)
                      YPL(NPL)=YL(IP2,2)
                      ADDED=.TRUE.
C                      print *,' Added point ',ip2,' along 2 +'
                 ENDIF
            ENDIF
            IF(IDIR.EQ.-1.OR.IDIR.EQ.0)THEN
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                (XL(IP2,2)+XL(1+MOD(IP2+N2-2,N2),2))/2,
     -                (YL(IP2,2)+YL(1+MOD(IP2+N2-2,N2),2))/2,
     -                INSIDE,EDGE)
                 IF(INSIDE)THEN
                      IP2=1+MOD(IP2+N2-2,N2)
                      IL=2
                      IDIR=-1
                      IP1=IREF(IP2,2,1)
                      IF(IP1.EQ.IS1)THEN
                           IF(NREFO+1.LE.MXPLAN)THEN
                                NREFO=NREFO+1
                                ITYPO(NREFO)=1
                                CALL PLABU2('STORE',IREFO(NREFO),
     -                               NPL,XPL,YPL,ZPL,
     -                               APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                           ELSE
                                PRINT *,' !!!!!! PLAOVL WARNING :'//
     -                               ' Output list too long.'
                           ENDIF
C                           call gpl2(npl,xpl,ypl)
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                     '' reached, '',I5,'' points.'')') NPL
                           GOTO 430
                      ELSEIF(IP1.NE.0)THEN
                           MARK1(IP1)=.TRUE.
                      ENDIF
                      NPL=NPL+1
                      XPL(NPL)=XL(IP2,2)
                      YPL(NPL)=YL(IP2,2)
                      ADDED=.TRUE.
C                      print *,' Added point ',ip2,' along 2 -'
                 ENDIF
            ENDIF
            IF(.NOT.ADDED)THEN
                 IP1=1+MOD(IP1,N1)
                 IF(IP1.EQ.IS1)THEN
                      IF(NREFO+1.LE.MXPLAN)THEN
                           NREFO=NREFO+1
                           ITYPO(NREFO)=1
                           CALL PLABU2('STORE',IREFO(NREFO),
     -                          NPL,XPL,YPL,ZPL,
     -                          APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                      ELSE
                           PRINT *,' !!!!!! PLAOVL WARNING :'//
     -                          ' Output list too long.'
                      ENDIF
C                      call gpl2(npl,xpl,ypl)
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                     '' reached, '',I5,'' points.'')') NPL
                      GOTO 430
                 ELSEIF(IP1.NE.0)THEN
                      MARK1(IP1)=.TRUE.
                 ENDIF
                 NPL=NPL+1
                 XPL(NPL)=XL(IP1,IL)
                 YPL(NPL)=YL(IP1,IL)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Continued over 1'')')
            ENDIF
*   On curve 2 normal vertex (outside 1 hopefully).
       ELSEIF(IL.EQ.2.AND.IT(MAX(1,IP2),2).EQ.1)THEN
            IP2=1+MOD(IP2+IDIR-1+N2,N2)
            IP1=IREF(IP2,2,1)
            IF(IP1.EQ.IS1)THEN
                 IF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=1
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                     ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 430
            ELSEIF(IP1.NE.0)THEN
                 MARK1(IP1)=.TRUE.
            ENDIF
            NPL=NPL+1
            XPL(NPL)=XL(IP2,IL)
            YPL(NPL)=YL(IP2,IL)
C            print *,' Went to point ',ip2,' on 2.'
*   On curve 2 and on edge of 1
       ELSEIF(IL.EQ.2)THEN
            IP1=IREF(IP2,2,1)
            IP1=1+MOD(IP1,N1)
            IL=1
            IF(IP1.EQ.IS1)THEN
                 IF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=1
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                     ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 430
            ENDIF
            NPL=NPL+1
            XPL(NPL)=XL(IP1,IL)
            YPL(NPL)=YL(IP1,IL)
C            print *,' Resumed 1 at point ',ip1
*   Other cases should not occur
       ELSE
            PRINT *,' !!!!!! PLAOVL : Unexpected case.'
       ENDIF
       GOTO 420
*** Same for curve 2.
500    CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Starting search for'',
     -      '' starting point on 2'')')
       DO 510 I=1,N2
*   Skip parts already processed.
       IF(MARK2(I).OR.MARK2(1+MOD(I,N2)))GOTO 510
*   Skip if mid point is inside other volume.
       CALL INTERD(NPL1,XPL1,YPL1,
     -      (XL(I,2)+XL(1+MOD(I,N2),2))/2,
     -      (YL(I,2)+YL(1+MOD(I,N2),2))/2,
     -      INSIDE,EDGE)
       IF(INSIDE.OR.EDGE)GOTO 510
*   Found one.
       IP2=I
       XPL(1)=XL(IP2,2)
       YPL(1)=YL(IP2,2)
       IS2=IP2
       MARK2(IP2)=.TRUE.
C       print *,' Start from point ',ip2,' on 2.'
       IP2=1+MOD(IP2,N2)
       XPL(2)=XL(IP2,2)
       YPL(2)=YL(IP2,2)
       MARK2(IP2)=.TRUE.
C       print *,' Next point is ',ip2,' on 2.'
       NPL=2
       IL=2
       IDIR=0
       GOTO 520
510    CONTINUE
*   Finished
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''No further non-overlapped'',
     -      '' areas of 2.'')')
       GOTO 600
*   Trace this part of 2 outside 1
520    CONTINUE
       IF(NPL+1.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Too many vertices on 2.'
            RETURN
*   On curve 2 and not on the edge of curve 1 ?
       ELSEIF(IL.EQ.2.AND.IT(MAX(1,IP2),IL).EQ.1)THEN
            IP2=1+MOD(IP2,N2)
            IF(IP2.EQ.IS2)THEN
                 IF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=2
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                     ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 500
            ENDIF
            MARK2(IP2)=.TRUE.
            NPL=NPL+1
            XPL(NPL)=XL(IP2,IL)
            YPL(NPL)=YL(IP2,IL)
C            print *,' Went to point ',ip2,' on 2.'
*   On curve 2 and on the edge of curve 1 ?
       ELSEIF(IL.EQ.2)THEN
            IP1=IREF(IP2,2,1)
            ADDED=.FALSE.
            IF(IDIR.EQ.+1.OR.IDIR.EQ.0)THEN
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                (XL(IP1,1)+XL(1+MOD(IP1,N1),1))/2,
     -                (YL(IP1,1)+YL(1+MOD(IP1,N1),1))/2,
     -                INSIDE,EDGE)
                 IF(INSIDE)THEN
                      IP1=1+MOD(IP1,N1)
                      IL=1
                      IDIR=+1
                      IP2=IREF(IP1,1,2)
                      IF(IP2.EQ.IS2)THEN
                           IF(NREFO+1.LE.MXPLAN)THEN
                                NREFO=NREFO+1
                                ITYPO(NREFO)=2
                                CALL PLABU2('STORE',IREFO(NREFO),
     -                               NPL,XPL,YPL,ZPL,
     -                               APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
                           ELSE
                                PRINT *,' !!!!!! PLAOVL WARNING :'//
     -                               ' Output list too long.'
                           ENDIF
C                           call gpl2(npl,xpl,ypl)
C                           print *,' End reached, ',npl,' points'
                           GOTO 500
                      ELSEIF(IP2.NE.0)THEN
                           MARK2(IP2)=.TRUE.
                      ENDIF
                      NPL=NPL+1
                      XPL(NPL)=XL(IP1,1)
                      YPL(NPL)=YL(IP1,1)
                      ADDED=.TRUE.
C                      print *,' Added point ',ip1,' along 1 +'
                 ENDIF
            ENDIF
            IF(IDIR.EQ.-1.OR.IDIR.EQ.0)THEN
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                (XL(IP1,1)+XL(1+MOD(IP1+N1-2,N1),1))/2,
     -                (YL(IP1,1)+YL(1+MOD(IP1+N1-2,N1),1))/2,
     -                INSIDE,EDGE)
                 IF(INSIDE)THEN
                      IP1=1+MOD(IP1+N1-2,N1)
                      IL=1
                      IDIR=-1
                      IP2=IREF(IP1,1,2)
                      IF(IP2.EQ.IS2)THEN
                           IF(NREFO+1.LE.MXPLAN)THEN
                                NREFO=NREFO+1
                                ITYPO(NREFO)=2
                                CALL PLABU2('STORE',IREFO(NREFO),
     -                               NPL,XPL,YPL,ZPL,
     -                               APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
                           ELSE
                                PRINT *,' !!!!!! PLAOVL WARNING :'//
     -                               ' Output list too long.'
                           ENDIF
C                           call gpl2(npl,xpl,ypl)
C                           print *,' End reached, ',npl,' points'
                           GOTO 500
                      ELSEIF(IP2.NE.0)THEN
                           MARK2(IP2)=.TRUE.
                      ENDIF
                      NPL=NPL+1
                      XPL(NPL)=XL(IP1,1)
                      YPL(NPL)=YL(IP1,1)
                      ADDED=.TRUE.
C                      print *,' Added point ',ip1,' along 1 -'
                 ENDIF
            ENDIF
            IF(.NOT.ADDED)THEN
                 IP2=1+MOD(IP2,N2)
                 IF(IP2.EQ.IS2)THEN
                      IF(NREFO+1.LE.MXPLAN)THEN
                           NREFO=NREFO+1
                           ITYPO(NREFO)=2
                           CALL PLABU2('STORE',IREFO(NREFO),
     -                          NPL,XPL,YPL,ZPL,
     -                          APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
                      ELSE
                           PRINT *,' !!!!!! PLAOVL WARNING :'//
     -                          ' Output list too long.'
                      ENDIF
C                      call gpl2(npl,xpl,ypl)
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                     '' reached, '',I5,'' points.'')') NPL
                      GOTO 500
                 ELSEIF(IP2.NE.0)THEN
                      MARK2(IP2)=.TRUE.
                 ENDIF
                 NPL=NPL+1
                 XPL(NPL)=XL(IP2,IL)
                 YPL(NPL)=YL(IP2,IL)
C                 print *,' Continued over 2'
            ENDIF
*   On curve 1 normal vertex (outside 2 hopefully).
       ELSEIF(IL.EQ.1.AND.IT(MAX(1,IP1),1).EQ.1)THEN
            IP1=1+MOD(IP1+IDIR-1+N1,N1)
            IP2=IREF(IP1,1,2)
            IF(IP2.EQ.IS2)THEN
                 IF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=2
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                     ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 500
            ELSEIF(IP2.NE.0)THEN
                 MARK2(IP2)=.TRUE.
            ENDIF
            NPL=NPL+1
            XPL(NPL)=XL(IP1,IL)
            YPL(NPL)=YL(IP1,IL)
C            print *,' Went to point ',ip1,' on 1.'
*   On curve 1 and on edge of 2
       ELSEIF(IL.EQ.1)THEN
            IP2=IREF(IP1,1,2)
            IP2=1+MOD(IP2,N2)
            IL=2
            IF(IP2.EQ.IS2)THEN
                 IF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=2
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL2,BPL2,CPL2,DPL2,ICOL2,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                     ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 500
            ENDIF
            NPL=NPL+1
            XPL(NPL)=XL(IP2,IL)
            YPL(NPL)=YL(IP2,IL)
C            print *,' Resumed 1 at point ',ip2
*   Other cases should not occur
       ELSE
            PRINT *,' !!!!!! PLAOVL : Unexpected case.'
       ENDIF
       GOTO 520
*** Reset the flags.
600    CONTINUE
       DO 640 I=1,N1
       MARK1(I)=.FALSE.
640    CONTINUE
*** Look for the overlapped parts.
630    CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Starting search for'',
     -      '' starting point on overlap.'')')
       DO 610 I=1,N1
*   Skip points already processed.
       IF(MARK1(I))GOTO 610
*   Skip if not an edge point on both 1 and 2 or internal in 2.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''ip1 = '',2I5,'' ip2 = '',
     -       2I5)') I,IT(I,1),IREF(I,1,2),IT(MAX(1,IREF(I,1,2)),2)
       IP1=I
       IP2=IREF(IP1,1,2)
       IF(IP2.EQ.0.OR.IT(IP1,1).EQ.1)THEN
            CALL INTERD(NPL2,XPL2,YPL2,XL(IP1,1),YL(IP1,1),
     -           INSIDE,EDGE)
            IF(.NOT.(INSIDE.OR.EDGE))GOTO 610
       ELSEIF(IT(IP2,2).EQ.1)THEN
            GOTO 610
       ENDIF
*   Found one.
       NPL=1
       XPL(1)=XL(IP1,1)
       YPL(1)=YL(IP1,1)
       IS1=IP1
       IS2=IP2
       IL=1
       IDIR=0
       MARK1(IP1)=.TRUE.
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''Start from point '',I5,
     -      '' on curve '',I5)') IP1,IL
       GOTO 620
610    CONTINUE
*   Finished
       IF(LDEBUG)WRITE(LUNOUT,'(26X,''No further overlapped areas.'')')
       GOTO 700
*   Trace this overlapped part.
       IP1L=-1
       IP1LL=-1
620    CONTINUE
       IP1LL=IP1L
       IP1L=IP1
**  Is there still room ?
       IF(NPL+1.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAOVL WARNING : Too many vertices in'//
     -           ' overlapped part.'
            RETURN
**  On curve 1, see which way to continue.
       ELSEIF(IL.EQ.1)THEN
*   Maybe finished over line 1 ?
            IF(1+MOD(IP1,N1).EQ.IS1)THEN
                 IF(NPL.LE.2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Too few points'')')
                 ELSEIF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=3
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                    ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 630
            ENDIF
*   See whether the next point of 1 is on the edge or inside of 2.
            INSIDE=.FALSE.
            EDGE=.FALSE.
            IF(IREF(1+MOD(IP1,N1),1,2).GT.0)THEN
                 EDGE=.TRUE.
            ELSEIF(IT(1+MOD(IP1,N1),1).EQ.1)THEN
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                XL(1+MOD(IP1,N1),1),YL(1+MOD(IP1,N1),1),
     -                INSIDE,EDGE)
            ENDIF
*   If it is, check that it doesn't leave 2 at any stage.
            IF(INSIDE.OR.EDGE)CALL INTERD(NPL2,XPL2,YPL2,
     -           0.5*(XL(IP1,1)+XL(1+MOD(IP1,N1),1)),
     -           0.5*(YL(IP1,1)+YL(1+MOD(IP1,N1),1)),
     -           INSIDE,EDGE)
*   If it is, continue over 1.
            IF(INSIDE.OR.EDGE)THEN
                 IP1=1+MOD(IP1,N1)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Continued to point '',
     -                I5,'' on '',I5)') IP1,IL
                 NPL=NPL+1
                 XPL(NPL)=XL(IP1,1)
                 YPL(NPL)=YL(IP1,1)
                 MARK1(IP1)=.TRUE.
                 GOTO 620
            ENDIF
*   Else we have to continue over 2, ensure we really are on curve 2.
            IP2=IREF(IP1,1,2)
            IF(IP2.EQ.0)THEN
                 PRINT *,' !!!!!! PLAOVL WARNING : No point 2'//
     -                ' reference found; abandoned.'
                 RETURN
            ENDIF
*   Impose a direction on 2 to avoid returning.
            IF(IDIR.EQ.0)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Checking directions'',
     -                '' at point '',I5,'' of curve 2.''/26X,
     -                ''2+ ref on 1: '',I5,'', 2- ref on 1: '',I5)')
     -                IP2,IREF(1+MOD(IP2,N2),2,1),
     -                IREF(1+MOD(IP2-2+N2,N2),2,1)
                 IF(IREF(1+MOD(IP2,N2),2,1).EQ.IP1LL.AND.
     -              IREF(1+MOD(IP2-2+N2,N2),2,1).EQ.IP1LL)THEN
                      PRINT *,' !!!!!! PLAOVL WARNING : Both 2+'//
     -                     ' and 2- return on 1; not stored.'
                      GOTO 630
                 ELSEIF(IREF(1+MOD(IP2,N2),2,1).EQ.IP1LL)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''2+ is a return'',
     -                     '' to previous point on 1.'')')
                      IDIR=-1
                 ELSEIF(IREF(1+MOD(IP2-2+N2,N2),2,1).EQ.IP1LL)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''2- is a return'',
     -                     '' to previous point on 1.'')')
                      IDIR=+1
                 ELSE
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''Both ways are OK.'')')
                 ENDIF
            ENDIF
*   If not, try to continue over 2 in the + direction..
            IF(IDIR.EQ.+1.OR.IDIR.EQ.0)THEN
                 IP2=1+MOD(IP2,N2)
                 IF(IP2.EQ.IS2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Return to start over 2+'')')
                      IF(NPL.LE.2)THEN
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Too few points'')')
                      ELSEIF(NREFO+1.LE.MXPLAN)THEN
                           NREFO=NREFO+1
                           ITYPO(NREFO)=3
                           CALL PLABU2('STORE',IREFO(NREFO),
     -                          NPL,XPL,YPL,ZPL,
     -                          APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                      ELSE
                           PRINT *,' !!!!!! PLAOVL WARNING : Output'//
     -                         ' list too long.'
                      ENDIF
                      GOTO 630
                 ENDIF
                 CALL INTERD(NPL1,XPL1,YPL1,XL(IP2,2),YL(IP2,2),
     -                INSIDE,EDGE)
                 IF(INSIDE.OR.EDGE)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Going to 2+'',
     -                     '' (point '',I5,'' of 2)'')') IP2
                      NPL=NPL+1
                      XPL(NPL)=XL(IP2,2)
                      YPL(NPL)=YL(IP2,2)
                      IDIR=+1
                      IF(IREF(IP2,2,1).NE.0)THEN
                           IP1=IREF(IP2,2,1)
                           MARK1(IP1)=.TRUE.
                           IL=1
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,''This point'',
     -                          '' is also on curve 1: '',I5)') IP1
                      ELSE
                           IL=2
                      ENDIF
                      GOTO 620
                 ENDIF
                 IP2=1+MOD(IP2-2+N2,N2)
            ENDIF
*   Or if this still fails, try 2 in the - direction..
            IF(IDIR.EQ.-1.OR.IDIR.EQ.0)THEN
                 IP2=1+MOD(IP2-2+N2,N2)
                 IF(IP2.EQ.IS2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                     ''Return to start over 2-'')')
                      IF(NPL.LE.2)THEN
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,
     -                          ''Too few points'')')
                      ELSEIF(NREFO+1.LE.MXPLAN)THEN
                           NREFO=NREFO+1
                           ITYPO(NREFO)=3
                           CALL PLABU2('STORE',IREFO(NREFO),
     -                          NPL,XPL,YPL,ZPL,
     -                          APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                      ELSE
                           PRINT *,' !!!!!! PLAOVL WARNING : Output'//
     -                         ' list too long.'
                      ENDIF
                      GOTO 630
                 ENDIF
                 CALL INTERD(NPL1,XPL1,YPL1,XL(IP2,2),YL(IP2,2),
     -                INSIDE,EDGE)
                 IF(INSIDE.OR.EDGE)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Going to 2-'',
     -                     '' (point '',I5,'' of 2)'')') IP2
                      NPL=NPL+1
                      XPL(NPL)=XL(IP2,2)
                      YPL(NPL)=YL(IP2,2)
                      IDIR=-1
                      IF(IREF(IP2,2,1).NE.0)THEN
                           IP1=IREF(IP2,2,1)
                           MARK1(IP1)=.TRUE.
                           IL=1
                           IF(LDEBUG)WRITE(LUNOUT,'(26X,''This point'',
     -                          '' is also on 1: '',I5)') IP1
                      ELSE
                           IL=2
                      ENDIF
                      GOTO 620
                 ENDIF
             ENDIF
*   Should not get here
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Dead end.'')')
            GOTO 630
**  If we are on curve 2
       ELSEIF(IL.EQ.2)THEN
*   Ensure the direction is set
            IF(IDIR.EQ.0)THEN
                 PRINT *,' !!!!!! PLAOVL WARNING : Direction not'//
     -                    ' set; abandoned.'
                 RETURN
            ENDIF
*   Maybe finished over line 2 ?
            IP2=1+MOD(IP2+IDIR-1+N2,N2)
            IF(IP2.EQ.IS2)THEN
                 IF(NPL.LE.2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(26X,''Too few points'')')
                 ELSEIF(NREFO+1.LE.MXPLAN)THEN
                      NREFO=NREFO+1
                      ITYPO(NREFO)=3
                      CALL PLABU2('STORE',IREFO(NREFO),NPL,XPL,YPL,ZPL,
     -                     APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                 ELSE
                      PRINT *,' !!!!!! PLAOVL WARNING : Output list'//
     -                    ' too long.'
                 ENDIF
C                 call gpl2(npl,xpl,ypl)
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''End of curve'',
     -                '' reached, '',I5,'' points.'')') NPL
                 GOTO 630
            ENDIF
*   Next step over 2.
            IF(LDEBUG)WRITE(LUNOUT,'(26X,''Stepped over 2 to point '',
     -           I5,'' of 2.'')') IP2
            NPL=NPL+1
            XPL(NPL)=XL(IP2,2)
            YPL(NPL)=YL(IP2,2)
            IF(IREF(IP2,2,1).NE.0)THEN
                 IP1=IREF(IP2,2,1)
                 MARK1(IP1)=.TRUE.
                 IL=1
                 IF(LDEBUG)WRITE(LUNOUT,'(26X,''This point is also'',
     -                '' on curve 1: '',I5)') IP1
            ELSE
                 IL=2
            ENDIF
            GOTO 620
       ENDIF
700    CONTINUE
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLAOVL DEBUG   : Ready'',
     -      '' - no errors.'')')
*** Seems to have worked.
       IFAIL=0
       RETURN
*** Error.
250    CONTINUE
       PRINT *,' !!!!!! PLAOVL WARNING : Abandoning after error. '
       IFAIL=1
       END
