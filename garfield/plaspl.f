CDECK  ID>, PLASPL.
       SUBROUTINE PLASPL(IREF1,IREF2,NREF,IREFO,KEEP,IFAIL)
*-----------------------------------------------------------------------
*   PLASPL - Isolates the parts of plane 1 that are not hidden by 2.
*   (Last changed on 20/10/10.)
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
     -      XINT,YINT,ZINT,AINT,BINT,CDUM,EPSD,
     -      XSEPA,YSEPA,XSEPB,YSEPB,XMEAN,YMEAN,
     -      XPL(MXEDGE),YPL(MXEDGE),ZPL(MXEDGE),
     -      XCUT(MXCORN),YCUT(MXCORN),ZCUT(MXCORN),
     -      XL(MXCORN,3),YL(MXCORN,3),ZL(MXCORN,3),
     -      Q(MXCORN,3),QMIN,XAUX,YAUX,ZAUX,QAUX,
     -      XC,YC,ZC,EPSX,EPSY,EPSZ,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     -      ZAUX1,ZAUX2,ZAUX3,ZAUX4,
     -      XMIN1,YMIN1,ZMIN1,XMAX1,YMAX1,ZMAX1,
     -      XMIN2,YMIN2,ZMIN2,XMAX2,YMAX2,ZMAX2,
     -      X1,Y1,PHI0,PHI1,PHI2,PHI3,PHI4,PHI5,PHI6,PHIOPT,DX,DY,STEP
       INTEGER NPL1,NPL2,IFAIL1,IFAIL2,IFAIL,I,J,K,N1,N2,NS,
     -      M1,M2,IQMIN,IAUX,IT(MXCORN,3),IREF(MXCORN,3,3),
     -      NPL,IL,JL,IP,JP,JP2,JP3,NP,IDIR,JDIR,NFOUND,NFOUN1,NFOUN2,
     -      INITP,INITD,INITL,NCUT,J0,J1,K0,K1,IREFO(MXPLAN),
     -      IREF1,IREF2,NREF,ICOL1,ICOL2,IR,
     -      ISIDE0,ISIDE1,ISIDE2,ISIDE3,ISIDE4,ISIDE5,ISIDE6,
     -      N1L,N1R,N2L,N2R
       LOGICAL ADD,INSIDE,IN1,IN2,IN3,IN4,EDGE,EDGE1,EDGE2,EDGE3,EDGE4,
     -      ONLIND,CROSSD,START,OK,LSEP,MARK1(MXCORN),MARK2(MXCORN),
     -      SWAP,KEEP,HOLE
       EXTERNAL ONLIND,CROSSD
*** Initial setting of the number of produced planes.
       NREF=0
*** Retrieve both planes.
       CALL PLABU2('READ',IREF1,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -      ICOL1,IFAIL1)
       CALL PLABU2('READ',IREF2,NPL2,XPL2,YPL2,ZPL2,APL2,BPL2,CPL2,DPL2,
     -      ICOL2,IFAIL2)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Unable to retrieve a'//
     -           ' projected polygon; skipped.'
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASPL DEBUG   :''//
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
*   Don't process planes that have no z-component.
       IF(CPL1**2.LT.1.0D-6*(APL1**2+BPL1**2).OR.CPL1.EQ.0.OR.
     -      CPL2**2.LT.1.0D-6*(APL2**2+BPL2**2).OR.CPL2.EQ.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASPL DEBUG   :'',
     -           '' No z-component found; return with IFAIL=1.'')')
            IFAIL=1
            RETURN
       ENDIF
*** Don't try to split parallel planes.
       IF((BPL1*CPL2-BPL2*CPL1)**2+(CPL1*APL2-CPL2*APL1)**2+
     -      (APL1*BPL2-APL2*BPL1)**2.LT.
     -      1.0D-4*SQRT((APL1**2+BPL1**2+CPL1**2)*
     -      (APL2**2+BPL2**2+CPL2**2)))THEN
            LSEP=.FALSE.
            IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASPL DEBUG   :'',
     -           '' Parallel planes, no separation computed.'')')
*   Otherwise compute separation plane.
       ELSE
            CALL PLASEP(
     -           NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -           NPL2,XPL2,YPL2,ZPL2,APL2,BPL2,CPL2,DPL2,
     -           XINT,YINT,ZINT,AINT,BINT,CDUM,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Unable to compute'//
     -                ' a separation plane; plot may be incorrect.'
                 KEEP=.TRUE.
                 IFAIL=1
                 RETURN
            ENDIF
            LSEP=.TRUE.
       ENDIF
*** Compute the various tolerances.
       EPSD=0
       XMIN1=XPL1(1)
       YMIN1=YPL1(1)
       ZMIN1=ZPL1(1)
       XMAX1=XPL1(1)
       YMAX1=YPL1(1)
       ZMAX1=ZPL1(1)
       XMEAN=0
       YMEAN=0
       DO 10 I=1,NPL1
       EPSD=MAX(EPSD,ABS(APL2*XPL1(I)+BPL2*YPL1(I)+CPL2*ZPL1(I)))
       XMIN1=MIN(XMIN1,XPL1(I))
       YMIN1=MIN(YMIN1,YPL1(I))
       ZMIN1=MIN(ZMIN1,ZPL1(I))
       XMAX1=MAX(XMAX1,XPL1(I))
       YMAX1=MAX(YMAX1,YPL1(I))
       ZMAX1=MAX(ZMAX1,ZPL1(I))
       XMEAN=XMEAN+XPL1(I)
       YMEAN=YMEAN+YPL1(I)
10     CONTINUE
       XMIN2=XPL2(1)
       YMIN2=YPL2(1)
       ZMIN2=ZPL2(1)
       XMAX2=XPL2(1)
       YMAX2=YPL2(1)
       ZMAX2=ZPL2(1)
       DO 20 I=1,NPL2
       EPSD=MAX(EPSD,ABS(APL1*XPL2(I)+BPL1*YPL2(I)+CPL1*ZPL2(I)))
       XMIN2=MIN(XMIN2,XPL2(I))
       YMIN2=MIN(YMIN2,YPL2(I))
       ZMIN2=MIN(ZMIN2,ZPL2(I))
       XMAX2=MAX(XMAX2,XPL2(I))
       YMAX2=MAX(YMAX2,YPL2(I))
       ZMAX2=MAX(ZMAX2,ZPL2(I))
       XMEAN=XMEAN+XPL2(I)
       YMEAN=YMEAN+YPL2(I)
20     CONTINUE
       XMIN=MIN(XMIN1,XMIN2)
       YMIN=MIN(YMIN1,YMIN2)
       ZMIN=MIN(ZMIN1,ZMIN2)
       XMAX=MAX(XMAX1,XMAX2)
       YMAX=MAX(YMAX1,YMAX2)
       ZMAX=MAX(ZMAX1,ZMAX2)
       EPSD=1.0D-6*EPSD
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
            EPSZ=EPSGZ
       ELSE
            EPSX=1.0D-6*MAX(ABS(XMAX),ABS(XMIN))
            EPSY=1.0D-6*MAX(ABS(YMAX),ABS(YMIN))
            EPSZ=1.0D-6*MAX(ABS(ZMAX),ABS(ZMIN))
       ENDIF
       XMEAN=XMEAN/DBLE(NPL1+NPL2)
       YMEAN=YMEAN/DBLE(NPL1+NPL2)
*   Override the z-tolerance.
       EPSD=EPSZ
*   Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASPL DEBUG   :'',
     -      '' Tolerances: x='',E12.5,'', y='',E12.5/38X,
     -      ''z='',E12.5,'', d='',E12.5)') EPSX,EPSY,EPSZ,EPSD
*   If curve 1 is entirely above 2, simply keep.
       IF(ZMIN1.GE.ZMAX2)THEN
            KEEP=.TRUE.
            IFAIL=0
            RETURN
*   If the curves don't overlap at all, simply keep.
       ELSEIF(XMIN1.GE.XMAX2.OR.XMIN2.GE.XMAX1.OR.
     -      YMIN1.GE.YMAX2.OR.YMIN2.GE.YMAX1)THEN
            KEEP=.TRUE.
            IFAIL=0
            RETURN
*   Otherwise, try to eliminate pieces of curve 1.
       ELSE
            KEEP=.FALSE.
       ENDIF
*   Compute start and end point of a separation line.
       XAUX=ABS(XMAX-XMIN)
       XMIN=XMIN-XAUX
       XMAX=XMAX+XAUX
       YAUX=ABS(YMAX-YMIN)
       YMIN=YMIN-YAUX
       YMAX=YMAX+YAUX
       IF(LSEP.AND.ABS(AINT).GT.ABS(BINT).AND.AINT.NE.0)THEN
            XSEPA=XMIN
            YSEPA=YINT+(XMIN-XINT)*BINT/AINT
            XSEPB=XMAX
            YSEPB=YINT+(XMAX-XINT)*BINT/AINT
            CALL CLIP2D(XSEPA,YSEPA,XSEPB,YSEPB,XMIN,YMIN,XMAX,YMAX,
     -           IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 XSEPA=XMAX
                 YSEPA=YMAX
                 XSEPB=XMAX
                 YSEPB=YMAX
            ENDIF
       ELSEIF(LSEP.AND.BINT.NE.0)THEN
            XSEPA=XINT+(YMIN-YINT)*AINT/BINT
            YSEPA=YMIN
            XSEPB=XINT+(YMAX-YINT)*AINT/BINT
            YSEPB=YMAX
            CALL CLIP2D(XSEPA,YSEPA,XSEPB,YSEPB,XMIN,YMIN,XMAX,YMAX,
     -           IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 XSEPA=XMAX
                 YSEPA=YMAX
                 XSEPB=XMAX
                 YSEPB=YMAX
            ENDIF
       ELSE
            XSEPA=XMAX
            YSEPA=YMAX
            XSEPB=XMAX
            YSEPB=YMAX
       ENDIF
*   Show the separation line in debugging mode.
       IF(LDEBUG.AND.LSEP)THEN
            XPL(1)=XSEPA
            YPL(1)=YSEPA
            XPL(2)=XSEPB
            YPL(2)=YSEPB
            CALL GSLN(2)
            CALL GSPLCI(8)
            CALL GPL2(2,XPL,YPL)
       ENDIF
*** Check whether we have to do anything, first non-parallel planes.
       IF(LSEP)THEN
            N1L=0
            N1R=0
            N2L=0
            N2R=0
            DO 30 I=1,NPL1
            IF((XPL1(I)-XINT)*BINT-(YPL1(I)-YINT)*AINT.GT.EPSD)THEN
                 N1L=N1L+1
            ELSEIF((XPL1(I)-XINT)*BINT-(YPL1(I)-YINT)*AINT.LT.-EPSD)THEN
                 N1R=N1R+1
            ENDIF
            MARK1(I)=.FALSE.
30          CONTINUE
            DO 40 I=1,NPL2
            IF((XPL2(I)-XINT)*BINT-(YPL2(I)-YINT)*AINT.GT.EPSD)THEN
                 N2L=N2L+1
            ELSEIF((XPL2(I)-XINT)*BINT-(YPL2(I)-YINT)*AINT.LT.-EPSD)THEN
                 N2R=N2R+1
            ENDIF
            MARK2(I)=.FALSE.
40          CONTINUE
            IF((N1L.EQ.0.AND.N2R.EQ.0).OR.(N1R.EQ.0.AND.N2L.EQ.0))THEN
                 KEEP=.TRUE.
                 IFAIL=0
                 RETURN
            ELSE
                 KEEP=.FALSE.
            ENDIF
*   Next parallel planes.
       ELSE
            IF((DPL1-APL1*XMEAN-BPL1*YMEAN)/CPL1.GE.
     -            (DPL2-APL2*XMEAN-BPL2*YMEAN)/CPL2-EPSD)THEN
                 KEEP=.TRUE.
                 IFAIL=0
                 RETURN
            ELSE
                 KEEP=.FALSE.
            ENDIF
       ENDIF
*** Establish the list of special points around polygon 1.
       N1=0
       NS=0
       OK=.TRUE.
       DO 100 I=1,NPL1
*   Add the vertex.
       IF(N1+1.GT.MXCORN)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -           ' points around a polygon ; list reduced.'
            OK=.FALSE.
            GOTO 150
       ENDIF
       N1=N1+1
       XL(N1,1)=XPL1(I)
       YL(N1,1)=YPL1(I)
       ZL(N1,1)=ZPL1(I)
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
*   Preset HOLE to False, i.e. do look for intersect crossings.
       HOLE=.FALSE.
*   See whether this line segment crosses plane 2.
C      print *,' PLASPL case 1 call to PLALIN'
       CALL PLALIN(XPL1(1+MOD(I-1,NPL1)),YPL1(1+MOD(I-1,NPL1)),
     -      ZPL1(1+MOD(I-1,NPL1)),XPL1(1+MOD(I  ,NPL1)),
     -      YPL1(1+MOD(I  ,NPL1)),ZPL1(1+MOD(I  ,NPL1)),
     -      XPL2(1),YPL2(1),ZPL2(1),APL2,BPL2,CPL2,XC,YC,ZC,IFAIL1)
       IF(IFAIL1.EQ.0.AND.
     -      (ABS(XPL1(1+MOD(I-1,NPL1))-XC).GT.EPSX.OR.
     -       ABS(YPL1(1+MOD(I-1,NPL1))-YC).GT.EPSY).AND.
     -      (ABS(XPL1(1+MOD(I  ,NPL1))-XC).GT.EPSX.OR.
     -       ABS(YPL1(1+MOD(I  ,NPL1))-YC).GT.EPSY))THEN
*   Shouldn't be a located anywhere on the foreign curve.
            CALL INTERD(NPL2,XPL2,YPL2,XC,YC,INSIDE,EDGE)
            ADD=.NOT.EDGE
*   Add it to the list, if it remains.
            IF(ADD)THEN
                 IF(N1+1.GT.MXCORN)THEN
                      PRINT *,' !!!!!! PLASPL WARNING : Too many'//
     -                     ' points around a polygon ; list reduced.'
                      OK=.FALSE.
                      GOTO 150
                 ENDIF
                 N1=N1+1
                 XL(N1,1)=XC
                 YL(N1,1)=YC
                 ZL(N1,1)=ZC
                 IF(INSIDE)THEN
                      IT(N1,1)=4
                 ELSE
                      IT(N1,1)=5
                 ENDIF
*   If added, don't add the corners to the separation line.
                 MARK1(1+MOD(I-1,NPL1))=.TRUE.
                 MARK1(1+MOD(I  ,NPL1))=.TRUE.
*   Seems to be a hole.
                 HOLE=.TRUE.
            ENDIF
*   See whether the point is already in the separation list.
            DO 180 J=1,NS
            IF(ABS(XC-XL(J,3)).LT.EPSX.AND.
     -           ABS(YC-YL(J,3)).LT.EPSY)ADD=.FALSE.
180         CONTINUE
*   Add this to the separation points, if not already in it.
            IF(ADD)THEN
                 IF(NS+1.GT.MXCORN)THEN
                      PRINT *,' !!!!!! PLASPL WARNING : Too many'//
     -                     ' points around a polygon ; list reduced.'
                      OK=.FALSE.
                      GOTO 150
                 ENDIF
                 NS=NS+1
                 XL(NS,3)=XC
                 YL(NS,3)=YC
                 ZL(NS,3)=ZC
                 IF(INSIDE.AND..NOT.EDGE)THEN
                      IT(NS,3)=4
                 ELSE
                      IT(NS,3)=5
                 ENDIF
            ENDIF
       ENDIF
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
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 150
            ENDIF
            N1=N1+1
            XL(N1,1)=XPL2(J)
            YL(N1,1)=YPL2(J)
            ZL(N1,1)=(DPL1-APL1*XPL2(J)-BPL1*YPL2(J))/CPL1
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
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 150
            ENDIF
            N1=N1+1
            XL(N1,1)=XC
            YL(N1,1)=YC
            ZL(N1,1)=(DPL1-APL1*XC-BPL1*YC)/CPL1
            IT(N1,1)=3
       ENDIF
*   Perhaps also add to the separation list.
       IF(ADD.AND.ONLIND(XSEPA,YSEPA,XSEPB,YSEPB,XC,YC))THEN
            IF(NS+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 150
            ENDIF
            NS=NS+1
            XL(NS,3)=XC
            YL(NS,3)=YC
            ZL(NS,3)=(DPL1-APL1*XC-BPL1*YC)/CPL1
            IT(NS,3)=3
       ENDIF
110    CONTINUE
*   See whether this segment crosses the separation line.
       IF(.NOT.HOLE)THEN
            CALL CRSPND(
     -           XPL1(1+MOD(I-1,NPL1)),YPL1(1+MOD(I-1,NPL1)),
     -           XPL1(1+MOD(I  ,NPL1)),YPL1(1+MOD(I  ,NPL1)),
     -           XSEPA,YSEPA,XSEPB,YSEPB,XC,YC,ADD)
            IF(ADD)THEN
                 IF((ABS(XPL1(1+MOD(I-1,NPL1))-XC).LT.EPSX.AND.
     -               ABS(YPL1(1+MOD(I-1,NPL1))-YC).LT.EPSY).OR.
     -              (ABS(XPL1(1+MOD(I  ,NPL1))-XC).LT.EPSX.AND.
     -               ABS(YPL1(1+MOD(I  ,NPL1))-YC).LT.EPSY))ADD=.FALSE.
            ENDIF
            IF(ADD)THEN
                 DO 195 J=1,NPL2
                 IF(ABS(XC-XPL2(J)).LT.EPSX.AND.
     -                ABS(YC-YPL2(J)).LT.EPSY)ADD=.FALSE.
195              CONTINUE
            ENDIF
            IF(ADD)THEN
                 ADD=.TRUE.
                 DO 190 J=M1,N1
                 IF(ABS(XC-XL(J,1)).LT.EPSX.AND.
     -                ABS(YC-YL(J,1)).LT.EPSY)ADD=.FALSE.
190              CONTINUE
                 IF(ADD)THEN
                      IF(N1+1.GT.MXCORN)THEN
                           PRINT *,' !!!!!! PLASPL WARNING : Too many'//
     -                          ' special points around a polygon ;'//
     -                          ' list reduced.'
                           OK=.FALSE.
                           GOTO 150
                      ENDIF
                      N1=N1+1
                      XL(N1,1)=XC
                      YL(N1,1)=YC
                      ZL(N1,1)=(DPL1-APL1*XC-BPL1*YC)/CPL1
                      IT(N1,1)=1
                 ENDIF
                 ADD=.TRUE.
                 DO 170 J=1,NS
                 IF(ABS(XC-XL(J,3)).LT.EPSX.AND.
     -                ABS(YC-YL(J,3)).LT.EPSY)ADD=.FALSE.
170              CONTINUE
                 IF(ADD)THEN
                      IF(NS+1.GT.MXCORN)THEN
                           PRINT *,' !!!!!! PLASPL WARNING : Too many'//
     -                          ' special points around a polygon ;'//
     -                          ' list reduced.'
                           OK=.FALSE.
                           GOTO 150
                      ENDIF
                      NS=NS+1
                      XL(NS,3)=XC
                      YL(NS,3)=YC
                      ZL(NS,3)=(DPL1-APL1*XC-BPL1*YC)/CPL1
                      IT(NS,3)=1
                 ENDIF
            ENDIF
       ENDIF
*   Compute the lambda's for these points.
       DO 120 J=M1,N1
       CALL PLALAM(XPL1(1+MOD(I-1,NPL1)),XL(J,1),XPL1(1+MOD(I,NPL1)),
     -      YPL1(1+MOD(I-1,NPL1)),YL(J,1),YPL1(1+MOD(I,NPL1)),Q(J,1))
C      if(q(j,1).gt.1.5.and.lgstop)then
C           print *,' Case 1'
C           do k=1,npl1
C           print *,' 1: ',xpl1(k),ypl1(k),zpl1(k)
C           enddo
C           print *,' '
C           do k=1,npl2
C           print *,' 2: ',xpl2(k),ypl2(k),zpl2(k)
C           enddo
C      endif
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
            ZAUX=ZL(J,1)
            QAUX=Q (J,1)
            IAUX=IT(J,1)
            XL(J,1)=XL(IQMIN,1)
            YL(J,1)=YL(IQMIN,1)
            ZL(J,1)=ZL(IQMIN,1)
            Q (J,1)=Q (IQMIN,1)
            IT(J,1)=IT(IQMIN,1)
            XL(IQMIN,1)=XAUX
            YL(IQMIN,1)=YAUX
            ZL(IQMIN,1)=ZAUX
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
            PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -           ' points around a polygon ; list reduced.'
            OK=.FALSE.
            GOTO 250
       ENDIF
       N2=N2+1
       XL(N2,2)=XPL2(I)
       YL(N2,2)=YPL2(I)
       ZL(N2,2)=ZPL2(I)
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
*   See whether this line segment crosses plane 1.
C      print *,' PLASPL case 2 call to PLALIN'
       CALL PLALIN(XPL2(1+MOD(I-1,NPL2)),YPL2(1+MOD(I-1,NPL2)),
     -      ZPL2(1+MOD(I-1,NPL2)),XPL2(1+MOD(I  ,NPL2)),
     -      YPL2(1+MOD(I  ,NPL2)),ZPL2(1+MOD(I  ,NPL2)),
     -      XPL1(1),YPL1(1),ZPL1(1),APL1,BPL1,CPL1,XC,YC,ZC,IFAIL1)
       IF(IFAIL1.EQ.0.AND.
     -      (ABS(XPL2(1+MOD(I-1,NPL2))-XC).GT.EPSX.OR.
     -       ABS(YPL2(1+MOD(I-1,NPL2))-YC).GT.EPSY).AND.
     -      (ABS(XPL2(1+MOD(I  ,NPL2))-XC).GT.EPSX.OR.
     -       ABS(YPL2(1+MOD(I  ,NPL2))-YC).GT.EPSY))THEN
*   Shouldn't be a located anywhere on the foreign curve.
            CALL INTERD(NPL1,XPL1,YPL1,XC,YC,INSIDE,EDGE)
            ADD=.NOT.EDGE
*   Add this point to the list if not a vertex.
            IF(ADD)THEN
                 IF(N2+1.GT.MXCORN)THEN
                      PRINT *,' !!!!!! PLASPL WARNING : Too many'//
     -                     ' points around a polygon ; list reduced.'
                      OK=.FALSE.
                      GOTO 250
                 ENDIF
                 N2=N2+1
                 XL(N2,2)=XC
                 YL(N2,2)=YC
                 ZL(N2,2)=ZC
                 IF(INSIDE)THEN
                      IT(N2,2)=4
                 ELSE
                      IT(N2,2)=5
                 ENDIF
*   If added, don't add the corners to the separation line.
                 MARK2(1+MOD(I-1,NPL2))=.TRUE.
                 MARK2(1+MOD(I  ,NPL2))=.TRUE.
            ENDIF
*   See whether the point is already in the separation list.
            DO 280 J=1,NS
            IF(ABS(XC-XL(J,3)).LT.EPSX.AND.
     -           ABS(YC-YL(J,3)).LT.EPSY)ADD=.FALSE.
280         CONTINUE
*   Add this to the separation points, if not already in it.
            IF(ADD)THEN
                 IF(NS+1.GT.MXCORN)THEN
                      PRINT *,' !!!!!! PLASPL WARNING : Too many'//
     -                     ' points around a polygon ; list reduced.'
                      OK=.FALSE.
                      GOTO 250
                 ENDIF
                 NS=NS+1
                 XL(NS,3)=XC
                 YL(NS,3)=YC
                 ZL(NS,3)=ZC
                 IF(INSIDE)THEN
                      IT(NS,3)=4
                 ELSE
                      IT(NS,3)=5
                 ENDIF
            ENDIF
       ENDIF
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
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 250
            ENDIF
            N2=N2+1
            XL(N2,2)=XPL1(J)
            YL(N2,2)=YPL1(J)
            ZL(N2,2)=(DPL2-APL2*XPL1(J)-BPL2*YPL1(J))/CPL2
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
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points around a polygon ; list reduced.'
                 OK=.FALSE.
                 GOTO 250
            ENDIF
            N2=N2+1
            XL(N2,2)=XC
            YL(N2,2)=YC
            ZL(N2,2)=(DPL2-APL2*XC-BPL2*YC)/CPL2
            IT(N2,2)=3
       ENDIF
210    CONTINUE
*   Compute the lambda's for these points.
       DO 220 J=M2,N2
       CALL PLALAM(XPL2(1+MOD(I-1,NPL2)),XL(J,2),XPL2(1+MOD(I,NPL2)),
     -      YPL2(1+MOD(I-1,NPL2)),YL(J,2),YPL2(1+MOD(I,NPL2)),Q(J,2))
C      if(q(j,2).gt.1.5.and.lgstop)then
C           print *,' Case 2'
C           do k=1,npl1
C           print *,' 1: ',xpl1(k),ypl1(k),zpl1(k)
C           enddo
C           print *,' '
C           do k=1,npl2
C           print *,' 2: ',xpl2(k),ypl2(k),zpl2(k)
C           enddo
C      endif
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
            ZAUX=ZL(J,2)
            QAUX=Q (J,2)
            IAUX=IT(J,2)
            XL(J,2)=XL(IQMIN,2)
            YL(J,2)=YL(IQMIN,2)
            ZL(J,2)=ZL(IQMIN,2)
            Q (J,2)=Q (IQMIN,2)
            IT(J,2)=IT(IQMIN,2)
            XL(IQMIN,2)=XAUX
            YL(IQMIN,2)=YAUX
            ZL(IQMIN,2)=ZAUX
            Q (IQMIN,2)=QAUX
            IT(IQMIN,2)=IAUX
       ENDIF
240    CONTINUE
*   Next vertex.
200    CONTINUE
*** Establish the list of special points along the separation line.
250    CONTINUE
*   Add the vertices of plane 1 that are on the separation line.
       DO 300 I=1,NPL1
       IF(.NOT.MARK1(I).AND.
     -      ONLIND(XSEPA,YSEPA,XSEPB,YSEPB,XPL1(I),YPL1(I)))THEN
            IF(NS+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points along separation ; list reduced.'
                 OK=.FALSE.
                 GOTO 350
            ENDIF
            NS=NS+1
            XL(NS,3)=XPL1(I)
            YL(NS,3)=YPL1(I)
            ZL(NS,3)=ZPL1(I)
            IT(NS,3)=1
       ENDIF
300    CONTINUE
*   Add the vertices of plane 2 which are not also vertices of 1.
       DO 310 I=1,NPL2
       DO 360 J=1,NPL1
       IF(ABS(XPL2(I)-XPL1(J)).LT.EPSX.AND.
     -      ABS(YPL2(I)-YPL1(J)).LT.EPSY)GOTO 310
360    CONTINUE
       IF(.NOT.MARK2(I).AND.
     -      ONLIND(XSEPA,YSEPA,XSEPB,YSEPB,XPL2(I),YPL2(I)))THEN
            IF(NS+1.GT.MXCORN)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Too many special'//
     -                ' points along separation ; list reduced.'
                 OK=.FALSE.
                 GOTO 350
            ENDIF
            NS=NS+1
            XL(NS,3)=XPL2(I)
            YL(NS,3)=YPL2(I)
            ZL(NS,3)=ZPL2(I)
            CALL INTERD(NPL1,XPL1,YPL1,XPL2(I),YPL2(I),INSIDE,EDGE)
            IF(EDGE)THEN
                 IT(NS,3)=1
            ELSE
                 IT(NS,3)=2
            ENDIF
       ENDIF
310    CONTINUE
*   Compute the lambda's for these points.
       DO 320 I=1,NS
       CALL PLALAM(XSEPA,XL(I,3),XSEPB,YSEPA,YL(I,3),YSEPB,Q(I,3))
      if(q(i,3).gt.1.5.and.lgstop)then
           print *,' Case 3'
           do k=1,npl1
           print *,' 1: ',xpl1(k),ypl1(k),zpl1(k)
           enddo
           print *,' '
           do k=1,npl2
           print *,' 2: ',xpl2(k),ypl2(k),zpl2(k)
           enddo
           print *,' List of special points:'
           do k=1,ns
           print *,xl(k,3),yl(k,3)
           enddo
           print *,' Epsilons:'
           print *,epsx,epsy,epsz
      endif
320    CONTINUE
*   Sort the list by using the lambda's.
       DO 340 J=1,NS
       QMIN=Q(J,3)
       IQMIN=J
       DO 330 K=J+1,NS
       IF(Q(K,3).LT.QMIN)THEN
            IQMIN=K
            QMIN=Q(K,3)
       ENDIF
330    CONTINUE
       IF(J.NE.IQMIN)THEN
            XAUX=XL(J,3)
            YAUX=YL(J,3)
            ZAUX=ZL(J,3)
            QAUX=Q (J,3)
            IAUX=IT(J,3)
            XL(J,3)=XL(IQMIN,3)
            YL(J,3)=YL(IQMIN,3)
            ZL(J,3)=ZL(IQMIN,3)
            Q (J,3)=Q (IQMIN,3)
            IT(J,3)=IT(IQMIN,3)
            XL(IQMIN,3)=XAUX
            YL(IQMIN,3)=YAUX
            ZL(IQMIN,3)=ZAUX
            Q (IQMIN,3)=QAUX
            IT(IQMIN,3)=IAUX
       ENDIF
340    CONTINUE
*** Look up the cross-links.
350    CONTINUE
**  Links from plane 1 to plane 2.
       DO 500 I=1,N1
       IREF(I,1,1)=I
       NFOUND=0
       IREF(I,1,2)=0
       DO 510 J=1,N2
       IF(ABS(XL(I,1)-XL(J,2)).LT.EPSX.AND.
     -      ABS(YL(I,1)-YL(J,2)).LT.EPSY)THEN
            NFOUND=NFOUND+1
            IREF(I,1,2)=J
       ENDIF
510    CONTINUE
       IF(NFOUND.EQ.0.AND.(IT(I,1).EQ.2.OR.IT(I,1).EQ.3))THEN
            PRINT *,' !!!!!! PLASPL WARNING : Expected match not'//
     -           ' found (1-2)'
            OK=.FALSE.
            IREF(I,1,2)=0
       ELSEIF(NFOUND.GT.1)THEN
            PRINT *,' !!!!!! PLASPL WARNING : More than 1 match'//
     -           ' found (1-2).'
            OK=.FALSE.
            IREF(I,1,2)=0
       ENDIF
*   Links from plane 1 to the separation line.
       NFOUND=0
       IREF(I,1,3)=0
       DO 530 J=1,NS
       IF(ABS(XL(I,1)-XL(J,3)).LT.EPSX.AND.
     -      ABS(YL(I,1)-YL(J,3)).LT.EPSY)THEN
            NFOUND=NFOUND+1
            IREF(I,1,3)=J
       ENDIF
530    CONTINUE
       IF(NFOUND.EQ.0.AND.(IT(I,1).EQ.4.OR.IT(I,1).EQ.5))THEN
            PRINT *,' !!!!!! PLASPL WARNING : Expected match not'//
     -           ' found (1-S).'
            OK=.FALSE.
            IREF(I,1,3)=0
       ELSEIF(NFOUND.GT.1)THEN
            PRINT *,' !!!!!! PLASPL WARNING : More than 1 match'//
     -           ' found (1-S).'
            OK=.FALSE.
            IREF(I,1,3)=0
       ENDIF
500    CONTINUE
**  Links from plane 2 to plane 1.
       DO 540 I=1,N2
       IREF(I,2,2)=I
       NFOUND=0
       IREF(I,2,1)=0
       DO 550 J=1,N1
       IF(ABS(XL(I,2)-XL(J,1)).LT.EPSX.AND.
     -      ABS(YL(I,2)-YL(J,1)).LT.EPSY)THEN
            NFOUND=NFOUND+1
            IREF(I,2,1)=J
       ENDIF
550    CONTINUE
       IF(NFOUND.EQ.0.AND.(IT(I,2).EQ.2.OR.IT(I,2).EQ.3))THEN
            PRINT *,' !!!!!! PLASPL WARNING : Expected match not'//
     -           ' found (2-1).'
            OK=.FALSE.
            IREF(I,2,1)=0
       ELSEIF(NFOUND.GT.1)THEN
            PRINT *,' !!!!!! PLASPL WARNING : More than 1 match'//
     -           ' found (2-1).'
            OK=.FALSE.
            IREF(I,2,1)=0
       ENDIF
*   Links from plane 2 to the separation line.
       NFOUND=0
       IREF(I,2,3)=0
       DO 560 J=1,NS
       IF(ABS(XL(I,2)-XL(J,3)).LT.EPSX.AND.
     -      ABS(YL(I,2)-YL(J,3)).LT.EPSY)THEN
            NFOUND=NFOUND+1
            IREF(I,2,3)=J
       ENDIF
560    CONTINUE
       IF(NFOUND.EQ.0.AND.(IT(I,2).EQ.4.OR.IT(I,2).EQ.5))THEN
            PRINT *,' !!!!!! PLASPL WARNING : Expected match not'//
     -           ' found (2-S).'
            OK=.FALSE.
            IREF(I,2,3)=0
       ELSEIF(NFOUND.GT.1)THEN
            PRINT *,' !!!!!! PLASPL WARNING : More than 1 match'//
     -           ' found (2-S).'
            OK=.FALSE.
            IREF(I,2,3)=0
       ENDIF
540    CONTINUE
**  Links from the separation line to planes 1 and 2.
       DO 570 I=1,NS
       IREF(I,3,3)=I
       NFOUN1=0
       IREF(I,3,1)=0
       DO 580 J=1,N1
       IF(ABS(XL(I,3)-XL(J,1)).LT.EPSX.AND.
     -      ABS(YL(I,3)-YL(J,1)).LT.EPSY)THEN
            NFOUN1=NFOUN1+1
            IREF(I,3,1)=J
       ENDIF
580    CONTINUE
       IREF(I,3,2)=0
       NFOUN2=0
       DO 590 J=1,N2
       IF(ABS(XL(I,3)-XL(J,2)).LT.EPSX.AND.
     -      ABS(YL(I,3)-YL(J,2)).LT.EPSY)THEN
            NFOUN2=NFOUN2+1
            IREF(I,3,2)=J
       ENDIF
590    CONTINUE
       IF(NFOUN1.EQ.0.AND.NFOUN2.EQ.0)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Expected match not'//
     -           ' found (S-1,2).'
            OK=.FALSE.
            IREF(I,3,1)=0
            IREF(I,3,2)=0
       ELSEIF(NFOUN1.GT.1.OR.NFOUN2.GT.1)THEN
            PRINT *,' !!!!!! PLASPL WARNING : More than 1 match'//
     -           ' found (S-1,2).'
            OK=.FALSE.
            IREF(I,3,1)=0
            IREF(I,3,2)=0
       ENDIF
570    CONTINUE
*   List the points for debugging.
       IF(LDEBUG)THEN
            DO 610 J=1,3
            WRITE(LUNOUT,'(''  ++++++ PLASPL DEBUG   : Polygon '',I1,
     -           '':''/''   No Type            x            y'',
     -           ''            z         Q    links'')') J
            CALL GSMK(2)
            IF(J.EQ.1)THEN
                 NP=N1
                 CALL GSMK(2)
            ELSEIF(J.EQ.2)THEN
                 NP=N2
                 CALL GSMK(4)
            ELSEIF(J.EQ.3)THEN
                 NP=NS
                 CALL GSMK(5)
            ENDIF
            DO 600 I=1,NP
            WRITE(LUNOUT,'(2X,I3,I5,3F13.6,F10.3,3I3)') I,IT(I,J),
     -           XL(I,J),YL(I,J),ZL(I,J),Q(I,J),(IREF(I,J,K),K=1,3)
            CALL GPM2(1,XL(I,J),YL(I,J))
600         CONTINUE
C      call testtest(np,xl(1,J),yl(1,j),zl(1,j))
610         CONTINUE
       ENDIF
*** If a mistake was found, simply draw the curve.
       IF(.NOT.OK)THEN
            PRINT *,' !!!!!! PLASPL WARNING : No further processing'//
     -           ' because of the above errors ; please report.'
            LGSIG=.TRUE.
            DO 2020 I=1,NREF
            CALL PLABU2('DELETE',IREFO(I),NPL1,XPL1,YPL1,ZPL1,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
2020        CONTINUE
            NREF=0
            IFAIL=1
            KEEP=.TRUE.
            RETURN
       ENDIF
*** Draw the visible part of 1, first locate visible points.
       DO 400 I=1,N1
       IF(IREF(I,1,3).NE.0)THEN
            MARK1(I)=.TRUE.
       ELSEIF(IT(I,1).EQ.1)THEN
            CALL INTERD(NPL2,XPL2,YPL2,XL(I,1),YL(I,1),INSIDE,EDGE)
            IF(INSIDE.OR.EDGE)THEN
                 IF((DPL1-APL1*XL(I,1)-BPL1*YL(I,1))/CPL1.GE.
     -                (DPL2-APL2*XL(I,1)-BPL2*YL(I,1))/CPL2)THEN
                      MARK1(I)=.FALSE.
                 ELSE
                      MARK1(I)=.TRUE.
                 ENDIF
            ELSE
                 MARK1(I)=.FALSE.
            ENDIF
       ELSE
            MARK1(I)=.FALSE.
       ENDIF
400    CONTINUE
*** Resume from here for the next piece of curve.
410    CONTINUE
*** Find a point that still hasn't been marked.
       DO 420 I=1,N1
*   Skip points that are marked.
C      if(MARK1(i))print *,' Search skips point ',i,' (marked)'
       IF(MARK1(I))GOTO 420
*   Set reference variables.
       IP=I
       IL=1
*   See which side of the surve is visible.
       CALL INTERD(NPL2,XPL2,YPL2,
     -      0.5*(XL(IP,IL)+XL(1+MOD(IP,N1),IL)),
     -      0.5*(YL(IP,IL)+YL(1+MOD(IP,N1),IL)),IN1,EDGE1)
       ZAUX1=(DPL1-
     -      APL1*0.5*(XL(IP,IL)+XL(1+MOD(IP,N1),IL))-
     -      BPL1*0.5*(YL(IP,IL)+YL(1+MOD(IP,N1),IL)))/CPL1
       ZAUX2=(DPL2-
     -      APL2*0.5*(XL(IP,IL)+XL(1+MOD(IP,N1),IL))-
     -      BPL2*0.5*(YL(IP,IL)+YL(1+MOD(IP,N1),IL)))/CPL2
       CALL INTERD(NPL2,XPL2,YPL2,
     -      0.5*(XL(IP,IL)+XL(1+MOD(IP-2+N1,N1),IL)),
     -      0.5*(YL(IP,IL)+YL(1+MOD(IP-2+N1,N1),IL)),IN2,EDGE2)
       ZAUX3=(DPL1-
     -      APL1*0.5*(XL(IP,IL)+XL(1+MOD(IP-2+N1,N1),IL))-
     -      BPL1*0.5*(YL(IP,IL)+YL(1+MOD(IP-2+N1,N1),IL)))/CPL1
       ZAUX4=(DPL2-
     -      APL2*0.5*(XL(IP,IL)+XL(1+MOD(IP-2+N1,N1),IL))-
     -      BPL2*0.5*(YL(IP,IL)+YL(1+MOD(IP-2+N1,N1),IL)))/CPL2
*   Find the direction in which to move.
       IF(.NOT.(IN1.OR.EDGE1))THEN
            IDIR=+1
       ELSEIF(.NOT.(IN2.OR.EDGE2))THEN
            IDIR=-1
       ELSEIF(ZAUX1.GT.ZAUX2+EPSD)THEN
            IDIR=+1
       ELSEIF(ZAUX3.GT.ZAUX4+EPSD)THEN
            IDIR=-1
       ELSE
C      print *,' Search skips point ',i,' (no visible way out)'
            MARK1(I)=.TRUE.
            GOTO 410
       ENDIF
*   Leave the loop, we found a point.
       GOTO 440
420    CONTINUE
*** No point found anymore, continue with the cut-outs.
       GOTO 1000
*** Initial settings for the curve.
440    CONTINUE
       INITP=IP
       INITD=IDIR
       INITL=IL
       XPL(1)=XL(IP,1)
       YPL(1)=YL(IP,1)
       ZPL(1)=ZL(IP,1)
       MARK1(IP)=.TRUE.
       IP=1+MOD(IP+IDIR-1+N1,N1)
       NPL=1
       START=.TRUE.
       IF(LDEBUG)WRITE(LUNOUT,'(''  Starting from list '',I3,
     -      '' point '',I3,'' direction '',I2)') INITL,INITP,INITD
**  Make a step along the edges.
430    CONTINUE
       IF(IL.EQ.1.AND.IDIR.NE.INITD)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Change in direction on'//
     -           ' main curve ; abandoned.'
            DO 2040 I=1,NREF
            CALL PLABU2('DELETE',IREFO(I),NPL1,XPL1,YPL1,ZPL1,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
2040        CONTINUE
            NREF=0
            KEEP=.TRUE.
            IFAIL=1
            LGSIG=.TRUE.
            RETURN
       ENDIF
*** See whether we are back where we started.
       IF((.NOT.START).AND.
     -      ABS(XL(IP,IL)-XL(INITP,INITL)).LT.EPSX.AND.
     -      ABS(YL(IP,IL)-YL(INITP,INITL)).LT.EPSY)THEN
*   Store the plane.
            IF(NREF+1.LE.MXPLAN)THEN
                 CALL PLARED(NPL,XPL,YPL,ZPL,APL1,BPL1,CPL1,DPL1)
                 IF(NPL.GE.3)THEN
                      NREF=NREF+1
                      CALL PLABU2('STORE',IREFO(NREF),NPL,XPL,YPL,ZPL,
     -                     APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
                      IF(IFAIL1.NE.0)THEN
                           PRINT *,' !!!!!! PLASPL WARNING : Unable'//
     -                          ' to store a plane ; plot probably'//
     -                          ' incomplete.'
                           NREF=NREF-1
                      ENDIF
                 ENDIF
            ELSE
                 PRINT *,' !!!!!! PLASPL WARNING : Unable to store'//
     -                ' a plane ; plot probably incomplete.'
            ENDIF
*   And resume search.
            GOTO 410
       ENDIF
*** Now we have really started.
       START=.FALSE.
*   Mark the current point if we're in plane 1.
       IF(IREF(IP,IL,1).NE.0)MARK1(IREF(IP,IL,1))=.TRUE.
*   Set the number of points in the current list.
       IF(IL.EQ.1)THEN
            NP=N1
       ELSEIF(IL.EQ.2)THEN
            NP=N2
       ELSE
            NP=NS
       ENDIF
*   Add this point to the list if there still is room.
       IF(NPL+1.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Curve exceeds maximum'//
     -           ' length ; truncated.'
            LGSIG=.TRUE.
            DO 2010 I=1,NREF
            CALL PLABU2('DELETE',IREFO(I),NPL1,XPL1,YPL1,ZPL1,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
2010        CONTINUE
            NREF=0
            KEEP=.TRUE.
            IFAIL=1
            RETURN
       ENDIF
       IF(NPL.GE.2)THEN
            IF(.NOT.ONLIND(XPL(NPL-1),YPL(NPL-1),XL(IP,IL),YL(IP,IL),
     -           XPL(NPL),YPL(NPL)))NPL=NPL+1
       ELSE
            NPL=NPL+1
       ENDIF
       XPL(NPL)=XL(IP,IL)
       YPL(NPL)=YL(IP,IL)
       ZPL(NPL)=(DPL1-APL1*XL(IP,IL)-BPL1*YL(IP,IL))/CPL1
**  In debugging mode, print where we are now.
       IF(LDEBUG)WRITE(LUNOUT,'(''  Currently at list  '',I3,
     -      '' point '',I3,'' direction '',I2,'' type '',I1)')
     -      IL,IP,IDIR,IT(IP,IL)
**  If a private vertex, simply move on.
       IF(IT(IP,IL).EQ.1.AND.IL.NE.3)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  Own vertex.'')')
            IP=1+MOD(IP+IDIR-1+NP,NP)
            GOTO 430
**  If this is a triple intersect.
       ELSEIF(IREF(IP,IL,1).NE.0.AND.IREF(IP,IL,2).NE.0.AND.
     -      IREF(IP,IL,3).NE.0)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  Triple intersect, list 1: '',
     -           I3,'', list 2: '',I3,'' list 3: '',I3)')
     -           IREF(IP,IL,1),IREF(IP,IL,2),IREF(IP,IL,3)
*   Step size check, also used for side determination.
            STEP=SQRT(
     -           (XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))**2+
     -           (YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL))**2)
            IF(STEP.LE.0.OR.
     -           (IL.EQ.3.AND.IP.EQ. 1.AND.IDIR.EQ.+1).OR.
     -           (IL.EQ.3.AND.IP.EQ.NP.AND.IDIR.EQ.-1))THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Not a valid'//
     -                ' step into crossing ; skipped.'
                 LGSIG=.TRUE.
                 IP=1+MOD(IP+IDIR-1+NP,NP)
                 GOTO 1200
            ENDIF
*   Compute the incidence angle.
            PHI0=ATAN2(
     -           YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL),
     -           XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))
*   See on which side of this line we enter into 1.
            X1= (XL(1+MOD(IP-IDIR-1+NP,NP),IL)+XL(IP,IL))/2
            Y1= (YL(1+MOD(IP-IDIR-1+NP,NP),IL)+YL(IP,IL))/2
            DO 1210 K=3,10
            DX=-(YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            IF((IN1.OR.IN2).AND..NOT.(EDGE1.OR.EDGE2))THEN
                 CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
                 ZAUX3=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX4=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF(IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD))THEN
                      ISIDE0=+1
                 ELSEIF(IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD))THEN
                      ISIDE0=-1
                 ELSE
                      PRINT *,' !!!!!! PLASPL WARNING : Line does'//
     -                     ' not seem to follow a visible part of'//
     -                     ' plane 1 ; skipped.'
                      DO 2030 I=1,NREF
                      CALL PLABU2('DELETE',IREFO(I),
     -                     NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -                     ICOL1,IFAIL1)
2030                  CONTINUE
                      NREF=0
                      IFAIL=1
                      KEEP=.TRUE.
                 ENDIF
                 GOTO 1220
            ENDIF
1210        CONTINUE
            PRINT *,' !!!!!! PLASPL WARNING : Line doesn''t seem'//
     -           ' to follow plane 1 ; abandoning overlap test.'
            LGSIG=.TRUE.
            DO 2000 I=1,NREF
            CALL PLABU2('DELETE',IREFO(I),NPL1,XPL1,YPL1,ZPL1,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
2000        CONTINUE
            NREF=0
            KEEP=.TRUE.
            IFAIL=0
            RETURN
1220        CONTINUE
*   Check each branch for angle and 1-side, start with plane 1-.
            JP=IREF(IP,IL,1)
*   Compute the incidence angle.
            PHI1=MOD(ATAN2(
     -           YL(1+MOD(JP-2+N1,N1),1)-YL(IP,IL),
     -           XL(1+MOD(JP-2+N1,N1),1)-XL(IP,IL))-PHI0,2.0D0*PI)
            IF(PHI1.LT.-PI)PHI1=PHI1+2.0D0*PI
            IF(PHI1.GT.+PI)PHI1=PHI1-2.0D0*PI
            IF(ISIDE0.EQ.+1.AND.PHI1.LT.0)PHI1=PHI1+2.0D0*PI
            IF(ISIDE0.EQ.-1.AND.PHI1.GT.0)PHI1=PHI1-2.0D0*PI
*   See on which side of this line we enter visibly into 1.
            X1= (XL(1+MOD(JP-2+N1,N1),1)+XL(IP,IL))/2
            Y1= (YL(1+MOD(JP-2+N1,N1),1)+YL(IP,IL))/2
            STEP=SQRT(
     -           (XL(1+MOD(JP-2+N1,N1),1)-XL(IP,IL))**2+
     -           (YL(1+MOD(JP-2+N1,N1),1)-YL(IP,IL))**2)
            DO 1230 K=3,10
            DX=-(YL(1+MOD(JP-2+N1,N1),1)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(JP-2+N1,N1),1)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
            CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
            IF(IN1.AND..NOT.(EDGE1.OR.IN2.OR.EDGE2))THEN
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 IF(.NOT.(IN3.OR.EDGE3).OR.ZAUX1.GT.ZAUX2-EPSD)THEN
                      ISIDE1=+1
                 ELSE
                      ISIDE1=0
                 ENDIF
                 GOTO 1240
            ELSEIF(IN2.AND..NOT.(EDGE2.OR.IN1.OR.EDGE1))THEN
                 ZAUX1=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF(.NOT.(IN4.OR.EDGE4).OR.ZAUX1.GT.ZAUX2-EPSD)THEN
                      ISIDE1=-1
                 ELSE
                      ISIDE1=0
                 ENDIF
                 GOTO 1240
            ENDIF
1230        CONTINUE
            ISIDE1=0
1240        CONTINUE
*   Verify whether this branch is at all visible.
            CALL INTERD(NPL2,XPL2,YPL2,X1,Y1,IN1,EDGE1)
            ZAUX1=(DPL1-APL1*X1-BPL1*Y1)/CPL1
            ZAUX2=(DPL2-APL2*X1-BPL2*Y1)/CPL2
            IF((IN1.OR.EDGE1).AND.ZAUX1.LT.ZAUX2-EPSD)ISIDE1=0
*   Check plane 1+, compute the incidence angle.
            PHI2=MOD(ATAN2(
     -           YL(1+MOD(JP,N1),1)-YL(IP,IL),
     -           XL(1+MOD(JP,N1),1)-XL(IP,IL))-PHI0,2.0D0*PI)
            IF(PHI2.LT.-PI)PHI2=PHI2+2.0D0*PI
            IF(PHI2.GT.+PI)PHI2=PHI2-2.0D0*PI
            IF(ISIDE0.EQ.+1.AND.PHI2.LT.0)PHI2=PHI2+2.0D0*PI
            IF(ISIDE0.EQ.-1.AND.PHI2.GT.0)PHI2=PHI2-2.0D0*PI
*   See on which side of this line we enter visibly into 1.
            X1= (XL(1+MOD(JP,N1),1)+XL(IP,IL))/2
            Y1= (YL(1+MOD(JP,N1),1)+YL(IP,IL))/2
            STEP=SQRT(
     -           (XL(1+MOD(JP,N1),1)-XL(IP,IL))**2+
     -           (YL(1+MOD(JP,N1),1)-YL(IP,IL))**2)
            DO 1250 K=3,10
            DX=-(YL(1+MOD(JP,N1),1)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(JP,N1),1)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
            CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
            IF(IN1.AND..NOT.(EDGE1.OR.IN2.OR.EDGE2))THEN
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 IF(.NOT.(IN3.OR.EDGE3).OR.ZAUX1.GT.ZAUX2-EPSD)THEN
                      ISIDE2=+1
                 ELSE
                      ISIDE2=0
                 ENDIF
                 GOTO 1260
            ELSEIF(IN2.AND..NOT.(EDGE2.OR.IN1.OR.EDGE1))THEN
                 ZAUX1=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF(.NOT.(IN4.OR.EDGE4).OR.ZAUX1.GT.ZAUX2-EPSD)THEN
                      ISIDE2=-1
                 ELSE
                      ISIDE2=0
                 ENDIF
                 GOTO 1260
            ENDIF
1250        CONTINUE
            ISIDE2=0
1260        CONTINUE
*   Verify whether this branch is at all visible.
            CALL INTERD(NPL2,XPL2,YPL2,X1,Y1,IN1,EDGE1)
            ZAUX1=(DPL1-APL1*X1-BPL1*Y1)/CPL1
            ZAUX2=(DPL2-APL2*X1-BPL2*Y1)/CPL2
            IF((IN1.OR.EDGE1).AND.ZAUX1.LT.ZAUX2-EPSD)ISIDE2=0
*   Plane 2-.
            JP=IREF(IP,IL,2)
*   Compute the incidence angle.
            PHI3=MOD(ATAN2(
     -           YL(1+MOD(JP-2+N2,N2),2)-YL(IP,IL),
     -           XL(1+MOD(JP-2+N2,N2),2)-XL(IP,IL))-PHI0,2.0D0*PI)
            IF(PHI3.LT.-PI)PHI3=PHI3+2.0D0*PI
            IF(PHI3.GT.+PI)PHI3=PHI3-2.0D0*PI
            IF(ISIDE0.EQ.+1.AND.PHI3.LT.0)PHI3=PHI3+2.0D0*PI
            IF(ISIDE0.EQ.-1.AND.PHI3.GT.0)PHI3=PHI3-2.0D0*PI
*   See on which side of this line we enter visibly into 1.
            X1= (XL(1+MOD(JP-2+N2,N2),2)+XL(IP,IL))/2
            Y1= (YL(1+MOD(JP-2+N2,N2),2)+YL(IP,IL))/2
            STEP=SQRT(
     -           (XL(1+MOD(JP-2+N2,N2),2)-XL(IP,IL))**2+
     -           (YL(1+MOD(JP-2+N2,N2),2)-YL(IP,IL))**2)
            DO 1270 K=3,10
            DX=-(YL(1+MOD(JP-2+N2,N2),2)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(JP-2+N2,N2),2)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            IF((IN1.OR.IN2).AND..NOT.(EDGE1.OR.EDGE2))THEN
                 CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
                 ZAUX3=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX4=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF((IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD)).AND.
     -                (IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD)))THEN
                      ISIDE3=2
                 ELSEIF(IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD))THEN
                      ISIDE3=+1
                 ELSEIF(IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD))THEN
                      ISIDE3=-1
                 ELSE
                      ISIDE3=0
                 ENDIF
                 GOTO 1280
            ENDIF
1270        CONTINUE
            ISIDE3=0
1280        CONTINUE
*   Verify whether this branch is at all visible.
            ZAUX1=(DPL1-APL1*X1-BPL1*Y1)/CPL1
            ZAUX2=(DPL2-APL2*X1-BPL2*Y1)/CPL2
            IF(ZAUX2.LT.ZAUX1-EPSD)ISIDE3=0
*   Check plane 2+, compute the incidence angle.
            PHI4=MOD(ATAN2(
     -           YL(1+MOD(JP,N2),2)-YL(IP,IL),
     -           XL(1+MOD(JP,N2),2)-XL(IP,IL))-PHI0,2.0D0*PI)
            IF(PHI4.LT.-PI)PHI4=PHI4+2.0D0*PI
            IF(PHI4.GT.+PI)PHI4=PHI4-2.0D0*PI
            IF(ISIDE0.EQ.+1.AND.PHI4.LT.0)PHI4=PHI4+2.0D0*PI
            IF(ISIDE0.EQ.-1.AND.PHI4.GT.0)PHI4=PHI4-2.0D0*PI
*   See on which side of this line we enter visibly into 1.
            X1= (XL(1+MOD(JP,N2),2)+XL(IP,IL))/2
            Y1= (YL(1+MOD(JP,N2),2)+YL(IP,IL))/2
            STEP=SQRT(
     -           (XL(1+MOD(JP,N2),2)-XL(IP,IL))**2+
     -           (YL(1+MOD(JP,N2),2)-YL(IP,IL))**2)
            DO 1290 K=3,10
            DX=-(YL(1+MOD(JP,N2),2)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(JP,N2),2)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            IF((IN1.OR.IN2).AND..NOT.(EDGE1.OR.EDGE2))THEN
                 CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
                 ZAUX3=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX4=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF((IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD)).AND.
     -                (IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD)))THEN
                      ISIDE4=2
                 ELSEIF(IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD))THEN
                      ISIDE4=+1
                 ELSEIF(IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD))THEN
                      ISIDE4=-1
                 ELSE
                      ISIDE4=0
                 ENDIF
                 GOTO 1300
            ENDIF
1290        CONTINUE
            ISIDE4=0
1300        CONTINUE
*   Verify whether this branch is at all visible.
            ZAUX1=(DPL1-APL1*X1-BPL1*Y1)/CPL1
            ZAUX2=(DPL2-APL2*X1-BPL2*Y1)/CPL2
            IF(ZAUX2.LT.ZAUX1-EPSD)ISIDE4=0
*   Check separation line - side.
            JP=IREF(IP,IL,3)
*   Make sure we are at all allowed to go in this direction.
            IF(JP.LE.1)THEN
                 ISIDE5=0
                 PHI5=3*PI
                 GOTO 1320
            ENDIF
*   Compute the incidence angle.
            PHI5=MOD(ATAN2(
     -           YL(1+MOD(JP-2+NS,NS),3)-YL(IP,IL),
     -           XL(1+MOD(JP-2+NS,NS),3)-XL(IP,IL))-PHI0,2.0D0*PI)
            IF(PHI5.LT.-PI)PHI5=PHI5+2.0D0*PI
            IF(PHI5.GT.+PI)PHI5=PHI5-2.0D0*PI
            IF(ISIDE0.EQ.+1.AND.PHI5.LT.0)PHI5=PHI5+2.0D0*PI
            IF(ISIDE0.EQ.-1.AND.PHI5.GT.0)PHI5=PHI5-2.0D0*PI
*   See on which side of this line we enter visibly into 1.
            X1= (XL(1+MOD(JP-2+NS,NS),3)+XL(IP,IL))/2
            Y1= (YL(1+MOD(JP-2+NS,NS),3)+YL(IP,IL))/2
            STEP=SQRT(
     -           (XL(1+MOD(JP-2+NS,NS),3)-XL(IP,IL))**2+
     -           (YL(1+MOD(JP-2+NS,NS),3)-YL(IP,IL))**2)
            DO 1310 K=3,10
            DX=-(YL(1+MOD(JP-2+NS,NS),3)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(JP-2+NS,NS),3)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            IF((IN1.OR.IN2).AND..NOT.(EDGE1.OR.EDGE2))THEN
                 CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
                 ZAUX3=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX4=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF((IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD)).AND.
     -                (IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD)))THEN
                      ISIDE5=2
                 ELSEIF(IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD))THEN
                      ISIDE5=+1
                 ELSEIF(IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD))THEN
                      ISIDE5=-1
                 ELSE
                      ISIDE5=0
                 ENDIF
                 GOTO 1320
            ENDIF
1310        CONTINUE
            ISIDE5=0
1320        CONTINUE
*   Separation line, + side, can we go in this direction.
            IF(JP.GE.NS)THEN
                 ISIDE6=0
                 PHI6=3*PI
                 GOTO 1340
            ENDIF
*   Compute the incidence angle.
            PHI6=MOD(ATAN2(
     -           YL(1+MOD(JP,NS),3)-YL(IP,IL),
     -           XL(1+MOD(JP,NS),3)-XL(IP,IL))-PHI0,2.0D0*PI)
            IF(PHI6.LT.-PI)PHI6=PHI6+2.0D0*PI
            IF(PHI6.GT.+PI)PHI6=PHI6-2.0D0*PI
            IF(ISIDE0.EQ.+1.AND.PHI6.LT.0)PHI6=PHI6+2.0D0*PI
            IF(ISIDE0.EQ.-1.AND.PHI6.GT.0)PHI6=PHI6-2.0D0*PI
*   See on which side of this line we enter visibly into 1.
            X1= (XL(1+MOD(JP,NS),3)+XL(IP,IL))/2
            Y1= (YL(1+MOD(JP,NS),3)+YL(IP,IL))/2
            STEP=SQRT(
     -           (XL(1+MOD(JP,NS),3)-XL(IP,IL))**2+
     -           (YL(1+MOD(JP,NS),3)-YL(IP,IL))**2)
            DO 1330 K=3,10
            DX=-(YL(1+MOD(JP,NS),3)-YL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            DY=+(XL(1+MOD(JP,NS),3)-XL(IP,IL))*
     -           (2**K)*SQRT(EPSX**2+EPSY**2)/STEP
            CALL INTERD(NPL1,XPL1,YPL1,X1+DX,Y1+DY,IN1,EDGE1)
            CALL INTERD(NPL1,XPL1,YPL1,X1-DX,Y1-DY,IN2,EDGE2)
            IF((IN1.OR.IN2).AND..NOT.(EDGE1.OR.EDGE2))THEN
                 CALL INTERD(NPL2,XPL2,YPL2,X1+DX,Y1+DY,IN3,EDGE3)
                 ZAUX1=(DPL1-APL1*(X1+DX)-BPL1*(Y1+DY))/CPL1
                 ZAUX2=(DPL2-APL2*(X1+DX)-BPL2*(Y1+DY))/CPL2
                 CALL INTERD(NPL2,XPL2,YPL2,X1-DX,Y1-DY,IN4,EDGE4)
                 ZAUX3=(DPL1-APL1*(X1-DX)-BPL1*(Y1-DY))/CPL1
                 ZAUX4=(DPL2-APL2*(X1-DX)-BPL2*(Y1-DY))/CPL2
                 IF((IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD)).AND.
     -                (IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD)))THEN
                      ISIDE6=2
                 ELSEIF(IN1.AND.(.NOT.(IN3.OR.EDGE3).OR.
     -                ZAUX1.GT.ZAUX2-EPSD))THEN
                      ISIDE6=+1
                 ELSEIF(IN2.AND.(.NOT.(IN4.OR.EDGE4).OR.
     -                ZAUX3.GT.ZAUX4-EPSD))THEN
                      ISIDE6=-1
                 ELSE
                      ISIDE6=0
                 ENDIF
                 GOTO 1340
            ENDIF
1330        CONTINUE
            ISIDE6=0
1340        CONTINUE
*   Make sure we are at all allowed to go in this direction.
            IF(JP.GE.NS)ISIDE6=0
*   Don't follow 2+ or 2- is degenerate with s+ or s-.
            JP2=IREF(IP,IL,2)
            JP3=IREF(IP,IL,3)
            IF(IREF(1+MOD(JP2-2+N2,N2),2,3).EQ.1+MOD(JP3-2+NS,NS).AND.
     -           IREF(1+MOD(JP2-2+N2,N2),2,3).NE.0.AND.
     -           (ISIDE3*ISIDE5.EQ.-1.OR.ABS(ISIDE3*ISIDE5).GE.2).AND.
     -           ABS(PHI3-PHI5).LT.0.001)THEN
                 ISIDE3=0
                 ISIDE5=0
C      print *,' Eliminated 2-/s- degeneracy'
            ENDIF
            IF(IREF(1+MOD(JP2-2+N2,N2),2,3).EQ.1+MOD(JP3     ,NS).AND.
     -           IREF(1+MOD(JP2-2+N2,N2),2,3).NE.0.AND.
     -           (ISIDE3*ISIDE6.EQ.-1.OR.ABS(ISIDE3*ISIDE6).GE.2).AND.
     -           ABS(PHI3-PHI6).LT.0.001)THEN
                 ISIDE3=0
                 ISIDE6=0
C      print *,' Eliminated 2-/s+ degeneracy'
            ENDIF
            IF(IREF(1+MOD(JP2     ,N2),2,3).EQ.1+MOD(JP3-2+NS,NS).AND.
     -           IREF(1+MOD(JP2     ,N2),2,3).NE.0.AND.
     -           (ISIDE4*ISIDE5.EQ.-1.OR.ABS(ISIDE4*ISIDE5).GE.2).AND.
     -           ABS(PHI4-PHI5).LT.0.001)THEN
                 ISIDE4=0
                 ISIDE5=0
C      print *,' Eliminated 2+/s- degeneracy'
            ENDIF
            IF(IREF(1+MOD(JP2     ,N2),2,3).EQ.1+MOD(JP3     ,NS).AND.
     -           IREF(1+MOD(JP2     ,N2),2,3).NE.0.AND.
     -           (ISIDE4*ISIDE6.EQ.-1.OR.ABS(ISIDE4*ISIDE6).GE.2).AND.
     -           ABS(PHI4-PHI6).LT.0.001)THEN
                 ISIDE4=0
                 ISIDE6=0
C      print *,' Eliminated 2+/s+ degeneracy'
            ENDIF
*   Find the optimal branch to take.
            PHIOPT=3*PI
            IF(ISIDE0*ISIDE1.EQ.-1.AND.ISIDE0*PHI1.LT.PHIOPT-0.001)THEN
                 JDIR=-1
                 JP=1+MOD(IREF(IP,IL,1)+JDIR-1+N1,N1)
                 JL=1
                 PHIOPT=ISIDE0*PHI1
            ENDIF
            IF(ISIDE0*ISIDE2.EQ.-1.AND.ISIDE0*PHI2.LT.PHIOPT-0.001)THEN
                 JDIR=+1
                 JP=1+MOD(IREF(IP,IL,1)+JDIR-1+N1,N1)
                 JL=1
                 PHIOPT=ISIDE0*PHI2
            ENDIF
            IF(ISIDE0*ISIDE3.EQ.-1.AND.ISIDE0*PHI3.LT.PHIOPT-0.001)THEN
                 JDIR=-1
                 JP=1+MOD(IREF(IP,IL,2)+JDIR-1+N2,N2)
                 JL=2
                 PHIOPT=ISIDE0*PHI3
            ENDIF
            IF(ISIDE0*ISIDE4.EQ.-1.AND.ISIDE0*PHI4.LT.PHIOPT-0.001)THEN
                 JDIR=+1
                 JP=1+MOD(IREF(IP,IL,2)+JDIR-1+N2,N2)
                 JL=2
                 PHIOPT=ISIDE0*PHI4
            ENDIF
            IF(ISIDE0*ISIDE5.EQ.-1.AND.ISIDE0*PHI5.LT.PHIOPT-0.001)THEN
                 JDIR=-1
                 JP=1+MOD(IREF(IP,IL,3)+JDIR-1+NS,NS)
                 JL=3
                 PHIOPT=ISIDE0*PHI5
            ENDIF
            IF(ISIDE0*ISIDE6.EQ.-1.AND.ISIDE0*PHI6.LT.PHIOPT-0.001)THEN
                 JDIR=+1
                 JP=1+MOD(IREF(IP,IL,3)+JDIR-1+NS,NS)
                 JL=3
                 PHIOPT=ISIDE0*PHI6
            ENDIF
            IF(LDEBUG)WRITE(LUNOUT,'(
     -           5X,''Incoming, side='',I2,'' angle=         '',F10.3/
     -           5X,''List 1 -, side='',I2,'' relative angle='',F10.3/
     -           5X,''List 1 +, side='',I2,'' relative angle='',F10.3/
     -           5X,''List 2 -, side='',I2,'' relative angle='',F10.3/
     -           5X,''List 2 +, side='',I2,'' relative angle='',F10.3/
     -           5X,''Split  -, side='',I2,'' relative angle='',F10.3/
     -           5X,''Split  +, side='',I2,'' relative angle='',F10.3/
     -           5X,''Selected list '',I3,'' point '',I3,
     -           '' direction '',I3)')
     -           ISIDE0,PHI0,ISIDE1,PHI1,ISIDE2,PHI2,ISIDE3,PHI3,
     -           ISIDE4,PHI4,ISIDE5,PHI5,ISIDE6,PHI6,JL,JP,JDIR
*   See whether a solution has been found.
            IF(PHIOPT.GT.2.0D0*PI)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Did not find a'//
     -                ' way out of the triple crossing ; skipping.'
                 LGSIG=.TRUE.
                 IP=1+MOD(IP+IDIR-1+NP,NP)
            ELSE
                 IP=JP
                 IL=JL
                 IDIR=JDIR
            ENDIF
1200        CONTINUE
**  If this is an intersect or a vertex of the other plane.
       ELSEIF((IT(IP,IL).EQ.2.OR.IT(IP,IL).EQ.3).AND.IL.NE.3)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  Crossing / foreign vertex'')')
*   Compute offsets for plane 1 (ZAUX1) and for plane 2 (ZAUX2).
            ZAUX1=(DPL1-APL1*XL(IP,IL)-BPL1*YL(IP,IL))/CPL1
            ZAUX2=(DPL2-APL2*XL(IP,IL)-BPL2*YL(IP,IL))/CPL2
*   If on plane 2 and crossing under 1, follow 1 in old direction.
            IF(IL.EQ.2.AND.ZAUX1.GT.ZAUX2-EPSD)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(5X,''On 2, going under 1'')')
                 IL=1
                 IDIR=INITD
                 IP=1+MOD(IREF(IP,2,1)+IDIR-1+N1,N1)
*   If on plane 2 and crossing over 1, follow visible part of 1.
            ELSEIF(IL.EQ.2)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(5X,''On 2, going over 1'')')
                 JP=IREF(IP,IL,3-IL)
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                0.5*(XL(1+MOD(JP-2+N1,N1),1)+
     -                     XL(1+MOD(JP-1   ,N1),1)),
     -                0.5*(YL(1+MOD(JP-2+N1,N1),1)+
     -                     YL(1+MOD(JP-1   ,N1),1)),IN1,EDGE1)
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                0.5*(XL(1+MOD(JP     ,N1),1)+
     -                     XL(1+MOD(JP-1   ,N1),1)),
     -                0.5*(YL(1+MOD(JP     ,N1),1)+
     -                     YL(1+MOD(JP-1   ,N1),1)),IN2,EDGE2)
                 IF(.NOT.(IN1.OR.IN2.OR.EDGE1.OR.EDGE2))THEN
                      PHI0=ATAN2(
     -                     YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL),
     -                     XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))
                      PHI1=MOD(ATAN2(
     -                     YL(1+MOD(IP+IDIR-1+NP,NP),IL)-YL(IP,IL),
     -                     XL(1+MOD(IP+IDIR-1+NP,NP),IL)-XL(IP,IL))-
     -                     PHI0,2.0D0*PI)
                      IF(PHI1.LT.-PI)PHI1=PHI1+2*PI
                      IF(PHI1.GT.+PI)PHI1=PHI1-2*PI
                      PHI2=MOD(ATAN2(
     -                     YL(1+MOD(JP-2+N1,N1),1)-YL(IP,IL),
     -                     XL(1+MOD(JP-2+N1,N1),1)-XL(IP,IL))-
     -                     PHI0,2.0D0*PI)
                      IF(PHI2.LT.-PI)PHI2=PHI2+2*PI
                      IF(PHI2.GT.+PI)PHI2=PHI2-2*PI
                      PHI3=MOD(ATAN2(
     -                     YL(1+MOD(JP     ,N1),1)-YL(IP,IL),
     -                     XL(1+MOD(JP     ,N1),1)-XL(IP,IL))-
     -                     PHI0,2.0D0*PI)
                      IF(PHI3.LT.-PI)PHI3=PHI3+2*PI
                      IF(PHI3.GT.+PI)PHI3=PHI3-2*PI
                      IF((ABS(PHI2).LT.ABS(PHI1).AND.
     -                     PHI1*PHI2.GE.0).OR.
     -                     (ABS(PHI3).LT.ABS(PHI1).AND.
     -                     PHI1*PHI3.GE.0))THEN
                           IF(ABS(PHI2).LT.ABS(PHI3))THEN
                                IP=1+MOD(JP-2+N1,N1)
                                IDIR=-1
                           ELSE
                                IP=1+MOD(JP     ,N1)
                                IDIR=+1
                           ENDIF
                      ELSE
                           IF(PHI1.GT.0)THEN
                                IF(PHI2.LT.0)PHI2=PHI2+2*PI
                                IF(PHI3.LT.0)PHI3=PHI3+2*PI
                           ELSE
                                IF(PHI2.GT.0)PHI2=PHI2-2*PI
                                IF(PHI3.GT.0)PHI3=PHI3-2*PI
                           ENDIF
                           IF(ABS(PHI2).GT.ABS(PHI3))THEN
                                IP=1+MOD(JP-2+N1,N1)
                                IDIR=-1
                           ELSE
                                IP=1+MOD(JP     ,N1)
                                IDIR=+1
                           ENDIF
                      ENDIF
                      IL=1
                 ELSEIF(.NOT.(IN1.OR.EDGE1))THEN
                      IL=1
                      IDIR=-1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(.NOT.(IN2.OR.EDGE2))THEN
                      IL=1
                      IDIR=+1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSE
                      IL=2
                      IP=1+MOD(IP+IDIR-1+N2,N2)
                 ENDIF
*   If on plane 1 and crossing under 2, follow part of 2 entering 1.
            ELSEIF(IL.EQ.1.AND.ZAUX1.LT.ZAUX2-EPSD)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(5X,''On 1, going under 2'')')
                 JP=IREF(IP,IL,3-IL)
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                0.5*(XL(1+MOD(JP-2+N2,N2),2)+
     -                     XL(1+MOD(JP-1   ,N2),2)),
     -                0.5*(YL(1+MOD(JP-2+N2,N2),2)+
     -                     YL(1+MOD(JP-1   ,N2),2)),IN1,EDGE1)
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                0.5*(XL(1+MOD(JP     ,N2),2)+
     -                     XL(1+MOD(JP-1   ,N2),2)),
     -                0.5*(YL(1+MOD(JP     ,N2),2)+
     -                     YL(1+MOD(JP-1   ,N2),2)),IN2,EDGE2)
                 IF(IN1.AND.IN2)THEN
                      PHI0=ATAN2(
     -                     YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL),
     -                     XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))
                      PHI1=MOD(ATAN2(
     -                     YL(1+MOD(IP+IDIR-1+NP,NP),IL)-YL(IP,IL),
     -                     XL(1+MOD(IP+IDIR-1+NP,NP),IL)-XL(IP,IL))-
     -                     PHI0,2.0D0*PI)
                      IF(PHI1.LT.-PI)PHI1=PHI1+2*PI
                      IF(PHI1.GT.+PI)PHI1=PHI1-2*PI
                      PHI2=MOD(ATAN2(
     -                     YL(1+MOD(JP-2+N2,N2),2)-YL(IP,IL),
     -                     XL(1+MOD(JP-2+N2,N2),2)-XL(IP,IL))-
     -                     PHI0,2.0D0*PI)
                      IF(PHI2.LT.-PI)PHI2=PHI2+2*PI
                      IF(PHI2.GT.+PI)PHI2=PHI2-2*PI
                      PHI3=MOD(ATAN2(
     -                     YL(1+MOD(JP     ,N2),2)-YL(IP,IL),
     -                     XL(1+MOD(JP     ,N2),2)-XL(IP,IL))-
     -                     PHI0,2.0D0*PI)
                      IF(PHI3.LT.-PI)PHI3=PHI3+2*PI
                      IF(PHI3.GT.+PI)PHI3=PHI3-2*PI
                      IF((ABS(PHI2).LT.ABS(PHI1).AND.
     -                     PHI1*PHI2.GE.0).OR.
     -                     (ABS(PHI3).LT.ABS(PHI1).AND.
     -                     PHI1*PHI3.GE.0))THEN
                           IF(ABS(PHI2).LT.ABS(PHI3))THEN
                                IP=1+MOD(JP-2+N2,N2)
                                IDIR=-1
                           ELSE
                                IP=1+MOD(JP     ,N2)
                                IDIR=+1
                           ENDIF
                      ELSE
                           IF(PHI1.GT.0)THEN
                                IF(PHI2.LT.0)PHI2=PHI2+2*PI
                                IF(PHI3.LT.0)PHI3=PHI3+2*PI
                           ELSE
                                IF(PHI2.GT.0)PHI2=PHI2-2*PI
                                IF(PHI3.GT.0)PHI3=PHI3-2*PI
                           ENDIF
                           IF(ABS(PHI2).GT.ABS(PHI3))THEN
                                IP=1+MOD(JP-2+N2,N2)
                                IDIR=-1
                           ELSE
                                IP=1+MOD(JP     ,N2)
                                IDIR=+1
                           ENDIF
                      ENDIF
                      IL=2
                 ELSEIF(IN1)THEN
                      IL=2
                      IDIR=-1
                      IP=1+MOD(JP+IDIR-1+N2,N2)
                 ELSEIF(IN2)THEN
                      IL=2
                      IDIR=+1
                      IP=1+MOD(JP+IDIR-1+N2,N2)
                 ELSE
                      IL=1
                      IP=1+MOD(IP+IDIR-1+N1,N1)
                 ENDIF
*   If on plane 1 and crossing above 2, simply continue.
            ELSEIF(IL.EQ.1)THEN
                 IF(LDEBUG)WRITE(LUNOUT,'(5X,''On 1, going over 2'')')
                 IP=1+MOD(IP+IDIR-1+NP,NP)
            ENDIF
**  If this is a vertex lying on the intersection line.
       ELSEIF((IT(IP,IL).EQ.1.OR.IT(IP,IL).EQ.2).AND.IL.EQ.3)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  Crossing or vertex of list '',
     -           I3,'' on the separation line.'')') IT(IP,IL)
*   Check visibility ZAUX1/3 on plane 1, ZAUX2/4 on plane 2.
            IF(IT(IP,IL).EQ.1)THEN
                 JP=IREF(IP,3,1)
                 ZAUX1=(DPL1-
     -                APL1*(XL(IP,IL)+XL(1+MOD(JP-2+N1,N1),1))/2-
     -                BPL1*(YL(IP,IL)+YL(1+MOD(JP-2+N1,N1),1))/2)/CPL1
                 ZAUX2=(DPL2-
     -                APL2*(XL(IP,IL)+XL(1+MOD(JP-2+N1,N1),1))/2-
     -                BPL2*(YL(IP,IL)+YL(1+MOD(JP-2+N1,N1),1))/2)/CPL2
                 ZAUX3=(DPL1-
     -                APL1*(XL(IP,IL)+XL(1+MOD(JP,N1),1))/2-
     -                BPL1*(YL(IP,IL)+YL(1+MOD(JP,N1),1))/2)/CPL1
                 ZAUX4=(DPL2-
     -                APL2*(XL(IP,IL)+XL(1+MOD(JP,N1),1))/2-
     -                BPL2*(YL(IP,IL)+YL(1+MOD(JP,N1),1))/2)/CPL2
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                (XL(IP,IL)+XL(1+MOD(JP-2+N1,N1),1))/2,
     -                (YL(IP,IL)+YL(1+MOD(JP-2+N1,N1),1))/2,
     -                IN1,EDGE1)
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                (XL(IP,IL)+XL(1+MOD(JP,N1),1))/2,
     -                (YL(IP,IL)+YL(1+MOD(JP,N1),1))/2,
     -                IN2,EDGE2)
                 IF(.NOT.(IN1.OR.IN2.OR.EDGE1.OR.EDGE2))THEN
                      IF(((XL(1+MOD(JP-2+N1     ,N1),1 )-XL(IP,IL))*
     -                    (XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))+
     -                    (YL(1+MOD(JP-2+N1     ,N1),1 )-YL(IP,IL))*
     -                    (YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL)))*
     -                    SQRT((XL(1+MOD(JP     ,N1),1 )-XL(IP,IL))**2+
     -                         (YL(1+MOD(JP     ,N1),1 )-YL(IP,IL))**2)
     -                    .LT.
     -                   ((XL(1+MOD(JP          ,N1),1 )-XL(IP,IL))*
     -                    (XL(1+MOD(IP-IDIR-1+NP,NP),IL)-XL(IP,IL))+
     -                    (YL(1+MOD(JP          ,N1),1 )-YL(IP,IL))*
     -                    (YL(1+MOD(IP-IDIR-1+NP,NP),IL)-YL(IP,IL)))*
     -                    SQRT((XL(1+MOD(JP-2+N1,N1),1 )-XL(IP,IL))**2+
     -                         (YL(1+MOD(JP-2+N1,N1),1 )-YL(IP,IL))**2)
     -                  )THEN
                          IDIR=-1
                      ELSE
                          IDIR=+1
                      ENDIF
C      print *,'    Both ways visible, choosing ',IDIR
                      IL=1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF((.NOT.(IN1.OR.EDGE1).OR.ZAUX1.GE.ZAUX2).AND.
     -                (.NOT.(IN2.OR.EDGE2).OR.ZAUX3.GE.ZAUX4))THEN
C      print *,'    Choosing initial direction.'
                      IL=1
                      IDIR=INITD
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(.NOT.(IN1.OR.EDGE1))THEN
C      print *,'    Choosing -'
                      IL=1
                      IDIR=-1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(.NOT.(IN2.OR.EDGE2))THEN
C      print *,'    Choosing +'
                      IL=1
                      IDIR=+1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(ZAUX1.GE.ZAUX2)THEN
C      print *,'    Choosing -'
                      IL=1
                      IDIR=-1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(ZAUX3.GE.ZAUX4)THEN
C      print *,'    Choosing +'
                      IL=1
                      IDIR=+1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSE
                      PRINT *,' !!!!!! PLASPL WARNING : Found no way'//
     -                     ' out of a vertex on intersect.'
                      LGSIG=.TRUE.
                      IP=1+MOD(IP+IDIR-1+NP,NP)
                 ENDIF
*   Continue via plane 2.
            ELSEIF(IT(IP,IL).EQ.2)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Crossed plane 2',
     -                ' via the separation line; skipped.'
                 IP=1+MOD(IP+IDIR-1+NP,NP)
            ENDIF
**  If this is a hole in the other plane.
       ELSEIF(IT(IP,IL).EQ.4)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  Hole in other plane.'')')
*   If on plane 1, follow separation line entering plane 1.
            IF(IL.EQ.1)THEN
                 JP=IREF(IP,IL,3)
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                0.5*(XL(1+MOD(JP-2+NS,NS),3)+
     -                     XL(1+MOD(JP-1   ,NS),3)),
     -                0.5*(YL(1+MOD(JP-2+NS,NS),3)+
     -                     YL(1+MOD(JP-1   ,NS),3)),IN1,EDGE1)
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                0.5*(XL(1+MOD(JP     ,NS),3)+
     -                     XL(1+MOD(JP-1   ,NS),3)),
     -                0.5*(YL(1+MOD(JP     ,NS),3)+
     -                     YL(1+MOD(JP-1   ,NS),3)),IN2,EDGE2)
                 IF(JP.LE.1)THEN
                      IP=1
                      IDIR=+1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSEIF(JP.GE.NS)THEN
                      IP=NS
                      IDIR=-1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSEIF(IN1.OR.(EDGE1.AND..NOT.IN2))THEN
                      IP=JP
                      IDIR=-1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSEIF(IN2.OR.(EDGE2.AND..NOT.IN1))THEN
                      IP=JP
                      IDIR=+1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSE
                      IP=1+MOD(IP+IDIR-1+NP,NP)
                 ENDIF
*   If on plane 2, follow separation line entering plane 2.
            ELSEIF(IL.EQ.2)THEN
                 JP=IREF(IP,IL,3)
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                0.5*(XL(1+MOD(JP-2+NS,NS),3)+
     -                     XL(1+MOD(JP-1   ,NS),3)),
     -                0.5*(YL(1+MOD(JP-2+NS,NS),3)+
     -                     YL(1+MOD(JP-1   ,NS),3)),IN1,EDGE1)
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                0.5*(XL(1+MOD(JP     ,NS),3)+
     -                     XL(1+MOD(JP-1   ,NS),3)),
     -                0.5*(YL(1+MOD(JP     ,NS),3)+
     -                     YL(1+MOD(JP-1   ,NS),3)),IN2,EDGE2)
                 IF(JP.LE.1)THEN
                      IP=1
                      IDIR=+1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSEIF(JP.GE.NS)THEN
                      IP=NS
                      IDIR=-1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSEIF(IN1.OR.(EDGE1.AND..NOT.IN2))THEN
                      IP=JP
                      IDIR=-1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSEIF(IN2.OR.(EDGE2.AND..NOT.IN1))THEN
                      IP=JP
                      IDIR=+1
                      IL=3
                      IP=1+MOD(IP+IDIR-1+NS,NS)
                 ELSE
                      IP=1+MOD(IP+IDIR-1+NP,NP)
                 ENDIF
*   If on separation line, follow visible part of plane entered.
            ELSEIF(IL.EQ.3)THEN
*   Find out which plane we enter.
                 IF(IREF(IP,3,1).NE.0)THEN
                      JP=IREF(IP,3,1)
                      IL=1
                      ZAUX1=(DPL1-
     -                     APL1*0.5*(XL(JP,IL)+XL(1+MOD(JP,N1),IL))-
     -                     BPL1*0.5*(YL(JP,IL)+YL(1+MOD(JP,N1),IL)))/
     -                     CPL1
                      ZAUX2=(DPL2-
     -                     APL2*0.5*(XL(JP,IL)+XL(1+MOD(JP,N1),IL))-
     -                     BPL2*0.5*(YL(JP,IL)+YL(1+MOD(JP,N1),IL)))/
     -                     CPL2
                      ZAUX3=(DPL1-
     -                     APL1*0.5*(XL(JP,IL)+
     -                               XL(1+MOD(JP-2+N1,N1),IL))-
     -                     BPL1*0.5*(YL(JP,IL)+
     -                               YL(1+MOD(JP-2+N1,N1),IL)))/CPL1
                      ZAUX4=(DPL2-
     -                     APL2*0.5*(XL(JP,IL)+
     -                               XL(1+MOD(JP-2+N1,N1),IL))-
     -                     BPL2*0.5*(YL(JP,IL)+
     -                               YL(1+MOD(JP-2+N1,N1),IL)))/CPL2
                      IF(ZAUX1.GT.ZAUX2)THEN
                           IDIR=+1
                      ELSEIF(ZAUX3.GT.ZAUX4)THEN
                           IDIR=-1
                      ENDIF
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(IREF(IP,3,2).NE.0)THEN
                      JP=IREF(IP,3,2)
                      IL=2
                      ZAUX1=(DPL1-
     -                     APL1*0.5*(XL(JP,IL)+XL(1+MOD(JP,N2),IL))-
     -                     BPL1*0.5*(YL(JP,IL)+YL(1+MOD(JP,N2),IL)))/
     -                     CPL1
                      ZAUX2=(DPL2-
     -                     APL2*0.5*(XL(JP,IL)+XL(1+MOD(JP,N2),IL))-
     -                     BPL2*0.5*(YL(JP,IL)+YL(1+MOD(JP,N2),IL)))/
     -                     CPL2
                      ZAUX3=(DPL1-
     -                     APL1*0.5*(XL(JP,IL)+
     -                               XL(1+MOD(JP-2+N2,N2),IL))-
     -                     BPL1*0.5*(YL(JP,IL)+
     -                               YL(1+MOD(JP-2+N2,N2),IL)))/CPL1
                      ZAUX4=(DPL2-
     -                     APL2*0.5*(XL(JP,IL)+
     -                               XL(1+MOD(JP-2+N2,N2),IL))-
     -                     BPL2*0.5*(YL(JP,IL)+
     -                               YL(1+MOD(JP-2+N2,N2),IL)))/CPL2
                      IF(ZAUX2.GT.ZAUX1)THEN
                           IDIR=+1
                      ELSEIF(ZAUX4.GT.ZAUX3)THEN
                           IDIR=-1
                      ENDIF
                      IP=1+MOD(JP+IDIR-1+N2,N2)
                 ELSE
                      PRINT *,' !!!!!! PLASPL WARNING : Hole has no'//
     -                     ' matching plane.'
                      IP=1+MOD(IP+IDIR-1+NP,NP)
                 ENDIF
            ENDIF
**  If this is a crossing with the separation line.
       ELSEIF(IT(IP,IL).EQ.5)THEN
            IF(LDEBUG)WRITE(LUNOUT,'(''  Plane crosses separation.'')')
*   If we are on plane 1, ensure we don't dive under other plane.
            IF(IL.EQ.1)THEN
                 CALL INTERD(NPL2,XPL2,YPL2,
     -                (XL(IP,IL)+XL(1+MOD(IP+IDIR-1,NP),IL))/2,
     -                (YL(IP,IL)+YL(1+MOD(IP+IDIR-1,NP),IL))/2,
     -                IN1,EDGE1)
                 ZAUX1=(DPL1-
     -                APL1*(XL(IP,IL)+XL(1+MOD(IP+IDIR-1,NP),IL))/2-
     -                BPL1*(YL(IP,IL)+YL(1+MOD(IP+IDIR-1,NP),IL))/2)/
     -                CPL1
                 ZAUX2=(DPL2-
     -                APL2*(XL(IP,IL)+XL(1+MOD(IP+IDIR-1,NP),IL))/2-
     -                BPL2*(YL(IP,IL)+YL(1+MOD(IP+IDIR-1,NP),IL))/2)/
     -                CPL2
                 IF((IN1.OR.EDGE1).AND.ZAUX1.LT.ZAUX2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(5X,
     -                     ''On 1, going over 2'')')
                      JP=IREF(IP,IL,3)
                      CALL INTERD(NPL1,XPL1,YPL1,
     -                     (XL(JP,3)+XL(1+MOD(JP-2+NS,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP-2+NS,NS),3))/2,
     -                     IN1,EDGE1)
                      CALL INTERD(NPL1,XPL1,YPL1,
     -                     (XL(JP,3)+XL(1+MOD(JP     ,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP     ,NS),3))/2,
     -                     IN2,EDGE2)
                      CALL INTERD(NPL2,XPL2,YPL2,
     -                     (XL(JP,3)+XL(1+MOD(JP-2+NS,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP-2+NS,NS),3))/2,
     -                     IN3,EDGE3)
                      CALL INTERD(NPL2,XPL2,YPL2,
     -                     (XL(JP,3)+XL(1+MOD(JP     ,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP     ,NS),3))/2,
     -                     IN4,EDGE4)
                      IF((IN1.OR.EDGE1).AND.(IN3.OR.EDGE3))THEN
                           IF(JP.LE.1)THEN
                                PRINT *,' !!!!!! PLASPL WARNING :'//
     -                               ' Reached start of separation.'
                                LGSIG=.TRUE.
                                IDIR=+1
                           ELSE
                                IDIR=-1
                           ENDIF
                           IL=3
                           IP=JP+IDIR
                      ELSEIF((IN2.OR.EDGE2).AND.(IN4.OR.EDGE4))THEN
                           IF(JP.GE.NS)THEN
                                PRINT *,' !!!!!! PLASPL WARNING :'//
     -                               ' Reached end of separation.'
                                LGSIG=.TRUE.
                                IDIR=-1
                           ELSE
                                IDIR=+1
                           ENDIF
                           IL=3
                           IP=JP+IDIR
                      ELSE
C      print *,'    No interest in changing line.'
                           IP=1+MOD(IP+IDIR-1+NP,NP)
                      ENDIF
                 ELSE
C      print *,'    Staying on curve'
                      IP=1+MOD(IP+IDIR-1+NP,NP)
                 ENDIF
*   If we are on plane 2, ensure we don't dive under other plane.
            ELSEIF(IL.EQ.2)THEN
                 CALL INTERD(NPL1,XPL1,YPL1,
     -                (XL(IP,IL)+XL(1+MOD(IP+IDIR-1,NP),IL))/2,
     -                (YL(IP,IL)+YL(1+MOD(IP+IDIR-1,NP),IL))/2,
     -                IN1,EDGE1)
                 ZAUX1=(DPL1-
     -                APL1*(XL(IP,IL)+XL(1+MOD(IP+IDIR-1,NP),IL))/2-
     -                BPL1*(YL(IP,IL)+YL(1+MOD(IP+IDIR-1,NP),IL))/2)/
     -                CPL1
                 ZAUX2=(DPL2-
     -                APL2*(XL(IP,IL)+XL(1+MOD(IP+IDIR-1,NP),IL))/2-
     -                BPL2*(YL(IP,IL)+YL(1+MOD(IP+IDIR-1,NP),IL))/2)/
     -                CPL2
                 IF((IN1.OR.EDGE1).AND.ZAUX1.GT.ZAUX2)THEN
                      IF(LDEBUG)WRITE(LUNOUT,'(5X,
     -                     ''On 1, going over 2'')')
                      JP=IREF(IP,IL,3)
                      CALL INTERD(NPL2,XPL2,YPL2,
     -                     (XL(JP,3)+XL(1+MOD(JP-2+NS,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP-2+NS,NS),3))/2,
     -                     IN1,EDGE1)
                      CALL INTERD(NPL2,XPL2,YPL2,
     -                     (XL(JP,3)+XL(1+MOD(JP     ,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP     ,NS),3))/2,
     -                     IN2,EDGE2)
                      CALL INTERD(NPL1,XPL1,YPL1,
     -                     (XL(JP,3)+XL(1+MOD(JP-2+NS,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP-2+NS,NS),3))/2,
     -                     IN3,EDGE3)
                      CALL INTERD(NPL1,XPL1,YPL1,
     -                     (XL(JP,3)+XL(1+MOD(JP     ,NS),3))/2,
     -                     (YL(JP,3)+YL(1+MOD(JP     ,NS),3))/2,
     -                     IN4,EDGE4)
                      IF((IN1.OR.EDGE1).AND.(IN3.OR.EDGE3))THEN
                           IF(JP.LE.1)THEN
                                PRINT *,' !!!!!! PLASPL WARNING :'//
     -                               ' Reached start of separation.'
                                LGSIG=.TRUE.
                                IDIR=+1
                           ELSE
                                IDIR=-1
                           ENDIF
                           IL=3
                           IP=JP+IDIR
                      ELSEIF((IN2.OR.EDGE2).AND.(IN4.OR.EDGE4))THEN
                           IF(JP.GE.NS)THEN
                                PRINT *,' !!!!!! PLASPL WARNING :'//
     -                               ' Reached end of separation.'
                                LGSIG=.TRUE.
                                IDIR=-1
                           ELSE
                                IDIR=+1
                           ENDIF
                           IL=3
                           IP=JP+IDIR
                      ELSE
C      print *,'    No interest in changing line.'
                           IP=1+MOD(IP+IDIR-1+NP,NP)
                      ENDIF
                 ELSE
C      print *,'    Staying on curve'
                      IP=1+MOD(IP+IDIR-1+NP,NP)
                 ENDIF
*   If on intersect, continue on the new plane.
            ELSEIF(IL.EQ.3)THEN
*   If crossing plane 1, continue in original direction.
                 IF(IREF(IP,3,1).NE.0)THEN
C      print *,'    Entering plane 1'
                      JP=IREF(IP,3,1)
                      CALL INTERD(NPL2,XPL2,YPL2,
     -                     (XL(JP,1)+XL(1+MOD(JP-2+N1,N1),1))/2,
     -                     (YL(JP,1)+YL(1+MOD(JP-2+N1,N1),1))/2,
     -                     IN1,EDGE1)
                      CALL INTERD(NPL2,XPL2,YPL2,
     -                     (XL(JP,1)+XL(1+MOD(JP     ,N1),1))/2,
     -                     (YL(JP,1)+YL(1+MOD(JP     ,N1),1))/2,
     -                     IN2,EDGE2)
                      ZAUX1=(DPL1-
     -                     APL1*(XL(JP,1)+XL(1+MOD(JP-2+N1,N1),1))/2-
     -                     BPL1*(YL(JP,1)+YL(1+MOD(JP-2+N1,N1),1))/2)/
     -                     CPL1
                      ZAUX2=(DPL2-
     -                     APL2*(XL(JP,1)+XL(1+MOD(JP-2+N1,N1),1))/2-
     -                     BPL2*(YL(JP,1)+YL(1+MOD(JP-2+N1,N1),1))/2)/
     -                     CPL2
                      ZAUX3=(DPL1-
     -                     APL1*(XL(JP,1)+XL(1+MOD(JP     ,N1),1))/2-
     -                     BPL1*(YL(JP,1)+YL(1+MOD(JP     ,N1),1))/2)/
     -                     CPL1
                      ZAUX4=(DPL2-
     -                     APL2*(XL(JP,1)+XL(1+MOD(JP     ,N1),1))/2-
     -                     BPL2*(YL(JP,1)+YL(1+MOD(JP     ,N1),1))/2)/
     -                     CPL2
                      IF(.NOT.(IN1.OR.EDGE1))THEN
                           IDIR=-1
                      ELSEIF(.NOT.(IN2.OR.EDGE2))THEN
                           IDIR=+1
                      ELSEIF(ZAUX1.GT.ZAUX2)THEN
                           IDIR=-1
                      ELSEIF(ZAUX3.GT.ZAUX4)THEN
                           IDIR=+1
                      ELSE
C      print *,'    Resuming plane 1 in old direction.'
                           IDIR=INITD
                      ENDIF
                      IL=1
                      IP=1+MOD(JP+IDIR-1+N1,N1)
                 ELSEIF(IREF(IP,3,2).NE.0)THEN
                      JP=IREF(IP,3,2)
                      IL=2
                      PRINT *,' !!!!!! PLASPL WARNING : Entered plane'//
     -                     ' 2.'
                      LGSIG=.TRUE.
                      ZAUX1=(DPL1-
     -                     APL1*0.5*(XL(JP,IL)+XL(1+MOD(JP,N2),IL))-
     -                     BPL1*0.5*(YL(JP,IL)+YL(1+MOD(JP,N2),IL)))/
     -                     CPL1
                      ZAUX2=(DPL2-
     -                     APL2*0.5*(XL(JP,IL)+XL(1+MOD(JP,N2),IL))-
     -                     BPL2*0.5*(YL(JP,IL)+YL(1+MOD(JP,N2),IL)))/
     -                     CPL2
                      ZAUX3=(DPL1-
     -                     APL1*0.5*(XL(JP,IL)+
     -                               XL(1+MOD(JP-2+N2,N2),IL))-
     -                     BPL1*0.5*(YL(JP,IL)+
     -                               YL(1+MOD(JP-2+N2,N2),IL)))/CPL1
                      ZAUX4=(DPL2-
     -                     APL2*0.5*(XL(JP,IL)+
     -                               XL(1+MOD(JP-2+N2,N2),IL))-
     -                     BPL2*0.5*(YL(JP,IL)+
     -                               YL(1+MOD(JP-2+N2,N2),IL)))/CPL2
                      IF(ZAUX2.GT.ZAUX1)THEN
                           IDIR=+1
                      ELSEIF(ZAUX4.GT.ZAUX3)THEN
                           IDIR=-1
                      ENDIF
                      IP=1+MOD(JP+IDIR-1+N2,N2)
                 ELSE
                      PRINT *,' !!!!!! PLASPL WARNING : No connection'//
     -                     ' found.'
                      LGSIG=.TRUE.
                      IP=1+MOD(JP+IDIR-1+NP,NP)
                 ENDIF
*   Move in the direction in which the line visible.
            ENDIF
**  Anything else.
       ELSE
            PRINT *,' !!!!!! PLASPL WARNING : Unknown type for a'//
     -           ' point; skipped.'
            IP=1+MOD(IP+IDIR-1+NP,NP)
       ENDIF
*** Resume the loop.
       GOTO 430
*** And process cut-outs, pieces of 2 sticking out above the plane.
1000   CONTINUE
**  Loop over the planes that were produced.
       DO 1010 IR=1,NREF
       IF(IREFO(IR).LE.0)GOTO 1010
       CALL PLABU2('READ',IREFO(IR),NPL,XPL,YPL,ZPL,
     -      APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
       IF(IFAIL1.NE.0)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Unable to retrieve a'//
     -           ' plane ; not checked for cut-outs.'
            GOTO 1010
       ENDIF
*   Find a first vertex sticking out.
       DO 710 I=1,N2
       IF(IT(I,2).NE.1)GOTO 710
       CALL INTERD(NPL,XPL,YPL,XL(I,2),YL(I,2),INSIDE,EDGE)
       IF(INSIDE.AND.(.NOT.EDGE).AND.
     -      ZL(I,2).GT.(DPL1-APL1*XL(I,2)-BPL1*YL(I,2))/CPL1)THEN
            IP=I
            IL=2
            NCUT=0
C      print *,' Found a vertex sticking out IP/IL=',ip,il
C      print *,'    xyz:    ',xl(i,2),yl(i,2),zl(i,2)
C      print *,'    offset: ',(dpl1-apl1*xl(i,2)-bpl1*yl(i,2))/cpl1
**  Trace the curve from here.
            START=.TRUE.
720         CONTINUE
*   See whether the loop is closed.
            IF(.NOT.START.AND.
     -           ABS(XL(IP,IL)-XCUT(1)).LT.EPSX.AND.
     -           ABS(YL(IP,IL)-YCUT(1)).LT.EPSY)THEN
                 IF(NCUT.LT.3)THEN
C      print *,'    Loop closed, not long enough'
                      GOTO 710
                 ELSE
C      print *,'    Loop closed, length=',ncut
                      GOTO 730
                 ENDIF
            ENDIF
            START=.FALSE.
*   Add the current point.
            IF(NCUT+1.LE.MXCORN)THEN
                 NCUT=NCUT+1
                 XCUT(NCUT)=XL(IP,IL)
                 YCUT(NCUT)=YL(IP,IL)
                 ZCUT(NCUT)=(DPL1-APL1*XL(IP,IL)-BPL1*YL(IP,IL))/CPL1
            ELSE
                 PRINT *,' !!!!!! PLASPL WARNING : Cut-out too long'//
     -                ' ; truncated.'
C      print *,'    Length=',ncut
                 GOTO 730
            ENDIF
*   Ensure there is no link with plane 1.
            IF(IREF(IP,IL,1).NE.0)THEN
C      print *,'    Linked with 1, abandoned'
                 GOTO 710
*   See whether this is a vertex of 2.
            ELSEIF(IL.EQ.2.AND.IT(IP,IL).EQ.1)THEN
C      print *,'    Vertex IP/IL=',IP,IL
                 CALL INTERD(NPL,XPL,YPL,XL(IP,IL),YL(IP,IL),
     -                INSIDE,EDGE)
                 IF((.NOT.INSIDE).OR.EDGE.OR.ZL(IP,IL).LT.
     -                (DPL1-APL1*XL(IP,IL)-BPL1*YL(IP,IL))/CPL1)THEN
C      print *,'    - Not useable, abandoned.'
                      GOTO 710
                 ENDIF
                 IP=1+MOD(IP,N2)
*   See whether this is an intersect with the separation.
            ELSEIF(IL.EQ.2.AND.IT(IP,IL).EQ.4)THEN
C      print *,'    Intersect with separation IP/IL=',IP,IL
                 JP=IREF(IP,2,3)
                 CALL INTERD(N2,XL(1,2),YL(1,2),
     -                (XL(JP,3)+XL(1+MOD(JP     ,NS),3))/2,
     -                (YL(JP,3)+YL(1+MOD(JP     ,NS),3))/2,IN1,EDGE1)
                 CALL INTERD(N2,XL(1,2),YL(1,2),
     -                (XL(JP,3)+XL(1+MOD(JP-2+NS,NS),3))/2,
     -                (YL(JP,3)+YL(1+MOD(JP-2+NS,NS),3))/2,IN2,EDGE2)
                 IF(JP.LE.1.AND..NOT.(IN1.OR.EDGE1))THEN
C      print *,'    - Lost trace on separation'
                      GOTO 710
                 ELSEIF(JP.LE.1)THEN
                      IP=2
                      IL=3
                 ELSEIF(JP.GE.NS.AND..NOT.(IN2.OR.EDGE2))THEN
C      print *,'    - Lost trace on separation'
                      GOTO 710
                 ELSEIF(JP.GE.NS)THEN
                      IP=NS-1
                      IL=3
                 ELSEIF(IN1.OR.EDGE1)THEN
                      IP=JP+1
                      IL=3
                 ELSEIF(IN2.OR.EDGE2)THEN
                      IP=JP-1
                      IL=3
                 ELSE
C      print *,'    - No way out.'
                      GOTO 710
                 ENDIF
*   See whether the intersect crosses plane 2 here.
            ELSEIF(IL.EQ.3.AND.(IT(IP,IL).EQ.4.OR.IT(IP,IL).EQ.2))THEN
C      print *,'    Crossing 2'
                 JP=1+MOD(IREF(IP,3,2),N2)
                 CALL INTERD(NPL,XPL,YPL,XL(JP,2),YL(JP,2),INSIDE,EDGE)
                 IF(IREF(IP,3,2).EQ.0.OR.
     -                (.NOT.INSIDE).OR.EDGE.OR.ZL(JP,2).LT.
     -                (DPL1-APL1*XL(JP,2)-BPL1*YL(JP,2))/CPL1)THEN
C      print *,'    - Not useable, abandoned.'
                      GOTO 710
                 ELSE
                      IP=JP
                      IL=2
                 ENDIF
*   Other cases should not occur.
            ELSE
                 PRINT *,' !!!!!! PLASPL WARNING : Unknown cut-out'//
     -                ' case seen.'
                 LGSIG=.TRUE.
            ENDIF
*   Make another step.
            GOTO 720
       ENDIF
710    CONTINUE
*   End of vertex loop.
       GOTO 1010
*   Check number of points.
730    CONTINUE
C      print *,'    Genuine cut-out:'
C      call gsplci(9)
C      call gsln(1)
C      call gpl2(ncut,xcut,ycut)
C      call gspmci(9)
C      call gsmk(5)
**  Find a place where we can connect cutout and curve.
       DO 770 K=1,NPL-1
       DO 740 J=1,NCUT
*   Check for intersects with the visible parts of curve 2.
       DO 760 I=1,N2
       IF( (ABS(XCUT(1+MOD(J-1,NCUT))-XL(1+MOD(I-1,N2),2)).GT.EPSX.OR.
     -      ABS(YCUT(1+MOD(J-1,NCUT))-YL(1+MOD(I-1,N2),2)).GT.EPSY).AND.
     -     (ABS(XCUT(1+MOD(J-1,NCUT))-XL(1+MOD(I  ,N2),2)).GT.EPSX.OR.
     -      ABS(YCUT(1+MOD(J-1,NCUT))-YL(1+MOD(I  ,N2),2)).GT.EPSY).AND.
     -      CROSSD(
     -      XPL (K              ),YPL (K              ),
     -      XCUT(1+MOD(J-1,NCUT)),YCUT(1+MOD(J-1,NCUT)),
     -      XL  (1+MOD(I-1,N2),2),YL  (1+MOD(I-1,N2),2),
     -      XL  (1+MOD(I  ,N2),2),YL  (1+MOD(I  ,N2),2)))GOTO 740
       IF( (ABS(XCUT(1+MOD(J-1,NCUT))-XL(1+MOD(I-1,N2),2)).GT.EPSX.OR.
     -      ABS(YCUT(1+MOD(J-1,NCUT))-YL(1+MOD(I-1,N2),2)).GT.EPSY).AND.
     -      ONLIND(
     -      XPL (K              ),YPL (K              ),
     -      XCUT(1+MOD(J-1,NCUT)),YCUT(1+MOD(J-1,NCUT)),
     -      XL  (1+MOD(I-1,N2),2),YL  (1+MOD(I-1,N2),2)))GOTO 740
       IF( (ABS(XCUT(1+MOD(J  ,NCUT))-XL(1+MOD(I-1,N2),2)).GT.EPSX.OR.
     -      ABS(YCUT(1+MOD(J  ,NCUT))-YL(1+MOD(I-1,N2),2)).GT.EPSY).AND.
     -     (ABS(XCUT(1+MOD(J  ,NCUT))-XL(1+MOD(I  ,N2),2)).GT.EPSX.OR.
     -      ABS(YCUT(1+MOD(J  ,NCUT))-YL(1+MOD(I  ,N2),2)).GT.EPSY).AND.
     -      CROSSD(
     -      XPL (K+1            ),YPL (K+1            ),
     -      XCUT(1+MOD(J  ,NCUT)),YCUT(1+MOD(J  ,NCUT)),
     -      XL  (1+MOD(I-1,N2),2),YL  (1+MOD(I-1,N2),2),
     -      XL  (1+MOD(I  ,N2),2),YL  (1+MOD(I  ,N2),2)))GOTO 740
       IF( (ABS(XCUT(1+MOD(J  ,NCUT))-XL(1+MOD(I-1,N2),2)).GT.EPSX.OR.
     -      ABS(YCUT(1+MOD(J  ,NCUT))-YL(1+MOD(I-1,N2),2)).GT.EPSY).AND.
     -      ONLIND(
     -      XPL (K+1            ),YPL (K+1            ),
     -      XCUT(1+MOD(J  ,NCUT)),YCUT(1+MOD(J  ,NCUT)),
     -      XL  (1+MOD(I-1,N2),2),YL  (1+MOD(I-1,N2),2)))GOTO 740
760    CONTINUE
*   Check for intersects with the cut-out.
       DO 755 I=1,NCUT
       IF(  1+MOD(J-1,NCUT).NE.1+MOD(I-1,NCUT).AND.
     -      1+MOD(J-1,NCUT).NE.1+MOD(I  ,NCUT).AND.
     -      CROSSD(
     -      XPL (K              ),YPL (K              ),
     -      XCUT(1+MOD(J-1,NCUT)),YCUT(1+MOD(J-1,NCUT)),
     -      XCUT(1+MOD(I-1,NCUT)),YCUT(1+MOD(I-1,NCUT)),
     -      XCUT(1+MOD(I  ,NCUT)),YCUT(1+MOD(I  ,NCUT))))GOTO 740
       IF(  1+MOD(J-1,NCUT).NE.1+MOD(I-1,NCUT).AND.
     -      ONLIND(
     -      XPL (K              ),YPL (K              ),
     -      XCUT(1+MOD(J-1,NCUT)),YCUT(1+MOD(J-1,NCUT)),
     -      XCUT(1+MOD(I-1,NCUT)),YCUT(1+MOD(I-1,NCUT))))GOTO 740
       IF(  1+MOD(J  ,NCUT).NE.1+MOD(I-1,NCUT).AND.
     -      1+MOD(J  ,NCUT).NE.1+MOD(I  ,NCUT).AND.
     -      CROSSD(
     -      XPL (K+1            ),YPL (K+1            ),
     -      XCUT(1+MOD(J  ,NCUT)),YCUT(1+MOD(J  ,NCUT)),
     -      XCUT(1+MOD(I-1,NCUT)),YCUT(1+MOD(I-1,NCUT)),
     -      XCUT(1+MOD(I  ,NCUT)),YCUT(1+MOD(I  ,NCUT))))GOTO 740
       IF(  1+MOD(J  ,NCUT).NE.1+MOD(I-1,NCUT).AND.
     -      ONLIND(
     -      XPL (K+1            ),YPL (K+1            ),
     -      XCUT(1+MOD(J  ,NCUT)),YCUT(1+MOD(J  ,NCUT)),
     -      XCUT(1+MOD(I-1,NCUT)),YCUT(1+MOD(I-1,NCUT))))GOTO 740
755    CONTINUE
*   Check for intersects with the curve.
       DO 750 I=1,NPL
       IF(  K  .NE.1+MOD(I-1,NPL).AND.
     -      K  .NE.1+MOD(I  ,NPL).AND.
     -      CROSSD(
     -      XPL (K              ),YPL (K              ),
     -      XCUT(1+MOD(J-1,NCUT)),YCUT(1+MOD(J-1,NCUT)),
     -      XPL (1+MOD(I-1,NPL )),YPL (1+MOD(I-1,NPL )),
     -      XPL (1+MOD(I  ,NPL )),YPL (1+MOD(I  ,NPL ))))GOTO 740
       IF(  K  .NE.1+MOD(I-1,NPL).AND.
     -      ONLIND(
     -      XPL (K              ),YPL (K              ),
     -      XCUT(1+MOD(J-1,NCUT)),YCUT(1+MOD(J-1,NCUT)),
     -      XPL (1+MOD(I-1,NPL )),YPL (1+MOD(I-1,NPL ))))GOTO 740
       IF(  K+1.NE.1+MOD(I-1,NPL).AND.
     -      K+1.NE.1+MOD(I  ,NPL).AND.
     -      CROSSD(
     -      XPL (K+1            ),YPL (K+1            ),
     -      XCUT(1+MOD(J  ,NCUT)),YCUT(1+MOD(J  ,NCUT)),
     -      XPL (1+MOD(I-1,NPL )),YPL (1+MOD(I-1,NPL )),
     -      XPL (1+MOD(I  ,NPL )),YPL (1+MOD(I  ,NPL ))))GOTO 740
       IF(  K+1.NE.1+MOD(I-1,NPL).AND.
     -      ONLIND(
     -      XPL (K+1            ),YPL (K+1            ),
     -      XCUT(1+MOD(J  ,NCUT)),YCUT(1+MOD(J  ,NCUT)),
     -      XPL (1+MOD(I-1,NPL )),YPL (1+MOD(I-1,NPL ))))GOTO 740
750    CONTINUE
*   Found a pair.
       K0=K
       K1=K+1
C      call gspmci(1)
C      call gsmk(4)
C      call gpm2(1,xpl(k0),ypl(k0))
C      call gpm2(1,xpl(k1),ypl(k1))
C      print *,'    Point 0 on curve:  ',k0,xpl(k0),ypl(k0)
C      print *,'    Point 1 on curve:  ',k1,xpl(k1),ypl(k1)
C      print *,'    (Range: ',1,npl,')'
       J0=1+MOD(J-1,NCUT)
       J1=1+MOD(J  ,NCUT)
C      call gsmk(2)
C      call gpm2(1,xcut(j0),ycut(j0))
C      call gpm2(1,xcut(j1),ycut(j1))
C      print *,'    Point 0 on cutout: ',j0,xcut(j0),ycut(j0)
C      print *,'    Point 1 on cutout: ',j1,xcut(j1),ycut(j1)
C      print *,'    (Range: ',1,ncut,')'
       GOTO 780
*   Continue loops.
740    CONTINUE
770    CONTINUE
*   No connection found.
       PRINT *,' !!!!!! PLASPL WARNING : Can''t connect cut-out'//
     -      ' to outer plane ; cut-out ignored.'
       GOTO 1010
**  Constract the 2 halves and store separately.
780    CONTINUE
*   See whether we have memory for this at all.
       IF(NCUT+NPL.GT.MXEDGE.OR.NREF+2.GT.MXPLAN)THEN
            PRINT *,' !!!!!! PLASPL WARNING : Lack of reference space'//
     -           ' or list length for cut-out.'
            GOTO 1010
       ENDIF
*   See whether the junction lines cross.
       IF(CROSSD(XPL(K0),YPL(K0),XCUT(J0),YCUT(J0),
     -      XPL(K1),YPL(K1),XCUT(J1),YCUT(J1)))THEN
            IAUX=J1
            J1=J0
            J0=IAUX
C      print *,'    Interchanging J0/J1'
       ENDIF
*   First make the small 4-point loop.
       XPL1(1)=XPL(K0)
       YPL1(1)=YPL(K0)
       ZPL1(1)=ZPL(K0)
       XPL1(2)=XCUT(J0)
       YPL1(2)=YCUT(J0)
       ZPL1(2)=(DPL1-APL1*XCUT(J0)-BPL1*YCUT(J0))/CPL1
       XPL1(3)=XCUT(J1)
       YPL1(3)=YCUT(J1)
       ZPL1(3)=(DPL1-APL1*XCUT(J1)-BPL1*YCUT(J1))/CPL1
       XPL1(4)=XPL(K1)
       YPL1(4)=YPL(K1)
       ZPL1(4)=ZPL(K1)
       NPL1=4
*   Test to see whether this includes a point of the cut-out.
       SWAP=.FALSE.
       DO 820 I=1,NCUT
       IF(I.EQ.J0.OR.I.EQ.J1)GOTO 820
       CALL INTERD(NPL1,XPL1,YPL1,XCUT(I),YCUT(I),INSIDE,EDGE)
       IF(INSIDE.OR.EDGE)SWAP=.TRUE.
820    CONTINUE
C      if(swap)print *,'    Found an internal point of cut-out.'
*   If there was, select the other branch.
       IF(SWAP)THEN
            XPL1(1)=XPL(K0)
            YPL1(1)=YPL(K0)
            ZPL1(1)=ZPL(K0)
            IF(MOD(J0-J1+NCUT,NCUT).EQ.+1)THEN
                 DO 830 J=J0,J1+NCUT
                 XPL1(1+J-J0+1)=XCUT(1+MOD(J-1,NCUT))
                 YPL1(1+J-J0+1)=YCUT(1+MOD(J-1,NCUT))
                 ZPL1(1+J-J0+1)=(DPL1-APL1*XPL1(1+J-J0+1)-
     -                BPL1*YPL1(1+J-J0+1))/CPL1
830              CONTINUE
            ELSE
                 DO 840 J=J0,J1-NCUT,-1
                 XPL1(1+J0-J+1)=XCUT(1+MOD(J-1+NCUT,NCUT))
                 YPL1(1+J0-J+1)=YCUT(1+MOD(J-1+NCUT,NCUT))
                 ZPL1(1+J0-J+1)=(DPL1-APL1*XPL1(1+J0-J+1)-
     -                BPL1*YPL1(1+J0-J+1))/CPL1
840              CONTINUE
            ENDIF
            XPL1(NCUT+2)=XPL(K1)
            YPL1(NCUT+2)=YPL(K1)
            ZPL1(NCUT+2)=ZPL(K1)
            NPL1=NCUT+2
       ENDIF
C      call gsln(1)
C      call gsplci(8)
C      call gpl2(npl1,xpl1,ypl1)
*   Store this part of the curve.
       CALL PLARED(NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1)
       IF(NPL1.GE.3)THEN
            NREF=NREF+1
            CALL PLABU2('STORE',IREFO(NREF),NPL1,XPL1,YPL1,ZPL1,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
            IF(IFAIL1.NE.0)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Unable to store'//
     -                ' small half of a split plane.'
                 NREF=NREF-1
            ENDIF
        ENDIF
*   Now make the large loop.
       IF(SWAP)THEN
            XPL2(1)=XCUT(J1)
            YPL2(1)=YCUT(J1)
            ZPL2(1)=(DPL1-APL1*XPL2(1)-BPL1*YPL2(1))/CPL1
            XPL2(2)=XCUT(J0)
            YPL2(2)=YCUT(J0)
            ZPL2(2)=(DPL1-APL1*XPL2(2)-BPL1*YPL2(2))/CPL1
            NPL2=2
       ELSEIF(MOD(J1-J0+NCUT,NCUT).EQ.+1)THEN
            DO 790 J=J1,J0+NCUT
            XPL2(J-J1+1)=XCUT(1+MOD(J-1,NCUT))
            YPL2(J-J1+1)=YCUT(1+MOD(J-1,NCUT))
            ZPL2(J-J1+1)=(DPL1-APL1*XPL2(J-J1+1)-
     -      BPL1*YPL2(J-J1+1))/CPL1
790         CONTINUE
            NPL2=NCUT
       ELSE
            DO 810 J=J1,J0-NCUT,-1
            XPL2(J1-J+1)=XCUT(1+MOD(J-1+NCUT,NCUT))
            YPL2(J1-J+1)=YCUT(1+MOD(J-1+NCUT,NCUT))
            ZPL2(J1-J+1)=(DPL1-APL1*XPL2(J1-J+1)-
     -           BPL1*YPL2(J1-J+1))/CPL1
810         CONTINUE
            NPL2=NCUT
       ENDIF
       DO 800 K=K0+NPL,K1,-1
       XPL2(NPL2+K0+NPL-K+1)=XPL(1+MOD(K-1,NPL))
       YPL2(NPL2+K0+NPL-K+1)=YPL(1+MOD(K-1,NPL))
       ZPL2(NPL2+K0+NPL-K+1)=ZPL(1+MOD(K-1,NPL))
800    CONTINUE
       NPL2=NPL2+NPL
C      call gsln(1)
C      call gsplci(12)
C      call gpl2(npl2,xpl2,ypl2)
*   Store this part of the curve.
       CALL PLARED(NPL2,XPL2,YPL2,ZPL2,APL1,BPL1,CPL1,DPL1)
       IF(NPL2.GE.3)THEN
            NREF=NREF+1
            CALL PLABU2('STORE',IREFO(NREF),NPL2,XPL2,YPL2,ZPL2,
     -           APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL2)
            IF(IFAIL2.NE.0)THEN
                 PRINT *,' !!!!!! PLASPL WARNING : Unable to store'//
     -                ' large half of a split plane.'
                 NREF=NREF-1
            ENDIF
       ENDIF
C      call guwk(1,0)
C      read *,iaux
**  Delete original plane and start from scratch.
       CALL PLABU2('DELETE',IREFO(IR),NPL1,XPL1,YPL1,ZPL1,
     -      APL1,BPL1,CPL1,DPL1,ICOL1,IFAIL1)
       IREFO(IR)=0
       GOTO 1000
**  Continue with next plane.
1010   CONTINUE
*** Seems to have worked.
       IFAIL=0
       END
