CDECK  ID>, PLASEP.
       SUBROUTINE PLASEP(
     -      NPL1,XPL1,YPL1,ZPL1,A1,B1,C1,D1,
     -      NPL2,XPL2,YPL2,ZPL2,A2,B2,C2,D2,
     -      X0,Y0,Z0,AI,BI,CI,IFAIL)
*-----------------------------------------------------------------------
*   PLASEP - Computes a plane that includes the crossing between plane
*            1 and 2 and doesn't coincide with either.
*            them for plotting.
*   (Last changed on 29/ 9/98.)
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
       PARAMETER (MXWIRE=   300,MXSW  =   50)
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
       PARAMETER (MXMAP =  5000,MXEPS =   10)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER IFAIL,NPL1,NPL2,NCOM,I,J,K
       DOUBLE PRECISION A1,B1,C1,D1,A2,B2,C2,D2,XC,YC,ZC,XL,
     -      X0,Y0,Z0,AI,BI,CI,FNORM,
     -      EPSX,EPSY,EPSZ,XMIN,YMIN,ZMIN,XMAX,YMAX,ZMAX,
     -      XPL1(NPL1),YPL1(NPL1),ZPL1(NPL1),
     -      XPL2(NPL2),YPL2(NPL2),ZPL2(NPL2),
     -      XCOM(MXEDGE),YCOM(MXEDGE),ZCOM(MXEDGE)
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
            EPSZ=EPSGZ
       ELSE
            XMIN=XPL1(1)
            XMAX=XPL1(1)
            YMIN=YPL1(1)
            YMAX=YPL1(1)
            ZMIN=ZPL1(1)
            ZMAX=ZPL1(1)
            DO 10 I=2,NPL1
            XMIN=MIN(XMIN,XPL1(I))
            XMAX=MAX(XMAX,XPL1(I))
            YMIN=MIN(YMIN,YPL1(I))
            YMAX=MAX(YMAX,YPL1(I))
            ZMIN=MIN(ZMIN,ZPL1(I))
            ZMAX=MAX(ZMAX,ZPL1(I))
10          CONTINUE
            DO 20 I=1,NPL2
            XMIN=MIN(XMIN,XPL2(I))
            XMAX=MAX(XMAX,XPL2(I))
            YMIN=MIN(YMIN,YPL2(I))
            YMAX=MAX(YMAX,YPL2(I))
            ZMIN=MIN(ZMIN,ZPL2(I))
            ZMAX=MAX(ZMAX,ZPL2(I))
20          CONTINUE
            EPSX=1.0D-8*ABS(XMAX-XMIN)
            EPSY=1.0D-8*ABS(YMAX-YMIN)
            EPSZ=1.0D-8*ABS(ZMAX-ZMIN)
            IF(EPSX.LE.0)EPSX=1.0D-8
            IF(EPSY.LE.0)EPSY=1.0D-8
            IF(EPSZ.LE.0)EPSZ=1.0D-8
       ENDIF
*** Initial values for the return parameters.
       X0=0
       Y0=0
       Z0=0
       AI=0
       BI=0
       CI=0
*** See whether the planes are parallel.
       IF((B1*C2-B2*C1)**2+(C1*A2-C2*A1)**2+(A1*B2-A2*B1)**2.LT.
     -      1.0D-6*SQRT((A1**2+B1**2+C1**2)*(A2**2+B2**2+C2**2)))THEN
            IFAIL=1
            RETURN
       ENDIF
*** See how many common points there are between the curves.
       NCOM=0
       DO 100 I=1,NPL1
       DO 110 J=1,NPL2
       IF(ABS(XPL1(I)-XPL2(J)).LE.EPSX.AND.
     -      ABS(YPL1(I)-YPL2(J)).LE.EPSY.AND.
     -      ABS(ZPL1(I)-ZPL2(J)).LE.EPSZ)THEN
            DO 120 K=1,NCOM
            IF(ABS(XPL1(I)+XPL2(J)-2*XCOM(K)).LE.EPSX.AND.
     -           ABS(YPL1(I)+YPL2(J)-2*YCOM(K)).LE.EPSY.AND.
     -           ABS(ZPL1(I)+ZPL2(J)-2*ZCOM(K)).LE.EPSZ)GOTO 110
120         CONTINUE
            NCOM=NCOM+1
            IF(NCOM.GE.MXEDGE)GOTO 110
            XCOM(NCOM)=(XPL1(I)+XPL2(J))/2
            YCOM(NCOM)=(YPL1(I)+YPL2(J))/2
            ZCOM(NCOM)=(ZPL1(I)+ZPL2(J))/2
       ENDIF
110    CONTINUE
100    CONTINUE
*** Debugging output.
       IF(LDEBUG)THEN
            WRITE(LUNOUT,'(''  ++++++ PLASEP DEBUG   : Number of'',
     -           '' common points: '',I3)') NCOM
            DO 130 I=1,NCOM
            WRITE(LUNOUT,'(26X,''Point '',I3,'' (x,y,z)='',3F12.5)')
     -           I,XCOM(I),YCOM(I),ZCOM(I)
130         CONTINUE
       ENDIF
*** No common points.
       IF(NCOM.EQ.0)THEN
*   Compute a point on the separation line.
            IF(ABS(B1*C2-B2*C1).GT.ABS(C1*A2-C2*A1).AND.
     -           ABS(B1*C2-B2*C1).GT.ABS(A1*B2-A2*B1))THEN
                 X0=0
                 Y0=+(D1*C2-D2*C1)/(B1*C2-B2*C1)
                 Z0=-(D1*B2-D2*B1)/(B1*C2-B2*C1)
            ELSEIF(ABS(C1*A2-C2*A1).GT.ABS(A1*B2-A2*B1))THEN
                 X0=+(D1*C2-D2*C1)/(A1*C2-A2*C1)
                 Y0=0
                 Z0=-(D1*A2-D2*A1)/(A1*C2-A2*C1)
            ELSE
                 X0=+(D1*B2-D2*B1)/(A1*B2-A2*B1)
                 Y0=-(D1*A2-D2*A1)/(A1*B2-A2*B1)
                 Z0=0
            ENDIF
*   Establish the parameters along the separation line.
            AI=B1*C2-C1*B2
            BI=C1*A2-A1*C2
            CI=A1*B2-B1*A2
            FNORM=SQRT(AI**2+BI**2+CI**2)
            IF(FNORM.LE.0)THEN
                 PRINT *,' !!!!!! PLASEP WARNING : Intersect line'//
     -                ' not found; no separation plane.'
                 IFAIL=1
                 RETURN
            ENDIF
            AI=AI/FNORM
            BI=BI/FNORM
            CI=CI/FNORM
*** A single point in common.
       ELSEIF(NCOM.EQ.1)THEN
*   Use the point as reference.
            X0=XCOM(1)
            Y0=YCOM(1)
            Z0=ZCOM(1)
*   Still compute the parameters of the separation line.
            AI=B1*C2-C1*B2
            BI=C1*A2-A1*C2
            CI=A1*B2-B1*A2
            FNORM=SQRT(AI**2+BI**2+CI**2)
            IF(FNORM.LE.0)THEN
                 PRINT *,' !!!!!! PLASEP WARNING : Intersect line'//
     -                ' not found; no separation plane.'
                 IFAIL=1
                 RETURN
            ENDIF
            AI=AI/FNORM
            BI=BI/FNORM
            CI=CI/FNORM
*** Two points in common.
       ELSEIF(NCOM.EQ.2)THEN
*   Use the first point as reference.
            X0=XCOM(1)
            Y0=YCOM(1)
            Z0=ZCOM(1)
*   Compute the separation line from the other point.
            AI=XCOM(2)-XCOM(1)
            BI=YCOM(2)-YCOM(1)
            CI=ZCOM(2)-ZCOM(1)
            FNORM=SQRT(AI**2+BI**2+CI**2)
            IF(FNORM.LE.0)THEN
                 PRINT *,' !!!!!! PLASEP WARNING : Intersect line'//
     -                ' not found; no separation plane.'
                 IFAIL=1
                 RETURN
            ENDIF
            AI=AI/FNORM
            BI=BI/FNORM
            CI=CI/FNORM
*** More than 2 points in common.
       ELSE
*   Use the first point as reference.
            X0=XCOM(1)
            Y0=YCOM(1)
            Z0=ZCOM(1)
*   Compute the separation line from the other point.
            AI=XCOM(2)-XCOM(1)
            BI=YCOM(2)-YCOM(1)
            CI=ZCOM(2)-ZCOM(1)
            FNORM=SQRT(AI**2+BI**2+CI**2)
            IF(FNORM.LE.0)THEN
                 PRINT *,' !!!!!! PLASEP WARNING : Intersect line'//
     -                ' not found; no separation plane.'
                 IFAIL=1
                 RETURN
            ENDIF
            AI=AI/FNORM
            BI=BI/FNORM
            CI=CI/FNORM
*   See whether the other points are on the line.
            DO 200 I=3,NCOM
            XL=((XCOM(I)-X0)*AI+(YCOM(I)-Y0)*BI+(ZCOM(I)-Z0)*CI)/FNORM
            XC=X0+XL*AI
            YC=Y0+XL*BI
            ZC=Z0+XL*CI
            IF(ABS(XCOM(I)-XC).GT.EPSX.OR.
     -           ABS(YCOM(I)-YC).GT.EPSY.OR.
     -           ABS(ZCOM(I)-ZC).GT.EPSZ)THEN
                 PRINT *,' !!!!!! PLASEP WARNING : Found non-colinear'//
     -                ' common points; no separation plane.'
                 IFAIL=1
                 RETURN
            ENDIF
200         CONTINUE
       ENDIF
*** Debugging result.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ PLASEP DEBUG   : Point: '',
     -      4X,3F12.5/26X,''Direction: '',3F12.5)') X0,Y0,Z0,AI,BI,CI
*** Seems to have worked.
       IFAIL=0
       END
