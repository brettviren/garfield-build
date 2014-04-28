CDECK  ID>, PLAGT.
       LOGICAL FUNCTION PLAGT(I1,I2)
*-----------------------------------------------------------------------
*   PLAGT  - Determines whick plane partially overlaps the other.
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
       INTEGER I1,I2,NPL1,NPL2,ICOL1,ICOL2,IFAIL1,IFAIL2,I,J
       DOUBLE PRECISION XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),
     -      XPL2(MXEDGE),YPL2(MXEDGE),ZPL2(MXEDGE),
     -      APL1,BPL1,CPL1,DPL1,APL2,BPL2,CPL2,DPL2,
     -      OFFSET,OFF1,OFF2,XC,YC,EPS,ZMAX,ZMIN
       LOGICAL INSIDE,EDGE,LT12,EQ12,GT12,CROSS
*** If the planes are identical, return True.
       IF(I1.EQ.I2)THEN
            PLAGT=.TRUE.
            RETURN
       ENDIF
*** Fetch both planes.
       CALL PLABU2('READ',I1,NPL1,XPL1,YPL1,ZPL1,APL1,BPL1,CPL1,DPL1,
     -      ICOL1,IFAIL1)
       CALL PLABU2('READ',I2,NPL2,XPL2,YPL2,ZPL2,APL2,BPL2,CPL2,DPL2,
     -      ICOL2,IFAIL2)
       IF(IFAIL1.NE.0.OR.IFAIL2.NE.0.OR.NPL1.LE.2.OR.NPL2.LE.2)THEN
            PRINT *,' !!!!!! PLAGT  WARNING : Error fetching a plane'//
     -           ' ; overlap set to False.'
            PLAGT=.FALSE.
            RETURN
       ENDIF
*** Compute and epsilon for equality comparisons.
       IF(LEPSG)THEN
            EPS=EPSGZ
       ELSE
            ZMIN=ZPL1(1)
            ZMAX=ZPL1(1)
            DO 50 I=2,NPL1
            ZMIN=MIN(ZMIN,ZPL1(I))
            ZMAX=MAX(ZMAX,ZPL1(I))
50          CONTINUE
            DO 60 I=1,NPL2
            ZMIN=MIN(ZMIN,ZPL2(I))
            ZMAX=MAX(ZMAX,ZPL2(I))
60          CONTINUE
            EPS=1.0D-6*ABS(ZMAX-ZMIN)
       ENDIF
*** Check for perpendicular planes.
       IF(CPL1.EQ.0.OR.CPL2.EQ.0)THEN
            PLAGT=.FALSE.
            RETURN
       ENDIF
*** Initial setting of the flags.
       LT12=.FALSE.
       EQ12=.FALSE.
       GT12=.FALSE.
*** Find the corners of 1 internal to 2.
       DO 10 I=1,NPL1
       CALL INTERD(NPL2,XPL2,YPL2,XPL1(I),YPL1(I),INSIDE,EDGE)
*   For these points, compute the offset projected on plane 2.
       IF(INSIDE.OR.EDGE)THEN
            OFFSET=(DPL2-APL2*XPL1(I)-BPL2*YPL1(I))/CPL2
            IF(ABS(OFFSET-ZPL1(I)).LT.EPS)THEN
                 EQ12=.TRUE.
            ELSEIF(ZPL1(I).GT.OFFSET)THEN
                 GT12=.TRUE.
            ELSEIF(ZPL1(I).LT.OFFSET)THEN
                 LT12=.TRUE.
            ENDIF
       ENDIF
10     CONTINUE
*** Find the corners of 2 internal to 1.
       DO 20 I=1,NPL2
       CALL INTERD(NPL1,XPL1,YPL1,XPL2(I),YPL2(I),INSIDE,EDGE)
*   For these points, compute the offset projected on plane 1.
       IF(INSIDE.OR.EDGE)THEN
            OFFSET=(DPL1-APL1*XPL2(I)-BPL1*YPL2(I))/CPL1
            IF(ABS(OFFSET-ZPL2(I)).LT.EPS)THEN
                 EQ12=.TRUE.
            ELSEIF(OFFSET.GT.ZPL2(I))THEN
                 GT12=.TRUE.
            ELSEIF(OFFSET.LT.ZPL2(I))THEN
                 LT12=.TRUE.
            ENDIF
       ENDIF
20     CONTINUE
*** Check for mid-line intersects.
       DO 30 I=1,NPL1
       DO 40 J=1,NPL2
       CALL CRSPND(
     -      XPL1(1+MOD(I-1,NPL1)),YPL1(1+MOD(I-1,NPL1)),
     -      XPL1(1+MOD(I  ,NPL1)),YPL1(1+MOD(I  ,NPL1)),
     -      XPL2(1+MOD(J-1,NPL2)),YPL2(1+MOD(J-1,NPL2)),
     -      XPL2(1+MOD(J  ,NPL2)),YPL2(1+MOD(J  ,NPL2)),
     -      XC,YC,CROSS)
       IF(CROSS)THEN
            OFF1=(DPL1-APL1*XC-BPL1*YC)/CPL1
            OFF2=(DPL2-APL2*XC-BPL2*YC)/CPL2
            IF(ABS(OFF1-OFF2).LT.EPS)THEN
                 EQ12=.TRUE.
            ELSEIF(OFF1.GT.OFF2)THEN
                 GT12=.TRUE.
            ELSEIF(OFF1.LT.OFF2)THEN
                 LT12=.TRUE.
            ENDIF
       ENDIF
40     CONTINUE
30     CONTINUE
*** Check the final flags.
       IF(LT12.AND.GT12)THEN
            PRINT *,' !!!!!! PLAGT  WARNING : Planes probably'//
     -           ' intersect ; plot probably incorrect.'
            PLAGT=.TRUE.
       ELSEIF(GT12)THEN
            PLAGT=.TRUE.
       ELSE
            PLAGT=.FALSE.
       ENDIF
       END
