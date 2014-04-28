CDECK  ID>, BUTFLY.
       SUBROUTINE BUTFLY(NPL,XPL,YPL,ZPL)
*----------------------------------------------------------------------
*   BUTFLY - Tries to eliminate "butterflies", i.e. the crossing of 2
*            adjacent segments of a polygon, by point exchanges.
*   (Last changed on 28/10/07.)
*----------------------------------------------------------------------
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
       INTEGER NPL,I,J,K,NPASS,IAXIS,NNEW
       REAL XPL(NPL),YPL(NPL),ZPL(NPL),XAUX,YAUX,ZAUX,
     -      XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX,EPSX,EPSY,EPSZ,
     -      XSURF,YSURF,ZSURF
       LOGICAL CROSS,REPASS,MARK(MXEDGE)
       EXTERNAL CROSS
*** Check the number of points.
       IF(NPL.GT.MXEDGE)THEN
            PRINT *,' !!!!!! BUTFLY WARNING : Received more than'//
     -           ' MXEDGE points; data not processed.'
            RETURN
       ELSEIF(NPL.LT.3)THEN
            RETURN
       ENDIF
*** Compute range.
       XMIN=XPL(1)
       XMAX=XPL(1)
       YMIN=YPL(1)
       YMAX=YPL(1)
       ZMIN=ZPL(1)
       ZMAX=ZPL(1)
       XSURF=0
       YSURF=0
       ZSURF=0
       DO 100 I=2,NPL
       XMIN=MIN(XMIN,XPL(I))
       XMAX=MAX(XMAX,XPL(I))
       YMIN=MIN(YMIN,YPL(I))
       YMAX=MAX(YMAX,YPL(I))
       ZMIN=MIN(ZMIN,ZPL(I))
       ZMAX=MAX(ZMAX,ZPL(I))
       IF(I.GE.3)THEN
            XSURF=XSURF+ABS(
     -           (YPL(I  )-YPL(1))*(ZPL(I-1)-ZPL(1))-
     -           (YPL(I-1)-YPL(1))*(ZPL(I  )-ZPL(1)))
            YSURF=YSURF+ABS(
     -           (XPL(I  )-XPL(1))*(ZPL(I-1)-ZPL(1))-
     -           (XPL(I-1)-XPL(1))*(ZPL(I  )-ZPL(1)))
            ZSURF=ZSURF+ABS(
     -           (XPL(I  )-XPL(1))*(YPL(I-1)-YPL(1))-
     -           (XPL(I-1)-XPL(1))*(YPL(I  )-YPL(1)))
       ENDIF
100    CONTINUE
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
            EPSZ=EPSGZ
       ELSE
            EPSX=1.0E-5*ABS(XMAX-XMIN)
            EPSY=1.0E-5*ABS(YMAX-YMIN)
            EPSZ=1.0E-5*ABS(ZMAX-ZMIN)
            IF(EPSX.LE.1E-6)EPSX=1.0E-6
            IF(EPSY.LE.1E-6)EPSY=1.0E-6
            IF(EPSZ.LE.1E-6)EPSZ=1.0E-6
       ENDIF
*** Eliminate points appearing twice, initialise marks.
       DO 50 I=1,NPL
       MARK(I)=.FALSE.
50     CONTINUE
*   Scan the list.
       DO 110 I=1,NPL
       IF(MARK(I))GOTO 110
       DO 120 J=I+1,NPL
       IF(ABS(XPL(I)-XPL(J)).LE.EPSX.AND.
     -      ABS(YPL(I)-YPL(J)).LE.EPSY.AND.
     -      ABS(ZPL(I)-ZPL(J)).LE.EPSZ)MARK(J)=.TRUE.
120    CONTINUE
110    CONTINUE
*   And remove the duplicate points.
       NNEW=0
       DO 130 I=1,NPL
       IF(.NOT.MARK(I))THEN
            NNEW=NNEW+1
            XPL(NNEW)=XPL(I)
            YPL(NNEW)=YPL(I)
            ZPL(NNEW)=ZPL(I)
       ENDIF
130    CONTINUE
*   Update the number of points.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BUTFLY DEBUG   : Old /'',
     -      '' new number of points: '',2I3)') NPL,NNEW
       NPL=NNEW
*** No risk of having a butterfly with less than 4 points.
       IF(NPL.LE.3)RETURN
*** Select the axis with the largest norm.
       IF(XSURF.GT.YSURF.AND.XSURF.GT.ZSURF)THEN
            IAXIS=1
       ELSEIF(YSURF.GT.ZSURF)THEN
            IAXIS=2
       ELSE
            IAXIS=3
       ENDIF
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ BUTFLY DEBUG   : Main'',
     -      '' axis: '',I3/26X,''x-Surface: '',E15.8/
     -      26X,''y-Surface: '',E15.8/26X,''z-Surface: '',E15.8)')
     -      IAXIS,XSURF,YSURF,ZSURF
*** Set number of passes to avoid endless loop.
       NPASS=0
*** Make a pass.
40     CONTINUE
       NPASS=NPASS+1
       REPASS=.FALSE.
       DO 10 I=1,NPL
       DO 20 J=I+2,NPL
       IF(J+1.GT.NPL.AND.1+MOD(J,NPL).GE.I)GOTO 20
*   Check for a crossing.
       IF((IAXIS.EQ.1.AND.CROSS(
     -      YPL(1+MOD(I-1,NPL)),ZPL(1+MOD(I-1,NPL)),
     -      YPL(1+MOD(I  ,NPL)),ZPL(1+MOD(I  ,NPL)),
     -      YPL(1+MOD(J-1,NPL)),ZPL(1+MOD(J-1,NPL)),
     -      YPL(1+MOD(J  ,NPL)),ZPL(1+MOD(J  ,NPL)))).OR.
     -      (IAXIS.EQ.2.AND.CROSS(
     -      XPL(1+MOD(I-1,NPL)),ZPL(1+MOD(I-1,NPL)),
     -      XPL(1+MOD(I  ,NPL)),ZPL(1+MOD(I  ,NPL)),
     -      XPL(1+MOD(J-1,NPL)),ZPL(1+MOD(J-1,NPL)),
     -      XPL(1+MOD(J  ,NPL)),ZPL(1+MOD(J  ,NPL)))).OR.
     -      (IAXIS.EQ.3.AND.CROSS(
     -      XPL(1+MOD(I-1,NPL)),YPL(1+MOD(I-1,NPL)),
     -      XPL(1+MOD(I  ,NPL)),YPL(1+MOD(I  ,NPL)),
     -      XPL(1+MOD(J-1,NPL)),YPL(1+MOD(J-1,NPL)),
     -      XPL(1+MOD(J  ,NPL)),YPL(1+MOD(J  ,NPL)))))THEN
*   If there is a crossing, exchange the portion in between.
            DO 30 K=1,(J-I)/2
            XAUX=XPL(1+MOD(I+K-1,NPL))
            YAUX=YPL(1+MOD(I+K-1,NPL))
            ZAUX=ZPL(1+MOD(I+K-1,NPL))
            XPL(1+MOD(I+K-1,NPL))=XPL(1+MOD(J-K,NPL))
            YPL(1+MOD(I+K-1,NPL))=YPL(1+MOD(J-K,NPL))
            ZPL(1+MOD(I+K-1,NPL))=ZPL(1+MOD(J-K,NPL))
            XPL(1+MOD(J-K,NPL))=XAUX
            YPL(1+MOD(J-K,NPL))=YAUX
            ZPL(1+MOD(J-K,NPL))=ZAUX
30          CONTINUE
*   And remember we have to do another pass after this.
            REPASS=.TRUE.
       ENDIF
20     CONTINUE
10     CONTINUE
*** See whether we have to do another pass.
       IF(REPASS.AND.NPASS.LE.NPL)THEN
            GOTO 40
       ELSEIF(REPASS)THEN
            PRINT *,' !!!!!! BUTFLY WARNING : Unable to eliminate'//
     -           ' the internal crossings; plot probably incorrect.'
            IF(LGSTOP)THEN
                 OPEN(UNIT=12,FILE='butfly.dat',STATUS='UNKNOWN')
                 WRITE(12,*) EPSGX,EPSGY,EPSGZ,LEPSG
                 WRITE(12,*) NPL
                 DO 60 I=1,NPL
                 WRITE(12,*) XPL(I),YPL(I),ZPL(I)
60               CONTINUE
                 CLOSE(12)
                 CALL QUIT
            ENDIF
       ENDIF
       END
