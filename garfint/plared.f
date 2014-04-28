CDECK  ID>, PLARED.
       SUBROUTINE PLARED(NPL,XPL,YPL,ZPL,A,B,C,D)
*-----------------------------------------------------------------------
*   PLARED - Removes duplicate branches from a curve.
*   (Last changed on  2/ 2/98.)
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
       INTEGER NPL,I,J,NNEW,JCUT
       DOUBLE PRECISION XPL(NPL),YPL(NPL),ZPL(NPL),EPSX,EPSY,
     -      XSHIFT,YSHIFT,EPS,A,B,C,D,XMIN,YMIN,XMAX,YMAX
       LOGICAL MARK(MXEDGE),ONLIND
       EXTERNAL ONLIND
*** Check number of points.
       IF(NPL.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLARED WARNING : Too many points.'
            RETURN
       ELSEIF(NPL.LT.3)THEN
            RETURN
       ENDIF
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
       ELSE
*   Compute range.
            XMIN=XPL(1)
            XMAX=XPL(1)
            YMIN=YPL(1)
            YMAX=YPL(1)
            DO 90 I=2,NPL
            XMIN=MIN(XMIN,XPL(I))
            XMAX=MAX(XMAX,XPL(I))
            YMIN=MIN(YMIN,YPL(I))
            YMAX=MAX(YMAX,YPL(I))
90          CONTINUE
*   Set epsilons accordingly.
            EPSX=1.0D-8*ABS(XMAX-XMIN)
            EPSY=1.0D-8*ABS(YMAX-YMIN)
            IF(EPSX.LE.0)EPSX=1.0D-8
            IF(EPSY.LE.0)EPSY=1.0D-8
       ENDIF
*** Make a first marker list.
100    CONTINUE
       DO 10 I=1,NPL
       MARK(I)=.FALSE.
10     CONTINUE
*** Find a point that is surrounded on both side by equal points.
       DO 20 I=1,NPL
       JCUT=0
       DO 30 J=1,NPL/2
       IF(  ABS(XPL(1+MOD(I+J-1    ,NPL))-
     -          XPL(1+MOD(I-J-1+NPL,NPL))).GT.EPSX.OR.
     -      ABS(YPL(1+MOD(I+J-1    ,NPL))-
     -          YPL(1+MOD(I-J-1+NPL,NPL))).GT.EPSY)GOTO 40
       JCUT=J
30     CONTINUE
40     CONTINUE
*   See whether we found one.
       IF(JCUT.GT.0)THEN
C      print *,' Cutting a tail of ',JCUT,' points.'
            DO 70 J=I-JCUT+1,I+JCUT
            MARK(1+MOD(J-1+NPL,NPL))=.TRUE.
70          CONTINUE
            GOTO 50
       ENDIF
20     CONTINUE
*** See whether there are partial returns.
       DO 80 I=1,NPL
       IF(ONLIND(
     -      XPL(1+MOD(I-1    ,NPL)),YPL(1+MOD(I-1    ,NPL)),
     -      XPL(1+MOD(I      ,NPL)),YPL(1+MOD(I      ,NPL)),
     -      XPL(1+MOD(I-2+NPL,NPL)),YPL(1+MOD(I-2+NPL,NPL))).OR.
     -    ONLIND(
     -      XPL(1+MOD(I-1    ,NPL)),YPL(1+MOD(I-1    ,NPL)),
     -      XPL(1+MOD(I-2+NPL,NPL)),YPL(1+MOD(I-2+NPL,NPL)),
     -      XPL(1+MOD(I      ,NPL)),YPL(1+MOD(I      ,NPL))))THEN
            MARK(1+MOD(I-1    ,NPL))=.TRUE.
C      print *,' Cutting a partial return.'
            GOTO 50
       ENDIF
80     CONTINUE
*** No further cuts, move points which appear twice.
       DO 120 I=1,NPL
       DO 110 J=I+1,NPL
*   Identify the points.
       IF(ABS(XPL(I)-XPL(J)).LT.100*EPSX.AND.
     -      ABS(YPL(I)-YPL(J)).LT.100*EPSY)THEN
*   Find the axis along which to displace the points.
            XSHIFT=(XPL(1+MOD(I-2+NPL,NPL))+XPL(1+MOD(I,NPL)))/2-
     -           XPL(I)
            YSHIFT=(YPL(1+MOD(I-2+NPL,NPL))+YPL(1+MOD(I,NPL)))/2-
     -           YPL(I)
            IF(SQRT(XSHIFT**2+YSHIFT**2).LE.SQRT(EPSX**2+EPSY**2))THEN
                 PRINT *,' !!!!!! PLARED WARNING : Curve is too'//
     -                ' small ; eliminated.'
                 NPL=0
                 RETURN
            ENDIF
            EPS=1000*SQRT(EPSX**2+EPSY**2)/SQRT(XSHIFT**2+YSHIFT**2)
            XPL(I)=XPL(I)+XSHIFT*EPS
            YPL(I)=YPL(I)+YSHIFT*EPS
            ZPL(I)=(D-A*XPL(I)-B*YPL(I))/C
            XSHIFT=(XPL(1+MOD(J-2+NPL,NPL))+XPL(1+MOD(J,NPL)))/2-
     -           XPL(J)
            YSHIFT=(YPL(1+MOD(J-2+NPL,NPL))+YPL(1+MOD(J,NPL)))/2-
     -           YPL(J)
            IF(SQRT(XSHIFT**2+YSHIFT**2).LE.SQRT(EPSX**2+EPSY**2))THEN
                 PRINT *,' !!!!!! PLARED WARNING : Curve is too'//
     -                ' small ; eliminated.'
                 NPL=0
                 RETURN
            ENDIF
            EPS=1000*SQRT(EPSX**2+EPSY**2)/SQRT(XSHIFT**2+YSHIFT**2)
            XPL(J)=XPL(J)+XSHIFT*EPS
            YPL(J)=YPL(J)+YSHIFT*EPS
            ZPL(J)=(D-A*XPL(J)-B*YPL(J))/C
C      print *,' Shifting a point to avoid overlaps.'
       ENDIF
110    CONTINUE
120    CONTINUE
       RETURN
*** Eliminate the piece.
50     CONTINUE
       NNEW=0
       DO 60 I=1,NPL
       IF(MARK(I))GOTO 60
       NNEW=NNEW+1
       XPL(NNEW)=XPL(I)
       YPL(NNEW)=YPL(I)
       ZPL(NNEW)=ZPL(I)
60     CONTINUE
       NPL=NNEW
       GOTO 100
       END
