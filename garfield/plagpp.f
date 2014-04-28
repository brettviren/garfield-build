CDECK  ID>, PLAGPP.
       SUBROUTINE PLAGPP(NPL,XPL,YPL,ZPL)
*-----------------------------------------------------------------------
*   PLAGPP - Plots a curve through the visible parts.
*   (Last changed on 26/ 7/10.)
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
       DOUBLE PRECISION CBUF(MXSBUF)
       CHARACTER SOLTYP(MXSOLI)
       INTEGER NSOLID,ISTART(MXSOLI),ISOLTP(MXSOLI),INDSOL(MXSOLI),
     -      ICCURR,IQ(MXPLAN),NQ,ISOLMT(MXSOLI),IWFBEM(MXSW)
       COMMON /SOLIDS/ CBUF,ISTART,INDSOL,IWFBEM,ISOLTP,NSOLID,ICCURR,
     -      IQ,NQ,ISOLMT
       COMMON /SOLCHR/ SOLTYP
       REAL USERX0,USERX1,USERY0,USERY1,FRXMIN,FRXMAX,FRYMIN,FRYMAX,
     -      ARRANG,ARRLEN,BARFRC,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT
       LOGICAL LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,
     -      LWAITA,LWAITB,LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      WKMULT(MXWKLS)
       INTEGER NWK,WKID(MXWKLS),WKCON(MXWKLS),WKFREF(MXWKLS),
     -         WKLUN(MXWKLS),WKSTAT(MXWKLS),WKSREQ(MXWKLS),
     -         NCWKNM(MXWKLS),NCSTMP,IGHIST,IGBAR,NCGKS
       CHARACTER*20 WKNAME(MXWKLS),WKATTR(MXWKLS)
       CHARACTER*80 STAMP
       CHARACTER*(MXNAME) GKSLOG
       COMMON /GRADAT/ USERX0,USERX1,USERY0,USERY1,ARRANG,ARRLEN,
     -      BARFRC,
     -      FRXMIN,FRXMAX,FRYMIN,FRYMAX,DISPX0,DISPX1,DISPY0,DISPY1,
     -      GPXN,GPXN10,GPYN,GPYN10,GPXL,GPYL,GPXT,
     -      LGRID,LGRALL,LOGX,LOGY,LSTAMP,LGCLRB,LGCLRA,LWAITA,LWAITB,
     -      LXCCH,LGLCLP,LGMCLP,LGACLP,LGTCLP,
     -      NWK,WKID,WKCON,WKFREF,WKLUN,WKSTAT,WKSREQ,NCWKNM,NCSTMP,
     -      IGHIST,IGBAR,NCGKS,WKMULT
       COMMON /GRACHR/ WKNAME,WKATTR,STAMP,GKSLOG
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       INTEGER NPL,I,J,K,L,NPL1,ICOL,IFAIL,NCUR,NNEW,NL,IQMIN,I0
       DOUBLE PRECISION XPL(NPL),YPL(NPL),ZPL(NPL),
     -      XPL1(MXEDGE),YPL1(MXEDGE),ZPL1(MXEDGE),
     -      XCUR(2*MXLIST),YCUR(2*MXLIST),ZCUR(2*MXLIST),
     -      XNEW(2*MXLIST),YNEW(2*MXLIST),ZNEW(2*MXLIST),
     -      XL(MXEDGE),YL(MXEDGE),ZL(MXEDGE),QL(MXEDGE),
     -      APL,BPL,CPL,DPL,XC,YC,ZC,XAUX,YAUX,ZAUX,QMIN,QAUX,
     -      X0,Y0,Z0,X1,Y1,Z1,EPSX,EPSY,EPSZ
       LOGICAL DRAW(2*MXLIST),DRAWN(2*MXLIST),INSIDE,EDGE,CROSS,CURIN,
     -      LASTIN
*** Set tolerances.
       IF(LEPSG)THEN
            EPSX=EPSGX
            EPSY=EPSGY
            EPSZ=EPSGZ
       ELSE
            EPSX=1.0D-8*ABS(GXMAX-GXMIN)
            EPSY=1.0D-8*ABS(GYMAX-GYMIN)
            EPSZ=1.0D-8*ABS(GZMAX-GZMIN)
            IF(EPSX.LE.0)EPSX=1.0D-8
            IF(EPSY.LE.0)EPSY=1.0D-8
            IF(EPSZ.LE.0)EPSZ=1.0D-8
       ENDIF
*** Copy the curve, section by section, set initial number of points.
       NCUR=0
*   Loop over the points.
       DO 10 I=1,NPL-1
*   Make copies of the current and the last point.
       X0=XPL(I)
       Y0=YPL(I)
       Z0=ZPL(I)
       LASTIN=X0.GE.GXMIN.AND.X0.LE.GXMAX.AND.
     -      Y0.GE.GYMIN.AND.Y0.LE.GYMAX.AND.
     -      Z0.GE.GZMIN.AND.Z0.LE.GZMAX
       X1=XPL(I+1)
       Y1=YPL(I+1)
       Z1=ZPL(I+1)
       CURIN=X1.GE.GXMIN.AND.X1.LE.GXMAX.AND.
     -      Y1.GE.GYMIN.AND.Y1.LE.GYMAX.AND.
     -      Z1.GE.GZMIN.AND.Z1.LE.GZMAX
*   Adjust this piece to the dimensions of the box.
       CALL CLIP3D(X0,Y0,Z0,X1,Y1,Z1,
     -      GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX,IFAIL)
*   If outside the box, skip the section altogether.
       IF(IFAIL.NE.0)THEN
            GOTO 10
*   Crossing of box, last point in: add current point.
       ELSEIF(LASTIN)THEN
            IF(NCUR.EQ.0)THEN
                 NCUR=1
                 CALL PLACO3(X0,Y0,Z0,XCUR(NCUR),YCUR(NCUR),ZCUR(NCUR))
            ENDIF
            IF(NCUR.GE.2*MXLIST)GOTO 3010
            DRAW(NCUR)=.TRUE.
            NCUR=NCUR+1
            CALL PLACO3(X1,Y1,Z1,XCUR(NCUR),YCUR(NCUR),ZCUR(NCUR))
            DRAW(NCUR)=CURIN
*   Crossing of box, last not in: add entry and exit.
       ELSE
            IF(NCUR.GT.0)DRAW(NCUR)=.FALSE.
            IF(NCUR.GE.2*MXLIST)GOTO 3010
            NCUR=NCUR+1
            CALL PLACO3(X0,Y0,Z0,XCUR(NCUR),YCUR(NCUR),ZCUR(NCUR))
            DRAW(NCUR)=.TRUE.
            IF(NCUR.GE.2*MXLIST)GOTO 3010
            NCUR=NCUR+1
            CALL PLACO3(X1,Y1,Z1,XCUR(NCUR),YCUR(NCUR),ZCUR(NCUR))
            DRAW(NCUR)=CURIN
       ENDIF
10     CONTINUE
*** See whether we have collected anything.
       IF(NCUR.LT.2)RETURN
*** Load all plot panels to see whether there is a crossing.
       DO 20 J=1,NQ
       CALL PLABU2('READ',IQ(J),NPL1,XPL1,YPL1,ZPL1,APL,BPL,CPL,DPL,
     -      ICOL,IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! PLAGPP WARNING : Unable to load a'//
     -           ' plot panel ; curve not plotted.'
            RETURN
       ENDIF
       IF(NPL1.LE.2)GOTO 20
*   Skip this panel if it is almost normal.
       IF(ABS(CPL).LT.1.0D-4*SQRT(APL**2+BPL**2))GOTO 20
*** Go over all line segments.
       NNEW=0
       DO 30 I=1,NCUR-1
*   For invisible and point segments, just register the starting point.
       IF((.NOT.DRAW(I)).OR.
     -      (ABS(XCUR(I+1)-XCUR(I)).LE.EPSX.AND.
     -       ABS(YCUR(I+1)-YCUR(I)).LE.EPSY.AND.
     -       ABS(ZCUR(I+1)-ZCUR(I)).LE.EPSZ))THEN
            IF(NNEW+1.GT.2*MXLIST)THEN
                 PRINT *,' !!!!!! PLAGPP WARNING : Too many'//
     -                ' points generated on curve; not plotted.'
                 RETURN
            ENDIF
            IF(NNEW.GE.2*MXLIST)GOTO 3020
            NNEW=NNEW+1
            XNEW(NNEW)=XCUR(I)
            YNEW(NNEW)=YCUR(I)
            ZNEW(NNEW)=ZCUR(I)
            DRAWN(NNEW)=.FALSE.
            GOTO 30
       ENDIF
*   Establish the list of crossings.
       NL=2
       XL(1)=XCUR(I)
       YL(1)=YCUR(I)
       ZL(1)=ZCUR(I)
       QL(1)=0
       XL(2)=XCUR(I+1)
       YL(2)=YCUR(I+1)
       ZL(2)=ZCUR(I+1)
       QL(2)=1
*   Check for crossings in the plane.
       CALL PLALIN(
     -      XCUR(I)  ,YCUR(I)  ,ZCUR(I)  ,
     -      XCUR(I+1),YCUR(I+1),ZCUR(I+1),
     -      XPL1(1)  ,YPL1(1)  ,ZPL1(1)  ,
     -      APL,BPL,CPL,XC,YC,ZC,IFAIL)
       IF(IFAIL.EQ.0)THEN
            CALL INTERD(NPL1,XPL1,YPL1,XC,YC,INSIDE,EDGE)
            IF(INSIDE)THEN
                 IF(NL+1.GT.MXEDGE)THEN
                      PRINT *,' !!!!!! PLAGPP WARNING : Too many'//
     -                     '  crossings between curve and surface'//
     -                     ' elements; not plotted.'
                      RETURN
                 ENDIF
                 NL=NL+1
                 XL(NL)=XC
                 YL(NL)=YC
                 ZL(NL)=(DPL-APL*XL(NL)-BPL*YL(NL))/CPL
                 CALL PLALAM(XCUR(I),XL(NL),XCUR(I+1),
     -                YCUR(I),YL(NL),YCUR(I+1),QL(NL))
            ENDIF
       ENDIF
*   Check for crossings on the edges.
       DO 40 K=1,NPL1
       CALL CRSPND(
     -      XPL1(1+MOD(K-1,NPL1)),YPL1(1+MOD(K-1,NPL1)),
     -      XPL1(1+MOD(K  ,NPL1)),YPL1(1+MOD(K  ,NPL1)),
     -      XCUR(I),YCUR(I),XCUR(I+1),YCUR(I+1),
     -      XC,YC,CROSS)
       IF(.NOT.CROSS)GOTO 40
       IF(NL+1.GT.MXEDGE)THEN
            PRINT *,' !!!!!! PLAGPP WARNING : Too many crossings'//
     -           ' between curve and surface elements; not plotted.'
            RETURN
       ENDIF
       NL=NL+1
       XL(NL)=XC
       YL(NL)=YC
       CALL PLALAM(XCUR(I),XL(NL),XCUR(I+1),YCUR(I),YL(NL),YCUR(I+1),
     -      QL(NL))
       ZL(NL)=ZCUR(I)+QL(NL)*(ZCUR(I+1)-ZCUR(I))
40     CONTINUE
*   Sort the list by using the lambda's.
       DO 60 K=1,NL-1
       QMIN=QL(K)
       IQMIN=K
       DO 50 L=K+1,NL
       IF(QL(L).LT.QMIN)THEN
            IQMIN=L
            QMIN=QL(L)
       ENDIF
50     CONTINUE
       IF(K.NE.IQMIN)THEN
            XAUX=XL(K)
            YAUX=YL(K)
            ZAUX=ZL(K)
            QAUX=QL(K)
            XL(K)=XL(IQMIN)
            YL(K)=YL(IQMIN)
            ZL(K)=ZL(IQMIN)
            QL(K)=QL(IQMIN)
            XL(IQMIN)=XAUX
            YL(IQMIN)=YAUX
            ZL(IQMIN)=ZAUX
            QL(IQMIN)=QAUX
       ENDIF
60     CONTINUE
*   Copy the points to the new vector.
       DO 70 K=1,NL-1
       IF(NNEW+1.GT.2*MXLIST)THEN
            PRINT *,' !!!!!! PLAGPP WARNING : Too many points'//
     -           ' generated on curve; not plotted.'
            RETURN
       ENDIF
       IF(NNEW.GE.2*MXLIST)GOTO 3020
       NNEW=NNEW+1
       XNEW(NNEW)=XL(K)
       YNEW(NNEW)=YL(K)
       ZNEW(NNEW)=ZL(K)
       CALL INTERD(NPL1,XPL1,YPL1,(XL(K)+XL(K+1))/2,(YL(K)+YL(K+1))/2,
     -      INSIDE,EDGE)
       IF(.NOT.(INSIDE.OR.EDGE).OR.
     -      (ZL(K)+ZL(K+1))/2.GE.(DPL-APL*(XL(K)+XL(K+1))/2-
     -      BPL*(YL(K)+YL(K+1))/2)/CPL)THEN
            DRAWN(NNEW)=.TRUE.
       ELSE
            DRAWN(NNEW)=.FALSE.
       ENDIF
70     CONTINUE
*   Next line segment.
30     CONTINUE
*   Place the last point of this section in the list.
       IF(NNEW.GE.2*MXLIST)GOTO 3020
       NNEW=NNEW+1
       XNEW(NNEW)=XCUR(NCUR)
       YNEW(NNEW)=YCUR(NCUR)
       ZNEW(NNEW)=ZCUR(NCUR)
       DRAWN(NNEW)=.TRUE.
*   Copy this list back to the main curve, eliminating invisible parts.
       IF(DRAWN(1))THEN
            NCUR=1
            XCUR(NCUR)=XNEW(1)
            YCUR(NCUR)=YNEW(1)
            ZCUR(NCUR)=ZNEW(1)
            DRAW(NCUR)=DRAWN(1)
       ELSE
            NCUR=0
       ENDIF
       DO 80 I=2,NNEW
       IF(.NOT.DRAWN(I).AND..NOT.DRAWN(I-1))GOTO 80
       IF(NCUR.GE.2*MXLIST)GOTO 3010
       NCUR=NCUR+1
       XCUR(NCUR)=XNEW(I)
       YCUR(NCUR)=YNEW(I)
       ZCUR(NCUR)=ZNEW(I)
       DRAW(NCUR)=DRAWN(I)
80     CONTINUE
*   Next panel.
20     CONTINUE
*** Plot the remaining line.
       I0=1
       DO 100 I=1,NCUR-1
       IF(.NOT.DRAW(I))THEN
            IF(I-I0+1.GE.2)CALL GPL2(I-I0+1,XCUR(I0),YCUR(I0))
            I0=I+1
       ENDIF
100    CONTINUE
       IF(NCUR-I0+1.GE.2)CALL GPL2(NCUR-I0+1,XCUR(I0),YCUR(I0))
       RETURN
*** Error processing.
3010   CONTINUE
       PRINT *,' !!!!!! PLAGPP WARNING : Curve contains too many'//
     -      ' points ; curve not plotted.'
       RETURN
3020   CONTINUE
       PRINT *,' !!!!!! PLAGPP WARNING : Too many points'//
     -      ' generated on curve; not plotted.'
       END
