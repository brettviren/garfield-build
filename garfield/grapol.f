CDECK  ID>, GRAPOL.
       SUBROUTINE GRAPOL(RMIN1,PMIN1,RMAX1,PMAX1,RTXT,PTXT,TITLE)
*----------------------------------------------------------------------
*   GRAPOL - Subroutine plotting axis, annotating them and adding
*            tickmarks along them.
*            This routine is used for polar coordinate systems.
*   VARIABLES : RMIN,RMAX   : User minimum and maximum for plots in r
*               PMIN,PMAX   : User minimum and maximum for plots in phi
*               XTXT,YTXT   : Title along the x and y axis.
*               TITLE       : Global title.
*   (Last changed on 27/10/11.)
*----------------------------------------------------------------------
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL XPL(101),YPL(101)
       CHARACTER*(*) TITLE
       CHARACTER*40 RTXT,PTXT
       CHARACTER*66 TEXT
       CHARACTER*13 TICK
*** Define some formats
1010   FORMAT(A40,' Scaling factor= 10^',I2,'   ')
1020   FORMAT(A40,'                        ')
*** Define 2 statement function to convert from user to disp frame.
       XUTOD(X)=DISPX0+(DISPX1-DISPX0)*(X-USERX0)/(USERX1-USERX0)
       YUTOD(Y)=DISPY0+(DISPY1-DISPY0)*(Y-USERY0)/(USERY1-USERY0)
*** Switch to graphics mode.
       CALL GRGRAF(.TRUE.)
*** Define display area of screen.
       CALL GSVP(1,DISPX0,DISPX1,DISPY0,DISPY1)
*** Transform input parameters to polar coordinates.
       RMIN=EXP(MIN(RMIN1,RMAX1))
       RMAX=EXP(MAX(RMIN1,RMAX1))
       PMIN=MOD(MIN(PMIN1,PMAX1),2.0*PI)
       PMAX=MOD(MAX(PMIN1,PMAX1),2.0*PI)
*** Check input data, could cause overflows.
       IF(PMIN.EQ.PMAX)THEN
            WRITE(10,*) ' !!!!!! GRAPOL WARNING : Phi bounds are'//
     -           ' equal ; set to -pi, pi.'
            PMIN=-PI
            PMAX=+PI
       ENDIF
       IF(RMIN.EQ.RMAX)THEN
            WRITE(10,*) ' !!!!!! GRAPOL WARNING : R bounds are'//
     -           ' equal ; set to 1, 10.'
            RMIN=1.0
            RMAX=10.0
       ENDIF
*** Produce some debugging output
       IF(LDEBUG)WRITE(10,'(''  ++++++ GRAPOL DEBUG   : Polar'',
     -      '' bounds are ('',E12.5,'','',E12.5,''), ('',E12.5,
     -      '','',E12.5,'').'')') RMIN,PMIN,RMAX,PMAX
*** Prepare a box around the user area and find the area,
       XMIN=RMIN*COS(PMIN)
       XMAX=XMIN
       YMIN=RMIN*SIN(PMIN)
       YMAX=YMIN
       DO 10 I=0,49
       IF(PMIN.GT.PMAX)THEN
            ANGLE=PMIN+I*(PMAX-PMIN+2.0*PI)/49.0
       ELSE
            ANGLE=PMIN+I*(PMAX-PMIN)/49.0
       ENDIF
       XPL(I+1)=RMIN*COS(ANGLE)
       YPL(I+1)=RMIN*SIN(ANGLE)
       XPL(100-I)=RMAX*COS(ANGLE)
       YPL(100-I)=RMAX*SIN(ANGLE)
       XMIN=MIN(XMIN,XPL(I+1),XPL(100-I))
       XMAX=MAX(XMAX,XPL(I+1),XPL(100-I))
       YMIN=MIN(YMIN,YPL(I+1),YPL(100-I))
       YMAX=MAX(YMAX,YPL(I+1),YPL(100-I))
10     CONTINUE
       XPL(101)=XPL(1)
       YPL(101)=YPL(1)
*   make the box squared.
       DIFF=YMAX-YMIN-XMAX+XMIN
       IF(DIFF.GT.0.0)THEN
            XMAX=XMAX+DIFF/2.0
            XMIN=XMIN-DIFF/2.0
       ELSE
            YMAX=YMAX-DIFF/2.0
            YMIN=YMIN+DIFF/2.0
       ENDIF
*** Store frame size.
       FRXMIN=XMIN
       FRXMAX=XMAX
       FRYMIN=YMIN
       FRYMAX=YMAX
*** Define user area in the plot frame.
       USERX0=XMIN-0.1*(XMAX-XMIN)/(DISPX1-DISPX0-0.2)
       USERX1=XMAX+0.1*(XMAX-XMIN)/(DISPX1-DISPX0-0.2)
       USERY0=YMIN-0.1*(YMAX-YMIN)/(DISPY1-DISPY0-0.2)
       USERY1=YMAX+0.1*(YMAX-YMIN)/(DISPY1-DISPY0-0.2)
       CALL GSWN(1,USERX0,USERX1,USERY0,USERY1)
       CALL GSELNT(1)
*** Plot the box.
       CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       CALL GPL(101,XPL,YPL)
*** Find reasonable scale order-of-magnitude, first in r.
       KR=INT(LOG10(RMAX-RMIN))
       KKR=3*INT(LOG10(RMAX-RMIN)/3.0)
       IF(LOG10(RMAX-RMIN).LT.0.0)KR=KR-1
       IF(RMAX-RMIN.LT.1.0)KKR=KKR-3
       DR=(RMAX-RMIN)/10.0**KR
*   And also in phi.
       IF(PMIN.LT.PMAX)THEN
            KP=INT(LOG10(180.0*(PMAX-PMIN)/PI))
            KKP=3*INT(LOG10(180.0*(PMAX-PMIN)/PI)/3.0)
            IF(LOG10(180.0*(PMAX-PMIN)/PI).LT.0.0)KP=KP-1
            IF(180.0*(PMAX-PMIN)/PI.LT.1.0)KKP=KKP-1
            DP=ABS(180.0*(PMAX-PMIN)/PI)/10.0**KP
       ELSE
            KP=INT(LOG10(ABS(180.0*(PMAX-PMIN+2.0*PI)/PI)))
            KKP=3*INT(LOG10(ABS(180.0*(PMAX-PMIN+2.0*PI)/PI))/3.0)
            IF(LOG10(ABS(180.0*(PMAX-PMIN+2.0*PI)/PI)).LT.0.0)KP=KP-1
            IF(ABS(180.0*(PMAX-PMIN+2.0*PI)/PI).LT.1.0)KKP=KKP-1
            DP=ABS(180.0*(PMAX-PMIN+2.0*PI)/PI)/10.0**KP
       ENDIF
*   Find the distance between 2 tickmarks.
       IF(DR.LT.2.0)DR=0.1
       IF(DR.GE.2.0.AND.DR.LT.5.0)DR=0.2
       IF(DR.GE.5.0)DR=0.5
       IF(DP.LT.2.0)DP=0.1
       IF(DP.GE.2.0.AND.DP.LT.5.0)DP=0.2
       IF(DP.GE.5.0)DP=0.5
       DR=DR*10.0**KR
       DP=(PI/180.0)*DP*10.0**KP
*** Plot tickmarks and scale on the arcs, compute number of tick marks.
       IF(PMAX.GT.PMIN)THEN
            NTICK=(PMAX-PMIN)/DP
       ELSE
            NTICK=(PMAX-PMIN+2.0*PI)/DP
       ENDIF
*   Set graphics attributes for the labels.
       CALL GRATTS('NUMBERS','TEXT')
*   Loop over the tickmarks.
       DO 20 I=0,NTICK+1
       ANGLE=DP*(I+INT(PMIN/DP))
       IF(PMIN.GT.PMAX.AND.ANGLE.GT.PMAX+2.0*PI)GOTO 20
       IF(PMIN.LE.PMAX.AND.(ANGLE.GT.PMAX.OR.ANGLE.LT.PMIN))GOTO 20
*   Plot the grid if requested.
       IF(LGRID)THEN
            XPL(1)=RMIN*COS(ANGLE)
            YPL(1)=RMIN*SIN(ANGLE)
            XPL(2)=RMAX*COS(ANGLE)
            YPL(2)=RMAX*SIN(ANGLE)
            CALL GRATTS('GRID','POLYLINE')
            CALL GPL(2,XPL,YPL)
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       ENDIF
*   Plot tickmarks.
       XPL(1)=RMIN*COS(ANGLE)
       YPL(1)=RMIN*SIN(ANGLE)
       IF(XPL(1)**2+YPL(1)**2.GT.0.0)THEN
            XPL(2)=XPL(1)*(1.0+0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2))
            YPL(2)=YPL(1)*(1.0+0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2))
            CALL GPL(2,XPL,YPL)
       ENDIF
       XPL(1)=RMAX*COS(ANGLE)
       YPL(1)=RMAX*SIN(ANGLE)
       IF(XPL(1)**2+YPL(1)**2.GT.0.0)THEN
            XPL(2)=XPL(1)*(1.0-0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2))
            YPL(2)=YPL(1)*(1.0-0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2))
            CALL GPL(2,XPL,YPL)
       ENDIF
*   Bring the angle in the normal range.
       ANGSCL=MOD(180.0*ANGLE/PI,360.0)
       IF(ANGSCL.GT.+180.0)ANGSCL=ANGSCL-360.0
       IF(ANGSCL.LE.-180.0)ANGSCL=ANGSCL+360.0
*   Format the number.
       CALL OUTFMT(ANGSCL/10.0**KKP,2,TICK,NC,'LEFT')
*   And plot the number.
       IF(XPL(1)**2+YPL(1)**2.GT.0.0)THEN
            XSC=XUTOD((RMAX+0.015*(RMAX-RMIN))*COS(ANGLE))
            YSC=YUTOD((RMAX+0.015*(RMAX-RMIN))*SIN(ANGLE))
            CALL GSELNT(0)
            CALL GSCHUP(YPL(2)-YPL(1),XPL(1)-XPL(2))
            CALL GSTXAL(1,3)
            CALL GRTX(XSC,YSC,TICK(1:NC))
            CALL GSELNT(1)
       ENDIF
20     CONTINUE
*** Tickmarks and scale on one of the straight segments.
       NTICK=ABS(RMAX-RMIN)/DR
*   Loop over the tickmarks.
       DO 30 I=0,NTICK+1
       RVAL=DR*(I+INT(RMIN/DR))
       IF(RVAL.GT.RMAX.OR.RVAL.LT.RMIN)GOTO 30
*   Optional grid.
       IF(LGRID)THEN
            DO 40 J=1,100
            XPL(J)=RVAL*COS(PMIN+REAL(J-1)*(PMAX-PMIN)/99.0)
            YPL(J)=RVAL*SIN(PMIN+REAL(J-1)*(PMAX-PMIN)/99.0)
40          CONTINUE
            CALL GRATTS('GRID','POLYLINE')
            CALL GPL(100,XPL,YPL)
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       ENDIF
*   Plot the tickmarks, plot scale at the same time.
       XPL(1)=RVAL*COS(PMIN)
       YPL(1)=RVAL*SIN(PMIN)
       IF(XPL(1)**2+YPL(1)**2.GT.0.0)THEN
            XPL(2)=XPL(1)-YPL(1)*0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2)
            YPL(2)=YPL(1)+XPL(1)*0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2)
            CALL GPL(2,XPL,YPL)
            IF(SIN(PMIN).LT.SIN(PMAX))THEN
                 CALL OUTFMT(RVAL/10.0**KKR,2,TICK,NC,'LEFT')
                 XSC=XUTOD(XPL(1)+YPL(1)*0.015*(XMAX-XMIN)/
     -                SQRT(XPL(1)**2+YPL(1)**2))
                 YSC=YUTOD(YPL(1)-XPL(1)*0.015*(XMAX-XMIN)/
     -                SQRT(XPL(1)**2+YPL(1)**2))
                 CALL GSELNT(0)
                 CALL GSCHUP(YPL(2)-YPL(1),XPL(1)-XPL(2))
                 CALL GSTXAL(1,3)
                 CALL GRTX(XSC,YSC,TICK(1:NC))
                 CALL GSELNT(1)
            ENDIF
       ENDIF
*   And tickmarks and perhaps a scale on the other axis.
       XPL(1)=RVAL*COS(PMAX)
       YPL(1)=RVAL*SIN(PMAX)
       IF(XPL(1)**2+YPL(1)**2.GT.0.0)THEN
            XPL(2)=XPL(1)+YPL(1)*0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2)
            YPL(2)=YPL(1)-XPL(1)*0.01*(XMAX-XMIN)/
     -           SQRT(XPL(1)**2+YPL(1)**2)
            CALL GPL(2,XPL,YPL)
            IF(SIN(PMIN).GE.SIN(PMAX))THEN
                 CALL OUTFMT(RVAL/10.0**KKR,2,TICK,NC,'LEFT')
                 XSC=XUTOD(XPL(1)-YPL(1)*0.015*(XMAX-XMIN)/
     -                SQRT(XPL(1)**2+YPL(1)**2))
                 YSC=YUTOD(YPL(1)+XPL(1)*0.015*(XMAX-XMIN)/
     -                SQRT(XPL(1)**2+YPL(1)**2))
                 CALL GSELNT(0)
                 CALL GSCHUP(YPL(1)-YPL(2),XPL(2)-XPL(1))
                 CALL GSTXAL(1,3)
                 CALL GRTX(XSC,YSC,TICK(1:NC))
                 CALL GSELNT(1)
            ENDIF
       ENDIF
30     CONTINUE
*** Write the titles and the orders of magnitudes at the bottom,
       CALL GSELNT(0)
       CALL GSCHUP(0.0,1.0)
       IF(KKP.NE.0)THEN
            WRITE(TEXT,1010) PTXT,KKP
       ELSE
            WRITE(TEXT,1020) PTXT
       ENDIF
       CALL GSTXAL(1,0)
       CALL GRATTS('LABELS','TEXT')
       CALL GRTX(0.1,0.01,TEXT)
       IF(KKR.NE.0)THEN
            WRITE(TEXT,1010) RTXT,KKR
       ELSE
            WRITE(TEXT,1020) RTXT
       ENDIF
       CALL GRTX(0.1,0.04,TEXT)
       CALL GRATTS('TITLE','TEXT')
       CALL GRTX(0.1,0.97,TITLE)
*   reset GKS parameters.
       CALL GSELNT(1)
       CALL GSTXAL(0,0)
*** Reset the bar chart and histogram counters.
       IGBAR=0
       IGHIST=0
       END
