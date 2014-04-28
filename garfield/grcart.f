CDECK  ID>, GRCART.
       SUBROUTINE GRCART(XMIN1,YMIN1,XMAX1,YMAX1,XTXT,YTXT,TITLE)
*----------------------------------------------------------------------
*   GRCART - Subroutine plotting axis, annotating them and adding
*            tickmarks along them.
*            This routine is for cartesian coordinates.
*   VARIABLES : XMIN,XMAX   : User minimum and maximum for plots in x.
*               XTXT,YTXT   : Titel along the x and y axis.
*               TITLE       : Global title.
*   (Last changed on 16/ 5/08.)
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
       LOGICAL         LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD
       INTEGER LUNOUT,JFAIL,JEXMEM
       COMMON /PRTPLT/ LINPUT,LCELPR,LCELPL,LWRMRK,LISOCL,LCHGCH,
     -         LDRPLT,LDRPRT,LCLPRT,LCLPLT,LMAPCH,LCNTAM,
     -         LDEBUG,LIDENT,LKEYPL,LRNDMI,LPROPR,LPROF,LGSTOP,LGSIG,
     -         LSYNCH,LINPRD,LUNOUT,JFAIL,JEXMEM
       REAL XU(5),YU(5),XMIN1,YMIN1,XMAX1,YMAX1,XUTOD,YUTOD,X,Y,
     -      XMIN,YMIN,XMAX,YMAX,DX,DY,TICKX,TICKY,XVAL,YVAL,XSC,YSC,
     -      CPX,CPY,XBOX(5),YBOX(5),XPOWER,YPOWER,YSHIFT
       INTEGER NDECX,NDECY,NDEC0,NDEC1,KX,KKX,KY,KKY,NC,I,IDEC,IERR,
     -      IWK
       CHARACTER*(*) XTXT,YTXT,TITLE
       CHARACTER*13 AUX
       CHARACTER*13 TICK
*** Define 2 statement function to convert from USER to DISP.
       XUTOD(X)=DISPX0+(DISPX1-DISPX0)*(X-USERX0)/(USERX1-USERX0)
       YUTOD(Y)=DISPY0+(DISPY1-DISPY0)*(Y-USERY0)/(USERY1-USERY0)
*** Set a workstation for inquiries of the power-of-10 box size.
       IWK=1
*** Output the requested area, if debugging is requested.
       IF(LDEBUG)WRITE(LUNOUT,*) ' ++++++ GRCART DEBUG   : Requested'//
     -      ' area  (',XMIN1,YMIN1,') to (',XMAX1,YMAX1,')'
*** Check input and define maxima and minima, first order x.
       IF(XMAX1.LT.XMIN1)PRINT *,' !!!!!! GRCART WARNING : Maximum'//
     -      ' for x exceeds the minimum ; reversed.'
       XMIN=MIN(XMIN1,XMAX1)
       XMAX=MAX(XMIN1,XMAX1)
*   Check for very small ranges.
       IF(ABS(XMAX-XMIN).LT.1.0E-5*(1.0E-25+ABS(XMIN)+ABS(XMAX)))THEN
            IF(LOGX)THEN
                 XMAX=XMAX*2
                 XMIN=XMIN/2
                 PRINT *,' !!!!!! GRCART WARNING : Zero range in x;'//
     -                ' range enlarged by a factor 2.'
           ELSE
                 XMAX=XMAX+1.0E-4*MAX(1.0,ABS(XMAX))
                 XMIN=XMIN-1.0E-4*MAX(1.0,ABS(XMIN))
                 PRINT *,' !!!!!! GRCART WARNING : Zero range in x;'//
     -                ' range scaled up by 1E-4.'
           ENDIF
       ENDIF
*   Order y.
       IF(YMAX1.LT.YMIN1)PRINT *,' !!!!!! GRCART WARNING : Maximum'//
     -      ' for y exceeds the minimum ; reversed.'
       YMIN=MIN(YMIN1,YMAX1)
       YMAX=MAX(YMIN1,YMAX1)
*   Check for very small ranges.
       IF(ABS(YMAX-YMIN).LT.1.0E-5*(1.0E-25+ABS(YMIN)+ABS(YMAX)))THEN
            IF(LOGY)THEN
                 YMAX=YMAX*2
                 YMIN=YMIN/2
                 PRINT *,' !!!!!! GRCART WARNING : Zero range in y;'//
     -                ' range enlarged by a factor 2.'
           ELSE
                 YMAX=YMAX+1.0E-4*MAX(1.0,ABS(YMAX))
                 YMIN=YMIN-1.0E-4*MAX(1.0,ABS(YMIN))
                 PRINT *,' !!!!!! GRCART WARNING : Zero range in y;'//
     -                ' range scaled up by 1E-4.'
           ENDIF
       ENDIF
*   Avoid negative values on log scales.
       IF((LOGX.AND.(XMAX.LE.0.0.OR.XMIN.LE.0.0)).OR.
     -      (LOGY.AND.(YMAX.LE.0.0.OR.YMIN.LE.0.0)))THEN
            PRINT *,' !!!!!! GRCART WARNING : Non-positive bounds'//
     -           ' found for an axis with log scale; range modified.'
            IF(LOGX.AND.XMIN.LE.0.0.OR.XMAX.LE.0.0)THEN
                 XMIN=MAX(XMIN,1.0E-3)
                 XMAX=MAX(XMIN,XMAX)
                 IF(XMIN.GE.XMAX)THEN
                      XMIN=XMIN/2
                      XMAX=XMAX*2
                 ENDIF
            ENDIF
            IF(LOGY.AND.YMIN.LE.0.0.OR.YMAX.LE.0.0)THEN
                 YMIN=MAX(YMIN,1.0E-3)
                 YMAX=MAX(YMIN,YMAX)
                 IF(YMIN.GE.YMAX)THEN
                      YMIN=YMIN/2
                      YMAX=YMAX*2
                 ENDIF
            ENDIF
       ENDIF
*** Store frame size.
       FRXMIN=XMIN
       FRXMAX=XMAX
       FRYMIN=YMIN
       FRYMAX=YMAX
       IF(LOGX)THEN
            FRXMIN=LOG10(FRXMIN)
            FRXMAX=LOG10(FRXMAX)
       ENDIF
       IF(LOGY)THEN
            FRYMIN=LOG10(FRYMIN)
            FRYMAX=LOG10(FRYMAX)
       ENDIF
*** Switch to graphics mode.
       CALL GRGRAF(.TRUE.)
*** Define display area of frame.
       CALL GSVP(1,DISPX0,DISPX1,DISPY0,DISPY1)
*** Define the user area in the plot frame.
       IF(LOGX)THEN
            USERX0=LOG10(XMIN)-0.1*LOG10(XMAX/XMIN)/(DISPX1-DISPX0-0.2)
            USERX1=LOG10(XMAX)+0.1*LOG10(XMAX/XMIN)/(DISPX1-DISPX0-0.2)
       ELSE
            USERX0=XMIN-0.1*(XMAX-XMIN)/(DISPX1-DISPX0-0.2)
            USERX1=XMAX+0.1*(XMAX-XMIN)/(DISPX1-DISPX0-0.2)
       ENDIF
       IF(LOGY)THEN
            USERY0=LOG10(YMIN)-0.1*LOG10(YMAX/YMIN)/(DISPY1-DISPY0-0.2)
            USERY1=LOG10(YMAX)+0.1*LOG10(YMAX/YMIN)/(DISPY1-DISPY0-0.2)
       ELSE
            USERY0=YMIN-0.1*(YMAX-YMIN)/(DISPY1-DISPY0-0.2)
            USERY1=YMAX+0.1*(YMAX-YMIN)/(DISPY1-DISPY0-0.2)
       ENDIF
       CALL GSWN(1,USERX0,USERX1,USERY0,USERY1)
       CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       CALL GRATTS('NUMBERS','TEXT')
       CALL GSTXP(0)
*** Figure out number of decades for log scaled plots.
       NDECX=0
       NDECY=0
       IF(LOGX)NDECX=NINT(LOG10(MAX(XMIN,XMAX)/MIN(XMIN,XMAX)))
       IF(LOGY)NDECY=NINT(LOG10(MAX(YMIN,YMAX)/MIN(YMIN,YMAX)))
       IF(LDEBUG)WRITE(10,'(''  ++++++ GRAXIS DEBUG   : Number of'',
     -      '' decades in x='',I3,'' in y='',I3)') NDECX,NDECY
*** Find a reasonable scale order-of-magnitude in x.
       IF(NDECX.LE.1)THEN
            KX=INT(LOG10(XMAX-XMIN))
            KKX=3*INT(LOG10(XMAX-XMIN)/3.0)
            IF(LOG10(XMAX-XMIN).LT.0.0)KX=KX-1
            IF(XMAX-XMIN.LT.0.1)KKX=KKX-3
            DX=(XMAX-XMIN)/10.0**KX
            IF(DX.LT.2.0)DX=0.1
            IF(DX.GE.2.0.AND.DX.LT.5.0)DX=0.2
            IF(DX.GE.5.0)DX=0.5
            DX=DX*10.0**KX
       ELSE
            KKX=0
       ENDIF
*   And same thing in y.
       IF(NDECY.LE.1)THEN
            KY=INT(LOG10(YMAX-YMIN))
            KKY=3*INT(LOG10(YMAX-YMIN)/3.0)
            IF(LOG10(YMAX-YMIN).LT.0.0)KY=KY-1
            IF(YMAX-YMIN.LT.0.1)KKY=KKY-3
            DY=(YMAX-YMIN)/10.0**KY
            IF(DY.LT.2.0)DY=0.1
            IF(DY.GE.2.0.AND.DY.LT.5.0)DY=0.2
            IF(DY.GE.5.0)DY=0.5
            DY=DY*10.0**KY
       ELSE
            KKY=0
       ENDIF
*** Calculate the length of a tick mark.
       IF(LOGX)THEN
            TICKX=10.0**(LOG10(XMAX/XMIN)/100.0)
       ELSE
            TICKX=(XMAX-XMIN)/100.0
       ENDIF
       IF(LOGY)THEN
            TICKY=10.0**(LOG10(YMAX/YMIN)/100.0)
       ELSE
            TICKY=(YMAX-YMIN)/100.0
       ENDIF
       IF(LDEBUG)WRITE(10,'(''  ++++++ GRAXIS DEBUG   : Tickmark size'',
     -      '' in x='',E12.5,'' in y='',E12.5)') TICKX,TICKY
*** Plot a box around the user area.
       XU(1)=XMIN
       YU(1)=YMIN
       XU(2)=XMAX
       YU(2)=YMIN
       XU(3)=XMAX
       YU(3)=YMAX
       XU(4)=XMIN
       YU(4)=YMAX
       XU(5)=XMIN
       YU(5)=YMIN
       CALL GSELNT(1)
       CALL GRLINE(5,XU,YU)
*** x-Axis: tickmarks and scales.
       IF(NDECX.LE.1)THEN
            CALL GSCHUP(+1.0,0.0)
            CALL GSTXAL(1,3)
            DO 20 I=0,1+INT((XMAX-XMIN)/DX)
            XVAL=DX*(INT(XMIN/DX)+I)
            IF(XMIN.GE.XVAL.OR.XVAL.GE.XMAX)GOTO 20
*   Tickmarks.
            XU(1)=XVAL
            XU(2)=XVAL
            YU(1)=YMIN
            IF(LOGY)THEN
                 YU(2)=YMIN*TICKY
            ELSE
                 YU(2)=YMIN+TICKY
            ENDIF
            CALL GRLINE(2,XU,YU)
            YU(1)=YMAX
            IF(LOGY)THEN
                 YU(2)=YMAX/TICKY
            ELSE
                 YU(2)=YMAX-TICKY
            ENDIF
            CALL GRLINE(2,XU,YU)
*   Optional grid.
            IF(LGRID)THEN
                 IF(LOGY)THEN
                      YU(1)=YMIN*TICKY
                      YU(2)=YMAX/TICKY
                 ELSE
                      YU(1)=YMIN+TICKY
                      YU(2)=YMAX-TICKY
                 ENDIF
                 CALL GRATTS('GRID','POLYLINE')
                 CALL GRLINE(2,XU,YU)
                 CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            ENDIF
*   Scale.
            CALL OUTFMT(XVAL/10.0**KKX,2,TICK,NC,'LEFT')
            CALL GSELNT(0)
            IF(.NOT.LOGX)XSC=XUTOD(XVAL)
            IF(LOGX)XSC=XUTOD(LOG10(XVAL))
            CALL GRTX(XSC,DISPY0+0.1-GPXN,TICK(1:NC))
            CALL GSELNT(1)
20          CONTINUE
**  Log scale of 3 decades and less: 1-9 every decade.
       ELSE
*   Compute the size of the power-of-10 box.
            CALL GSCHUP(0.0,1.0)
            CALL GSTXAL(0,0)
            CALL GSELNT(0)
            CALL GQTXX(IWK,0.5,0.5,'9',IERR,CPX,CPY,XBOX,YBOX)
            YPOWER=MAX(YBOX(1),YBOX(2),YBOX(3),YBOX(4))-
     -           MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4))
            CALL GSELNT(1)
*   Establish range of decades.
            NDEC0=INT(LOG10(XMIN))-1
            NDEC1=INT(LOG10(XMAX))+1
*   Loop over the decades.
            DO 30 IDEC=NDEC0,NDEC1
            DO 40 I=1,9
            XVAL=I*10.0**IDEC
            IF(XVAL.LE.XMIN.OR.XVAL.GE.XMAX)GOTO 40
*   Tickmarks.
            XU(1)=XVAL
            XU(2)=XVAL
            YU(1)=YMIN
            IF(LOGY)THEN
                 YU(2)=YMIN*TICKY
            ELSE
                 YU(2)=YMIN+TICKY
            ENDIF
            CALL GRLINE(2,XU,YU)
            YU(1)=YMAX
            IF(LOGY)THEN
                 YU(2)=YMAX/TICKY
            ELSE
                 YU(2)=YMAX-TICKY
            ENDIF
            CALL GRLINE(2,XU,YU)
*   Optional grid.
            IF(LGRID.AND.(LGRALL.OR.I.EQ.1))THEN
                 IF(LOGY)THEN
                      YU(1)=YMIN*TICKY
                      YU(2)=YMAX/TICKY
                 ELSE
                      YU(1)=YMIN+TICKY
                      YU(2)=YMAX-TICKY
                 ENDIF
                 CALL GRATTS('GRID','POLYLINE')
                 CALL GRLINE(2,XU,YU)
                 CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            ENDIF
*   Scale.
            CALL GSELNT(0)
*   Decades.
            IF(I.EQ.1)THEN
                 IF(LOGX)THEN
                      XSC=XUTOD(LOG10(XVAL))
                 ELSE
                      XSC=XUTOD(XVAL)
                 ENDIF
                 IF(IDEC.EQ.0)THEN
                      CALL GSTXAL(2,1)
                      CALL GRTX(XSC,DISPY0+0.1-GPXN10-YPOWER,'1')
                 ELSEIF(IDEC.EQ.1)THEN
                      CALL GSTXAL(2,1)
                      CALL GRTX(XSC,DISPY0+0.1-GPXN10-YPOWER,'10')
                 ELSE
                      CALL GSTXAL(2,1)
                      CALL GRTX(XSC,DISPY0+0.1-GPXN10-YPOWER,'10')
                      CALL GQTXX(IWK,0.5,0.5,'10',IERR,CPX,CPY,
     -                     XBOX,YBOX)
                      XPOWER=MAX(XBOX(1),XBOX(2),XBOX(3),XBOX(4))-
     -                     MIN(XBOX(1),XBOX(2),XBOX(3),XBOX(4))
                      CALL OUTFMT(REAL(IDEC),2,TICK,NC,'LEFT')
                      CALL GSTXAL(1,0)
                      CALL GRTX(XSC+XPOWER/2,DISPY0+0.1-GPXN10-YPOWER,
     -                     TICK(1:NC))
                 ENDIF
*   Numbers.
            ELSEIF(NDECX.LE.3)THEN
                 CALL OUTFMT(REAL(I),2,TICK,NC,'LEFT')
                 IF(LOGX)THEN
                      XSC=XUTOD(LOG10(XVAL))
                 ELSE
                      XSC=XUTOD(XVAL)
                 ENDIF
                 CALL GSTXAL(2,1)
                 CALL GRTX(XSC,DISPY0+0.1-GPXN,TICK(1:NC))
            ENDIF
            CALL GSELNT(1)
40          CONTINUE
30          CONTINUE
       ENDIF
*** y-Axis: Tickmarks and scales.
       CALL GSCHUP(0.0,1.0)
       IF(NDECY.LE.1)THEN
            CALL GSTXAL(3,3)
            DO 50 I=0,1+INT((YMAX-YMIN)/DY)
            YVAL=DY*(INT(YMIN/DY)+I)
            IF(YMIN.GE.YVAL.OR.YVAL.GE.YMAX)GOTO 50
*   Tickmarks.
            YU(1)=YVAL
            YU(2)=YVAL
            XU(1)=XMIN
            IF(LOGX)THEN
                 XU(2)=XMIN*TICKX
            ELSE
                 XU(2)=XMIN+TICKX
            ENDIF
            CALL GRLINE(2,XU,YU)
            XU(1)=XMAX
            IF(LOGX)THEN
                 XU(2)=XMAX/TICKX
            ELSE
                 XU(2)=XMAX-TICKX
            ENDIF
            CALL GRLINE(2,XU,YU)
*   Optional grid.
            IF(LGRID)THEN
                 IF(LOGX)THEN
                      XU(1)=XMIN*TICKX
                      XU(2)=XMAX/TICKX
                 ELSE
                      XU(1)=XMIN+TICKX
                      XU(2)=XMAX-TICKX
                 ENDIF
                 CALL GRATTS('GRID','POLYLINE')
                 CALL GRLINE(2,XU,YU)
                 CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            ENDIF
*   Scale.
            CALL OUTFMT(YVAL/10.0**KKY,2,TICK,NC,'LEFT')
            CALL GSELNT(0)
            IF(LOGY)THEN
                 YSC=YUTOD(LOG10(YVAL))
            ELSE
                 YSC=YUTOD(YVAL)
            ENDIF
            CALL GRTX(DISPX0+0.1-GPYN,YSC,TICK(1:NC))
            CALL GSELNT(1)
50          CONTINUE
**  Log scale of 3 decades and less: 1-9 every decade.
       ELSE
*   Compute decade range.
            NDEC0=INT(LOG10(YMIN))-1
            NDEC1=INT(LOG10(YMAX))+1
*   Loop over the decades.
            DO 60 IDEC=NDEC0,NDEC1
            DO 70 I=1,9
            YVAL=I*10.0**IDEC
            IF(YVAL.LE.YMIN.OR.YVAL.GE.YMAX)GOTO 70
*   Tickmarks.
            XU(1)=XMIN
            IF(LOGX)THEN
                 XU(2)=XMIN*TICKX
            ELSE
                 XU(2)=XMIN+TICKX
            ENDIF
            YU(1)=YVAL
            YU(2)=YVAL
            CALL GRLINE(2,XU,YU)
            XU(1)=XMAX
            IF(LOGX)THEN
                 XU(2)=XMAX/TICKX
            ELSE
                 XU(2)=XMAX-TICKX
            ENDIF
            CALL GRLINE(2,XU,YU)
*   Optional grid.
            IF(LGRID.AND.(LGRALL.OR.I.EQ.1))THEN
                 IF(LOGX)THEN
                      XU(1)=XMIN*TICKX
                      XU(2)=XMAX/TICKX
                 ELSE
                      XU(1)=XMIN+TICKX
                      XU(2)=XMAX-TICKX
                 ENDIF
                 CALL GRATTS('GRID','POLYLINE')
                 CALL GRLINE(2,XU,YU)
                 CALL GRATTS('BOX-TICKMARKS','POLYLINE')
            ENDIF
*   Scale.
            CALL GSELNT(0)
            IF(I.EQ.1)THEN
                 IF(LOGY)THEN
                      YSC=YUTOD(LOG10(YVAL))
                 ELSE
                      YSC=YUTOD(YVAL)
                 ENDIF
                 IF(IDEC.EQ.0)THEN
                      CALL GSTXAL(3,3)
                      CALL GRTX(DISPX0+0.1-GPYN10,YSC,'1')
                 ELSEIF(IDEC.EQ.1)THEN
                      CALL GSTXAL(3,3)
                      CALL GRTX(DISPX0+0.1-GPYN10,YSC,'10')
                 ELSE
                      CALL GSTXAL(3,3)
                      CALL OUTFMT(REAL(IDEC),2,TICK,NC,'LEFT')
                      CALL GQTXX(IWK,0.5,0.5,TICK(1:NC),IERR,CPX,CPY,
     -                     XBOX,YBOX)
                      XPOWER=MAX(XBOX(1),XBOX(2),XBOX(3),XBOX(4))-
     -                     MIN(XBOX(1),XBOX(2),XBOX(3),XBOX(4))
                      YPOWER=MAX(YBOX(1),YBOX(2),YBOX(3),YBOX(4))-
     -                     MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4))
                      CALL GRTX(DISPX0+0.1-GPYN10-XPOWER,YSC,'10')
                      CALL GSTXAL(1,0)
                      CALL GRTX(DISPX0+0.1-GPYN10-XPOWER,YSC+YPOWER/2,
     -                     TICK(1:NC))
                 ENDIF
            ELSEIF(NDECY.LE.3)THEN
                 CALL OUTFMT(REAL(I),2,TICK,NC,'LEFT')
                 IF(LOGY)THEN
                      YSC=YUTOD(LOG10(YVAL))
                 ELSE
                      YSC=YUTOD(YVAL)
                 ENDIF
                 CALL GSTXAL(3,3)
                 CALL GRTX(DISPX0+0.1-GPYN,YSC,TICK(1:NC))
            ENDIF
            CALL GSELNT(1)
70          CONTINUE
60          CONTINUE
       ENDIF
*** Plot the title at the top and labels along the axis.
       CALL GSELNT(0)
*   Title.
       CALL GSCHUP(0.0,1.0)
       CALL GSTXAL(1,1)
       CALL GRATTS('TITLE','TEXT')
       CALL GRTX(DISPX0+0.1,DISPY1-GPXT,TITLE)
*   Label the x-axis.
       CALL GSTXAL(3,0)
       CALL GSCHUP(0.0,1.0)
       CALL GRATTS('LABELS','TEXT')
       CALL GQTXX(IWK,0.5,0.5,XTXT,IERR,CPX,CPY,XBOX,YBOX)
       YSHIFT=0.5-MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4))
       CALL GRTX(DISPX1-0.1,DISPY0+GPXL+YSHIFT,XTXT)
       IF(KKX.NE.0)THEN
            CALL GSTXAL(1,0)
            CALL GSCHUP(1.0,0.0)
            CALL OUTFMT(REAL(KKX),2,AUX,NC,'LEFT')
            CALL GQTXX(IWK,0.5,0.5,AUX(1:NC),IERR,CPX,CPY,XBOX,YBOX)
            XPOWER=MAX(XBOX(1),XBOX(2),XBOX(3),XBOX(4))-
     -           MIN(XBOX(1),XBOX(2),XBOX(3),XBOX(4))
            YPOWER=MAX(YBOX(1),YBOX(2),YBOX(3),YBOX(4))-
     -           MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4))
            CALL GSTXAL(3,1)
            CALL GRATTS('LABELS','TEXT')
            CALL GRTX(DISPX1-GPYL-XPOWER,DISPY0+GPXL+YPOWER,'*10')
            CALL GRATTS('NUMBERS','TEXT')
            CALL GRTX(DISPX1-GPYL,DISPY0+GPXL,AUX(1:NC))
       ENDIF
*   And label the y-axis.
       CALL GSTXAL(3,1)
       CALL GSCHUP(-1.0,0.0)
       CALL GRATTS('LABELS','TEXT')
       CALL GRTX(DISPX0+GPYL,DISPY1-0.1,YTXT)
       IF(KKY.NE.0)THEN
            CALL GSTXAL(0,0)
            CALL GSCHUP(0.0,1.0)
            CALL OUTFMT(REAL(KKY),2,AUX,NC,'LEFT')
            CALL GQTXX(IWK,0.5,0.5,'*10',IERR,CPX,CPY,XBOX,YBOX)
            XPOWER=MAX(XBOX(1),XBOX(2),XBOX(3),XBOX(4))-
     -           MIN(XBOX(1),XBOX(2),XBOX(3),XBOX(4))
            YPOWER=MAX(YBOX(1),YBOX(2),YBOX(3),YBOX(4))-
     -           MIN(YBOX(1),YBOX(2),YBOX(3),YBOX(4))
            CALL GRATTS('LABELS','TEXT')
            CALL GRTX(DISPX0+GPYL,DISPY1-0.08,'*10')
            CALL GRATTS('NUMBERS','TEXT')
            CALL GRTX(DISPX0+GPYL+XPOWER,DISPY1-0.08+YPOWER,AUX(1:NC))
       ENDIF
*   Reset normalisation transformation, alignment and up-vector.
       CALL GSELNT(1)
       CALL GSTXAL(0,0)
       CALL GSCHUP(0.0,1.0)
*** Set a reasonable default representation.
       CALL GRATTS('FUNCTION-1','POLYLINE')
       CALL GRATTS('FUNCTION-1','POLYMARKER')
       CALL GRATTS('FUNCTION-1','TEXT')
       CALL GRATTS('FUNCTION-1','AREA')
*** Reset the bar chart and histogram counters.
       IGBAR=0
       IGHIST=0
       END
