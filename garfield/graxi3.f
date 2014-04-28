CDECK  ID>, GRAXI3.
       SUBROUTINE GRAXI3(VXMIN,VYMIN,VXMAX,VYMAX,
     -      XTXT,YTXT,ZTXT,TITLE,OPTION)
*----------------------------------------------------------------------
*   GRAXI3 - Plots axes for a 3D view, with tickmarks along them.
*   VARIABLES : VXMIN etc   : View limits.
*               [X/Y/Z]TXT  : Labels for the x, y and z axes
*               TITLE       : Global title.
*               OPTION      : VIEW (compute view) or PLOT (plot frame).
*   (Last changed on  5/ 8/02.)
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
       DOUBLE PRECISION XU(101),YU(101),
     -      XUTOD,YUTOD,X,Y,DX,DY,DZ,
     -      TICKX,TICKY,TICKZ,XVAL,YVAL,ZVAL,XSC,YSC,XAUX,YAUX,
     -      X1,X2,X3,X4,X5,X6,X7,X8,Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8,
     -      XLAB,YLAB,ZLAB,QLAB,XSHIFT,YSHIFT,SNORM,XPERP,YPERP,
     -      WW,ASPECT,VXMIN,VYMIN,VXMAX,VYMAX
       INTEGER KX,KKX,KY,KKY,KZ,KKZ,NCTICK,NC,I,ICOL
       LOGICAL INVERT,SEEN(12)
       CHARACTER*(*) XTXT,YTXT,ZTXT,TITLE,OPTION
       CHARACTER*80 STRING
       CHARACTER*13 TICK
*** Define 2 statement function to convert from USER to DISP.
       XUTOD(X)=DISPX0+(DISPX1-DISPX0)*(X-USERX0)/(USERX1-USERX0)
       YUTOD(Y)=DISPY0+(DISPY1-DISPY0)*(Y-USERY0)/(USERY1-USERY0)
*** Output the requested area, if debugging is requested.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ GRAXI3 DEBUG   :'',
     -      '' Requested area is ''/26X,''('',E10.3,'','',E10.3,'','',
     -      E10.3,'') to''/26X,''('',E10.3,'','',E10.3,'','',E10.3,
     -      '')'')') GXMIN,GYMIN,GZMIN,GXMAX,GYMAX,GZMAX
*** Compute dimensions of projected box.
       CALL PLACOO(GXMIN,GYMIN,GZMIN,X1,Y1)
       CALL PLACOO(GXMIN,GYMIN,GZMAX,X2,Y2)
       CALL PLACOO(GXMIN,GYMAX,GZMIN,X3,Y3)
       CALL PLACOO(GXMIN,GYMAX,GZMAX,X4,Y4)
       CALL PLACOO(GXMAX,GYMIN,GZMIN,X5,Y5)
       CALL PLACOO(GXMAX,GYMIN,GZMAX,X6,Y6)
       CALL PLACOO(GXMAX,GYMAX,GZMIN,X7,Y7)
       CALL PLACOO(GXMAX,GYMAX,GZMAX,X8,Y8)
*** Compute frame size.
       VXMIN=MIN(X1,X2,X3,X4,X5,X6,X7,X8)
       VXMAX=MAX(X1,X2,X3,X4,X5,X6,X7,X8)
       VYMIN=MIN(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
       VYMAX=MAX(Y1,Y2,Y3,Y4,Y5,Y6,Y7,Y8)
*** Return here unless OPTION has been set to PLOT.
       IF(INDEX(OPTION,'PLOT').EQ.0)RETURN
*** Store frame size.
       FRXMIN=VXMIN
       FRXMAX=VXMAX
       FRYMIN=VYMIN
       FRYMAX=VYMAX
       IF(FRXMAX.EQ.FRXMIN)THEN
            PRINT *,' !!!!!! GRAXI3 WARNING : Frame has zero size in'//
     -           ' x; enlarged.'
            FRXMIN=FRXMIN-2*ABS(FRXMIN)-1
            FRXMAX=FRXMAX+2*ABS(FRXMAX)+1
       ENDIF
       IF(FRYMAX.EQ.FRYMIN)THEN
            PRINT *,' !!!!!! GRAXI3 WARNING : Frame has zero size in'//
     -           ' y; enlarged.'
            FRYMIN=FRYMIN-2*ABS(FRYMIN)-1
            FRYMAX=FRYMAX+2*ABS(FRYMAX)+1
       ENDIF
*** Compute aspect ratio.
       IF(FRYMAX.EQ.FRYMIN.OR.FRXMAX.EQ.FRXMIN)THEN
            ASPECT=1
            PRINT *,' !!!!!! GRAXI3 WARNING : Aspect ratio 0'//
     -           ' or infinite; set to 1 (program bug)'
       ELSE
            ASPECT=SQRT(ABS((FRXMAX-FRXMIN)/(FRYMAX-FRYMIN)))
       ENDIF
*** Switch to graphics mode.
       CALL GRGRAF(.TRUE.)
*** Define display area of frame.
       CALL GSVP(1,DISPX0,DISPX1,DISPY0,DISPY1)
*** Define the user area in the plot frame.
       USERX0=FRXMIN-0.1*(FRXMAX-FRXMIN)/(DISPX1-DISPX0-0.2)
       USERX1=FRXMAX+0.1*(FRXMAX-FRXMIN)/(DISPX1-DISPX0-0.2)
       USERY0=FRYMIN-0.1*(FRYMAX-FRYMIN)/(DISPY1-DISPY0-0.2)
       USERY1=FRYMAX+0.1*(FRYMAX-FRYMIN)/(DISPY1-DISPY0-0.2)
       CALL GSWN(1,USERX0,USERX1,USERY0,USERY1)
       CALL GSTXP(0)
*** Shade the planes in which the light shines, set the representation.
       CALL GSELNT(1)
       CALL GRATTS('BOX-TICKMARKS','AREA')
       CALL GRATTS('BOX-TICKMARKS','POLYLINE')
*   Generate the colour table.
       IF(ICOLBX.EQ.0)THEN
            ICOLBX=ICOL0
            CALL COLSHD(ICOLBX)
            ICOL0=ICOL0+NPRCOL
       ENDIF
*   Set the SEEN flags for the edges of the box.
       DO 100 I=1,12
       SEEN(I)=.FALSE.
100    CONTINUE
*   The x=xmin plane.
       IF(FPROJA.GT.0)THEN
            CALL COLWGT(+1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLBX+2+MIN(NINT(WW*(NPRCOL-1)),NPRCOL-3)
            ELSE
                 ICOL=ICOLBX
                 PRINT *,' !!!!!! GRAXI3 WARNING : Request to plot'//
     -                ' a face seen from the back (program bug).'
            ENDIF
            CALL GSFACI(ICOL)
            XU(1)=X1
            YU(1)=Y1
            XU(2)=X3
            YU(2)=Y3
            XU(3)=X4
            YU(3)=Y4
            XU(4)=X2
            YU(4)=Y2
            XU(5)=X1
            YU(5)=Y1
            CALL GFA2(5,XU,YU)
            CALL GPL2(5,XU,YU)
            SEEN(1)=.TRUE.
            SEEN(2)=.TRUE.
            SEEN(3)=.TRUE.
            SEEN(4)=.TRUE.
*   Or the x=xmax plane.
       ELSEIF(FPROJA.LT.0)THEN
            CALL COLWGT(-1.0D0,0.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLBX+2+MIN(NINT(WW*(NPRCOL-1)),NPRCOL-3)
            ELSE
                 ICOL=ICOLBX
                 PRINT *,' !!!!!! GRAXI3 WARNING : Request to plot'//
     -                ' a face seen from the back (program bug).'
            ENDIF
            CALL GSFACI(ICOL)
            XU(1)=X5
            YU(1)=Y5
            XU(2)=X7
            YU(2)=Y7
            XU(3)=X8
            YU(3)=Y8
            XU(4)=X6
            YU(4)=Y6
            XU(5)=X5
            YU(5)=Y5
            CALL GFA2(5,XU,YU)
            CALL GPL2(5,XU,YU)
            SEEN(5)=.TRUE.
            SEEN(6)=.TRUE.
            SEEN(7)=.TRUE.
            SEEN(8)=.TRUE.
       ENDIF
*   The y=ymin plane.
       IF(FPROJB.GT.0)THEN
            CALL COLWGT(0.0D0,+1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLBX+2+MIN(NINT(WW*(NPRCOL-1)),NPRCOL-3)
            ELSE
                 ICOL=ICOLBX
                 PRINT *,' !!!!!! GRAXI3 WARNING : Request to plot'//
     -                ' a face seen from the back (program bug).'
            ENDIF
            CALL GSFACI(ICOL)
            XU(1)=X1
            YU(1)=Y1
            XU(2)=X2
            YU(2)=Y2
            XU(3)=X6
            YU(3)=Y6
            XU(4)=X5
            YU(4)=Y5
            XU(5)=X1
            YU(5)=Y1
            CALL GFA2(5,XU,YU)
            CALL GPL2(5,XU,YU)
            SEEN(1)=.TRUE.
            SEEN(5)=.TRUE.
            SEEN(9)=.TRUE.
            SEEN(12)=.TRUE.
*   Or the y=ymax plane.
       ELSEIF(FPROJB.LT.0)THEN
            CALL COLWGT(0.0D0,-1.0D0,0.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLBX+2+MIN(NINT(WW*(NPRCOL-1)),NPRCOL-3)
            ELSE
                 ICOL=ICOLBX
                 PRINT *,' !!!!!! GRAXI3 WARNING : Request to plot'//
     -                ' a face seen from the back (program bug).'
            ENDIF
            CALL GSFACI(ICOL)
            XU(1)=X3
            YU(1)=Y3
            XU(2)=X4
            YU(2)=Y4
            XU(3)=X8
            YU(3)=Y8
            XU(4)=X7
            YU(4)=Y7
            XU(5)=X3
            YU(5)=Y3
            CALL GFA2(5,XU,YU)
            CALL GPL2(5,XU,YU)
            SEEN(3)=.TRUE.
            SEEN(7)=.TRUE.
            SEEN(10)=.TRUE.
            SEEN(11)=.TRUE.
       ENDIF
*   The z=zmin plane.
       IF(FPROJC.GT.0)THEN
            CALL COLWGT(0.0D0,0.0D0,+1.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLBX+2+MIN(NINT(WW*(NPRCOL-1)),NPRCOL-3)
            ELSE
                 ICOL=ICOLBX
                 PRINT *,' !!!!!! GRAXI3 WARNING : Request to plot'//
     -                ' a face seen from the back (program bug).'
            ENDIF
            CALL GSFACI(ICOL)
            XU(1)=X1
            YU(1)=Y1
            XU(2)=X3
            YU(2)=Y3
            XU(3)=X7
            YU(3)=Y7
            XU(4)=X5
            YU(4)=Y5
            XU(5)=X1
            YU(5)=Y1
            CALL GFA2(5,XU,YU)
            CALL GPL2(5,XU,YU)
            SEEN(2)=.TRUE.
            SEEN(6)=.TRUE.
            SEEN(9)=.TRUE.
            SEEN(10)=.TRUE.
*   Or the z=zmax plane.
       ELSEIF(FPROJC.LT.0)THEN
            CALL COLWGT(0.0D0,0.0D0,-1.0D0,WW)
            IF(WW.GE.0)THEN
                 ICOL=ICOLBX+2+MIN(NINT(WW*(NPRCOL-1)),NPRCOL-3)
            ELSE
                 ICOL=ICOLBX
                 PRINT *,' !!!!!! GRAXI3 WARNING : Request to plot'//
     -                ' a face seen from the back (program bug).'
            ENDIF
            CALL GSFACI(ICOL)
            XU(1)=X2
            YU(1)=Y2
            XU(2)=X4
            YU(2)=Y4
            XU(3)=X8
            YU(3)=Y8
            XU(4)=X6
            YU(4)=Y6
            XU(5)=X2
            YU(5)=Y2
            CALL GFA2(5,XU,YU)
            CALL GPL2(5,XU,YU)
            SEEN(4)=.TRUE.
            SEEN(8)=.TRUE.
            SEEN(11)=.TRUE.
            SEEN(12)=.TRUE.
       ENDIF
*** Find a reasonable scale order-of-magnitude in x.
       KX=INT(LOG10(GXMAX-GXMIN))
       IF(LOG10(GXMAX-GXMIN).LT.0.0)KX=KX-1
       DX=(GXMAX-GXMIN)/10.0**KX
       IF(DX.LT.2.0)DX=0.1
       IF(DX.GE.2.0.AND.DX.LT.5.0)DX=0.2
       IF(DX.GE.5.0)DX=0.5
       DX=DX*10.0**KX
       IF(KX.GE.0.AND.KX.LE.1)THEN
            KKX=0
       ELSEIF(INDEX(OPTION,'NOCELL').NE.0)THEN
            KKX=3*INT(LOG10(GXMAX-GXMIN)/3.0)
            IF(GXMAX-GXMIN.LT.0.1)KKX=KKX-3
       ELSE
            KKX=2+3*INT(LOG10(0.01*(GXMAX-GXMIN))/3.0)
            IF(0.01*(GXMAX-GXMIN).LT.0.1)KKX=KKX-3
       ENDIF
*   And same thing in y.
       KY=INT(LOG10(GYMAX-GYMIN))
       IF(LOG10(GYMAX-GYMIN).LT.0.0)KY=KY-1
       DY=(GYMAX-GYMIN)/10.0**KY
       IF(DY.LT.2.0)DY=0.1
       IF(DY.GE.2.0.AND.DY.LT.5.0)DY=0.2
       IF(DY.GE.5.0)DY=0.5
       DY=DY*10.0**KY
       IF(KY.GE.0.AND.KY.LE.1)THEN
            KKY=0
       ELSEIF(INDEX(OPTION,'NOCELL').NE.0)THEN
            KKY=3*INT(LOG10(GYMAX-GYMIN)/3.0)
            IF(GYMAX-GYMIN.LT.0.1)KKY=KKY-3
       ELSE
            KKY=2+3*INT(LOG10(0.01*(GYMAX-GYMIN))/3.0)
            IF(0.01*(GYMAX-GYMIN).LT.0.1)KKY=KKY-3
       ENDIF
*   And same thing in z.
       KZ=INT(LOG10(GZMAX-GZMIN))
       IF(LOG10(GZMAX-GZMIN).LT.0.0)KZ=KZ-1
       DZ=(GZMAX-GZMIN)/10.0**KZ
       IF(DZ.LT.2.0)DZ=0.1
       IF(DZ.GE.2.0.AND.DZ.LT.5.0)DZ=0.2
       IF(DZ.GE.5.0)DZ=0.5
       DZ=DZ*10.0**KZ
       IF(KZ.GE.0.AND.KZ.LE.1)THEN
            KKZ=0
       ELSEIF(INDEX(OPTION,'NOCELL').NE.0)THEN
            KKZ=3*INT(LOG10(GZMAX-GZMIN)/3.0)
            IF(GZMAX-GZMIN.LT.0.1)KKZ=KKZ-3
       ELSE
            KKZ=2+3*INT(LOG10(0.01*(GZMAX-GZMIN))/3.0)
            IF(0.01*(GZMAX-GZMIN).LT.0.1)KKZ=KKZ-3
       ENDIF
*** Calculate the length of a tick mark.
       TICKX=(GXMAX-GXMIN)/100.0
       TICKY=(GYMAX-GYMIN)/100.0
       TICKZ=(GZMAX-GZMIN)/100.0
       IF(LDEBUG)WRITE(10,'(''  ++++++ GRAXI3 DEBUG   : Tickmark size'',
     -      '' in x='',E12.5,'' in y='',E12.5,'' in z='',E12.5)')
     -      TICKX,TICKY,TICKZ
*** x-Axis: tickmarks and scales.
       CALL GSTXAL(1,3)
       CALL GRATTS('NUMBERS','TEXT')
*   Determine optimal side to label.
       XPERP=Y6-Y2
       YPERP=X2-X6
       IF(XPERP+YPERP.GT.0)THEN
            XPERP=-XPERP
            YPERP=-YPERP
            INVERT=.TRUE.
       ELSE
            INVERT=.FALSE.
       ENDIF
       YLAB=GYMIN
       ZLAB=GZMIN
       QLAB=XPERP*X1+YPERP*Y1
       IF(XPERP*X2+YPERP*Y2.GT.QLAB)THEN
            QLAB=XPERP*X2+YPERP*Y2
            YLAB=GYMIN
            ZLAB=GZMAX
       ENDIF
       IF(XPERP*X3+YPERP*Y3.GT.QLAB)THEN
            QLAB=XPERP*X3+YPERP*Y3
            YLAB=GYMAX
            ZLAB=GZMIN
       ENDIF
       IF(XPERP*X4+YPERP*Y4.GT.QLAB)THEN
            QLAB=XPERP*X2+YPERP*Y2
            YLAB=GYMAX
            ZLAB=GZMAX
       ENDIF
       XSHIFT=XUTOD(XPERP)-XUTOD(0.0D0)
       YSHIFT=YUTOD(YPERP)-YUTOD(0.0D0)
       SNORM=SQRT(XSHIFT**2+YSHIFT**2)
       IF(SNORM.GT.0)THEN
            XSHIFT=XSHIFT/SNORM
            YSHIFT=YSHIFT/SNORM
       ENDIF
*   Loop over the intervals.
       DO 10 I=0,1+INT((GXMAX-GXMIN)/DX)
       XVAL=DX*(INT(GXMIN/DX)+I)
       IF(GXMIN.GE.XVAL.OR.XVAL.GE.GXMAX.OR.
     -      (FPROJB.EQ.0.AND.FPROJC.EQ.0))GOTO 10
*   Tickmarks.
       IF(SEEN(9))THEN
            CALL PLACOO(XVAL,GYMIN,GZMIN,XU(1),YU(1))
            CALL PLACOO(XVAL,GYMIN+TICKY,GZMIN+TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(12))THEN
            CALL PLACOO(XVAL,GYMIN,GZMAX,XU(1),YU(1))
            CALL PLACOO(XVAL,GYMIN+TICKY,GZMAX-TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(10))THEN
            CALL PLACOO(XVAL,GYMAX,GZMIN,XU(1),YU(1))
            CALL PLACOO(XVAL,GYMAX-TICKY,GZMIN+TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(11))THEN
            CALL PLACOO(XVAL,GYMAX,GZMAX,XU(1),YU(1))
            CALL PLACOO(XVAL,GYMAX-TICKY,GZMAX-TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
*   Optional grid.
       IF(LGRID)THEN
            CALL GRATTS('GRID','POLYLINE')
            IF(FPROJB.GT.0)THEN
                 CALL PLACOO(XVAL,GYMIN,GZMIN+TICKZ,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMIN,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ELSEIF(FPROJB.LT.0)THEN
                 CALL PLACOO(XVAL,GYMAX,GZMIN+TICKZ,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMAX,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(FPROJC.GT.0)THEN
                 CALL PLACOO(XVAL,GYMIN+TICKY,GZMIN,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMAX-TICKY,GZMIN,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ELSEIF(FPROJC.LT.0)THEN
                 CALL PLACOO(XVAL,GYMIN+TICKY,GZMAX,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMAX-TICKY,GZMAX,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       ENDIF
*   Scale.
       CALL OUTFMT(REAL(XVAL/10.0**KKX),2,TICK,NCTICK,'LEFT')
       IF(XPERP.LT.0)THEN
            CALL GSCHUP(REAL(YPERP/ASPECT),REAL(-XPERP*ASPECT))
            CALL GSTXAL(3,3)
       ELSE
            CALL GSCHUP(REAL(-YPERP/ASPECT),REAL(XPERP*ASPECT))
            CALL GSTXAL(1,3)
       ENDIF
       CALL PLACOO(XVAL,YLAB,ZLAB,XAUX,YAUX)
       XSC=XUTOD(XAUX)+0.01*XSHIFT
       YSC=YUTOD(YAUX)+0.01*YSHIFT
       CALL GSELNT(0)
       CALL GRTX(REAL(XSC),REAL(YSC),TICK(1:NCTICK))
       CALL GSELNT(1)
10     CONTINUE
*   Label the axis.
       IF(FPROJB.NE.0.OR.FPROJC.NE.0)THEN
            STRING=XTXT
            NC=LEN(XTXT)
            IF(INDEX(OPTION,'NOCELL').NE.0)THEN
                 IF(KKX.NE.0)THEN
                      CALL OUTFMT(REAL(KKX),2,TICK,NCTICK,'LEFT')
                      STRING(NC+1:NC+NCTICK+7)=' * 10**'//TICK(1:NCTICK)
                      NC=NC+NCTICK+7
                 ENDIF
            ELSEIF(KKX.EQ.2)THEN
                 STRING(NC+1:NC+4)=' [m]'
                 NC=NC+NCTICK+4
            ELSEIF(KKX.EQ.0)THEN
                 STRING(NC+1:NC+5)=' [cm]'
                 NC=NC+NCTICK+5
            ELSEIF(KKX.EQ.-1)THEN
                 STRING(NC+1:NC+5)=' [mm]'
                 NC=NC+NCTICK+5
            ELSEIF(KKX.EQ.-4)THEN
                 STRING(NC+1:NC+9)=' [micron]'
                 NC=NC+NCTICK+9
            ELSEIF(KKX.EQ.-7)THEN
                 STRING(NC+1:NC+5)=' [nm]'
                 NC=NC+NCTICK+5
            ELSE
                 CALL OUTFMT(REAL(KKX),2,TICK,NCTICK,'LEFT')
                 STRING(NC+1:NC+NCTICK+10)=' [10**'//TICK(1:NCTICK)//
     -                ' cm]'
                 NC=NC+NCTICK+10
            ENDIF
            IF(YPERP.LT.0)THEN
                 CALL GSCHUP(REAL(-XPERP*ASPECT),REAL(-YPERP/ASPECT))
            ELSE
                 CALL GSCHUP(REAL(XPERP*ASPECT),REAL(YPERP/ASPECT))
            ENDIF
            IF(INVERT)THEN
                 IF(YPERP.LT.0)THEN
                      CALL GSTXAL(1,0)
                 ELSE
                      CALL GSTXAL(3,1)
                 ENDIF
            ELSE
                 IF(YPERP.LT.0)THEN
                      CALL GSTXAL(3,0)
                 ELSE
                      CALL GSTXAL(1,1)
                 ENDIF
            ENDIF
            CALL PLACOO(GXMAX,YLAB,ZLAB,XAUX,YAUX)
            XSC=XUTOD(XAUX)+0.09*XSHIFT*ASPECT
            YSC=YUTOD(YAUX)+0.09*YSHIFT/ASPECT
            CALL GRATTS('LABELS','TEXT')
            CALL GSELNT(0)
            CALL GRTX(REAL(XSC),REAL(YSC),STRING(1:NC))
            CALL GSELNT(1)
       ENDIF
*** y-Axis: tickmarks and scales.
       CALL GSTXAL(1,3)
       CALL GRATTS('NUMBERS','TEXT')
*   Determine optimal side to label.
       XPERP=Y4-Y2
       YPERP=X2-X4
       IF(XPERP+YPERP.GT.0)THEN
            XPERP=-XPERP
            YPERP=-YPERP
            INVERT=.TRUE.
       ELSE
            INVERT=.FALSE.
       ENDIF
       XLAB=GXMIN
       ZLAB=GZMIN
       QLAB=XPERP*X1+YPERP*Y1
       IF(XPERP*X2+YPERP*Y2.GT.QLAB)THEN
            QLAB=XPERP*X2+YPERP*Y2
            XLAB=GXMIN
            ZLAB=GZMAX
       ENDIF
       IF(XPERP*X5+YPERP*Y5.GT.QLAB)THEN
            QLAB=XPERP*X5+YPERP*Y5
            XLAB=GXMAX
            ZLAB=GZMIN
       ENDIF
       IF(XPERP*X6+YPERP*Y6.GT.QLAB)THEN
            QLAB=XPERP*X6+YPERP*Y6
            XLAB=GXMAX
            ZLAB=GZMAX
       ENDIF
       XSHIFT=XUTOD(XPERP)-XUTOD(0.0D0)
       YSHIFT=YUTOD(YPERP)-YUTOD(0.0D0)
       SNORM=SQRT(XSHIFT**2+YSHIFT**2)
       IF(SNORM.GT.0)THEN
            XSHIFT=XSHIFT/SNORM
            YSHIFT=YSHIFT/SNORM
       ENDIF
*   Loop over the intervals.
       DO 20 I=0,1+INT((GYMAX-GYMIN)/DY)
       YVAL=DY*(INT(GYMIN/DY)+I)
       IF(GYMIN.GE.YVAL.OR.YVAL.GE.GYMAX.OR.
     -      (FPROJA.EQ.0.AND.FPROJC.EQ.0))GOTO 20
*   Tickmarks.
       IF(SEEN(2))THEN
            CALL PLACOO(GXMIN,YVAL,GZMIN,XU(1),YU(1))
            CALL PLACOO(GXMIN+TICKX,YVAL,GZMIN+TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(4))THEN
            CALL PLACOO(GXMIN,YVAL,GZMAX,XU(1),YU(1))
            CALL PLACOO(GXMIN+TICKX,YVAL,GZMAX-TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(6))THEN
            CALL PLACOO(GXMAX,YVAL,GZMIN,XU(1),YU(1))
            CALL PLACOO(GXMAX-TICKX,YVAL,GZMIN+TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(8))THEN
            CALL PLACOO(GXMAX,YVAL,GZMAX,XU(1),YU(1))
            CALL PLACOO(GXMAX-TICKX,YVAL,GZMAX-TICKZ,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
*   Optional grid.
       IF(LGRID)THEN
            CALL GRATTS('GRID','POLYLINE')
            IF(FPROJA.GT.0)THEN
                 CALL PLACOO(GXMIN,YVAL,GZMIN+TICKZ,XU(1),YU(1))
                 CALL PLACOO(GXMIN,YVAL,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ELSEIF(FPROJA.LT.0)THEN
                 CALL PLACOO(GXMAX,YVAL,GZMIN+TICKZ,XU(1),YU(1))
                 CALL PLACOO(GXMAX,YVAL,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(FPROJC.GT.0)THEN
                 CALL PLACOO(GXMIN+TICKX,YVAL,GZMIN,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,YVAL,GZMIN,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ELSEIF(FPROJC.LT.0)THEN
                 CALL PLACOO(GXMIN+TICKX,YVAL,GZMAX,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,YVAL,GZMAX,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       ENDIF
*   Scale.
       CALL OUTFMT(REAL(YVAL/10.0**KKY),2,TICK,NCTICK,'LEFT')
       IF(XPERP.LT.0)THEN
            CALL GSCHUP(REAL(YPERP/ASPECT),REAL(-XPERP*ASPECT))
            CALL GSTXAL(3,3)
       ELSE
            CALL GSCHUP(REAL(-YPERP/ASPECT),REAL(XPERP*ASPECT))
            CALL GSTXAL(1,3)
       ENDIF
       CALL PLACOO(XLAB,YVAL,ZLAB,XAUX,YAUX)
       XSC=XUTOD(XAUX)+0.01*XSHIFT
       YSC=YUTOD(YAUX)+0.01*YSHIFT
       CALL GSELNT(0)
       CALL GRTX(REAL(XSC),REAL(YSC),TICK(1:NCTICK))
       CALL GSELNT(1)
20     CONTINUE
*   Label the axis.
       IF(FPROJA.NE.0.OR.FPROJC.NE.0)THEN
            STRING=YTXT
            NC=LEN(YTXT)
            IF(INDEX(OPTION,'NOCELL').NE.0)THEN
                 IF(KKY.NE.0)THEN
                      CALL OUTFMT(REAL(KKY),2,TICK,NCTICK,'LEFT')
                      STRING(NC+1:NC+NCTICK+7)=' * 10**'//TICK(1:NCTICK)
                      NC=NC+NCTICK+7
                 ENDIF
            ELSEIF(KKY.EQ.2)THEN
                 STRING(NC+1:NC+4)=' [m]'
                 NC=NC+NCTICK+4
            ELSEIF(KKY.EQ.0)THEN
                 STRING(NC+1:NC+5)=' [cm]'
                 NC=NC+NCTICK+5
            ELSEIF(KKY.EQ.-1)THEN
                 STRING(NC+1:NC+5)=' [mm]'
                 NC=NC+NCTICK+5
            ELSEIF(KKY.EQ.-4)THEN
                 STRING(NC+1:NC+9)=' [micron]'
                 NC=NC+NCTICK+9
            ELSEIF(KKY.EQ.-7)THEN
                 STRING(NC+1:NC+5)=' [nm]'
                 NC=NC+NCTICK+5
            ELSE
                 CALL OUTFMT(REAL(KKY),2,TICK,NCTICK,'LEFT')
                 STRING(NC+1:NC+NCTICK+10)=' [10**'//TICK(1:NCTICK)//
     -                ' cm]'
                 NC=NC+NCTICK+10
            ENDIF
            IF(YPERP.LT.0)THEN
                 CALL GSCHUP(REAL(-XPERP*ASPECT),REAL(-YPERP/ASPECT))
            ELSE
                 CALL GSCHUP(REAL(XPERP*ASPECT),REAL(YPERP/ASPECT))
            ENDIF
            IF(INVERT)THEN
                 IF(YPERP.LT.0)THEN
                      CALL GSTXAL(1,0)
                 ELSE
                      CALL GSTXAL(3,1)
                 ENDIF
            ELSE
                 IF(YPERP.LT.0)THEN
                      CALL GSTXAL(3,0)
                 ELSE
                      CALL GSTXAL(1,1)
                 ENDIF
            ENDIF
            CALL PLACOO(XLAB,GYMAX,ZLAB,XAUX,YAUX)
            XSC=XUTOD(XAUX)+0.09*XSHIFT*ASPECT
            YSC=YUTOD(YAUX)+0.09*YSHIFT/ASPECT
            CALL GRATTS('LABELS','TEXT')
            CALL GSELNT(0)
            CALL GRTX(REAL(XSC),REAL(YSC),STRING(1:NC))
            CALL GSELNT(1)
       ENDIF
*** z-Axis: tickmarks and scales.
       CALL GSTXAL(1,3)
       CALL GRATTS('NUMBERS','TEXT')
*   Determine optimal side to label.
       XPERP=Y2-Y1
       YPERP=X1-X2
       IF(XPERP+YPERP.GT.0)THEN
            XPERP=-XPERP
            YPERP=-YPERP
            INVERT=.TRUE.
       ELSE
            INVERT=.FALSE.
       ENDIF
       XLAB=GXMIN
       YLAB=GYMIN
       QLAB=XPERP*X1+YPERP*Y1
       IF(XPERP*X3+YPERP*Y3.GT.QLAB)THEN
            QLAB=XPERP*X3+YPERP*Y3
            XLAB=GXMIN
            YLAB=GYMAX
       ENDIF
       IF(XPERP*X5+YPERP*Y5.GT.QLAB)THEN
            QLAB=XPERP*X5+YPERP*Y5
            XLAB=GXMAX
            YLAB=GYMIN
       ENDIF
       IF(XPERP*X7+YPERP*Y7.GT.QLAB)THEN
            QLAB=XPERP*X7+YPERP*Y7
            XLAB=GXMAX
            YLAB=GYMAX
       ENDIF
       XSHIFT=XUTOD(XPERP)-XUTOD(0.0D0)
       YSHIFT=YUTOD(YPERP)-YUTOD(0.0D0)
       SNORM=SQRT(XSHIFT**2+YSHIFT**2)
       IF(SNORM.GT.0)THEN
            XSHIFT=XSHIFT/SNORM
            YSHIFT=YSHIFT/SNORM
       ENDIF
*   Loop over the intervals.
       DO 30 I=0,1+INT((GZMAX-GZMIN)/DZ)
       ZVAL=DZ*(INT(GZMIN/DZ)+I)
       IF(GZMIN.GE.ZVAL.OR.ZVAL.GE.GZMAX.OR.
     -      (FPROJA.EQ.0.AND.FPROJB.EQ.0))GOTO 30
*   Tickmarks.
       IF(SEEN(1))THEN
            CALL PLACOO(GXMIN,GYMIN,ZVAL,XU(1),YU(1))
            CALL PLACOO(GXMIN+TICKX,GYMIN+TICKY,ZVAL,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(3))THEN
            CALL PLACOO(GXMIN,GYMAX,ZVAL,XU(1),YU(1))
            CALL PLACOO(GXMIN+TICKX,GYMAX-TICKY,ZVAL,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(5))THEN
            CALL PLACOO(GXMAX,GYMIN,ZVAL,XU(1),YU(1))
            CALL PLACOO(GXMAX-TICKX,GYMIN+TICKY,ZVAL,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
       IF(SEEN(7))THEN
            CALL PLACOO(GXMAX,GYMAX,ZVAL,XU(1),YU(1))
            CALL PLACOO(GXMAX-TICKX,GYMAX-TICKY,ZVAL,XU(2),YU(2))
            CALL GPL2(2,XU,YU)
       ENDIF
*   Optional grid.
       IF(LGRID)THEN
            CALL GRATTS('GRID','POLYLINE')
            IF(FPROJA.GT.0)THEN
                 CALL PLACOO(GXMIN,GYMIN+TICKY,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMIN,GYMAX-TICKY,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ELSEIF(FPROJA.LT.0)THEN
                 CALL PLACOO(GXMAX,GYMIN+TICKY,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMAX,GYMAX-TICKY,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(FPROJB.GT.0)THEN
                 CALL PLACOO(GXMIN+TICKX,GYMIN,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,GYMIN,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ELSEIF(FPROJB.LT.0)THEN
                 CALL PLACOO(GXMIN+TICKX,GYMAX,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,GYMAX,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       ENDIF
*   Scale.
       CALL OUTFMT(REAL(ZVAL/10.0**KKZ),2,TICK,NCTICK,'LEFT')
       IF(XPERP.LT.0)THEN
            CALL GSCHUP(REAL(YPERP/ASPECT),REAL(-XPERP*ASPECT))
            CALL GSTXAL(3,3)
       ELSE
            CALL GSCHUP(REAL(-YPERP/ASPECT),REAL(XPERP*ASPECT))
            CALL GSTXAL(1,3)
       ENDIF
       CALL PLACOO(XLAB,YLAB,ZVAL,XAUX,YAUX)
       XSC=XUTOD(XAUX)+0.01*XSHIFT
       YSC=YUTOD(YAUX)+0.01*YSHIFT
       CALL GSELNT(0)
       CALL GRTX(REAL(XSC),REAL(YSC),TICK(1:NCTICK))
       CALL GSELNT(1)
30     CONTINUE
*   Label the axis.
       IF(FPROJA.NE.0.OR.FPROJB.NE.0)THEN
            STRING=ZTXT
            NC=LEN(ZTXT)
            IF(INDEX(OPTION,'NOCELL').NE.0)THEN
                 IF(KKZ.NE.0)THEN
                      CALL OUTFMT(REAL(KKZ),2,TICK,NCTICK,'LEFT')
                      STRING(NC+1:NC+NCTICK+7)=' * 10**'//TICK(1:NCTICK)
                      NC=NC+NCTICK+7
                 ENDIF
            ELSEIF(KKZ.EQ.2)THEN
                 STRING(NC+1:NC+4)=' [m]'
                 NC=NC+NCTICK+4
            ELSEIF(KKZ.EQ.0)THEN
                 STRING(NC+1:NC+5)=' [cm]'
                 NC=NC+NCTICK+5
            ELSEIF(KKZ.EQ.-1)THEN
                 STRING(NC+1:NC+5)=' [mm]'
                 NC=NC+NCTICK+5
            ELSEIF(KKZ.EQ.-4)THEN
                 STRING(NC+1:NC+9)=' [micron]'
                 NC=NC+NCTICK+9
            ELSEIF(KKZ.EQ.-7)THEN
                 STRING(NC+1:NC+5)=' [nm]'
                 NC=NC+NCTICK+5
            ELSE
                 CALL OUTFMT(REAL(KKZ),2,TICK,NCTICK,'LEFT')
                 STRING(NC+1:NC+NCTICK+10)=' [10**'//TICK(1:NCTICK)//
     -                ' cm]'
                 NC=NC+NCTICK+10
            ENDIF
            IF(YPERP.LT.0)THEN
                 CALL GSCHUP(REAL(-XPERP*ASPECT),REAL(-YPERP/ASPECT))
            ELSE
                 CALL GSCHUP(REAL(XPERP*ASPECT),REAL(YPERP/ASPECT))
            ENDIF
            IF(INVERT)THEN
                 IF(YPERP.LT.0)THEN
                      CALL GSTXAL(1,0)
                 ELSE
                      CALL GSTXAL(3,1)
                 ENDIF
            ELSE
                 IF(YPERP.LT.0)THEN
                      CALL GSTXAL(3,0)
                 ELSE
                      CALL GSTXAL(1,1)
                 ENDIF
            ENDIF
            CALL PLACOO(XLAB,YLAB,GZMAX,XAUX,YAUX)
            XSC=XUTOD(XAUX)+0.09*XSHIFT*ASPECT
            YSC=YUTOD(YAUX)+0.09*YSHIFT/ASPECT
            CALL GRATTS('LABELS','TEXT')
            CALL GSELNT(0)
            CALL GRTX(REAL(XSC),REAL(YSC),STRING(1:NC))
            CALL GSELNT(1)
       ENDIF
*** Now plot the cell elements.
       IF(INDEX(OPTION,'NOCELL').EQ.0)CALL CELLA3
*** And plot box panels that are seen from the back, attributes.
       IF(LFULLB)THEN
            CALL GRATTS('BOX-TICKMARKS','POLYLINE')
*   The x=xmin plane.
            IF(FPROJA.LT.0)THEN
                 XU(1)=X1
                 YU(1)=Y1
                 XU(2)=X3
                 YU(2)=Y3
                 XU(3)=X4
                 YU(3)=Y4
                 XU(4)=X2
                 YU(4)=Y2
                 XU(5)=X1
                 YU(5)=Y1
                 CALL GPL2(5,XU,YU)
*   Or the x=xmax plane.
            ELSEIF(FPROJA.GT.0)THEN
                 XU(1)=X5
                 YU(1)=Y5
                 XU(2)=X7
                 YU(2)=Y7
                 XU(3)=X8
                 YU(3)=Y8
                 XU(4)=X6
                 YU(4)=Y6
                 XU(5)=X5
                 YU(5)=Y5
                 CALL GPL2(5,XU,YU)
            ENDIF
*   The y=ymin plane.
            IF(FPROJB.LT.0)THEN
                 XU(1)=X1
                 YU(1)=Y1
                 XU(2)=X2
                 YU(2)=Y2
                 XU(3)=X6
                 YU(3)=Y6
                 XU(4)=X5
                 YU(4)=Y5
                 XU(5)=X1
                 YU(5)=Y1
                 CALL GPL2(5,XU,YU)
*   Or the y=ymax plane.
            ELSEIF(FPROJB.GT.0)THEN
                 XU(1)=X3
                 YU(1)=Y3
                 XU(2)=X4
                 YU(2)=Y4
                 XU(3)=X8
                 YU(3)=Y8
                 XU(4)=X7
                 YU(4)=Y7
                 XU(5)=X3
                 YU(5)=Y3
                 CALL GPL2(5,XU,YU)
            ENDIF
*   The z=zmin plane.
            IF(FPROJC.LT.0)THEN
                 XU(1)=X1
                 YU(1)=Y1
                 XU(2)=X3
                 YU(2)=Y3
                 XU(3)=X7
                 YU(3)=Y7
                 XU(4)=X5
                 YU(4)=Y5
                 XU(5)=X1
                 YU(5)=Y1
                 CALL GPL2(5,XU,YU)
*   Or the z=zmax plane.
            ELSEIF(FPROJC.GT.0)THEN
                 XU(1)=X2
                 YU(1)=Y2
                 XU(2)=X4
                 YU(2)=Y4
                 XU(3)=X8
                 YU(3)=Y8
                 XU(4)=X6
                 YU(4)=Y6
                 XU(5)=X2
                 YU(5)=Y2
                 CALL GPL2(5,XU,YU)
            ENDIF
*** And complete with the tickmarks, loop over the x-axis.
            DO 40 I=0,1+INT((GXMAX-GXMIN)/DX)
            XVAL=DX*(INT(GXMIN/DX)+I)
            IF(GXMIN.GE.XVAL.OR.XVAL.GE.GXMAX)GOTO 40
            IF(.NOT.SEEN(9))THEN
                 CALL PLACOO(XVAL,GYMIN,GZMIN,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMIN+TICKY,GZMIN+TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(12))THEN
                 CALL PLACOO(XVAL,GYMIN,GZMAX,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMIN+TICKY,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(10))THEN
                 CALL PLACOO(XVAL,GYMAX,GZMIN,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMAX-TICKY,GZMIN+TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(11))THEN
                 CALL PLACOO(XVAL,GYMAX,GZMAX,XU(1),YU(1))
                 CALL PLACOO(XVAL,GYMAX-TICKY,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
40          CONTINUE
*   Over the y-axis.
            DO 50 I=0,1+INT((GYMAX-GYMIN)/DY)
            YVAL=DY*(INT(GYMIN/DY)+I)
            IF(GYMIN.GE.YVAL.OR.YVAL.GE.GYMAX)GOTO 50
            IF(.NOT.SEEN(2))THEN
                 CALL PLACOO(GXMIN,YVAL,GZMIN,XU(1),YU(1))
                 CALL PLACOO(GXMIN+TICKX,YVAL,GZMIN+TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(4))THEN
                 CALL PLACOO(GXMIN,YVAL,GZMAX,XU(1),YU(1))
                 CALL PLACOO(GXMIN+TICKX,YVAL,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(6))THEN
                 CALL PLACOO(GXMAX,YVAL,GZMIN,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,YVAL,GZMIN+TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(8))THEN
                 CALL PLACOO(GXMAX,YVAL,GZMAX,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,YVAL,GZMAX-TICKZ,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
50          CONTINUE
*   And the z-axis.
            DO 60 I=0,1+INT((GZMAX-GZMIN)/DZ)
            ZVAL=DZ*(INT(GZMIN/DZ)+I)
            IF(GZMIN.GE.ZVAL.OR.ZVAL.GE.GZMAX)GOTO 60
            IF(.NOT.SEEN(1))THEN
                 CALL PLACOO(GXMIN,GYMIN,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMIN+TICKX,GYMIN+TICKY,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(3))THEN
                 CALL PLACOO(GXMIN,GYMAX,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMIN+TICKX,GYMAX-TICKY,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(5))THEN
                 CALL PLACOO(GXMAX,GYMIN,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,GYMIN+TICKY,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
            IF(.NOT.SEEN(7))THEN
                 CALL PLACOO(GXMAX,GYMAX,ZVAL,XU(1),YU(1))
                 CALL PLACOO(GXMAX-TICKX,GYMAX-TICKY,ZVAL,XU(2),YU(2))
                 CALL GPL2(2,XU,YU)
            ENDIF
60          CONTINUE
       ENDIF
*** Plot the title at the top.
       CALL GRATTS('TITLE','TEXT')
       CALL GSTXAL(1,1)
       CALL GSCHUP(0.0,1.0)
       CALL GSELNT(0)
       CALL GRTX(DISPX0+0.1,DISPY1-GPXT,TITLE)
       CALL GSELNT(1)
*** And make a little sketch of the light source.
       CALL GSELNT(0)
       CALL GRATTS('BOX-TICKMARKS','POLYLINE')
       DO 110 I=1,101
       XU(I)=DISPX1-0.05+0.04*COS(0.02*I*PI)
       YU(I)=DISPY0+0.05+0.04*SIN(0.02*I*PI)
110    CONTINUE
       CALL GPL2(101,XU,YU)
       CALL PLACOO(DBLE(PRAL),DBLE(PRBL),DBLE(PRCL),XAUX,YAUX)
       XAUX=XAUX*0.04
       YAUX=YAUX*0.04
       CALL GRATTS('FUNCTION-1','POLYLINE')
       XU(1)=DISPX1-0.05+XAUX
       YU(1)=DISPY0+0.05+YAUX+0.005
       XU(2)=DISPX1-0.05+XAUX
       YU(2)=DISPY0+0.05+YAUX-0.005
       CALL GPL2(2,XU,YU)
       XU(1)=DISPX1-0.05+XAUX+0.005
       YU(1)=DISPY0+0.05+YAUX
       XU(2)=DISPX1-0.05+XAUX-0.005
       YU(2)=DISPY0+0.05+YAUX
       CALL GPL2(2,XU,YU)
       CALL GSELNT(1)
*** Reset the bar chart and histogram counters.
       IGBAR=0
       IGHIST=0
       CALL GSTXAL(0,0)
       END
