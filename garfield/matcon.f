CDECK  ID>, MATCON.
       SUBROUTINE MATCON(IREFM,IREFX,IREFY,XTXT,YTXT,TITLE,NCHTS,OPTION)
*-----------------------------------------------------------------------
*   MATCON - Plots contours for a matrix.
*   (Last changed on 29/11/01.)
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
       REAL MVEC(MXEMAT)
       INTEGER MSIZ(MXMAT,MXMDIM),MDIM(MXMAT),MREF(MXMAT+1),MMOD(MXMAT),
     -      MORG(MXMAT+1),MLEN(MXMAT+1),NREFL
       COMMON /MATDAT/ MVEC,MSIZ,MDIM,MMOD,MORG,MLEN,MREF,NREFL
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
       INTEGER MATSLT,MATADR,IA(MXMDIM),NX,NY,NCHTS,MXWS,IFAIL,IX,IY,
     -      ISLOTX,ISLOTY,ISLOTM,IREFX,IREFY,IREFM,IELEM
       REAL XMIN,XMAX,YMIN,YMAX,ZMIN,ZMAX
       CHARACTER*(*) XTXT,YTXT,TITLE,OPTION
       EXTERNAL MATSLT,MATADR
       REAL WS,PAR,COLFLG,CMIN,CMAX,STEP
       PARAMETER(MXWS=2*MXWIRE**2+8*MXWIRE-31)
       COMMON /MATRIX/ WS(MXWS),PAR(37)
*** Identify the routine if requested.
       IF(LIDENT)PRINT *,' /// ROUTINE MATCON ///'
*** Debugging output.
       IF(LDEBUG)WRITE(LUNOUT,'(''  ++++++ MATCON DEBUG   : Plotting'',
     -      '' matrix '',I5,'' axes '',2I5,'' #contours='',I5/
     -      26X,''x-Axis label: '',A/26X,''y-Axis label: '',A/
     -      26X,''Title: '',A/26X,''Options: '',A/)')
     -      IREFM,IREFX,IREFY,NCHTS,XTXT,YTXT,TITLE,OPTION
*** Verify number of contours.
       IF(NCHTS.LT.2.OR.NCHTS.GT.50)THEN
            PRINT *,' !!!!!! MATCON WARNING : Number of contours out'//
     -           ' of range; set to 10.'
            NCHTS=10
       ENDIF
*** Locate the matrix.
       ISLOTM=MATSLT(IREFM)
       IF(ISLOTM.LE.0)THEN
            PRINT *,' !!!!!! MATCON WARNING : Matrix to be plotted'//
     -           ' does not exist; not plotted.'
            RETURN
       ENDIF
       NX=MSIZ(ISLOTM,1)
       NY=MSIZ(ISLOTM,2)
*** See whether the coordinates are present.
       ISLOTX=MATSLT(IREFX)
       IF(ISLOTX.GT.0)THEN
            IF(MDIM(ISLOTX).EQ.1.AND.MSIZ(ISLOTX,1).EQ.NX)THEN
                 XMIN=MVEC(MORG(ISLOTX)+1)
                 XMAX=MVEC(MORG(ISLOTX)+MLEN(ISLOTX))
            ELSE
                 PRINT *,' !!!!!! MATCON WARNING : x-Coordinate'//
     -                ' vector does not have the right format.'
                 XMIN=0
                 XMAX=1
            ENDIF
       ELSE
            PRINT *,' ------ MATCON MESSAGE : x-Range of plot not'//
     -           ' given; set to [0,1].'
            XMIN=0
            XMAX=1
       ENDIF
       ISLOTY=MATSLT(IREFY)
       IF(ISLOTY.GT.0)THEN
            IF(MDIM(ISLOTY).EQ.1.AND.MSIZ(ISLOTY,1).EQ.NY)THEN
                 YMIN=MVEC(MORG(ISLOTY)+1)
                 YMAX=MVEC(MORG(ISLOTY)+MLEN(ISLOTY))
            ELSE
                 PRINT *,' !!!!!! MATCON WARNING : y-Coordinate'//
     -                ' vector does not have the right format.'
                 YMIN=0
                 YMAX=1
            ENDIF
       ELSE
            PRINT *,' ------ MATCON MESSAGE : y-Range of plot not'//
     -           ' given; set to [0,1].'
            YMIN=0
            YMAX=1
       ENDIF
*** Make sure that this matrix has the right dimensions.
       IF(MDIM(ISLOTM).NE.2.OR.
     -      NX.LT.2.OR.NY.LT.2.OR.
     -      NX*NY.GT.MXWS)THEN
            PRINT *,' !!!!!! MATCON WARNING : The matrix to be'//
     -           ' plotted doesn''t have the right dimensions.'
            RETURN
       ENDIF
*** Obtain the matrix for surface plotting.
       CALL BOOK('BOOK','MATRIX','MATCONT',IFAIL)
       IF(IFAIL.NE.0)THEN
            PRINT *,' !!!!!! MATCON WARNING : Unable to obtain'//
     -           ' storage for the surface plot; plot not made.'
            RETURN
       ENDIF
*** Transfer the matrix to the fixed arrays, establish range.
       IELEM=0
       DO 10 IY=1,NY
       DO 20 IX=1,NX
       IA(1)=IX
       IA(2)=IY
       IELEM=IELEM+1
       WS(IELEM)=MVEC(MATADR(ISLOTM,IA))
       IF(IELEM.EQ.1)THEN
            ZMIN=WS(IELEM)
            ZMAX=WS(IELEM)
       ELSE
            IF(ZMIN.GT.WS(IELEM))ZMIN=WS(IELEM)
            IF(ZMAX.LT.WS(IELEM))ZMAX=WS(IELEM)
       ENDIF
20     CONTINUE
10     CONTINUE
*** Make the plot, go to to graphics mode.
       CALL GRGRAF(.TRUE.)
*   Compute a reasonable set of contours.
       CMIN=ZMIN
       CMAX=ZMAX
       IF(INDEX(OPTION,'ROUND').NE.0)THEN
            CALL ROUND(CMIN,CMAX,NCHTS,'SMALLER',STEP)
            IF(STEP.NE.0)THEN
                 NCHTS=1+NINT((CMAX-CMIN)/STEP)
            ELSE
                 CMIN=ZMIN
                 CMAX=ZMAX
                 NCHTS=10
            ENDIF
       ENDIF
       IF(INDEX(OPTION,'COLOUR').NE.0)THEN
            COLFLG=0
       ELSEIF(INDEX(OPTION,'TYPE').NE.0)THEN
            COLFLG=1
       ELSE
            COLFLG=2
       ENDIF
*   Fill the PAR vector.
       IF(COLFLG.LT.0.5)THEN
            PAR(1)=0
            PAR(2)=0
       ELSE
            PAR(1)=NCHTS
            PAR(2)=COLFLG
       ENDIF
       PAR(3)=XMIN-0.5*(XMAX-XMIN)/REAL(NX-1)
       PAR(4)=XMAX+0.5*(XMAX-XMIN)/REAL(NX-1)
       PAR(5)=YMIN-0.5*(YMAX-YMIN)/REAL(NY-1)
       PAR(6)=YMAX+0.5*(YMAX-YMIN)/REAL(NY-1)
       PAR(7)=CMIN
       PAR(8)=CMAX
       PAR(9)=0
       PAR(10)=0
*   Plot the contours.
       CALL GRCART(PAR(3),PAR(5),PAR(4),PAR(6),XTXT,YTXT,TITLE)
       CALL ISVP(1,DISPX0+0.1,DISPX1-0.1,
     -      DISPY0+0.1,DISPY1-0.1)
       CALL ISELNT(1)
       IF(COLFLG.LT.0.5)THEN
            CALL IGTABL(NX,NY,WS,10,PAR,'COL')
       ELSE
            CALL IGTABL(NX,NY,WS,10,PAR,'C')
       ENDIF
*** Close the plot and register it.
       CALL GRNEXT
       CALL TIMLOG('Making a contour plot of a matrix:      ')
       CALL GRALOG('Contour plot of a matrix:')
*** Release the matrix.
       CALL BOOK('RELEASE','MATRIX','MATCONT',IFAIL)
       END
