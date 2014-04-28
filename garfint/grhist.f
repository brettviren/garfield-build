CDECK  ID>, GRHIST.
       SUBROUTINE GRHIST(CONTEN,NCHA,XMIN,XMAX,XTXT,TITLE,FRAME)
*----------------------------------------------------------------------
*   GRHIST - Subroutine plotting a histogram using the vector CONTEN
*            as contents and XMIN and XMAX as lower and upper x-bounds.
*   (Last changed on 22/ 3/07.)
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
       CHARACTER*(*) XTXT,TITLE
       CHARACTER*20 AUX1,AUX2,AUX3,STR
       INTEGER NCHA,I,IOUT,NC1,NC2,NC3,NCSTR
       REAL XPL(MXLIST),YPL(MXLIST),CONTEN(0:NCHA+1),SUM,XMIN,XMAX,
     -      YMIN,YMAX,FACT
       LOGICAL FRAME,SETRAN
*** Determine maximum and minimum y and compute the total contents.
       SETRAN=.FALSE.
       SUM=0
       DO 10 I=1,NCHA
       IF((.NOT.LOGY).OR.CONTEN(I).GT.0)THEN
            IF(.NOT.SETRAN)THEN
                 YMIN=CONTEN(I)
                 YMAX=CONTEN(I)
                 SETRAN=.TRUE.
            ELSE
                 IF(YMIN.GT.CONTEN(I))YMIN=CONTEN(I)
                 IF(YMAX.LT.CONTEN(I))YMAX=CONTEN(I)
            ENDIF
       ENDIF
       SUM=SUM+CONTEN(I)
10     CONTINUE
*** Check that a range has been set.
       IF(.NOT.SETRAN)THEN
            PRINT *,' !!!!!! GRHIST WARNING : No range can be set'//
     -           ' for the histogram plot.'
            IF(LOGY)THEN
                 YMIN=1
                 YMAX=10
            ELSE
                 YMIN=-1
                 YMAX=+1
            ENDIF
       ENDIF
*** Make the range look a bit nicer.
       IF(LOGY)THEN
            IF(YMIN.LE.0.AND.YMAX.GT.0)YMIN=1.0E-3*YMAX
            IF(YMIN.LE.0)YMIN=1.0
            IF(YMAX.LE.YMIN)YMAX=YMIN*10.0
            FACT=EXP(0.1*LOG(YMAX/YMIN))
            YMIN=YMIN/FACT
            YMAX=YMAX*FACT
       ELSE
            IF(YMIN.GT.0.0)YMIN=0.0
            IF(YMAX.LE.YMIN)YMAX=YMIN+1.0
            YMAX=1.1*YMAX
       ENDIF
*** Plot a frame using GRCART.
       IF(FRAME)CALL GRCART(XMIN,YMIN,XMAX,YMAX,XTXT,
     -      'Entries or probability',TITLE)
*** Set the correct graphics representation for the histogram.
       IGHIST=IGHIST+1
       IF(IGHIST.GT.7)IGHIST=1
       CALL OUTFMT(REAL(IGHIST),2,STR,NCSTR,'LEFT')
       CALL GRATTS('HISTOGRAM-'//STR(1:NCSTR),'POLYLINE')
*** Plot the histogram.
       IOUT=0
       DO 20 I=1,NCHA
*   Draw the horizontal segment of the bin.
       XPL(IOUT+1)=XMIN+REAL(I-1)*(XMAX-XMIN)/REAL(NCHA)
       XPL(IOUT+2)=XMIN+REAL(I  )*(XMAX-XMIN)/REAL(NCHA)
       YPL(IOUT+1)=CONTEN(I)
       YPL(IOUT+2)=CONTEN(I)
*   Check for 0 entries.
       IF(LOGX.AND.XPL(IOUT+1).LE.0)XPL(IOUT+1)=10.0**FRXMIN
       IF(LOGX.AND.XPL(IOUT+2).LE.0)XPL(IOUT+2)=10.0**FRXMIN
       IF(LOGY.AND.YPL(IOUT+1).LE.0)YPL(IOUT+1)=10.0**FRYMIN
       IF(LOGY.AND.YPL(IOUT+2).LE.0)YPL(IOUT+2)=10.0**FRYMIN
*   Increment the count.
       IOUT=IOUT+2
*   Check against buffer overflow.
       IF(IOUT.GE.MXLIST-1)THEN
            CALL GRLINE(IOUT,XPL,YPL)
            XPL(1)=XPL(IOUT)
            YPL(1)=YPL(IOUT)
            IOUT=1
       ENDIF
20     CONTINUE
*   Plot the remainder of the line.
       IF(IOUT.GE.2)CALL GRLINE(IOUT,XPL,YPL)
*** Indicate over- and underflow.
       IF(FRAME)THEN
            CALL OUTFMT(CONTEN(0)     ,2,AUX1,NC1,'LEFT')
            CALL OUTFMT(SUM           ,2,AUX2,NC2,'LEFT')
            CALL OUTFMT(CONTEN(NCHA+1),2,AUX3,NC3,'LEFT')
            CALL GRCOMM(3,'Under: '//AUX1(1:NC1)//', in: '//
     -           AUX2(1:NC2)//', over: '//AUX3(1:NC3))
       ENDIF
       END
