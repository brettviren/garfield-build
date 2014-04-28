CDECK  ID>, GRMARK.
       SUBROUTINE GRMARK(NU,XU,YU)
*-----------------------------------------------------------------------
*   GRMARK - Draws a polymarker in either log or linear coordinates.
*   (Last changed on 18/10/06.)
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
       INTEGER NU,I,NOUT
       REAL XU(NU),YU(NU),XPL(MXLIST),YPL(MXLIST),XMRK,YMRK
*** Check number of points.
       IF(LDEBUG)WRITE(10,'(''  ++++++ GRMARK DEBUG   : Line has '',I3,
     -      '' points, scales: '',2L1)') NU,LOGX,LOGY
*** Copy, transforming if needed.
       NOUT=0
       DO 10 I=1,NU
       IF(LOGX)THEN
            IF(XU(I).LE.0.0)THEN
                 WRITE(10,'(''  !!!!!! GRMARK WARNING : Negative'',
     -                '' value x='',E12.5,'' received.'')') XU(I)
                 XMRK=FRXMIN-2*ABS(FRXMAX-FRXMIN)
            ELSE
                 XMRK=LOG10(XU(I))
            ENDIF
       ELSE
            XMRK=XU(I)
       ENDIF
       IF(LOGY)THEN
            IF(YU(I).LE.0.0)THEN
                 WRITE(10,'(''  !!!!!! GRMARK WARNING : Negative'',
     -                '' value y='',E12.5,'' received.'')') YU(I)
                 YMRK=FRYMIN-2*ABS(FRYMAX-FRYMIN)
            ELSE
                 YMRK=LOG10(YU(I))
            ENDIF
       ELSE
            YMRK=YU(I)
       ENDIF
*   Always store if there is no clipping.
       IF(.NOT.LGMCLP)THEN
            IF(NOUT.GE.MXLIST)THEN
                 CALL GPM(NOUT,XPL,YPL)
                 NOUT=0
            ENDIF
            NOUT=NOUT+1
            XPL(NOUT)=XMRK
            YPL(NOUT)=YMRK
*   Store if in frame.
       ELSEIF(XMRK.GE.FRXMIN.AND.XMRK.LE.FRXMAX.AND.
     -      YMRK.GE.FRYMIN.AND.YMRK.LE.FRYMAX)THEN
            IF(NOUT.GE.MXLIST)THEN
                 CALL GPM(NOUT,XPL,YPL)
                 NOUT=0
            ENDIF
            NOUT=NOUT+1
            XPL(NOUT)=XMRK
            YPL(NOUT)=YMRK
            IF(LDEBUG)WRITE(10,'(26X,2E12.5,'' -> '',2E12.5)')
     -           XU(I),YU(I),XPL(NOUT),YPL(NOUT)
*   Otherwise simply skip.
       ELSE
            IF(LDEBUG)WRITE(10,'(26X,2E12.5,''    not plotted'')')
     -           XU(I),YU(I)
       ENDIF
10     CONTINUE
*** Plot the selected markers.
       IF(NOUT.GE.1)CALL GPM(NOUT,XPL,YPL)
       END
