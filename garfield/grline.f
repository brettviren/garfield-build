CDECK  ID>, GRLINE.
       SUBROUTINE GRLINE(NU,XU,YU)
*-----------------------------------------------------------------------
*   GRLINE - Draws a line in either log or linear coordinates.
*   (Last changed on  6/ 8/02.)
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
       REAL XU(*),YU(*),XPL(MXLIST),YPL(MXLIST),XCUR,YCUR,XLAST,YLAST,
     -      X0,Y0,X1,Y1
       INTEGER NPL,IFAIL,NU,I
       LOGICAL CURIN,LASTIN
*** Check number of points.
       IF(NU.LE.1)RETURN
*** Initial settings.
       LASTIN=.FALSE.
       NPL=0
*** Loop over the input array.
       DO 10 I=1,NU
*   Transform x-coordinate if requested.
       IF(LOGX)THEN
            IF(XU(I).LE.0.0)THEN
                 XCUR=FRXMIN-2*ABS(FRXMAX-FRXMIN)
            ELSE
                 XCUR=LOG10(XU(I))
            ENDIF
       ELSE
            XCUR=XU(I)
       ENDIF
*   Transform y-coordinate if requested.
       IF(LOGY)THEN
            IF(YU(I).LE.0.0)THEN
                 YCUR=FRYMIN-2*ABS(FRYMAX-FRYMIN)
            ELSE
                 YCUR=LOG10(YU(I))
            ENDIF
       ELSE
            YCUR=YU(I)
       ENDIF
*   If no clipping is to be performed, then this is all.
       IF(.NOT.LGLCLP)THEN
            IF(NPL.GE.MXLIST)THEN
                 CALL GPL(NPL,XPL,YPL)
                 XPL(1)=XPL(NPL)
                 YPL(1)=YPL(NPL)
                 NPL=1
            ENDIF
            NPL=NPL+1
            XPL(NPL)=XCUR
            YPL(NPL)=YCUR
            GOTO 10
       ENDIF
*   See whether this point is located is inside the frame.
       IF(XCUR.GE.FRXMIN.AND.XCUR.LE.FRXMAX.AND.
     -      YCUR.GE.FRYMIN.AND.YCUR.LE.FRYMAX)THEN
            CURIN=.TRUE.
       ELSE
            CURIN=.FALSE.
       ENDIF
*   If this is the first point, add to the list and skip the rest.
       IF(I.EQ.1)THEN
            IF(CURIN)THEN
                 NPL=1
                 XPL(NPL)=XCUR
                 YPL(NPL)=YCUR
            ENDIF
            GOTO 20
       ENDIF
*   Compute fragment of this that fits in the frame.
       X0=XLAST
       Y0=YLAST
       X1=XCUR
       Y1=YCUR
       CALL CLIP(X0,Y0,X1,Y1,FRXMIN,FRYMIN,FRXMAX,FRYMAX,IFAIL)
*   If fully out (IFAIL=1) then skip the rest.
       IF(IFAIL.NE.0)THEN
            GOTO 20
*   If both current and last point are 'in', add the point.
       ELSEIF(LASTIN.AND.CURIN)THEN
            IF(NPL.GE.MXLIST)THEN
                 CALL GPL(NPL,XPL,YPL)
                 XPL(1)=XPL(NPL)
                 YPL(1)=YPL(NPL)
                 NPL=1
            ENDIF
            NPL=NPL+1
            XPL(NPL)=X1
            YPL(NPL)=Y1
*   If the last point was 'in' and current 'out', add and plot.
       ELSEIF(LASTIN.AND.(.NOT.CURIN))THEN
            IF(NPL.GE.MXLIST)THEN
                 CALL GPL(NPL,XPL,YPL)
                 XPL(1)=XPL(NPL)
                 YPL(1)=YPL(NPL)
                 NPL=1
            ENDIF
            NPL=NPL+1
            XPL(NPL)=X1
            YPL(NPL)=Y1
            IF(NPL.GT.1)CALL GPL(NPL,XPL,YPL)
            NPL=0
*   If the last point was 'out' and the current 'in', start a new line.
       ELSEIF(CURIN.AND.(.NOT.LASTIN))THEN
            IF(NPL.GT.1)CALL GPL(NPL,XPL,YPL)
            XPL(1)=X0
            YPL(1)=Y0
            XPL(2)=X1
            YPL(2)=Y1
            NPL=2
*   If both this point and the last are out, draw this line.
       ELSE
            IF(NPL.GT.1)CALL GPL(NPL,XPL,YPL)
            XPL(1)=X0
            YPL(1)=Y0
            XPL(2)=X1
            YPL(2)=Y1
            CALL GPL(2,XPL,YPL)
            NPL=0
       ENDIF
*   Move 'current' point to 'last' point.
20     CONTINUE
       XLAST=XCUR
       YLAST=YCUR
       LASTIN=CURIN
10     CONTINUE
*** Plot what remains in the buffer.
       IF(NPL.GE.2)CALL GPL(NPL,XPL,YPL)
       END
